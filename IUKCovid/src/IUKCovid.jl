module IUKCovid

using Distributions
using SpecialFunctions: gamma
using StatsBase: Weights
using DataFrames, DataFramesMeta
using Dates
using ThreadTools
#using BenchmarkTools, InteractiveUtils, FreqTables # Debug packages
#using ProgressMeter

global debug = false

function activateDebugging(choice::Bool)
	global debug = choice
end

function testConnection()
	return(true)
end

## Slower version due to type instability
# function getDistr(type, mean, stdDev)
#
# 	if debug println("getDistr") end
#
# 	if type == "Exponential"
# 		distr = Exponential(1 / mean)
#
# 	elseif type == "Weibull"
# 		shape = (stdDev / mean)^(-1.086)
# 		scale = mean / gamma( 1 + 1 / shape)
#
# 		distr = Weibull(shape, scale)
#
# 	elseif type == "Gamma"
# 		shape = mean^2 / stdDev^2
# 		scale = stdDev^2 / mean
#
# 		distr = Gamma(shape, scale)
#
# 	else
# 		error("Distribution not recognized")
# 	end
#
# 	n = 200
#
# 	ret = cdf.(distr, 0:n)
#
# 	ret[2:(n + 1)] - ret[1:(n)]
# end

function getDistr(type, mean, stdDev)

	if debug println("getDistr") end

	n = 200

	if type == "Exponential"
		#distr = Exponential(1 / mean)
		ret = cdf.(Exponential( mean), 0:n)
	elseif type == "Weibull"
		shape = (stdDev / mean)^(-1.086)
		scale = mean / gamma( 1 + 1 / shape)

		#distr = Weibull(shape, scale)
		ret = cdf.(Weibull(shape, scale), 0:n)
	elseif type == "Gamma"
		shape = mean^2 / stdDev^2
		scale = stdDev^2 / mean

		#distr = Gamma(shape, scale)
		ret = cdf.(Gamma(shape, scale), 0:n)
	else
		error("Distribution not recognized")
	end

	#ret = cdf(distr, 0:n)

	ret[2:(n + 1)] - ret[1:(n)]
end

function pullPatients(nPat, pL, day, args)

	nnPat::Int64 = rand(Binomial(nPat, 1 - pL))

	ActualTimes = DataFrame(tAH = Int64[], lGW = Int64[], lIC = Int64[], lSD = Int64[],
		directIC = Int64[], admIC = Int64[], admSD = Int64[], type = String[],
		day = Int64[], patID = Int64[])

	if nnPat > 0

		Distr = map(x -> sample(1:length(x), Weights(x), nnPat), args[1]) |> DataFrame

		Probs = map(p -> sample([0, 1], Weights([1 - p, p]), nnPat), args[2]) |> DataFrame

		Probs.admSD = Probs.admSD .* Probs.admIC

		ActualTimes = hcat(Distr, Probs) |>
		df -> @transform(
			df,
			type = "R",
			day = day,
			lGW = ifelse.(:directIC .== 1, 0, :lGW),
			patID = 0
		)

	end

	ActualTimes
end

function createPatientsVaryAdmRateLOSbased(incidence::Vector{Int64}, pLv, pN, pI,
		pGW2IC, pIC2SD, alphaH, losGW, losIC, losSD, startOffset::Int64 = 1)

	if debug println("createPatientsVaryAdmRateLOSbased") end

	#incidence = round.(Int64, incidence)

	if (pN + pI) != 1 error("Normal and ICU parts != 1") end

	args = [
		(tAH = alphaH, lGW = losGW, lIC = losIC, lSD = losSD),
		(directIC = pI, admIC = pGW2IC, admSD = pIC2SD)
	]

	AllActualTimes = DataFrame(tAH = Int64[], lGW = Int64[], lIC = Int64[], lSD = Int64[],
		directIC = Int64[], admIC = Int64[], admSD = Int64[], type = String[],
		day = Int64[], patID = Int64[])

	## TODO: The following loop could be made faster by working at the column level or by preallocating the DataFrame
	if debug println("pullPatients") end
	@inbounds for i in startOffset:length(incidence)
		if incidence[i] != 0
			#if i > length(pLv)
				 NewDay = pullPatients(incidence[i], 1 - pLv[minimum([i,length(pLv)])], i, args)
			#else
			#	 NewDay = pullPatients(incidence[i], 1 - pLv[i], i, args)
			#end
		# else
		# 	NewDay = DataFrame()
		append!(AllActualTimes, NewDay)

		end
	end

	if debug
		# println(first(AllActualTimes, 10))
		# println(freqtable(AllActualTimes.type))
		# println(names(AllActualTimes))
	end

	# What if AllActualTimes is zero? should we exit?
	if nrow(AllActualTimes) == 0
		return (HospChangeEvents = DataFrame(time = Int64[], event = Int64[], patID = Int64[]),
			IcuChangeEvents = DataFrame(time = Int64[], event = Int64[], patID = Int64[]),
			AllActualTimes = AllActualTimes)
	end


	AllActualTimes.patID = 1:nrow(AllActualTimes)

	IcuPats = AllActualTimes[(AllActualTimes.directIC .== 1) .| (AllActualTimes.admIC .== 1), :] # strangely, .| has higher priority than .==

	icuAdm = IcuPats.day .+ IcuPats.lGW .+ IcuPats.tAH .- 1
	icuDis = icuAdm .+ IcuPats.lIC

	IcuChangeEvents = DataFrame(
		time = vcat(icuAdm, icuDis),
		event = vcat(ones(Int64,length(icuAdm)), -ones(Int64,length(icuDis))),
		patID = vcat(IcuPats.patID, IcuPats.patID)
		) |> x -> sort(x, :time)

	###

	HospPats = AllActualTimes[AllActualTimes.directIC .== 0, :]

	hospAdm = HospPats.day .+ HospPats.tAH .- 1
	hospDis = hospAdm .+ HospPats.lGW

	HospChangeEvents = DataFrame(
		time = vcat(hospAdm, hospDis),
		event = vcat(ones(Int64,length(hospAdm)), -ones(Int64,length(hospDis))),
		patID = vcat(HospPats.patID, HospPats.patID)
		)

	###

	StepdownPats = AllActualTimes[AllActualTimes.admSD .== 1, :]
	stepDownAdm = StepdownPats.day .+ StepdownPats.lGW .+ StepdownPats.lIC .+ StepdownPats.tAH .- 1
	stepDownDis = stepDownAdm .+ StepdownPats.lSD

	HospChangeEventsSD = DataFrame(
		time = vcat(stepDownAdm, stepDownDis),
		event = vcat(ones(Int64,length(stepDownAdm)), -ones(Int64,length(stepDownDis))),
		patID = vcat(StepdownPats.patID, StepdownPats.patID)
		)

	HospChangeEvents = vcat(HospChangeEvents, HospChangeEventsSD) |>
		x -> sort(x, :time)

	return (HospChangeEvents = HospChangeEvents::DataFrame,
		IcuChangeEvents = IcuChangeEvents::DataFrame)
		#,
		#AllActualTimes = AllActualTimes::DataFrame)
end

function addPatients(oldDB, ReplacementPatients, numGW::Int64, numICU::Int64, startOffset::Int64 = 1)

	if debug println("addPatients") end

	Selected = (
		selectedICU	= sample(ReplacementPatients.presentAtICU, numICU),
		selectedGW	= sample(ReplacementPatients.presentAtGW, numGW)
	)

	args = (
		ICUICU = (
			selected = "selectedICU",
			patients = "IcuChangeEvents"
			),
		ICUGW = (
			selected = "selectedICU",
			patients = "HospChangeEvents"
			),
		GWICU = (
			selected = "selectedGW",
			patients = "IcuChangeEvents"
			),
		GWGW = (
			selected = "selectedGW",
			patients = "HospChangeEvents"
			)
	)

	Sets = map(args) do set
		selected = Selected[Symbol(set.selected)]
		data = ReplacementPatients[Symbol(set.patients)]

		which = map(x -> findall(data.patID .== x), selected)
		which = vcat(which...)

		data = data[which, :]
		data[data.time .> 50, :]
	end

	### The following code was compacted in the map() above

	# whichICUICU = map(x -> findall(ReplacementPatients.IcuChangeEvents.patID .== x), selectedICU)
	# whichICUICU = vcat(whichICUICU...)

	# selRecICUICU = ReplacementPatients.IcuChangeEvents[whichICUICU, :]
	# selRecICUICU = selRecICUICU[selRecICUICU.time .> 50, :]

	# whichICUGW	= map(x -> findall(ReplacementPatients.HospChangeEvents.patID .== x), selectedICU)
	# whichICUGW 	= vcat(whichICUGW...)

	# selRecICUGW	= ReplacementPatients.HospChangeEvents[whichICUGW, :]
	# selRecICUGW	= selRecICUGW[selRecICUGW.time .> 50, :]

	# whichGWICU	= map(x -> findall(ReplacementPatients.IcuChangeEvents.patID .== x), selectedGW)
	# whichGWICU 	= vcat(whichGWICU...)

	# selRecGWICU = ReplacementPatients.HospChangeEvents[whichGWICU, :]
	# selRecGWICU = selRecGWICU[selRecGWICU.time .> 50, :]

	# whichGWGW	= map(x -> findall(ReplacementPatients.HospChangeEvents.patID .== x), selectedGW)
	# whichGWGW 	= vcat(whichGWGW...)

	# selRecGWGW 	= ReplacementPatients.HospChangeEvents[whichGWGW, :]
	# selRecGWGW 	= selRecGWGW[selRecGWGW.time .> 50, :]

	allSelected = vcat(Selected.selectedICU, Selected.selectedGW)

	BothICUChangeEvents = vcat(Sets.ICUICU, Sets.GWICU)

	if numICU > 0
		BothICUChangeEvents = vcat(BothICUChangeEvents, DataFrame(
			time = 50,
			event = 1,
			patID = 1:numICU
			))
	end

	BothICUChangeEvents.time = BothICUChangeEvents.time .+ (startOffset - 50)

	IcuChangeEvents = vcat(oldDB.IcuChangeEvents, BothICUChangeEvents) |>
	x -> sort(x, :time)

	BothHospChangeEvents = vcat(Sets.ICUGW, Sets.GWGW)

	if numGW > 0
		BothHospChangeEvents = vcat(BothHospChangeEvents, DataFrame(
			time = 50,
			event = 1,
			patID = 1:numGW
			))
	end

	BothHospChangeEvents.time = BothHospChangeEvents.time .+ (startOffset - 50)

	HospChangeEvents = vcat(oldDB.HospChangeEvents, BothHospChangeEvents) |>
		x -> sort(x, :time)

	return (IcuChangeEvents = IcuChangeEvents, HospChangeEvents = HospChangeEvents)
end

function createReplacementPatients(propGW2IC::Float64, propIC2SD::Float64,
		losGW::Vector{Float64}, losIC::Vector{Float64}, losSD::Vector{Float64})

	if debug println("createReplacementPatients") end

	untilDate = Date(2020, 12, 31)
	numPatients = 400
	days = 100

	ReplacementPatients = createPatientsVaryAdmRateLOSbased(
		fill(numPatients, days),
		[1, 1],
		1, 0, propGW2IC, propGW2IC, [1, 0],
		losGW, losIC, losSD)

	AdmBeforeICU = ReplacementPatients.IcuChangeEvents[
		@with(ReplacementPatients.IcuChangeEvents, (:time .<= 50) .& (:event .== 1)), :]

	DisAfterICU = ReplacementPatients.IcuChangeEvents[
		@with(ReplacementPatients.IcuChangeEvents, (:time .> 50) .& (:event .== -1)), :]

    presentAtICU = DisAfterICU.patID[DisAfterICU.patID .∈ (AdmBeforeICU.patID, )]

    AdmBeforeGW = ReplacementPatients.HospChangeEvents[
		@with(ReplacementPatients.HospChangeEvents, (:time .<= 50) .& (:event .== 1)), :]

	DisAfterGW = ReplacementPatients.HospChangeEvents[
		@with(ReplacementPatients.HospChangeEvents, (:time .> 50) .& (:event .== -1)), :]

	presentAtGW = DisAfterGW.patID[(DisAfterGW.patID .∈ (AdmBeforeGW.patID, )) .& (DisAfterGW.patID .∉ (presentAtICU, ))]

	# if(debug::Bool) println(table(as.data.frame(table(ReplacementPatients$HospChangeEvents[
 #        (ReplacementPatients$HospChangeEvents$patID %in% ReplacementPatients$presentAtGW)&(ReplacementPatients$HospChangeEvents$time<=50),
 #        ]$patID))$Freq))

    return merge(ReplacementPatients, (presentAtICU = presentAtICU::Vector{Int64}, presentAtGW = presentAtGW::Vector{Int64}))
end

function createInHRunWSD(nRuns::Int64, incidenceDF, thisAdmProps, directICUProp,
		propGW2IC, propIC2SD, losGWT, losGWM, losGWS, losICT, losICM, losICS,
		losSDT, losSDM, losSDS, numGW, numICU, startOffset)

	if debug println("createInHRunWSD") end

	startOffset = round(Int64, startOffset)

	runs = 1:nRuns

	Replacements = createReplacementPatients(propGW2IC / 100, propIC2SD / 100,
			getDistr(losGWT, losGWM, losGWS),
			getDistr(losICT, losICM, losICS),
			getDistr(losSDT, losSDM, losSDS))

	if debug println("Begin createInHRunWSD loop") end

	res = tmap1(run -> begin

		if debug println("run: $run") end
		RawRun = createPatientsVaryAdmRateLOSbased(
			incidenceDF.underlying[incidenceDF.runNum .== run],
			thisAdmProps,
			1 - directICUProp,
			directICUProp,
			propGW2IC / 100,
			propIC2SD / 100,
			[1, 0],
			getDistr(losGWT, losGWM, losGWS),
			getDistr(losICT, losICM, losICS),
			getDistr(losSDT, losSDM, losSDS),
			startOffset + 1)

		addPatients(RawRun, Replacements, numGW,
		numICU, startOffset)
	end, runs)


	res2=extractRunInfo(res)
	GC.gc()
	res2
end


function extractRunInfo(runResults)
	if debug println("extractRunInfo") end

	measuredTLlens = @inbounds map(1:length(runResults)) do i
		vcat(runResults[i].IcuChangeEvents.time, runResults[i].HospChangeEvents.time)::Vector{Int64} |>
			maximum
	end

	thisTLsdf = map(1:length(runResults)) do i

		res = DataFrame(
			ICU = getTimeLineFromCE(runResults[i].IcuChangeEvents, maximum(measuredTLlens)).absolute,
			GW = getTimeLineFromCE(runResults[i].HospChangeEvents, maximum(measuredTLlens)).absolute,
			runNum = i)

		res.time = 1:nrow(res)

		res
	end

	thisTLsdf = vcat(thisTLsdf...)

    groupby(thisTLsdf, :time) |>
	    df -> combine(df,
	        nrow => :n,
	        # :ICU .=> [x -> quantile(x, q) for q in [0.05, 0.25, 0.5, 0.75, 0.95]] .=>
			# 	[:ICUQ05, :ICUQ25, :ICUQ50, :ICUQ75, :ICUQ95], # More verbose, for older DataFrames versions
			:ICU => (x -> [quantile(x, [0.05, 0.25, 0.5, 0.75, 0.95])]) =>
				[:ICUQ05, :ICUQ25, :ICUQ50, :ICUQ75, :ICUQ95],
	        :ICU => mean => :ICUmean,
			:GW => (x -> [quantile(x, [0.05, 0.25, 0.5, 0.75, 0.95])]) =>
				[:GWQ05, :GWQ25, :GWQ50, :GWQ75, :GWQ95],
	        # :GW .=> [x -> quantile(x, q) for q in [0.05, 0.25, 0.5, 0.75, 0.95]] .=>
			# 	[:GWQ05, :GWQ25, :GWQ50, :GWQ75, :GWQ95], # More verbose, for older DataFrames versions
	        :GW => mean => :GWmean
	    )

end

function getTimeLineFromCE(ce, tlLength::Int64)
	if debug println("getTimeLineFromCE") end

	## We don't use the function with tlLength as NULL for now
    # if(is.null(tlLength)){
    #   tempDF<-data.frame(time=1:max(ce$time)+1,event=0,patID=0)
    #   tempCE<-rbind(ce,tempDF)
    # } else {
    #   tempCE<-rbind(ce[ce$time<=tlLength,],data.frame(time=1:tlLength,event=0,patID=0))
    # }

    tempCE = vcat(
        ce[ce.time .<= tlLength, :],
        DataFrame(time = 1:tlLength, event = 0, patID = 0),
    )

    # tempCE |> # Uglier but faster
    #   df -> sort(df, :time) |>
    # 	df -> df[df.time .> 0, :] |>
    # 	df -> groupby(df, :time) |>
    # 	df -> combine(df, nrow => :n, :event => sum => :changes) |>
    # 	df -> transform(df, :changes => cumsum => :absolute)

    @linq tempCE |> # DataFrameMeta version, slower but nicer
          orderby(:time) |>
          where(:time .> 0) |>
          groupby(:time) |>
          combine(n = length(:time), changes = sum(:event)) |> #
          transform(absolute = cumsum(:changes))
end

end
