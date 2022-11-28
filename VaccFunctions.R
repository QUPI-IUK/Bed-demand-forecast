
vaccAdjustHospRisk<-function(propVaccinated,DEBUG=FALSE){
  
  #population risk of hospitalization as per Gesundheitsamt Fr Br
  riskDF<-data.frame(
    risk=c(0.02247191011236,0.01,
           0.018093556928508,0.033044420368364,
           0.051803278688525,0.076649746192893,
           0.15028901734104,0.374358974358974,
           0.42566510172144,0.330275229357798),
    popProp=c(0.094064428344652,0.094661208120979,
              0.111920079252354,0.12976379456453,
              0.122256304978337,0.160987312461955,
              0.127042478784479,0.089170834178771,
              0.058412804506881,0.011720754807061)
  )
  riskDF$cpopprop<-rev(cumsum(rev(riskDF$popProp)))
  riskDF$vacGroup<-riskDF$cpopprop-propVaccinated
  if(DEBUG)print(riskDF)
  if(DEBUG)print(paste0("average risk is ",sum(riskDF$risk*riskDF$popProp)))
  
  riskDFvac<-riskDF[riskDF$vacGroup>0,]
  riskDFvac[nrow(riskDFvac),"popProp"]<-riskDFvac[nrow(riskDFvac),"vacGroup"]
  riskDFvac$nonVacPopProp<-riskDFvac$popProp/sum(riskDFvac$popProp)
  
  if(DEBUG) print(riskDFvac)
  if(DEBUG)print(paste0("average after vaccination risk is ",sum(riskDFvac$risk*riskDFvac$nonVacPopProp)))
  return(sum(riskDFvac$risk*riskDFvac$nonVacPopProp))
  
}

vaccProtectionTransm<-function(vaccTime,protectDistr,DEBUG=FALSE){
  #vaccTime is a time series of daily vaccinated people.
  
  if(length(vaccTime)>length(protectDistr)){
    protectDistr<-c(protectDistr,rep(protectDistr[length(protectDistr)],length(vaccTime)-length(protectDistr)))
  } else {
    protectDistr<-protectDistr[1:length(vaccTime)]
  }
  totalProtection<-unlist(lapply(1:length(vaccTime),function(x){
    return(sum(vaccTime[1:x]*protectDistr[x:1]))
  }))
  return(totalProtection)
}

vaccProtectionTransm2<-function(vaccTime1,vaccTime2,protectDistr1,protectDistr2,DEBUG=FALSE){
  #vaccTime is a time series of daily vaccinated people.
  if(length(vaccTime1)!=length(vaccTime2))stop("1st and 2nd vaccination timelines should have the same length")
  
  if(length(vaccTime1)>length(protectDistr1)){
    protectDistr1<-c(protectDistr1,rep(protectDistr1[length(protectDistr1)],length(vaccTime1)-length(protectDistr1)))
  } else {
    protectDistr1<-protectDistr1[1:length(vaccTime1)]
  }
  if(length(vaccTime2)>length(protectDistr2)){
    protectDistr2<-c(protectDistr2,rep(protectDistr2[length(protectDistr2)],length(vaccTime2)-length(protectDistr2)))
  } else {
    protectDistr2<-protectDistr2[1:length(vaccTime2)]
  }
  
  totalProtection<-unlist(lapply(1:length(vaccTime1),function(x){
    prot1<-(sum(vaccTime1[1:x]*protectDistr1[x:1]))
    prot2<-(sum(vaccTime2[1:x]*protectDistr2[x:1]))
    return(prot1+prot2)
  }))
  return(totalProtection)
}

vaccProtectionTransm3<-function(vaccTime1,vaccTime2,vaccTime3,protectDistr1,protectDistr2,protectDistr3,DEBUG=FALSE){
  #vaccTime is a time series of daily vaccinated people.
  if(length(vaccTime1)!=length(vaccTime2))stop("1st and 2nd vaccination timelines should have the same length")
  
  if(length(vaccTime1)>length(protectDistr1)){
    protectDistr1<-c(protectDistr1,rep(protectDistr1[length(protectDistr1)],length(vaccTime1)-length(protectDistr1)))
  } else {
    protectDistr1<-protectDistr1[1:length(vaccTime1)]
  }
  if(length(vaccTime2)>length(protectDistr2)){
    protectDistr2<-c(protectDistr2,rep(protectDistr2[length(protectDistr2)],length(vaccTime2)-length(protectDistr2)))
  } else {
    protectDistr2<-protectDistr2[1:length(vaccTime2)]
  }
  if(length(vaccTime3)>length(protectDistr3)){
    protectDistr3<-c(protectDistr3,rep(protectDistr3[length(protectDistr3)],length(vaccTime3)-length(protectDistr3)))
  } else {
    protectDistr3<-protectDistr3[1:length(vaccTime3)]
  }
  
  totalProtection<-unlist(lapply(1:length(vaccTime1),function(x){
    prot1<-(sum(vaccTime1[1:x]*protectDistr1[x:1]))
    prot2<-(sum(vaccTime2[1:x]*protectDistr2[x:1]))
    prot3<-(sum(vaccTime3[1:x]*protectDistr3[x:1]))
    return(prot1+prot2+prot3)
  }))
  return(totalProtection)
}

getVaccDelay<-function(vaccTable,DistrictsMulti,districtPops,startSimDate,DEBUG=FALSE){##TODO
  
  selDistrictIDs<-getDistrictCodes(DistrictsMulti,districtPops)
  selectedPops<-vaccTable[vaccTable$idDistrict %in% selDistrictIDs,]
  
  localVaccData<-
    selectedPops%>%
    group_by(Date)%>%
    summarise(vaccinationsTotalDistrict=sum(vaccinationsTotalDistrict),
              peopleFirstTotalDistrict=sum(peopleFirstTotalDistrict),
              peopleFullTotalDistrict=sum(peopleFullTotalDistrict)
    )%>%
    as.data.frame()
  if(DEBUG) print(localVaccData)
  localVaccData<-localVaccData[(localVaccData$Date<=startSimDate),]
  
  ret<-unlist(lapply(0:7, function(x){
    thisDaysTotalFirst<-sum(localVaccData[localVaccData$Date==(startSimDate-x),"peopleFullTotalDistrict"])
    thisDelay<-(sum(localVaccData$peopleFirstTotalDistrict>thisDaysTotalFirst)-x)
    return(thisDelay)
  }
  ))
  if(DEBUG)print(ret)
  if(DEBUG)print(mean(ret))
  return(mean(ret))
}

getVaccTable <- function(dataLoc,districtPops,DEBUG=FALSE){
  if(DEBUG) print("Getting vaccination data (Stage: Populations)")
  pops<-districtPops
  
  # pops$idState<-substr(pops$idDistrict,1,2)
  vaccByState<-downloadVaccInfo(dataLoc,districtPops)
  
  # pops$idState<-substr(pops$idDistrict,1,2) #This should move up to the districtPops() function
  rownames(pops)<-pops$idDistrict
  pops<-
    pops%>%
    group_by(idState)%>%
    mutate(
      propPopOfState=Pop/sum(Pop)
    )
  
  vaccByDistrict<-vaccByState %>%
    group_by(Date) %>%
    full_join(pops,by=c("code"="idState"))%>%
    mutate(
      vaccinationsTotalDistrict=round(vaccinationsTotal*propPopOfState),
      peopleFirstTotalDistrict=round(peopleFirstTotal*propPopOfState),
      peopleFullTotalDistrict=round(peopleFullTotal*propPopOfState),
      peopleBoosterTotalDistrict=round(peopleBoosterTotal*propPopOfState)
    )%>%
    as.data.frame()

  if(DEBUG){
    print(vaccByDistrict)
    print(selectedPops)
  }
  return(vaccByDistrict)
}

getVaccTimeSeries<-function(inci,vaccTable,districtPops,startSimDate,vaccPop,vaccPopB,vaccDelayB,simLength,DistrictsMulti,
                            protDelay1T="Gamma",protDelay1M=15,protDelay1S=3.8,protEffac1=25,
                            protDelay2T="Gamma",protDelay2M=15,protDelay2S=6.5,protEffac2=75,
                            protDelay3T="Gamma",protDelay3M=7,protDelay3S=3.8,protEffac3=85,DEBUG=FALSE
                            
){
  currentDay=startSimDate
  incRKI <- inci
  population=incRKI[1,5]
  startDate=min(incRKI[,"Date"])-2
  dosesPerDay=min(vaccPop,population)
  dosesPerDayBooster=min(vaccPopB,population)
  runLength=simLength
  
  allLength<-(as.numeric(difftime(currentDay,startDate)))
  protectDistr1=cumsum(getDistr(protDelay1T,protDelay1M,protDelay1S))*(protEffac1/100)
  protectDistr2=cumsum(getDistr(protDelay2T,protDelay2M,protDelay2S))*((protEffac2-protEffac1)/100)
  protectDistr3=cumsum(getDistr(protDelay3T,protDelay3M,protDelay3S))*((protEffac3-protEffac2)/100)
  
  #protectDistr=(cumsum((getGammaDistr(15,3.8,distrLength=80)*0.65)+(getGammaDistr(40,6.5,distrLength=80)*.10)))
  
  selDistrictIDs<-getDistrictCodes(DistrictsMulti,districtPops)
  selectedPops<-vaccTable[vaccTable$idDistrict %in% selDistrictIDs,]
  if(DEBUG) print(paste0("Nr of records: ",sum(vaccTable$idDistrict %in% selDistrictIDs)))
  TSdf<-selectedPops%>%
    group_by(Date)%>%
    summarise(Pop=sum(Pop),
              peopleFirstTotal=sum(peopleFirstTotalDistrict),
              peopleFullTotal=sum(peopleFullTotalDistrict),
              peopleBoosterTotal=sum(peopleBoosterTotalDistrict)
    )%>%as.data.frame()
  #Insert missing dates
  TSdf<-merge(data.frame(Date=min(TSdf$Date)+(0:as.numeric(max(TSdf$Date)-min(TSdf$Date)))),TSdf,by="Date",all=T)

  TSdf$Pop<-extraInterPolate(TSdf$Pop)
  TSdf$peopleFirstTotal<-round(extraInterPolate(TSdf$peopleFirstTotal))
  TSdf$peopleFullTotal<-round(extraInterPolate(TSdf$peopleFullTotal))
  TSdf$peopleBoosterTotal<-round(extraInterPolate(TSdf$peopleBoosterTotal))
  
  TSdf$peopleBoosterTotal[is.na(TSdf$peopleBoosterTotal)]<-0 #replace NA with 0 for the Booster
  
  TSdf<-TSdf[order(TSdf$Date),] #make sure it's ordered correctly
  TSdf$peopleFirstDaily<-pmax(c(0,(TSdf[2:(nrow(TSdf)),"peopleFirstTotal"]-TSdf[1:(nrow(TSdf)-1),"peopleFirstTotal"])),0)
  TSdf$peopleFullDaily<-pmax(c(0,(TSdf[2:(nrow(TSdf)),"peopleFullTotal"]-TSdf[1:(nrow(TSdf)-1),"peopleFullTotal"])),0)
  TSdf$peopleBoosterDaily<-pmax(c(0,(TSdf[2:(nrow(TSdf)),"peopleBoosterTotal"]-TSdf[1:(nrow(TSdf)-1),"peopleBoosterTotal"])),0)
  if(DEBUG)print(TSdf$peopleFirstDaily)
  if(DEBUG)print(TSdf$peopleFullDaily)
  if(DEBUG)print(TSdf$peopleBoosterDaily)
  # Remove the known future
  TSdf<-TSdf[TSdf$Date<=currentDay,]
  
  
  ###### calculate vaccine-produced protection ######
  
  if(nrow(TSdf)==0){
    vaccTimeLength<-allLength+1
  } else {
    vaccTimeLength<-as.numeric(TSdf[[1,"Date"]]-startDate)
  }
  vaccTime<-TSdf$peopleFirstDaily
  startOffset<-vaccTimeLength+length(vaccTime)
  
  vaccForecast<-c(rep(0,vaccTimeLength),vaccTime,rep(dosesPerDay,runLength))
  vaccForecast<-diff(pmin(c(0,cumsum(vaccForecast)),population)) #Make sure the cumulative number of doses doesn't exceed the population size
  
  ######################################
  allDosesPerDay=min(dosesPerDay,population)
  vdel<-round(getVaccDelay(vaccTable,DistrictsMulti,districtPops,startSimDate))
  if(vdel<=0) vdel=46
  if(DEBUG) print(paste0("Working with ",allDosesPerDay," total doses per day"))
  if(DEBUG) print(paste0("With a delay of ",vdel," days between doses"))
  
  vaccForecastF<-c(TSdf$peopleFullTotal,cumsum(vaccForecast)[1+startOffset+(1:runLength)-vdel])
  
  for(ii in 2:length(vaccForecastF)) vaccForecastF[ii]<-max(vaccForecastF[ii],vaccForecastF[ii-1])
  vaccForecastFdaily<-c(rep(0,vaccTimeLength),diff(c(0,vaccForecastF)))
  
  ######Forecast for Booster
  vaccTimeB<-TSdf$peopleBoosterDaily
  
  vaccForecastBoosterDaily<-c(rep(0,vaccTimeLength),vaccTimeB,rep(dosesPerDayBooster,runLength))
  vaccForecastBoosterDaily<-diff(pmin(c(0,cumsum(vaccForecastBoosterDaily)),population))
  vaccForecastBoosterDaily<-diff(pmin(c(0,cumsum(vaccForecastBoosterDaily)),c(0,rep(0,vaccDelayB),cumsum(vaccForecastFdaily))[1:(length(vaccForecastBoosterDaily)+1)])) 
  
  
  ######################################
  
  #Adding columns and rows:
  alignedPast=data.frame(
    Date=startDate+(0:(vaccTimeLength-1)),
    Pop=population,#TSdf[1,"Pop"],
    peopleBoosterDaily=0,
    peopleFullDaily=0,
    peopleFirstDaily=0,
    peopleBoosterTotal=0,
    peopleFullTotal=0,
    peopleFirstTotal=0
  )
  alignedFuture=data.frame(
    Date=currentDay+(1:(runLength)),
    Pop=population,#TSdf[1,"Pop"],
    peopleBoosterDaily=0,
    peopleFullDaily=0,
    peopleFirstDaily=0,
    peopleBoosterTotal=0,
    peopleFullTotal=0,
    peopleFirstTotal=0
  )
  TSdf<-rbind(rbind(alignedPast,TSdf),alignedFuture)
  TSdf<-TSdf[order(TSdf$Date),] #just to be on the safe side
  
  TSdf$vaccForecast<-vaccForecast #vaccForecast1
  TSdf$vaccFullForecast<-vaccForecastFdaily
  TSdf$vaccBoosterForecast<-vaccForecastBoosterDaily
  TSdf$devVaccProtect<-pmin(vaccProtectionTransm3(vaccForecast,vaccForecastFdaily,vaccForecastBoosterDaily,protectDistr1,protectDistr2,protectDistr3)/population,1)#change this one
  TSdf$peopleBoosterProp<-pmin(cumsum(TSdf$vaccBoosterForecast)/population,1)
  TSdf$peopleFullProp<-pmin(cumsum(TSdf$vaccFullForecast)/population,1)
  TSdf$peopleFirstProp<-pmin(cumsum(TSdf$vaccForecast)/population,1)
 
  return(TSdf)
}