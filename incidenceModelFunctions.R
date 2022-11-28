
extractWeeklyPattern<-function(epiCurve,DEBUG=FALSE){ #returns the weekly "rhythm" in reporting due to weekday/weekends testing differences and reporting.
  totals<-unlist(lapply(0:6,function(x) sum(epiCurve[((1:length(epiCurve))%%7==x)],na.rm = T)))
  props<-totals/mean(totals)
  return(props)
}

removeWeeklyPattern<-function(curve,wklyPattern,DEBUG=FALSE){
  correctingSubset<-1:length(curve)
  fullWeeks<-floor(length(correctingSubset)/7)
  daysLeft<-length(correctingSubset)-(fullWeeks*7)
  if(daysLeft==0){
    allPattern<-c(rep(wklyPattern,fullWeeks))
  } else {
    allPattern<-c(rep(wklyPattern,fullWeeks),wklyPattern[1:daysLeft])
  }
  return(round(curve/allPattern))
}

forwardInci<-function(runInput,
                      popSize,
                      serialDistr,
                      runLength,
                      VOCadv=0,
                      immAdv=1,
                      crossIm=1,
                      underreporting=1,
                      DEBUG=FALSE,
                      inclVOC=FALSE,
                      useETS=FALSE,
                      peopleFullProp=0){#remove
  #Dont use the VOC devBeta. Incorporate in the model here.
  startOffset=length(runInput[,"epiCurveNonVOCnc"])-runLength

  if(inclVOC){
    newCurve<-runInput[,"epiCurveNonVOCnc"]
    newCurve2<-runInput[,"epiCurveVOCnc"]
    susPop<-runInput[,"susPopNonVOC"]
    susPop2<-runInput[,"susPopVOC"]
    InfedOrVaxxed1<-runInput[,"InfedOrVaxxedNonVOC"]
    InfedOrVaxxed2<-runInput[,"InfedOrVaxxedVOC"]
    immune1<-runInput[,"immuneNonVOC"]
    immune2<-runInput[,"immuneVOC"]
    R0dev=runInput[,"R0StaticVOC"]      
    if(useETS){ 
      R0dev=runInput[,"devR0backgroundETS"]
    }
  } else {
    newCurve<-runInput[,"epicurveNC"]
    newCurve2<-rep(0,length(newCurve))
    susPop<-runInput[,"susPop"]
    susPop2<-rep(popSize,length(newCurve))
    InfedOrVaxxed1<-runInput[,"InfedOrVaxxed"]
    InfedOrVaxxed2<-rep(0,length(newCurve))
    immune1<-runInput[,"immune"]
    immune2<-rep(0,length(newCurve))
    R0dev=runInput[,"R0Static"]
    if(useETS){ 
      R0dev=runInput[,"devR0ets"]
    }
  }

  if(!inclVOC) {
    devVOC <-0
    VOCadv <-0
  } else {
    devVOC <-runInput$devVOC
  }
  
  newCurve[is.na(newCurve)]<-0
  newCurve2[is.na(newCurve2)]<-0
  
  reinfections1<-(1-(((1-InfedOrVaxxed1)*(1-InfedOrVaxxed2))/(1-(immune1/popSize))))*newCurve
  reinfections2<-(1-(((1-InfedOrVaxxed1)*(1-InfedOrVaxxed2))/(1-(immune2/popSize))))*newCurve2

  devBeta<-R0dev/(popSize)
  devBeta2<-(R0dev*(VOCadv))/(popSize)

  runDay<-function(d){ #,beta){
    if(d<length(serialDistr)){
      pressure<-rev(serialDistr[1:d])*newCurve[1:d]
      pressure2<-rev(serialDistr[1:d])*newCurve2[1:d]
      
    } else {
      pressure<-rev(serialDistr)*newCurve[(d-(length(serialDistr)-1)):(d)]
      pressure2<-rev(serialDistr)*newCurve2[(d-(length(serialDistr)-1)):(d)]
    }

    
    if(popSize-sum(newCurve[1:d])<0) stop("Susceptible population under 0, shouldn't happen")
    if(popSize-sum(newCurve2[1:d])<0) stop("Susceptible population 2 under 0, shouldn't happen")
    immune1[d+1]<-popSize-round((popSize-sum(newCurve[1:d]))*(1-runInput$devVaccProtect[d]))
    immune2[d+1]<-sum(newCurve2[1:d])#for now, we assume there is no vaccaination against a new variant. This will be the case until March 2022 in all likelihood
    
    susPop[d+1]<-round(popSize*(((1-(immune1[d+1]/popSize))*(1-(crossIm*immune2[d+1]/popSize)))))#"Random assignment", previously infected individuals also able to receive vaccine
    susPop2[d+1]<-round(popSize*(((1-(immune2[d+1]/popSize))*(1-(crossIm*immune1[d+1]/popSize)))))#"Random assignment", previously infected individuals also able to receive vaccine

    if(susPop[d+1]<0) susPop[d+1]<-0 # can happen with modelled vaccination
    if(susPop2[d+1]<0) susPop2[d+1]<-0 # can happen with modelled vaccination
    if(DEBUG) print(susPop)
    
    if(devBeta[d+1]>1)devBeta[d+1]<-1
    perPersonInfPres<-(1-((1-devBeta[d+1])^sum(pressure)))
    if(perPersonInfPres>1) stop(paste0("Infection pressure larger than 1: ", perPersonInfPres))
    newCurve[d+1]<-rbinom(1,susPop[d+1],perPersonInfPres)
    
    if(inclVOC){
      if(devBeta2[d+1]>1)devBeta2[d+1]<-1
      perPersonInfPres2<-(1-((1-devBeta2[d+1])^sum(pressure2)))
      if(perPersonInfPres2>1) stop(paste0("Infection pressure larger than 1: ", perPersonInfPres))
      newCurve2[d+1]<-rbinom(1,susPop2[d+1],perPersonInfPres2)
    } else {
      newCurve2[d+1]<-0
    }   
    
    InfedOrVaxxed1[d+1]<-1-((1-(sum(newCurve[1:d+1])/popSize))*(1-runInput$peopleFullProp[d+1]))
    InfedOrVaxxed2[d+1]<-sum(newCurve2[1:d+1])/popSize
    reinfections1[d+1]<-(1-(((1-InfedOrVaxxed1[d+1])*(1-InfedOrVaxxed2[d+1]))/(1-(immune1[d+1]/popSize))))*newCurve[d+1]
    reinfections2[d+1]<-(1-(((1-InfedOrVaxxed1[d+1])*(1-InfedOrVaxxed2[d+1]))/(1-(immune2[d+1]/popSize))))*newCurve2[d+1]

    return(list(
      newCurve=newCurve,
      newCurve2=newCurve2,
      reinfections1=reinfections1,
      reinfections2=reinfections2
    ))
  }

  for(it in ((startOffset):(startOffset+runLength-1))) {
    dayResults<-runDay(it)#,beta)
    newCurve<-dayResults$newCurve
    newCurve2<-dayResults$newCurve2
    reinfections1<-dayResults$reinfections1
    reinfections2<-dayResults$reinfections2
  }
  propReinfTemp=(reinfections1+reinfections2)/(newCurve+newCurve2)
  propReinfTemp<-extraInterPolate(propReinfTemp)
  return(
    data.frame(
      devR0=R0dev,
      recentR0=R0dev[1],
      underlying=newCurve+newCurve2,
      reportedEpi=newCurve+newCurve2,
      bgVariant=newCurve,
      vocVariant=newCurve2,
      reinfections1=reinfections1,
      reinfections2=reinfections2,
      propReinf=propReinfTemp
    )
  )
}

incidenceModel <- function(inci,
                           Rdevs,
                           vaccTimeSeries,
                           startSimDate,
                           simLength,
                           numberOfRuns,
                           serialinterval,
                           inclVOC=FALSE,
                           useETS=FALSE,
                           advanVOC=1,
                           ImmEvasionAdvantage=1,
                           crossIm=1,DEBUG=FALSE){
  
  population=inci[1,"Pop"]
  startDate=min(Rdevs$Date)
  
  wklyPattern<-extractWeeklyPattern(c(rep(0,6),Rdevs[Rdevs$runNum==1,"epicurveNC"]))
  
  mrResRaw<-lapply(1:numberOfRuns,function(x){

    runInput<-Rdevs[Rdevs$runNum==x,]
    runInput$smoothEpicurve<-removeWeeklyPattern(runInput$epicurveNC,extractWeeklyPattern(runInput$epicurveNC))
    runInput[runInput$Date>startSimDate,c("epicurveRaw","epicurveNC","epiCurveVOC","epiCurveNonVOC","epiCurveVOCnc","epiCurveNonVOCnc")]<-0 #Any future cases are set to zero
    runInput<-runInput[runInput$Date<=(startSimDate+simLength),]
    
    runres<-forwardInci(
                runInput=runInput,
                popSize=population,
                serialDistr=serialinterval,
                runLength=simLength,
                VOCadv = advanVOC,
                immAdv =ImmEvasionAdvantage,
                inclVOC=inclVOC,
                useETS=useETS,
                underreporting = 1,
                crossIm=crossIm)
                
    runres$Date<-runInput$Date #startDate+(1:nrow(runres))
    runres$ncMean <- runInput$epicurveNC
    correctingSubset<-((1):length(runres$reportedEpi))
    fullWeeks<-floor(length(correctingSubset)/7)
    daysLeft<-length(correctingSubset)-(fullWeeks*7)
    if(daysLeft==0){
      allPattern<-c(rep(wklyPattern,fullWeeks))
    } else {
      allPattern<-c(rep(wklyPattern,fullWeeks),wklyPattern[1:daysLeft])
    }
    runres$wklyReported<-c(round(runres$reportedEpi[correctingSubset]*allPattern))

    runres<-merge(runres,data.frame(Date=inci$Date,report=inci$Cases),by="Date",all=T)
    runres$runNum<-x
    runres$runPassed<-TRUE#(sum(runres$devR0)!=0)
    runres$currentDay<-startSimDate
    runres$startDate<-startDate
    runres$underlying<-runres$reportedEpi
    
    runres$time<-runres$Date #startDate+(1:nrow(runres))

    return(runres)
  })
  mrRes<-subset(Reduce(rbind,mrResRaw),runPassed==TRUE)
  
  return(mrRes)
}

