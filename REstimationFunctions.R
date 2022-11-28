
variantProp<-function(thisDate,refDate,advan,refProp,serialInt=6.6,DEBUG=FALSE){#NOTE: advan is now given as the relative R, no longer the relative increase in R
  refDateVars<-refDate #as.Date("2021-02-01")+7
  advantage<-advan#0.895
  startProp<-refProp#0.1182
  
  if(DEBUG) print(paste0(refDateVars," : ", advantage," : ",startProp))
  px<-function(t,px0,a){
    1/(1+(((1-px0)/px0)*exp(-(log(a)/serialInt)*t)))
  }
  relativeT<-as.numeric(thisDate-refDateVars)
  return(px(relativeT,startProp,advantage))
}

splitRbyVariant<-function(inputR,inputDate,eachDaysVOCprop,susPop,susPopVOC,susPopNonVOC,advanV,popSize=10,DEBUG=FALSE){
  #inputDF is DF with R and Date.
  
  if(length(inputDate)<length(eachDaysVOCprop)){
    inputDate<-c(inputDate,rep(NA,length(eachDaysVOCprop)-length(inputDate)))
    inputR<-c(inputR,rep(NA,length(eachDaysVOCprop)-length(inputR)))
  }
  baseR0single<-inputR/((susPop/popSize))
  baseR0<-inputR/((((susPopNonVOC/popSize))*(1-eachDaysVOCprop))+(advanV*eachDaysVOCprop*((susPopVOC/popSize))))
  nonVOCRt<-baseR0*((susPopNonVOC/popSize))
  VOCRt<-baseR0*((susPopVOC/popSize))*(advanV)
  
  resDF<-(data.frame(Date=inputDate,
                     propVOC=eachDaysVOCprop,
                     nonVOC=nonVOCRt,
                     VOC=VOCRt,#This one is off...
                     combi=inputR,
                     baseR0single=baseR0single,
                     baseR0multi=baseR0))
  
  return(resDF)
}

constructRdev<-function(histoR,fittedETS,fittedETS2,startDate,currentDay,runLength,eachDaysVOCprop,advanV,immEvAdv,DEBUG=FALSE){
  subHistoR<-histoR[histoR$Date<=currentDay,]
  
  #The "naive" R0
  mu=subHistoR[[nrow(subHistoR),"R_mean_nc"]] # mean
  sigma=subHistoR[[nrow(subHistoR),"R_Std_nc"]]
  alpha=(mu^2)/(sigma^2)
  beta=mu/(sigma^2)
  avRe<-rgamma(1,alpha,beta)
  
  if(DEBUG) print(paste0("Picked an R of ", avRe))
  
  devRdf<-data.frame(
    Date=c(subHistoR$Date,max(subHistoR$Date)+((1:runLength))),
    devR0=avRe,
    RTstatic=avRe
  ) 
  devRdf<-merge(devRdf,histoR,by="Date",all=T)
  
  startRef=which(devRdf$Date==currentDay)[1]
  
  devRdf$R0Static<-devRdf$RTstatic/(devRdf[startRef,"susPop"]/devRdf[startRef,"Pop"])
  devRdf$R0StaticVOC<-devRdf$RTstatic/(((1-devRdf[startRef,"propVOC_nc"])*(devRdf[startRef,"susPopNonVOC"]/devRdf[startRef,"Pop"]))+
                                         (devRdf[startRef,"propVOC_nc"]*(devRdf[startRef,"susPopVOC"]/devRdf[startRef,"Pop"])*advanV))
  
  avReVOC<-avRe/((1-devRdf[startRef,"propVOC_nc"])+((advanV*immEvAdv)*devRdf[startRef,"propVOC_nc"]))
  devRdf$devRTVOC<-(devRdf$R0StaticVOC*(((1-devRdf$propVOC_nc)*(devRdf$susPopNonVOC/devRdf$Pop))+
                                          ((devRdf$susPopVOC/devRdf$Pop)*advanV*devRdf$propVOC_nc)))
  
  initial_seed=as.integer(runif(1,min=0,max=1000000))
  set.seed(initial_seed)
  levelTrack<-runif(1,min=0.01,max=99.9)
  columnTrack<-1+ceiling(runif(1,min=0.5,max=1.5))
  
  ensmblForecast<-as.data.frame(forecast(fittedETS,h=runLength+1,level=levelTrack))[-1,columnTrack]
  etsTemp<-c(subHistoR$base_R0single_nc,exp(ensmblForecast))
  if(length(etsTemp)<nrow(devRdf)){
    etsTemp<-c(etsTemp,rep(NA,nrow(devRdf)-length(etsTemp)))
  }
  devRdf$devR0ets<-etsTemp#c(subHistoR$base_R0single_nc,exp(ensmblForecast))
  devRdf$devRTets<-devRdf$devR0ets*(devRdf$susPop/devRdf$Pop)
  
  ensmblForecast2<-as.data.frame(forecast(fittedETS2,h=runLength+1,level=levelTrack))[-1,columnTrack]
  ensmblForecast2Corrected<-c(subHistoR$base_R0multi_nc,exp(ensmblForecast2)) 
  if(nrow(devRdf)>length(ensmblForecast2Corrected)){
    ensmblForecast2Corrected<-c(ensmblForecast2Corrected,rep(NA,nrow(devRdf)-length(ensmblForecast2Corrected))) 
  }
  devRTetsVOC<-(ensmblForecast2Corrected*(1-devRdf$propVOC_nc)*(devRdf$susPopNonVOC/devRdf$Pop))+
    (ensmblForecast2Corrected*devRdf$propVOC_nc*(advanV)*(devRdf$susPopVOC/devRdf$Pop))
  
  devRdf$devRTetsVOC<-devRTetsVOC
  
  if(length(ensmblForecast2Corrected)<nrow(devRdf)){
    ensmblForecast2Corrected<-c(ensmblForecast2Corrected,rep(NA,nrow(devRdf)-length(ensmblForecast2Corrected)))
  }
  devRdf$devR0backgroundETS<-ensmblForecast2Corrected
  devRdf$devR0background<-avReVOC
  
  return(devRdf)
}

fitETS<-function(Rseries,etsAlpha=0.25,etsBeta=0.15,etsLength=100,DEBUG=FALSE){
  fitR <- ets(log(tail(Rseries,n=etsLength)),alpha=etsAlpha,beta=etsBeta,model="AAN",use.initial.values=F)
  return(fitR)

}

getRdevCurves<-function(histoR,startSimDate,numberOfRuns,simLength,startVOCdate,advanVOC,immEvAdv,startVOCprop,serialInt=6.6,
                        etsAlpha=0.25,etsBeta=0.15,etsLength=100,DEBUG=FALSE){

  # This function can be used to adjust the R0 development in the future.
  startDataDate=min(histoR$Date)#-2 #Is this still correct? TODO

  eachDaysVOCprop<-unlist(lapply(1:(as.numeric(difftime(startSimDate,startDataDate)+simLength+1)),function(x) variantProp(startDataDate+x-1,
                                                startVOCdate,
                                                advanVOC*immEvAdv,
                                                startVOCprop/100,
                                                serialInt=serialInt)))

  etsFit<-fitETS(histoR[histoR$Date<=(startSimDate+1),"base_R0single_nc"],etsAlpha=etsAlpha,etsBeta=etsBeta,etsLength=etsLength)# Fit the ETS model on the mean nowcast.
 #etsFit<-fitETS(histoR[histoR$Date<=startSimDate,"combi_nc"]) # Fit the ETS model on the mean nowcast.
  etsFit2<-fitETS(histoR[histoR$Date<=(startSimDate+1),"base_R0multi_nc"],etsAlpha=etsAlpha,etsBeta=etsBeta,etsLength=etsLength) # Fit the ETS model on the mean nowcast.
  #etsFit2<-fitETS(histoR[histoR$Date<=startSimDate,"nonVOC_nc"]) # Fit the ETS model on the mean nowcast.
  
  devR0s<-Reduce(rbind,lapply(1:numberOfRuns,function(x){
    devRdf<-constructRdev(histoR,etsFit,etsFit2,startDataDate,startSimDate,simLength,eachDaysVOCprop,advanVOC,immEvAdv)
    devRdf$runNum=x
    if(length(eachDaysVOCprop)<nrow(devRdf)){
      eachDaysVOCprop<-c(eachDaysVOCprop,rep(NA,nrow(devRdf)-length(eachDaysVOCprop)))
    }
    devRdf$devVOC=eachDaysVOCprop
    return(devRdf)
  })
  )
  tempDevR0s<<-devR0s
  return(devR0s)
}

cumsumRMNA<-function(x,settoval=0){
  x[is.na(x)]<-settoval
  return(cumsum(x))
}

get_Historical_R<-function(inci,nowcast,vaccTS,SerialIntervalDistr,startVOCdate,advanVOC,ImmEvasionAdvantage,
                           startVOCprop,epimethod="non_parametric_si",serialInt=6.6,crossIm=1){
  
  t_start <- seq(2, nrow(inci)-6)   
  t_end <- t_start + 6  
  
  res<-suppressWarnings(estimate_R(
    data.frame(
      I=inci$Cases,
      dates=inci$Date
    ),
    method = epimethod,
    si_data = NULL,
    si_sample = NULL,
    config = make_config(
      list(
        si_distr =  c(0,SerialIntervalDistr),
        t_start = t_start, 
        t_end = t_end
      )
    )
  ))
  
  popSize<-max(inci$Pop)
  erres<-as.data.frame(res$R)
  mncDate<-res$dates
  colnames(erres)<-c("t_start_raw","t_end_raw","R_mean_raw","R_Std_raw","R_Q025_raw","R_Q05_raw","R_Q25_raw","R_Q50_raw","R_Q75_raw","R_Q95_raw","R_Q975_raw")
  erresDate<-(res$dates)
  erres$Date<-erresDate[(length(erresDate)-nrow(erres)):(length(erresDate)-1)]+1

  erresNC<-as.data.frame(nowcast$ncMean$rEstimate$R)
  colnames(erresNC)<-c("t_start_nc","t_end_nc","R_mean_nc","R_Std_nc","R_Q025_nc","R_Q05_nc","R_Q25_nc","R_Q50_nc","R_Q75_nc","R_Q95_nc","R_Q975_nc")
  erres<-cbind(erres,erresNC)
  
  startDataDate=min(inci$Date)-2

  eachDaysVOCpropNC<-unlist(lapply(0:(length(nowcast$ncMean$epiCurveNowcast$dates)-1),function(x) 
    variantProp(min(nowcast$ncMean$epiCurveNowcast$dates)+x,
      startVOCdate,
      advanVOC*ImmEvasionAdvantage,
      startVOCprop/100,
      serialInt=serialInt)))

  epicurves=data.frame(
              Date=nowcast$ncMean$epiCurveNowcast$dates,
              propVOC3=eachDaysVOCpropNC,
              epicurveRaw=inci$Cases,
              epicurveNC=nowcast$ncMean$epiCurveNowcast$I,
              epiCurveNonVOCnc=round(nowcast$ncMean$epiCurveNowcast$I*(1-eachDaysVOCpropNC)),
              epiCurveVOCnc=round(nowcast$ncMean$epiCurveNowcast$I*(eachDaysVOCpropNC)),
              epiCurveNonVOC=round(inci$Cases*(1-eachDaysVOCpropNC)),
              epiCurveVOC=round(inci$Cases*(eachDaysVOCpropNC))
               )
  erres<-merge(erres,epicurves,by="Date",all=T)
  erres<-merge(erres,vaccTS,by="Date",all=T)
  erres$MGAA<-1
  erres$InfedOrVaxxed<-1-((1-(cumsumRMNA(erres$epicurveNC)/popSize))*(1-erres$peopleFirstProp))
  erres$InfedOrVaxxedNonVOC<-1-((1-(cumsumRMNA(erres$epiCurveNonVOCnc)/popSize))*(1-erres$peopleFirstProp))
  erres$InfedOrVaxxedVOC<-(cumsumRMNA(erres$epiCurveVOCnc)/popSize)
  
  erres$immune<-popSize-round((popSize-cumsumRMNA(erres$epicurveNC))*(1-erres$devVaccProtect))
  erres$immuneNonVOC<-popSize-round((popSize-cumsumRMNA(erres$epiCurveNonVOCnc))*(1-erres$devVaccProtect))
  erres$immuneVOC<-cumsumRMNA(erres$epiCurveVOCnc)#for now, we assume there is no vaccination against a new variant. This will be the case until March 2022 in all likelihood
  
  erres$susPop<-round(popSize*(((1-(erres$immune/popSize)))))#"Random assignment", previously infected individuals also able to receive vaccine
  erres$susPopNonVOC<-round(popSize*(((1-(erres$immuneNonVOC/popSize))*(1-(crossIm*erres$immuneVOC/popSize)))))#"Random assignment", previously infected individuals also able to receive vaccine
  erres$susPopVOC<-round(popSize*(((1-(erres$immuneVOC/popSize))*(1-(crossIm*erres$immuneNonVOC/popSize)))))#"Random assignment", previously infected individuals also able to receive vaccine
    
  eachDaysVOCprop<-unlist(lapply(1:length(erres$R_mean_raw),function(x) variantProp(startDataDate+x,
                                                                                    startVOCdate,
                                                                                    advanVOC*ImmEvasionAdvantage,
                                                                                    startVOCprop/100,
                                                                                    serialInt=serialInt)))
  
  vocSplit<-splitRbyVariant(erres$R_mean_raw,
                            erres$Date,
                            eachDaysVOCprop,
                            erres$susPop,
                            erres$susPopVOC,
                            erres$susPopNonVOC,
                            advanVOC,
                            startVOCprop/100,
                            popSize=popSize)
  vocSplit<-vocSplit[!is.na(vocSplit$Date),]
  colnames(vocSplit)<-c("Date","propVOC_raw","nonVOC_raw","VOC_raw","combi_raw","base_R0single_raw","base_R0multi_raw")
  erres<-merge(erres,vocSplit,by="Date",all=T)
  
  vocSplit_nc<-splitRbyVariant(erres$R_mean_nc,
                               erres$Date,
                               eachDaysVOCprop,
                               erres$susPop,
                               erres$susPopVOC,
                               erres$susPopNonVOC,
                               advanVOC,
                               startVOCprop/100,
                               popSize=popSize)
  vocSplit_nc<-vocSplit_nc[!is.na(vocSplit_nc$Date),]
  colnames(vocSplit_nc)<-c("Date","propVOC_nc","nonVOC_nc","VOC_nc","combi_nc","base_R0single_nc","base_R0multi_nc")
  erres<-merge(erres,vocSplit_nc,by="Date",all=T)
  
  tempERRES<<-erres
  return(erres)
}
