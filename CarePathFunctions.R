
getPropDirect<-function(startSimDate,admRateEsti,withinHospPara,DEBUG=FALSE){
  if(withinHospPara$propManual==TRUE) {
    thisProp <- withinHospPara$propDirects
  } else {
    
    if(is.na(admRateEsti[admRateEsti$Date==startSimDate,"Normal"])){
      print(paste0("No info available for this date "))  
      thisProp<-NA
    } else {
      expectedICU<-admRateEsti[admRateEsti$Date==startSimDate,"Hosp"]*(admRateEsti[admRateEsti$Date==startSimDate,"admPressICU"]/admRateEsti[admRateEsti$Date==startSimDate,"admPress"])
      surplusICU<-admRateEsti[admRateEsti$Date==startSimDate,"ICU"]-expectedICU
      thisProp<-surplusICU/admRateEsti[admRateEsti$Date==startSimDate,"Normal"]
      if(length(thisProp)>1)thisProp<-thisProp[length(thisProp)]
      if(thisProp>1)thisProp<-1
      if(thisProp<0)thisProp<-0
    }
  }
  return(thisProp)
}


getParameters_depr<-function(inci,mrres,startSimDate,runLength,numberOfRuns,withinHospPara,bedObs,GWbeds,ICUbeds,admRateEsti,replacements,thisParaVersion,losDistr=NULL,DEBUG=FALSE){

  endDate <- startSimDate #war im Code als as.Date("2020-04-05")
  directICUProp<-getPropDirect(startSimDate,admRateEsti,withinHospPara)
  
  numRuns<-numberOfRuns
  startOffset=as.numeric(mrres[[1,"currentDay"]]-min(mrres[,"Date"]))#-100 #The -100 is for double checking if the estimated admission rate & proportion direct to ICU get to the correct initial values

  thisAdmProps<-admRateEsti[admRateEsti$Date==startSimDate,"admProps"]

  args <- list(
    thisParaVersion=thisParaVersion,
    numRuns = as.integer(numRuns),
    mrres = mrres[mrres$Date<startSimDate+runLength,c("runNum","underlying")],
    observedBeds <- bedObs,
    thisAdmProps = thisAdmProps,
    directICUProp = directICUProp,
    propGW2IC = withinHospPara$propGW2IC,
    propIC2SD = withinHospPara$propIC2SD,
    losGWT = withinHospPara$losGWT,
    losGWM = withinHospPara$losGWM,
    losGWS = withinHospPara$losGWS,
    losICT = withinHospPara$losICT,
    losICM = withinHospPara$losICM,
    losICS = withinHospPara$losICS,
    losSDT = withinHospPara$losSDT,
    losSDM = withinHospPara$losSDM,
    losSDS = withinHospPara$losSDS,
    replacements = (replacements),
    numGW = as.integer(GWbeds),
    numICU = as.integer(ICUbeds),
    startOffset = startOffset,
    tzero=startSimDate-startOffset
  )
  return(args)
}


getTimeLineFromCE<-function(ce,tlLength=NULL,DEBUG=FALSE){
  if(is.null(tlLength)){
    tempDF<-data.frame(Date=1:max(ce$Date)+1,event=0,patID=0)
    tempCE<-rbind(ce,tempDF)
  } else {
    tempCE<-rbind(ce[ce$Date<=tlLength,],data.frame(Date=1:tlLength,event=0,patID=0))
  }
  tempCE<-tempCE[order(tempCE$Date),]
  tempCE<-tempCE[tempCE$Date>0,]
  timelineTemp<-tempCE %>%
    group_by(Date) %>%
    dplyr::summarise(
      n=n(),
      changes=sum(event)
    )
  
  timelineTemp <- mutate(timelineTemp,
                         absolute=cumsum(changes)
  )
  return(timelineTemp)
}



getLOSdistr<-function(withinHospsPara,DEBUG=FALSE){
  numPatients <- 10000
  #untilDate <- max(obsData()$Date)
  untilDate<-as.Date("2021-01-01")
  directICUProp <- 0 #input$directICUval
  if(DEBUG) {
    print("Recreating LOS distribution")
    print(withinHospsPara$propGW2IC/100)
    print(withinHospsPara$propIC2SD/100)
  }
  mypatsTemp<-createPatientsVaryAdmRateLOSbased(
    c(numPatients,0),                  #incidence
    c(1,1,1),                          # proportion admission
    (1-directICUProp),                 # proportion of admission to normal ward
    (directICUProp),                   # proportion of admission direct to ICU
    withinHospsPara$propGW2IC/100, # proportion of admission direct to ICU
    withinHospsPara$propIC2SD/100, # proportion of admission direct to ICU
    c(1,0), #Distribution time to admission
    getDistr(withinHospsPara$losGWT,withinHospsPara$losGWM,withinHospsPara$losGWS),#losGW,  # LOS Distribution for GW
    getDistr(withinHospsPara$losICT,withinHospsPara$losICM,withinHospsPara$losICS),#losIC,  # LOS Distribution for IC
    getDistr(withinHospsPara$losSDT,withinHospsPara$losSDM,withinHospsPara$losSDS),#losSD,  # LOS Distribution for S
    startOffset=1)
  
  timelineTemp<-getTimeLineFromCE(rbind(mypatsTemp$hospChangeEvents,mypatsTemp$ICUChangeEvents))
  resLOSdistr<-timelineTemp$absolute/numPatients
  
  timelineTempICU<-getTimeLineFromCE(
    rbind(data.frame(
      Date=1:max(mypatsTemp$hospChangeEvents$Date),
      event=0,
      patID=-1),
      mypatsTemp$ICUChangeEvents))
  resLOSdistrICU<-timelineTempICU$absolute/numPatients
  
  timelineTempGW<-getTimeLineFromCE(
    rbind(data.frame(
      Date=1:max(c(mypatsTemp$ICUChangeEvents$Date,1)),
      event=0,
      patID=-1),
      mypatsTemp$hospChangeEvents))
  resLOSdistrGW<-timelineTempGW$absolute/numPatients
  
  return(data.frame(
    Days=1:length(resLOSdistrGW),
    Normal=resLOSdistrGW,
    ICU=resLOSdistrICU,
    Hosp=resLOSdistr)
  )
}


createPatientsVaryAdmRateLOSbased<-function(
  incidence,
  pLv,    # proportion admission
  pN,     # proportion of admission to normal ward
  pI,     # proportion of admission direct to ICU
  pGW2IC, # proportion of admission direct to ICU
  pIC2SD, # proportion of admission direct to ICU
  alphaH, #Distribution time to admission
  losGW,  # LOS Distribution for GW
  losIC,  # LOS Distribution for IC
  losSD,  # LOS Distribution for SD
  startOffset=1,
  DEBUG=FALSE){
  
  incidence<-round(incidence)
  if((pN+pI)!=1) stop("Normal and ICU parts != 1")
  
  pullPatients<-function(nPat,pL,day){
    nnPat<-rbinom(1,nPat,1-pL)
    if(nnPat>0){
      tAH<-sample(1:(length(alphaH)),nnPat,prob = alphaH,replace = T)
      lGW<-sample(1:(length(losGW)),nnPat,prob = losGW,replace = T)
      lIC<-sample(1:(length(losIC)),nnPat,prob = losIC,replace = T)
      lSD<-sample(1:(length(losSD)),nnPat,prob = losSD,replace = T)
      directIC<-sample(c(0,1),nnPat,prob = c(1-pI,pI),replace = T)
      admIC<-sample(c(0,1),nnPat,prob = c(1-pGW2IC,pGW2IC),replace = T)
      admSD<-sample(c(0,1),nnPat,prob = c(1-pIC2SD,pIC2SD),replace = T)
      admSD<-admSD*admIC
      
      actualTimes<-data.frame(tAH,lGW,lIC,lSD,directIC,admIC,admSD,type="R") #R for "real" record
      actualTimes$day<-day
      actualTimes[(actualTimes$directIC==1),"lGW"]<-0
      
    } else {
      actualTimes<-(data.frame(tAH=0,lGW=0,lIC=0,lSD=0,directIC=0,admIC=0,admSD=0,type="D",day=day))
    }
    return(actualTimes)
  }
  #if(DEBUG) print(paste0("PLV is ",pLv))
  allActualTimes<-Reduce(rbind,lapply(startOffset:length(incidence),
                                      function(x){
                                        if(incidence[x]!=0)
                                        {
                                          if(x>length(pLv)){
                                            return(pullPatients(incidence[x],1-pLv[length(pLv)],x))
                                          } else {
                                            if(x<=length(pLv)) return(pullPatients(incidence[x],1-pLv[x],x))
                                            
                                          }
                                        } else {
                                          return(actualTimes<-(data.frame(tAH=0,lGW=0,lIC=0,lSD=0,directIC=0,admIC=0,admSD=0,type="D",day=x)))
                                        }
                                      }
  )
  )
  
  allActualTimes<-allActualTimes[allActualTimes$type!="D",]
  if(nrow(allActualTimes)>0) allActualTimes$patID<-1:nrow(allActualTimes)
  
  hospPats<-allActualTimes[(allActualTimes$directIC==0),]
  #admission is current day plus time to hospital admission:
  hospadm<-hospPats[,"day"]+hospPats[,"tAH"]-1
  hospdis<-hospadm+hospPats[,"lGW"]
  
  icuPats<-allActualTimes[(allActualTimes$directIC==1)|(allActualTimes$admIC==1),]
  ICUadm<-icuPats[,"day"]+icuPats[,"lGW"]+icuPats[,"tAH"]-1
  ICUdis<-ICUadm+icuPats[,"lIC"]
  
  stepdownPats<-allActualTimes[allActualTimes$admSD==1,]
  stepDownAdm<-stepdownPats[,"day"]+stepdownPats[,"lGW"]+stepdownPats[,"lIC"]+stepdownPats[,"tAH"]-1
  stepDownDis<-stepDownAdm+stepdownPats[,"lSD"]
  
  ICUChangeEvents<-data.frame(Date=c(ICUadm,ICUdis),
                              event=c(rep(1,length(ICUadm)),rep(-1,length(ICUdis))),
                              patID=c(icuPats$patID,icuPats$patID)
  )
  ICUChangeEvents<-ICUChangeEvents[order(ICUChangeEvents$Date),]
  HospChangeEvents<-data.frame(Date=c(hospadm,hospdis),
                               event=c(rep(1,length(hospadm)),rep(-1,length(hospdis))),
                               patID=c(hospPats$patID,hospPats$patID))
  HospChangeEventsSD<-data.frame(Date=c(stepDownAdm,stepDownDis),
                                 event=c(rep(1,length(stepDownAdm)),rep(-1,length(stepDownDis))),
                                 patID=c(stepdownPats$patID,stepdownPats$patID))
  
  HospChangeEvents<-rbind(HospChangeEvents,HospChangeEventsSD)
  HospChangeEvents<-HospChangeEvents[order(HospChangeEvents$Date),]
  
  return(list(hospChangeEvents=HospChangeEvents,
              ICUChangeEvents=ICUChangeEvents,
              allActualTimes=allActualTimes)
  )
}

createPatientsVaryAdmRate<-function(incidence,pLv,pN,pI,alphaH,alphaI,alphaD,deltaH,deltaI,deltaD,pHosp=NULL,startOffset=1,DEBUG=FALSE){
  incidence<-round(incidence)
  if((pN+pI)!=1) stop("Normal and ICU parts != 1")
  pullPatients<-function(nPat,pL,day){
    nnPat<-rbinom(1,nPat,1-pL)
    if(nnPat>0){
      tAH<-sample(1:(length(alphaH)),nnPat,prob = alphaH,replace = T)
      tAI<-sample(1:(length(alphaI)),nnPat,prob = alphaI,replace = T)
      tAD<-sample(1:(length(alphaD)),nnPat,prob = alphaD,replace = T)
      tDH<-sample(1:(length(deltaH)),nnPat,prob = deltaH,replace = T)
      tDI<-sample(1:(length(deltaI)),nnPat,prob = deltaI,replace = T)
      tDD<-sample(1:(length(deltaD)),nnPat,prob = deltaD,replace = T)
      
      timeDF<-data.frame(tAH,tAI,tAD,tDH,tDI,tDD,hospStay<-1,ICUstay<-1,stepStay<-1,type<-1)
      
      pType<-       # H I D H I D H I D
        sample(list(c(0,0,0,0,0,0,0,0,0,"L"),       #Stay at home (low-symptomatic)
                    c(1,1,1,1,1,1,1,1,1,"H"),       #First to hospital
                    c(1,0,1,0,1,1,1,1,1,"I")        #Immediate ICU admission
        ),
        #nPat,prob=c(pL, pN*(1-pL), pI*(1-pL)),replace = T
        nnPat,prob=c(0, pN, pI),replace = T
        )
      hospChoice<-rep(1,nnPat)
      
      typeDF<-as.data.frame(t(matrix(unlist((pType)),nrow=10)))
      typeDF2<-sapply(typeDF[,1:9],function(x)as.numeric(as.character(x)))
      actualTimes<-(timeDF[,1:9]*typeDF2)
      colnames(actualTimes)<-c("tAH","tAI","tAD","tDH","tDI","tDD","hospStay","ICUstay","stepStay")
      actualTimes$type<-typeDF[,10]
      actualTimes$hosp<-hospChoice
      actualTimes$day<-day
    } else {
      actualTimes<-(data.frame(tAH=1,tAI=1,tAD=1,tDH=1,tDI=1,tDD=1,hospStay=1,ICUstay=1,stepStay=1,type="L",hosp=-1,day=day))
    }
    return(actualTimes)
  }
  #if(DEBUG) print(paste0("PLV is ",pLv))
  allActualTimes<-Reduce(rbind,lapply(startOffset:length(incidence),
                                      function(x){
                                        if(incidence[x]!=0)
                                        {
                                          if(x>length(pLv)){
                                            return(pullPatients(incidence[x],1-pLv[length(pLv)],x))
                                          } else {
                                            if(x<=length(pLv)) return(pullPatients(incidence[x],1-pLv[x],x))
                                            
                                          }
                                        }
                                        else {
                                          return(data.frame(tAH=1,tAI=1,tAD=1,tDH=1,tDI=1,tDD=1,hospStay=1,ICUstay=1,stepStay=1,type="L",hosp=-1,day=x))
                                        }
                                      }
  )
  )
  
  allActualTimes<-allActualTimes[allActualTimes$hosp!=-1,]
  if(nrow(allActualTimes)>0) allActualTimes$patID<-1:nrow(allActualTimes)
  hospPats<-allActualTimes[(allActualTimes$type=="H"),]
  #End-of-stay is discharge from hospital:
  hospPats$endStay<-hospPats$tDH
  #Unless admission to ICu comes earlier:
  hospPats[hospPats$tAI<hospPats$tDH,"endStay"]<-hospPats[hospPats$tAI<hospPats$tDH,"tAI"]
  #admission is current day plus time to hospital admission:
  hospadm<-hospPats[,"day"]+hospPats[,"tAH"]-1
  hospdis<-hospadm+hospPats[,"endStay"]
  
  icuPats<-allActualTimes[(allActualTimes$type=="I")|
                            ((allActualTimes$type=="H")&(allActualTimes$tAI<allActualTimes$tDH)),]
  icuPats$endStay<-icuPats$tDI
  icuPats[icuPats$tAD<icuPats$tDI,"endStay"]<-icuPats[icuPats$tAD<icuPats$tDI,"tAD"]
  
  ICUadm<-icuPats[,"day"]+icuPats[,"tAH"]+icuPats[,"tAI"]-1
  ICUdis<-ICUadm+icuPats[,"endStay"]
  
  stepdownPats<-allActualTimes[
    ((allActualTimes$type=="I")&(allActualTimes$tAD<allActualTimes$tDI))|
      ((allActualTimes$type=="H")&(allActualTimes$tAD<allActualTimes$tDI)&(allActualTimes$tAI<allActualTimes$tDH)),]
  
  stepDownAdm<-stepdownPats[,"day"]+stepdownPats[,"tAH"]+stepdownPats[,"tAI"]+stepdownPats[,"tAD"]-1
  stepDownDis<-stepDownAdm+stepdownPats[,"tDD"]
  
  ICUChangeEvents<-data.frame(Date=c(ICUadm,ICUdis),
                              event=c(rep(1,length(ICUadm)),rep(-1,length(ICUdis))),
                              patID=c(icuPats$patID,icuPats$patID)
  )
  ICUChangeEvents<-ICUChangeEvents[order(ICUChangeEvents$Date),]
  HospChangeEvents<-data.frame(Date=c(hospadm,hospdis),
                               event=c(rep(1,length(hospadm)),rep(-1,length(hospdis))),
                               patID=c(hospPats$patID,hospPats$patID))
  HospChangeEventsSD<-data.frame(Date=c(stepDownAdm,stepDownDis),
                                 event=c(rep(1,length(stepDownAdm)),rep(-1,length(stepDownDis))),
                                 patID=c(stepdownPats$patID,stepdownPats$patID))
  HospChangeEvents<-rbind(HospChangeEvents,HospChangeEventsSD)
  HospChangeEvents<-HospChangeEvents[order(HospChangeEvents$Date),]
  return(list(hospChangeEvents=HospChangeEvents,
              ICUChangeEvents=ICUChangeEvents,
              allActualTimes=allActualTimes)
  )
}


admRateEstimator <- function(inciDF,withinHospsPara=NULL,obsData=NULL,losDistr=NULL,DEBUG=FALSE){
  inciDF$Cases<-inciDF$reportedEpi
  #inciDF is a data.frame with (at least) following variables:
  # - Date : date of reporting
  # - all  : number of re <- ed cases, in all age classes
  # inciDF <- inci()
  #bedObservations is a data.frame with columns:
  # - Date : date of observation
  # - ICU  : ICU beds occupied
  # - GW   : General ward beds occupied
  # - Hosp : All occupied beds in the hospital (GW+ICU)
  if(is.null(obsData)) stop("No no. Need at least one observation")
  bedObservations <- obsData
  inciDF<-inciDF[inciDF$runNum==1,]
  if(is.null(losDistr)){
    #if(is.null(withinHospsPara)) 
    stop("No no. Go back")
    #theDistr<-getLOSdistr(withinHospsPara)#$Hosp
  } else {
    theDistr<-losDistr
  }
  
  runLength=sum(inciDF$Date>inciDF$currentDay)
  
  theDistr$deltaHosp<-c(-diff(theDistr$Hosp),0)

  # calculate the proportion admitted among all reported cases in the reversed LOS distribution for each "observed" day.
  pressureCalculator<-function(ddDate){
    thisObsDate=which(inciDF$Date==ddDate)
    calcLen<-min(thisObsDate-1,nrow(theDistr))
    totalAdmPressureHosp=sum(inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"Cases"]*theDistr[calcLen:1,"Hosp"])
    totalAdmPressureICU=sum(inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"Cases"]*theDistr[calcLen:1,"ICU"])
    totalAdmPressureNormal=sum(inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"Cases"]*theDistr[calcLen:1,"Normal"])
    
    return(list(
      totalAdmPressure=totalAdmPressureHosp,
      totalAdmPressureICU=totalAdmPressureICU,
      totalAdmPressureNormal=totalAdmPressureNormal
    )
    )
  }

  #Begin calculating a week after the first case was reported.
  inclRecs<-(inciDF$Date>(min(inciDF$Date)+6))&(inciDF$Date<=inciDF$currentDay)
  admPressList=(lapply(inciDF[inclRecs,"Date"],function(x)pressureCalculator(x)))

  resDF<-data.frame(
    Date=inciDF[inclRecs,"Date"],
    admPress=unlist(lapply(1:sum(inclRecs),function(x)admPressList[[x]][["totalAdmPressure"]])),
    admPressICU=unlist(lapply(1:sum(inclRecs),function(x)admPressList[[x]][["totalAdmPressureICU"]])),
    admPressNormal=unlist(lapply(1:sum(inclRecs),function(x)admPressList[[x]][["totalAdmPressureNormal"]]))
  )
  resDFpost<-data.frame(
    Date=inciDF[[max(which(inclRecs)),"Date"]]+(1:runLength),
    admPress=NA,
    admPressICU=NA,
    admPressNormal=NA
    # cases=0,
    # cLastweek=0,
    # thisPop=inciDF[[1,"Pop"]],
    # RKIcut=0
  )
  # 
  
  if(DEBUG){
  print(paste0("Length pre-AdmProp: ",nrow(resDFpre)))
  print(paste0("Length AdmProp: ",nrow(resDF)))
  print(paste0("Length post-AdmProp: ",nrow(resDFpost)))
  }
  resDF=rbind(resDF,resDFpost)
  resDF=merge(resDF,bedObservations,by="Date",all=T)

  resDF$admProps<-resDF$Hosp/resDF$admPress
  resDF$admPropsICU<-resDF$ICU/resDF$admPressICU
  resDF$admPropsNormal<-resDF$Normal/resDF$admPressNormal

  resDF$allAvAR<-(unlist(lapply(resDF$Date,function(x)sum(resDF[resDF$Date<=x,"Hosp"],na.rm = T)/sum(resDF[resDF$Date<=x,"admPress"],na.rm = T))))
  
  calcThese<-resDF[!is.na(resDF$Hosp),]
  resDF[!is.na(resDF$Hosp),"movAvAR"]<-(unlist(lapply(calcThese$Date,function(x)
    sum(calcThese[(calcThese$Date<=(x+15))&(calcThese$Date>=(x-15)),"Hosp"],na.rm=T)/
      sum(calcThese[(calcThese$Date<=(x+15))&(calcThese$Date>=(x-15)),"admPress"],na.rm = T)
  )))
  
  resDF[is.na(resDF$Hosp),"movAvAR"]<-NA
  resDF$movAvAR<-extraInterPolate(resDF$movAvAR)
  resDF$admProps<-extraInterPolate(resDF$admProps) #Note that here, we extrapolate the last admission proportion forward in time

  resDF$admPropsNormal<-extraInterPolate(resDF$admPropsNormal)
  resDF$admPropsICU<-extraInterPolate(resDF$admPropsICU)
  
  resDF[resDF$admProps>1,"admProps"]<-1
  resDF[resDF$admPropsICU>1,"admPropsICU"]<-1

  resDF[resDF$admPropsNormal>1,"admPropsNormal"]<-1
  
  resDF$movAvWeightAR<-(unlist(lapply(resDF$Date,function(x)
    sum(resDF[(resDF$Date<=(x+15))&(resDF$Date>=(x-15)),"Hosp"],na.rm=F)/
      sum(resDF[(resDF$Date<=(x+15))&(resDF$Date>=(x-15)),"admPress"],na.rm = F)
  )))
  
  resDF$admPropsVacc<-resDF$admProps
  
  wPropReinfCalculator<-function(ddDate){
    thisObsDate=which(inciDF$Date==ddDate)
    thisObsDateResDF=which(resDF$Date==ddDate)
    
    calcLen<-min(thisObsDate-1,nrow(theDistr))
    totalAdmPressureHosp=sum(inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"Cases"]*theDistr[calcLen:1,"deltaHosp"])
    wPropReinf=sum(inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"Cases"]*theDistr[calcLen:1,"deltaHosp"]*inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"propReinf"])/totalAdmPressureHosp

    primaryHrisk<-resDF[thisObsDateResDF,"Hosp"]/
      sum(inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"Cases"]*
            theDistr[calcLen:1,"deltaHosp"]*
            ((1-inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"propReinf"])+
               (inciDF[((thisObsDate-calcLen)):(thisObsDate-1),"propReinf"]*0.1))
          )
   
    return(list(
      totalAdmPressure=totalAdmPressureHosp,
      wPropReinf=wPropReinf,
      primaryHrisk=primaryHrisk
    )
    )
  }
  
  
  inclRecs<-(inciDF$Date>(min(inciDF$Date)+6))
  reinfList=(lapply(inciDF[inclRecs,"Date"],function(x)wPropReinfCalculator(x)))
  
  resDFReinf<-data.frame(
    Date=inciDF[inclRecs,"Date"],
    wPropReinf=unlist(lapply(1:sum(inclRecs),function(x)reinfList[[x]][["wPropReinf"]])),
    propReinf=inciDF[inclRecs,"propReinf"]
  )
  resDF=merge(resDF,resDFReinf,by="Date",all=T)

  return(resDF)
}


carePathRunner<-function(args,IUKCovid,DEBUG=FALSE){
      Sys.setenv("JULIACONNECTOR_SERVER" = paste0("localhost:", juliaPort)) # Necessary since Julia was started after plan(multissession) (S. Lenz is smart indeed!)
      start <- Sys.time()
      args$mrres$runNum<-as.integer(args$mrres$runNum)
      args$mrres$underlying<-as.integer(args$mrres$underlying)
      res <- as.data.frame(
        with(args, {IUKCovid$createInHRunWSD(
          numRuns,
          mrres,
          thisAdmProps, directICUProp,
          propGW2IC, propIC2SD,
          losGWT, losGWM, losGWS,
          losICT,  losICM, losICS,
          losSDT, losSDM, losSDS,
          numGW, numICU, as.integer(startOffset)
        )
        })
      ) 
  return(res)
}


createReplacementPatients<-function(withinHospPara,DEBUG=FALSE){
  untilDate=as.Date("2020-12-31")
  numPatients=400
  days=100
  replacementPatients<-createPatientsVaryAdmRateLOSbased(
    rep(numPatients,days),
    c(1,1),
    (1),
    0,
    withinHospPara$propGW2IC/100, # proportion of admission direct to ICU
    withinHospPara$propIC2SD/100, # proportion of admission direct to ICU
    c(1,0),#pexp(1:100,1/3)-pexp(0:99,1/3),
    getDistr(withinHospPara$losGWT,withinHospPara$losGWM,withinHospPara$losGWS),
    getDistr(withinHospPara$losICT,withinHospPara$losICM,withinHospPara$losICS),
    getDistr(withinHospPara$losSDT,withinHospPara$losSDM,withinHospPara$losSDS)
  )
  
  admBeforeICU<-replacementPatients$ICUChangeEvent[
    (replacementPatients$ICUChangeEvent$Date<=50&
       replacementPatients$ICUChangeEvent$event==1),]
  
  disAfterICU<-replacementPatients$ICUChangeEvent[
    (replacementPatients$ICUChangeEvent$Date>50&
       replacementPatients$ICUChangeEvent$event==-1),]
  
  replacementPatients$presentAtICU<-disAfterICU[disAfterICU$patID%in%admBeforeICU$patID,"patID"]
  
  admBeforeGW<-replacementPatients$hospChangeEvent[
    (replacementPatients$hospChangeEvent$Date<=50&
       replacementPatients$hospChangeEvent$event==1),]
  disAfterGW<-replacementPatients$hospChangeEvent[
    (replacementPatients$hospChangeEvent$Date>50&
       replacementPatients$hospChangeEvent$event==-1),]
  
  replacementPatients$presentAtGW<-
    disAfterGW[(disAfterGW$patID%in%admBeforeGW$patID)&
                 (!(disAfterGW$patID%in%replacementPatients$presentAtICU))
               ,"patID"]
  
  if(DEBUG) print(table(as.data.frame(table(replacementPatients$hospChangeEvent[
    (replacementPatients$hospChangeEvent$patID %in% replacementPatients$presentAtGW)&(replacementPatients$hospChangeEvent$Date<=50),
  ]$patID))$Freq))
  return(replacementPatients)
}