compensateRepDelay<-function(inci,meanResult=F,DEBUG=FALSE){
  repDelayDistr<-c(0.315395494,0.038019312,0.013125383,0.003831764,0.003309058,0.001599567,0.001361257)
  inci<-rev(inci)
  addCases<-unlist(lapply(1:7,function(x){
    if(DEBUG) print(paste0("Inc: ",inci[x]," prob: ",1-repDelayDistr[x]))
    if(inci[x]==0) {
      return(0)
    } else {
      if(meanResult){
        return(round(inci[x]*((repDelayDistr[x])/(1-repDelayDistr[x]))))
      } else {
        return(rnbinom(1,inci[x],1-repDelayDistr[x]))
      }
    }
  }))
  inci[1:7]<-inci[1:7]+addCases
  return(rev(inci))
}

getNowcast<-function(inci,numberOfRuns,SerialIntervalDistr,DEBUG=FALSE){
  epicurve <- inci$Cases
  nowcastedTime<-7
  nowcastedRunin<-31
  
  subRoutine<-function(x,meanResult = F,origCurve=NULL){
    incInput<-compensateRepDelay(epicurve,meanResult = meanResult)
    if(DEBUG) print("Using Cori estimation")
    

    inc <- data.frame(I=incInput,
                      dates=inci$Date
    )
    origInc<-inc
    if(!meanResult){
      inc<-inc[(nrow(inc)-(nowcastedTime+nowcastedRunin)):nrow(inc),]
    }
    
    if(DEBUG) print(inc)
    
    t_start <- seq(2, nrow(inc)-6)   
    t_end <- t_start + 6  
    epiconfig<-make_config(list(si_distr =  c(0,SerialIntervalDistr),
                                t_start = t_start, 
                                t_end = t_end
    ))
    
    suppressWarnings(
      epiresultTemp <- estimate_R(
        inc,
        method = "non_parametric_si",
        si_data = NULL,
        si_sample = NULL,
        config = epiconfig
      )
    )
    if(!meanResult){
      epiresultTemp2<-origCurve
      epiresultTemp2$R[(nrow(epiresultTemp2$R)-nowcastedTime):nrow(epiresultTemp2$R),]<-
        epiresultTemp$R[(nrow(epiresultTemp$R)-nowcastedTime):nrow(epiresultTemp$R),]
      
      epiresultTemp<-epiresultTemp2
    }
    return(
      list(
        epiCurveNowcast=origInc,
        rEstimate=epiresultTemp,
        run=x
      )
    )
  }
  
  numRuns=numberOfRuns
  meanNowcast=subRoutine(1,meanResult = T)
  
  theNowCasts<-lapply(1:numRuns,function(y){
    return(subRoutine(y,meanResult = F,origCurve=meanNowcast$rEstimate))
  })
  
  return(list(
    ncRuns=theNowCasts,
    ncMean=meanNowcast
  )
  )
}