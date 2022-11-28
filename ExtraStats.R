removeNegatives<-function(cCases,forward=FALSE,DEBUG=FALSE){
  if(forward) {
    return(unlist(lapply(1:length(cCases),function(x) max(cCases[1:x]))))
  } else {
    return(unlist(lapply(1:length(cCases),function(x) min(cCases[x:length(cCases)]))))
  }
}

getIncCases<-function(cCases,DEBUG=FALSE){
  cCases<-removeNegatives(cCases)
  cCases-c(0,cCases[-length(cCases)])
}

getMoveAv<-function(l,w,DEBUG=FALSE){
  starts<-1:(length(l)-w)
  return(c(rep(0,floor(w)),unlist(lapply(starts,function(x)mean(l[x:(x+w)])))))
  return(c(rep(0,floor(w/2)),unlist(lapply(starts,function(x)mean(l[x:(x+w)])))))#this doesn't allow for "current" data use
}

getMoveAv2<-function(l,w,startVal,DEBUG=FALSE){
  starts<-1:(length(l)-w)
  initialGroup<-unlist(lapply(1:(w-1),function(x)mean(c(rep(startVal,w-x),l[1:x]))))
  
  return(c(initialGroup,unlist(lapply(starts,function(x)mean(l[x:(x+w)])))))
  #return(c(rep(0,floor(w/2)),unlist(lapply(starts,function(x)mean(l[x:(x+w)])))))#this doesn't allow for "current" data use
}

convexhull <- function(x){
  for(i in 1:length(x)){
    if (x[i] < 0) {
      value <- x[i]
      x[i]<-0
      j <- 0
      while (value < 0 && (i+j)<length(x)) {
        j <- j + 1
        if (!is.na(x[i+j]) ) {
          value <- value + x[i+j]
          x[i+j] <- max(0,value)
        }
      }
    }
  }
  return(x)
}

getWeibullDistr<-function(distrMean,distrStDev,distrLength=200,DEBUG=FALSE){
  myShape=(distrStDev/distrMean)^(-1.086)
  myScale=(distrMean)/gamma(1+1/myShape)
  if(DEBUG) print(paste0("Match weibull distribution to shape=",myShape," and scale=",myScale))
  
  return(
    pweibull(1:distrLength,shape=myShape,scale=myScale)-
      pweibull(0:(distrLength-1),shape=myShape,scale=myScale)
  )
}
getGammaDistr<-function(distrMean,distrStDev,distrLength=200,DEBUG=FALSE){
  alpha=((distrMean^2)/(distrStDev^2))
  beta=(distrMean)/(distrStDev^2)
  
  if(DEBUG) print(paste0("Match gamma distribution to shape=",alpha," and rate=",beta))
  
  return(
    pgamma(1:distrLength,alpha,beta)-
      pgamma(0:(distrLength-1),alpha,beta)
  )
}
getExponentialDistr<-function(distrMean,distrStDev,distrLength=200,DEBUG=FALSE){
  myRate=(1/distrMean)
  if(DEBUG) print(paste0("Exponential distribution with shape=",myRate))
  return(
    pexp(1:distrLength,rate=myRate)-
      pexp(0:(distrLength-1),rate=myRate)
  )
}

getDistr<-function(distrType,distrMean,distrStDev,DEBUG=FALSE){
  if(DEBUG) print(distrType)
  if(distrType=="Weibull") {return(getWeibullDistr(distrMean,distrStDev))}
  if(distrType=="Exponential") {return(getExponentialDistr(distrMean))}
  if(distrType=="Gamma") {return(getGammaDistr(distrMean,distrStDev))}
}


extraInterPolate<-function(x,DEBUG=FALSE){
  lenX<-length(x)
  #set first and last element as first and last known (!is.na) element
  if(is.na(x[1]))x[1]<-x[!is.na(x)][1]
  if(is.na(x[lenX]))x[lenX]<-x[!is.na(x)][sum(!is.na(x))]
  
  #Then, interpolate
  unknowns<-(which(is.na(x)))
  knowns<-(which(!is.na(x)))
  begins<-unlist(lapply(unknowns,function(y)(max(knowns[knowns<y]))))
  ends<-unlist(lapply(unknowns,function(y)(min(knowns[knowns>y]))))
  distright<-ends-unknowns
  distleft<-unknowns-begins
  interVals<-((x[begins]*distright)+(x[ends]*distleft))/(distright+distleft)
  x[unknowns]<-interVals
  return(x)
}