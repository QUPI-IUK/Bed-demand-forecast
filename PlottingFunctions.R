
# Stolen from the \code{RmcdrPlugin.KMggplot2} (slightly modified by idaTools, and there stole by @TjibbeD again)
#https://github.com/adibender/ldatools/blob/master/R/geom_stepribbon.R

geom_stepribbon <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "identity",
  position    = "identity",
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomStepribbon,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, ... )
  )
}

GeomStepribbon <- ggproto(
  "GeomStepribbon", GeomRibbon,
  extra_params = c("na.rm"),
  draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
    if (na.rm) data <- data[complete.cases(data[c("x", "ymin", "ymax")]), ]
    data   <- rbind(data, data)
    data   <- data[order(data$x), ]
    data$x <- c(data$x[2:nrow(data)], NA)
    data   <- data[complete.cases(data["x"]), ]
    GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
  }
)

reportedIncidencePlot<-function(plotInciDF,nowcast,xrange,i18n=NULL,DEBUG=FALSE){
  plotWidth<-as.numeric(xrange[2]-xrange[1])
  
  ncMean_df<-data.frame(Date=nowcast$ncMean$epiCurveNowcast$dates,
                        ncMeanInci=nowcast$ncMean$epiCurveNowcast$I
                        )
  
  ncMeanSevenDays <- lapply(1:nrow(ncMean_df), function(i) {
    100000*(sum(ncMean_df$ncMeanInci[i:(i+6)])/plotInciDF$Pop[i])
  })
  
  ncMeanSevenDays <- c(rep(0,6),unlist(ncMeanSevenDays))
  ncMean_df$ncMeanSevenDays <- ncMeanSevenDays[!is.na(ncMeanSevenDays)]
  
  plotInciDF<-merge(plotInciDF,ncMean_df,by="Date",all=T)
  
  includeData<-(plotInciDF$Date>=xrange[1])&(plotInciDF$Date<=xrange[2])
  
  multiplier10=10^round(log(plotInciDF[1,"Pop"]/700000,base=10))
  multiplier102=multiplier10*(2^round(log(plotInciDF[1,"Pop"]/(700000*multiplier10),base=2)))
  
  part1=10^round(log(max(plotInciDF[includeData,"Cases"])/4,base=10))
  part2=5^round(log(max(plotInciDF[includeData,"Cases"])/(part1*4),base=5))
  part3=2^round(log(max(plotInciDF[includeData,"Cases"])/(part1*4*part2),base=2))
  breaksLeft<-0:6*part1*part2*part3
  breaksRight<-breaksLeft/multiplier102
  
  ggplot(plotInciDF[includeData,])+
    geom_step(aes(x=Date,y=Cases,color=i18n$t("Daily number of cases (RKI)")),direction="vh")+
    geom_step(aes(x=Date,y=ncMeanInci,color=i18n$t("Nowcast")), linetype="dashed",direction="vh")+
    geom_line(aes(x=Date,y=multiplier102*`SevenDaysInci`,color=i18n$t("7 day incidence / 100 000 ")))+
    geom_line(aes(x=Date,y=multiplier102*`ncMeanSevenDays`,color=i18n$t("7 day incidence / 100 000 (Nowcast)")), linetype="dashed")+
    (if(plotWidth<(75)){
      scale_x_date(limits=xrange, date_breaks = "1 week", date_labels =  "%d %b",date_minor_breaks="1 day")
    } else if(plotWidth<(2*100)) {
      scale_x_date(limits=xrange, date_breaks = "1 month", date_labels =  "%b %Y",date_minor_breaks="1 week")
    } else  {
      scale_x_date(limits=xrange, date_breaks = "3 months", date_labels =  "%b %Y",date_minor_breaks="1 months")
    }
    )+
    scale_y_continuous(breaks=breaksLeft,sec.axis = sec_axis(~ ./multiplier102,name = i18n$t("7 day incidence / 100 000 "),breaks=breaksRight))+
    labs(x=i18n$t('Date'),y=i18n$t("Daily number of cases (RKI)"))+
    scale_colour_manual(
      limits=c(i18n$t("Daily number of cases (RKI)"), i18n$t("Nowcast"), i18n$t("7 day incidence / 100 000 "), i18n$t("7 day incidence / 100 000 (Nowcast)")),#,i18n$t("35 cases /100.000 /7 days")),
      values=c("#3030FF","#3030FF","black","black"))+
    theme_minimal(base_size=18)+
    theme(legend.title=element_blank(),legend.position="top")+
    guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed", "solid", "dashed"))))
  
}


getBedShadedPlot<-function(thisRunInfo,startD,runLength=10,cutDate,xlimits,bothObs=NULL,ylimits=NULL, todayDate=NULL,onlyICU=F,
                           i18n=NULL,DEBUG=FALSE){
  
  plotWidth<-as.numeric(xlimits[2]-xlimits[1])
  
  thisRunInfo$Date<-startD+thisRunInfo$time
  thisRunInfo<-thisRunInfo[((thisRunInfo$Date)>=cutDate)&((thisRunInfo$Date)<=(cutDate+runLength)),]
  thisRunInfoSub<-thisRunInfo[((thisRunInfo$Date)>=xlimits[1])&((thisRunInfo$Date)<=xlimits[2]),]
  bothObsSub<-bothObs[((bothObs$Date)>=xlimits[1])&((bothObs$Date)<=xlimits[2]),]
    
  if(is.null(ylimits)){
    if(onlyICU){
      ylimits=c(0,max(c(thisRunInfoSub$ICUQ95,
                        thisRunInfoSub$ICUQ50*1.1,
                        bothObs$ICU)))
    }else{
      ylimits=c(0,max(c(thisRunInfoSub$ICUQ95,
                        thisRunInfoSub$GWQ95,
                        thisRunInfoSub$GWQ50*1.1,
                        thisRunInfoSub$ICUQ50*1.1,
                        bothObs$ICU,
                        bothObs$Normal)))
    }
  }
  
  if(DEBUG) print("Plotting")
  ggplot(
    thisRunInfo[((thisRunInfo$time+startD)>=cutDate)&((thisRunInfo$time+startD)<=(cutDate+runLength)),]
  )+
    (if(!onlyICU) geom_ribbon(aes(x=startD+time,ymin=pmax(GWQ25,ylimits[[1]]),ymax=pmin(GWQ75,ylimits[[2]]),fill=i18n$t("GW"))))+
    (if(!onlyICU) geom_ribbon(aes(x=startD+time,ymin=pmax(GWQ05,ylimits[[1]]),ymax=pmin(GWQ95,ylimits[[2]]),fill=i18n$t("GW"))))+
    (if(!onlyICU) geom_line(aes(x=startD+time,y=GWQ50,color=i18n$t("General ward"))))+
    (if((!onlyICU)&!is.null(bothObs)) geom_point(data=subset(bothObs,Date<=cutDate),
                                                 aes(x=Date,y=Normal),
                                                 pch=21, size=2,color="black",fill="#0000FFFF"))+
    (if((!onlyICU)&!is.null(bothObs)) geom_point(data=subset(bothObs,Date>cutDate),
                                                 aes(x=Date,y=Normal),
                                                 pch=21, size=2,color="#0000FF80",fill="#00000000"))+
    geom_ribbon(aes(x=startD+time,ymin=pmax(ICUQ25,ylimits[[1]]),ymax=pmin(ICUQ75,ylimits[[2]]),fill=i18n$t("ICU")))+
    geom_ribbon(aes(x=startD+time,ymin=pmax(ICUQ05,ylimits[[1]]),ymax=pmin(ICUQ95,ylimits[[2]]),fill=i18n$t("ICU")))+
    geom_line(aes(x=startD+time,y=ICUQ50,color=i18n$t("Intensive care")))+
    geom_vline(aes(xintercept=cutDate),linetype=3) +
    (if(!is.null(bothObs)) geom_point(data=subset(bothObs,Date<=cutDate),
                                      aes(x=Date,y=ICU),
                                      pch=21, size=2,color="black",fill="#FF0000FF"))+
    (if(!is.null(bothObs)) geom_point(data=subset(bothObs,Date>cutDate),
                                      aes(x=Date,y=ICU),
                                      pch=21, size=2,color="#FF000080",fill="#00000000"))+
    (if(plotWidth<(75)){
      scale_x_date(limits=xlimits, date_breaks = "1 week", date_labels =  "%d %b",date_minor_breaks="1 day")
    } else if(plotWidth<(2*100)) {
      scale_x_date(limits=xlimits, date_breaks = "1 month", date_labels =  "%b %Y",date_minor_breaks="1 week")
    } else  {
      scale_x_date(limits=xlimits, date_breaks = "3 months", date_labels =  "%b %Y",date_minor_breaks="1 months")
    }
    )+
    scale_color_manual(limits=c(i18n$t("Intensive care"),i18n$t("General ward")),
                       values=c("#FF0000","#0000FF"))+
    scale_fill_manual(limits=c(i18n$t("ICU"),i18n$t("GW")),values=c("#FF000044","#0000FF44"))+
    scale_y_continuous(limits=ylimits)+
    theme_minimal(base_size=18)+
    theme(legend.position="top",legend.box = "horizontal")+
    labs(x=i18n$t("Date"),y=i18n$t("Occupied beds"),fill = i18n$t("IQR & 95% Range"),color=i18n$t("Mean"))
}

#deprecated?
getBedPlaceholderPlot<-function(startD,runLength=10,bothObs=NULL,xlimits=NULL,ylimits=NULL,cutDate=NULL, todayDate=NULL,onlyICU=F,i18n=NULL,DEBUG=FALSE){
  
  plotWidth<-as.numeric(xlimits[2]-xlimits[1])
  if(is.null(ylimits)){
    ylimits=c(0,100)
  }
  if(DEBUG) print("Plotting")
  
  dateLength<- as.numeric(xlimits[2]-cutDate)
  midplotx=xlimits[1]+round(plotWidth/2)
  midploty=ylimits[1]+((ylimits[2]-ylimits[1])/2)
  ggplot(data.frame(x=cutDate+(0:dateLength),y=1)
  )+
    (if(!onlyICU)geom_ribbon(aes(x=x,ymin=y+5,ymax=min(y+7,ylimits[[2]]),fill=i18n$t("GW"))))+
    (if(!onlyICU)geom_ribbon(aes(x=x,ymin=y+4,ymax=min(y+8,ylimits[[2]]),fill=i18n$t("GW"))))+
    (if(!onlyICU)geom_line(aes(x=x,y=y+6,color=i18n$t("General ward"))))+
    
    geom_ribbon(aes(x=x,ymin=y-1,ymax=min(y+1,ylimits[[2]]),fill=i18n$t("ICU")))+
    geom_ribbon(aes(x=x,ymin=y-2,ymax=min(y+2,ylimits[[2]]),fill=i18n$t("ICU")))+
    geom_line(aes(x=x,y=y,color=i18n$t("Intensive care")))+
    geom_vline(aes(xintercept=cutDate),linetype=3) +
    (if(plotWidth<(75)){
      scale_x_date(limits=xlimits, date_breaks = "1 week", date_labels =  "%d %b",date_minor_breaks="1 day")
    } else if(plotWidth<(2*100)) {
      scale_x_date(limits=xlimits, date_breaks = "1 month", date_labels =  "%b %Y",date_minor_breaks="1 week")
    } else  {
      scale_x_date(limits=xlimits, date_breaks = "3 months", date_labels =  "%b %Y",date_minor_breaks="1 months")
    }
    )+
    annotate("text", label = "Simulating patient care paths.\n This may take a while,\n please wait", x = midplotx, y = midploty, size = 8, colour = "black")+
    scale_color_manual(limits=c(i18n$t("Intensive care"),i18n$t("General ward")),
                       values=c("#FF0000","#0000FF"))+
    scale_fill_manual(limits=c(i18n$t("ICU"),i18n$t("GW")),values=c("#FF000044","#0000FF44"))+
    scale_y_continuous(limits=ylimits)+
    theme_minimal(base_size=18)+
    theme(legend.position="top",legend.box = "horizontal")+
    labs(x=i18n$t("Date"),y=i18n$t("Occupied beds"),fill = i18n$t("IQR & 95% Range"),color=i18n$t("Mean"))
}


getSprinklePlotIQR<-function(runResults,xlimits=NULL,ylimits=NULL,cutDate=NULL,plotWeeklyPattern=FALSE,i18n=NULL,DEBUG=FALSE){
  if(plotWeeklyPattern){
    plotVar<-"wklyReported"
  } else {
    plotVar<-"reportedEpi"
  }
  
  summaryRes<-runResults %>%
    group_by(time) %>%
    summarise(origRep=min(report),
              nowcast = ncMean,
              med = quantile(get(plotVar),probs=0.5,na.rm=T),
              #  mea = mean(reportedRaw),
              Q05 = quantile(get(plotVar),probs=0.05,na.rm=T),
              Q25 = quantile(get(plotVar),probs=0.25,na.rm=T),
              Q75 = quantile(get(plotVar),probs=0.75,na.rm=T),
              Q95 = quantile(get(plotVar),probs=0.95,na.rm=T)
    ) %>%
    as.data.frame()

  if(is.null(cutDate)) cutDate<-runResults[[1,"currentDay"]]#-1
  
  if(is.null(xlimits)){
    xlimits=c(min(runResults$time),max(runResults$time))
  }

  obsLastDate<-runResults[[1,"currentDay"]]-2

  if(is.null(ylimits)){
    subRunResults<-summaryRes[summaryRes$time>=xlimits[1]&summaryRes$time<=xlimits[2],]
    ylimits=c(0,max(subRunResults$Q95,na.rm = T))
  }
  plotWidth<-as.numeric(xlimits[2]-xlimits[1])

  if(plotWeeklyPattern){  
  prePlot<-ggplot(runResults)+
    geom_step(data=subset(summaryRes,time<=(cutDate+1)),
              aes(x=time,y=origRep,color=i18n$t("Reported")),direction="vh")+
    geom_step(data=subset(summaryRes,time>cutDate),
              aes(x=time-1,y=origRep),color="grey",direction="hv")+
    geom_step(data=subset(summaryRes,time<=cutDate,direction="vh"),
              aes(x=time,y=nowcast,color=i18n$t("Nowcast")), linetype="dashed", direction="vh")+
    
    geom_stepribbon(data=subset(summaryRes,time>cutDate),
                    aes(x=time-1,ymin=pmax(Q25,ylimits[[1]]),ymax=pmin(Q75,ylimits[[2]]),fill=i18n$t("IQR & 95% Range")))+
    geom_stepribbon(data=subset(summaryRes,time>cutDate),
                    aes(x=time-1,ymin=pmax(Q05,ylimits[[1]]),ymax=pmin(Q95,ylimits[[2]]),fill=i18n$t("IQR & 95% Range")))+
    
    geom_step(data=subset(summaryRes,time>cutDate),
              aes(x=time-1,y=med),color="#FFFFFFBB",size=3,direction="hv")+
    geom_step(data=subset(summaryRes,time>cutDate),
              aes(x=time-1,y=med,color=i18n$t("Forecast")),direction="hv")
  } else {
    prePlot<-ggplot(runResults)+
      geom_step(data=subset(summaryRes,time<=(cutDate)),
                aes(x=time,y=origRep,color=i18n$t("Reported")),direction="vh")+
      geom_step(data=subset(summaryRes,time<=cutDate),
                aes(x=time,y=nowcast,color=i18n$t("Nowcast")), linetype="dashed" ,direction="vh")+
      geom_line(data=subset(runResults,time>(cutDate)),aes(x=time,y=reportedEpi,group=runNum,color=i18n$t("Individual runs")))+
      geom_ribbon(data=subset(summaryRes,time>cutDate),
                      aes(x=time,ymin=pmax(Q25,ylimits[[1]]),ymax=pmin(Q75,ylimits[[2]]),fill=i18n$t("IQR & 95% Range")))+
      geom_ribbon(data=subset(summaryRes,time>cutDate),
                      aes(x=time,ymin=pmax(Q05,ylimits[[1]]),ymax=pmin(Q95,ylimits[[2]]),fill=i18n$t("IQR & 95% Range")))+
      geom_line(data=subset(summaryRes,time>cutDate),
                aes(x=time,y=med),color="#FFFFFFBB",size=3,direction="hv")+
      geom_line(data=subset(summaryRes,time>cutDate),
                aes(x=time,y=med,color=i18n$t("Forecast")),direction="hv")
  }
    
  #scale_x_date(limits=xlimits)+
    return(
      prePlot+
          (if(plotWidth<(75)){
            scale_x_date(limits=xlimits, date_breaks = "1 week", date_labels =  "%d %b",date_minor_breaks="1 day")
          } else if(plotWidth<(2*100)) {
            scale_x_date(limits=xlimits, date_breaks = "1 month", date_labels =  "%b %Y",date_minor_breaks="1 week")
          } else  {
            scale_x_date(limits=xlimits, date_breaks = "3 months", date_labels =  "%b %Y",date_minor_breaks="1 months")
          }
        )+
        scale_y_continuous(limits=ylimits)+
        scale_color_manual(limits=c(i18n$t("Forecast"),i18n$t("Reported"), i18n$t("Nowcast"), i18n$t("Individual runs")),
                           values = c("orange","black","black","#66666624"))+
        scale_fill_manual(limits=c(i18n$t("IQR & 95% Range")),values = c("#FF450033"))+
        labs(x=i18n$t("Date"),y=i18n$t("Confirmed cases"),color=i18n$t("Cases"),fill="")+
        theme_minimal(base_size=18)+
        theme(legend.position="top",legend.box = "horizontal")+
        guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed", "solid"))))
    )
}

getVaccPlot<-function(vaccdata,xlimits=NULL,ylimits=NULL,cutDate=NULL,i18n=NULL,DEBUG=FALSE){
  if(is.null(cutDate)) cutDate<-today()-1
  if(is.null(xlimits)){
    xlimits=c(min(vaccdata$date),max(vaccdata$date))
  }
  if(is.null(ylimits)){
    ylimits=c(0,100)    #c(0,max(percFullLK))
  }
  
  plotWidth<-as.numeric(xlimits[2]-xlimits[1])

  ggplot(vaccdata) +
    geom_area(aes(x=Date,y = peopleFirstProp*100,
                  color=i18n$t("1. vaccination"),
                  fill=i18n$t("1. vaccination"),
                  linetype=i18n$t("1. vaccination")
    )) +
    #geom_area(data=subset(vaccdata,date<=cutDate),aes(x=date,y = peopleFullProp*100,
    geom_area(data=(vaccdata),aes(x=Date,y = peopleFullProp*100,
                                  color=i18n$t("Full vaccination"),
                                  fill=i18n$t("Full vaccination"),
                                  linetype=i18n$t("Full vaccination")
    )) +
    geom_area(data=(vaccdata),aes(x=Date,y = peopleBoosterProp*100,
                                  color=i18n$t("Booster vaccination"),
                                  fill=i18n$t("Booster vaccination"),
                                  linetype=i18n$t("Booster vaccination")
    )) +
    geom_line(data=vaccdata,aes(x=Date,y=devVaccProtect*100,
                                color=i18n$t("Projected population-based protection"),
                                fill=NULL,
                                linetype=i18n$t("Projected population-based protection")
    )) +
     geom_vline(aes(xintercept=cutDate),linetype=3) +
    #scale_x_date(limits=xlimits)+
    (if(plotWidth<(75)){
      scale_x_date(limits=xlimits, date_breaks = "1 week", date_labels =  "%d %b",date_minor_breaks="1 day")
    } else if(plotWidth<(2*100)) {
      scale_x_date(limits=xlimits, date_breaks = "1 month", date_labels =  "%b %Y",date_minor_breaks="1 week")
    } else  {
      scale_x_date(limits=xlimits, date_breaks = "3 months", date_labels =  "%b %Y",date_minor_breaks="1 months")
    }
    )+
    scale_color_manual(limits=c(i18n$t("1. vaccination"),
                                i18n$t("Full vaccination"),
                                i18n$t("Booster vaccination"),
                                i18n$t("Projected population-based protection")),
                       values = c("#00AA00","#006600","#003f00","black"))+
    scale_fill_manual(limits=c(i18n$t("1. vaccination"),
                               i18n$t("Full vaccination"),
                               i18n$t("Booster vaccination"),
                               i18n$t("Projected population-based protection")),
                      values = c("#00990055","#00990055","#00990055","#FFFFFF00"))+
    scale_linetype_manual(limits=c(i18n$t("1. vaccination"),
                                   i18n$t("Full vaccination"),
                                   i18n$t("Booster vaccination"),
                                   i18n$t("Projected population-based protection")),
                          values = c(1,1,1,2))+
    scale_y_continuous(limits=ylimits)+
    labs(x=i18n$t("Date"),y=i18n$t("Proportion of vaccinations in %"),
         color=i18n$t("Vaccinations"),
         fill=i18n$t("Vaccinations"),
         linetype=i18n$t("Vaccinations")
    )+
    theme_minimal(base_size=18)+
    theme(legend.position="top",legend.box = "horizontal")
}



epiestim2plot<-function(inc,epi_result,nowcast,Rdevs,startSimDate,simLength,viewRangeRt,inclVOC,useETS,i18n,DEBUG=FALSE){

  backwards<-(as.numeric(inc[nrow(inc),"Date"]-(startSimDate-1)))
  #erres<-as.data.frame(epi_result$R)
  #erresDate<-(epi_result$dates)
  #erres$Date<-erresDate[(length(erresDate)-nrow(erres)):(length(erresDate)-1)]+1
  
  erres<-epi_result
  
  ## S3 method for class 'estimate_R'

  if(inclVOC){
    Rdevs$SelectDevR0=Rdevs[,"devRTVOC"]
    if(useETS){ 
      Rdevs$SelectDevR0=Rdevs[,"devRTetsVOC"]
    }
  } else {
    Rdevs$SelectDevR0=Rdevs[,"RTstatic"]
    if(useETS){ 
      Rdevs$SelectDevR0=Rdevs[,"devRTets"]
    }
  }
  
  allnowcasts<-
    Reduce(rbind,lapply(1:length(nowcast$ncRuns),function(x){
      firstNowcast<-as.data.frame(nowcast$ncRuns[[x]]$rEstimate$R)
      
      firstNowcastDate<-nowcast$ncRuns[[x]]$rEstimate$dates
      firstNowcast$Date<-firstNowcastDate[
        (length(firstNowcastDate)-nrow(firstNowcast)):(length(firstNowcastDate)-1)]+1
      firstNowcast$run<-x ##assuming they're the same length
      return(firstNowcast)
    })
    )
  if(DEBUG) print(allnowcasts)

  plotWidth<-as.numeric(viewRangeRt[2]-viewRangeRt[1])
  maxY=max(
    max(subset(allnowcasts,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$`Quantile.0.975(R)`,na.rm = T),
    max(subset(erres,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$R_Q975_raw,na.rm = T),
    max(subset(Rdevs,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$SelectDevR0,na.rm = T)
  )
  minY=min(
    min(subset(allnowcasts,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$`Quantile.0.025(R)`,na.rm = T),
    min(subset(erres,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$R_Q025_raw,na.rm = T),
    min(subset(Rdevs,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$SelectDevR0,na.rm = T)
  )
  print(subset(erres,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$nonVOC_nc)

  if(inclVOC) { 
    maxY=max(
      maxY,
      max(subset(erres,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$VOC_nc,na.rm = T),
      na.rm = T
    )
     minY= min(
       minY,
       min(subset(erres,(Date>=viewRangeRt[[1]])&(Date<=viewRangeRt[[2]]))$nonVOC_nc,na.rm = T),
       na.rm = T
     )
  }
  print(paste0("minY: ",minY," maxY: ",maxY))
  if(DEBUG) print(Rdevs[Rdevs$Date>startSimDate,])
  thePlot<-ggplot(erres)+
    (if(inclVOC) geom_line(aes(x=Date,y=nonVOC_nc),colour="blue"))+
    (if(inclVOC) geom_line(aes(x=Date,y=VOC_nc),colour="red"))+
    geom_line(data=allnowcasts,aes(x=Date,y=`Mean(R)`,group=run,color=i18n$t("Nowcast")))+
    geom_line(data=Rdevs[Rdevs$Date>startSimDate,],aes(x=Date,y=SelectDevR0,group=runNum,color=i18n$t("Forecast")))+
    
    geom_line(data=allnowcasts,aes(x=Date,y=`Quantile.0.025(R)`,group=run,fill="95% CI"),linetype=3)+#,fill="#00000022")+
    geom_line(data=allnowcasts,aes(x=Date,y=`Quantile.0.975(R)`,group=run,fill="95% CI"),linetype=3)+#,fill="#00000022")+
    geom_ribbon(aes(x=Date,ymin=R_Q025_raw,ymax=R_Q975_raw,fill="95% CI"))+#,fill="#00000022")+
    geom_line(aes(x=Date,y=R_mean_raw,color=i18n$t("Mean(R)")))+
    geom_hline(aes(yintercept = 1,color="R=1"),linetype=2)+
    geom_vline(xintercept = startSimDate,color= "#f3c483")+
    geom_vline(xintercept = inc[nrow(inc),"Date"],color="#749dae")+
    # scale_x_date(limits=viewRangeRt)+
    (if(plotWidth<(75)){
      scale_x_date(limits=viewRangeRt, date_breaks = "1 week", date_labels =  "%d %b",date_minor_breaks="1 day")
    } else if(plotWidth<(2*100)) {
      scale_x_date(limits=viewRangeRt, date_breaks = "1 month", date_labels =  "%b %Y",date_minor_breaks="1 week")
    } else  {
      scale_x_date(limits=viewRangeRt, date_breaks = "3 months", date_labels =  "%b %Y",date_minor_breaks="1 months")
    }
    )+
    scale_y_continuous(limits = c(minY,maxY))+
    labs(x=i18n$t("Date"),y=i18n$t("Mean(R)"))+
    scale_fill_manual(limits=c("95% CI"),values=c("#00000022"))+#22
    scale_color_manual(limits=c(i18n$t("Mean(R)"),"R=1",i18n$t("Nowcast"),i18n$t("Forecast")),
                       values=c("black","red","#00000040","#00FFFF80"))+
    theme_minimal(base_size=18)+
    theme(legend.title=element_blank(),legend.position = "top")
  return(thePlot)
}



