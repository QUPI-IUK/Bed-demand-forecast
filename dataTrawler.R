#!/usr/bin/env Rscript
library(utils)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)

logFilename<-"/var/www/public_files/CovidDataLogFile.txt"
dataFilename<-"/var/www/public_files/LocalCovidCopy.rds"
diviFilename<-paste0("/var/www/public_files/LocalDiviCopy.rds")
diviHistoryFilename<-paste0("/var/www/public_files/DiviHistory.rds")
diviHistoryTestFilename<-paste0("/var/www/public_files/DiviHistory-test.rds")

DKGEVdataFilename<-"/var/www/public_files/LocalDKGEVCopy.rds"
#GWdataFilename<-"/var/www/public_files/LocalGWCopy.rds"

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

readUrl <- function() {
  out <- tryCatch(
    {
      read.csv("https://opendata.arcgis.com/datasets/9644cad183f042e79fb6ad00eadc4ecf_0.csv")
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    }
  )
  return(out)
}

inciData<-readUrl()
doHeavyTrawl<-FALSE

if(is.null(inciData)){
  # the load failed, write this to our log file
  line=paste0("- Failed loading incidence data directly",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load unsuccessful")
  #test if data needs to be trawled (i.e. if the datenstand is incorrect)
} else {
  # the load was successful, log and write
  
  print("Load successful")
  
  dwnlDatenstand<-as.Date(substr(inciData$Datenstand,0,10),format="%d.%m.%Y")
  if(max(dwnlDatenstand)==today()) {
    saveRDS(inciData,dataFilename)
    line=paste0("+ Passed incidence data directly ",now()," !*")
    write(line,file=logFilename,append=TRUE)
  } else{
    line=paste0("!! Incorrect datenstand summed file ",now()," !*")
    write(line,file=logFilename,append=TRUE)
    
    #Check if we need to update the data.
    curCachedData<-readRDS("/var/www/public_files/LocalCovidCopy.rds")
    curCacheDatenstand<-as.Date(substr(curCachedData$Datenstand,0,10),format="%d.%m.%Y")
    if(max(curCacheDatenstand)<today()){
      doHeavyTrawl<-TRUE
      line=paste0("! Trying the heavy trawler ",now()," !*")
      write(line,file=logFilename,append=TRUE)   
    } else {
      #assume correct data
      line=paste0("! Datenstand cached data is correct, assumed correct data ",now()," !*")
      write(line,file=logFilename,append=TRUE)   
    }
  }
}

if(doHeavyTrawl){
  source("/usr/share/DashboardScripts/EmergencyTrawler.R")
  tryDate<-getSingleDatenstand()
  if(tryDate==max(dwnlDatenstand)) {
    line=paste0("! Aborted heavy trawler, online data not newer than cached ",now()," !*")
    write(line,file=logFilename,append=TRUE)    
    line=paste0("! Keeping old cached file ",now()," !*")
    write(line,file=logFilename,append=TRUE)    
  } else {
    
    inciData<-heavyDataTrawler()
    if(is.null(inciData)){
      line=paste0("- Failed incidence data using heavy trawler ",now()," !*")
      write(line,file=logFilename,append=TRUE)    
      line=paste0("! Keeping old cached file ",now()," !*")
      write(line,file=logFilename,append=TRUE)    
    } else {
      saveRDS(inciData,dataFilename)
      line=paste0("+ Passed incidence data using heavy trawler ",now()," !*")
      write(line,file=logFilename,append=TRUE)
      
    }
  }
}

vaccByBL <- data.table::fread("https://impfdashboard.de/static/data/germany_vaccinations_by_state.tsv",sep='\t')
vaccByBL[, date:=today()]
vaccdataFilename<-paste0("/var/www/public_files/vaccData", today(), ".rds")

if(is.null(vaccByBL)){
  # the load failed, write this to our log file
  line=paste0("- Failed loading vaccination data ",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of vaccination data unsuccessful")
} else {
  line=paste0("+ Passed vaccination data ",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of vaccination data successful")
  saveRDS(vaccByBL,vaccdataFilename)
}

getDKGEVBelegungsZahlen <- function(i, sheetnumber = 4) {
  day <- as.Date(Sys.time())-i
  print(day)
  datestring1 <- format(day, "%d.%m.%Y")
  datestring2 <- format(day, "%Y%m%d")
  filename <- paste0("https://www.dkgev.de/fileadmin/default/Mediapool/Corona/",datestring1,"/Covid_",datestring2,".xlsx")
  data <- rio::import(file = filename, which = sheetnumber)
  return(data)
}


readDKGEVBelegungsZahlen <- function() {
  sheet <- 4
  nosuccess <- TRUE
  i <- 0
  while (nosuccess && i < 30) {
    j <- i
    tryCatch(data <- getDKGEVBelegungsZahlen(j, sheet), error = function(data) {i <<- i + 1} )
    if (i == j) {
      nosuccess <- FALSE
    }
    print(data)
  }
  #getDKGEVBelegungsZahlen(10,4)
  
  ##### Copied this into the function for more clarity
  df <- as.data.frame(data)
  names(df) <- as.matrix(df[1, ])
  df <- tail(df, -1)
  dates <- df[1]
  colnames(dates) <- "date"
  date <- as.Date(as.numeric(dates$date), origin = "1899-12-30")
  df$Summe <- NULL
  df <- cbind(date, df[,-1])
  df <- df %>% tidyr::pivot_longer(!date, names_to = "Bundesland", values_to = "Belegung")
  df$daten_stand <- max(df$date)
  df$Belegung<-as.numeric(df$Belegung)
  #df<-fillMissingBelegungen(df) # is that needed?
  df<-df[order(df$Bundesland,df$date),]
  #df$Belegung <- round(extraInterPolate(df$Belegung))
  return(df)
}

belegungsdaten <- readDKGEVBelegungsZahlen()

if(is.null(belegungsdaten)){
  # the load failed, write this to our log file
  line=paste0("- Failed loading DKGEV data from today",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of DKGEV data unsuccessful")
  #test if data needs to be trawled (i.e. if the datenstand is incorrect)
} else {
  print("Load of DKGEV data successful")
  # Case 1: cached file didn't exist yet, save.
  if(!file.exists(DKGEVdataFilename)){
    saveRDS(belegungsdaten,DKGEVdataFilename)
  } else {
    # the load was successful, log and write
    curCachedData<-readRDS(DKGEVdataFilename)
    dwnlDatenstand<-as.Date(belegungsdaten$daten_stand,format="%d.%m.%Y")
    curCacheDatenstand<-as.Date(curCachedData$daten_stand,format="%d.%m.%Y")
    if(max(dwnlDatenstand)>=max(curCacheDatenstand)) {
      # Case 2: cached file is older.
      print("Case 2, needed updating")
      saveRDS(belegungsdaten,DKGEVdataFilename)
      line=paste0("+ Passed DKGEV data directly ",now()," !*")
      write(line,file=logFilename,append=TRUE)
    } else{
      # Case 3: cached file doesn't need updating.
      print("Case 3")
      line=paste0("!! DKGEV data doesn't need updating ",now()," !*")
      write(line,file=logFilename,append=TRUE)
    }
  }
}

tempDIVIdata<-as.data.frame(data.table::fread("https://diviexchange.blob.core.windows.net/%24web/DIVI_Intensivregister_Auszug_pro_Landkreis.csv"))
if(is.null(tempDIVIdata)){
  # the load failed, write this to our log file
  line=paste0("- Failed loading DIVI data ",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of DIVI data unsuccessful")
} else {
  tempDIVIdata$AGS<-str_pad(as.character(tempDIVIdata$gemeindeschluessel),5,side="left",pad="0")
  line=paste0("+ Passed DIVI data ",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of DIVI data successful")
  saveRDS(tempDIVIdata,diviFilename)
}

tempDIVIdata2<-as.data.frame(data.table::fread("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-tagesdaten.csv"))
belegungsdaten <- readDKGEVBelegungsZahlen()

if(is.null(tempDIVIdata2)){
  # the load failed, write this to our log file
  line=paste0("- Failed loading DIVI historical data ",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of DIVI historical data unsuccessful")
} else {
  tempDIVIdata2$AGS<-str_pad(as.character(tempDIVIdata2$gemeindeschluessel),5,side="left",pad="0")
  tempDIVIdata2$date<-as.Date(tempDIVIdata2$date)
  saveRDS(tempDIVIdata2,diviHistoryFilename)
  
  #popLKtemp <-read.csv("Resources/RKI_Corona_Landkreise.csv")     #FILE MUST EXIST ON SERVER
  popLKtemp <-read.csv("/usr/share/DashboardScripts/RKI_Corona_Landkreise.csv")     #FILE MUST EXIST ON SERVER
  
  popLKtemp$AGS<-str_pad(as.character(popLKtemp$AGS),5,side="left",pad="0")
  popLKtemp$RS<-str_pad(as.character(popLKtemp$RS),5,side="left",pad="0")
  
  blICU<-tempDIVIdata2%>%
    group_by(bundesland,date)%>%
    summarise(ICU=sum(faelle_covid_aktuell)
    )%>%as.data.frame()
  
  convertTable<-data.frame(BL=c("Schleswig-Holstein","Hamburg","Niedersachsen","Bremen","Nordrhein-Westfalen","Hessen", "Rheinland-Pfalz", "Baden-Württemberg", "Bayern", "Saarland", "Brandenburg", "Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen","Berlin"),
             BL_ID=c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,11))
  
  belegungsdaten<-merge(belegungsdaten,convertTable,by.x="Bundesland",by.y="BL")
  bedsMerged<-merge(belegungsdaten,blICU,by.x=c("date","BL_ID"),by.y=c("date","bundesland"),all=T)

  bedsMerged$conv<-bedsMerged$Belegung/bedsMerged$ICU
  bedsMerged[bedsMerged$date==as.Date("2021-11-01"),"conv"]<-2.3
  bedsMerged[bedsMerged$date==as.Date("2020-04-24"),"conv"]<-2.3

  temp<-lapply(unique(bedsMerged$BL_ID),function(x){
    d1=max(bedsMerged[bedsMerged$BL_ID==x,"date"])
    d2=max(bedsMerged[(!is.na(bedsMerged$conv))&(bedsMerged$BL_ID==x),"date"])
    bedsMerged[(bedsMerged$BL_ID==x)&(bedsMerged$date==d1),"conv"]<<-bedsMerged[(bedsMerged$BL_ID==x)&(bedsMerged$date==d2),"conv"]
  })
  
  bedsMerged<-bedsMerged[order(bedsMerged$BL_ID,bedsMerged$date),]
  bedsMerged$conv<-extraInterPolate(bedsMerged$conv)
  bedsMerged$estGWBL<-round(bedsMerged$ICU*bedsMerged$conv)
 
  tempDIVIdata3<-merge(tempDIVIdata2,popLKtemp[,c("BL","BL_ID","EWZ","EWZ_BL","county","RS")],by.x="AGS",by.y="RS",all=T)
  
  #Do Berlin
  tempDIVIdata3[tempDIVIdata3$AGS=="11000","BL"]<-"Berlin"
  tempDIVIdata3[tempDIVIdata3$AGS=="11000","BL_ID"]<-11
  tempDIVIdata3[tempDIVIdata3$AGS=="11000","EWZ"]<-sum(popLKtemp[popLKtemp$BL_ID==11,"EWZ"])
  tempDIVIdata3[tempDIVIdata3$AGS=="11000","EWZ_BL"]<-popLKtemp[popLKtemp$BL_ID==11,"EWZ_BL"][[1]]
  
  tempDIVIdata3<-merge(tempDIVIdata3,bedsMerged,by=c("BL_ID","date"),all=T)

  tempDIVIdata3$propEWZ<-tempDIVIdata3$EWZ/tempDIVIdata3$EWZ_BL
  tempDIVIdata3$estGWLK<-round(tempDIVIdata3$estGWBL*tempDIVIdata3$propEWZ)
  tempDIVIdata4<-tempDIVIdata3[,c("date","faelle_covid_aktuell","AGS","estGWLK")]
  colnames(tempDIVIdata4)<-c("date","faelle_covid_aktuell","AGS","estGWLK")

  line=paste0("+ Passed DIVI historical data ",now()," !*")
  write(line,file=logFilename,append=TRUE)
  print("Load of DIVI historical data successful")
  saveRDS(tempDIVIdata4,diviHistoryTestFilename)
}

line=paste0("+ Reached end of the data trawler script ",now()," ")
write(line,file=logFilename,append=TRUE)


#######

