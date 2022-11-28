
readRDS2<-function(filename,DEBUG=FALSE){
  if(str_detect(filename,"http")){
    print("online!")
    download.file(filename,"tempFile.rds", method="curl", extra="-k")
    return(readRDS("tempFile.rds"))
  } else {
    return(readRDS(filename))
  }
}

readcsvguessed<-function(filename,targetNCol=3){
  possibleSep<-c(";",",","&","|","$","\t")
  for(x in possibleSep){
    tempRead<-read.csv(filename, sep=x)
    if(ncol(tempRead)==targetNCol){
      print(paste0("Used delimiter guess. Guessed ",x))
      return(tempRead)
    }
  }
  warning("Load failed for bed occupancy. Used placeholder to continue.")
  reportLine("Load failed for bed occupancy. Used placeholder to continue.",session)
  return(data.frame(Date=today()-1,ICU=1,Normal=1))
}

getSelectedBedNumbers<-function(bedNums,LandkreiseMulti,districtPops){
  
  selDistrictIDs<-getDistrictCodes(LandkreiseMulti,districtPops)
  thisSet<-bedNums[bedNums$idDistrict%in%selDistrictIDs,]
  selectedBeds<-as.data.frame(
    thisSet%>%
      group_by(Date) %>%
      dplyr::summarise(
        ICU=sum(ICU),
        Normal=sum(Normal),
        Hosp=sum(ICU)+sum(Normal))
  )
  return(selectedBeds)
}

getSelectedInci <- function(inci, LandkreiseMulti,districtPops){ #TODO: This also needs to go into a data loading function.
  selDistrictIDs<-getDistrictCodes(LandkreiseMulti,districtPops)
  thisSet<-inci[inci$IdLandkreis%in%selDistrictIDs,]
  
  totPop<-sum(unlist(lapply(unique(thisSet$IdLandkreis), function(x) ((districtPops[!is.na(districtPops$idDistrict),"Pop"][districtPops[!is.na(districtPops$idDistrict),"idDistrict"]==(x)])))))
  
  selectedInci<-as.data.frame(
    thisSet%>%
      group_by(Date) %>%
      dplyr::summarise(Cases=sum(Cases))
  )
  selectedInci$Cases<-convexhull(selectedInci$Cases) #remove negative numbers.
  
  selectedInci$WeeklyCases<-c(rep(0,6),unlist(lapply(7:nrow(selectedInci),function(x)sum(selectedInci[x-0:6,"Cases"]))))
  selectedInci$SevenDaysInci<-100000*(selectedInci$WeeklyCases/totPop)
  selectedInci$Pop<-totPop
  
  
  dateandinc <- as.data.frame(selectedInci[,c("Date", "Cases", "WeeklyCases", "SevenDaysInci", "Pop")])
  colnames(dateandinc) <- c("Date", "Cases", "WeeklyCases", "SevenDaysInci", "Pop")
  return(dateandinc)
}

getDistrictCode<-function(district,districtPops,DEBUG=FALSE){#Loading a single Landkreis
  #changed all "GEN" to "districtName"
  districtCodes=unique(districtPops[,c("districtName","idDistrict","stateName")])
  districtCodes<-(districtCodes[!is.na(districtCodes$idDistrict),])

  if(!(district %in% districtCodes$idDistrict)){
    found=FALSE
    if(district %in% districtCodes$districtName){
      district<-districtCodes[districtCodes$districtName==district,"idDistrict"]
      found=T
    } else {
      detectedPattern<-which(str_detect(districtCodes$districtName,district))
      if(length(detectedPattern)>1){
        stopstring<-paste0("Landkreis identifier ",district," ambivalent, multiple matches: ")
        for(ii in 1:length(detectedPattern)) stopstring<-paste0(stopstring,"\n -",districtCodes[detectedPattern[[ii]],"districtName"])
        
        
        stop(stopstring)
      }
      if(length(detectedPattern)==1){
        district<-districtCodes[detectedPattern[[1]],"idDistrict"]
        found=T
      }
    }
    if(!found) stop(paste0("Didn't find identifier: ",district))
  }
  return(district)
}

getDistrictCodes<-function(districts,districtPops,DEBUG=FALSE){
  return(unlist(lapply(districts,function(x) getDistrictCode(x,districtPops))))
}



###############################################################
#             Germany specific functions                      #
###############################################################

getDataLocationDE<-function(DEBUG=FALSE){
 # return("online_only")
  serverDockerfile<-"/mnt/mydata/LocalCovidCopy.rds"
  serverfile<-"/var/www/public_files/LocalCovidCopy.rds"
  if(file.exists(serverDockerfile)){
    print("We're working in our Docker container")
    return("docker")
  }
  if(file.exists(serverfile)){
    print("We're working in our server")
    return("server")
  }
  print("We're working elsewhere")
  return("online_only")
}

getDistrictPopsDE<-function(DEBUG=FALSE){
  if(DEBUG) print("read population sizes")
  popLKtemp<-read.csv("Resources/RKI_Corona_Landkreise.csv")
  popLKtemp$AGS<-str_pad(as.character(popLKtemp$RS),5,side = "left",pad="0")
  popLKtemp<-popLKtemp[,c("AGS","RS","NUTS","county","EWZ","BL")]
  colnames(popLKtemp)<-c("idDistrict","RS","NUTS","districtName","Pop","stateName")
  popLKtemp$idState<-substr(popLKtemp$idDistrict,1,2)
  return(popLKtemp)
}

readHistoricalBedInfoDE<-function(dataLoc,districtPops,maxDate=(today()-1),DEBUG=FALSE){#"Current" data from DIVI is not a big dataset, easier to read entire thing at once on initial load of dashboard
  
  tempDIVIdata<-case_when(
    dataLoc == "docker" ~ "/mnt/mydata/DiviHistoryTest.rds",
    dataLoc == "server" ~ "/var/www/public_files/DiviHistory-test.rds",
    dataLoc == "online_only" ~ "https://iuk-forecast.uniklinik-freiburg.de/DiviHistoryTest.rds"
  )%>%readRDS2()
  
  tempDIVIdataTime<-case_when(
    dataLoc == "docker" ~ "/mnt/mydata/DiviHistoryTest.rds",
    dataLoc == "server" ~ "/var/www/public_files/DiviHistory-test.rds",
    dataLoc == "online_only" ~ "https://iuk-forecast.uniklinik-freiburg.de/DiviHistoryTest.rds"
  )%>%(function(x){
    
    if(str_detect(x,"http")){
      print("online! v2 - historical beds")
      return(NA)
    } else {
      return(file.info(x)$ctime)
    }
  })
  
  nonBerlinPart<-tempDIVIdata[tempDIVIdata$AGS!=11000,]
  berlinPart<-tempDIVIdata[tempDIVIdata$AGS==11000,]
  
  tempDIVIdataNonBerlin=data.frame(
    Date=nonBerlinPart$date,
    ICU=nonBerlinPart$faelle_covid_aktuell,
    dwnldtime=tempDIVIdataTime,
    idDistrict=nonBerlinPart$AGS,
    Normal=nonBerlinPart$estGWLK
  )
  berlinRecs<-(floor(districtPops$RS/1000)==11)
  berlinProps<-districtPops[berlinRecs,"Pop"]/sum(districtPops[berlinRecs,"Pop"])
  tempDIVIdataBerlin<-bind_rows(apply(berlinPart,1, function(r) {
    return(data.frame(
    Date=as.Date(r["date"]),
    ICU=round(as.numeric(r["faelle_covid_aktuell"])*berlinProps),
    dwnldtime=tempDIVIdataTime,
    idDistrict=districtPops[berlinRecs,"RS"],
    Normal=round(as.numeric(r["estGWLK"])*berlinProps)
  ))}))
  
  tempDIVIdata<-rbind(tempDIVIdataBerlin,tempDIVIdataNonBerlin)
  tempDIVIdata$daten_stand<-max(tempDIVIdata$date)
  tempDIVIdata$dwnldtime<-tempDIVIdataTime
  
  #tempDIVIdata$Normal<-ceiling(tempDIVIdata$ICU*2.3)
  tempDIVIdata$Hosp<-tempDIVIdata$ICU+tempDIVIdata$Normal
  
  tempDIVIdata<-tempDIVIdata[tempDIVIdata$Date<=maxDate,]
  return(tempDIVIdata)
}

getInciDataDE<- function(dataLoc,DEBUG=FALSE){ #Note: This should be the only function that needs adjustment to load incidence data from another country.

  temp<-case_when(
    dataLoc == "docker" ~ "/mnt/mydata/LocalCovidCopy.rds",
    dataLoc == "server" ~ "/var/www/public_files/LocalCovidCopy.rds",
    dataLoc == "online_only" ~ "https://iuk-forecast.uniklinik-freiburg.de/rkiCOVID.rds"
  )%>%readRDS2()
  
  tempdataTime<-case_when(
    dataLoc == "docker" ~ "/mnt/mydata/LocalCovidCopy.rds",
    dataLoc == "server" ~ "/var/www/public_files/LocalCovidCopy.rds",
    dataLoc == "online_only" ~ "https://iuk-forecast.uniklinik-freiburg.de/rkiCOVID.rds"
  )%>%(function(x){
    if(str_detect(x,"http")){
      print("online! v2 - incidence")
      return(NA)
    } else {
      return(file.info(x)$ctime)
    }
  })

  temp2=data.frame(
    Cases=temp$AnzahlFall,
    IdLandkreis=stringr::str_pad(as.character(temp$IdLandkreis),5,side="left",pad="0"),
    Date=as.Date(substr(temp$Meldedatum,0,10),format="%Y/%m/%d"),
    Updated=as.Date(substr(temp$Datenstand,0,10),format="%d.%m.%Y"),
    dwnldtime=tempdataTime
  )
  return(temp2)
}

downloadVaccInfoDE <- function(dataLoc,districtPops,DEBUG=FALSE){#TODO
  print("Getting vaccination data (Stage: Vaccinations)")
  
  pops<-districtPops
  #pops$idState<-substr(pops$idDistrict,1,2)
  popsStates<-pops%>%
    group_by(idState)%>%
    summarise(pop=sum(Pop))%>%
    as.data.frame()
  rownames(popsStates)<-popsStates$idState
  
  vaccByState<-case_when(
    dataLoc == "docker" ~ "/mnt/mydata/vaccDataTimeSeries.rds",
    dataLoc == "server" ~ "/var/www/public_files/vaccDataTimeSeries.rds",
    dataLoc == "online_only" ~ "https://iuk-forecast.uniklinik-freiburg.de/vaccDataTimeSeries.rds"
  )%>%readRDS2()
  
  
  tempVaccdataTime<-case_when(
    dataLoc == "docker" ~ "/mnt/mydata/vaccDataTimeSeries.rds",
    dataLoc == "server" ~ "/var/www/public_files/vaccDataTimeSeries.rds",
    dataLoc == "online_only" ~ "https://iuk-forecast.uniklinik-freiburg.de/vaccDataTimeSeries.rds"
  )%>%(function(x){
    
    if(str_detect(x,"http")){
      print("online! v2 - Vaccinations")
      return(NA)
    } else {
      return(file.info(x)$ctime)
    }
  })
  
  if(max(vaccByState$date)<(today()-1)){
    lastDate<-(max(vaccByState$date))
    needAdding<-lastDate+(1:((today()-1)-lastDate))
    
    substi<-vaccByState[vaccByState$date==lastDate,]
    newSet<-bind_rows(lapply(needAdding,function(x){
      thisDate<-substi
      thisDate$date<-x
      return(thisDate)}
    ))
    vaccByState<-(rbind(vaccByState,newSet))
  }
  
  
  vaccByState$Date<-vaccByState$date
  vaccByState$pop<-popsStates[vaccByState$code,"pop"]
  
  keepCols <- c("code","vaccinationsTotal","peopleFirstTotal","peopleFullTotal","peopleBoosterTotal","Date","pop")
  vaccByState <- vaccByState[,..keepCols]
  
  vaccByStatetmp<-data.frame(
    code="99",
    vaccinationsTotal=0,
    peopleFirstTotal=0,
    peopleFullTotal=0,
    peopleBoosterTotal=0,
    Date=as.Date("2020-01-01")+1:600,
    pop=1
  )
  
  
  if(DEBUG){
    print(vaccByState[1:10,])
    print(vaccByStatetmp[1:10,])
  }
  vaccByState<-rbind(vaccByState,vaccByStatetmp)
  
  if(DEBUG) print("Getting vaccination data (Stage: Proportions)")
  vaccByState$vaccinationsProp<-vaccByState$vaccinationsTotal/vaccByState$pop
  vaccByState$peopleFirstProp<-vaccByState$peopleFirstTotal/vaccByState$pop
  vaccByState$peopleFullProp<-vaccByState$peopleFullTotal/vaccByState$pop
  vaccByState$peopleBoosterProp<-vaccByState$peopleBoosterTotal/vaccByState$pop
  vaccByState$dwnldtime<-tempVaccdataTime
  if(DEBUG) print("Done getting vaccination data")
  return(vaccByState)
}


###################################################################
# Pointing data loading to specific country (in our case Germany) #
###################################################################
downloadVaccInfo <- function(dataLoc,districtPops,DEBUG=FALSE){
  downloadVaccInfoDE(dataLoc,districtPops,DEBUG=DEBUG)
}

getDistrictPops<-function(DEBUG=FALSE){
  getDistrictPopsDE(DEBUG=DEBUG)
}

getInciData<- function(dataLoc,DEBUG=FALSE){ 
  getInciDataDE(dataLoc,DEBUG=DEBUG)
}

readHistoricalBedInfo<-function(dataLoc,districtPops,maxDate=(today()-1),DEBUG=FALSE){
  readHistoricalBedInfoDE(dataLoc,districtPops,maxDate=maxDate,DEBUG=DEBUG)
}
getDataLocation<-function(DEBUG=FALSE){
  getDataLocationDE(DEBUG=DEBUG)
}

