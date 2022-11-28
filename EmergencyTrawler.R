#inciData<-read.csv(url("https://www.arcgis.com/home/item.html?id=9644cad183f042e79fb6ad00eadc4ecf#data"))
#library("dplyr")
#library(RJSONIO)

library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(lubridate)


altersgruppe=c("A00-A04","A05-A14","A15-A34","A35-A59","A60-A79","A80%2B","unbekannt")

idLKnrs<-c(1002, 1003,  1001,  1004,  1051,  1053,  1054,  1055, 1056,  1059,  1058,  1057,  1060,  2000,  1062,  1061, 3101,  3153,  3103,  3102,  3151,  3154,  3157,  3158, 3155,  3159,  3252,  3251,  3241,  3254,  3255,  3257,
3256,  3351,  3354,  3352,  3355,  3353,  3358,  3359, 3357,  3356,  3360,  3402,  3361,  3401,  3403,  3405, 3451,  3404,  3452,  3454,  3455,  3453,  3456,  3459, 3458,  3460,  3457,  3462,  3461,  4012,  4011,  5111,
5113,  5114,  5112,  5116,  5119,  5120,  5117,  5122, 5154,  5124,  5158,  5162,  5170,  5314,  5166,  5315, 5334,  5362,  5316,  5358, 5366,  5378,  5374,  5370, 5382,  5512,  5515,  5513, 5554,  5566,  5562,  5558,
5570,  5754,  5762,  5758,  5711,  5770,  5766,  5911, 5774,  5913, 5914,  5916,  5915,  5954,  5966,  5958,   5962,  5970,  6411,  5974,  5978,  6412,  6431,6414,   6413,  6432,  6433,  6436,  6435,  6434,  6437, 6440,
  6439,  6438,  6531,  6532,  6534,  6533,  6535,  6611,  6631,  6632,  6633,  6634,  6636,  7111,  6635,  7131,   7133,  7132,  7134,  7135,  7138,  7140,  7137,  7141,   7143,  7211,  7231,  7232,  7311,  7235,  7312,  7233,
  7313,  7315,  7316,  7314,  7318,  7320,  7319,  7317,   7331,  7334,  7333,  7332,  7335,  7336,  7337,  7338,   7339,  8111,  8115,  7340,  8116,  8121,  8118,  8117,   8119,  8127,  8125,  8128,  8135,  8126,  8136,  8211,
  8212,  8215,  8216,  8221,  8222,  8225,  8235,  8226,   8231,  8236,  8311,  8316,  8315,  8237,  8325,  8326,   8317,  8327,  8335,  8337,  8336,  8415,  8416,  8425,   8417,  8421,  8426,  8436,  8435,  8437,  9161,  9171,
 9172,  9162,  9163,  9174,  9176,  9175,  9173,  9177,   9180,  9181,  9179,  9178,  9182,  9183,  9184,  9185,   9187,  9186,  9188,  9189,  9262,  9261,  9190,  9263,   9272,  9271,  9273,  9274,  9275,  9278,  9277,  9276,
  9279,  9361,  9362,  9363,  9371,  9372,  9373,  9374,   9375,  9376,  9461,  9377,  9462,  9472,  9464,  9471,   9463,  9473,  9475,  9476,  9474,  9477,  9561,  9478,   9479,  9562,  9564,  9565,  9563,  9571,  9573,  9574,
  9572,  9575,  9577,  9661,  9576,  9662,  9663,  9671,   9672,  9673,  9674,  9675,  9676,  9677,  9678,  9762, 9679,  9761,  9763,  9772,  9773,  9764,  9771,  9776,   9775,  9774,  9777,  9778,  9780,  9779, 10041, 10042,
 10043, 10044, 10045, 10046, 11003, 11002, 11001, 11004,  11005, 11007, 11006, 11008, 11012, 11010, 11009, 11011,  12051, 12052, 12053, 12054, 12060, 12063, 12061, 12062,  12064, 12067, 12065, 12066, 12068, 12071, 12072, 12069,
 12070, 13004, 13071, 13003, 12073, 13072, 13073, 13074,  13075, 13076, 14521, 14511, 14522, 14523, 14612, 14625,  14524, 14626, 14628, 14729, 14713, 14627, 14730, 15003,  15002, 15001, 15081, 15084, 15083, 15082, 15085, 15086,
 15088, 15087, 15089, 16051, 15090, 15091, 16052, 16055,  16053, 16054, 16063, 16064, 16062, 16061, 16065, 16066, 16068, 16067, 16069, 16072, 16070, 16071, 16073,  16076, 16075, 16074, 16077)

idLKchar<-str_pad(as.character(idLKnrs),width=5,side="left",pad="0")

getSingleDatenstand<-function(){
  
  RKIgot<-GET(paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/Covid19_hubv/FeatureServer/0/query?where=ObjectId%20%3D%20'",
                     "1271011","'&outFields=*&outSR=4326&f=json"))
  tempRKIdataJSON<-fromJSON(content(RKIgot,"text"))
  tempDF<-as.data.frame(tempRKIdataJSON$features)$attributes
  tempDF$Meldedatum<-as.Date("1970-01-01")+(tempDF$Meldedatum/(3600*24*1000))
  tempDF$Datenstand<-as.Date(substr(tempDF$Datenstand,0,10),format="%d.%m.%Y")
  return(tempDF[1,"Datenstand"])
}

getLK_age_data<-function(LK,age){
  #print(paste0("Requesting ",age," for LK ",LK))
  RKIgot<-GET(paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/Covid19_hubv/FeatureServer/0/query?where=IdLandkreis%20%3D%20'",
                     LK,"'%20AND%20Altersgruppe%20%3D%20'",age,"'&outFields=*&outSR=4326&f=json"))
  tempRKIdataJSON<-fromJSON(content(RKIgot,"text"))
  tempDF<-as.data.frame(tempRKIdataJSON$features)$attributes
  tempDF$Meldedatum<-as.Date("1970-01-01")+(tempDF$Meldedatum/(3600*24*1000))
  tempDF$Datenstand<-as.Date(substr(tempDF$Datenstand,0,10),format="%d.%m.%Y")
  return(tempDF)
}

getLKdata<-function(LK){
  #print(paste0("Requesting LK ",LK))
  RKIgot<-GET(paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/Covid19_hubv/FeatureServer/0/query?where=IdLandkreis='",
                   LK,
                   "'&outFields=*&f=json")) 
  
  tempRKIdataJSON<-fromJSON(content(RKIgot,"text"))
  tempDF<-as.data.frame(tempRKIdataJSON$features)$attributes
  tempDF$Meldedatum<-as.Date("1970-01-01")+(tempDF$Meldedatum/(3600*24*1000))
  tempDF$Datenstand<-as.Date(substr(tempDF$Datenstand,0,10),format="%d.%m.%Y")
  return(tempDF)
}

heavyDataTrawler<-function(mySubset=(1:length(idLKchar))){
  listLKdata<-lapply(idLKchar[mySubset],getLKdata)
  gc()
  maxedOuts<-(unlist(lapply(listLKdata,nrow))==25000)
  if(sum(maxedOuts)>0){
    print("Some maxed out.")
    repeatLKs<-idLKchar[mySubset][maxedOuts]
    print(repeatLKs)
    listLKdata<-c(listLKdata[mySubset][!maxedOuts],
                  lapply(repeatLKs,function(lk)
                    bind_rows(
                      lapply(altersgruppe,function(a) getLK_age_data(lk,a)))))
  }  
  
  nullResults<-unlist(lapply(listLKdata,is.null))
  if(sum(nullResults)>0){
    print("Missed a couple of LKs")
    repeatLKs<-idLKchar[mySubset][nullResults]
    print(repeatLKs)
    listLKdata<-c(listLKdata[mySubset][!nullResults],lapply(repeatLKs,getLKdata))
  }
  #unlist(lapply(listLKdata,nrow))
  # I don't think the following statement actaully works.
  if(sum(unlist(lapply(listLKdata,is.null)))>0) {
    return(listLKdata)
    listLKdata<-NULL
  }
  
  listLKdata<-bind_rows(listLKdata)
  gc()
  allCodeCombo<-unique(listLKdata[,c("IdLandkreis","Landkreis","Bundesland","IdBundesland")])
#  print(allCodeCombo)
  
  dummyRecs<-bind_rows(
    lapply(idLKchar[mySubset],
    function(LK){
      LK<-as.numeric(LK)
      #print(paste0("Creating dummy for LK ",LK))
      #print(nrow(allCodeCombo[allCodeCombo$IdLandkreis==LK,]))
      tLandkreis<-allCodeCombo[allCodeCombo$IdLandkreis==LK,"Landkreis"][[1]]
      tBundesland<-allCodeCombo[allCodeCombo$IdLandkreis==LK,"Bundesland"][[1]]
      tIdBundesland<-allCodeCombo[allCodeCombo$IdLandkreis==LK,"IdBundesland"][[1]]
      data.frame(
            IdLandkreis=LK,
            Landkreis=tLandkreis,
            Bundesland=tBundesland,
            IdBundesland=tIdBundesland,
            Altersgruppe="A00-A04",
            Geschlecht="M",
            ObjectId=-1,
            NeuerFall=0,
            NeuerTodesfall=0,
            Refdatum=as.Date("2000-01-01"),
            NeuGenesen=0,
            IstErkrankungsbeginn=0,
            Altersgruppe2="Nicht übermittelt",
            Meldedatum=min(listLKdata$Meldedatum)+(0:(difftime(max(listLKdata$Meldedatum),min(listLKdata$Meldedatum)))),
             AnzahlFall=0,
             AnzahlTodesfall=0,
             AnzahlGenesen=0,
             Datenstand=max(listLKdata$Datenstand)
      )
  })
  )

  dfAllData<-
    rbind(listLKdata,dummyRecs)%>%
    group_by(IdLandkreis,Landkreis,Meldedatum,Bundesland,IdBundesland)%>%
      summarise(AnzahlFall=sum(AnzahlFall),
            AnzahlTodesfall=sum(AnzahlTodesfall),
            AnzahlGenesen=sum(AnzahlGenesen),
            Datenstand=max(Datenstand))%>%as.data.frame()
  gc()

#Getting the data back into the poor, but standard, format
  dfAllData$IdLandkreis<-as.numeric(dfAllData$IdLandkreis)
  dfAllData$Datenstand<-paste0(str_pad(day(dfAllData$Datenstand),width=2,side="left",pad="0"),".",str_pad(month(dfAllData$Datenstand),width=2,side="left",pad="0"),".",year(dfAllData$Datenstand))
  dfAllData$Meldedatum<-
    paste0(year(dfAllData$Meldedatum),"/",
                             str_pad(month(dfAllData$Meldedatum),width=2,side="left",pad="0"),"/",
                            str_pad(day(dfAllData$Meldedatum),width=2,side="left",pad="0"))
  return(dfAllData) 

}
