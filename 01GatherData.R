library(dplyr)

basepath = "F:/_MyDocuments/GoogleDrive/Data"

km = read.csv(file(paste0(basepath,"/金門隨機點19987_依人口及門牌比例utf8.csv"),encoding="UTF-8"),stringsAsFactors=F)

kmtransit = data.frame(stringsAsFactors = F)

for (m in  1:2) {
  if (m == 1) {
    load(paste0(basepath,"/road/RandomRoadData2_transit_population_07-13.RData"))
    RandomRoadData = RandomRoadData2 ;rm(RandomRoadData2)
    } else {
    load(paste0(basepath,"/road/RandomRoadData2_transit_population_16-22.RData"))
    RandomRoadData = RandomRoadData2 ;rm(RandomRoadData2)
    }
  
  for (i in 1:length(RandomRoadData)) {
    Temp = RandomRoadData[[i]]
    
    if (Temp$status == "OK") {
      
      Temp2 = data.frame(
        targetx1 = km$X[Temp$target[1]],
        targety1 = km$Y[Temp$target[1]],
        targetx2 = km$X[Temp$target[2]],
        targety2 = km$Y[Temp$target[2]],
        d = Temp$d,
        day = ifelse(m==1,"days","nights"),
        x1 = Temp$routes[[1]]$legs[[1]]$start_location$lng,
        y1 = Temp$routes[[1]]$legs[[1]]$start_location$lat,
        x2 = Temp$routes[[1]]$legs[[1]]$end_location$lng,
        y2 = Temp$routes[[1]]$legs[[1]]$end_location$lat,
        departuretime = ifelse(is.null(Temp$routes[[1]]$legs[[1]]$departure_time$text),"",Temp$routes[[1]]$legs[[1]]$departure_time$text),
        arrivaltime = ifelse(is.null(Temp$routes[[1]]$legs[[1]]$arrival_time$text),"",Temp$routes[[1]]$legs[[1]]$arrival_time$text),
        transitdistance = Temp$routes[[1]]$legs[[1]]$distance$value,
        transittime = Temp$routes[[1]]$legs[[1]]$duration$value,
        stringsAsFactors = F
      )
      
      kmtransit = rbind(kmtransit,Temp2)
      rm(Temp);rm(Temp2)
    } else {
      rm(Temp)
    }
    if (i%%1000 ==0) {print(paste(i,Sys.time()))}
  }
  
  
}

kmtransit$transittime = round(kmtransit$transittime/60,1)
kmtransit$transitdistance = round(kmtransit$transitdistance/1000,1)

write.csv(kmtransit,file=paste0(basepath,"/kmtransit.csv"),fileEncoding = "UTF-8")

