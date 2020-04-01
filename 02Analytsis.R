library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(1090401)
basepath = "F:/_MyDocuments/GoogleDrive/Data"

kmtransit = read.csv(file("kmtransit.csv",encoding="UTF-8"),stringsAsFactors = F)
kmtransit$day = ifelse(kmtransit$day=="days",1,0)

km = read.csv(file(paste0(basepath,"/金門隨機點19987_依人口及門牌比例utf8.csv"),encoding="UTF-8"),stringsAsFactors=F)


## PCA ####

km.pca = prcomp(formula=~targetx2+targety2+day+td+transittime,data=kmtransit)
km.pca

plot(km.pca,type="line") 
abline(h=1, col="blue")

## kmeans ####

km.scale = select(kmtransit,-departuretime,-arrivaltime) %>% 
  filter(bustime!=0) %>% 
  scale()
km.kmeans = kmeans(km.scale,55)


kmtransit$cluster = km.kmeans$cluster
km.kmeans = km.kmeans$centers
kmtransit$cluster2 = F 
kmtransit$cluster2[as.numeric(row.names(km.kmeans[km.kmeans[,16]>2,]))] = T

## tree ####

km.tree = rpart(formula=td~targetx1+targety1+targetx2+targety2+d,data=kmtransit)

rpart.plot(km.tree)


## tree2 ####

km.tree = rpart(formula=transittime~targetx2+targety2+d+day,data=kmtransit)

rpart.plot(km.tree)

km$predicttime = lapply(km,function (x) {rpart.predict(km.tree,
                                                       newdata=data.frame(
                                                         targetx2=x$X,targety2=x$Y,d=5000,day=1))[[1]]}
                        )

#加上離五大車站的直線距離

km$predicttime = 0

for (i in 1:nrow(km)) {
  km$predicttime[i] = rpart.predict(km.tree,
                                    newdata=data.frame(
                                      targetx2=km$X[i],targety2=km$Y[i],d=5000,day=1))[[1]]
}

## lm ####

km.lm = lm(formula=transittime~d+targetx2+targety2+day,data=kmtransit)
