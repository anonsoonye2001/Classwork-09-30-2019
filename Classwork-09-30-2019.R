load("fish_data.Rdata")
F<-Fish
 # subsetting---
# indexing by condition
fd<- f[f$depth_fac=="Deep",]
fd2<-subset(x=f,depth_fac=="Deep")
#shallow tows
#east
#patches

fd4<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac"))
##filter
library(dplyr)
fd3<-filter(.datra=f, depth_fac=="Deep")

##which
fd5<-f[which(f$depth_fac=='Deep' & f$area_fac=="east"),]
fd6<-f[which(f$depth_fac=='Deep' &f$area_fac=="east")&f$yr_fac!="2014",]
str(fd6)
head(fd6)

#subset & combineusing rowbind(rbind function)----
d1<-f[which(f$depth_fac=='Deep' & f$area_fac=="east"),]
d2<-f[which(f$depth_fac=='shallow' & f$area_fac=="west"),]
#combine d1 and d2 into a single dataframe
nrow(d2)
nrow(d1)+nrow(d2)
d3<-rbind(d1,d2)
nrow(d3)

#combine dataframes with separate columns into a single dataframe
c1<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac"))
c2<-subset(x=f,depth_fac=="Deep", select=c("area_fac","parcel.length.m","group"))
c3<-cbind(c1,c2)
head(c3)
##filter