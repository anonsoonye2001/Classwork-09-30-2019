load("fish_data.Rdata")

f<-fish
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
fd3<-filter(.data=f, depth_fac=="Deep")

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


#merging 2 dataframes ensuring that observations from one dataframe are connected with
#observationin the second data frame correctly
m1<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac"))
m1$seq<-seq(1,nrow(m1),1)
head(m1)
m2<-subset(x=f,depth_fac=="Deep", select=c("transect.id","area_fac","parcel.length.m","group"))
m2$seq<-seq(1,nrow(m2),1)
head(m2)

# create a sequence of data
m2$seq<-seq(from=1, to=nrow(m2),by=1)
v<-seq(5,20,0.5)
vc<-cut(x=v,breaks=c(1:2),labels = T,include.lowest = T)

##merge I
?merge()
mt<-merge(x=m1, y=m2,by=c("transect.id","seq"),all.x=T,no.dups=T)
nrow(m1)+nrow(m2)

##join
library(dplyr)
mj<-dplyr::right_join(x=m1,y=m2,by=c('transect.id'))
nrow(m1)+nrow(m2)
