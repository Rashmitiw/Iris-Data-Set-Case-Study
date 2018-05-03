library(dplyr) #for data manipulation grammar (the pipe operator)
library(tidyr) #for tidying up the data
library(ggplot2) #for data viz
#loading data
iris1<-read.table("iris.txt",comment.char = "#",header=FALSE,sep=",",na.strings = "")
dim(iris1)
summary(iris)#provides stat summary of the data

cnames<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
names(iris1)<-cnames
head(iris1)

# Transformation
long_iris <- iris%>%
  gather(part,value,Sepal.Length,Sepal.Width,Petal.Length ,Petal.Width)%>%
  separate(part, c('part', 'measure'), sep = '\\.')
head(long_iris, n=10)
sapply(long_iris,class)
fcts<-c('part','measure')
long_iris[fcts]<-lapply(long_iris[fcts],as.factor)
sapply(long_iris,class)
Missing_d<- function(x){sum(is.na(x))/length(x)*100}
apply(long_iris,2,Missing_d)
is_special<-function(x){if(is.numeric(x))! is.finite(x) else is.na(x)}
sapply(long_iris,is_special)
sum(is.na(long_iris$value))
p<ggplot(long_iris,aes(x=Species,y = value , col=part))
p+geom_jitter(alpha=0.4,size=0.8)+facet_grid(.~measure)
p+geom_jitter(alpha=0.3,size=0.8)+stat_boxplot(alpha=0.5)+facet_grid(.~measure)
p+geom_jitter(alpha=0.5,size=0.8)+stat_boxplot(alpha=0.5)+facet_grid(.~part)





iris$Flower<-1:nrow(iris)
#create wide_iris
wide_iris<-iris%>% 
  gather(key,value,-Species,-Flower)%>%
  separate(key,C("Part","Measure"),sep="\\.")%>%
  spread(Measure,value)
  
head(wide_iris,n=10)


q<-ggplot(wide_iris,aes(x=width,y=Length,col=Species))
q+geom_jitter(alpha=0.4,size=0.8)+facet_grid(.~Species)+
  stat_smooth(method='lm',se=F)
q+geom_jitter(alpha=0.4,size=0.8)+facet_grid(.~Part)
q+geom_point(alpha=0.4,size=0.8)+stat_smooth(method='lm',fullrange=T,size=0.5)
