library(ggplot2) #Data visualization
library(readr)#CSV file I/O 
library(RColorBrewer)
library(reshape)

df<-read.csv("iris.csv")
head(df)
summary(df)

sapply(df,function(x) sum(is.na(x)))

names(df)

#Sepal Length vs Width

ggplot(data=df,aes(x=SepalLengthCm,y=SepalWidthCm,color=Species))+ 
  geom_point(size=3)+ggtitle('SepalLength vs Width') + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Sepal Length vs Petal Length

ggplot(data=df,aes(x=SepalLengthCm,y=PetalLengthCm,color=Species)) +
   geom_point(size=3) + ggtitle('SepalLength vs PetalLength') + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Sepal LengthCm per Species

ggplot(data=df,aes(x=Species,y=SepalLengthCm)) +
  geom_boxplot(aes(fill=Species),outlier.size = 4) +
  scale_fill_discrete(name='Species') + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('SepalLengthCm per Species')

#PetalLengthCm per Species
ggplot(data=df,aes(x=Species,y=PetalLengthCm))+
  geom_boxplot(aes(fill=Species),outlier.size = 4)+
  scale_fill_discrete(name='Species')+theme_bw() +
  theme(plot.title = element_text(hjust=0.5))+
    ggtitle('PetalLengthCm per Species')

#SepalWidthCm per Species
ggplot(data=df,aes(x=Species,y=SepalWidthCm))+
  geom_boxplot(aes(fill=Species),outlier.size = 4)+
  scale_fill_discrete(name='Speceis')+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle('SepalWidthCm per Species')

#PetalLengthCm Distribution per Species

ggplot(data=df,aes(x=PetalLengthCm,fill=Species))+
  geom_bar(color='black')+
  scale_fill_discrete(name='Species')+
  theme_bw()+
  theme(plot.title = element_text(hjust =0.5))+
  ggtitle('PetalLengthCm distribution per species')

      
#SepalLengthCm Distribution per Species
ggplot(data=df,aes(x=SepalLengthCm,fill=Species))+
  geom_bar(color='black')+
  scale_fill_discrete(name='Species')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle('SepalLengthCm Distribution per Species')

#SepalWidthCm Distribution per Species
ggplot(data=df,aes(x=SepalWidthCm,fill=Species))+
  geom_bar(color='black')+
  scale_fill_discrete(name='Species')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle('SepalWidthCm Distribution per Species')


#SepalWidthCm Distribution per Speceis with density curve
ggplot(data=df,aes(x=SepalWidthCm))+
  geom_density(alpha=0.5,aes(group=Species,color=Species))+
                 theme_bw()+
                 theme(plot.title = element_text(hjust=0.5))+
                 ggtitle('SepalWidth Distribution per Species')
               
               