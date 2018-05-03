library(tidyverse)
library(gridExtra)
library(wesanderson)
library(dplyr)

Iris_n<-read.csv("iris.csv")

plot1<-ggplot(data=Iris_n,aes(x=SepalWidthCm,y=SepalLengthCm,color=Species))+
  geom_point()+geom_smooth()+ scale_color_manual(values=wes_palette(n=3,name='GrandBudapest'))

plot2<-ggplot(data=Iris_n,aes(x=PetalWidthCm,y=PetalLengthCm,color=Species))+
  + geom_point()+scale_color_manual(values = wes_palette(n=3,name='Moonrise2'))

grid.arrange(plot1, plot2, nrow=2, ncol=1)

### Density Plot of all the measurements 

plot10<-ggplot(Iris_n,aes(SepalLengthCm,fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette = 3)+
  theme(legend.position = "right")
plot11<-ggplot(Iris_n,aes(SepalWidthCm,fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette = 4)+
  theme(legend.position = "right")
plot12<-ggplot(Iris_n,aes(PetalLengthCm,fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette = 5)+
  theme(legend.position = "right")
plot13<-ggplot(Iris_n,aes(PetalWidthCm,fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_brewer(palette = 6)+
  theme(legend.position = "right")

grid.arrange(plot10, plot11,plot12,plot13, nrow=2, ncol=2)



## To calculate the average Sepal and petal measurements of the 3 Species of Iris 
  Iris_Species=group_by(Iris_n,Species)
  Mean_Calculations=summarise(Iris_Species,MeanSL=mean(SepalLengthCm),
                              MeanSW=mean(SepalWidthCm),
                              MeanPL=mean(PetalLengthCm),
                              MeanPW=mean(PetalWidthCm))
  Mean_Calculations<-data.frame(Mean_Calculations)
  Mean_Calculations
  
  
  ## K means Clustering on Iris Data 
  library(plotly)
  # training data
  trainingData <- Iris_n[,(2:5)]
  # plot with two attributes: Sepal.Length Sepal.Width 
  plotData <- Iris_n[,c(2,4,6)]
  #  -------- K-Means
  library(broom)
  modelkmeans<-kmeans(trainingData,3,n=100)
  modelkmeans$cluster<-factor(modelkmeans$cluster)

  
  classified_data_aug<-(augment(modelkmeans,Iris_n))
  tidied_data<-tidy(modelkmeans)
  
  plotly(classified_data_aug,x=~SepalLengthCm,y=~PetalLengthCm,color=~.cluster,symbol=~Species,
         type="scatter",mode="markers",marker=list(size=20,opacity=0.5),
         text=~paste("Classifier:",modelkmeans$cluster,"</br>Species:",Species),
         showlegend=FALSE)%>%layout(title="K Means Ckustering vs actual  species  Classification")
  
  comparekmeans<-table(modelkmeans$cluster,Iris$Species)
  
  library(cluster)
  clusplot(plotData,modelkmeans$cluster,color=TRUE,shade=TRUE,labels=5,lines=0)
  
  ## Heirarchical clustering on Iris Data 
  d_iris <- dist(Iris[,2:5])
  
  hc_iris <- hclust(d_iris, method = "complete")
  iris_species <- rev(levels(Iris[,6]))
  library(dendextend)
  library(colorspace)
  dend <- as.dendrogram(hc_iris)
  dend <- rotate(dend, 1:150)
  dend <- color_branches(dend, k=3) 
  labels_colors(dend) <-
    rainbow_hcl(3)[sort_levels_values(
      as.numeric(Iris[,6])[order.dendrogram(dend)]
    )]
  labels(dend) <- paste(as.character(Iris[,6])[order.dendrogram(dend)],
                        "__",labels(dend),
                        sep = "")
  dend <- hang.dendrogram(dend,hang_height=0.1)
  dend <- set(dend, "labels_cex", 0.5)
  # And plot:
  par(mar = c(3,3,3,7))
  plot(dend,main = "Heirarchical Clustered Iris data set", 
       horiz =  TRUE,  nodePar = list(cex = .007))
  legend("topleft", legend = iris_species, fill = rainbow_hcl(3))
    
  