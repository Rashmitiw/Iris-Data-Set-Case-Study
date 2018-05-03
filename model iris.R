data(iris)
dataset<-iris

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

#summarize data
#dimension of data

dim(dataset)

#type of attribute

sapply(dataset,class)

#peal at the data
head(dataset)

#levels of the class
#list the level for the class

levels(dataset$Species)

#Class Distribution
#summarize the  class distribution
percentage<-prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species),percentage=percentage)


#Statistical summary
summary(dataset)

#Visulaize dataset

#univariate plot to understand each attribute
x<-dataset[,1:4]
y<-dataset[,5]

#boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4){}
  boxplot(x[,i],main=names(iris)[i])
  }

plot(y)

#multivariate plot
#scatterplot matrix 
featurePlot(x=x,y=y,plot="ecllipse")
featurePlot(x=x,y=y,plot="box")

#density plots for each attribute  by class value
scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot="density",scales=scales)

#Evaluate some algorithm
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#a)linear algorithm 
set.seed(7)
fit.lda<-train(Species~.,data=dataset,method="lda",metric=metric,trControl=control)

#b) non linear algorithm
#CART
set.seed(7)
fit.cart<-train(Species~.,data=dataset,method="rpart",metric=metric,trControl=control)
#kNN
set.seed(7)
fit.knn<-train(Species~.,data=dataset,method="knn",metric=metric,trControl=control)
# c) advanced algorithms
#SVM
set.seed(7)
fit.svm<-train(Species~.,data=dataset,method="svmRadial",metric=metric,trControl=control)

#Random Forest

set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)
print(fit.lda)

predictions<-predict(fit.lda,validation)
confusionMatrix(predictions,Validation$Species)
