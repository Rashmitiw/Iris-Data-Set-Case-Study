library(dplyr)
tbl_df(iris)
summarise(iris,mean(Petal.Length))
 
summarise(iris,average=mean(Petal.Length),standard_deviation=sd(Petal.Length))

summarise(iris,correlation=cor(Petal.Width,Petal.Length))

summarise_each(iris,funs(mean))

summarise_each(iris,funs(mean),Petal.Length,Petal.Width)
summarise_each(iris,funs(mean),-Species)

summarise_each(iris, funs(mean), starts_with("Petal"))

summarise_each(iris, funs(mean), ends_with("Length"))

summarise_each(iris, funs(mean), contains("."))
summarise_each(iris, funs(mean), matches("^S.*th$"))
columns <- c("Petal.Length", "Sepal.Width")
summarise_each(iris, funs(mean), one_of(columns))

mutate_each(iris, funs(. * 10), -Species) %>% tbl_df


#Grouping 
iris%>% group_by((Species)%>%summarise(mean(Petal.Length)))
iris%>%group_by(Species)%>%summarise(n(),mean(Petal.Length),sd(Petal.Length))

iris%>%group_by(Species)%>%summarise(N=n(),'Average deviation petal length'=sd(Petal.Length))%>%
  mutate_each(funs(signif(.,digits = 2)),'Average petal Length','Standard Deviation petal length')


iris%>%group_by(Species)%>%summarise(n=n())
count(iris,Species)
