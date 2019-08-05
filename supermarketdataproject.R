library(dplyr)
library(arules)
library(ggplot2)
coles<- read.csv(file = "Simulated Coles Data1.csv",header= TRUE,nrows = 58000)
attach(coles)

coles$homeown<- factor(coles$homeown,labels = c("Yes","No","Unknown"))
coles$pmethod<- factor(coles$pmethod,labels = c("cash","creditCard","eftpos","other"))
coles$sex<- factor(coles$sex,labels = c("male","female"))
which(coles$PostCode=="")
summary(coles$PostCode,maxsum=10000)

####
levels(coles$fruit)<-c("0","1",NA,NA,NA,NA,NA,NA)
coles$fruit[is.na(coles$fruit)] = "0"
coles$fruit<- as.numeric(coles$fruit)
coles$fruit[coles$fruit==1]<-0
coles$fruit[coles$fruit==2]<-1
coles$fruitjuice[coles$fruitjuice==2]<-1
coles$cannedveg[is.na(coles$cannedveg)] = 0
coles$confectionery[is.na(coles$confectionery)] = 0
coles$milk[is.na(coles$milk)] = 0
coles$PizzaBase[is.na(coles$PizzaBase)] = 0
coles$cereal[is.na(coles$cereal)] = 0
summary(coles)
colesmarket<-coles[-(1:9)]
summary(colesmarket)

colesmarket<-as(colesmarket ,"matrix")
colesmarket<-as(colesmarket ,"itemMatrix")

colesrules1 <- apriori(colesmarket,
      parameter = list(minlen = 2, maxlen = 3, supp = 0.10,conf= 0.80))
inspect<-inspect(sort(colesrules1, decreasing = TRUE, by = "lift")[1:10])
knitr::kable(inspect)
itemFrequencyPlot(items(colesrules1),decreasing=TRUE,topN =50,ylim=c(0,0.6))
itemFrequencyGGPlot <- function(x, topN) {
  library(tidyverse)
  x %>%
    itemFrequency %>%
    sort %>%
    tail(topN) %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    ggplot(aes(reorder(rowname, `.`),`.`)) + 
    labs(title = "Purchase frequency of products",x = "Products")+
    geom_col() + 
    coord_flip()
}  
itemFrequencyGGPlot(colesmarket, 50)

###plots
library(mtcar)
ggplot(coles,aes(coles$pmethod,fill=pmethod))+
  geom_bar()+coord_flip()+
  labs(title = "Barplot of paying methods",x = "Paying methods")

ggplot(coles,aes(coles$sex,fill=sex))+
  geom_bar(stat='count')+ coord_flip()+
  labs(title = "Barplot of sex",x = "Sex")

ggplot(coles, aes(coles$sex, coles$Value,color=sex  ) )+ 
  geom_point() + geom_boxplot()+ coord_flip()+
  labs(title = "Boxplot of transaction value by sex",
       y="Value of transaction",x = "Sex")

ggplot(coles,aes(coles$homeown,fill=homeown))+
  geom_bar(stat='count')+ coord_flip()+
  labs(title = "Barplot of owning a house",x = "Owning a house")

ggplot(coles,aes(coles$sex,fill=homeown))+
  geom_bar(stat='count')+ coord_flip()+
  labs(title = "Barplot of owning a house by sex",x = "Sex")

ggplot(coles, aes(x="", y=coles$Value) )+
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+ coord_flip()+
  labs(title = "Boxplot of value of transactions",
       x = "customers",y="Value of transactions")

ggplot(coles,aes(x=coles$sex,y=coles$income))+
  geom_boxplot()+
  coord_flip()+
  labs(title = "Boxplot of income of customers by sex",
       x = "Sex",
       y="Income of customers")

ggplot(coles,aes(x="",y=coles$income))+
  geom_boxplot()+
  coord_flip()+
  labs(title = "Boxplot of income of customers ",
       x = "customers",
       y="Income of customers")                


ggplot(coles, aes(coles$nchildren, 
                  fill = factor(coles$nchildren) ) ) +
  geom_bar(position="dodge") + 
  scale_fill_discrete(name="Number of children")+
  labs(title = "Barplot of Number of children",
       x = "Number of children")


ggplot(coles,aes(coles$age))+geom_histogram(binwidth = 1)+
  labs(title = "Histogram of the Age",x = "Age")


ggplot(coles, aes(coles$homeown, coles$Value  ) ) +
  geom_point() + geom_boxplot()


prop.table(table(coles$sex))
prop.table(table(coles$nchildren))

#clustering
summary(coles$income)
coles$age[is.na(coles$age)] = 38
coles$income[is.na(coles$income)] = 70169
coles$nchildren[is.na(coles$nchildren)] = 1
coles$income[which(coles$income>500000)]<-70169
summary(nchildren)
coles$nchildren[is.na(coles$nchildren)]<-1
coles$nchildren[which(coles$nchildren>6)]<-1
hist(nchildren)
str(Value)
boxplot(income)
coles$Value[which(coles$Value>500)]<-63.50

set.seed(123)
kcoles<-kmeans(coles[,c(2,6,7,9)],centers = 2,nstart = 50)

kcoles$cluster <- as.factor(kcoles$cluster)

kcoles$centers

wss <- rep(0,6)
for (i in 1:6) {
  wss[i] <- sum(kmeans(coles[,c(2,6,7,9)], centers = i)$withinss)
}

plot(1:6, wss,
     type="b", pch = 19,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



ggplot(coles, aes( income,age, color = kcoles$cluster))+geom_density2d()
ggplot(coles, aes( nchildren,age, color = kcoles$cluster))+geom_density2d()
ggplot(coles, aes( Value,nchildren, color = kcoles$cluster))+geom_point()
ggplot(coles, aes( income,Value, color = kcoles$cluster))+geom_density2d()+scale_fill_discrete(name="")
ggplot(coles, aes( age,Value, color = kcoles$cluster))+geom_density2d()


