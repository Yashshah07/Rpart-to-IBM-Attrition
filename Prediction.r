rm(list=ls())   # Clears the environment , removes all the objects 

## Applying Rpart to IBM Attrition V2 dataset ##
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

## load the file
rm(list=ls())
Emp1 <-read.csv("IBM_Employee_Attrition_V2.csv")
View(Emp1)

?na.omit() ## remove all missing records ##

index <- seq (1,nrow(Emp1),by=5)
test<-Emp1[index,]
training<-Emp1[-index,]

table(Emp1$Attrition)

#rpart()

mytree <- rpart(factor(Attrition)~. , data = Emp1)
summary(mytree)
table(Emp1$Attrition,Emp1$Gender)
prp(mytree)

##Prediction 
prediction <-predict( mytree ,test , type="class" )
table(actual=test[,2],prediction)
wrong<- (test[,2]==prediction)*100
R_part <- sum(wrong)/length(wrong)
View(R_part)
