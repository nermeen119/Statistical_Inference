# Task 1
calc <- function(x = 10){USArrests$UrbanPop*x}
calc()


#Task2,3
par(mfrow = c(1,2))
rape<-USArrests$Rape
hist(rape,col = "lightblue",xlab="rape")
assul<-USArrests$Assault
plot(assul,ylab = "Assault",ylim = c(45,100),pch=4) 
mtext("USArrests Plot", side = 3, line =2) 


#task4
library(dplyr)
result <- USArrests %>% filter(Murder<5) %>% arrange(desc(Murder))
result


#task5

Murders<-(USArrests$Murder)
Rapes<-(USArrests$Rape)
Assults<-(USArrests$Assault)
t.test(Murders,Rapes,var.equal=T)
#Accept the null hypothesis
t.test(Murders,Assults,var.equal=F)
#Reject the null hypothesis


#task6
murder<-USArrests$Murder
assult<-USArrests$Assault
Rape<-USArrests$Rape
urban<-USArrests$UrbanPop

fit<-lm(urban~murder+assult+Rape)
lm(formula =urban~murder+assult+Rape)
attributes(fit)
cor(USArrests$UrbanPop,USArrests$Rape)
predictions<-(fit$coefficients[1]+fit$coefficients[2]*11+fit$coefficients[3]*110+fit$coefficients[4]*21.2)
predictions



#task7 bonous

library(e1071)
banPop<-factor(USArrests$UrbanPop)
murder<-USArrests$Murder
assult<-USArrests$Assault
Rape<-USArrests$Rape
urban<-USArrests$UrbanPop
newdat <- rbind(USArrests[1:5,],USArrests[11:20,])
newdat <- rbind(newdat,USArrests[31:40,])
testdt<-rbind(USArrests[6:10,],USArrests[21:30,])
testdt<-rbind(testdt,USArrests[41:50,])
classyn <- naiveBayes(UrbanPop~.,data=newdat)
Urban_Prediction<-predict(classyn,newdata=testdt)
Urban_Prediction
table(Urban_Prediction)




