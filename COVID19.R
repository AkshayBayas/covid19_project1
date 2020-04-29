#project#


CDH <- read.csv(file = "covid_19_data.csv", header = TRUE,na.strings=c("","NA"))
CDH
Data1 <- CDH
summary(CDH)

#CDH <- na.omit(CDH)

View(CDH)
#---------------------------------------------------------------------------#
#keep same date format for data set
class(Data1)
str(Data1)

LD <- as.Date(Data1$Last.Update, format=' %m %d %y')
OD<- as.Date(Data1$ObservationDate, format =' %m %d %y' )
View(LD)

Data1$ObservationDate<- as.factor(Data1$ObservationDate)
Data1$Last.Update<- as.factor(Data1$Last.Update)
summary(Data1)
View(CDH)
str(Data1)
summary(CDH)
Data1
#---------------------------------------------------------------------------#
P<- function(X)
{ sum(is.na(X))/ length(X)*100}

apply(CDH, 2,P)

library(mice)
md.pattern(CDH)
md.pairs(CDH)


#replace NA data with country of respective Province state.

Data1[is.na(Data1$Province.State)]
Data1$Province.State<- as.character(Data1$Province.State)
Data1$Province.State[(Data1$Province.State == " ")] <- NA

Data1$Province.State[which(is.na(Data1$Province.State))]<-'other_region'


View(Data1)
summary(Data1)




#graphical representtion country vise .. 
class(Data1)
Data1<- as.data.frame(Data1)
#confirmed cases country vise:

library(ggplot2)

ggplot(Data1, aes(x= Country.Region, fill = Confirmed))+ geom_bar()

summary(Data1)

ggplot(Data1, aes(x=Country.Region, fill = Deaths))+ geom_bar()

ggplot(Data1, aes(x= Country.Region, fill = Confirmed))+ geom_boxplot()

head (ggplot(Data1, aes(x = Deaths, y = Last.Update)) + geom_point())

library(rJava)
library(iplots)



plot(Data1$Deaths,Data1$Confirmed )
plot(Data1$Deaths, Data1$Recovered)
plot(Data1$Confirmed, Data1$Recovered)


#-------------------------------------------------------------------------------------#
View(Data1)

pairs(Data1[6:8])



#-------------------------------------------------------------------------------------#

# building linear model for the COnfirmed cases vs Recovered and Deaths
library(caTools)

St <- sample.split(Data1$Confirmed, SplitRatio = 0.60)
Train<- subset(Data1, St == T)
Test <- subset(Data1 , St == F)
nrow(Train)
nrow(Test)

#Model Confirm vs Recovered

Model_conf_rec <- lm(Confirmed~Recovered, data = Train)
summary(Model_conf_rec)
#p-value: < 2.2e-16,   Multiple R-squared:  0.454,
1-2.2e-16

Result1 <- predict(Model_conf_rec, newdata = Test)

Result1

#find out error:

View(Result1)

FD1 <- table(Actual = Test$Confirmed, Predicted = Result1)
FD<- as.data.frame(FD1)
Error <- FD$Actual-FD$Predicted

Actual <- Test$Confirmed
Predicted <- Result1
View(Predicted)
View(Actual)

Error <- Actual - Predicted

View(Error)
Final_Data <- cbind(Actual,Predicted, Error)

Final_Data

class(Final_Data)
FD<- as.data.frame(Final_Data)
View(FD)
plot(FD$Actual, FD$Predicted)
plot(FD$Error)

ggplot(FD, aes(x= Actual, y = Predicted))+ geom_point()+ geom_smooth(method = "lm")+ ggtitle("Plot Linear progression model")
RMS <- sqrt(mean((FD$Error)^2))  
RMS
11719.71
#-----------------------------------------------------------------------#
#Multiple linear progression MOdel (M2)

M2 <- lm(Confirmed~Deaths+Recovered, data = Train)
M2

summary(M2)
# p-value: < 2.2e-16

#Analysis of variance
anova(Model_conf_rec,M2)

Result2 <- predict(M2, newdata = Test)

View(Result2)

Act2 <- Test$Confirmed
Pred2<- Result2
Error2 <- Act2-Pred2

cbind(Act2,Pred2,Error2)->FD2
FD2
FD2 <- as.data.frame(FD2)
plot(FD2$Act2,FD2$Pred2)
ggplot(FD2, aes(x= Act2, y = Pred2))+ geom_point()+ geom_smooth(method = "lm")
#root mean square
RMS2 <- sqrt(mean((FD2$Error2)^2))
RMS2
5955.463
#---------------------------------------------------------------------------------------#
library(dplyr)
library(broom)

#Model 1 = Model_conf_rec 
#Moodel 2 = M2

ggplot(augment(Model_conf_rec), aes(y =Confirmed, x = Recovered)) +
  geom_point()+geom_line(aes(y=.fitted), size = 1, col = "blue")+
    labs(x = "Total recovered cases", y= " Total Confirmed case", title = "Linear Regression Model")


#---plot via geom smooth
ggplot(augment(Model_conf_rec), aes(y =Confirmed, x = Recovered)) +geom_point()+
  geom_smooth(method = "lm", se= FALSE)+
  labs(x = "Total recovered cases", y= " Total Confirmed case", title = "Linear Regression Model")

#Model 2

ggplot(augment(M2), aes(y =Confirmed, x = Recovered+Deaths)) +
  geom_point()+geom_line(aes(y=.fitted), size = 1, col = "green")+
  labs(x = "Deaths and recovered", y= " Total Confirmed case", title = "Linear Regression Model")


library(plotly)
library(plot3D)
plot(M2)
plot(Model_conf_rec)



#build model TIme vs arest 

Sample1<- sample.split(Data1$ObservationDate, SplitRatio = 0.65)
TRN <- subset(Data1, Sample1 == T)
TST <- subset(Data1, Sample1 == F)

nrow(TRN)
nrow(TST)

#build model with confirm cases


Model1<- lm(ObservationDate~Confirmed, data = TRN)
Model1

summary(Model1)
A<- plot(Data1$ObservationDate, Data1$Confirmed, lwd=1,ylab= "Confirmed cases", xlab = "Dates",main ="COnfirmed cases")

B<-plot(Data1$ObservationDate, Data1$Deaths,lwd=1,ylab= "Death cases", xlab = "Dates",main ="Death cases")

c<- plot(Data1$ObservationDate, Data1$Recovered,lwd=1,ylab= "Recovered cases", xlab = "Dates",main ="Recovered cases")








