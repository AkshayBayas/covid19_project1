#----------------------------------------------------------------#
Data<- read.csv("COVID19_line_list_data.csv")
Data
#----------------------------------------------------------------#
Data ->Df
View(Df)
str(Df)

DF1 <- (!complete.cases(Df))
DF1

summary(Df)

Df <- Df[-1]
summary(Df)
Df$case_in_country
Df$X
#X does not hold any value hence we will drop X from Data set
#-------------------------------------------------------------------------------------------------------
backupdata <- Df
backupdata
#--------------------------------------------------------------------------------------------------------

Df<- Df[-3]
View(Df)

Df<- Df[-20:-25]

#remove link  colounm , it is not needed for analysis. 

Df <- Df[-19]

View(Df)

str(Df)
#----------------------------------------------------------------------#
# replace "NA" values in case_in_country to "0" 

Df[is.na(Df$case_in_country),"case_in_country"] <- 0


#----------------------------------------------------------------------#
 #Replacing NA in dates with previous date in reported date. 

summary(Df$reporting.date)

summary(Df)

which(is.na(Df$reporting.date))
Df$reporting.date[261]
Df$reporting.date[263]
Df$reporting.date[262] = Df$reporting.date[261]

Df$reporting.date[262]
#----------------------------------------------------------------------#
#Replacing missing dates in SYmptom O[set with 15 days ahead of reported date.

as.Date("2002-01-01")+ 45

as.POSIXct(Df$reporting.date, format = "%m/%d/%Y")-> Df$reporting.date

aa
View(aa)
length(aa)
length(Df$reporting.date)
#-------------------------------------------------
#add 15 days to reporting date 
as.Date(Df$reporting.date)+ 15 ->pp
pp


#--------------------------------------------------
#Vlaue <- Df[is.na(Df$reporting.date),] & Df$symptom_onset

which(is.na(Df$symptom_onset),)

Df[is.na(Df$symptom_onset),"symptom_onset"] <- pp
View(Df)
#--------------------------------------------------------------------------
#Bakup2 <- Df
Bakup2
#--------------------------------------------------------------------------

#reset date format for all date columns
Df$symptom_onset<-  as.POSIXct(Df$symptom_onset, format = "%m/%d/%Y")
Df$hosp_visit_date<-as.POSIXct(Df$hosp_visit_dat, format = "%m/%d/%Y")
Df$exposure_start <-as.POSIXct(Df$exposure_start, format = "%m/%d/%Y")
Df$exposure_end <-  as.POSIXct(Df$exposure_end, format= "%m/%d/%Y")
Df$death<- as.POSIXct(Df$death, format = "%m/%d/%Y")
Df$recovered <- as.POSIXct(Df$recovered, format = "%m/%d/%Y")

#--------------------------------------------------------------------------

#in Symptom column, many blank values are found, we will replace the Blank cell with "fever" coz it is most common sympton

Bakup2$symptom -> Df$symptom

View(Df$symptom)
which(is.na(Df$symptom))
which(is.null(Df$symptom))
typeof(Df$symptom)

Df$symptom <- as.data.frame(Df$symptom)
typeof(Df$symptom)
summary(Df$symptom)

Df$symptom <- ifelse(nchar(Df$symptom)== 0, "Fever", Df$symptom)
View(Df)

which(Df$symptom == "") 

#------------------------------------------
Df$symptom
#------------------------------------------
gsub(" ", "", Df$symptom)
str(Df)
typeof(Df$symptom)
Df$symptom<- as.factor(Df$symptom)
Df$symptom
View(Df$symptom)

#Remove whte spaces from whold Data set
library(stringr)
library(dplyr)

Df <- Df%>% mutate_if(is.character, str_trim)
Df

view
View(Df)
#removing summary tab as no needed for analysis

Df<- Df[-3]

#Backup3 <- Df

#Re replace blank cell with "Fever" in Symptome

Df$symptom[1]
summary(Df$symptom)

summary(as.list(Df$symptom))

View (as.list(Df$symptom))
View(Df)

summary(Df)

#------------------------------------------------------------------------------#
#Remove whte spaces from whole Data set
library(stringr)
library(dplyr)

Df <- Df%>% mutate_if(is.character, str_trim)
Df
Data<- as.data.frame(Df)

# Replace Missing Genders to Female 
'> table((Df$gender))

female   male 
   382    520 
> table(Data$gender)'

Data[(is.na(Df$gender)),"gender"] <- "female"

table(Data$gender)
View(Data)
#-------------------------------------------------------------------------------#
Data$source<- backupdata$source
#-------------------------------------------------------------------------------#
# Replace garbeg characters in source with "The Local"

Data$source <- gsub("\\äººæ°'æ-¥æS¥å®~æ-¹å¾®ås", "The Local", Data$source)
Data$source <- gsub("\\äººæ°'æ-¥æS¥", "The Local", Data$source)
Data$source <- gsub("\\å¤®è§???æ-°é-»", "The Local", Data$source)
Data$source<- gsub("\\äººæ°'æ-¥æS¥", "The Local", Data$source)
Data$source<- gsub("\\å¤®è§???æ-°é-»", "The Local", Data$source)
Data$source<- gsub("\\äººæ°'æ-¥æS¥å®~æ-¹å¾®å\u008ds", "The Local", Data$source)
Data$source <- gsub("\\æ-°æµª", "The Local", Data$source)
Data$source <- gsub("\\äººæ°'æ-¥æS¥", "The Local", Data$source)
Data$source[145]
Data$source
#-------------------------------------------------------------------------------#
#check if any Missing record in {Visiting Wuhan and From Wuhan}
summary(Data$visiting.Wuhan)
is.na(Data$visiting.Wuhan)
is.na(Data$from.Wuhan)
#There is no 

#backup excel file 
View(Data)
write.csv(Data, "CovData.csv")
Data<- read.csv("CovData.csv")
Data
#-----------------------------------------------------------------------------#
#exposer start date NA values set to Dec 1 2019:

Data[is.na(Data$exposure_start),"exposure_start"] <- Data$exposure_start[16]
View(Data)

#put Missing Symptom on set date to  reporting date

Data[-1]

Data[is.na(Data$symptom_onset),"symptom_onset" ] <- Data[is.na(Data$symptom_onset), "reporting.date" ]

View(Data)

#Take Missing Age as median of age 
Data$age <- backupdata$age

Data[is.na(Data$age),"age"] <- median(Data$age,na.rm = TRUE) 

View(Data)
summary(Data$age)
which(is.na(Data$age))

summary(Data)

#replace hospital visit date with reported date.

Data[is.na(Data$hosp_visit_date),"hosp_visit_date" ] <- Data[is.na(Data$hosp_visit_date),"reporting.date"]

#=========================================================================================================#

sum(Data$visiting.Wuhan)

sum(Data$from.Wuhan, na.rm = TRUE)

#=======================================================================#










