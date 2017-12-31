#Initial 
install.packages("lubridate")
install.packages("randomForest")
install.packages("rminer")
install.packages("araastat")

#Read Dataset
df = read.csv("D:/RPI/Spring-16/Data Analytics/Big Assignment/Proposoal/New_York_State_Forest_Ranger_Wildland_Fire_Reporting_Database__Beginning_2008.csv")
df_forest= df
#Read Dataset End

install.packages('stringr')
library(stringr)

df2 = str_split_fixed(df$NFFL.Fuel.Model, ' - ',2)
df2 =as.data.frame(df2)
colnames(df2)[1]="Fuel_Model"
colnames(df2)[2]="Fuel_Complex"
df3 = cbind.data.frame(df,df2)
df_exp = df2
df_exp$Fuel_Complex =as.numeric(df_exp$Fuel_Complex)
df3$Fuel_Complex =as.numeric(df3$Fuel_Complex)



#Informal
df_informal = df;
df_informal$Reporting.Ranger = as.factor(df_informal$Reporting.Ranger);
df_informal$Acreage = as.numeric(df_informal$Acreage)

plot(df_informal$Acreage, df_informal$Reporting.Ranger)

#Informal


#Delete Useless Columns START
df3$Railroad.Name =NULL;
df3$Fatalities =NULL;
df$Injuries = NULL;
df3$Homes.Lost = NULL;
df3$Homes.Threatened = NULL;
df3$Other.Structures.Lost = NULL;
df3$Other.Structures.Threatened = NULL;
df3$Injuries= NULL;
df3$NFFL.Fuel.Model =NULL;
df3$Reporting.Ranger=NULL;
df3$Fire.Report.Method = NULL;
#Delete Useless Columns END



#Dataset Assign to Experimental Dataframe START
exploratory_df = df3
#Dataset Assign to Experimental Dataframe END

#NULLIFY More Columns which are clearly irrelevant to forest fire prediction
exploratory_df$State.Land.Unit.Name =NULL
exploratory_df$Location.1 =NULL
exploratory_df$Fire.Number =NULL
exploratory_df$Incident.Name =NULL
#NULLIFY END



#Get Season,Month,Year of Fire (For accurate predictive modelling)
report = (exploratory_df$Initial.Report.Date)
library(zoo)
library(lubridate)
report=as.Date(as.character(exploratory_df$Fire.Start.Date), format="%m/%d/%Y")

Month_of_fire = month(as.POSIXlt(report,  format="%m/%d/%Y"))
Month_of_fire = as.data.frame(Month_of_fire)
Year_of_fire = year(as.POSIXlt(report,  format="%m/%d/%Y"))
Year_of_fire = as.data.frame(Year_of_fire)
exploratory_df$Month = Month_of_fire;
exploratory_df$Year = Year_of_fire


report = as.yearqtr(as.yearmon(report,"%m/%d/%Y") +1/12)
exploratory_df$season = factor(format(report,"%q"), levels =1:4,
                labels = c("Winter","Spring","Summer","Fall"))
rm(df_forest,df_exp,df_exp1,df2,report)
rm(Month_of_fire,Year_of_fire)
#Get Season of Fire End


#Get No. of Days of Fire Sart
days_of_fire <- as.data.frame(as.Date(as.character(exploratory_df$Fire.Start.Date), format="%m/%d/%Y"))
days_of_fire_end <- as.data.frame(as.Date(as.character(exploratory_df$Fire.Out.Date), format="%m/%d/%Y"))
total_days = as.data.frame((days_of_fire_end - days_of_fire) +1  )
colnames(total_days) = c("Total.Days")
exploratory_df = cbind(exploratory_df,total_days)
exploratory_df$Total.Days = as.numeric(exploratory_df$Total.Days)

rm(days_of_fire_end,days_of_fire_end,total_days,days_of_fire)
#Get No. of Days of Fire End

#Convert Area Ownership & Fire Cause to Numeric to faciliate Prediction Model
exploratory_df$Ownership = as.numeric(exploratory_df$Ownership)
exploratory_df$Cause = as.numeric(exploratory_df$Cause)
#Convert END


hist(prescribe_dataset$Cause, main="Fire Frequency by Cause")


#Exploratory Data Analysis START
plot(exploratory_df$Region,exploratory_df$Acreage,xlab = 'Region by ID', ylab= 'Acreage Lost',main="Acreage Lost by Region")

cor(exploratory_df$Fire.Start.Date)
plot(exploratory_df$Total.Days, exploratory_df$Acreage, xlim = c(1,50), ylim = c(1,100));


#Check Relation of County with Fire Acreage
county_num = as.numeric(exploratory_df$County);
acreage_lost = as.numeric(exploratory_df$Acreage);
zone_factor = as.factor(exploratory_df$Zone);
region_factor = as.factor(exploratory_df$Region)
municipality_factor = as.numeric(exploratory_df$Municipality)
plot(county_num,acreage_lost)
plot(zone_factor,acreage_lost , main='Acreage Frequency by Zone')
plot(region_factor,acreage_lost, main='Acreage Frequency by Region')
plot(municipality_factor,acreage_lost);

#Based on Distribution of fire, it seems clear that except for some big fires favoured in particular 
#counties, the magnitude of fire does not favour any specific county. when seen on a zone basis, higher
#magnitude fire reduce to one zone, and further reducing to region basis. It seems clear that big fires
#favour region 3, and then region 5

#Municipality and County Need to Go
exploratory_df$Municipality = NULL;
exploratory_df$County = NULL;
rm(county_num,acreage_lost,zone_factor,region_factor,municipality_factor)


#Now Lets Speak about Dates, do we need to keep them for Creating a Model
#Report Date is out: no use for it
exploratory_df$Initial.Report.Date = NULL
#Retreived Month & Date of fire and seperated it to form 2 columns
#Date Columns are done for now
exploratory_df$Fire.Start.Date=NULL
exploratory_df$Fire.Out.Date = NULL
#Season can also go; Until something better comes up
exploratory_df$season =NULL


#Lets think about more rows:
#Total: Non personal expenses for extinguishing fires...Hmmm...Lets relate it to Ownership
plot(exploratory_df$Ownership,exploratory_df$Total)
plot(exploratory_df$Fuel_Complex,exploratory_df$Total)

#Some amount of data for state forest, but mostly 0 figure data, 
#coorelate to 12 fuel Complex but too little data, this column has to go
exploratory_df$Total=NULL


#Lets correlate acreage with latitude and longitude
plot(exploratory_df$Acreage,exploratory_df$Latitude)
#Nothing special with Latitude, except big fires between lat 41-42 (Latitudes can be categorized)
#Lets plot against longitude now
plot(exploratory_df$Acreage,exploratory_df$Longitude)
#Promising but got an outlier at 40 Degree Longitude, need to get rid of that (its longitude should be 73.93)
#First lets convert longitude to positive
exploratory_df$Longitude = -(exploratory_df$Longitude)
#Get that value Fixed
exploratory_df[65,7] = 73.93
#Great..Now lets find that longitude range again
plot(exploratory_df$Acreage,exploratory_df$Longitude)
#Big Fire for Longitude 74, Also new York is between 72 to 80 Longitude Lines.
#I think it should make sense to keep latitude and longitude till 1 decimal place
exploratory_df$Latitude= format(round(exploratory_df$Latitude, 1), nsmall = 1)
exploratory_df$Longitude= format(round(exploratory_df$Longitude, 1), nsmall = 1)
#Plots still looks the same but I suppose SVM could work better now
exploratory_df$Zone = as.factor(exploratory_df$Zone)

#Complex Type seems vague, lets find its property
plot(exploratory_df$Complex.Type, exploratory_df$Ownership)
plot(exploratory_df$Complex.Type,exploratory_df$Fuel_Model)
plot(exploratory_df$Complex.Type,exploratory_df$Total.Days)
plot(exploratory_df$Complex.Type,exploratory_df$Acreage)
#Seems totally independent from others, I dont know why I am keeping it, but I am keeping it.

#Seems Like Done, Now last thing is to divide fire by severity, but first fork into SVM Dataset
exploratory_df$Acreage = as.numeric(exploratory_df$Acreage)
hist(exploratory_df$Acreage)
boxplot(exploratory_df$Acreage)
fire_svm_dataset = exploratory_df;

#Exploratory Data Analysis END


######Prescribed Fire
prescribe_dataset = fire_svm_dataset_ref
prescribe_dataset$Acreage = exploratory_df$Acreage
prescribe_dataset$Zone = as.numeric(prescribe_dataset$Zone)

cause = as.data.frame(prescribe_dataset$Cause)
cause = within(cause, {
  prescribed = ifelse(cause == 11, 1,
                        ifelse(cause != 11 , 0,NA))
})
#Things which could change confusion matrix
prescribe_dataset$Homes_Lost = NULL #as.numeric(df$Homes.Lost)
prescribe_dataset$Homes_Threatened = NULL #as.numeric(df$Homes.Threatened)
prescribe_dataset$Fatality = NULL #as.numeric(df$Fatalities);
prescribe_dataset$Structures = NULL #as.numeric(df$Other.Structures.Lost);
prescribe_dataset$Longitude = NULL #fire_svm_dataset_ref$Longitude Keep this
prescribe_dataset$Reporting = NULL #as.numeric(df$Fire.Report.Method)

#Do this
prescribe_dataset$Fuel_Model = as.numeric(prescribe_dataset$Fuel_Model)
prescribe_dataset$Total.Days = NULL;
prescribe_dataset$Acreage = NULL;
prescribe_dataset$Year = fire_svm_dataset_ref$Year #NULL
#
prescribe_dataset$Ranger = as.numeric(df$Reporting.Ranger)
prescribe_dataset$Ranger = NULL

prescribe_dataset$Prescribed = cause$prescribed

rm(COMPARISON_TBL,prescribed,cause)

cor(prescribe_dataset$Ranger, prescribe_dataset$Prescribed)

plot(as.numeric(exploratory_df$Cause),as.numeric(exploratory_df$Acreage))

prescribe_dataset$Prescribed = NULL
prescribe_dataset$Cause =NULL

prescribe_dataset$Prescribed = as.numeric(unlist(prescribe_dataset$Prescribed))

prescribe_train_Set = subset(prescribe_dataset, prescribe_dataset$Year <2016)
prescribe_test_Set = subset(prescribe_dataset, prescribe_dataset$Year == 2016)

svm_Model_prescribe = svm(prescribe_train_Set$Prescribed~., data = prescribe_train_Set,type ='C-classification')
predict_set = predict(svm_Model_prescribe, prescribe_test_Set)
confusionMatrix(predict_set, prescribe_test_Set$Prescribed)

#Check Fits
lm_pres = lm(prescribe_dataset$Prescribed~., data = prescribe_dataset)
summary(lm_pres)
#Check Fits

library(randomForest)
rf_Model_prescribe = randomForest(as.factor(prescribe_train_Set$Prescribed)~., data = prescribe_train_Set)
plot(rf_Model_prescribe)
varImpPlot(rf_Model_prescribe)
predict_set = predict(rf_Model_prescribe, prescribe_test_Set)
confusionMatrix(predict_set, prescribe_test_Set$Prescribed)

tuneRF(prescribe_train_Set[,-8], prescribe_train_Set[,8],
       stepFactor = 0.05,
       ntreeTry = 400,
       improve = 0.05)

reprtree:::plot.getTree(rf_Model_prescribe)

######Prescribe

#Naive Baiyes
library(rminer);

naive_baiyes <- naiveBayes(as.factor(prescribe_train_Set$Prescribed)~., data = prescribe_train_Set )
print(naive_baiyes)

predict_NB <- predict(naive_baiyes, prescribe_test_Set)

confusionMatrix(predict_NB, prescribe_test_Set$Prescribed)

###Prescribed Fire End

attach(fire_svm_dataset)

#Lets Try SVM Model

#Import Library
library(e1071)

#Fire Rating
Acreage_Rating = as.data.frame(exploratory_df$Acreage)
Acreage_Rating = within(Acreage_Rating, {
  Acreage_Rating = ifelse(Acreage_Rating >= 100, 5,
                          ifelse(Acreage_Rating >= 75 & Acreage_Rating <100, 4,
                                 ifelse(Acreage_Rating >= 50 & Acreage_Rating <75, 3 ,
                                        ifelse(Acreage_Rating >= 25 & Acreage_Rating <50, 2,
                                               ifelse(Acreage_Rating >=0 & Acreage_Rating < 25, 1,NA)))))
})

Acreage_Rating = as.data.frame(exploratory_df$Acreage)
Acreage_Rating = within(Acreage_Rating, {
  Acreage_Rating = ifelse(Acreage_Rating >= 100, 3,
                          ifelse(Acreage_Rating >= 10 & Acreage_Rating <100, 2,
                                               ifelse(Acreage_Rating  < 10, 1,NA)))
})
Acreage_Rating$`exploratory_df$Acreage`=NULL
fire_svm_dataset$Acreage_Rating=NULL
fire_svm_dataset$Acreage_Rating = Acreage_Rating$Acreage_Rating

#No
fire_svm_dataset$`exploratory_df$Acreage`=NULL
fire_svm_dataset$Acreage_Rating=NULL
fire_svm_dataset$Acreage=NULL
#I am removing the complex type as I am having 13 columns which I think is unlucky
fire_svm_dataset$Complex.Type =NULL
rm(fire,Acreage_Rating)

fire_svm_dataset[801,11]=2013

#Bit of a fiddling around cause SVM tanked
#fire_svm_dataset_ref = fire_svm_dataset
fire_svm_dataset = fire_svm_dataset_ref
#Bit of a fiddling around cause SVM tanked


fire_svm_dataset$Acreage_Rating = as.numeric(fire_svm_dataset$Acreage_Rating)
fire_svm_dataset$Latitude = as.numeric(fire_svm_dataset$Latitude)
fire_svm_dataset$Longitude = as.numeric(fire_svm_dataset$Longitude)

fire_svm_dataset$Zone = as.numeric(fire_svm_dataset$Zone)
fire_svm_dataset$Fuel_Model = as.numeric(fire_svm_dataset$Fuel_Model)
fire_svm_dataset$Fuel_Complex = as.numeric(fire_svm_dataset$Fuel_Complex)

fire_svm_dataset$Month = as.numeric(unlist(fire_svm_dataset$Month))
fire_svm_dataset$Year  = as.numeric(unlist(fire_svm_dataset$Year))

#Bit of a fiddling around cause SVM tanked
fire_svm_dataset$Year =NULL
fire_svm_dataset$Longitude =NULL
fire_svm_dataset$Latitude =NULL

fire_svm_dataset$Zone = as.numeric(fire_svm_dataset$Zone)
#Bit of a fiddling around cause SVM tanked
#Lets get rid of fuel complex & Fuel Model
fire_svm_dataset$Fuel_Complex=NULL
fire_svm_dataset$Fuel_Model = NULL

lm_acreage = lm(fire_svm_dataset$Acreage_Rating~., fire_svm_dataset)
summary(lm_acreage)


#2008-2014 Train Set / Also 2008-2015 Train Set
#fire_train_Set = fire_svm_dataset[sample(1, 1048, replace=FALSE),]
fire_train_Set = subset(fire_svm_dataset, fire_svm_dataset$Year <2016)
#fire_train_Set = subset(fire_svm_dataset, fire_svm_dataset$Year <2016)

#2015,2016 Test Set
fire_test_Set <- subset(fire_svm_dataset, fire_svm_dataset$Year ==2016)
#fire_test_Set <- subset(fire_svm_dataset, fire_svm_dataset$Year = 2015)

#Bit of a fiddling around cause SVM tanked
fire_train_Set$Year=NULL
fire_test_Set$Year=NULL
#Bit of a fiddling around cause SVM tanked
#Worked with 66% Accuracy using SVM, but single value prediction only

#SVM Training
svm_Model_Fire = svm(fire_train_Set$Acreage_Rating~., data = fire_train_Set, type ='C-classification')

predict_set = predict(svm_Model_Fire, fire_test_Set)


COMPARISON_TBL = data.frame(PREDICTED = round(as.numeric(predict_set)), 
                            Original = as.numeric(fire_test_Set$Acreage_Rating))

View(COMPARISON_TBL)

confusionMatrix(predict_set, fire_test_Set$Acreage_Rating)
plot(svm_Model_Fire,fire_test_Set)

xmean(COMPARISON_TBL$PREDICTED == COMPARISON_TBL$Original)
#SVM Training

#Random FOrest
library(randomForest)
fire_train_Set$Acreage_Rating = as.factor(fire_train_Set$Acreage_Rating)
fire_test_Set$Acreage_Rating = as.factor(fire_test_Set$Acreage_Rating)
rf_Model_Fire = randomForest(as.factor(fire_train_Set$Acreage_Rating)~., data = fire_train_Set)
#rf_Model_Fire = randomForest(as.factor(fire_train_Set$Acreage_Rating)~., data = fire_train_Set, mtry=3)
plot(rf_Model_Fire)

reprtree:::plot.getTree(rf_Model_Fire)

varImpPlot(rf_Model_Fire)



predict_set = predict(rf_Model_Fire, fire_test_Set)

COMPARISON_TBL = data.frame(PREDICTED = as.numeric(predict_set), 
                            Original = fire_test_Set$Acreage_Rating)

confusionMatrix(predict_set, fire_test_Set$Acreage_Rating)

tuneRF(fire_train_Set[,-10], fire_train_Set[,10],
       stepFactor = 0.05,
       ntreeTry = 400,
       improve = 0.05)

#Naive Baiyes
library(rminer);

naive_baiyes <- naiveBayes((as.factor(fire_train_Set$Acreage_Rating))~., data = fire_train_Set )
#print(NB)
print(naive_baiyes)
predict_NB <- predict(naive_baiyes, fire_test_Set)

confusionMatrix(predict_NB, fire_test_Set$Acreage_Rating)


library(caret)
predicted = c(predict_set)
reference = c(COMPARISON_TBL$Original)
u = union(predicted, reference)
t = table(factor(predicted, u), factor(reference, u))
confusionMatrix(t)

#Tree from Random Forest

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
library(reprtree)
