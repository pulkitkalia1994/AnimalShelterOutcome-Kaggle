setwd("C:\\R-Programming\\animal shelter outcomes")
library(caret)
library(dplyr)
train<-read.csv("train.csv",na.strings = "")

train$Sex<-ifelse(grepl("Male",train$SexuponOutcome)==TRUE,"Male","Female")
train$Sex<-as.factor(train$Sex)


train$isIntact<-ifelse(grepl("Intact",train$SexuponOutcome)==TRUE,1,0)
train$isIntact<-as.factor(train$isIntact)

train$isNeutered<-ifelse(grepl("Neutered",train$SexuponOutcome)==TRUE,1,0)
train$isNeutered<-as.factor(train$isNeutered)

train$isSpayed<-ifelse(grepl("Spayed",train$SexuponOutcome)==TRUE,1,0)
train$isSpayed<-as.factor(train$isSpayed)

train$AgeuponOutcome<-as.character(train$AgeuponOutcome)
noOfDays<-c()
for(i in train$AgeuponOutcome){
  if(!is.na(i)){
    strings<-strsplit(i," ")
    if(strings[[1]][2]=="day" || strings[[1]][2]=="days"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1]))
    }
    else if(strings[[1]][2]=="week" || strings[[1]][2]=="weeks"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1])*7)
    }
    else if(strings[[1]][2]=="months" || strings[[1]][2]=="month"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1])*30)
    }
    else if(strings[[1]][2]=="years" || strings[[1]][2]=="year"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1])*365)
    }
  }
  else{noOfDays<-c(noOfDays,i)}
}

noOfDays<-as.numeric(noOfDays)

plot(density(noOfDays,na.rm = TRUE))

#daysSpend<-cut(noOfDays,breaks = c(0,100,250,500,1000,2000,4000,10000),include.lowest = TRUE)
daysSpend<-cut(noOfDays,breaks = c(0,50,250,350,800,1100),include.lowest = TRUE)
daysSpend<-ifelse(is.na(daysSpend),"Missing",daysSpend)

train$noOfDays<-noOfDays
train$daysSpend<-as.factor(daysSpend)
noOfDays<-NULL
daysSpend<-NULL


train$SexuponOutcome<-as.character(train$SexuponOutcome)
train$SexuponOutcome[is.na(train$SexuponOutcome)]<-"Missing"
train$SexuponOutcome<-as.factor(train$SexuponOutcome)


train$isMix<-ifelse(grepl("Mix",as.character(train$Breed),ignore.case = TRUE),1,0)
train$isMix<-as.factor(train$isMix)

train$Breed<-as.character(train$Breed)
train$Breed<-gsub("Mix", "",train$Breed)
train$Breed<-gsub(" $", "",train$Breed)


breed1<-c()
breed2<-c()
for(j in train$Breed){
  strings<-strsplit(j,"/")
  if(is.na(strings[[1]][2])){
    breed1<-c(breed1,strings[[1]][1])
    breed2<-c(breed2,"none")
    
  }
  else{
    breed1<-c(breed1,strings[[1]][1])
    breed2<-c(breed2,strings[[1]][2])
  
  }
}

a<-as.data.frame(table(unlist(breed1)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
train$breed1<-ifelse(breed1 %in% a,breed1,"others")

a<-as.data.frame(table(unlist(breed2)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
train$breed2<-ifelse(breed2 %in% a,breed2,"others")

train$breed1<-as.factor(breed1)
train$breed2<-as.factor(breed2)



train$Color<-as.character(train$Color)
isMixColor<-c()
color1<-c()
color2<-c()
for(j in train$Color){
  strings<-strsplit(j,"/")
  if(is.na(strings[[1]][2])){
    color1<-c(color1,strings[[1]][1])
    color2<-c(color2,"none")
    isMixColor<-c(isMixColor,0)    
  }
  else{
    color1<-c(color1,strings[[1]][1])
    color2<-c(color2,strings[[1]][2])
    isMixColor<-c(isMixColor,1)  
  }
}

a<-as.data.frame(table(unlist(color1)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
train$color1<-ifelse(color1 %in% a,color1,"others")

a<-as.data.frame(table(unlist(color2)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
train$color2<-ifelse(color2 %in% a,color2,"others")

train$color1<-as.factor(color1)
train$color2<-as.factor(color2)
train$isMixColor<-as.factor(isMixColor)

train$isDomestic<-ifelse(grepl("Domestic",as.character(train$Breed),ignore.case = TRUE),1,0)
train$isDomestic<-as.factor(train$isDomestic)

train$AgeuponOutcome[is.na(train$AgeuponOutcome)]<-"Missing"
train$AgeuponOutcome<-as.factor(train$AgeuponOutcome)

train$Name<-as.character(train$Name)
train$isNameExist<-ifelse(is.na(train$Name),0,1)
train$isNameExist<-as.factor(train$isNameExist)
train$Name[is.na(train$Name)]<-"Missing"
train$Name<-as.factor(train$Name)

train$Name<-as.character(train$Name)
train$Name<-gsub(" ","",train$Name)
train$Name<-gsub("'","",train$Name)

train$Name<-as.factor(train$Name)

train$noOfDays[is.na(train$noOfDays)]<-median(train$noOfDays,na.rm = TRUE)


month<-c()
date<-c()
hours<-c()
train$DateTime<-as.character(train$DateTime)
for(i in train$DateTime){
  strings<-strsplit(i," ")
  dateString<-strsplit(strings[[1]][1],"-")
  month<-c(month,dateString[[1]][[2]])
  date<-c(date,dateString[[1]][[3]])
  timeString<-strsplit(strings[[1]][2],":")
  hours<-c(hours,timeString[[1]][1])
}

train$month<-as.factor(month)
train$date<-as.numeric(date)
train$hours<-as.numeric(hours)

train$categoryDate<-cut(train$date,breaks = c(0,10,20,31),include.lowest = TRUE)
levels(train$categoryDate)<-c("Starting","Mid","End")

train$hours<-cut(train$hours,breaks = c(0,5,11,16,19,24),include.lowest = TRUE)
levels(train$hours)<-c("Night","Morning","Afternoon","Evening","Night")

train$month<-as.numeric(month)
train$quater<-cut(train$month,breaks = c(2,4,9,12),include.lowest = TRUE)
train$quater[is.na(train$quater)]<-as.factor("(9,12]")
levels(train$quater)<-c("Starting","Mid","End")

train$month<-as.factor(month)

train$isPuppy<-ifelse(train$noOfDays<=366,1,0)
train$isPuppy<-as.factor(train$isPuppy)

library(lubridate)
dayOfWeek<-wday(train$DateTime)
train$dayOfWeek<-as.factor(dayOfWeek)
train$isWeekend<-ifelse(dayOfWeek %in% c(1,7),1,0)
train$isWeekend<-as.factor(train$isWeekend)


tr<-trainControl(method = "cv", number = 5)
cartGrid = expand.grid( .cp = seq(0.00002,0.001,0.00002))


train$isSimpleColor<-as.factor(ifelse(as.character(train$color2)=="none" & grepl(" ",as.character(train$color1))==FALSE,1,0))
train$isSimpleBreed<-as.factor(ifelse(as.character(train$breed2)=="none" & grepl(" ",as.character(train$breed1))==FALSE,1,0))


modelrpart<-train(OutcomeType~AnimalType+Sex+isIntact+isNeutered+isSpayed+daysSpend+isMix+isMixColor+isDomestic+isNameExist+hours+quater+isPuppy+dayOfWeek+isWeekend,data=train,method="rpart",trControl=tr,tuneGrid=cartGrid)
o1<-predict(modelrpart,train,type="prob")

library(nnet)
ideal <- class.ind(train$OutcomeType)
seedsANN = nnet(o1, ideal, size=10, softmax=TRUE,decay=1e-04)
o2<-predict(seedsANN, o1, type="raw")



library(class)

train<-cbind(train,o1)


library("xgboost")
library("Ckmeans.1d.dp")

train$Breed<-NULL
train$Color<-NULL
train$AnimalID<-NULL
train$DateTime<-NULL
train$Name<-NULL
train$OutcomeSubtype<-NULL


t<- train %>% mutate_if(is.factor,as.numeric)

t$OutcomeType<-t$OutcomeType-1
##try to normalise the data and then try with XGBoost and Knn algorithm!!!! (might perform better)
#t<-preprocess(t[,-1],c("center","scale"))

data_variables <- as.matrix(t[,-1])
data_label <- t[,"OutcomeType"]
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

numberOfClasses <- length(unique(train$OutcomeType))
xgb_params <- list("objective" = "multi:softprob",eta=0.05,gamma=0.9,max_depth=10,eval_metric="mlogloss","num_class" = numberOfClasses)
xgbcv <- xgb.cv( params = xgb_params, data = data_matrix, nrounds = 300, nfold = 10, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 10, maximize = F)

nround    <- xgbcv$best_iteration # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions

bst_model <- xgb.train(params = xgb_params,
                       data = data_matrix,
                       nrounds = nround)







#####Test data
test<-read.csv("test.csv",na.strings = "")

test$Sex<-ifelse(grepl("Male",test$SexuponOutcome)==TRUE,"Male","Female")
test$Sex<-as.factor(test$Sex)


test$isIntact<-ifelse(grepl("Intact",test$SexuponOutcome)==TRUE,1,0)
test$isIntact<-as.factor(test$isIntact)

test$isNeutered<-ifelse(grepl("Neutered",test$SexuponOutcome)==TRUE,1,0)
test$isNeutered<-as.factor(test$isNeutered)

test$isSpayed<-ifelse(grepl("Spayed",test$SexuponOutcome)==TRUE,1,0)
test$isSpayed<-as.factor(test$isSpayed)

test$AgeuponOutcome<-as.character(test$AgeuponOutcome)
noOfDays<-c()
for(i in test$AgeuponOutcome){
  if(!is.na(i)){
    strings<-strsplit(i," ")
    if(strings[[1]][2]=="day" || strings[[1]][2]=="days"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1]))
    }
    else if(strings[[1]][2]=="week" || strings[[1]][2]=="weeks"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1])*7)
    }
    else if(strings[[1]][2]=="months" || strings[[1]][2]=="month"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1])*30)
    }
    else if(strings[[1]][2]=="years" || strings[[1]][2]=="year"){
      noOfDays<-c(noOfDays,as.numeric(strings[[1]][1])*365)
    }
  }
  else{noOfDays<-c(noOfDays,i)}
}

noOfDays<-as.numeric(noOfDays)

#plot(density(noOfDays,na.rm = TRUE))

#daysSpend<-cut(noOfDays,breaks = c(0,100,250,500,1000,2000,4000,10000),include.lowest = TRUE)
daysSpend<-cut(noOfDays,breaks = c(0,50,250,350,800,1100),include.lowest = TRUE)
daysSpend<-ifelse(is.na(daysSpend),"Missing",daysSpend)

test$noOfDays<-noOfDays
test$daysSpend<-as.factor(daysSpend)
noOfDays<-NULL
daysSpend<-NULL


test$SexuponOutcome<-as.character(test$SexuponOutcome)
test$SexuponOutcome[is.na(test$SexuponOutcome)]<-"Missing"
test$SexuponOutcome<-as.factor(test$SexuponOutcome)


test$isMix<-ifelse(grepl("Mix",as.character(test$Breed),ignore.case = TRUE),1,0)
test$isMix<-as.factor(test$isMix)

test$Breed<-as.character(test$Breed)
test$Breed<-gsub("Mix", "",test$Breed)
test$Breed<-gsub(" $", "",test$Breed)


breed1<-c()
breed2<-c()
for(j in test$Breed){
  strings<-strsplit(j,"/")
  if(is.na(strings[[1]][2])){
    breed1<-c(breed1,strings[[1]][1])
    breed2<-c(breed2,"none")
    
  }
  else{
    breed1<-c(breed1,strings[[1]][1])
    breed2<-c(breed2,strings[[1]][2])
    
  }
}

a<-as.data.frame(table(unlist(breed1)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
test$breed1<-ifelse(breed1 %in% a,breed1,"others")

a<-as.data.frame(table(unlist(breed2)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
test$breed2<-ifelse(breed2 %in% a,breed2,"others")

test$breed1<-as.factor(breed1)
test$breed2<-as.factor(breed2)





test$Color<-as.character(test$Color)
isMixColor<-c()
color1<-c()
color2<-c()
for(j in test$Color){
  strings<-strsplit(j,"/")
  if(is.na(strings[[1]][2])){
    color1<-c(color1,strings[[1]][1])
    color2<-c(color2,"none")
    isMixColor<-c(isMixColor,0)    
  }
  else{
    color1<-c(color1,strings[[1]][1])
    color2<-c(color2,strings[[1]][2])
    isMixColor<-c(isMixColor,1)  
  }
}

a<-as.data.frame(table(unlist(color1)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
test$color1<-ifelse(color1 %in% a,color1,"others")

a<-as.data.frame(table(unlist(color2)))
a<-a[order(-a$Freq),]
a<-a[1:40,]$Var1
test$color2<-ifelse(color2 %in% a,color2,"others")

test$color1<-as.factor(color1)
test$color2<-as.factor(color2)
test$isMixColor<-as.factor(isMixColor)

test$isDomestic<-ifelse(grepl("Domestic",as.character(test$Breed),ignore.case = TRUE),1,0)
test$isDomestic<-as.factor(test$isDomestic)

test$AgeuponOutcome[is.na(test$AgeuponOutcome)]<-"Missing"
test$AgeuponOutcome<-as.factor(test$AgeuponOutcome)

test$Name<-as.character(test$Name)
test$isNameExist<-ifelse(is.na(test$Name),0,1)
test$isNameExist<-as.factor(test$isNameExist)
test$Name[is.na(test$Name)]<-"Missing"
test$Name<-as.factor(test$Name)


test$Name<-as.character(test$Name)
test$Name<-gsub(" ","",test$Name)
test$Name<-gsub("'","",test$Name)

test$Name<-as.factor(test$Name)


test$noOfDays[is.na(test$noOfDays)]<-median(test$noOfDays,na.rm = TRUE)

month<-c()
date<-c()
hours<-c()
test$DateTime<-as.character(test$DateTime)
for(i in test$DateTime){
  strings<-strsplit(i," ")
  dateString<-strsplit(strings[[1]][1],"-")
  month<-c(month,dateString[[1]][[2]])
  date<-c(date,dateString[[1]][[3]])
  timeString<-strsplit(strings[[1]][2],":")
  hours<-c(hours,timeString[[1]][1])
}

test$month<-as.factor(month)
test$date<-as.numeric(date)
test$hours<-as.numeric(hours)

test$categoryDate<-cut(test$date,breaks = c(0,10,20,31),include.lowest = TRUE)
levels(test$categoryDate)<-c("Starting","Mid","End")

test$hours<-cut(test$hours,breaks = c(0,5,11,16,19,24),include.lowest = TRUE)
levels(test$hours)<-c("Night","Morning","Afternoon","Evening","Night")

test$month<-as.numeric(month)
test$quater<-cut(test$month,breaks = c(2,4,9,12),include.lowest = TRUE)
test$quater[is.na(test$quater)]<-as.factor("(9,12]")
levels(test$quater)<-c("Starting","Mid","End")

test$month<-as.factor(month)

test$isPuppy<-ifelse(test$noOfDays<=366,1,0)
test$isPuppy<-as.factor(test$isPuppy)

library(lubridate)
dayOfWeek<-wday(test$DateTime)
test$dayOfWeek<-as.factor(dayOfWeek)
test$isWeekend<-ifelse(dayOfWeek %in% c(1,7),1,0)
test$isWeekend<-as.factor(test$isWeekend)

train$isSimpleColor<-as.factor(ifelse(as.character(train$color2)=="none" & grepl(" ",as.character(train$color1))==FALSE,1,0))
train$isSimpleBreed<-as.factor(ifelse(as.character(train$breed2)=="none" & grepl(" ",as.character(train$breed1))==FALSE,1,0))


#####Test data end




predictions<-predict(modelrpart,test,type="prob")
test<-cbind(test,predictions)
#predictionsXG<-predict(bst_model,newdata=test_matrix)


test$Breed<-NULL
test$Color<-NULL
test$AnimalID<-NULL
ID<-test$ID
test$ID<-NULL
test$DateTime<-NULL
test$Name<-NULL
test$OutcomeSubtype<-NULL


t<- test %>% mutate_if(is.factor,as.numeric)

test_matrix <- xgb.DMatrix(data = as.matrix(t))

predictionsXG<-predict(bst_model,newdata=test_matrix)

j<-1
Adoption<-c()
Died<-c()
Euthanasia<-c()
Return_to_owner<-c()
Transfer<-c()
for(i in seq(1,57280,5)){
  j<-i
  Adoption<-c(Adoption,predictionsXG[j])
  Died<-c(Died,predictionsXG[j+1])
  Euthanasia<-c(Euthanasia,predictionsXG[j+2])
  Return_to_owner<-c(Return_to_owner,predictionsXG[j+3])
  Transfer<-c(Transfer,predictionsXG[j+4])
  
}


#dummydf<-data.frame(ID=test$ID,predictions)
dummydfXG<-data.frame(ID=ID,Adoption=Adoption,Died=Died,Euthanasia=Euthanasia,Return_to_owner=Return_to_owner,Transfer=Transfer)


#write.csv(dummydf,"output.csv",row.names=FALSE)
write.csv(dummydfXG,"output.csv",row.names=FALSE)


## Exploratory Analysis
library(dplyr)

#plot(train$OutcomeType,noOfDays)

#plot(as.factor(train$OutcomeType))

library(ggplot2)
g<-ggplot(train,aes(OutcomeType,fill=OutcomeSubtype))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=AnimalType))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=Sex))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isIntact))+geom_histogram(stat = "count")
print(g)


g<-ggplot(train,aes(OutcomeType,fill=isNeutered))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isSpayed))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=SexuponOutcome))+geom_histogram(stat = "count")
print(g)


g<-ggplot(train,aes(OutcomeType,fill=daysSpend))+geom_histogram(stat = "count")
print(g)


g<-ggplot(train,aes(OutcomeType,log(noOfDays)))+geom_point(aes(col=OutcomeType))
print(g)

g<-ggplot(train,aes(OutcomeType,fill=hours))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=categoryDate))+geom_histogram(stat = "count")
print(g)


g<-ggplot(train,aes(OutcomeType,fill=month))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=quater))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isNameExist))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isPuppy))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isWeekend))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=dayOfWeek))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=alphabet))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isSimpleColor))+geom_histogram(stat = "count")
print(g)

g<-ggplot(train,aes(OutcomeType,fill=isSimpleBreed))+geom_histogram(stat = "count")
print(g)
