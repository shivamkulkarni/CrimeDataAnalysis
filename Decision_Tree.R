library(SparkR)
library(cluster)
library(factoextra)
library(dplyr)
library(datasets)
library(psych)
library(rpart)
library(rpart.plot)
library(DMwR)

set.seed(20)

#Reads the dataset into a dataframe
chicago.df <- read.csv("clean_data.csv", header = T, stringsAsFactors = F)

#Splits the dataset into train and test datasets in 80-20 proportion 
indx <- sample(1:nrow(chicago.df), 0.8*nrow(chicago.df))
train.sample.chicago.df <- chicago.df[indx, ]
test.sample.chicago.df <- chicago.df[-indx, ]

#Makes sure the predictors and target variable are factors 
train.new.primary.type <- as.factor(train.sample.chicago.df$Primary.Type)
train.new.location.description <- as.factor(train.sample.chicago.df$Location.Description)
train.new.district <- as.factor(train.sample.chicago.df$District)

#Splits the the dates as date and time 
time1 <- strsplit(train.sample.chicago.df$Date, " ")
times <- c()
for(n in 1:nrow(train.sample.chicago.df))
{
  times <- c(times, as.character(time1[[n]][2]))
}

#Splits the time as HH MM 
split <- strsplit(times, ":")
hour <- c()
for(n in 1:nrow(train.sample.chicago.df))
{
  hour <- c(hour,as.numeric(split[[n]][1]))
}

#Extracts the hour
time2 <- c()
for(n in 1:nrow(train.sample.chicago.df))
{
  time2 <- c(time2, as.character(time1[[n]][3]))
}

time.of.the.day <- c(rep("Night", nrow(train.sample.chicago.df)))
time.df <- data.frame(hour,time2,time.of.the.day,stringsAsFactors = F)

#Adds 12 hours if is PM i.e. past noon
condition <- time.df$time2 == "PM"
time.df$hour[condition] <- time.df$hour[condition] + 12

#Categories the hours in 5 times of the day 
for(n in 1:nrow(train.sample.chicago.df))
{
  if(time.df[n,1] >= 1 && time.df[n,1] < 4)
  {
    time.df[n,3] = "Early Morning"
  }
  else if (time.df[n,1] >= 4 && time.df[n,1] < 6)
  {
    time.df[n,3] = "Dawn"
  }
  else if (time.df[n,1] >= 6 && time.df[n,1] < 12)
  {
    time.df[n,3] = "Morning"
  }
  else if (time.df[n,1] >= 12 && time.df[n,1] < 16)
  {
    time.df[n,3] = "Afternoon"
  }
  else if(time.df[n,1] >= 16 && time.df[n,1] < 21)
  {
    time.df[n,3] = "Evening"
  }
}

TOTD <- as.factor(time.df$time.of.the.day)


new.for.corelation <- data.frame(train.new.primary.type, train.new.location.description, train.new.district, TOTD)
new.df <- filter(new.for.corelation, train.new.location.description == "STREET" | train.new.location.description == "RESIDENCE" | train.new.location.description == "APARTMENT" | train.new.location.description == "SIDEWALK" | train.new.location.description == "OTHER" | train.new.location.description == "CTA TRAIN" | train.new.location.description == "BAR OR TAVERN" | train.new.location.description == "ALLEY" | train.new.location.description == "DEPARTMENT STORE" | train.new.location.description == "RESTRAURANT")
new.df <- filter(new.df, train.new.primary.type == "BATTERY" | train.new.primary.type == "NARCOTICS" |  train.new.primary.type == "THEFT")

new.df$train.new.primary.type <- factor(new.df$train.new.primary.type)
new.df$train.new.location.description <- factor(new.df$train.new.location.description)
new.df$train.new.district <- factor(new.df$train.new.district)

#Smote technique is used to handle class imbalance
new.df.smote <- SMOTE(train.new.primary.type ~ train.new.location.description + train.new.district + TOTD, new.df, perc.over = 100, perc.under = 400)

#Makes sure the predictors and target variable are factors
test.new.primary.type <- as.factor(test.sample.chicago.df$Primary.Type)
test.new.location.description <- as.factor(test.sample.chicago.df$Location.Description)
test.new.district <- as.factor(test.sample.chicago.df$District)

#Splits the the dates as date and time 
test.time1 <- strsplit(test.sample.chicago.df$Date, " ")
test.times <- c()
for(n in 1:nrow(test.sample.chicago.df))
{
  test.times <- c(test.times, as.character(test.time1[[n]][2]))
}
test.split <- strsplit(test.times, ":")
test.hour <- c()
for(n in 1:nrow(test.sample.chicago.df))
{
  test.hour <- c(test.hour,as.numeric(test.split[[n]][1]))
}

test.time2 <- c()
for(n in 1:nrow(test.sample.chicago.df))
{
  test.time2 <- c(test.time2, as.character(test.time1[[n]][3]))
}


test.time.of.the.day <- c(rep("Night", nrow(test.sample.chicago.df)))
test.time.df <- data.frame(test.hour,test.time2,test.time.of.the.day,stringsAsFactors = F)

test.condition <- test.time.df$test.time2 == "PM"
test.time.df$test.hour[test.condition] <- test.time.df$test.hour[test.condition] + 12


for(n in 1:nrow(test.sample.chicago.df))
{
  if(test.time.df[n,1] >= 1 && test.time.df[n,1] < 4)
  {
    test.time.df[n,3] = "Early Morning"
  }
  else if (test.time.df[n,1] >= 4 && test.time.df[n,1] < 6)
  {
    test.time.df[n,3] = "Dawn"
  }
  else if (test.time.df[n,1] >= 6 && test.time.df[n,1] < 12)
  {
    test.time.df[n,3] = "Morning"
  }
  else if (test.time.df[n,1] >= 12 && test.time.df[n,1] < 16)
  {
    test.time.df[n,3] = "Afternoon"
  }
  else if(test.time.df[n,1] >= 16 && test.time.df[n,1] < 21)
  {
    test.time.df[n,3] = "Evening"
  }
}

test.TOTD <- as.factor(test.time.df$test.time.of.the.day)


test.new.for.corelation <- data.frame(test.new.primary.type, test.new.location.description, test.new.district, test.TOTD)
test.new.df <- filter(test.new.for.corelation, test.new.location.description == "STREET" | test.new.location.description == "RESIDENCE" | test.new.location.description == "APARTMENT" | test.new.location.description == "SIDEWALK" | test.new.location.description == "OTHER" | test.new.location.description == "CTA TRAIN" | test.new.location.description == "BAR OR TAVERN" | test.new.location.description == "ALLEY" | test.new.location.description == "DEPARTMENT STORE" | test.new.location.description == "RESTRAURANT")
test.new.df <- filter(test.new.df, test.new.primary.type == "BATTERY" | test.new.primary.type == "NARCOTICS" | test.new.primary.type == "THEFT")
test.new.df<-test.new.df[test.new.df$test.new.district != 31,]
colnames(test.new.df) <- c("train.new.primary.type", "train.new.location.description", "train.new.district", "TOTD")


test.new.df$train.new.primary.type <- factor(test.new.df$train.new.primary.type)
test.new.df$train.new.location.description <- factor(test.new.df$train.new.location.description)
test.new.df$train.new.district <- factor(test.new.df$train.new.district)

#Trais the model
model <- rpart(train.new.primary.type ~ train.new.location.description + train.new.district + TOTD, method="class", data=new.df.smote)

#Creates the tree for visualization
rpart.plot(model, extra=104, fallen.leaves = T, type=4, main="Rpart on New Data (Full Tree)")

#Makes the prediction on the test data 
pred <- predict(model, test.new.df, type="class")

#Confusion matrix gives us a good idea about accuracy as well as class-wise sensitivity,specificity and balanced accuracy
confusionMatrix(pred, test.new.df$train.new.primary.type)
