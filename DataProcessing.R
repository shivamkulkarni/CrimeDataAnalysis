#Read cleaned data
new.chicago.df <- read.csv("clean_data.csv", header = T, stringsAsFactors = F)

#Splits the the dates as date and time 
time1 <- strsplit(new.chicago.df$Date, " ")

#Extracts the time
times <- c()
for(n in 1:nrow(new.chicago.df))
{
  times <- c(times, as.character(time1[[n]][2]))
}

#Splits the time as HH MM 
split <- strsplit(times, ":")

#Extracts the hour
hour <- c()
for(n in 1:nrow(new.chicago.df))
{
  hour <- c(hour,as.numeric(split[[n]][1]))
}
#Extracts either AM or PM
time2 <- c()
for(n in 1:nrow(new.chicago.df))
{
  time2 <- c(time2, as.character(time1[[n]][3]))
}

#Converts time in 24 hour format
condition <- time2 == "PM"
hour[condition] <- hour[condition] + 12

hour.of.the.day <- hour

#Adds the hour column to the data
new.chicago.df <- cbind(new.chicago.df,hour.of.the.day)


dates <- c()
for(n in 1:nrow(new.chicago.df))
{
  dates <- c(dates, as.character(time1[[n]][1]))
}

split.d <- strsplit(dates, "/")
month <- c()
for(n in 1:nrow(new.chicago.df))
{
  month <- c(month,as.numeric(split.d[[n]][1]))
}

season <- c(rep("Winter", nrow(new.chicago.df)))

#Assigns the correct season according to the month
for(n in 1:nrow(new.chicago.df))
{
  if(month[n] >= 3 && month[n] < 6)
  {
    season[n] = "Spring"
  }
  else if (month[n] >= 6 && month[n] < 9)
  {
    season[n] = "Summer"
  }
  else if (month[n] >= 9 && month[n] < 12)
  {
    season[n] = "Fall"
  }
}

new.chicago.df <- cbind(new.chicago.df,season)
write.csv(new.chicago.df,"C:/R_programming/new_clean_data_final.csv")
