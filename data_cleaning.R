library(cluster)
library(factoextra)
library(dplyr)
library(datasets)
set.seed(20)

setwd("C:/R_programming")
chicago.df <- read.csv("Crimes_-_2019.csv", header = T, stringsAsFactors = F)
#str(chicago.df)

sapply(chicago.df, function(x) sum(x=="" | is.na(x)))


sapply(chicago.df, function(x) sum(x=="" | is.na(x)))*100/nrow(chicago.df)

missing <- apply(chicago.df, 1, function(x) sum(x=="" | is.na(x)))/ncol(chicago.df)

head(missing[order(-missing)])

chicago.df <- chicago.df[missing == 0,]


boxplot(chicago.df$X.Coordinate)
boxplot(chicago.df$Y.Coordinate)
 

chicago.df <- filter(chicago.df, X.Coordinate != 0, Y.Coordinate != 0)

write.csv(chicago.df,"C:/R_programming/clean_data.csv")
