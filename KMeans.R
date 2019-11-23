#Import all the required libraries
library(datasets)
library(SparkR)
library(cluster)
library(factoextra)
library(dplyr)

#Initialize and set sqlContext 
sc <- sparkR.init(appName="SparkR-DataFrame-example")
sqlContext <- sparkRSQL.init(sc)
set.seed(20)

#Read the data and store it in a dataframe
chicago.df <- read.csv("cleaned_data.csv", header = T, stringsAsFactors = F)

#Store all the X-Coordinates in a vector 
xcor <- chicago.df$X.Coordinate

#Store all the corresponding Y-Coordinates in a vector
ycor <- chicago.df$Y.Coordinate

#Store all the corresponding primary crime type in a vector
primary <- chicago.df$Primary.Type

#Create a dataframe called map.df with xcor, ycor and primary 
map.df <- data.frame(xcor,ycor,primary)

#Converting data.frame to DataFrame as spark.means require DataFrame
map <- createDataFrame(map.df)

# Fit a k-means model with spark.kmeans
kmeansModel <- spark.kmeans(map, ~ xcor + ycor,
                            k = 30)

#Visualize clustering results 
fviz_cluster(kmeansModel, data=map.df[1:2])

# Model summary
summary(kmeansModel)

#Table gives the class-wise frequency 
test.table <- table(kmeansModel$cluster, map.df$primary)

#Get all the cluster numbers in one vector
cluster_number <-c(1:60)

#Extract most occurring crime for every cluster
most_occurring_crime <- colnames(test.table)[apply(test.table,1,which.max)]

#Create a dataframe with cluster numbers and their most occurring crimes
cluster_results <- data.frame(cluster_number, most_occurring_crime)

#Print result on the screen
cluster_results
