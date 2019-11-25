library(ggplot2)
library(gplots)

crimes.by.season <- read.csv("pig1_results.csv", header = F, stringsAsFactors = F)
colnames(crimes.by.season) <- c("Season", "Crime", "Freq")
crimes.by.season$Season <- as.factor(crimes.by.season$Season)
crimes.by.season$Crime <- as.factor(crimes.by.season$Crime)
crimes.by.season <- crimes.by.season[order(crimes.by.season$Season),]
test.res <- scale(xtabs(crimes.by.season$Freq~crimes.by.season$Season+crimes.by.season$Crime, data=crimes.by.season))
heatmap.2(test.res, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none", key = T, dendrogram="none",srtCol=25)


locations.by.season <- read.csv("pig2_results.csv", header = F, stringsAsFactors = F)
colnames(locations.by.season) <- c("Season", "Location", "Freq")
locations.by.season$Season <- as.factor(locations.by.season$Season)
locations.by.season$Location <- as.factor(locations.by.season$Location)
locations.by.season <- locations.by.season[order(locations.by.season$Season),]
test.res <- scale(xtabs(locations.by.season$Freq~locations.by.season$Season+locations.by.season$Location, data=locations.by.season))
heatmap.2(test.res, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none", key = T, dendrogram="none", Rowv = FALSE, Colv=FALSE)


battery.all <- read.csv("pig3_results.csv", header = F, stringsAsFactors = F)
colnames(battery.all) <- c("Location","Freq")
ggplot(battery.all, aes(x=battery.all$Location, y= battery.all$Freq))+
  xlab("LOCATION")+
  ylab("FREQUENCY") +
  geom_bar(stat = "identity", fill="blue") +
  coord_flip() +
  labs(title="Most Battery Occurrences By Location", 
       subtitle="In City Of Chicago") +
  theme(axis.text.x = element_text( vjust=0.6))

