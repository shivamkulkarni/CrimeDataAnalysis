library(ggplot2)
library(gplots)

crime.freq.df <- read.csv("hive_que1.csv", header = F, stringsAsFactors = F)

ggplot(crime.freq.df, aes(x=crimes, y= crimes.frequency))+
  xlab("CRIMES")+
  ylab("FREQUENCY") +
geom_bar(stat = "identity", fill="red") +
  coord_flip() +
  labs(title="Most Occuring Crimes", 
       subtitle="In City of Chicago") +
  theme(axis.text.x = element_text(vjust=0.6))


theme_set(theme_classic())

location.freq.df  <- read.csv("hive_que2.csv", header = F, stringsAsFactors = F)
colnames(location.freq.df) <- c("Locations", "Frequency")
pie <- ggplot(location.freq.df, aes(x = "", y=Frequency, fill = factor(Locations))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Locations", 
       x=NULL, 
       y=NULL, 
       title="Location wise crime frequncy")

pie + coord_polar(theta = "y", start=0)

hours.crimes.df <- read.csv("hive_que3.csv", header = F, stringsAsFactors = F)
hours.crimes.df[1,1] = 1
colnames(hours.crimes.df) <- c("AtHour", "CrimeType", "Freq")
hours.crimes.df$AtHour <- as.numeric(hours.crimes.df$AtHour)
hours.crimes.df$CrimeType <- as.factor(hours.crimes.df$CrimeType)
hours.crimes.df <- hours.crimes.df[order(hours.crimes.df$AtHour),]
test.res <- xtabs(hours.crimes.df$Freq~hours.crimes.df$AtHour+hours.crimes.df$CrimeType, data=hours.crimes.df)

heatmap.2(test.res, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none", key = T)
