getwd()
#working directory
setwd("E:/Sita/BACP/R Data")
my_data=read.csv("Cold_Storage_Temp_Data.csv",header=T)
summary(my_data)
attach(my_data)
#Mean of different seasons
Mean_Season = aggregate(my_data$Temperature, list(my_data$Season),mean)
Mean_Season
#Mean and sd
Mean_Year = mean(my_data$Temperature)
SD_Year = sd(my_data$Temperature)
#Probability calculation
probability_lower = pnorm(2,2.9627,0.5086,lower.tail = TRUE)
probability_upper = pnorm(4,2.9627,0.5086,lower.tail=FALSE)
