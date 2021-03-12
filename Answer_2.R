getwd()
#Import dataset
my_data = read.csv("Cold_Storage_Mar2018.csv",header=TRUE)
summary(my_data)
#mean and sd of the dataset
mean_temp = mean(my_data$Temperature)
sd_temp=sd(my_data$Temperature)
#hypothesis test
t.test(my_data$Temperature,mu =3.9,alt='greater',conf.level = 0.9)

