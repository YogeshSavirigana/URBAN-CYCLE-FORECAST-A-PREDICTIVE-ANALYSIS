rm(list=ls())
library(rio)
library(moments)
bikes_data=import("Bike_data.xlsx")
colnames(bikes_data)=tolower(make.names(colnames(bikes_data))) 
attach(bikes_data)
names(bikes_data)

set.seed(123456)
bikes_data_sample=bikes_data[sample(1:nrow(bikes_data),100),]
attach(bikes_data_sample)

continous_factors <- bikes_data[, c("rented.bike.count","hour","temperature",
                                    "humidity","wind.speed","visibility",
                                    "dew.point.temperature","solar.radiation","rainfall",
                                    "snowfall")]
#plot(continous_factors,pch=19)

corelation_data= cor(continous_factors)
corelation_data

library(corrplot)
corrplot(corelation_data,method="number")

is.factor(bikes_data_sample$functioningday)
bikes_data_sample$functioningday=as.factor(bikes_data_sample$functioningday)
is.factor(bikes_data_sample$functioningday)

#Continous variables X1, X2 are X1=hour and X2=temperature
#Binary variable is X3=FunctioningHour
#Y is the target, Y=RentedBikesCount

Y_X1 =lm(rented.bike.count~hour,data=bikes_data_sample)
summary(Y_X1)

Y_X2=lm(rented.bike.count~temperature,data=bikes_data_sample)
summary(Y_X2)

Y_X3=lm(rented.bike.count~functioningday,data=bikes_data_sample)
summary(Y_X3)

Y_X1_X2= lm(rented.bike.count~hour+temperature,data=bikes_data_sample)
summary(Y_X1_X2)

Y_X2_X3= lm(rented.bike.count~temperature+functioningday,data=bikes_data_sample)
summary(Y_X2_X3)

Y_X1_X3= lm(rented.bike.count~hour+functioningday,data=bikes_data_sample)
summary(Y_X1_X3)

Y_X1_X2_X3= lm(rented.bike.count~hour+temperature+functioningday,data=bikes_data_sample)
summary(Y_X1_X2_X3)

Y_X1_X2_X1.X2= lm(rented.bike.count~hour+temperature+(hour*temperature),data=bikes_data_sample)
summary(Y_X1_X2_X1.X2)

Y_X1_X1.X1= lm(rented.bike.count~hour+I(hour^2),data=bikes_data_sample)
summary(Y_X1_X1.X1)

Y_X2_X2.X2= lm(rented.bike.count~temperature+I(temperature^2),data=bikes_data_sample)
summary(Y_X2_X2.X2)

#As the P-values of all the factors are less than 0.05 and multiple r-squared value is 0.5019 better for multiple regression model for Y, X1, X2, X3
#Checking Linearity, Normality and Equality for linear model of (Y,X1,X2,X3)
# Linearity
plot(rented.bike.count,Y_X1_X2_X3$fitted.values,
     pch=19, xlim=c(0,3000),ylim=c(0,1600),main="Bikes count vs Fitted Values")
abline(0,1,col="red",lwd=2)

#Normality
qqnorm(Y_X1_X2_X3$residuals,pch=19,main="Bikes count  Normality Plot")
qqline(Y_X1_X2_X3$residuals,col="red",lwd=2)

#Equality of variance
plot(Y_X1_X2_X3$fitted.values,Y_X1_X2_X3$residuals,pch=19,
     main="cars Residuals")
abline(0,0,col="red",lwd=2)


#prediction 1
data1 <- data.frame(functioningday=factor("Yes", levels=c("Yes", "No")), 
                    hour=12,
                    temperature=8)

predict(Y_X1_X2_X3,data1,interval="predict")
predict(Y_X1_X2_X3,data1,interval="confidence")

#prediction2
data2 <- data.frame(functioningday=factor("No", levels=c("Yes", "No")), 
                    hour=10,
                    temperature=20)
predict(Y_X1_X2_X3,data2,interval="predict")
predict(Y_X1_X2_X3,data2,interval="confidence")