# Rainyar


# Reading the data from csv file
weather_data <- read.csv("D:\\SRINIDI\\SEM-3\\PACKAGE\\R_LAB\\AUS_Weather_data.csv")

# Creating the dataframe
weather_data <- as.data.frame(weather_data)

# Listing all the possible locations and getting user's choice of place
loc <- levels(weather_data$Location)
loc <- as.data.frame(loc)

print("List of available locations:")
print(loc)
ch <- readline(prompt = "Enter your choice:")
ch <- as.integer(ch)
my_loc <- as.character(loc[ch,])
print(my_loc)

# Creating a subset of required values
print(my_loc)
#sub_weather_data <- weather_data[which(sub_weather_data$Location==my_loc),]
sub_weather_data<-subset(weather_data,Location==my_loc)
weather_data2 <- subset(weather_data, select = -c(Date, Location, RISK_MM, Rainfall, RainToday))


# Plotting to get an overview
par(mfrow=c(2,3))
plot(x=sub_weather_data$Humidity9am,y=sub_weather_data$Rainfall,cex=0.5,pch=15,xlab="Humidity9am",ylab="Rainfall")
plot(sub_weather_data$Rainfall ~ sub_weather_data$Humidity3pm,cex=0.5,pch=15,xlab="Humidity3pm",ylab="Rainfall")
plot(sub_weather_data$Rainfall ~ sub_weather_data$Pressure9am,cex=0.5,pch=15,xlab="Pressure9am",ylab="Rainfall")
plot(sub_weather_data$Rainfall ~ sub_weather_data$Pressure3pm,cex=0.5,pch=15,xlab="Pressure3pm",ylab="Rainfall")
plot(sub_weather_data$Rainfall ~ sub_weather_data$RISK_MM,cex=0.5,pch=15,xlab="RISK_MM",ylab="Rainfall")

# Checking the skewness of data
par(mfrow=c(3,3))
hist(sub_weather_data$MinTemp,xlab="MinTemp",ylab="",main="")
hist(sub_weather_data$MaxTemp,xlab="MaxTemp",ylab="",main="")
hist(sub_weather_data$WindGustSpeed,xlab="WindGustSpeed",ylab="",main="")
hist(sub_weather_data$WindSpeed9am,xlab="WindSpeed9am",ylab="",main="")
hist(sub_weather_data$WindSpeed3pm,xlab="WindSpeed3pm",ylab="",main="")
hist(sub_weather_data$Humidity9am,xlab="Humidity9am",ylab="",main="")
hist(sub_weather_data$Humidity3pm,xlab="Humidity3pm",ylab="",main="")
hist(sub_weather_data$Temp9am,xlab="Temp9am",ylab="",main="")
hist(sub_weather_data$Temp3pm,xlab="Temp3pm",ylab="",main="")
hist(sub_weather_data$Pressure9am,xlab="Pressure9am",ylab="",main="")
hist(sub_weather_data$Pressure3pm,xlab="Pressure3pm",ylab="",main="")

# Give the box plot
par(mfrow=c(2,3))
boxplot(sub_weather_data$Rainfall)
boxplot(sub_weather_data$Humidity3pm)
boxplot(sub_weather_data$Pressure9am)
boxplot(sub_weather_data$Pressure3pm)
boxplot(sub_weather_data$RISK_MM)

# Finding correlation
x1 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$Humidity9am)
x1 <- as.double(x1$estimate)
x11 <- "Humidity9am"
x2 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$Humidity3pm)
x2 <- as.double(x2$estimate)
x22 <- "Humidity3pm"
x3 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$Pressure9am)
x3 <- as.double(x3$estimate)
x33 <- "Pressure9am"
x4 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$Pressure3pm)
x4 <- as.double(x4$estimate)
x44 <- "Pressure3pm"
x5 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$Temp9am)
x5 <- as.double(x5$estimate)
x55 <- "Temp9am"
x6 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$Temp3pm)
x6 <- as.double(x6$estimate)
x66 <- "Temp3pm"
x7 <- cor.test(sub_weather_data$Rainfall,sub_weather_data$RISK_MM)
x7 <- as.double(x7$estimate)
x77 <- "RISK_MM"

cor_mat <- as.array(c(x1,x2,x3,x4,x5,x6,x7))
length(cor_mat)
cor_mat[2]
cor_mat_names <- as.array(c(x11,x22,x33,x44,x55,x66,x77))
cor_mat_names
req_cor_col <- as.array(c(0))
req_cor_col_name <- as.array(c("no"))

pveno = 0
for(i in c(1:length(cor_mat)))
{
  if(cor_mat[i] > 0){
    req_cor_col <- append(req_cor_col,cor_mat[i])
    req_cor_col_name <- append(req_cor_col_name,cor_mat_names[i])
    pveno = pveno + 1
  }
}
req_cor_col <- req_cor_col[1:pveno+1]
req_cor_col_name <- req_cor_col_name[1:pveno+1]

# Separately performed chi squared test to find out whether they are independent
final_col <- c("Humidity9am","Humidity3pm","RISK_MM")

# Data Modeling  
sub_weather_data$WindGustDir=as.numeric(sub_weather_data$WindGustDir,warn_missing = FALSE)
sub_weather_data$WindDir9am=as.numeric(sub_weather_data$WindDir9am,warn_missing = FALSE)
sub_weather_data$WindDir3pm=as.numeric(sub_weather_data$WindDir3pm,warn_missing = FALSE)

# Convert Raintomrrow data to numeric
library(plyr)
sub_weather_data
sub_weather_data$RainTomorrow <- revalue(sub_weather_data$RainTomorrow, c("Yes"=1),warn_missing = FALSE)
sub_weather_data$RainTomorrow <- revalue(sub_weather_data$RainTomorrow, c("No"=0),warn_missing = FALSE)
sub_weather_data

# Data us split to test and train data in the ratio 80:20
library(caTools)
set.seed(123)
split = sample.split(sub_weather_data$RainTomorrow, SplitRatio = 0.8)
train = subset(sub_weather_data, split == TRUE)
test = subset(sub_weather_data, split == FALSE)
training_set$RainTomorrow
names(train)

# Modelling - Multiple Regression
linear_model = lm(Rainfall ~ Humidity9am + Humidity3pm + RISK_MM, data = train,na.action=na.exclude)
a1 = linear_model$coefficients[1]
a2 = linear_model$coefficients[2]
a3 = linear_model$coefficients[3]
a4 = linear_model$coefficients[4]
pred = function(x1, x2, x3){
  return (a1 + a2 * x1 + a3 * x2 + a4 * x3)}

pret<-pred(test$Humidity9am,test$Humidity3pm,test$RISK_MM)
ssq<-sum((test$Rainfall-pret)^2)                                   
# MEAN SQUARE ERROR

library(effects)
par(mfrow=c(1,3))
plot(allEffects(linear_model))

#install.packages("randomForest")
library(randomForest)
set.seed(100)
model2 <- randomForest(RainTomorrow ~ Humidity9am + Humidity3pm + RISK_MM, data = train,na.action = na.omit,importance=TRUE,ntree=20)
model2
predTrain <- predict(model2, newdata = test_set[-12], type = "class")
cm_rf1 = table(test_set[,25],predTrain)
cm_rf1

# Making the Confusion Matrix
# View the forest results.
jpeg("D:\\SRINIDI\\SEM-3\\PACKAGE\\R_LAB\\Final_plot_1.jpeg")
plot(model2)
dev.off()
