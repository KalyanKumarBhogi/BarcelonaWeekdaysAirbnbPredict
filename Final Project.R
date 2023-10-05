#File        : Final Project
#Project     : Intermediate AnalY1tics
#Author      : KalY1an Kumar Bhogi & Rutwiz Gangadhar Gullipalli
#Professor   : Amin Karimpour

#library data
library(readr)
library(psych)
library(corrplot)
library(randomForest)
library(caret)
install.packages("randomForest")
install.packages("caret")

#importing data
barcelona_weekdays <- read.csv("barcelona_weekdays.csv")
summary(barcelona_weekdays)
getwd()

#data cleaning
barcelona_weekdays[barcelona_weekdays==""]<- NA
barcelona_weekdays<-na.omit(barcelona_weekdays)
sapply(barcelona_weekdays,class)
dim(barcelona_weekdays)
psych::describe(barcelona_weekdays)
str(barcelona_weekdays)
#subset analysis
subset_analysis<-subset(barcelona_weekdays,room_private == "TRUE")
subset_analysis_1 <-subset(barcelona_weekdays,room_private == "TRUE" & host_is_superhost =="TRUE")
subset_analysis_2 <-subset(barcelona_weekdays,room_private == "TRUE" & host_is_superhost =="FALSE" )

#histogram for Guest Satisfaction 
hist(barcelona_weekdays$guest_satisfaction_overall,col =terrain.colors(13),xlab="Satisfaction", ylab="Frequency",main="Total Dataset ")

hist(subset_analysis$guest_satisfaction_overall,col=terrain.colors(7),xlab="Guest satisfaction", ylab="Frequency",main="Subset Private room")

#boxplot for person capacity
boxplot(barcelona_weekdays$person_capacity,main="Boxplot for total capacity")

boxplot(subset_analysis$person_capacity,main="Boxplot for total capacity with Private rooms")
#histogram for cleanliness ratings
hist(barcelona_weekdays$cleanliness_rating,col ="pink",xlab="Cleanliness rating", ylab="Frequency",main="Cleanliness rating of Airbnbs")

hist(subset_analysis$cleanliness_rating,col="pink",xlab="Cleanliness rating", ylab="Frequency",main="Cleanliness ratings for Airbnbs with Pvt Rooms")

#correlation table
corTable <- cor(barcelona_weekdays[sapply(barcelona_weekdays, is.numeric)])

corrplot(corTable)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(barcelona_weekdays$room_private, p = .8, list = FALSE)
train_data <- barcelona_weekdays[trainIndex, ]
test_data <- barcelona_weekdays[-trainIndex, ]
#logistic regression (GLM)
logreg<-glm(formula = realSum~room_type+room_private+person_capacity,data=barcelona_weekdays)
summary(logreg)
#plot
train_data$room_private<- ifelse(train_data$room_private == "private", 1, 0)
glm <- glm(room_private ~ ., data = train_data, family = "binomial", control = list(maxit = 100))
plot(glm)


#random forest 
model <- randomForest(room_private ~ ., data = train_data, ntree = 100)
predictions <- predict(rf_model, newdata = test_data)
confusion_matrix <- table(predictions, test_data$Attrition)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
confusion_matrix
accuracy
precision
recall
f1_score


