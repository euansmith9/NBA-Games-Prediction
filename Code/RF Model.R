setwd("/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project/Project Data")

data <- read.csv("finaldata.csv")

library(ranger)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)
library(MASS)
library(GGally)
library(randomForest)

data$teamRslt <- as.factor(data$teamRslt)
data$teamLoc <- as.factor(data$teamLoc)
data$gmDate <- as.Date(data$gmDate, format="%d/%m/%Y")
head(data)

train_data <- data[data$gmDate < as.Date("2021-03-07"),]
test_data <- data[data$gmDate > as.Date("2021-03-07"),]
train_data <- na.omit(train_data)
summary(train_data)

train_data$weights <- as.numeric(train_data$weights)
train_data$year <- as.integer(train_data$year)
train_data$teamAbbr <- as.factor(train_data$teamAbbr)

set.seed(123)

rfmodel <- randomForest(teamRslt == "Win" ~ teamLoc + teamDayOff + opptDayOff + teamAvgAST + teamForm + opptForm +
                           teamAvg3PM + teamAvg2PM + teamAvgTO + teamAvgBLK + teamAvgPF + 
                           teamAvgFTM + teamAvgTRB + opptAvgDRTG + teamEloPre + opptEloPre, data = train_data,
                        weights = train_data$weights)
                     
summary(rfmodel)

test_data$teamRslt <- as.factor(test_data$teamRslt == "Win")

test_data$pred_test_rf <- predict(rfmodel1, data = test_data, type = "response")

t2_rf <- table(predicted = test_data$pred_test_rf >= 0.5, actual = test_data$teamRslt)
t2_rf

ROC(train_data$pred_train, train_data$teamRslt=="Win", plot="ROC")
ROC(test_data$pred_test_rf, test_data$teamRslt=="Win", plot="ROC")



