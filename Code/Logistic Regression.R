setwd("/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project/Project Data")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)
library(MASS)
library(GGally)
library(speedglm)

data <- read.csv("train_data.csv")

data$gmDate <- as.Date(data$gmDate, format="%Y-%m-%d")

data <- data %>%
  group_by(teamAbbr, year) %>%
  mutate(prev_teamAST = lag(teamAST, order_by = gmDate)) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr, year) %>%
  mutate(prev_team3PM = lag(team3PM, order_by = gmDate)) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr, year) %>%
  mutate(prev_teamTRB = lag(teamTRB, order_by = gmDate)) %>%
  ungroup()

data_clean <- na.omit(data)

summary(data_clean)

modelAST <- glm.nb(teamAST ~ prev_teamAST + teamAvgAST + opptAvgDRTG, 
                   data = data_clean, weights = data_clean$weights,
                   maxit = 1000)

summary(modelAST)

set.seed(123)

train_ind <- rbinom(nrow(data_clean), 1, 0.8)

train_data <- data_clean[train_ind==1,]
test_data <- data_clean[train_ind!=1,]

modelAST_train <- glm.nb(teamAST ~  prev_teamAST + teamAvgAST + opptAvgDRTG, 
                          data = train_data, weights = train_data$weights,
                          maxit = 1000)

test_data$pred_test <- predict(modelAST_train, newdata = test_data, type = "response")

rmse <- sqrt(mean((test_data$teamAST - test_data$pred_test)^2))
rmse

model3PM <- glm.nb(team3PM ~ prev_team3PM + teamAvg3PM + opptAvgDRTG, 
                   data = data_clean, weights = data_clean$weights,
                   maxit = 1000)

summary(model3PM)

set.seed(123)

train_ind <- rbinom(nrow(data_clean), 1, 0.8)

train_data <- data_clean[train_ind==1,]
test_data <- data_clean[train_ind!=1,]

model3PM_train <- glm.nb(team3PM ~  prev_team3PM + teamAvg3PM + opptAvgDRTG, 
                         data = train_data, weights = train_data$weights,
                         maxit = 1000)

test_data$pred_test <- predict(model3PM_train, newdata = test_data, type = "response")

rmse <- sqrt(mean((test_data$team3PM - test_data$pred_test)^2))
rmse

summary(data$team3PM)
var(data$team3PM)

modelTRB <- glm.nb(teamTRB ~ prev_teamTRB + teamAvgTRB + opptAvgDRTG, 
                   data = data_clean, weights = data_clean$weights,
                   maxit = 1000)

summary(model3PM)

set.seed(123)

train_ind <- rbinom(nrow(data_clean), 1, 0.8)

train_data <- data_clean[train_ind==1,]
test_data <- data_clean[train_ind!=1,]

modelTRB_train <- glm.nb(teamTRB ~  prev_teamTRB + teamAvgTRB + opptAvgDRTG, 
                         data = train_data, weights = train_data$weights,
                         maxit = 1000)

test_data$pred_test <- predict(model3PM_train, newdata = test_data, type = "response")

rmse <- sqrt(mean((test_data$team3PM - test_data$pred_test)^2))
rmse

summary(data$team3PM)
var(data$team3PM)


future_data <- read.csv("test_data.csv")

future_data$gmDate <- as.Date(future_data$gmDate, format="%Y-%m-%d")

future_data <- future_data %>%
  arrange(future_data$gmDate)

future_data <- future_data %>%
  group_by(teamAbbr, year) %>%
  mutate(prev_teamAST = lag(teamAST, order_by = gmDate)) %>%
  ungroup()

future_data$teamAST <- predict(modelAST, newdata = future_data, type = "response")

summary(data_clean$teamAST)
summary(future_data$teamAST)










