setwd("/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project/Project Data")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)
library(MASS)
library(GGally)

data <- read.csv("train_data.csv")

data$gmDate <- as.Date(data$gmDate, format="%d/%m/%y")

data <- arrange(data, gmDate)

data$year <- as.integer(substr(data$season, 1, 4)) + 1

data$decade <- cut(data$year,
                   breaks = c(1980, 1990, 2000, 2010, 2020, 2030),
                   labels = c("1980s", "1990s", "2000s", "2010s", "2020s"), right = FALSE)

data$weights <- ifelse(data$year == 2021, 16,
                       ifelse(data$year == 2020, 8,
                              ifelse(data$year == 2019, 4,
                                     ifelse(data$year >= 2010 & data$year < 2019, 2, 1))))

data <- data %>%
  mutate(win = ifelse(teamRslt == "Win", 1, 0)) %>%
  group_by(teamAbbr) %>%
  mutate(teamForm = rollapply(win, 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  mutate(win = ifelse(opptRslt == "Win", 1, 0)) %>%
  group_by(opptAbbr) %>%
  mutate(opptForm = rollapply(win, 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  dplyr::select(game_id, gmDate, year, decade, teamAbbr, teamWins, teamLosses, teamForm, teamLoc, teamDayOff, teamPTS, 
                teamAST, teamTO, teamSTL, teamBLK, teamPF, team3PM, team2PM, teamFTM, opptAbbr, opptWins, opptLosses, 
                opptLoc, opptForm, opptDayOff, opptDrtg, teamPTS, teamAST, teamTO, teamSTL, teamBLK, teamPF, team3PM, 
                teamRslt, weights)

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvgAST = rollapply(teamAST, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvg3PM = rollapply(team3PM, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvg2PM = rollapply(team2PM, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvgTO = rollapply(teamTO, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvgBLK = rollapply(teamBLK, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvgSTL = rollapply(teamSTL, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvgPF = rollapply(teamPF, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(teamAbbr) %>%
  mutate(teamAvgFTM = rollapply(teamFTM, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

data <- data %>%
  group_by(opptAbbr) %>%
  mutate(opptAvgDRTG = rollapply(opptDrtg, width = 20, FUN = mean, partial = TRUE, align = 'right')) %>%
  ungroup()

summary(data)

ast_model <- glm.nb(teamAST ~ teamLosses+ teamAvgAST + teamLoc + opptAvgDRTG + 
                    opptLosses + opptWins, data = data, weights = data$weights, maxit = 1000)

par(mfrow=c(2,2))
plot(ast_model)

summary(data$teamAST)
var(data$teamAST)
summary(ast_model)

install.packages("car")
library(car)

linear_model <- lm(teamAST ~ teamLosses + teamAvgAST + teamLoc + opptAvgDRTG + opptLosses + opptWins, data = data, weights = data$weights)

vif(linear_model)


base.model <- glm.nb(teamAST ~ 1, data = data, weights = data$weights)

?glm.nb

ideal.model <- stepAIC(ast_model, scope = list(lower = base.model, upper = ast_model), direction = "both")

set.seed(123)
training_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
training_data <- data[training_indices, ]
test_data <- data[-training_indices, ]


ast_model <- glm.nb(teamAST ~ teamWins + teamLosses + teamAvgAST + teamLoc + opptDrtg + 
                      opptForm + opptLosses, data = training_data, weights = training_data$weights, maxit = 1000)

test_data$predicted_ast <- predict(ast_model, newdata = test_data, type = "response")

rmse <- sqrt(mean((test_data$teamAST - test_data$predicted_ast)^2))
rmse

ggplot(test_data, aes(x=predicted_ast, y=teamAST)) +
  geom_point() +
  geom_line(aes(x=teamAST, y=teamAST), color='red')

threepm_model <- glm.nb(team3PM ~ team3PA + team3P. + teamForm +teamWins + teamLosses + 
                        teamAvg3PM + teamLoc + opptDrtg + opptForm + opptLosses, 
                        data = data, weights = data$weights, maxit = 1000)
is.na(data)
summary(threepm_model)

base.model <- glm(teamRslt ~ 1, data = newdata, family = "binomial", weights = newdata$weights)

ideal.model <- stepAIC(threepm_model, scope = list(lower = base.model, upper = threepm_model), direction = "both")

set.seed(123)
training_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
training_data <- data[training_indices, ]
test_data <- data[-training_indices, ]


ast_model <- glm.nb(teamAST ~ teamWins + teamLosses + teamAvgAST + teamLoc + opptDrtg + 
                      opptForm + opptLosses, data = training_data, weights = training_data$weights)

test_data$predicted_ast <- predict(ast_model, newdata = test_data, type = "response")

rmse <- sqrt(mean((test_data$teamAST - test_data$predicted_ast)^2))
rmse

ggplot(test_data, aes(x=predicted_ast, y=teamAST)) +
  geom_point() +
  geom_line(aes(x=teamAST, y=teamAST), color='red')

