setwd("/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)
library(MASS)
library(GGally)

data1 <- read.csv("nba_elo.csv", stringsAsFactors = TRUE)

colnames(data1)[1] <- "gmDate"
colnames(data1)[2] <- "year"
colnames(data1)[4] <- "seasonType"
colnames(data1)[5] <- "opptAbbr"
colnames(data1)[6] <- "teamAbbr"
colnames(data1)[7] <- "opptEloPre"
colnames(data1)[8] <- "teamEloPre"
colnames(data1)[11] <- "opptEloPost"
colnames(data1)[12] <- "teamEloPost"

data1$gmDate <- as.Date(data1$gmDate, format="%d/%m/%Y")

data1 <- data1 %>%
  filter(gmDate > as.Date("1983-08-31"))

data1 <- data1 %>%
  filter(gmDate < as.Date("2021-8-31"))

data1$decade <- cut(data1$year,
                   breaks = c(1980, 1990, 2000, 2010, 2020, 2030),
                   labels = c("1980s", "1990s", "2000s", "2010s", "2020s"), right = FALSE)


data1 <- data1 %>%
  filter(!seasonType %in% c("t", "q", "s", "c", "f", "p"))

data1$seasonType <- "Regular"

write.csv(data1, "/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project/Project Data/elo_data.csv")

