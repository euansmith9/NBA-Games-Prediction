setwd("/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Other/data")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(zoo)
library(MASS)
library(GGally)

data <- read.csv("1949-2020_officialBoxScore.csv", stringsAsFactors = TRUE)

data$gmDate <- as.Date(data$gmDate, format="%d/%m/%Y")

data <- data %>% distinct()

data <- arrange(data, gmDate)

data <- data %>%
  filter(gmDate > as.Date("1983-08-31"))

data <- data %>%
  filter(seasonType == "Regular")

write.csv(data, "/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project/Project Data/firstdraft.csv")

setwd("/Users/euansmith9/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fourth Year/MM401/Project/Project Data")

data <- read.csv("firstdraft.csv")

team_mapping <- list(
  CHH = "CHA", 
  NJN = "BRK", 
  NOH = "NOP", 
  NOK = "NOP", 
  SEA = "OKC", 
  VAN = "MEM", 
  CHO = "CHA", 
  KCK = "SAC",
  SDC = "LAC",
  WSB = "WAS"  
)

data <- data %>%
  filter(gmDate < as.Date("2020-08-15"))

update_abbr <- function(abbr, mapping) {
  if (abbr %in% names(mapping)) {
    return(mapping[[abbr]])
  } else {
    return(abbr)
  }
}

data <- data %>%
  mutate(teamAbbr = sapply(teamAbbr, function(abbr) update_abbr(abbr, team_mapping)),
         opptAbbr = sapply(opptAbbr, function(abbr) update_abbr(abbr, team_mapping)))

data$gmDate <- as.Date(data$gmDate, format="%Y-%m-%d")

data$game_id <- ifelse(data$teamLoc == "Away", 
                       paste0(format(data$gmDate, "%Y%m%d"), data$opptAbbr), 
                       paste0(format(data$gmDate, "%Y%m%d"), data$teamAbbr))

