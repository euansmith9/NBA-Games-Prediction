logmodel80s <- glm(teamRslt == "Win" ~ teamAvgAST + teamAvg2PM + teamAvgTO +
                     teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB + opptAvgDRTG, 
                   family = binomial, data = data80s)
summary(logmodel80s)

logmodel90s <- glm(teamRslt == "Win" ~ teamAvgAST + teamAvg3PM + teamAvgTO +
                     teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB + opptAvgDRTG, 
                   family = binomial, data = data90s)
summary(logmodel80s)

logmodel15s <- glm(teamRslt == "Win" ~ teamAvgAST + teamAvg3PM + teamAvgTO +
                     teamAvgBLK + teamAvgPF + teamAvgFTM + teamAvgTRB + opptAvgDRTG, 
                   family = binomial, data = data15s)
summary(logmodel15s)

data15s <- na.omit(data15s)

data15s$pred_train <- predict(logmodel15s, type = "response")