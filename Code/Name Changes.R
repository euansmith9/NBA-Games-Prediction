team_mapping <- data.frame(
  old_names = c("AND", "ATL", "BAL", "BLB", "BOS", "BRK", "BUF", "CAP", "CHA", "CHH", 
                "CHI", "CHO", "CHP", "CHS", "CHZ", "CIN", "CLE", "DAL", "DEN", "DET", 
                "DNN", "FTW", "GSW", "HOU", "IND", "INO", "KCK", "KCO", "LAC", "LAL", 
                "MEM", "MIA", "MIL", "MIN", "MLH", "MNL", "NJN", "NOH", "NOJ", "NOK", 
                "NOP", "NYK", "NYN", "OKC", "ORL", "PHI", "PHO", "PHW", "POR", "ROC", 
                "SAC", "SAS", "SDC", "SDR", "SEA", "SFW", "SHE", "STB", "STL", "SYR", 
                "TOR", "TRI", "UTA", "VAN", "WAS", "WAT", "WSB", "WSC"),
  new_names = c("AND", "ATL", "WAS", "WAS", "BOS", "BKN", "LAC", "WAS", "CHA", "CHA", 
                "CHI", "CHA", "WAS", "CHS", "WAS", "SAC", "CLE", "DAL", "DEN", "DET", 
                "DEN", "DET", "GSW", "HOU", "IND", "INO", "SAC", "SAC", "LAC", "LAL", 
                "MEM", "MIA", "MIL", "MIN", "ATL", "LAL", "BKN", "NOP", "UTA", "NOP", 
                "NOP", "NYK", "BKN", "OKC", "ORL", "PHI", "PHX", "GSW", "POR", "SAC", 
                "SAC", "SAS", "LAC", "HOU", "OKC", "GSW", "SHE", "STB", "ATL", "PHI", 
                "TOR", "ATL", "UTA", "MEM", "WAS", "ATL", "WAS", "WSC")
)


length(unique(data$teamAbbr))

data <- data %>%
  left_join(team_mapping, by = c("teamAbbr" = "old_names")) %>%
  mutate(teamAbbr = ifelse(is.na(new_names), teamAbbr, new_names)) %>%
  dplyr::select(-new_names)

data <- data %>%
  left_join(team_mapping, by = c("opptAbbr" = "old_names")) %>%
  mutate(opptAbbr = ifelse(is.na(new_names), opptAbbr, new_names)) %>%
  dplyr::select(-new_names)