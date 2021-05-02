library(tidyverse)
library(janitor)

team_stats1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
team_stats1$X23 <- NULL
team_stats1$X24 <- NULL
team_stats1$X25 <- NULL
team_stats1 <- clean_names(team_stats1) 
# extract the varables that are not team based and can be calculated on an individual player basis
team_stats1 <- team_stats1 %>% 
  select(team,w, f_tr, x3p_ar, ts_percent, e_fg_percent, ft_fga)

# read in and clean names
team_stats2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")
team_stats2 <- clean_names(team_stats2) 

# join the two team stats together
team_stats <- left_join(team_stats1, team_stats2, by = "team")
team_stats[!complete.cases(team_stats),]
team_stats <- clean_names(team_stats)

# read in payroll
team_payroll <- read_csv("data/raw/2019-20_nba_team-payroll.csv")
# remove non numeric characters and convert to numeric
team_payroll$salary <- gsub(",", "", team_payroll$salary)
team_payroll$salary <- gsub("\\$", "", team_payroll$salary)
team_payroll$salary <- as.numeric(team_payroll$salary)

# read in additional data that contains the team names from the payroll and team stats
team_names <- read_csv("data/raw/team-salary-name_team-stats-name.csv")

# match the team stats name to the payroll name and drop the payroll name
team_payroll <- team_payroll %>% 
  right_join(team_names, by = "team") %>% 
  select(-team) %>% 
  rename(team = team_stats_name)

# join the payroll to the team_stats master data
team_stats <- left_join(team_stats, team_payroll, by = "team")

#pts and wins

#write out to the processed folder
write_csv(team_stats, "data/processed/cleaned_team-stats.csv")