library(tidyverse)
library(janitor)

player_stats <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
str(player_stats)

# clean variable names to remove % and when a number is the column name
player_stats <- clean_names(player_stats)

# Remove the total rows and leave the individuals tema rows for position and team calacultion
player_stats <- player_stats %>% 
  filter(!tm == "TOT")

# view which  players are duplicated and if they have a different age recorded
player_stats %>% 
  group_by(player_name, age) %>% 
  count() %>% 
  filter(n >1) %>% 
  pull %>%  
  length()

# some players have multiple positions listed so a primary position based on which one 
## they have played the most games on needs to be attributed
player_stats[player_stats$player_name == "Thon Maker",c("player_name","tm", "pos")]

# create a table that conatins how many games each player has played at each position
position_game <- player_stats %>% 
  group_by(player_name) %>% 
  summarise(PG = if_else(pos == "PG", g, 0),
            SG = if_else(pos == "SG", g, 0),
            SF = if_else(pos == "SF", g, 0),
            PF = if_else(pos == "PF", g, 0),
            C = if_else(pos == "C", g, 0)) 
# Attain the total number of games played at each position
position_names <- c("PG", "SG", "SF", "PF", "C")
position_game <- position_game %>% 
  group_by(player_name) %>%
  summarize_at(position_names, sum) %>%
  ungroup()

# return the column index of the position the player has teh max games
pos <- position_game %>% 
  select(position_names) %>% 
  apply(., 1, which.max)

# match the position and player name
player_pos <- tibble(player_name = position_game$player_name, Pos = position_names[pos]) 

# unselect the old poistion and right join the new position value
player_stats <- player_stats %>% 
  select(!pos) %>% 
  right_join(player_pos, by = "player_name") 

# double check the position names have been updated
player_stats[player_stats$player_name == "Thon Maker",c("player_name", "Pos")]

# perform the same process for players that played on multiple teams
team_game <- player_stats %>% 
  group_by(player_name) %>% 
  summarise(OKC = if_else(tm == "OKC", g, 0),
            PHO = if_else(tm == "PHO", g, 0),
            ATL = if_else(tm == "ATL", g, 0),
            MIA = if_else(tm == "MIA", g, 0),
            CLE = if_else(tm == "CLE", g, 0), 
            DEN = if_else(tm == "DEN", g, 0),
            SAS = if_else(tm == "SAS", g, 0),
            CHI = if_else(tm == "CHI", g, 0),
            UTA = if_else(tm == "UTA", g, 0),
            BRK = if_else(tm == "BRK", g, 0),
            NYK = if_else(tm == "NYK", g, 0),
            POR = if_else(tm == "POR", g, 0),
            MEM = if_else(tm == "MEM", g, 0),
            TOT = if_else(tm == "TOT", g, 0),
            IND = if_else(tm == "IND", g, 0),
            MIL = if_else(tm == "MIL", g, 0),
            DAL = if_else(tm == "DAL", g, 0),
            HOU = if_else(tm == "HOU", g, 0),
            TOR = if_else(tm == "TOR", g, 0),
            WAS = if_else(tm == "WAS", g, 0),
            ORL = if_else(tm == "ORL", g, 0),
            CHO = if_else(tm == "CHO", g, 0),
            SAC = if_else(tm == "SAC", g, 0),
            LAL = if_else(tm == "LAL", g, 0),
            MIN = if_else(tm == "MIN", g, 0),
            BOS = if_else(tm == "BOS", g, 0),
            GSW = if_else(tm == "GSW", g, 0),
            NOP = if_else(tm == "NOP", g, 0),
            LAC = if_else(tm == "LAC", g, 0),
            PHI = if_else(tm == "PHI", g, 0),
            DET = if_else(tm == "DET", g, 0)) 
team_game[team_game$player_name == "Alec Burks",]
team_names <- c("OKC", "PHO", "ATL", "MIA", "CLE", 
                "DEN", "SAS", "CHI", "UTA", "BRK", 
                "NYK", "POR", "MEM", "TOT", "IND", 
                "MIL", "DAL", "HOU", "TOR", "WAS", 
                "ORL", "CHO", "SAC", "LAL", "MIN", 
                "BOS", "GSW", "NOP", "LAC", "PHI", "DET")
team_game <- team_game %>% 
  group_by(player_name) %>%
  summarize_at(team_names, sum) %>%
  ungroup()
team <- team_game %>% 
  select(team_names) %>% 
  apply(., 1, which.max)
player_tm <- tibble(player_name = team_game$player_name, Tm = team_names[team]) 
player_stats <- player_stats %>%
  select(!tm) %>% 
  right_join(player_tm, by = "player_name") 
player_stats[player_stats$player_name == "Alec Burks",c("player_name", "Tm")]

# group by player names then sum so each player has one row in the final database
player_stats <- player_stats %>% 
  group_by(player_name) %>% 
  mutate(across(c("g":"pts"), sum)) %>% 
  distinct()

# find the rows that contain NA values
# all values seem to come from the FG, 2P, 3P and eFG percentages due to teh player not attempting that shot type
## or there are shot attempts but not calcualtion provided
player_stats[!complete.cases(player_stats),]

# Recalcaulte all the shot percentages to fill those that have attempts bu NA value
player_stats <- player_stats %>% 
  mutate(x3p_percent = x3p/x3pa,
         x2p_percent = x2p/x2pa,
         ft_percent = ft/fta,
         fg_percent = fg/fga,
         e_fg_percent = (fg + 0.5 * x3p)/ fga)

# if the player has no attmpt imput the % for that shot type as 0
player_stats <- player_stats %>% 
  mutate(x3p_percent = if_else(x3pa == 0 , 0, x3p_percent),
         x2p_percent = if_else(x2pa == 0 , 0, x2p_percent),
         ft_percent = if_else(fta == 0 , 0, ft_percent),
         fg_percent = if_else(fga == 0 , 0, fg_percent),
         e_fg_percent = if_else(fga == 0 & x3pa == 0, 0, e_fg_percent)) 

# check for an NA values
player_stats[!complete.cases(player_stats),]

write_csv(player_stats, "data/processed/cleaned_player-stats.csv")
