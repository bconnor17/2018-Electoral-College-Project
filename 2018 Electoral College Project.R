# 2018 Electoral College Project

# Libraries
library(politicaldata)
library(tidyverse)

# Load data
house_data_full <- read.csv("1976-2018-house2.csv")
pres_bycd <- politicaldata::pres_results_by_cd
electoral_votes <- read.csv("electoral votes.csv")

# Clean data

## Test case - 2016 and 2018
house2018 <- house_data_full %>%
  filter(year == "2018") %>%

house2016 <- house_data_full %>%
  filter(year == "2016") %>%
  
  

## Clean up, use only gen election data
  
### 2018
house2018 <- house2018 %>%
  filter(runoff == FALSE)

house2018.gen <- house2018 %>%
  select(year, state, state_po, district, stage, special, candidate, party, 
         candidatevotes, totalvotes, unofficial)%>%
  mutate(party = replace_na(party, "other")) %>%
  filter(stage == "gen",
         special == FALSE) %>%
  pivot_wider(id_cols = c(state_po, state,district, candidate, year, totalvotes), 
              names_from = party, 
              values_from = candidatevotes) %>%
  group_by(state_po, state, district, year, totalvotes) %>%
  summarize(dem_con = sum(democrat, na.rm = T),
            rep_con = sum(republican, na.rm = T)) %>%
  mutate(other_con = totalvotes - (dem_con + rep_con),
         demcon_share = dem_con/totalvotes,
         repcon_share = rep_con/totalvotes,
         othercon_share = other_con/totalvotes) %>%
  rename(state_abb = state_po)

house2018_state <- house2018.gen %>%
  group_by(state_abb) %>%
  summarize(totalvotes = sum(totalvotes),
            dem_vote = sum(dem_con),
            rep_vote = sum(rep_con),
            other_vote = sum(other_con)) %>%
  mutate(dem_share = dem_vote/totalvotes,
         rep_share = rep_vote/totalvotes,
         other_share = other_vote/totalvotes,
         winner = ifelse(dem_share>rep_share, "D", "R"))

house2018_state <- merge(house2018_state, electoral_votes) %>%
  mutate(rep_ev = ifelse(winner == "R", ev, 0),
         dem_ev = ifelse(winner == "D", ev, 0))

# In 2018, with a strict apportionment of congressional votes, the Democrats would
# have won 286 EVs (283 + DC), and the Republicans would have won 252.