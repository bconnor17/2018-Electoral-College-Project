# 2018 Electoral College Project

# Libraries
library(caret)
library(DataCombine)
library(ggplot2)
library(lmerTest)
library(modelr)
library(MuMIn)
library(politicaldata)
library(tidyverse)

# Load data
house_data_full <- read.csv("1976-2018-house2.csv", na.strings = c("","NA"))
pres_bycd <- politicaldata::pres_results_by_cd
electoral_votes <- read.csv("electoral votes.csv")

# Clean data

## Test case - 2016 and 2018
house2018 <- house_data_full %>%
  filter(year == "2018")

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

# Ok lets look at ticket splitting (at least generally)

# Pull pres data from 2004 to 2016

house2016 <- house_data_full %>%
  filter(year == "2016") %>%
  select(year, state, state_po, district, stage, special, candidate, party, 
         candidatevotes, totalvotes, unofficial) %>%
  mutate(party = replace_na(party, "other")) %>%
  filter(stage == "gen",
         special == FALSE) %>%
  pivot_wider(id_cols = c(state_po, state,district, candidate, year, totalvotes), 
              names_from = party, 
              values_from = candidatevotes)%>%
  group_by(state_po, state, district, year, totalvotes) %>%
  summarize(dem_con = sum(democrat, na.rm = T),
            rep_con = sum(republican, na.rm = T)) %>%
  mutate(other_con = totalvotes - (dem_con + rep_con),
         demcon_share = dem_con/totalvotes,
         repcon_share = rep_con/totalvotes,
         othercon_share = other_con/totalvotes) %>%
  rename(state_abb = state_po)

house_results <- politicaldata::house_results

#Clean it to get rid of "AL-" etc in district
house_results$district <- sub("...","", house_results$district)

house_results <- house_results %>%
  rename(congr_votes = total_votes,
         congr_rep = rep,
         congr_dem = dem,
         congr_other = other) %>%
  mutate(district = ifelse(district == "AL", "01", district))

house_results$district <- as.numeric(house_results$district)


house_pres <- merge(house_results,pres_bycd)%>%
  rename(pres_votes = total_votes,
         pres_rep = rep,
         pres_dem = dem,
         pres_other = other)


# Plotting

## First plot, all observations
cong_pres_plot1 <- ggplot(data = house_pres, aes(x = congr_dem, y = pres_dem)) +
  geom_point() +
  geom_smooth(method = lm)

cong_pres_plot1

## Splitting out years
cong_pres_plot2 <- ggplot(data = house_pres, aes(x = congr_dem, y = pres_dem)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~year)

cong_pres_plot2

## 2000 and 2016 with LMs
house_pres00 <- house_pres %>%
  filter(year == "2000") %>%
  distinct(year, state_abb,district, .keep_all = TRUE)

house_pres16 <- house_pres %>%
  filter(year == "2016")


cong_pres_plot00 <- ggplot(data = house_pres00, aes(x = congr_dem, y = pres_dem)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Congressional Democratic % vs. Presidential Democratic %",
       subtitle = "2000 Presidential Election",
       caption = "@bcon94")
cong_pres_plot00

cong_pres_plot16 <- ggplot(data = house_pres16, aes(x = congr_dem, y = pres_dem)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Congressional Democratic % vs. Presidential Democratic %",
       subtitle = "2016 Presidential Election",
       caption = "@bcon94")
cong_pres_plot16

cong_pres_lm00 <- lm(pres_dem ~ congr_dem, data = house_pres00)
cong_pres_lm16 <- lm(pres_dem ~ congr_dem, data = house_pres16)

summary(cong_pres_lm00)

summary(cong_pres_lm16)

## Findings: From cong_pres_plot2, we see that open years generally have more splitting than
## reelection years, with the glaring exception of 2016. Since this is year dependent,
## including past presidential and midterm results as lagged variables might make sense.

# Modeling!
## Some notes: To make things simpler, I will predict Democratic share of two party
## vote and total votes separately. This will allow us to back into Republican vote
## share and overall party votes (for EC purposes) after the fact.

## Model data
model_data.1 <- house_pres %>%
  filter(year == "2016" | year == "2012" |
         year == "2008" | year == "2004" | year == "2000") %>%
  mutate(open = ifelse(year == "2016" | year == "2008", 1, 0),   #Dummy variable for open seat
         state_district = paste0(state_abb,"-", district),
         congr_dem_twoparty = congr_dem/(congr_dem + congr_rep),  #Converting to two party share
         congr_rep_twoparty = congr_rep/(congr_dem + congr_rep),
         pres_dem_twoparty = pres_dem/(pres_dem + pres_rep),
         pres_rep_twoparty = pres_rep/(pres_dem + pres_rep)) %>% 
  arrange(state_district) %>%
  distinct(state_district, year, .keep_all = T) %>%
  slide(Var = "congr_votes", TimeVar = "year", GroupVar = "state_district", #Creating congr lags
      NewVar = "congr_votes_lag", slideBy = -1, keepInvalid = F) %>%
  slide(Var = "congr_dem_twoparty", TimeVar = "year", GroupVar = "state_district",
        NewVar = "congr_dem_lag_2", slideBy = -1, keepInvalid = F) %>%
  slide(Var = "congr_rep_twoparty", TimeVar = "year", GroupVar = "state_district",
        NewVar = "congr_rep_lag_2", slideBy = -1, keepInvalid = F) %>%
  slide(Var = "pres_votes", TimeVar = "year", GroupVar = "state_district", #Creating pres lags
        NewVar = "pres_votes_lag", slideBy = -1, keepInvalid = F) %>%
  slide(Var = "pres_dem_twoparty", TimeVar = "year", GroupVar = "state_district",
        NewVar = "pres_dem_lag_2", slideBy = -1, keepInvalid = F) %>%
  slide(Var = "pres_rep_twoparty", TimeVar = "year", GroupVar = "state_district",
        NewVar = "pres_rep_lag_2", slideBy = -1, keepInvalid = F) %>%
  filter(is.na(congr_votes_lag) == F)

model_data.1[is.na(model_data.1)] = 0     
model_data.1$state_abb <- as.factor(model_data.1$state_abb)

## Training, validation, and test sets.

# splits
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(model_data.1))
sampleSizeValidation <- floor(fractionValidation * nrow(model_data.1))
sampleSizeTest       <- floor(fractionTest       * nrow(model_data.1))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining.1    <- sort(sample(seq_len(nrow(model_data.1)), size=sampleSizeTraining))
indicesNotTraining.1 <- setdiff(seq_len(nrow(model_data.1)), indicesTraining.1)
indicesValidation.1  <- sort(sample(indicesNotTraining.1, size=sampleSizeValidation))
indicesTest.1        <- setdiff(indicesNotTraining.1, indicesValidation.1)

# Finally, output the three dataframes for training, validation and test.
model1Training   <- model_data.1[indicesTraining.1, ]
model1Validation <- model_data.1[indicesValidation.1, ]
model1Test       <- model_data.1[indicesTest.1, ]

## Train model 1
model.1 <- lmer(pres_dem_twoparty ~ congr_dem_twoparty + congr_dem_lag_2 + pres_dem_lag_2 +
                 pres_votes_lag + open + (1|state_abb), 
                 data = model1Training)

### Some metrics
summary(model.1)
r.squaredGLMM(model.1)


## Let's see how well calibrated Model 1 is

### Add predictions
model1Validation <- model1Validation %>%
  add_predictions(model.1, var = "pred_1")

### Graph them
model.1_predplot <- ggplot(data = model1Validation, aes(x = pred_1, y = pres_dem_twoparty)) +
  geom_point() +
  geom_smooth(method = loess)

model.1_predplot

### It looks .. ok. Some outliers.