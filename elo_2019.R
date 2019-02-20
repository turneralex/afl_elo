source("./functions_general.R")

library(tidyverse)
library(elo)

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 

final_elo_2018 <- elo_model %>% 
    final.elos() %>% 
    enframe() %>% 
    select(team = name, rating = value) %>% 
    arrange(desc(rating))

final_elo_2018

initial_elo_2019 <- final_elo_2018 %>% 
    mutate(rating = rating * (1 - elo_par["regress"]) + 1500 * elo_par["regress"])

initial_home_elo <- map_dbl(
    afl_fixture_2019$home_team[1:9], 
    ~ initial_elo_2019 %>% 
        filter(team == .x) %>% 
        pull(rating)
)

initial_away_elo <- map_dbl(
    afl_fixture_2019$away_team[1:9], 
    ~ initial_elo_2019 %>% 
        filter(team == .x) %>% 
        pull(rating)
)

home_app <- afl_fixture_2019 %>%
    filter(round == "Round 1") %>% 
    mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>% 
    pull(hga_app)

elo.prob(initial_home_elo + home_app * elo_par["hga"], initial_away_elo)