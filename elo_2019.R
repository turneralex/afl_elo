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

rd1_home_elo <- map_dbl(
    afl_fixture_2019$home_team[1:9], 
    ~ initial_elo_2019 %>% 
        filter(team == .x) %>% 
        pull(rating)
)

rd1_away_elo <- map_dbl(
    afl_fixture_2019$away_team[1:9], 
    ~ initial_elo_2019 %>% 
        filter(team == .x) %>% 
        pull(rating)
)

rd1_home <- afl_fixture_2019 %>%
    filter(round == "Round 1") %>% 
    mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>% 
    pull(hga_app)

rd1_prob <- elo.prob(rd1_home_elo + rd1_home * elo_par["hga"], rd1_away_elo)

pmap_chr(
    list(
        rd1_prob,
        afl_fixture_2019$home_team[1:9],
        afl_fixture_2019$away_team[1:9]
    ),
    ~ if_else(..1 > 0.5, ..2, ..3)
)

