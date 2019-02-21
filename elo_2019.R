source("./functions_general.R")

library(tidyverse)
library(elo)

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 

get_round_scores("https://en.wikipedia.org/wiki/2018_AFL_season", round = 1)   

afl_fixture_2019 %>% 
    filter(round == "Round 1") %>% 
    mutate()

bind_rows(
    afl_elo,
    afl_fixture_2019 %>% 
        filter(round == "Round 1") %>% 
        mutate(
            home_score = get_round_scores(2018, round = 1) %>% pull(home_score),
            away_score = get_round_scores(2018, round = 1) %>% pull(away_score),
            home_score_adjusted = home_score / (home_score + away_score),
            hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)
        )
) %>% 
    filter(season == "2019")
