source("./functions_general.R")

library(tidyverse)
library(elo)

afl_fixture_2019 <- afl_fixture_2019 %>% 
    mutate(
        home_score = replace(home_score, .$round == "Round 1", get_round_scores(2019, round = 1) %>% pull(home_score)),
        away_score = replace(away_score, .$round == "Round 1", get_round_scores(2019, round = 1) %>% pull(away_score)),
        home_score = replace(home_score, .$round == "Round 2", get_round_scores(2019, round = 2) %>% pull(home_score)),
        away_score = replace(away_score, .$round == "Round 2", get_round_scores(2019, round = 2) %>% pull(away_score)),
        home_score = replace(home_score, .$round == "Round 3", get_round_scores(2019, round = 3) %>% pull(home_score)),
        away_score = replace(away_score, .$round == "Round 3", get_round_scores(2019, round = 3) %>% pull(away_score)),
        home_score = replace(home_score, .$round == "Round 4", get_round_scores(2019, round = 4) %>% pull(home_score)),
        away_score = replace(away_score, .$round == "Round 4", get_round_scores(2019, round = 4) %>% pull(away_score))
    )

afl_elo <- bind_rows(
    afl_elo,
    afl_fixture_2019 %>% 
        filter(round %in% paste("Round", 1:4)) %>% # update this
        mutate(home_score_adjusted = home_score / (home_score + away_score),
               hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))
) 

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 

elo_model %>% 
    final.elos() %>% 
    sort(decreasing = T)

elo_model %>% 
    predict(
        newdata = afl_fixture_2019 %>% 
            filter(round == "Round 5") %>% 
            mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))
    ) %>% 
    set_names(
        afl_fixture_2019 %>% 
            filter(round == "Round 5") %>% 
            pull(home_team)
    )



