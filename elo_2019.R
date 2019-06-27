source("./functions_general.R")

library(tidyverse)
library(elo)

rounds_so_far <- 1:14

afl_fixture_2019 <- map_dfr(
    rounds_so_far,
    ~ afl_fixture_2019 %>% 
        filter(round == paste("Round", .x)) %>% 
        mutate(
            home_score = replace(home_score, .$round == paste("Round", .x), get_round_scores(2019, round = .x) %>% pull(home_score)),
            away_score = replace(away_score, .$round == paste("Round", .x), get_round_scores(2019, round = .x) %>% pull(away_score))
        )
) %>% 
    bind_rows(
        afl_fixture_2019 %>% 
            filter(!(round %in% paste("Round", rounds_so_far)))
    )

afl_elo <- bind_rows(
    afl_elo,
    afl_fixture_2019 %>% 
        filter(round %in% paste("Round", rounds_so_far)) %>% 
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
    sort(decreasing = T) %>% 
    enframe()

elo_model %>% 
    predict(
        newdata = afl_fixture_2019 %>% 
            filter(round == "Round 15") %>% 
            mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))
    ) %>% 
    set_names(
        afl_fixture_2019 %>% 
            filter(round == "Round 15") %>% 
            pull(home_team)
    )
