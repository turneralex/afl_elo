source(here::here("fixture scripts/fixture_all.R"))
source("./elo_optim.R")
source("./functions_general.R")

library(tidyverse)
library(elo)

afl_elo <- afl_fixture_all %>% 
    mutate(home_score_adjusted = home_score / (home_score + away_score),
           hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 

teams <- afl_fixture_all %>% 
    select(home_team) %>% 
    pull() %>% 
    unique() %>% 
    sort()

afl_elo_all <- elo_model$elos %>% 
    as_tibble(.name_repair = "unique") %>% 
    select(4, 7, 8) 

colnames(afl_elo_all) <- c("home_score_elo", "new_home_elo", "new_away_elo")

afl_elo_all <- afl_elo %>% 
    bind_cols(afl_elo_all) 

afl_elo_all %>% 
    filter((home_team == "Fremantle" | away_team == "Fremantle" ) & season == "2018") %>% 
    mutate(
        elo_rating = if_else(
            home_team == "Fremantle", new_home_elo, new_away_elo
        )
    ) %>% 
    ggplot(aes(date, elo_rating)) +
    geom_line()

teams %>% 
    map(
        ~ afl_elo_all %>% 
            filter(home_team == .x | away_team == .x) %>% 
            mutate(
                match_id = 1:nrow(.),
                elo_rating = if_else(
                    home_team == .x, new_home_elo, new_away_elo
                )
            ) %>% 
            ggplot(aes(match_id, elo_rating, colour = season)) +
            geom_line() +
            geom_point() +
            scale_y_continuous(limits = c(1375, 1625)) +
            labs(title = .x)
    )
