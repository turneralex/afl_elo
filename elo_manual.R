library(tidyverse)

source(here::here("fixture scripts/fixture_all.R"))
source("./functions_general.R")

afl_elo_data <- afl_fixture_all %>% 
    filter(season == "2017" | season == "2018" | season == "2019") %>% 
    mutate(match_id =  1:nrow(.),
           home_score_adjusted = home_score / (home_score + away_score),
           hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>% 
    select(season, match_id, home_team, away_team, home_score_adjusted, hga_app) %>% 
    pivot_longer(
        cols = c("home_team", "away_team"), 
        names_to = c("home_away", "temp"), 
        names_sep = "_",
        values_to = "team"
    ) %>% 
    select(season, match_id, team, home_away, hga_app, home_score_adjusted) %>% 
    mutate(
        score_adjusted = if_else(home_away == "away", 1 - home_score_adjusted, home_score_adjusted),
        score_expected = NA,
        hga_app = if_else(home_away == "away", as.integer(hga_app * -1), hga_app),
        start_elo = 1500,
        new_elo = 1500
    ) %>% 
    select(-home_away, -home_score_adjusted)

afl_elo_data

score_expected <- function(elo, elo_opp, hga_app = 0, hga = 0) {
    x <- -(elo - elo_opp + (hga_app * hga))
    1 / (1 + 10^(x / 400))
}

score_expected(1540, 1500)

elo_update <- function(elo, elo_opp, score_adjusted, score_expected, k = 70) {
    elo + k * (score_adjusted - score_expected)
}

elo_update(
    elo = 1600,
    elo_opp = 1450,
    score_adjusted = 0.67,
    score_expected = 0.8
)

elo_run <- function(elo_df) {
    all_games <- elo_df %>% 
        pull(match_id) %>% 
        unique()
    
    for (i in 1:nrow(elo_df)) {
        game <- elo_df %>% 
            filter(match_id == i)
        
        new_elo_1 <- elo_update(
            elo = game[1, "start_elo"],
            elo_opp = game[2, "start_elo"],
            score_adjusted = game[1, "score_adjusted"]
        )
        
        new_elo_2 <- elo_update(
            elo = game[2, "start_elo"],
            elo_opp = game[1, "start_elo"],
            score_adjusted = game[2, "score_adjusted"]
        )
        
        elo_df[elo_df$match_id == i, "new_elo"] <- c(new_elo_1, new_elo_2) %>% 
            unlist()
        
        elo_df <- elo_df %>%
            group_by(team) %>%
            mutate(start_elo = lag(new_elo, default = 1500)) %>%
            ungroup()
    }
    elo_df
}
