source(here::here("fixture scripts/fixture_2020.R"))
source(here::here("fixture scripts/fixture_all.R"))
source(here::here("elo update/elo_optim.R"))
source(here::here("elo update/functions_general.R"))

library(tidyverse)
library(elo)

rounds_so_far <- 8

results_2020 <- fitzRoy::get_match_results() %>% 
    filter(lubridate::year(Date) == 2020,
           Round %in% paste0("R", 1:rounds_so_far))

afl_fixture_2020 <- 1:rounds_so_far %>% 
    map_dfr(
       ~ afl_fixture_2020 %>% 
           filter(round == paste("Round", .x)) %>% 
           mutate(
               home_score = results_2020 %>% 
                   filter(Round == paste0("R", .x)) %>% 
                   pull(Home.Points),
               away_score = results_2020 %>% 
                   filter(Round == paste0("R", .x)) %>% 
                   pull(Away.Points)
           )
    ) %>% 
    bind_rows(
        afl_fixture_2020 %>% 
            filter(!(round %in% paste("Round", 1:rounds_so_far)))
    )

afl_elo <- afl_elo %>% 
    bind_rows(
        afl_fixture_2020 %>% 
            filter(round %in% paste("Round", 1:rounds_so_far)) %>% 
            mutate(home_score_adjusted = home_score / (home_score + away_score),
                   hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))
    ) 

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 

elo_model %>% 
    final.elos(regressed = T) %>% 
    sort(decreasing = T) %>% 
    enframe() %>% 
    mutate(plus_minus = value - mean(value)) %>% 
    ggplot(aes(fct_reorder(name, plus_minus), plus_minus)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# check home games
afl_fixture_2020 %>%
    filter(round == paste("Round", rounds_so_far + 1)) %>%
    mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>%
    select(home_team, away_team, hga_app, venue) 

elo_model %>%
    predict(
        newdata = afl_fixture_2020 %>%
            filter(round == paste("Round", rounds_so_far + 1)) %>%
            mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))
    ) %>%
    set_names(
        afl_fixture_2020 %>%
            filter(round == paste("Round", rounds_so_far + 1)) %>%
            pull(home_team)
    ) %>% 
    enframe()

# tips for round 1 (different to other rounds, due to regression to the mean)
# elo_regress <- elo_model %>%
#     final.elos(regressed = T)
# 
# afl_fixture_2020 %>%
#     filter(round == "Round 1") %>%
#     mutate(hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>%
#     select(home_team, away_team, hga_app) %>%
#     mutate(elo_pred = elo.prob(elo_regress[home_team], elo_regress[away_team], adjust.A = hga_app * elo_par["hga"]))
