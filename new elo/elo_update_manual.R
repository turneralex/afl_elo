source(here::here("fixture scripts/fixture_2020.R"))
source(here::here("fixture scripts/fixture_all.R"))
source(here::here("elo update/functions_general.R"))

library(tidyverse)

elo_par <- read_csv("./new elo/elo_par.csv") %>% 
    deframe()

rounds_so_far <- 8

results_2020 <- fitzRoy::get_match_results() %>% 
    filter(lubridate::year(Date) == 2020,
           Round %in% paste0("R", 1:rounds_so_far))

elo <- afl_fixture_all %>% 
    mutate(home_score_adjusted = home_score / (home_score + away_score),
           hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>% 
    bind_rows(
        1:rounds_so_far %>% 
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
        ) %>% 
    convert_elo_df() %>% 
    elo_run(hga = elo_par["hga"], k = elo_par["k"], regress = elo_par["regress"])

elo %>% 
    filter(!is.na(start_elo)) %>% 
    group_by(team) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    select(team, start_elo) %>% 
    arrange(start_elo) %>% 
    mutate(plus_minus = start_elo - mean(start_elo)) %>% 
    ggplot(aes(fct_reorder(team, plus_minus), plus_minus)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

elo %>% 
    filter(season == "2020", 
           round == paste("Round", rounds_so_far + 1)) %>% 
    group_by(match_id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(match_id, season, round, team, score_expected) %>% 
    write_csv(here::here("check.csv"))
