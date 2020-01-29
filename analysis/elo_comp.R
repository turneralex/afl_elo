source(here::here("fixture scripts", "fixture_all.R"))
source("./functions_general.R")

library(tidyverse)
library(elo)

total_correct <- function(elo_model) {
    library(tidyverse)
    
    elo_model %>% 
        as_tibble() %>% 
        mutate(
            correct = case_when(
                p.A > 0.5 & wins.A > 0.5 ~ 1,
                p.A < 0.5 & wins.A < 0.5 ~ 1,
                T                        ~ 0
            )
        ) %>% 
        pull(correct) %>% 
        sum()
}

afl_elo_history <- afl_fixture_all %>% 
    filter(season != "2019") %>% 
    mutate(home_score_adjusted = home_score / (home_score + away_score),
           home_score_shots_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
           hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))

afl_elo <- afl_fixture_all %>%
    filter(season == "2019") %>% 
    mutate(home_score_adjusted = home_score / (home_score + away_score),
           home_score_shots_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
           hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))

parameter_optim_1 <- function(data = afl_elo, par) {
    elo <- elo.run(
        home_score_adjusted ~ adjust(home_team, par[2] * hga_app) + away_team + regress(season, 1500, par[3]), 
        k = par[1],
        data = data
    ) %>% 
        as_tibble() 
    
    abs(elo$p.A - elo$wins.A) %>%
        sum()
}

elo_par_1 <- optim(
    par = c(50, 50, 0.5),
    lower = c(-Inf, -Inf, 0),
    upper = c(Inf, Inf, 1),
    parameter_optim_1, 
    data = afl_elo_history,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("k", "hga", "regress"))

elo_model_1_history <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par_1["hga"]) + away_team + regress(season, 1500, elo_par_1["regress"]), 
    k = elo_par_1["k"],
    data = afl_elo_history
) 

elo_model_1 <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par_1["hga"]) + away_team + regress(season, 1500, elo_par_1["regress"]), 
    k = elo_par_1["k"],
    data = afl_elo,
    initial.elos = elo_model_1_history %>% 
        final.elos(regressed = T)
) 

elo_model_1 %>% 
    total_correct()
    
parameter_optim_2 <- function(data = afl_elo, par) {
    elo <- elo.run(
        ((par[1] * home_score_adjusted) + (par[2] * home_score_shots_adjusted)) / (par[1] + par[2]) ~ 
            adjust(home_team, par[4] * hga_app) + 
            away_team + 
            regress(season, 1500, par[5]), 
        k = par[3],
        data = data
    ) %>% 
        as_tibble()
    
    abs(elo$p.A - elo$wins.A) %>% 
        sum()
}

elo_par_2 <- optim(
    par = c(0.5, 0.5, 50, 50, 0.5),
    lower = c(0, 0, -Inf, -Inf, 0),
    upper = c(Inf, Inf, Inf, Inf, 1),
    parameter_optim_2, 
    data = afl_elo_history,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("score_weight", "score_shots_weight", "k", "hga", "regress"))

elo_model_2_history <- elo.run(
    ((elo_par_2["score_weight"] * home_score_adjusted) + (elo_par_2["score_shots_weight"] * home_score_shots_adjusted)) / (elo_par_2["score_weight"] + elo_par_2["score_shots_weight"]) ~ 
        adjust(home_team, elo_par_2["hga"] * hga_app) + 
        away_team + 
        regress(season, 1500, elo_par_2["regress"]), 
    k = elo_par_2["k"],
    data = afl_elo_history
)

elo_model_2 <- elo.run(
    ((elo_par_2["score_weight"] * home_score_adjusted) + (elo_par_2["score_shots_weight"] * home_score_shots_adjusted)) / (elo_par_2["score_weight"] + elo_par_2["score_shots_weight"]) ~ 
        adjust(home_team, elo_par_2["hga"] * hga_app) + 
        away_team + 
        regress(season, 1500, elo_par_2["regress"]), 
    k = elo_par_2["k"],
    data = afl_elo,
    initial.elos = elo_model_2_history %>% 
        final.elos(regressed = T)
)

elo_model_2 %>% 
    total_correct()

parameter_optim_3 <- function(data = afl_elo, par) {
    elo <- elo.run(
        home_score_shots_adjusted ~ adjust(home_team, par[2] * hga_app) + away_team + regress(season, 1500, par[3]), 
        k = par[1],
        data = data
    ) %>% 
        as_tibble()
    
    abs(elo$p.A - elo$wins.A) %>% 
        sum()
}

elo_par_3 <- optim(
    par = c(50, 50, 0.5),
    lower = c(-Inf, -Inf, 0),
    upper = c(Inf, Inf, 1),
    parameter_optim_3, 
    data = afl_elo_history,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("k", "hga", "regress"))

elo_model_3_history <- elo.run(
    home_score_shots_adjusted ~ adjust(home_team, hga_app * elo_par_3["hga"]) + away_team + regress(season, 1500, elo_par_3["regress"]), 
    k = elo_par_3["k"],
    data = afl_elo_history
) 

elo_model_3 <- elo.run(
    home_score_shots_adjusted ~ adjust(home_team, hga_app * elo_par_3["hga"]) + away_team + regress(season, 1500, elo_par_3["regress"]), 
    k = elo_par_3["k"],
    data = afl_elo,
    initial.elos = elo_model_3_history %>% 
        final.elos(regressed = T)
) 

elo_model_3 %>% 
    total_correct()
