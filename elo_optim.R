source("./functions_general.R")

library(tidyverse)
library(elo)

afl_elo <- afl_fixture_all %>% 
    filter(season != "2019") %>% 
    mutate(home_score_adjusted = home_score / (home_score + away_score),
           hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))

parameter_optim <- function(data = afl_elo, par) {
    
    elo <- elo.run(
        home_score_adjusted ~ adjust(home_team, par[2] * hga_app) + away_team + regress(season, 1500, par[3]), 
        k = par[1],
        data = data
    ) %>% 
        as_tibble()
    
    abs(elo$p.A - elo$wins.A) %>% 
        sum()
}

parameter_optim(par = c(50, 50, 0.5)) 

elo_par <- optim(
    par = c(50, 50, 0.5),
    lower = c(-Inf, -Inf, 0),
    upper = c(Inf, Inf, 1),
    parameter_optim, 
    data = afl_elo,
    method="L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("k", "hga", "regress"))

elo_par

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
