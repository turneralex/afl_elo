source(here::here("fixture scripts/fixture_all.R"))
source("./functions_general.R")

library(tidyverse)
library(elo)

afl_elo <- afl_fixture_all %>% 
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

elo_par <- optim(
    par = c(50, 50, 0.5),
    lower = c(-Inf, -Inf, 0),
    upper = c(Inf, Inf, 1),
    parameter_optim, 
    data = afl_elo,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("k", "hga", "regress"))
