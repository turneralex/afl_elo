source(here::here("fixture scripts/fixture_all.R"))
source(here::here("elo update/functions_general.R"))

library(tidyverse)

afl_elo <- afl_fixture_all %>% 
    convert_elo_df()

parameter_optim <- function(elo_df, par) {
    
    all_games <- unique(elo_df$match_id)
    
    for (i in 1:(length(all_games))) {
        
        game <- elo_df[elo_df$match_id == i, ] 
        
        current_season <- game$season[1]
        
        row_id <- which(elo_df$match_id == i)[1]
        
        next_season <- elo_df %>% 
            slice(row_id:nrow(.)) %>% 
            group_by(team) %>% 
            mutate(season = lead(season, default = "2019")) %>% 
            filter(team == game[1, "team"]) %>% 
            pull(season) %>% 
            magrittr::extract2(1)
        
        if (current_season == next_season) {
            regress_app <- 0
        } else {
            regress_app <- 1
        }
        
        score_expected_1 = score_expected(game[1, "start_elo"], game[2, "start_elo"], hga_app = game[1, "hga_app"], hga = par[2])
        
        score_expected_2 = score_expected(game[2, "start_elo"], game[1, "start_elo"], hga_app = game[2, "hga_app"], hga = par[2])
        
        elo_df[elo_df$match_id == i, "score_expected"] <- c(score_expected_1, score_expected_2) %>%
            unlist()
        
        new_elo_1 <- elo_update(
            elo = game[1, "start_elo"],
            elo_opp = game[2, "start_elo"],
            score_adjusted = game[1, "score_adjusted"],
            score_expected = score_expected_1,
            k = par[1],
            regress = par[3],
            regress_app = regress_app
        )
        
        new_elo_2 <- elo_update(
            elo = game[2, "start_elo"],
            elo_opp = game[1, "start_elo"],
            score_adjusted = game[2, "score_adjusted"],
            score_expected = score_expected_2,
            k = par[1],
            regress = par[3],
            regress_app = regress_app
        )
        
        elo_df[elo_df$match_id == i, "new_elo"] <- c(new_elo_1, new_elo_2) %>%
            unlist()
        
        elo_df <- elo_df %>%
            group_by(team) %>%
            mutate(start_elo = lag(new_elo, default = 1500)) %>%
            ungroup()
        
    }
    
    elo_df %>% 
        mutate(diff = abs(score_adjusted - score_expected)) %>% 
        pull(diff) %>% 
        sum()
    
}

elo_par <- optim(
    par = c(50, 50, 0.5),
    lower = c(-Inf, -Inf, 0),
    upper = c(Inf, Inf, 1),
    parameter_optim, 
    elo_df = afl_elo,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("k", "hga", "regress")) %>% 
    enframe() %>% 
    rename(param = name) %>% 
    write_csv(here::here("new elo", "elo_par.csv"))
