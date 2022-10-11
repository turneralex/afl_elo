source(here::here("utils/fixture scripts/fixture_all.R"))
source(here::here("utils/functions_general.R"))

library(tidyverse)

seasons_exclude <- c(
    "2010", 
    "2020", 
    "2021"
)

afl_elo <- afl_fixture_all %>% 
    filter(
        !(season %in% seasons_exclude)
    ) %>%
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
            mutate(season = lead(season, default = "9999")) %>% 
            filter(team == game[1, "team"]) %>% 
            pull(season) %>% 
            magrittr::extract2(1)
        
        if (current_season == next_season) {
            regress_app <- 0
        } else {
            regress_app <- 1
        }
        
        location <- game$location[1]
        
        if (location == "VIC") {
            hga <- par[3]
        } else if (location == "NSW") {
            hga <- par[4]
        } else if (location == "QLD") {
            hga <- par[5]
        } else if (location == "SA") {
            hga <- par[6]
        } else if (location == "WA") {
            hga <- par[7]
        } else if (location == "GEE") {
            hga <- par[8]
        } else if (location == "TAS") {
            hga <- par[9]
        } else {
            hga <- par[10]
        }
        
        score_expected_1 = score_expected(game[1, "start_elo"], game[2, "start_elo"], hga_app = game[1, "hga_app"], hga = hga)
        
        score_expected_2 = score_expected(game[2, "start_elo"], game[1, "start_elo"], hga_app = game[2, "hga_app"], hga = hga)
        
        elo_df[elo_df$match_id == i, "score_expected"] <- c(score_expected_1, score_expected_2) %>%
            unlist()
        
        new_elo_1 <- elo_update(
            elo = game[1, "start_elo"],
            elo_opp = game[2, "start_elo"],
            score_adjusted = game[1, "score_adjusted"],
            score_expected = score_expected_1,
            k = par[1],
            regress = par[2],
            regress_app = regress_app
        )
        
        new_elo_2 <- elo_update(
            elo = game[2, "start_elo"],
            elo_opp = game[1, "start_elo"],
            score_adjusted = game[2, "score_adjusted"],
            score_expected = score_expected_2,
            k = par[1],
            regress = par[2],
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
    par = c(50, 0.5, rep(50, 8)),
    lower = rep(0, 11),
    upper = c(100, 1, rep(100, 8)),
    parameter_optim, 
    elo_df = afl_elo,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    set_names(c("k", "regress", "hga_vic", "hga_nsw", "hga_qld", "hga_sa", "hga_wa", "hga_gee", "hga_tas", "hga_other"))

elo_par %>%     
    enframe() %>% 
    rename(param = name) %>% 
    write_csv(
        here::here(
            "files",
            "params",
            paste0(
                "elo_par_score_shots_",
                Sys.Date(),
                ".csv"
            )
        )
    )
