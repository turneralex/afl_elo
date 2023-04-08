# run the elo model

elo_run <- function(elo_df, k, hga_vec, regress) {
    library(tidyverse)
    
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
            hga <- hga_vec["hga_vic"]
        } else if (location == "NSW") {
            hga <- hga_vec["hga_nsw"]
        } else if (location == "QLD") {
            hga <- hga_vec["hga_qld"]
        } else if (location == "SA") {
            hga <- hga_vec["hga_sa"]
        } else if (location == "WA") {
            hga <- hga_vec["hga_wa"]
        } else if (location == "GEE") {
            hga <- hga_vec["hga_gee"]
        } else if (location == "TAS") {
            hga <- hga_vec["hga_tas"]
        } else {
            hga <- hga_vec["hga_other"]
        }
        
        score_expected_1 = score_expected(game[1, "start_elo"], game[2, "start_elo"], hga_app = game[1, "hga_app"], hga = hga) %>%
            unname()
        
        score_expected_2 = score_expected(game[2, "start_elo"], game[1, "start_elo"], hga_app = game[2, "hga_app"], hga = hga) %>%
            unname()
        
        elo_df[elo_df$match_id == i, "score_expected"] <- c(score_expected_1, score_expected_2) %>%
            unlist()
        
        new_elo_1 <- elo_update(
            elo = game[1, "start_elo"],
            elo_opp = game[2, "start_elo"],
            score_adjusted = game[1, "score_adjusted"],
            score_expected = score_expected_1,
            k = k,
            regress = regress,
            regress_app = regress_app
        )
        
        new_elo_2 <- elo_update(
            elo = game[2, "start_elo"],
            elo_opp = game[1, "start_elo"],
            score_adjusted = game[2, "score_adjusted"],
            score_expected = score_expected_2,
            k = k,
            regress = regress,
            regress_app = regress_app
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
