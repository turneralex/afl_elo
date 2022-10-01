score_expected <- function(elo, elo_opp, hga_app, hga) {
    x <- -(elo - elo_opp + (hga_app * hga))
    1 / (1 + 10^(x / 400))
}

elo_update <- function(elo, elo_opp, score_adjusted, score_expected, k, regress, regress_app) {
    x <- elo + k * (score_adjusted - score_expected)
    y <- (1500 * regress * regress_app) + ((1 - regress * regress_app) * x)
    y
}

is_home <- function(season_year, game_venue, game_home_team, game_away_team, data_venues, data_fixture) {
    library(tidyverse)
    
    location <- data_venues %>% 
        filter(year == season_year & venue == game_venue) %>% 
        pull(location) 
    
    away_team_location <- data_venues %>% 
        unnest(cols = teams) %>% 
        filter(year == season_year & team == game_away_team) %>% 
        inner_join(
            data_fixture %>% 
                filter(season == season_year & home_team == game_away_team) %>% 
                group_by(venue) %>% 
                count(),
            by = "venue"
        ) %>% 
        filter(n > 1) %>% 
        pull(location) %>% 
        unique()
    
    home_team_location <- data_venues %>% 
        unnest(cols = teams) %>% 
        filter(year == season_year & team == game_home_team) %>% 
        inner_join(
            data_fixture %>% 
                filter(season == season_year & home_team == game_home_team) %>% 
                group_by(venue) %>% 
                count(),
            by = "venue"
        ) %>% 
        filter(n > 1) %>% 
        pull(location) %>% 
        unique()
    
    if (location %in% away_team_location & !(location %in% home_team_location)) {
        as.integer(-1)
    } else {
        as.integer(!(location %in% away_team_location)) * as.integer(location %in% home_team_location)
    }
    
}

change_team_name <- function(team) {
    library(tidyverse)
    
    case_when(
        team == "Adelaide Crows"    ~ "Adelaide",
        team == "Brisbane Lions"    ~ "Brisbane",
        team == "Geelong Cats"      ~ "Geelong",
        team == "Gold Coast Suns"   ~ "Gold Coast",
        team == "GWS Giants"        ~ "GWS",
        team == "Sydney Swans"      ~ "Sydney",
        team == "West Coast Eagles" ~ "West Coast",
        team == "Footscray"         ~ "Western Bulldogs",
        T                           ~ team
    )
    
}

convert_elo_df <- function(fixture_df) {
    library(tidyverse)
    
    fixture_df %>% 
        left_join(
            afl_venues_all %>% 
                select(venue, location) %>% 
                distinct(),
            by = "venue"
        ) %>% 
        mutate(
            match_id =  1:nrow(.),
            # share of scoring shots
            home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
            # share of scoring 
            # home_score_adjusted = home_score / (home_score + away_score),
            hga_app = pmap_int(
                list(season, venue, home_team, away_team), 
                is_home, 
                data_venues = afl_venues_all, 
                data_fixture = afl_fixture_all
            )
        ) %>% 
        select(season, round, match_id, venue, location, home_team, away_team, home_score_adjusted, hga_app) %>% 
        pivot_longer(
            cols = c("home_team", "away_team"), 
            names_to = c("home_away", "temp"), 
            names_sep = "_",
            values_to = "team"
        ) %>% 
        select(season, round, match_id, venue, location, team, home_away, hga_app, home_score_adjusted) %>% 
        mutate(
            score_adjusted = if_else(home_away == "away", 1 - home_score_adjusted, home_score_adjusted),
            score_expected = numeric(nrow(.)),
            hga_app = if_else(home_away == "away", as.integer(hga_app * -1), hga_app),
            start_elo = 1500,
            new_elo = 1500
        ) %>% 
        select(-home_away, -home_score_adjusted)
    
}

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
