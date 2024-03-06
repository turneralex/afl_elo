# matchup expected result

score_expected <- function(elo, elo_opp, hga_app, hga) {
    x <- -(elo - elo_opp + (hga_app * hga))
    1 / (1 + 10^(x / 400))
}

# update elo rating after game result

elo_update <- function(elo, elo_opp, score_adjusted, score_expected, k, regress, regress_app) {
    x <- elo + k * (score_adjusted - score_expected)
    y <- (1500 * regress * regress_app) + ((1 - regress * regress_app) * x)
    y
}

# check if home ground advantage should apply

is_home <- function(season_year, game_venue, game_home_team, game_away_team, data_venues, data_fixture) {
    
    library(dplyr)
    
    location <- data_venues %>% 
        filter(
            year == season_year 
            & venue == game_venue
        ) %>% 
        pull(location) 
    
    away_team_location <- data_venues %>% 
        tidyr::unnest(cols = teams) %>% 
        filter(
            year == season_year 
            & team == game_away_team
        ) %>% 
        inner_join(
            data_fixture %>% 
                filter(
                    season == season_year 
                    & home_team == game_away_team
                ) %>% 
                group_by(venue) %>% 
                count(),
            by = "venue"
        ) %>% 
        filter(n > 1) %>% 
        pull(location) %>% 
        unique()
    
    home_team_location <- data_venues %>% 
        tidyr::unnest(cols = teams) %>% 
        filter(
            year == season_year 
            & team == game_home_team
        ) %>% 
        inner_join(
            data_fixture %>% 
                filter(
                    season == season_year 
                    & home_team == game_home_team
                ) %>% 
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

# make team names consistent

change_team_name <- function(team) {
    
    dplyr::case_when(
        team == "Adelaide Crows"      ~ "Adelaide",
        team == "Brisbane Lions"      ~ "Brisbane",
        team == "Geelong Cats"        ~ "Geelong",
        team == "Gold Coast Suns"     ~ "Gold Coast",
        team %in% c(
            "Greater Western Sydney",
            "GWS Giants" 
        )                             ~ "GWS",
        team == "Sydney Swans"        ~ "Sydney",
        team == "West Coast Eagles"   ~ "West Coast",
        team == "Footscray"           ~ "Western Bulldogs",
        T                             ~ team
    )
    
}

# update venue names

change_venue_name <- function(venue) {
    
    # case_when valid for season >= 2024
    
    dplyr::case_when(
        # VIC
        venue == "M.C.G."                         ~ "MCG",
        venue %in% c(
            "Marvel Stadium",
            "Docklands"
        )                                         ~ "Docklands Stadium",
        venue == "Mars Stadium"                   ~ "Eureka Stadium",
        # GEE
        venue == "GMHBA Stadium"                  ~ "Kardinia Park",
        # WA
        venue == "Optus Stadium"                  ~ "Perth Stadium",
        # TAS
        venue == "Blundstone Arena"               ~ "Bellerive Oval",
        venue == "University of Tasmania Stadium" ~ "York Park",
        # NSW
        venue %in% c(
            "Sydney Showground",
            "GIANTS Stadium"
        )                                         ~ "Sydney Showground Stadium",
        venue == "S.C.G."                         ~ "SCG",
        # NT
        venue == "TIO Stadium"                    ~ "Marrara Oval",
        venue == "TIO Traeger Park"               ~ "Traeger Park",
        # QLD
        venue %in% c(
            "Carrara",
            "Heritage Bank Stadium"
        )                                         ~ "Carrara Stadium",
        T                                         ~ venue
    ) 
    
}

# map venues to locations

venue_location <- function(venue) {
    
    # case_when valid for season >= 2023
    
    dplyr::case_when(
        venue == "Adelaide Oval" | venue == "Adelaide Hills" | venue == "Norwood Oval"       ~ "SA",
        venue == "Gabba" | venue == "Carrara Stadium" | venue == "Cazaly's Stadium"          ~ "QLD",
        venue == "Manuka Oval"                                                               ~ "ACT",
        venue == "Perth Stadium"                                                             ~ "WA",
        venue == "SCG" | venue == "Sydney Showground Stadium" | venue == "Stadium Australia" ~ "NSW",
        venue == "Bellerive Oval" | venue == "York Park"                                     ~ "TAS",
        venue == "Kardinia Park"                                                             ~ "GEE",
        venue == "Jiangwan Stadium"                                                          ~ "Other",
        venue == "Traeger Park" | venue == "Marrara Oval"                                    ~ "NT",
        T                                                                                    ~ "VIC"
    )  
    
}

# run the elo model

elo_run <- function(elo_df, k, hga_vec, regress) {
    
    library(magrittr)
    
    all_games <- unique(elo_df$match_id)
    
    for (i in 1:(length(all_games))) {
        
        game <- elo_df[elo_df$match_id == i, ] 
        
        current_season <- game$season[1]
        
        row_id <- which(elo_df$match_id == i)[1]
        
        next_season <- data.table::as.data.table(elo_df) %>% 
            .[row_id:nrow(.)] %>% 
            .[, season := lead(season, n = 1, default = "9999"), by = team] %>% 
            .[team == game[1, "team"]] %>% 
            .$season %>% 
            .[1]
        
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
        
        score_expected_1 <- score_expected(
            game[1, "start_elo"], 
            game[2, "start_elo"], 
            hga_app = game[1, "hga_app"],
            hga = hga
        ) %>% 
            unname()
        
        score_expected_2 <- score_expected(
            game[2, "start_elo"], 
            game[1, "start_elo"], 
            hga_app = game[2, "hga_app"], 
            hga = hga
        ) %>% 
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
        
        elo_df <- data.table::as.data.table(elo_df) %>%
            .[, start_elo := lag(new_elo, n = 1, default = 1500), by = team]
        
    }
    
    dplyr::tibble(elo_df)
    
}

# upload tips for squiggle

upload_tips_squiggle <- function(pred_df) {
    
    googledrive::drive_auth(email = T)
    
    googlesheets4::gs4_auth(email = T)
    
    drive_id <- "1Aqkhb5uV-qU7b1Zm2k2KA6M05rpNDEFps1MwPaU3QaM"
    
    googlesheets4::sheet_write(
        data = pred_df,
        ss = drive_id, 
        sheet = "predictions"
    )
    
}
