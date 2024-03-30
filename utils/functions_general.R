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

is_home <- function(game_venue, game_home_team, game_away_team, data_venues, data_fixture) {
    
    library(dplyr)
    
    location <- data_venues %>% 
        filter(
            venue == game_venue
        ) %>% 
        pull(location) 
    
    away_team_location <- data_venues %>% 
        tidyr::unnest(cols = teams) %>% 
        filter(
            team == game_away_team
        ) %>% 
        inner_join(
            data_fixture %>% 
                filter(
                    home_team == game_away_team
                ) %>% 
                group_by(venue) %>% 
                count(),
            by = "venue"
        ) %>% 
        filter(n >= 10) %>% 
        pull(location) %>% 
        unique()
    
    home_team_location <- data_venues %>% 
        tidyr::unnest(cols = teams) %>% 
        filter(
            team == game_home_team
        ) %>% 
        inner_join(
            data_fixture %>% 
                filter(
                    home_team == game_home_team
                ) %>% 
                group_by(venue) %>% 
                count(),
            by = "venue"
        ) %>% 
        filter(n >= 10) %>% 
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
        tolower(team) == "adelaide crows"    ~ "Adelaide",
        tolower(team) == "brisbane lions"    ~ "Brisbane",
        tolower(team) == "geelong cats"      ~ "Geelong",
        tolower(team) == "gold coast suns"   ~ "Gold Coast",
        tolower(team) %in% c(
            "greater western sydney",
            "gws giants" 
        )                                    ~ "GWS",
        tolower(team) == "sydney swans"      ~ "Sydney",
        tolower(team) == "west coast eagles" ~ "West Coast",
        tolower(team) == "footscray"         ~ "Western Bulldogs",
        T                                    ~ team
    )
    
}

# update venue names

change_venue_name <- function(venue) {
    
    # case_when valid for season >= 2024
    
    dplyr::case_when(
        # VIC
        tolower(venue) == "m.c.g."                            ~ "MCG",
        tolower(venue) %in% c(
            "marvel stadium",
            "docklands"
        )                                                     ~ "Docklands Stadium",
        tolower(venue) == "mars stadium"                      ~ "Eureka Stadium",
        # GEE
        tolower(venue) == "gmhba stadium"                     ~ "Kardinia Park",
        # WA
        tolower(venue) == "optus stadium"                    ~ "Perth Stadium",
        tolower(venue) == "subiaco"                          ~ "Subiaco Oval",
        # TAS
        tolower(venue) == "blundstone arena"                  ~ "Bellerive Oval",
        tolower(venue) %in% c(
            "utas stadium",
            "university of tasmania stadium"
        )                                                     ~ "York Park",
        # NSW
        tolower(venue) == "blacktown"                         ~ "Blacktown International Sportspark",
        tolower(venue) %in% c(
            "sydney showground",
            "giants stadium",
            "engie stadium"
        )                                                      ~ "Sydney Showground Stadium",
        tolower(venue) == "s.c.g."                             ~ "SCG",
        # NT
        tolower(venue) == "tio stadium"                        ~ "Marrara Oval",
        tolower(venue) == "tio traeger park"                   ~ "Traeger Park",
        # ACT
        tolower(venue) == "unsw canberra oval"                 ~ "Manuka Oval",
        # QLD
        tolower(venue) %in% c(
            "carrara",
            "heritage bank stadium",
            "people first stadium"
        )                                                      ~ "Carrara Stadium",
        # other 
        tolower(venue) == "adelaide arena at jiangwan stadium" ~ "Jiangwan Stadium",
        tolower(venue) == "wellington"                         ~ "Wellington Regional Stadium",
        T                                                      ~ venue
    ) 
    
}

# map venues to locations

venue_location <- function(venue) {
    
    # case_when valid for season >= 2023
    
    dplyr::case_when(
        tolower(venue) == "adelaide oval" 
        | tolower(venue) == "adelaide hills" 
        | tolower(venue) == "norwood oval"
        | tolower(venue) == "football park"                      ~ "SA",
        
        tolower(venue) == "gabba" 
        | tolower(venue) == "carrara stadium" 
        | tolower(venue) == "cazaly's stadium" 
        | tolower(venue) == "riverway stadium"                   ~ "QLD",
        
        tolower(venue) == "scg" 
        | tolower(venue) == "sydney showground stadium" 
        | tolower(venue) == "stadium australia" 
        | tolower(venue) == "blacktown international sportspark" ~ "NSW",
        
        tolower(venue) == "bellerive oval" 
        | tolower(venue) == "york park"                          ~ "TAS",
        
        tolower(venue) == "traeger park" 
        | tolower(venue) == "marrara oval"                       ~ "NT",
        
        tolower(venue) == "manuka oval"                          ~ "ACT",
        
        tolower(venue) == "perth stadium"
        | tolower(venue) == "subiaco oval"                       ~ "WA",
        
        tolower(venue) == "kardinia park"                        ~ "GEE",
        
        tolower(venue) %in% c(
            "jiangwan stadium",
            "wellington regional stadium"
        )                                                        ~ "Other",
        T                                                        ~ "VIC"
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
            .[, season := lead(season, n = 1, default = current_season), by = team] %>% 
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
            hga <- 0
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

upload_tips_squiggle <- function(pred_df, drive_id) {
    
    googledrive::drive_auth(email = T)
    
    googlesheets4::gs4_auth(email = T)
    
    googlesheets4::sheet_write(
        data = pred_df,
        ss = drive_id, 
        sheet = "predictions"
    )
    
}
