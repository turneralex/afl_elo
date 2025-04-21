# matchup expected result

score_expected <- function(elo, elo_opp, hga) {
    x <- -(elo - elo_opp + hga)
    y <- 1 / (1 + 10^(x / 400))
    
    return(y)
}

# update elo rating after matchup result

elo_update <- function(elo, score_adjusted, score_expected, k, regress_app_flag, regress) {
    x <- elo + k * (score_adjusted - score_expected)
    
    y <- (1500 * regress * regress_app_flag) + ((1 - regress * regress_app_flag) * x)
    
    return(y)
}

# check if home ground advantage should apply

is_home <- function(game_venue, game_home_team, game_away_team, game_season, data_venues) {
    
    library(dplyr)
    
    location <- data_venues %>% 
        filter(
            venue == game_venue
        ) %>% 
        pull(location) %>% 
        unique()
    
    away_team_location <- data_venues %>% 
        tidyr::unnest(cols = data_teams) %>% 
        filter(
            team == game_away_team
            & season == game_season
        ) %>% 
        filter(
            total_flag == 1
            & current_flag == 1
        ) %>% 
        pull(location) %>% 
        unique()
    
    home_team_location <- data_venues %>% 
        tidyr::unnest(cols = data_teams) %>% 
        filter(
            team == game_home_team
            & season == game_season
        ) %>% 
        filter(
            total_flag == 1
            & current_flag == 1
        ) %>% 
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
        tolower(team) %in% c(
            "adelaide crows",
            "kuwarna"
        )                                  ~ "Adelaide",
        tolower(team) == "brisbane lions"  ~ "Brisbane",
        tolower(team) == "geelong cats"    ~ "Geelong",
        tolower(team) == "gold coast suns" ~ "Gold Coast",
        tolower(team) %in% c(
            "greater western sydney",
            "gws giants" 
        )                                  ~ "GWS",
        tolower(team) == "sydney swans"    ~ "Sydney",
        tolower(team) %in% c(
            "west coast eagles",
            "waalitj marawar"
        )                                  ~ "West Coast",
        tolower(team) == "footscray"       ~ "Western Bulldogs",
        tolower(team) == "narrm"           ~ "Melbourne",
        tolower(team) == "yartapuulti"     ~ "Port Adelaide",
        tolower(team) == "walyalup"        ~ "Fremantle",
        tolower(team) == "euro-yroke"      ~ "St Kilda",
        T                                  ~ team
    )
    
}

# update venue names

change_venue_name <- function(venue) {
    
    # case_when valid for season >= 2024
    
    dplyr::case_when(
        # VIC
        tolower(venue) == "m.c.g."                             ~ "MCG",
        tolower(venue) %in% c(
            "marvel stadium",
            "docklands"
        )                                                      ~ "Docklands Stadium",
        tolower(venue) == "mars stadium"                       ~ "Eureka Stadium",
        # GEE
        tolower(venue) == "gmhba stadium"                      ~ "Kardinia Park",
        # WA
        tolower(venue) == "optus stadium"                      ~ "Perth Stadium",
        tolower(venue) == "subiaco"                            ~ "Subiaco Oval",
        tolower(venue) == "w.a.c.a."                           ~ "WACA",
        # TAS
        tolower(venue) == "blundstone arena"                   ~ "Bellerive Oval",
        tolower(venue) == "north hobart"                       ~ "North Hobart Oval",
        tolower(venue) %in% c(
            "utas stadium",
            "university of tasmania stadium"
        )                                                      ~ "York Park",
        # NSW
        tolower(venue) == "blacktown"                          ~ "Blacktown International Sportspark",
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
        tolower(venue) == "bruce stadium"                      ~ "Canberra Stadium",
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
        | tolower(venue) == "football park" 
        | tolower(venue) == "barossa park"                      ~ "SA",
        
        tolower(venue) == "gabba" 
        | tolower(venue) == "carrara stadium" 
        | tolower(venue) == "cazaly's stadium" 
        | tolower(venue) == "riverway stadium"                   ~ "QLD",
        
        tolower(venue) == "scg" 
        | tolower(venue) == "sydney showground stadium" 
        | tolower(venue) == "stadium australia" 
        | tolower(venue) == "blacktown international sportspark" ~ "NSW",
        
        tolower(venue) == "bellerive oval" 
        | tolower(venue) == "york park"                          
        | tolower(venue) == "north hobart oval"                  ~ "TAS",
        
        tolower(venue) == "traeger park" 
        | tolower(venue) == "marrara oval"                       ~ "NT",
        
        tolower(venue) == "manuka oval"
        | tolower(venue) == "canberra stadium"                   ~ "ACT",
        
        tolower(venue) == "perth stadium"
        | tolower(venue) == "subiaco oval"                       
        | tolower(venue) == "waca"
        | tolower(venue) == "hands oval"                         ~ "WA",
        
        tolower(venue) == "kardinia park"                        ~ "GEE",
        
        tolower(venue) %in% c(
            "jiangwan stadium",
            "wellington regional stadium"
        )                                                        ~ "Other",
        T                                                        ~ "VIC"
    )  
    
}

# run the elo model for all past & future matches

elo_run <- function(data, k = 0, regress = 0) {
    # make a copy of the data
    data_copy <- data
    
    # get all matches in chronological order
    all_matches <- data %>%
        distinct(match_id) %>%
        pull(match_id)
    
    # start all elo ratings at 1500
    team_elo <- tibble(
        team = unique(data_copy$team), 
        current_elo = 1500
    )
    
    for (match_id_loop in all_matches) {
        # get match-specific data
        match_data <- data %>%
            filter(match_id == match_id_loop)
        
        # get teams
        home_team <- match_data %>% 
            filter(home_team_flag == 1) %>% 
            pull(team)
        away_team <- match_data %>% 
            filter(home_team_flag == 0) %>% 
            pull(team)
        
        # get current team ratings
        home_elo <- team_elo %>% 
            filter(team == home_team) %>% 
            pull(current_elo)
        away_elo <- team_elo %>% 
            filter(team == away_team) %>% 
            pull(current_elo)
        
        # update start_elo in the data_copy
        data_copy <- data_copy %>%
            mutate(
                start_elo = case_when(
                    match_id == match_id_loop & team == home_team ~ home_elo,
                    match_id == match_id_loop & team == away_team ~ away_elo,
                    T                                             ~ start_elo
                )
            )
        
        # get HGA values
        home_hga <- match_data %>% 
            filter(home_team_flag == 1) %>% 
            pull(hga)
        away_hga <- match_data %>% 
            filter(home_team_flag == 0) %>% 
            pull(hga)
        
        # calculate expected scores
        home_score_expected <- score_expected(
            elo = home_elo, 
            elo_opp = away_elo, 
            hga = home_hga
        )
        away_score_expected <- score_expected(
            elo = away_elo, 
            elo_opp = home_elo, 
            hga = away_hga
        )
        
        # update expected scores in data_copy
        data_copy <- data_copy %>%
            mutate(
                score_expected = case_when(
                    match_id == match_id_loop & team == home_team ~ home_score_expected,
                    match_id == match_id_loop & team == away_team ~ away_score_expected,
                    T                                             ~ score_expected
                )
            )
        
        # determine score_adjusted
        is_historical <- !is.na(match_data$score_adjusted[1])
        
        if (is_historical) {
            # for historical matches, use actual results
            home_score_adjusted <- match_data %>% 
                filter(home_team_flag == 1) %>% 
                pull(score_adjusted)
            away_score_adjusted <- match_data %>% 
                filter(home_team_flag == 0) %>% 
                pull(score_adjusted)
        } else {
            # for future matches, use expected_score
            home_score_adjusted <- home_score_expected
            away_score_adjusted <- away_score_expected
            
            # update score_adjusted in data_copy
            data_copy <- data_copy %>%
                mutate(
                    score_adjusted = case_when(
                        match_id == match_id_loop & team == home_team ~ home_score_adjusted,
                        match_id == match_id_loop & team == away_team ~ away_score_adjusted,
                        T                                             ~ score_adjusted
                    )
                )
        }
        
        # get regress_flag_app for each team
        home_regress_app_flag <- match_data %>% 
            filter(home_team_flag == 1) %>% 
            pull(regress_app_flag)
        away_regress_app_flag <- match_data %>% 
            filter(home_team_flag == 0) %>% 
            pull(regress_app_flag)
        
        # get new team ratings
        new_home_elo <- elo_update(
            elo = home_elo, 
            score_adjusted = home_score_adjusted, 
            score_expected = home_score_expected, 
            k, 
            regress_app_flag = home_regress_app_flag, 
            regress = regress
        )
        new_away_elo <- elo_update(
            elo = away_elo, 
            score_adjusted = away_score_adjusted, 
            score_expected = away_score_expected, 
            k, 
            regress_app_flag = away_regress_app_flag, 
            regress = regress
        )
        
        # update new_elo in data_copy
        data_copy <- data_copy %>%
            mutate(
                new_elo = case_when(
                    match_id == match_id_loop & team == home_team ~ new_home_elo,
                    match_id == match_id_loop & team == away_team ~ new_away_elo,
                    T                                             ~ new_elo
                )
            )
        
        # update current_elo in team_elo (for next match)
        team_elo <- team_elo %>%
            mutate(
                current_elo = case_when(
                    team == home_team ~ new_home_elo,
                    team == away_team ~ new_away_elo,
                    T                 ~ current_elo
                )
            )
    }
    
    return(data_copy)
    
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
