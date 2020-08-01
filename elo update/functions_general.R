score_expected <- function(elo, elo_opp, hga_app = 1, hga = 0) {
    x <- -(elo - elo_opp + (hga_app * hga))
    1 / (1 + 10^(x / 400))
}

elo_update <- function(elo, elo_opp, score_adjusted, score_expected, k = 70, regress = 0.5, regress_app = 0) {
    x <- elo + k * (score_adjusted - score_expected)
    y <- (1500 * regress * regress_app) + ((1 - regress * regress_app) * x)
    y
}

is_home <- function(season_year, game_venue, game_away_team, data) {
    library(tidyverse)
    
    location <- data %>% 
        filter(year == season_year & venue == game_venue) %>% 
        pull(location) 
    
    away_team_location <- data %>% 
        unnest(cols = teams) %>% 
        filter(year == season_year & team == game_away_team) %>% 
        pull(location) %>% 
        unique()
    
    as.integer(!(location %in% away_team_location))
}

convert_elo_df <- function(fixture_df) {
    library(tidyverse)
    
    fixture_df %>% 
        mutate(match_id =  1:nrow(.),
               home_score_adjusted = home_score / (home_score + away_score),
               hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all)) %>% 
        select(season, round, match_id, home_team, away_team, home_score_adjusted, hga_app) %>% 
        pivot_longer(
            cols = c("home_team", "away_team"), 
            names_to = c("home_away", "temp"), 
            names_sep = "_",
            values_to = "team"
        ) %>% 
        select(season, round, match_id, team, home_away, hga_app, home_score_adjusted) %>% 
        mutate(
            score_adjusted = if_else(home_away == "away", 1 - home_score_adjusted, home_score_adjusted),
            score_expected = numeric(nrow(.)),
            hga_app = if_else(home_away == "away", as.integer(hga_app * -1), hga_app),
            start_elo = 1500,
            new_elo = 1500
        ) %>% 
        select(-home_away, -home_score_adjusted)
    
}

elo_run <- function(elo_df, k, hga, regress) {
    
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
        
        score_expected_1 = score_expected(game[1, "start_elo"], game[2, "start_elo"], hga_app = game[1, "hga_app"], hga = hga)
        
        score_expected_2 = score_expected(game[2, "start_elo"], game[1, "start_elo"], hga_app = game[2, "hga_app"], hga = hga)
        
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

# get_round_scores <- function(season, round) {
#     library(tidyverse)
#     library(rvest)
#     
#     fixture_url <- paste0("https://en.wikipedia.org/wiki/", season, "_AFL_season")
#     round <- round + 2
#     
#     fixture_url %>% 
#         read_html() %>% 
#         html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", round, "]")) %>% 
#         html_table() %>% 
#         magrittr::extract2(1) %>% 
#         as_tibble() %>% 
#         filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) %>% 
#         select(X2, X4) %>% 
#         transmute(home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
#                   away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
#                   home_goals = X2 %>% str_remove_all("[:alpha:]* |\\..*") %>% as.integer(),
#                   away_goals = X4 %>% str_remove_all("[:alpha:]* |\\..*") %>% as.integer(),
#                   home_behinds = home_score - (home_goals * 6),
#                   away_behinds = away_score - (away_goals * 6))
# }
