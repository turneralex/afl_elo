is_home <- function(season_year, game_venue, game_away_team, data) {
    library(tidyverse)
    
    location <- data %>% 
        filter(year == season_year & venue == game_venue) %>% 
        pull(location) 
    
    away_team_location <- data %>% 
        unnest() %>% 
        filter(year == season_year & team == game_away_team) %>% 
        pull(location) %>% 
        unique()
    
    as.integer(!(location %in% away_team_location))
}

is_home("2018", "Cazaly's Stadium", "Western Bulldogs", afl_venues_all)

get_round_scores <- function(season, round) {
    library(tidyverse)
    library(rvest)
    
    fixture_url <- paste0("https://en.wikipedia.org/wiki/", season, "_AFL_season")
    round <- round + 2
    
    fixture_url %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", round, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) %>% 
        select(X2, X4) %>% 
        transmute(home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
                  away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
                  home_goals = X2 %>% str_remove_all("[:alpha:]* |\\..*") %>% as.integer(),
                  away_goals = X4 %>% str_remove_all("[:alpha:]* |\\..*") %>% as.integer(),
                  home_behinds = home_score - (home_goals * 6),
                  away_behinds = away_score - (away_goals * 6))
}

get_round_scores(2019, 1)    
