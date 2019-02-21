library(tidyverse)
library(rvest)

is_home <- function(season_year, game_venue, game_away_team, data) {
    location <- data %>% 
        filter(year == season_year & venue == game_venue) %>% 
        pull(location) %>% 
        as.character()
    
    away_team_location <- data %>% 
        unnest() %>% 
        filter(year == year & team == game_away_team) %>% 
        pull(location) %>% 
        as.character() %>% 
        unique()
    
    as.integer(!(location %in% away_team_location))
}

is_home("2018", "MCG", "Greater Western Sydney", afl_venues_all)

fixture_url_2019 <- "https://en.wikipedia.org/wiki/2019_AFL_season"

round_1 <- 3

fixture_url_2018 %>% 
    read_html() %>% 
    html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", "3", "]")) %>% 
    html_table() %>% 
    magrittr::extract2(1) %>% 
    as_tibble() %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) %>% 
    select(X2, X4) %>% 
    transmute(home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
              away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer())

get_round_scores <- function(fixture_url, table_id) {
    fixture_url_2018 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", "3", "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) %>% 
        select(X2, X4) %>% 
        transmute(home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
                  away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer())
}

get_round_scores(fixture_url_2019, )    
