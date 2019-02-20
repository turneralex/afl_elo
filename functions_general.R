is_home <- function(data, season_year, game_venue, game_away_team) {
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

is_home(afl_venues_all, "2018", "MCG", "Greater Western Sydney")
