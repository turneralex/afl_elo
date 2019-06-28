library(tidyverse)

teams <- afl_fixture_all %>% 
    select(home_team) %>% 
    pull() %>% 
    unique() %>% 
    sort()

afl_elo_all <- elo_model$elos %>% 
    as_tibble(.name_repair = "unique") %>% 
    select(3, 6, 7) 

col_names <- c("home_score_elo", "new_home_elo", "new_away_elo")

colnames(afl_elo_all) <- col_names

afl_elo_all <- afl_elo %>% 
    bind_cols(afl_elo_all) %>% 
    select(1:9, 11, 10, 12:14)

afl_elo_all

afl_elo_all %>% 
    filter(season == "2019") %>% 
    ggplot(aes(home_score_adjusted, home_score_elo)) +
    geom_point() 

afl_elo_all %>% 
    filter((home_team == "Fremantle" | away_team == "Fremantle" ) & season == "2018") %>% 
    select(season, match_id, home_team, away_team, new_home_elo, new_away_elo) %>% 
    unite("season_match_id", c("season", "match_id"), sep = "", remove = F) %>% 
    mutate(
        season_match_id = season_match_id %>% as.integer(),
        match_id = 1:nrow(.),
        elo_rating = if_else(
            home_team == "Fremantle", new_home_elo, new_away_elo
        )
    ) %>% 
    select(-new_home_elo, -new_away_elo) %>% 
    ggplot(aes(match_id, elo_rating)) +
    geom_line()
    
afl_elo_all %>% 
    filter(home_team == "Carlton" | away_team == "Carlton") %>% 
    select(season, match_id, home_team, away_team, new_home_elo, new_away_elo) %>% 
    unite("season_match_id", c("season", "match_id"), sep = "", remove = F) %>% 
    mutate(
        season_match_id = season_match_id %>% as.integer(),
        match_id = 1:nrow(.),
        elo_rating = if_else(
            home_team == "Carlton", new_home_elo, new_away_elo
        )
    ) %>% 
    select(-new_home_elo, -new_away_elo) %>% 
    ggplot(aes(match_id, elo_rating, colour = season)) +
    geom_line() +
    geom_point() +
    labs(title = .x)

teams %>% 
    map(
        ~ afl_elo_all %>% 
            filter(home_team == .x | away_team == .x) %>% 
            select(season, match_id, home_team, away_team, new_home_elo, new_away_elo) %>% 
            unite("season_match_id", c("season", "match_id"), sep = "", remove = F) %>% 
            mutate(
                season_match_id = season_match_id %>% as.integer(),
                match_id = 1:nrow(.),
                elo_rating = if_else(
                    home_team == .x, new_home_elo, new_away_elo
                )
            ) %>% 
            select(-new_home_elo, -new_away_elo) %>% 
            ggplot(aes(match_id, elo_rating, colour = season)) +
            geom_line() +
            geom_point() +
            scale_y_continuous(limits = c(1400, 1600)) +
            labs(title = .x)
    )

afl_elo_all$new_home_elo %>% max()
afl_elo_all$new_home_elo %>% min()
afl_elo_all$new_away_elo %>% max()
afl_elo_all$new_away_elo %>% min()
