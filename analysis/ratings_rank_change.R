library(dplyr)

# current elo position & change

afl_elo_rank_change <- afl_elo %>% 
    filter(
        !is.na(start_elo)
        & !is.na(new_elo)
    ) %>%
    group_by(
        season,
        team
    ) %>% 
    mutate(
        last_elo_prev_season = if_else(
            match_id == max(match_id)
            & season == prev_season,
            new_elo,
            0
        )
    ) %>% 
    group_by(team) %>% 
    mutate(
        last_elo_prev_season = max(last_elo_prev_season),
        elo_prev_5_games = lag(
            new_elo,
            n = 5,
            default = 1500
        )
    ) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    select(
        season,
        round, 
        team, 
        start_elo, 
        new_elo,
        last_elo_prev_season,
        elo_prev_5_games
    ) %>% 
    mutate(
        plus_minus_avg = new_elo - 1500,
        plus_minus_prev_week = new_elo - start_elo,
        plus_minus_prev_season = new_elo - last_elo_prev_season,
        plus_minus_prev_5_games = new_elo - elo_prev_5_games
    ) %>% 
    arrange(-plus_minus_prev_week) %>% 
    mutate(elo_position_prev = row_number()) %>% 
    arrange(-plus_minus_avg) %>% 
    mutate(
        elo_position_avg = row_number(),
        elo_position_team_avg = paste0(
            elo_position_avg,
            ": ",
            team
        )
    ) 
