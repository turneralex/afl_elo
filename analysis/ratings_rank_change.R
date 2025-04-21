library(dplyr)

# current elo position & change

afl_elo_rank_change <- afl_elo %>% 
    filter(
        team != "Fitzroy"
        & round_number <= rounds_so_far
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
        season = current_season,
        last_elo_prev_season = max(last_elo_prev_season),
        elo_prev_5_games = lag(
            new_elo,
            n = 5,
            default = 1500
        ),
        elo_prev_4_games = lag(
            new_elo,
            n = 4,
            default = 1500
        ),
        elo_prev_3_games = lag(
            new_elo,
            n = 3,
            default = 1500
        ),
        elo_prev_2_games = lag(
            new_elo,
            n = 2,
            default = 1500
        ),
        elo_prev_1_games = lag(
            new_elo,
            n = 1,
            default = 1500
        )
    ) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    select(
        season,
        round, 
        round_number,
        team, 
        start_elo, 
        new_elo,
        last_elo_prev_season,
        elo_prev_5_games,
        elo_prev_4_games,
        elo_prev_3_games,
        elo_prev_2_games,
        elo_prev_1_games
    ) %>% 
    mutate(
        plus_minus_avg = new_elo - 1500,
        score_expected = score_expected(elo = new_elo, elo_opp = 1500, hga = 0),
        plus_minus_prev_week = new_elo - start_elo,
        plus_minus_prev_season = new_elo - last_elo_prev_season,
        plus_minus_prev_5_games = new_elo - (
            (
                elo_prev_5_games
                + (elo_prev_4_games * 2)
                + (elo_prev_3_games * 3)
                + (elo_prev_2_games * 4)
                + (elo_prev_1_games  * 5)
            ) / 15
        ) 
    ) %>% 
    modelr::add_predictions(
        model = afl_margin_model,
        var = "pred_margin",
        type = "response"
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

afl_elo_rank_change %>% 
    mutate(round = rounds_so_far) %>% 
    select(
        season,
        round,
        team,
        score = new_elo
    ) %>% 
    print()
