# get tips for round 1 when no results are available for that season

afl_elo_pred <- afl_elo %>% 
    filter(
        season == current_season
        & round_number == rounds_so_far + 1
    ) %>% 
    modelr::add_predictions(
        model = afl_win_prob_model,
        var = "pred_win_prob",
        type = "response"
    ) %>% 
    modelr::add_predictions(
        model = afl_margin_model,
        var = "pred_margin",
        type = "response"
    ) %>% 
    mutate(
        pred_margin = if_else(
            pred_win_prob > 0.5 
            & pred_margin < 0,
            1,
            pred_margin
        )
    ) %>% 
    group_by(match_id) %>%
    mutate(
        away_team = lead(team, n = 1),
        away_elo = lead(start_elo, n = 1),
        away_pred_win_prob = lead(pred_win_prob, n = 1),
        away_pred_margin = lead(pred_margin, n = 1)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    rename(
        home_team = team,
        home_elo = start_elo
    ) %>% 
    mutate(
        hga = case_when(
            location == "VIC" ~ hga_app * elo_par["hga_vic"],
            location == "NSW" ~ hga_app * elo_par["hga_nsw"],
            location == "QLD" ~ hga_app * elo_par["hga_qld"],
            location == "SA"  ~ hga_app * elo_par["hga_sa"],
            location == "WA"  ~ hga_app * elo_par["hga_wa"],
            location == "GEE" ~ hga_app * elo_par["hga_gee"],
            location == "TAS" ~ hga_app * elo_par["hga_tas"],
            T                 ~ hga_app * elo_par["hga_other"]
        ),
        elo_diff_hga = home_elo - away_elo + hga,
        pred_winner = if_else(
            elo_diff_hga > 0,
            home_team,
            away_team
        ),
        pred_winner_win_prob = if_else(
            elo_diff_hga > 0,
            pred_win_prob,
            away_pred_win_prob
        ),
        pred_winner_margin = if_else(
            elo_diff_hga > 0,
            pred_margin,
            away_pred_margin
        )
    ) %>% 
    select(
        season,
        round,
        home_team,
        away_team,
        pred_winner,
        pred_winner_win_prob,
        pred_winner_margin
    )

afl_elo_pred %>%
    mutate(
        pred_winner_margin = pred_winner_margin %>% 
            round(),
        pred_winner_win_prob = pred_winner_win_prob %>% 
            round(2)
    ) %>% 
    select(
        season,
        round,
        home_team,
        away_team,
        pred_winner,
        pred_winner_margin,
        pred_winner_win_prob
    ) %>% 
    print()

