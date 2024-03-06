# get tips for round 1 when no results are available for that season

afl_elo_pred <- afl_elo %>% 
    filter(
        season == current_season
        & round == paste("Round", rounds_so_far)
    ) %>% 
    mutate(
        elo_rank = case_when(
            rank(-start_elo) == 1 ~ paste0(rank(-start_elo), "st"),
            rank(-start_elo) == 2 ~ paste0(rank(-start_elo), "nd"),
            rank(-start_elo) == 3 ~ paste0(rank(-start_elo), "rd"),
            T ~                     paste0(rank(-start_elo), "th")
        )
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
        away_elo_rank = lead(elo_rank, n = 1),
        away_pred_win_prob = lead(pred_win_prob, n = 1),
        away_pred_margin = lead(pred_margin, n = 1)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    select(
        round,
        location, 
        home_team = team, 
        away_team, 
        venue, 
        hga_app, 
        home_elo = start_elo, 
        away_elo, 
        score_expected,
        home_elo_rank = elo_rank,
        away_elo_rank,
        pred_win_prob,
        away_pred_win_prob,
        pred_margin,
        away_pred_margin
    ) %>% 
    mutate(
        season = current_season,
        game_id = row_number(),
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
        pred_winner_win_prob = round(
            if_else(
                elo_diff_hga > 0,
                pred_win_prob,
                away_pred_win_prob
            ),
            2
        ),
        pred_winner_margin = round(
            if_else(
                elo_diff_hga > 0,
                pred_margin,
                away_pred_margin
            )
        ),
        matchup = paste0(
            "Predicted winner: ", 
            pred_winner, 
            " by ", 
            round(pred_winner_margin), 
            ", with a ", 
            round(pred_winner_win_prob * 100), 
            "% chance of victory"
        )
    ) %>% 
    select(
        season,
        round,
        home_team, 
        away_team, 
        venue, 
        matchup,
        pred_winner,
        pred_winner_win_prob,
        pred_winner_margin
    )

# provide tips to squiggle

afl_elo_pred %>%
    select(
        season,
        round,
        home_team,
        away_team,
        pred_winner,
        pred_winner_win_prob,
        pred_winner_margin
    ) %>%
    upload_tips_squiggle()
