library(dplyr)

# get ladder

afl_ladder <- fitzRoy::fetch_ladder_afl(
    season = current_season, 
    round_number = rounds_so_far
)

# update column names 

afl_ladder <- afl_ladder %>% 
    rename_with(
        .fn = ~ .x %>% 
            tolower() %>% 
            stringr::str_replace_all(
                pattern = "[.]", 
                replacement = "_"
            )
    )

# update team names 

afl_ladder <- afl_ladder %>% 
    transmute(
        team = change_team_name(team_name),
        ladder_position = case_when(
            position == 1 ~ paste0(position, "st"),
            position == 2 ~ paste0(position, "nd"),
            position == 3 ~ paste0(position, "rd"),
            T             ~ paste0(position, "th")
        )
    ) 

# elo rankings

afl_elo_ladder <- afl_elo %>% 
    filter(
        !is.na(new_elo)
    ) %>% 
    select(
        team,
        new_elo
    ) %>% 
    group_by(team) %>% 
    slice(n()) %>%
    ungroup() %>% 
    mutate(
        elo_rank = case_when(
            rank(-new_elo) == 1 ~ paste0(rank(-new_elo), "st"),
            rank(-new_elo) == 2 ~ paste0(rank(-new_elo), "nd"),
            rank(-new_elo) == 3 ~ paste0(rank(-new_elo), "rd"),
            T                   ~ paste0(rank(-new_elo), "th")
        )
    ) %>% 
    arrange(
        desc(new_elo)
    )

# generate elo predictions for next round

afl_elo_pred_base <- afl_elo %>% 
    filter(
        season == current_season
        & !is.na(score_expected)
    ) %>% 
    inner_join(
        afl_ladder,
        by = "team"
    ) %>% 
    inner_join(
        afl_elo_ladder,
        by = "team"
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
        correct_tip = if_else(
            (score_expected >= 0.5 & margin > 0)
            | (score_expected < 0.5 & margin < 0),
            1,
            0
        ),
        pred_win_prob = if_else(
            score_expected >= 0.5
            & pred_win_prob < 0.5,
            0.5,
            pred_win_prob
        )
    ) %>% 
    group_by(match_id) %>%
    mutate(
        away_team = lead(team, n = 1),
        away_elo = lead(start_elo, n = 1),
        away_ladder_position = lead(ladder_position, n = 1),
        away_elo_rank = lead(elo_rank, n = 1),
        away_pred_win_prob = lead(pred_win_prob, n = 1),
        away_margin = lead(margin, n = 1),
        away_pred_margin = lead(pred_margin, n = 1)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    select(
        season,
        round,
        round_number,
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
        home_ladder_position = ladder_position,
        away_ladder_position,
        home_pred_win_prob = pred_win_prob,
        away_pred_win_prob,
        home_margin = margin,
        away_margin,
        home_pred_margin = pred_margin,
        away_pred_margin,
        correct_tip
    ) %>% 
    mutate(
        game_id = row_number(),
        hga = case_when(
            location == "VIC" ~ hga_app * elo_par["hga_vic"],
            location == "NSW" ~ hga_app * elo_par["hga_nsw"],
            location == "QLD" ~ hga_app * elo_par["hga_qld"],
            location == "SA"  ~ hga_app * elo_par["hga_sa"],
            location == "WA"  ~ hga_app * elo_par["hga_wa"],
            location == "GEE" ~ hga_app * elo_par["hga_gee"],
            location == "TAS" ~ hga_app * elo_par["hga_tas"],
            location == "ACT" ~ hga_app * elo_par["hga_act"],
            T                 ~ 0
        ),
        elo_diff_hga = home_elo - away_elo + hga,
        pred_winner = if_else(
            elo_diff_hga > 0,
            home_team,
            away_team
        ),
        pred_winner_win_prob = if_else(
            elo_diff_hga > 0,
            home_pred_win_prob,
            away_pred_win_prob
        ),
        pred_winner_margin = if_else(
            elo_diff_hga > 0,
            home_pred_margin,
            away_pred_margin
        )
    ) 

afl_elo_pred <- afl_elo_pred_base %>% 
    filter(
        round_number == rounds_so_far + 1
    ) %>% 
    select(
        season,
        round,
        home_team, 
        away_team, 
        venue,
        home_ladder_position,
        away_ladder_position,
        home_elo,
        away_elo,
        home_elo_rank,
        away_elo_rank,
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
