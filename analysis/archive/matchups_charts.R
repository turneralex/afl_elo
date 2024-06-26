library(dplyr)
library(ggplot2)

# scale variables to be on between 0 and 1

team_stats <- team_stats_base_adj %>% 
    mutate(
        across(
            c(
                score_shots_mean,
                i50_mean,
                turn_mean,
                cp_diff_mean,
                clear_diff_mean,
                
                score_shots_opp_mean,
                i50_opp_mean,
                turn_opp_mean,
                int_mean,
                tackles_mean,
                
                khb_ratio_mean,
                up_rate_mean,
                marks_mean,
                mg_disp_mean
            ),
            scales::rescale
        )
    )

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
    mutate(
        team = change_team_name(team_name)
    ) %>% 
    select(
        team, 
        ladder_position = position
    ) 

# check ladder

afl_ladder %>% 
    print()

# generate elo predictions for next round

afl_elo_pred_base <- afl_elo %>% 
    filter(
        season == current_season
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
            (score_expected >= 0.5 & margin >= 0)
            | (score_expected <= 0.5 & margin <= 0),
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
        away_pred_win_prob = lead(pred_win_prob, n = 1),
        away_margin = lead(margin, n = 1),
        away_pred_margin = lead(pred_margin, n = 1)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    select(
        season,
        round,
        location, 
        home_team = team, 
        away_team, 
        venue, 
        hga_app, 
        home_elo = start_elo, 
        away_elo, 
        score_expected,
        correct_tip,
        pred_win_prob,
        away_pred_win_prob,
        margin,
        away_margin,
        pred_margin,
        away_pred_margin
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
        margin = if_else(
            elo_diff_hga > 0,
            margin,
            away_margin
        ),
        pred_winner_margin = round(
            if_else(
                elo_diff_hga > 0,
                pred_margin,
                away_pred_margin
            )
        )
    ) 
    
afl_elo_pred <- afl_elo_pred_base %>% 
    filter(
        round == paste("Round", rounds_so_far + 1)
    ) %>% 
    mutate(
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

# create charts 

# base table

matchups_charts_base <- team_stats %>% 
    inner_join(
        afl_elo_pred %>% 
            select(
                home_team,
                away_team
            ) %>% 
            mutate(
                game_id = row_number()
            ) %>% 
            tidyr::pivot_longer(
                -game_id,
                names_to = "home_away",
                values_to = "team"
            ) %>% 
            select(
                game_id,
                team
            ) %>% 
            mutate(
                team_home = rep(
                    c(0, 1),
                    times = max(game_id)
                )
            ),
        by = "team"
    ) 

matchup_theme <- theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10)
)

# offense

charts_offense <- purrr::map(
    .x = seq(
        from = 1,
        to = matchups_charts_base %>% 
            pull(game_id) %>% 
            max(),
        by = 1
    ),
    ~ matchups_charts_base %>% 
        filter(
            game_id == .x
        ) %>%
        mutate(
            team = forcats::fct_reorder(
                .f = team,
                .x = team_home
            )
        ) %>% 
        select(
            Team = team, 
            `Scoring shots*^` = score_shots_mean, 
            `Inside 50s*` = i50_mean, 
            `Turnovers*` = turn_mean, 
            `Contested possession\ndifferential*` = cp_diff_mean,
            `Clearance\ndifferential*` = clear_diff_mean
        ) %>% 
        ggradar::ggradar(
            values.radar = c(NA, NA, NA),
            axis.label.size = 5,
            axis.label.offset = 1.1,
            group.line.width = 2,
            group.point.size = 8,
            background.circle.colour = "white",
            axis.line.colour = "gray80",
            gridline.min.colour = "gray80",
            gridline.mid.colour = "gray80",
            gridline.max.colour = "gray80"
        ) +
        scale_colour_brewer(palette = "Set2") +
        labs(
            title = paste0(
                "Round ", rounds_so_far + 1, 
                ": ",
                afl_elo_pred %>% 
                    select(home_team) %>% 
                    slice(.x) %>% 
                    pull(),
                " vs. ",
                afl_elo_pred %>% 
                    select(away_team) %>% 
                    slice(.x) %>% 
                    pull(),
                " @ ",
                afl_elo_pred %>% 
                    select(venue) %>% 
                    slice(.x) %>% 
                    pull()
            ),
            subtitle = "\nTeam offensive strengths & weaknesses - closer to the outside indicates greater strength",
            caption = "*Opposition adjusted
                        ^Exlcudes out on the full
                        Created by: footycharts. Source: AFL website"
        ) + matchup_theme
)

# defense

charts_defense <- purrr::map(
    .x = seq(
        from = 1,
        to = matchups_charts_base %>% 
            pull(game_id) %>% 
            max(),
        by = 1
    ),
    ~ matchups_charts_base %>% 
        filter(
            game_id == .x
        ) %>% 
        mutate(
            team = forcats::fct_reorder(
                .f = team,
                .x = team_home
            )
        ) %>% 
        select(
            Team = team,
            `Opposition scoring shots*^` = score_shots_opp_mean,
            `Opposition\ninside 50s*` = i50_opp_mean,
            `Opposition\nturnovers*` = turn_opp_mean,
            `Intercept marks\nper opp. inside 50` = int_mean,
            `Tackles` = tackles_mean
        ) %>% 
        ggradar::ggradar(
            values.radar = c(NA, NA, NA),
            axis.label.size = 5,
            axis.label.offset = 1.1,
            group.line.width = 2,
            group.point.size = 8,
            background.circle.colour = "white",
            axis.line.colour = "gray80",
            gridline.min.colour = "gray80",
            gridline.mid.colour = "gray80",
            gridline.max.colour = "gray80"
        ) +
        scale_colour_brewer(palette = "Set2") +
        labs(
            title = paste0(
                "Round ", rounds_so_far + 1, 
                ": ",
                afl_elo_pred %>% 
                    select(home_team) %>% 
                    slice(.x) %>% 
                    pull(),
                " vs. ",
                afl_elo_pred %>% 
                    select(away_team) %>% 
                    slice(.x) %>% 
                    pull(),
                " @ ",
                afl_elo_pred %>% 
                    select(venue) %>% 
                    slice(.x) %>% 
                    pull()
            ),
            subtitle = "\nTeam defensive strengths & weaknesses - closer to the outside indicates greater strength",
            caption = "*Opposition adjusted
                        ^Exlcudes out on the full
                        Created by: footycharts. Source: AFL website"
        ) + matchup_theme
)

# play style

charts_play_style <- purrr::map(
    .x = seq(
        from = 1,
        to = matchups_charts_base %>% 
            pull(game_id) %>% 
            max(),
        by = 1
    ),
    ~ matchups_charts_base %>% 
        filter(
            game_id == .x
        )  %>%
        mutate(
            team = forcats::fct_reorder(
                .f = team,
                .x = team_home
            )
        ) %>% 
        select(
            Team = team, 
            `Kick-handball ratio` = khb_ratio_mean, 
            `Uncontested\npossession rate` = up_rate_mean, 
            `Marks` = marks_mean, 
            `Metres gained\nper disposal` = mg_disp_mean
        ) %>% 
        ggradar::ggradar(
            values.radar = c(NA, NA, NA),
            axis.label.size = 5,
            axis.label.offset = 1.1,
            group.line.width = 2,
            group.point.size = 8,
            background.circle.colour = "white",
            axis.line.colour = "gray80",
            gridline.min.colour = "gray80",
            gridline.mid.colour = "gray80",
            gridline.max.colour = "gray80"
        ) +
        scale_colour_brewer(palette = "Set2") +
        labs(
            title = paste0(
                "Round ", rounds_so_far + 1, 
                ": ",
                afl_elo_pred %>% 
                    select(home_team) %>% 
                    slice(.x) %>% 
                    pull(),
                " vs. ",
                afl_elo_pred %>% 
                    select(away_team) %>% 
                    slice(.x) %>% 
                    pull(),
                " @ ",
                afl_elo_pred %>% 
                    select(venue) %>% 
                    slice(.x) %>% 
                    pull()
            ),
            subtitle = "\nTeam defensive strengths & weaknesses - closer to the outside indicates greater strength",
            caption = "*Opposition adjusted
                        ^Exlcudes out on the full
                        Created by: footycharts. Source: AFL website"
        ) + matchup_theme
)

# save charts

# offense

purrr::map2(
    .x = charts_offense,
    .y = seq(
        from = 1,
        to = nrow(afl_elo_pred),
        by = 1
    ),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            "Round ", rounds_so_far + 1,
            ": ",
            afl_elo_pred %>% 
                select(home_team) %>% 
                slice(.y) %>% 
                pull(),
            " vs. ",
            afl_elo_pred %>% 
                select(away_team) %>% 
                slice(.y) %>% 
                pull(),
            "_1_offense.png"
        ),
        path = paste0(
            round_path,
            "/matchups"
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)

# defense

purrr::map2(
    .x = charts_defense,
    .y = seq(
        from = 1,
        to = nrow(afl_elo_pred),
        by = 1
    ),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            "Round ", rounds_so_far + 1, 
            ": ",
            afl_elo_pred %>% 
                select(home_team) %>% 
                slice(.y) %>% 
                pull(),
            " vs. ",
            afl_elo_pred %>% 
                select(away_team) %>% 
                slice(.y) %>% 
                pull(),
            "_2_defense.png"
        ),
        path = paste0(
            round_path,
            "/matchups"
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)

# play style

purrr::map2(
    .x = charts_play_style,
    .y = seq(
        from = 1,
        to = nrow(afl_elo_pred),
        by = 1
    ),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            "Round ", rounds_so_far + 1, 
            ": ",
            afl_elo_pred %>% 
                select(home_team) %>% 
                slice(.y) %>% 
                pull(),
            " vs. ",
            afl_elo_pred %>% 
                select(away_team) %>% 
                slice(.y) %>% 
                pull(),
            "_3_play_style.png"
        ),
        path = paste0(
            round_path,
            "/matchups"
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)
