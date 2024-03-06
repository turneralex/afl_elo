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

# add ladder position to team stats table

team_stats <- team_stats %>% 
    inner_join(
        afl_ladder,
        by = "team"
    ) %>% 
    mutate(
        team_position = case_when(
            ladder_position == 1 ~ paste0(ladder_position, "st: ", team),
            ladder_position == 2 ~ paste0(ladder_position, "nd: ", team),
            ladder_position == 3 ~ paste0(ladder_position, "rd: ", team),
            T                    ~ paste0(ladder_position, "th: ", team)
        )
    ) 

# generate elo predictions for next round

afl_elo_pred_base <- afl_elo %>% 
    filter(
        season == current_season
    ) %>% 
    inner_join(
        afl_ladder,
        by = "team"
    ) %>% 
    mutate(
        elo_rank = case_when(
            rank(-start_elo) == 1 ~ paste0(rank(-start_elo), "st"),
            rank(-start_elo) == 2 ~ paste0(rank(-start_elo), "nd"),
            rank(-start_elo) == 3 ~ paste0(rank(-start_elo), "rd"),
            T ~                     paste0(rank(-start_elo), "th")
        ),
        ladder_position = case_when(
            ladder_position == 1 ~ paste0(ladder_position, "st"),
            ladder_position == 2 ~ paste0(ladder_position, "nd"),
            ladder_position == 3 ~ paste0(ladder_position, "rd"),
            T ~                    paste0(ladder_position, "th")
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
        correct_tip = if_else(
            (score_expected >= 0.5 & margin >= 0)
            | (score_expected <= 0.5 & margin <= 0),
            1,
            0
        ),
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
                round_name,
                ": ",
                as.character(afl_elo_pred[.x, 1]),
                " vs. ",
                as.character(afl_elo_pred[.x, 2]),
                " @ ",
                as.character(afl_elo_pred[.x, 3])
            ),
            subtitle = "\nTeam offensive strengths & weaknesses - closer to the outside indicates greater strength",
            caption = "*Opposition adjusted
                        ^Exlcudes out on the full
                        Created by: footycharts. Source: AFL website"
        ) +
        theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.position = "bottom",
            plot.caption = element_text(size = 10)
        )
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
                round_name,
                ": ",
                as.character(afl_elo_pred[.x, 1]),
                " vs. ",
                as.character(afl_elo_pred[.x, 2]),
                " @ ",
                as.character(afl_elo_pred[.x, 3])
            ),
            subtitle = "\nTeam defensive strengths & weaknesses - closer to the outside indicates greater strength",
            caption = "*Opposition adjusted
                        ^Exlcudes out on the full
                        Created by: footycharts. Source: AFL website"
        ) +
        theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.position = "bottom",
            plot.caption = element_text(size = 10)
        )
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
                round_name,
                ": ",
                as.character(afl_elo_pred[.x, 1]),
                " vs. ",
                as.character(afl_elo_pred[.x, 2]),
                " @ ",
                as.character(afl_elo_pred[.x, 3])
            ),
            subtitle = "\nTeam defensive strengths & weaknesses - closer to the outside indicates greater strength",
            caption = "*Opposition adjusted
                        ^Exlcudes out on the full
                        Created by: footycharts. Source: AFL website"
        ) +
        theme(
            plot.title = element_text(size = 20, hjust = 0.5), 
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.position = "bottom",
            plot.caption = element_text(size = 10)
        ) 
)

# save charts

# offense

purrr::map2(
    .x = charts_offense,
    .y = 1:nrow(afl_elo_pred),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            ": ",
            as.character(afl_elo_pred[.y, 1]),
            " vs. ",
            as.character(afl_elo_pred[.y, 2]),
            "_1_offense.png"
        ),
        path = here::here(
            "files",
            "charts", 
            paste0(
                current_season,
                "_",
                round_name
            ),
            "matchups"
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)

# defense

purrr::map2(
    .x = charts_defense,
    .y = 1:nrow(afl_elo_pred),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            ": ",
            as.character(afl_elo_pred[.y, 1]),
            " vs. ",
            as.character(afl_elo_pred[.y, 2]),
            "_2_defense.png"
        ),
        path = here::here(
            "files",
            "charts", 
            paste0(
                current_season,
                "_",
                round_name
            ),
            "matchups"
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)

# play style

purrr::map2(
    .x = charts_play_style,
    .y = 1:nrow(afl_elo_pred),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            ": ",
            as.character(afl_elo_pred[.y, 1]),
            " vs. ",
            as.character(afl_elo_pred[.y, 2]),
            "_3_play_style.png"
        ),
        path = here::here(
            "files",
            "charts", 
            paste0(
                current_season,
                "_",
                round_name
            ),
            "matchups"
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)
