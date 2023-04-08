library(dplyr)
library(ggplot2)

# get player stats

afl_stats <- fitzRoy::fetch_player_stats_afl(season = current_season) 

# update column names

afl_stats <- afl_stats %>% 
    rename_with(
        .fn = ~ .x %>% 
            tolower() %>% 
            stringr::str_replace_all(
                pattern = "[.]", 
                replacement = "_"
            )
    )

# get key stats:
# scoring shots
# inside 50s
# turnovers
# contested possessions

team_stats_base <- afl_stats %>% 
    filter(
        round_roundnumber %in% 1:as.integer(rounds_so_far)
        & !(round_name %in% finals_so_far)
    ) %>% 
    group_by(
        round_roundnumber, 
        home_team_club_name, 
        away_team_club_name
    ) %>% 
    mutate(
        score_shots_home = if_else(
            team_name == home_team_club_name,
            goals + behinds,
            0
        ) %>% 
            sum(),
        score_shots_away = if_else(
            team_name == away_team_club_name,
            goals + behinds,
            0
        ) %>% 
            sum(),
        i50_home = if_else(
            team_name == home_team_club_name,
            inside50s,
            0
        ) %>% 
            sum(),
        i50_away = if_else(
            team_name == away_team_club_name,
            inside50s,
            0
        ) %>% 
            sum(),
        turn_home = if_else(
            team_name == home_team_club_name,
            turnovers,
            0
        ) %>% 
            sum(),
        turn_away = if_else(
            team_name == away_team_club_name,
            turnovers,
            0
        ) %>% 
            sum(),
        cp_home = if_else(
            team_name == home_team_club_name,
            contestedpossessions,
            0
        ) %>% 
            sum(),
        cp_away = if_else(
            team_name == away_team_club_name,
            contestedpossessions,
            0
        ) %>% 
            sum()
    ) %>% 
    group_by(
        round_name, 
        team_name
    ) %>% 
    summarise(
        score_shots_total = sum(goals + behinds),
        score_shots_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                score_shots_away,
                score_shots_home
            )
        ),
        i50_total = sum(inside50s),
        i50_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                i50_away,
                i50_home
            )
        ),
        i50_diff = i50_total - i50_opp_total,
        
        turn_total = sum(turnovers),
        turn_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                turn_away,
                turn_home
            )
        ),
        turn_diff = turn_total - turn_opp_total,
        
        cp_total = sum(contestedpossessions),
        cp_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                cp_away,
                cp_home
            )
        ),
        cp_diff = cp_total - cp_opp_total
    ) %>% 
    rename(team = team_name) %>% 
    group_by(team) %>% 
    summarise(
        score_shots_mean = mean(score_shots_total),
        score_shots_opp_mean = mean(-score_shots_opp_total),
        i50_mean = mean(i50_total),
        i50_opp_mean = mean(-i50_opp_total),
        turn_diff_mean = mean(-turn_diff),
        cp_diff_mean = mean(cp_diff)
    ) %>% 
    mutate(
        team = change_team_name(team)
    ) 

# scale variables to be on between 0 and 1

team_stats <- team_stats_base %>% 
    mutate(
        across(
            c(
                score_shots_mean,
                score_shots_opp_mean,
                i50_mean,
                i50_opp_mean,
                turn_diff_mean,
                cp_diff_mean
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

afl_elo_pred <- afl_elo %>% 
    filter(
        season == current_season
        & round == paste("Round", rounds_so_far + 1)
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
    group_by(match_id) %>%
    mutate(
        away_team = lead(team, n = 1),
        away_elo = lead(start_elo, n = 1),
        away_ladder_position = lead(ladder_position, n = 1),
        away_elo_rank = lead(elo_rank, n = 1)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    select(
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
        away_ladder_position
    )  %>% 
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
        matchup = paste0(
            "HGA: +", round(hga), ", predicted winner: ", pred_winner,
            "\n\nLadder position\n",
            home_ladder_position, " vs. ", away_ladder_position,
            "\nElo ranking\n",
            home_elo_rank, " (", round(home_elo), ") vs. ", away_elo_rank, " (", round(away_elo), ")"
        )
    ) %>% 
    select(
        home_team, 
        away_team, 
        venue, 
        matchup
    )

# check predictions

afl_elo_pred %>% 
    pull(matchup) %>% 
    print()

# create charts 

charts <- purrr::map(
    .x = 1:nrow(afl_elo_pred),
    ~ team_stats %>% 
        inner_join(
            afl_elo_pred %>% 
                slice(.x) %>% 
                select(home_team:away_team) %>% 
                tidyr::pivot_longer(
                    cols = 1:2,
                    names_to = "home_away",
                    values_to = "team"
                ) %>% 
                select(team) %>% 
                mutate(
                    team_home = 0:1
                ),
            by = "team"
        ) %>%
        mutate(
            team = forcats::fct_reorder(
                .f = team,
                .x = team_home
            )
        ) %>%
        select(
            Team = team, 
            `Scoring shots*` = score_shots_mean, 
            `Opposition\nscoring shots*` = score_shots_opp_mean, 
            `Inside 50s` = i50_mean, 
            `Opposition\ninside 50s` = i50_opp_mean,
            `Turnover\ndifferential` = turn_diff_mean, 
            `Contested\npossession\ndifferential` = cp_diff_mean
        ) %>% 
        ggradar::ggradar(
            values.radar = c(NA, NA, NA),
            axis.label.size = 5,
            axis.label.offset = 1.1,
            group.line.width = 2,
            group.point.size = 8
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
            subtitle = paste0(
                afl_elo_pred[.x, 4],
                "\n\nTeam strengths & weaknesses - closer to the outside indicates greater strength"
            ),
            caption = "*Exlcudes out on the full
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

purrr::map2(
    .x = charts,
    .y = 1:nrow(afl_elo_pred),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            ": ",
            as.character(afl_elo_pred[.y, 1]),
            " vs. ",
            as.character(afl_elo_pred[.y, 2]),
            ".png"
        ),
        path = here::here(
            "files",
            "charts", 
            paste0(
                current_season,
                "_",
                round_name
            )
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)
