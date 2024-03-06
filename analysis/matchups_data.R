library(dplyr)

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

# available player stats check

if (exists("rounds_so_far")) {
    
    message(
        paste0(
            "player stats available for round ",
            rounds_so_far,
            ":"
        )
    )
    
    print(
        afl_stats %>% 
            filter(
                round_roundnumber == rounds_so_far
            ) %>% 
            distinct(
                home_team_name,
                away_team_name
            )
    )
    
}

# get key stats:

# offense

# scoring shots
# inside 50s
# turnovers
# contested possession differential
# clearance differential

# defense

# opp scoring shots
# opp inside 50s
# opp turnovers
# intercept marks per opp. inside 50
# tackles

# style

# kick to handball ratio
# uncontested possession rate
# total marks
# metres gained per disposal

team_round_stats_base <- afl_stats %>% 
    filter(
        round_roundnumber %in% 1:rounds_so_far
    ) %>% 
    group_by(
        round_name, 
        home_team_club_name, 
        away_team_club_name
    ) %>% 
    mutate(
        # offense
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
            sum(),
        clear_home = if_else(
            team_name == home_team_club_name,
            clearances_totalclearances,
            0
        ) %>% 
            sum(),
        clear_away = if_else(
            team_name == away_team_club_name,
            clearances_totalclearances,
            0
        ) %>% 
            sum(),
        int_home = if_else(
            team_name == home_team_club_name,
            extendedstats_interceptmarks,
            0
        ) %>% 
            sum(),
        int_away = if_else(
            team_name == away_team_club_name,
            extendedstats_interceptmarks,
            0
        ) %>% 
            sum(),
        tackles_home = if_else(
            team_name == home_team_club_name,
            tackles,
            0
        ) %>% 
            sum(),
        tackles_away = if_else(
            team_name == away_team_club_name,
            tackles,
            0
        ) %>% 
            sum(),
        kicks_home = if_else(
            team_name == home_team_club_name,
            kicks,
            0
        ) %>% 
            sum(),
        kicks_away = if_else(
            team_name == away_team_club_name,
            kicks,
            0
        ) %>% 
            sum(),
        hb_home = if_else(
            team_name == home_team_club_name,
            handballs,
            0
        ) %>% 
            sum(),
        hb_away = if_else(
            team_name == away_team_club_name,
            handballs,
            0
        ) %>% 
            sum(),
        up_home = if_else(
            team_name == home_team_club_name,
            uncontestedpossessions,
            0
        ) %>% 
            sum(),
        up_away = if_else(
            team_name == away_team_club_name,
            uncontestedpossessions,
            0
        ) %>% 
            sum(),
        marks_home = if_else(
            team_name == home_team_club_name,
            marks,
            0
        ) %>% 
            sum(),
        marks_away = if_else(
            team_name == away_team_club_name,
             marks,
            0
        ) %>% 
            sum(),
        mg_home = if_else(
            team_name == home_team_club_name,
            metresgained,
            0
        ) %>% 
            sum(),
        mg_away = if_else(
            team_name == away_team_club_name,
            metresgained,
            0
        ) %>% 
            sum(),
        disp_home = if_else(
            team_name == home_team_club_name,
            disposals,
            0
        ) %>% 
            sum(),
        disp_away = if_else(
            team_name == away_team_club_name,
            disposals,
            0
        ) %>% 
            sum()
    ) %>% 
    ungroup() %>% 
    select(
        round_name, 
        home_team_club_name, 
        away_team_club_name,
        team_name,
        score_shots_home,
        score_shots_away,
        i50_home,
        i50_away,
        turn_home,
        turn_away,
        cp_home,
        cp_away,
        clear_home,
        clear_away,
        int_home,
        int_away,
        tackles_home,
        tackles_away,
        kicks_home,
        kicks_away,
        hb_home,
        hb_away,
        up_home,
        up_away,
        marks_home,
        marks_away,
        mg_home,
        mg_away,
        disp_home,
        disp_away
    ) %>% 
    mutate(
        home_team_club_name = change_team_name(home_team_club_name), 
        away_team_club_name = change_team_name(away_team_club_name),
        team_name = change_team_name(team_name)
    ) %>% 
    distinct()

team_stats_base <- team_round_stats_base %>% 
    group_by(
        round_name, 
        team_name
    ) %>% 
    summarise(
        
        # offense
        
        # scoring shots
        # inside 50s
        # turnovers
        # contested possession differential
        # clearance differential
        
        score_shots_total = mean(
            if_else(
                team_name == home_team_club_name,
                score_shots_home,
                score_shots_away
            )
        ),
        i50_total = mean(
            if_else(
                team_name == home_team_club_name,
                i50_home,
                i50_away
            )
        ),
        turn_total = mean(
            if_else(
                team_name == home_team_club_name,
                turn_home,
                turn_away
            )
        ),
        cp_total = mean(
            if_else(
                team_name == home_team_club_name,
                cp_home,
                cp_away
            )
        ),
        cp_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                cp_away,
                cp_home
            )
        ),
        clear_total = mean(
            if_else(
                team_name == home_team_club_name,
                clear_home,
                clear_away
            )
        ),
        clear_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                clear_away,
                clear_home
            )
        ),
        
        # defense
        
        # opp scoring shots
        # opp inside 50s
        # opp turnovers
        # intercept marks
        # tackles
        
        score_shots_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                score_shots_away,
                score_shots_home
            )
        ),
        i50_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                i50_away,
                i50_home
            )
        ),
        turn_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                turn_away,
                turn_home
            )
        ),
        int_total = mean(
            if_else(
                team_name == home_team_club_name,
                int_home,
                int_away
            )
        ),
        tackles_total = mean(
            if_else(
                team_name == home_team_club_name,
                tackles_home,
                tackles_away
            )
        ),
        
        # style
        
        # kick to handball ratio
        # uncontested possession rate
        # total marks
        # metres gained per disposal
        
        kicks_total = mean(
            if_else(
                team_name == home_team_club_name,
                kicks_home,
                kicks_away
            )
        ),
        hb_total = mean(
            if_else(
                team_name == home_team_club_name,
                hb_home,
                hb_away
            )
        ),
        up_total = mean(
            if_else(
                team_name == home_team_club_name,
                up_home,
                up_away
            )
        ),
        marks_total = mean(
            if_else(
                team_name == home_team_club_name,
                marks_home,
                marks_away
            )
        ),
        mg_total = mean(
            if_else(
                team_name == home_team_club_name,
                mg_home,
                mg_away
            )
        ),
        disp_total = mean(
            if_else(
                team_name == home_team_club_name,
                disp_home,
                disp_away
            )
        ),
    ) %>% 
    rename(team = team_name) %>% 
    group_by(team) %>% 
    summarise(
        score_shots_mean = mean(score_shots_total),
        i50_mean = mean(i50_total),
        turn_mean = mean(turn_total),
        cp_mean = mean(cp_total),
        cp_opp_mean = mean(cp_opp_total),
        clear_mean = mean(clear_total),
        clear_opp_mean = mean(clear_opp_total),
        
        score_shots_opp_mean = mean(score_shots_opp_total),
        i50_opp_mean = mean(i50_opp_total),
        turn_opp_mean = mean(turn_opp_total),
        int_mean = mean(int_total) / mean(i50_opp_total),
        tackles_mean = mean(tackles_total),
        
        khb_ratio_mean = mean(kicks_total) / mean(hb_total),
        up_rate_mean = mean(up_total) / (mean(up_total) + mean(cp_total)),
        marks_mean = mean(marks_total),
        mg_disp_mean = mean(mg_total) / mean(disp_total)
    ) 

team_stats_base_adj <- team_round_stats_base %>% 
    mutate(
        team_name_opp = if_else(
            team_name == home_team_club_name,
            away_team_club_name,
            home_team_club_name
        )
    ) %>% 
    inner_join(
        team_stats_base,
        by = c(
            "team_name_opp" = "team"
        )
    ) %>% 
    mutate(
        score_shots_home_adj = score_shots_home - score_shots_opp_mean,
        score_shots_away_adj = score_shots_away - score_shots_opp_mean,
        i50_home_adj = i50_home - i50_opp_mean,
        i50_away_adj = i50_away - i50_opp_mean,
        turn_home_adj = turn_home - turn_opp_mean,
        turn_away_adj = turn_away - turn_opp_mean,
        cp_home_adj = cp_home - cp_opp_mean,
        cp_away_adj = cp_away - cp_opp_mean,
        clear_home_adj = clear_home - clear_opp_mean,
        clear_away_adj = clear_away - clear_opp_mean
    ) %>% 
    group_by(team_name) %>% 
    summarise(
        
        score_shots_mean = mean(
            if_else(
                team_name == home_team_club_name,
                score_shots_home_adj,
                score_shots_away_adj
            )
        ),
        i50_mean = mean(
            if_else(
                team_name == home_team_club_name,
                i50_home_adj,
                i50_away_adj
            )
        ),
        turn_mean = -mean( # convert to negative
            if_else(
                team_name == home_team_club_name,
                turn_home_adj,
                turn_away_adj
            )
        ),
        cp_diff_mean = mean(
            if_else(
                team_name == home_team_club_name,
                cp_home_adj - cp_away_adj,
                cp_away_adj - cp_home_adj
            )
        ),
        clear_diff_mean = mean(
            if_else(
                team_name == home_team_club_name,
                clear_home_adj - clear_away_adj,
                clear_away_adj - clear_home_adj
            )
        ),
        
        score_shots_opp_mean = -mean( # convert to negative 
            if_else(
                team_name == home_team_club_name,
                score_shots_away_adj,
                score_shots_home_adj
            )
        ),
        i50_opp_mean = -mean( # convert to negative
            if_else(
                team_name == home_team_club_name,
                i50_away_adj,
                i50_home_adj
            )
        ),
        turn_opp_mean = mean( 
            if_else(
                team_name == home_team_club_name,
                turn_away_adj,
                turn_home_adj
            )
        )
    ) %>% 
    rename(team = team_name) %>% 
    inner_join(
        team_stats_base %>% 
            select(
                team,
                int_mean,
                tackles_mean,
                khb_ratio_mean,
                up_rate_mean,
                marks_mean,
                mg_disp_mean
            ),
        by = "team"
    )
