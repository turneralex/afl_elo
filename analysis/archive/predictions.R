# elo mactchups & predictions for next round

afl_elo_pred <- afl_elo %>% 
    filter(season == current_season, 
           round == paste("Round", rounds_so_far + 1)) %>% 
    group_by(match_id) %>%
    mutate(away_team = lead(team),
           away_elo = lead(start_elo)) %>% 
    slice(1) %>%
    ungroup() %>%
    select(location, home_team = team, away_team, venue, hga_app, home_elo = start_elo, away_elo, score_expected) %>% 
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
            home_team,
            ": ",
            round(home_elo),
            " vs. ",
            away_team,
            ": ",
            round(away_elo),
            "\n@ ",
            venue,
            ", home ground advantage: +",
            round(hga),
            "\n Predicted winner: ",
            pred_winner
        )
    ) 

# table

afl_elo_pred %>% 
    select(home_team,
           away_team,
           venue,
           hga_app,
           hga,
           home_elo,
           away_elo,
           elo_diff_hga,
           score_expected)

# chart

afl_elo_pred %>% 
    ggplot(aes(fct_reorder(matchup, -game_id), elo_diff_hga, fill = -elo_diff_hga)) +
    geom_col(colour = "black", position = "dodge", width = 0.5) +
    geom_text(aes(fct_reorder(matchup, -game_id), 
                  y = elo_diff_hga + if_else(elo_diff_hga > 0, 16, -16),
                  label = if_else(elo_diff_hga > 0, paste0("+", round(elo_diff_hga, 1)), as.character(round(elo_diff_hga, 1)))),
              size = 5) +
    geom_hline(yintercept = 0) +
    scale_fill_distiller(palette = "RdPu") +
    scale_y_continuous(limits = c(-50, 195)) +
    coord_flip() +
    labs(title = paste0("Matchups & predictions - Round ", rounds_so_far + 1),
         subtitle = paste("Season:", current_season, "\nCorrect tips so far:", correct_tips),
         x = "Matchup",
         y = "Elo rating difference + home ground advantage",
         caption = "Created by: footycharts") +
    theme(axis.text.y = element_text(size = 15), 
          axis.title = element_text(size = 15), 
          plot.title = element_text(size = 20), 
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none") 