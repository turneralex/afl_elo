source(here::here("run/update.R"))

library(viridis)

rounds_so_far <- 23

# trended elo rating

afl_elo %>% 
    filter(season == current_season,
           round %in% paste("Round", 1:rounds_so_far)) %>%
    mutate(
        round = str_replace(round, "Round ", "") %>% 
            fct_inorder()
    ) %>% 
    ggplot(aes(round, new_elo, group = 1)) +
    geom_hline(yintercept = 1500, size = 1, colour = "firebrick1", alpha = 0.8) +
    geom_point() + 
    geom_line() +
    facet_wrap(. ~ team, nrow = 5, ncol = 4) +
    labs(title = "Team elo rating by round",
         subtitle = paste("Season:", current_season),
         x = "Round",
         y = "Team elo rating*",
         caption = "*Average team rating: 1500
                    Created by: footycharts") +
    theme(axis.text.y = element_text(size = 15), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title = element_text(size = 20), 
          plot.title = element_text(size = 20), 
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10),
          strip.text = element_text(size = 15)) 

# current elo position

afl_elo_rank <- afl_elo %>% 
    filter(season == current_season,
           round %in% paste("Round", 1:rounds_so_far)) %>% 
    group_by(team) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    select(team, new_elo) %>% 
    arrange(-new_elo) %>% 
    mutate(
        plus_minus = new_elo - 1500,
        elo_position = row_number(),
        elo_position_team = paste0(
            elo_position,
            ": ",
            team
        )
    ) 

afl_elo_rank

# current elo ratings

afl_elo_rank %>% 
    ggplot(aes(fct_reorder(elo_position_team, -elo_position), plus_minus, fill = plus_minus)) +
    geom_col(colour = "black") +
    geom_text(aes(fct_reorder(elo_position_team, -elo_position), 
                  y = plus_minus + if_else(new_elo > 1500, 6, -6),
                  label = round(new_elo)),
              size = 5) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_viridis(option = "magma") +
    labs(title = paste0("Team elo ratings - Round ", rounds_so_far),
         subtitle = paste("Season:", current_season),
         x = "Team & rank",
         y = "Elo rating*",
         caption = "*Average team rating: 1500
                    Created by: footycharts") +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title = element_text(size = 20), 
          plot.title = element_text(size = 20), 
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10),
          axis.text.y = element_text(size = 15),
          legend.position = "none")

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

# correct tips so far

correct_tips <- afl_fixture_2022 %>% 
    filter(season == current_season, 
           round %in% paste("Round", 1:rounds_so_far)) %>% 
    select(-match_id) %>% 
    inner_join(
        afl_elo,
        by = c("season", "round", "home_team" = "team")
    ) %>% 
    mutate(
        correct_tip = if_else(
            (score_expected >= 0.5 & home_score - away_score >= 0)
            | (score_expected <= 0.5 & home_score - away_score <= 0),
            1,
            0
        )
    ) %>% 
    pull(correct_tip) %>% 
    sum()

correct_tips

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
