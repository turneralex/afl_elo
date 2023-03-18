source(here::here("run/run.R"))

library(viridis)

current_season <- 2022
rounds_so_far <- 23

afl_elo %>% 
    filter(
        (season == current_season - 1 & round == "Round 23")
        | (season == current_season & round == paste("Round", rounds_so_far))
    ) %>% 
    group_by(team) %>% 
    mutate(
        round = str_replace(round, "Round ", "") %>% 
            fct_inorder(),
        elo_change = new_elo - lag(new_elo)
    ) %>% 
    ungroup() %>% 
    filter(season == current_season) %>% 
    ggplot(aes(fct_reorder(team, elo_change), elo_change, fill = elo_change)) +
    geom_col(colour = "black") +
    geom_text(aes(fct_reorder(team, elo_change),
                  y = elo_change + if_else(elo_change > 0, 2, -2),
                  label = if_else(
                      elo_change > 0,
                      paste0(
                          "+",
                          round(elo_change, 1)
                      ),
                      as.character(round(elo_change, 1))
                  )),
              size = 5) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_viridis() +
    labs(title = "Team elo ratings change vs. previous year",
         subtitle = paste("Season:", current_season),
         x = "Team",
         y = "Elo rating change*",
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
