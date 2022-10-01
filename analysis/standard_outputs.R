library(viridis)

# trended elo rating

trended_elo <- afl_elo %>% 
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

# current elo ratings

rank_elo <- afl_elo_rank %>% 
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

map2(
    list(
        trended_elo,
        rank_elo
    ),
    c("trend", "rank"),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            "_",
            .y,
            ".png"
        ),
        path = paste0(
            here::here("files/charts/"),
            current_season,
            "_",
            round_name
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)
