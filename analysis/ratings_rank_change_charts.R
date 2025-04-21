library(dplyr)
library(ggplot2)

# current elo ratings

ratings_rank <- afl_elo_rank_change %>% 
    ggplot(
        aes(
            x = forcats::fct_reorder(
                .f = elo_position_team_avg, 
                .x = -elo_position_avg
            ), 
            y = pred_margin, 
            fill = plus_minus_avg
        )
    ) +
    geom_col(colour = "black") +
    geom_text(
        aes(
            forcats::fct_reorder(
                .f = elo_position_team_avg, 
                .x = -elo_position_avg
            ), 
            y = pred_margin + if_else(pred_margin > 0, 2, -2),
            label = round(pred_margin, 1)
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_gradient(low = "white", high = "seagreen3") +
    labs(
        title = paste0("Team ratings - ", round_concluded),
        subtitle = paste("Season:", current_season),
        x = "Team & rank",
        y = "Team rating*",
        caption = "*Expected margin vs. league average team on a neutral field
                    Created by: footycharts"
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        legend.position = "none"
    ) 

# change vs. previous week

ratings_change_prev_week <- afl_elo_rank_change %>%
    filter(
        round_number == rounds_so_far
    ) %>%
    mutate(
        improve_flag = if_else(
            plus_minus_prev_week > 0,
            1,
            0
        ) %>%
            factor()
    ) %>%
    ggplot(
        aes(
            x = forcats::fct_reorder(
                .f = team,
                .x = -elo_position_prev
            ),
            y = plus_minus_prev_week,
            fill = improve_flag
        )
    ) +
    geom_col(colour = "black", alpha = 0.8) +
    geom_text(
        aes(
            forcats::fct_reorder(
                .f = team,
                .x = -elo_position_prev
            ),
            y = plus_minus_prev_week + if_else(plus_minus_prev_week > 0, 0.8, -0.8),
            label = if_else(
                round(plus_minus_prev_week, 1) > 0,
                paste0(
                    "+",
                    round(plus_minus_prev_week, 1)
                ),
                round(plus_minus_prev_week, 1) %>%
                    as.character()
            )
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_manual(values = c("firebrick1", "springgreen4")) +
    labs(
        title = paste0("Team ratings change vs. previous week - ", round_concluded),
        subtitle = paste("Season:", current_season),
        x = "Team",
        y = "Team rating change",
        caption = "Created by: footycharts"
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        legend.position = "none"
    )
