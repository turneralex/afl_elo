library(dplyr)
library(ggplot2)

plot_title <- "Team rating quadrants - is your team good & are they trending in the right direction?"
plot_subtitle <- paste0(
    "Season: ", 
    current_season
)
x_axis_title <- "Team rating vs. average*"
y_axis_title <- if (compare_change_start_season) {
    "Team rating change vs. start of season"
} else {
    "Team rating change over last 5 games"
} 
plot_caption <- "*Average team rating: 1500
                Created by: footycharts"

plus_minus_avg_low <- afl_elo_rank_change %>% 
    pull(plus_minus_avg) %>% 
    min() * 0.5

plus_minus_avg_high <- afl_elo_rank_change %>% 
    pull(plus_minus_avg) %>% 
    max() * 0.5

plus_minus_prev_low <- afl_elo_rank_change %>% 
    pull(
        if (compare_change_start_season) {
            plus_minus_prev_season
        } else {
            plus_minus_prev_5_games
        } 
    ) %>% 
    min() * 0.5

plus_minus_prev_high <- afl_elo_rank_change %>% 
    pull(
        if (compare_change_start_season) {
            plus_minus_prev_season
        } else {
            plus_minus_prev_5_games
        } 
    ) %>% 
    max() * 0.5

ratings_quadrants <- afl_elo_rank_change %>% 
    # filter(
    #   !(team %in% c("North Melbourne"))
    # ) %>%
    inner_join(
        team_logos,
        by = "team"
    ) %>% 
    ggplot(
        aes(
            x = plus_minus_avg,
            if (compare_change_start_season) {
                y = plus_minus_prev_season
            } else {
                y = plus_minus_prev_5_games
            },
            image = logo
        )
    ) +
    geom_vline(
        xintercept = 0, 
        colour = "firebrick1", 
        alpha = 0.8, 
        linetype = "dashed"
    ) +
    geom_hline(
        yintercept = 0, 
        colour = "firebrick1", 
        alpha = 0.8, 
        linetype = "dashed"
    ) +
    geom_text(
        aes(
            x = plus_minus_avg_high,
            y = plus_minus_prev_high,
            label = "Joy"
        ),
        size = 12,
        colour = "grey"
    ) +
    geom_text(
        aes(
            x = plus_minus_avg_high,
            y = plus_minus_prev_low,
            label = "Concern"
        ),
        size = 12,
        colour = "grey"
    ) +
    geom_text(
        aes(
            x = plus_minus_avg_low,
            y = plus_minus_prev_low,
            label = "Despair"
        ),
        size = 12,
        colour = "grey"
    ) +
    geom_text(
        aes(
            x = plus_minus_avg_low,
            y = plus_minus_prev_high,
            label = "Hope"
        ),
        size = 12,
        colour = "grey"
    ) +
    ggimage::geom_image() +
    labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = x_axis_title,
        y = y_axis_title,
        caption = plot_caption
    ) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10)
    )
