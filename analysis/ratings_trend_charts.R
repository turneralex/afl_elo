library(dplyr)
library(ggplot2)

# trended elo rating

# chart

plot_title <- "Team rating by round"
plot_subtitle <- paste("Season:", current_season)
x_axis_title <- "Round"
y_axis_title <- "Team rating*"
plot_caption <- "*Average team rating: 1500
                Created by: footycharts"

ratings_trend <- afl_elo %>% 
    filter(
        season == current_season
        & round_number %in% seq(
            from = 0,
            to = rounds_so_far,
            by = 1
        )
    ) %>%
    mutate(
        round = stringr::str_replace(
            round, 
            pattern = "Round ", 
            replacement = ""
        ) %>% 
            forcats::fct_inorder()
    ) %>% 
    ggplot(
        aes(
            x = round, 
            y = new_elo, 
            group = 1
        )
    ) +
    geom_hline(
        yintercept = 1500, 
        linewidth = 1, 
        colour = "firebrick1", 
        alpha = 0.8
    ) +
    geom_point() + 
    geom_line() +
    facet_wrap(
        . ~ team, 
        nrow = 5, 
        ncol = 4
    ) +
    labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = x_axis_title,
        y = y_axis_title,
        caption = plot_caption
    ) +
    theme_bw() +
    theme(
        axis.text.y = element_text(size = 15), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        strip.text = element_text(size = 15)
    ) 