library(dplyr)
library(ggplot2)

start_season <- "2022"
start_round <- 23
current_season <- "2023" # can be any season
end_round <- 24

# run the elo model

source(
    here::here(
        "run", 
        "run.R"
    )
) 
    
# chart

plot_title <- "Team elo ratings change"
plot_subtitle <- paste0(
    "Start: ",
    "Round ", start_round, ", ", start_season,
    ", ",
    "end: ",
    "Round ", end_round, ", ", current_season
    
)
x_axis_title <- "Team"
y_axis_title <- "Elo rating change*"
plot_caption <- "*Average team rating: 1500
                 Created by: footycharts"

afl_elo %>% 
    filter(
        (season == as.numeric(start_season) & round == paste("Round", start_round))
        | (season == as.numeric(current_season) & round == paste("Round", end_round))
    ) %>% 
    group_by(team) %>% 
    mutate(
        round = stringr::str_replace(round, "Round ", "") %>% 
            forcats::fct_inorder(),
        elo_change = new_elo - lag(new_elo)
    ) %>% 
    ungroup() %>% 
    filter(season == current_season) %>% 
    ggplot(
        aes(
            x = forcats::fct_reorder(team, elo_change), 
            y = elo_change, 
            fill = elo_change
        )
    ) +
    geom_col(colour = "black") +
    geom_text(
        aes(
            x = forcats::fct_reorder(team, elo_change),
            y = elo_change + if_else(elo_change > 0, 4, -4),
            label = if_else(
                elo_change > 0,
                paste0(
                    "+",
                    round(elo_change, 1)
                ),
                as.character(round(elo_change, 1))
            )
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_gradient(low = "white", high = "dodgerblue") +
    labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = x_axis_title,
        y = y_axis_title,
        caption = plot_caption
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
