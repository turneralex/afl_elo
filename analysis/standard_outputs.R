library(dplyr)
library(ggplot2)

# trended elo rating

trended_elo <- afl_elo %>% 
    filter(
        season == current_season
        & round %in% paste("Round", 1:rounds_so_far)
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
        size = 1, 
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
        title = "Team elo rating by round",
        subtitle = paste("Season:", current_season),
        x = "Round",
        y = "Team elo rating*",
        caption = "*Average team rating: 1500
                    Created by: footycharts"
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(size = 15), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        strip.text = element_text(size = 15)
    ) 

# current elo position

afl_elo_rank <- afl_elo %>% 
    filter(
        season == current_season
        & round %in% paste("Round", 1:rounds_so_far)
    ) %>% 
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
    ggplot(
        aes(
            x = forcats::fct_reorder(
                .f = elo_position_team, 
                .x = -elo_position
            ), 
            y = plus_minus, 
            fill = plus_minus
        )
    ) +
    geom_col(colour = "black") +
    geom_text(
        aes(
            forcats::fct_reorder(
                .f = elo_position_team, 
                .x = -elo_position
            ), 
            y = plus_minus + if_else(new_elo > 1500, 4, -4),
            label = round(new_elo)
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    viridis::scale_fill_viridis(option = "magma") +
    labs(
        title = paste0("Team elo ratings - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Team & rank",
        y = "Elo rating*",
        caption = "*Average team rating: 1500
                    Created by: footycharts"
    ) +
    theme_minimal() +
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

# save rank & trend

purrr::map2(
    .x = list(
        trended_elo,
        rank_elo
    ),
    .y = c("trend", "rank"),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            "_",
            .y,
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

# team logos

team_logos <- team_stats_base %>% 
    select(team) %>% 
    mutate(
        logo = here::here(
            "files",
            "team_logos",
            paste0(
                team %>% 
                    stringr::str_remove_all(pattern = " ") %>% 
                    tolower(),
                ".png"
            )
        )
    )

# team offense

offense_avg <- team_stats_base %>% 
    summarise(
        i50_mean = mean(i50_mean),
        score_shots_mean = mean(score_shots_mean)
    )

team_stats_base %>% 
    inner_join(
        team_logos,
        by = "team"
    ) %>% 
    ggplot(
        aes(
            x = i50_mean, 
            y = score_shots_mean, 
            image = logo
        )
    ) +
    geom_vline(
        xintercept = offense_avg$i50_mean, 
        colour = "firebrick1", 
        alpha = 0.8, 
        linetype = "dashed"
    ) +
    geom_hline(
        yintercept = offense_avg$score_shots_mean, 
        colour = "firebrick1", 
        alpha = 0.8, 
        linetype = "dashed"
    ) +
    ggimage::geom_image() +
    labs(
        title = paste0("Team offensive indicators - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Inside 50s",
        y = "Scoring shots*",
        caption = "*Excludes out on the full
                    Lines represent league average
                    Created by: footycharts. Source: AFL website"
    ) +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10)
    ) 

# save file

ggsave(
    filename = paste0(
        round_name,
        "_team_offense",
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

# team defense

defense_avg <- team_stats_base %>% 
    summarise(
        i50_opp_mean = mean(i50_opp_mean),
        score_shots_opp_mean = mean(score_shots_opp_mean)
    )

team_stats_base %>% 
    inner_join(
        team_logos,
        by = "team"
    ) %>% 
    ggplot(
        aes(
            x = i50_opp_mean, 
            y = score_shots_opp_mean, 
            image = logo
        )
    ) +
    geom_vline(
        xintercept = defense_avg$i50_opp_mean, 
        colour = "firebrick1", 
        alpha = 0.8, 
        linetype = "dashed"
    ) +
    geom_hline(
        yintercept = defense_avg$score_shots_opp_mean, 
        colour = "firebrick1", 
        alpha = 0.8, 
        linetype = "dashed"
    ) +
    ggimage::geom_image() +
    scale_x_continuous(labels = abs) +
    scale_y_continuous(labels = abs) +
    labs(
        title = paste0("Team defensive indicators - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Oppositon inside 50s",
        y = "Oppostion scoring shots*",
        caption = "*Excludes out on the full
                    Lines represent league average
                    Created by: footycharts. Source: AFL website"
    ) +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10)
    )

ggsave(
    filename = paste0(
        round_name,
        "_team_defense",
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
