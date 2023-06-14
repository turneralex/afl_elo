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
        title = "Team elo rating by round",
        subtitle = paste("Season:", current_season),
        x = "Round",
        y = "Team elo rating*",
        caption = "*Average team rating: 1500
                    Created by: footycharts"
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

# current elo position & change

afl_elo_rank_change <- afl_elo %>% 
    filter(
        !is.na(start_elo)
        & !is.na(new_elo)
    ) %>%
    group_by(team) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    select(round, team, start_elo, new_elo) %>% 
    mutate(
        plus_minus_avg = new_elo - 1500,
        plus_minus_prev = new_elo - start_elo
    ) %>% 
    arrange(-plus_minus_prev) %>% 
    mutate(
        elo_position_prev = row_number(),
        elo_position_team_prev = paste0(
            elo_position_prev,
            ": ",
            team
        )
    ) %>% 
    arrange(-plus_minus_avg) %>% 
    mutate(
        elo_position_avg = row_number(),
        elo_position_team_avg = paste0(
            elo_position_avg,
            ": ",
            team
        )
    ) 

# current elo ratings

rank_elo <- afl_elo_rank_change %>% 
    ggplot(
        aes(
            x = forcats::fct_reorder(
                .f = elo_position_team_avg, 
                .x = -elo_position_avg
            ), 
            y = plus_minus_avg, 
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
            y = plus_minus_avg + if_else(new_elo > 1500, 6, -6),
            label = round(new_elo)
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_gradient(low = "white", high = "seagreen3") +
    labs(
        title = paste0("Team elo ratings - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Team & rank",
        y = "Elo rating*",
        caption = "*Average team rating: 1500
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

change_elo <- afl_elo_rank_change %>% 
    filter(
        round == paste("Round", rounds_so_far)
    ) %>% 
    mutate(
        improve_flag = if_else(
            plus_minus_prev > 0,
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
            y = plus_minus_prev, 
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
            y = plus_minus_prev + if_else(plus_minus_prev > 0, 0.8, -0.8),
            label = if_else(
                round(plus_minus_prev, 1) > 0,
                paste0(
                    "+",
                    round(plus_minus_prev, 1)
                ),
                round(plus_minus_prev, 1) %>% 
                    as.character()
            )
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_manual(values = c("firebrick1", "springgreen4")) +
    labs(
        title = paste0("Team elo ratings change vs. previous week - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Team",
        y = "Elo rating change",
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

# save rank & trend

purrr::map2(
    .x = list(
        trended_elo,
        rank_elo,
        change_elo
    ),
    .y = c(
        "trend", 
        "rank",
        "change"
    ),
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

team_logos <- team_stats_base_adj %>% 
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

team_stats_base_adj %>% 
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
    ggimage::geom_image() +
    labs(
        title = paste0("Schedule-adjusted team offensive indicators - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Inside 50s",
        y = "Scoring shots*",
        caption = "*Excludes out on the full
                    Lines represent league average
                    Created by: footycharts. Source: AFL website"
    ) +
    theme_bw() +
    theme(
        axis.text = element_blank(),
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

team_stats_base_adj %>% 
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
    geom_label(
        aes(
            x = 0,
            y = team_stats_base_adj %>% 
                pull(score_shots_opp_mean) %>% 
                min(),
            label = "Better >"
        ),
        size = 5
    ) +
    ggtext::geom_richtext(
        aes(
            x = team_stats_base_adj %>% 
                pull(i50_opp_mean) %>% 
                min(),
            y = 0,
            label = "Better >"
        ),
        size = 5,
        angle = 90
    ) +
    ggimage::geom_image() +
    scale_x_continuous(labels = abs) +
    scale_y_continuous(labels = abs) +
    labs(
        title = paste0("Schedule-adjusted team defensive indicators - Round ", rounds_so_far),
        subtitle = paste("Season:", current_season),
        x = "Oppositon inside 50s",
        y = "Oppostion scoring shots*",
        caption = "*Excludes out on the full
                    Lines represent league average
                    Created by: footycharts. Source: AFL website"
    ) +
    theme_bw() +
    theme(
        axis.text = element_blank(),
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
