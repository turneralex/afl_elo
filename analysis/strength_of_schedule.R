library(dplyr)
library(ggplot2)

current_season <- "2023"
rounds_so_far <- 24

# run the elo model

source(
    here::here(
        "run", 
        "run.R"
    )
) 

afl_elo %>% 
    filter(
        season == current_season
    ) %>% 
    select(
        match_id,
        venue,
        location,
        team,
        hga_app,
        start_elo
    ) %>% 
    group_by(match_id) %>%
    mutate(
        opp_elo = coalesce(
            lead(start_elo, n = 1),
            lag(start_elo, n = 1)
        )
    ) %>% 
    ungroup() %>%
    mutate(
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
        opp_elo_hga = opp_elo - hga
    ) %>% 
    group_by(team) %>% 
    summarise(
        opp_elo_avg = mean(opp_elo),
        hga_home_avg = mean(
            if_else(
                hga_app == 1,
                hga,
                0
            )
        ),
        hga_away_avg = mean(
            if_else(
                hga_app == -1,
                hga,
                0
            )
        )
    ) %>% 
    mutate(
        opp_elo_diff = 1500 - opp_elo_avg,
        hga_diff = (1500 + hga_home_avg + hga_away_avg) - 1500,
        opp_elo_hga_diff = (1500 + hga_home_avg + hga_away_avg) - opp_elo_avg,
        # score_expected = 1 / (1 + 10^(-hga_diff / 400))
        # score_expected = 1 / (1 + 10^(-opp_elo_diff / 400))
        score_expected = 1 / (1 + 10^(-opp_elo_hga_diff / 400))
    ) %>% 
    modelr::add_predictions(
        model = afl_margin_model,
        var = "pred_margin",
        type = "response"
    ) %>% 
    arrange(
        desc(pred_margin)
    ) %>% 
    mutate(
        elo_position_avg = row_number(),
        elo_position_team_avg = paste0(
            elo_position_avg,
            ": ",
            team
        )
    ) %>% 
    ggplot(
        aes(
            x = forcats::fct_reorder(
                .f = elo_position_team_avg, 
                .x = -elo_position_avg
            ), 
            y = pred_margin, 
            fill = pred_margin
        )
    ) +
    geom_col(colour = "black") +
    geom_text(
        aes(
            forcats::fct_reorder(
                .f = elo_position_team_avg, 
                .x = elo_position_avg
            ), 
            y = pred_margin + if_else(pred_margin > 0, 0.25, -0.25),
            label = round(pred_margin, 1)
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_gradient(low = "firebrick1", high = "seagreen3") +
    labs(
        title = "Strength of schedule - based on opponent team rating & venue",
        subtitle = paste(
            "Season:", 
            current_season
        ),
        x = "Team & rank",
        y = "Average points gained per match from schedule",
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

