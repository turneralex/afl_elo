library(dplyr)
library(ggplot2)

start_season <- "2012" # optional
# note: at the beginning of start_season, all team ratings will be set to the average of 1500
current_season <- "2024"
rounds_so_far <- 23 # rounds to use in analysis

# run the elo model

source(
    here::here(
        "run", 
        "run.R"
    )
) 

# create base table

# hga: venue effect only
# opp: opponent effect only
# both: both effects
sos_type <- "both"

afl_sos <- afl_elo %>% 
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
            location == "ACT" ~ hga_app * elo_par["hga_act"],
            T                 ~ 0
        ),
        opp_elo_hga = opp_elo - hga
    ) %>% 
    group_by(team) %>% 
    summarise(
        opp_elo_avg = mean(opp_elo, na.rm = T),
        hga_home_avg = mean(
            if_else(
                hga_app == 1,
                hga,
                0
            ),
            na.rm = T
        ),
        hga_away_avg = mean(
            if_else(
                hga_app == -1,
                hga,
                0
            ),
            na.rm = T
        )
    ) %>% 
    mutate(
        opp_elo_diff = 1500 - opp_elo_avg,
        hga_diff = (1500 + hga_home_avg + hga_away_avg) - 1500,
        opp_elo_hga_diff = (1500 + hga_home_avg + hga_away_avg) - opp_elo_avg,
        score_expected = if (sos_type == "opp") {
            1 / (1 + 10^(-opp_elo_diff / 400))
        } else if (sos_type == "hga") {
            1 / (1 + 10^(-hga_diff / 400))
        } else if (sos_type == "both") {
            1 / (1 + 10^(-opp_elo_hga_diff / 400))
        }
    ) %>% 
    modelr::add_predictions(
        model = afl_margin_model,
        var = "pred_margin",
        type = "response"
    ) %>% 
    arrange(pred_margin) %>% 
    mutate(
        elo_position_avg = row_number(),
        elo_position_team_avg = paste0(
            elo_position_avg,
            ": ",
            team
        )
    ) 

# chart

plot_title <- if (sos_type == "hga") {
    "Strength of schedule - based on HGA only"
} else if (sos_type == "opp") {
    "Strength of schedule so far - based on opponent team rating only"
} else if (sos_type == "both") {
    "Strength of schedule so far - based on opponent team rating & HGA"
}
# plot_subtitle <- paste0(
#     "Rounds 1-",
#     rounds_so_far,
#     " inclusive, ",
#     current_season
# )
plot_subtitle <- "2024 regular season"
x_axis_title <- "Team & rank"
y_axis_title <- "Average points gained or lost per match from schedule"
plot_caption <- "Created by: footycharts"

afl_sos %>% 
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
            y = pred_margin + if_else(pred_margin > 0, 0.4, -0.4),
            label = round(pred_margin, 1)
        ),
        size = 5
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_gradientn(
        colors = c(
            "firebrick1",
            "white",
            "cornflowerblue"
        )
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
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20), 
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        legend.position = "none"
    ) 

