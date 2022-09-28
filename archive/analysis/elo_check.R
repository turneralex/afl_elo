source(here::here("fixture scripts/fixture_all.R"))
source(here::here("elo update/functions_general.R"))

library(tidyverse)
library(ggpubr)

elo_par <- read_csv("./new elo/elo_par.csv") %>% 
    deframe() 

elo_par[3:10] %>% 
    enframe(name = "location", value = "hga") %>% 
    mutate(
        location = str_replace(location, "hga_", "") %>% 
            toupper()
    ) %>% 
    ggplot(aes(fct_reorder(location, hga), hga, label = round(hga, 1))) +
    geom_col() +
    geom_text(vjust = -1) +
    scale_y_continuous(limits = c(0, max(elo_par[3:10]) + 5)) +
    labs(title = "Home Ground Advantage by Location",
         subtitle = "Season 2010 - 2019 Inclusive",
         x = "Location",
         y = "Home Ground Advantage Value") 

afl_elo <- afl_fixture_all %>%
    convert_elo_df() %>% 
    elo_run(k = elo_par["k"], hga = elo_par[3:10], regress = elo_par["regress"])

afl_elo

afl_elo %>% 
    mutate(
        win_loss = if_else(score_adjusted > 0.5, 1, 0) %>% 
            factor()
    ) %>% 
    filter(win_loss == 1) %>% 
    group_by(season) %>% 
    summarise(score_adjusted = mean(score_adjusted),
              score_expected = mean(score_expected)) %>% 
    pivot_longer(
        cols = c("score_adjusted", "score_expected"),
        names_to = "score_type",
        values_to = "mean"
    ) %>% 
    mutate(
        score_type = if_else(score_type == "score_expected", "Expected Score", "Actual Adjusted Score")
    ) %>% 
    ggplot(aes(season, mean, colour = score_type, group = score_type)) +
    geom_point(size = 6) +
    scale_colour_brewer(palette = "Set2") +
    scale_y_continuous(limits = c(0.4, 0.7)) +
    labs(title = "Mean Expected & Actual Adjusted Scores",
         subtitle = "Winning Teams",
         x = "Season",
         y = "Mean",
         colour = "",
         caption = "Adjusted Score: Winner Share of Total Points") +
    theme(legend.position = "bottom")

mean_sd_score <- afl_elo  %>% 
    group_by(match_id) %>% 
    slice(1) %>% 
    pivot_longer(
        cols = c("score_adjusted", "score_expected"),
        names_to = "score_type",
        values_to = "value"
    ) %>% 
    mutate(
        score_type = if_else(score_type == "score_expected", "Expected Score", "Actual Adjusted Score")
    ) %>% 
    group_by(season, score_type) %>% 
    summarise(mean = mean(value),
              sd = sd(value)) %>% 
    ungroup()

mean_sd_score

afl_elo %>% 
    group_by(match_id) %>% 
    slice(1) %>% 
    pivot_longer(
        cols = c("score_adjusted", "score_expected"),
        names_to = "score_type",
        values_to = "value"
    ) %>% 
    mutate(
        score_type = if_else(score_type == "score_expected", "Expected Score", "Actual Adjusted Score")
    ) %>% 
    ggplot(aes(value)) +
    geom_histogram(binwidth = 0.01) +
    geom_vline(data = mean_sd_score,
               aes(xintercept = mean),
               colour = "firebrick1",
               size = 1,
               alpha = 0.6) +
    facet_grid(season ~  score_type) +
    labs(title = "Mean Expected & Actual Adjusted Scores Distributions",
         subtitle = "Home Teams",
         x = "Value",
         y = "Count") 

afl_elo %>% 
    group_by(match_id) %>%
    slice(1) %>%
    pivot_longer(
        cols = c("score_adjusted", "score_expected"),
        names_to = "score_type",
        values_to = "value"
    ) %>%
    mutate(
        score_type = if_else(score_type == "score_expected", "Expected Score", "Actual Adjusted Score")
    ) %>% 
    ggplot(aes(season, value)) +
    geom_jitter(aes(colour = season), size = 4, alpha = 0.6, width = 0.25) +
    geom_point(data = mean_sd_score,
               aes(season, mean),
               size = 6) +
    geom_errorbar(data = mean_sd_score,
                  aes(season, mean, ymin = mean - sd, ymax = mean + sd),
                  width = 0.2,
                  size = 2) +
    facet_grid(score_type ~ ., scales = "free_y") +
    scale_colour_manual(values = randomcoloR::distinctColorPalette(10)) +
    labs(title = "Mean Expected & Actual Adjusted Scores Distributions inc. Error Bars",
         subtitle = "Home Teams",
         x = "Season",
         y = "Value",
         caption = "Error Bars: 1 Standard Deviation") +
    theme(legend.position = "none")

afl_elo %>%
    group_by(match_id) %>% 
    slice(1) %>%
    ggplot(aes(score_expected, score_adjusted)) +
    geom_point(size = 3, alpha = 0.5) +
    facet_wrap(. ~ season) +
    stat_cor(label.x = 0.6, 
             label.y = 0.25,
             p.accuracy = 0.01) +
    labs(title = "Mean Expected & Actual Adjusted Scores Distributions inc. Error Bars",
        subtitle = "Home Teams",
        x = "Expected Score",
        y = "Actual Adjusted Score",
        caption = "Error Bars: 1 Standard Deviation") 

afl_elo %>%
    group_by(match_id) %>% 
    slice(1) %>%
    mutate(
        tip_correct = if_else(
            (score_expected - 0.5) * (score_adjusted - 0.5) > 0, "Correct", "Incorrect"
        ) %>% 
            factor()
    ) %>% 
    ggplot(aes(score_expected, score_adjusted, colour = tip_correct)) +
    geom_point(size = 3, alpha = 0.5) +
    facet_wrap(. ~ season) +
    scale_colour_brewer(palette = "Dark2") +
    labs(title = "Mean Expected & Actual Adjusted Scores Distributions inc. Error Bars",
         subtitle = "Home Teams",
         x = "Expected Score",
         y = "Actual Adjusted Score",
         colour = "Tip Result",
         caption = "Error Bars: 1 Standard Deviation") +
    theme(legend.position = "bottom")
