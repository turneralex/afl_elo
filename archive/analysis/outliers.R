source(here::here("fixture scripts/fixture_all.R"))
source(here::here("elo update/functions_general.R"))

library(tidyverse)

elo_par <- read_csv("./new elo/elo_par.csv") %>% 
    deframe() 

afl_elo <- afl_fixture_all %>%
    convert_elo_df() %>% 
    elo_run(k = elo_par["k"], hga = elo_par[3:10], regress = elo_par["regress"])

# worst performances vs. expected 
worst_vs_expected_df <- afl_elo %>% 
    mutate(diff_score = score_adjusted - score_expected) %>% 
    arrange(diff_score) %>% 
    slice(1:10) %>% 
    mutate(
        group_rank = 1:nrow(.),
        type = "Worst Performances vs. Expected"
    )

worst_vs_expected_df

# upsets
upsets_df <- afl_elo %>% 
    filter(score_adjusted < 0.5,
           score_expected > 0.5) %>% 
    arrange(desc(score_expected)) %>% 
    slice(1:10) %>% 
    mutate(
        group_rank = 1:nrow(.),
        type = "Biggest Upsets"
    )

upsets_df

# biggest wins
biggest_wins_df <- afl_elo %>% 
    arrange(desc(score_adjusted)) %>% 
    slice(1:10) %>% 
    mutate(
        group_rank = 1:nrow(.),
        type = "Biggest Wins"
    )

biggest_wins_df
    
worst_vs_expected_df %>% 
    inner_join(upsets_df, by = "match_id") %>%
    select(match_id) 

all_outliers_df <- bind_rows(
    biggest_wins_df,
    upsets_df,
    worst_vs_expected_df
) %>% 
    left_join(
        afl_fixture_all %>% 
            select(match_id, home_team, away_team, home_score, away_score), 
        by = "match_id"
    ) %>% 
    arrange(score_expected) %>% 
    mutate(
        rank = 1:nrow(.),
        type_all = if_else(match_id == 905, "Worst Performances vs. Expected & Biggest Upsets", type),
        info = paste(season, round, venue, paste(home_team, away_team, sep = "-"), paste(home_score, away_score, sep = "-"), sep = ", ")
    ) %>% 
    pivot_longer(
        cols = c("score_adjusted", "score_expected"),
        names_to = "score_type",
        values_to = "value"
    ) %>% 
    mutate(
        score_type = if_else(score_type == "score_expected", "Expected Score", "Actual Adjusted Score")
    ) 

all_outliers_df %>% 
    pull(type) %>% 
    unique() %>% 
    map(
        ~ all_outliers_df %>% 
            filter(type == .x) %>% 
            ggplot(aes(fct_reorder(info, -group_rank), value, shape = score_type, label = round(value, 2))) +
            geom_hline(yintercept = 0.5, alpha = 0.5) +
            geom_point(size = 5) +
            geom_label(colour = "black", hjust = -0.5) +
            scale_y_continuous(limits = c(0.15, 0.9)) +
            coord_flip() +
            labs(title = .x,
                 subtitle = "Seasons 2010 - 2019 (Inclusive)",
                 x = "Game Info",
                 y = "Value",
                 shape = "")
    )

all_outliers_df %>% 
    ggplot(aes(fct_reorder(info, rank), value, shape = score_type, colour = type_all, label = round(value, 2))) +
    geom_hline(yintercept = 0.5, alpha = 0.5) +
    geom_point(size = 5) +
    geom_label(colour = "black", hjust = -0.5) +
    scale_y_continuous(limits = c(0.15, 0.9)) +
    coord_flip() 
