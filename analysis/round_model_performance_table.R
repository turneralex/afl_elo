library(dplyr)
library(gt)

# team logos

tick_cross <- tibble::tibble(
    correct_tip = c(0, 1),
    tick_cross = c("cross", 'tick')
) %>% 
    mutate(
        tick_cross_image = here::here(
            "files",
            "chart_pics",
            paste0(
                tick_cross %>% 
                    stringr::str_remove_all(pattern = " ") %>% 
                    tolower(),
                ".png"
            )
        )
    ) %>% 
    select(-tick_cross)

afl_round_model_performance <- afl_elo_pred_base %>% 
    inner_join(
        tick_cross,
        by = "correct_tip"
    ) %>% 
    inner_join(
        team_logos %>% 
            rename(home_logo = logo),
        by = c(
            "home_team" = "team"
        )
    ) %>% 
    inner_join(
        team_logos %>% 
            rename(away_logo = logo),
        by = c(
            "away_team" = "team"
        )
    ) %>% 
    mutate(
        # do this before filter
        mae = mean(
            abs(home_pred_margin - home_margin),
            na.rm = T
        ),
        margin_diff_colour = abs(home_pred_margin - home_margin) - mae
    ) %>% 
    filter(
        round_number == rounds_so_far
    ) %>% 
    mutate(
        margin_diff = abs(
            round(home_pred_margin - home_margin)
        ),
        home_pred_margin = round(home_pred_margin),
        bits = if_else(
            correct_tip == 1,
            1 + log2(pred_winner_win_prob),
            1 + log2(1 - pred_winner_win_prob)
        ) %>% 
            round(2),
        home_winner = if_else(
            home_margin > 0,
            1,
            0
        ),
        away_winner = if_else(
            home_margin < 0,
            1,
            0
        )
    ) %>% 
    select(
        home_logo,
        away_logo,
        home_winner,
        away_winner,
        home_margin,
        home_pred_margin,
        margin_diff,
        margin_diff_colour,
        tick_cross_image,
        bits
    ) %>% 
    gt() %>% 
    cols_hide(
        columns = c(
            home_winner,
            away_winner,
            margin_diff_colour
        )
    ) %>% 
    gtExtras::gt_img_rows(
        columns = home_logo,
        img_source = "local",
        height = 40
    ) %>%
    gtExtras::gt_img_rows(
        columns = away_logo,
        img_source = "local",
        height = 40
    ) %>%
    gtExtras::gt_img_rows(
        columns = tick_cross_image, 
        img_source = "local"
    ) %>%
    tab_header(
        title = md("**Model Report Card**"),
        subtitle = paste0(
            "*",
            round_concluded,
            ", ",
            current_season,
            "*"
        ) %>% 
            md()
    ) %>% 
    tab_source_note(
        source_note = "Created by: footycharts"
    ) %>% 
    tab_spanner(
        label = "Matchup",
        columns = c(
            home_logo, 
            away_logo
        )
    ) %>%
    tab_spanner(
        label = "Home team result vs. prediction",
        columns = c(
            home_margin,
            home_pred_margin,
            margin_diff,
            tick_cross_image,
            bits
        )
    ) %>% 
    tab_footnote(
        footnote = md("<sup>1</sup>More bits are gained through confident correct tips, but incorrect confident tips cost a lot of bits<br>Not many bits are gained or lost when predicted win probability is close to 50%<br>More info: https://probabilistic-footy.monash.edu/~footy/about.shtml"
        )
    ) %>% 
    cols_label(
        home_logo = "Home", 
        away_logo = "Away",
        home_margin = md("Actual<br>margin"),
        home_pred_margin = md("Predicted<br>margin"),
        margin_diff = "Difference",
        tick_cross_image = "Correct tip",
        bits = md("Bits<sup>1</sup>")
    ) %>% 
    data_color(
        columns = home_winner,
        target_columns = home_logo,
        fn = scales::col_factor(
            palette = c("white", "seagreen3"),
            domain = c(0, 1)
        )
    ) %>% 
    data_color(
        columns = away_winner,
        target_columns = away_logo,
        fn = scales::col_factor(
            palette = c("white", "seagreen3"),
            domain = c(0, 1)
        )
    ) %>% 
    data_color(
        columns = margin_diff_colour,
        target_columns = margin_diff,
        method = "numeric",
        palette = c("tomato", "white", "cornflowerblue"),
        reverse = T
    ) %>% 
    cols_align(align = "center") %>% 
    tab_style(
        style = "vertical-align:middle",
        locations = cells_column_labels()
    ) 

afl_round_model_performance %>% 
    gtsave(
        filename = "round_model_performance.png",
        path = round_path
    )
