library(dplyr)
library(gt)

afl_pred_actual_wins <- afl_elo %>%
    filter(
        season == current_season
        & !is.na(score_adjusted)
    ) %>% 
    select(
        team,
        margin,
        score_adjusted
    ) %>% 
    inner_join(
        team_logos,
        by = "team"
    ) %>% 
    rename(score_expected = score_adjusted) %>% 
    modelr::add_predictions(
        model = afl_win_prob_model,
        var = "pred_win_prob",
        type = "response"
    ) %>% 
    mutate(
        win_flag = if_else(
            margin > 0,
            1,
            0
        )
    ) %>% 
    group_by(
        team,
        logo
    ) %>% 
    summarise(
        actual_wins = sum(win_flag),
        pred_wins = sum(pred_win_prob) %>% 
            round(1)
    ) %>% 
    ungroup() %>% 
    mutate(
        wins_diff = round(
            actual_wins - pred_wins,
            1
        )
    ) %>% 
    arrange(
        desc(pred_wins)
    ) %>% 
    gt() %>% 
    cols_hide(columns = team) %>% 
    gtExtras::gt_img_rows(
        columns = logo,
        img_source = "local",
        height = 30
    ) %>% 
    tab_header(
        title = md("**Actual Wins vs. Model Wins**"),
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
        label = "Wins",
        columns = c(
            actual_wins,
            pred_wins
        )
    ) %>%
    cols_label(
        logo = md("Team<sup>1</sup>"),
        actual_wins = "Actual",
        pred_wins = md("Model<sup>2</sup>"),
        wins_diff = "Difference"
    ) %>%
    tab_footnote(
        footnote = md("<sup>1</sup>Sorted by: model wins<br><sup>2</sup>Model wins: the sum of the expected win % for each game, based on scoring shots"
        )
    ) %>% 
    data_color(
        columns = wins_diff,
        method = "numeric",
        palette = c("tomato", "white", "cornflowerblue")
    ) %>% 
    cols_align(align = "center") %>% 
    tab_style(
        style = "vertical-align:middle",
        locations = cells_column_labels()
    ) 

afl_pred_actual_wins %>% 
    gtsave(
        filename = "pred_actual_wins.png",
        path = round_path
    )