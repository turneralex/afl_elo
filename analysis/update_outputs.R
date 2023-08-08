library(dplyr)

# current season & round

current_season <- "2023"
rounds_so_far <- 13
finals_so_far <- c(
    # "Finals Week 1"
    # "Semi Finals", 
    # "Preliminary Finals", 
    # "Grand Final"
)
round_next <- rounds_so_far + 1
round_name <- paste("Round", round_next)

# create folder for charts

round_path <- here::here(
    "files", 
    "charts",
    paste0(
        current_season,
        "_",
        round_name
    )
)

dir.create(path = round_path)

dir.create(
    path = paste(
        round_path,
        "matchups",
        sep = "/"
    )
)

# run the elo model

source(
    here::here(
        "run", 
        "run.R"
    )
) 

# prep matchup data

source(
    here::here(
        "analysis", 
        "matchups_data.R"
    )
) 

# create matchup charts

source(
    here::here(
        "analysis", 
        "matchups.R"
    )
) 

# create rank, trend & offensive / defensive charts

source(
    here::here(
        "analysis", 
        "standard_outputs.R"
    )
) 

# model performance

afl_elo_pred_base %>% 
    summarise(
        correct_tips = sum(correct_tip, na.rm = T),
        bits = sum(
            if_else(
                correct_tip == 1,
                1 + log2(pred_winner_win_prob),
                1 + log2(1 - pred_winner_win_prob)
            ),
            na.rm = T
        ),
        mae = mean(
            abs(pred_winner_margin - margin),
            na.rm = T
        )
    )
