# new season checklist:
# 1. run optim.R 
# 2. create current season fixture & check mapping functions are up to date
# 3. run below script
# 4. run utils/qa_checks.R after running the model (run.R)

library(dplyr)

# current season & round

start_season <- "2012" # ensure consistency with optim.R
current_season <- "2024"
rounds_so_far <- 0 # set as -1 for pre-season
round_name <- "Round 0" # name of round just completed
first_round <- F

# create folder for charts

season_path <- here::here(
    "files", 
    "charts",
    current_season
)

dir.create(path = season_path)

round_path <- paste(
    season_path,
    round_name,
    sep = "/"
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

if (first_round) {
    
    # first round predictions
    
    source(
        here::here(
            "analysis", 
            "first_round_predictions.R"
        )
    ) 
    
} else {
    
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
    
}

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
