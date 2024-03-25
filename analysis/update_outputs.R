# new season checklist:
# 1. run optim.R 
# 2. create current season fixture & check mapping functions are up to date
# 3. run below script
# 4. run utils/qa_checks.R after running the model (run.R)

library(dplyr)

# current season & round

start_season <- "2012" # ensure consistency with optim.R
# note: at the beginning of start_season, all team ratings will be set to the average of 1500
current_season <- "2024"
prev_season <- "2023"
rounds_so_far <- 2 # set as -1 for pre-season
round_name <- "Round 2" # name of round just completed
first_round <- F
compare_change_start_season <- T # if F, then changes is vs. 5 games ago by team
upload_squiggle <- F

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
            "matchups_charts.R"
        )
    ) 
    
}

# provide tips to squiggle

if (upload_squiggle) {
    
    afl_elo_pred %>%
        select(
            season,
            round,
            home_team,
            away_team,
            pred_winner,
            pred_winner_win_prob,
            pred_winner_margin
        ) %>%
        upload_tips_squiggle(drive_id = "1Aqkhb5uV-qU7b1Zm2k2KA6M05rpNDEFps1MwPaU3QaM")
    
}

# ratings rank & change data frame

source(
    here::here(
        "analysis", 
        "ratings_rank_change.R"
    )
) 

# upload rankings for squiggle

if (upload_squiggle) {
    
    googledrive::drive_auth(email = T)
    
    googlesheets4::gs4_auth(email = T)
    
    drive_id <- "1IsVkFjmjSUKfzvyuIUXjxRJB5I8uwf2xmE36nnpUDGM"
    
    power_rankings <- afl_elo_rank_change %>% 
        mutate(round = rounds_so_far) %>% 
        select(
            season,
            round,
            team,
            score = new_elo
        )
    
    power_rankings %>% 
        googlesheets4::sheet_write(
            ss = drive_id, 
            sheet = "power_rankings"
        )
    
}

# charts

source(
    here::here(
        "analysis", 
        "ratings_rank_change_charts.R"
    )
) 

# ratings trend

source(
    here::here(
        "analysis", 
        "ratings_trend_charts.R"
    )
) 

# ratings quadrants

source(
    here::here(
        "analysis", 
        "ratings_quadrants_charts.R"
    )
) 

# save charts

purrr::map2(
    .x = list(
        ratings_rank,
        ratings_change_prev_week,
        ratings_trend,
        ratings_quadrants
    ),
    .y = c(
        "rank",
        "change_prev_week",
        "trend", 
        "quadrants"
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
            current_season,
            round_name
        ),
        width = 25,
        height = 25,
        units = "cm"
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
