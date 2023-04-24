library(dplyr)

# current season & round

current_season <- "2023"
rounds_so_far <- 5
finals_so_far <- c(
    # "Finals Week 1"
    # "Semi Finals", 
    # "Preliminary Finals", 
    # "Grand Final"
)
round_next <- rounds_so_far + 1
round_name <- paste("Round", round_next)

# create folder for charts

dir.create(
    path = here::here(
        "files", 
        "charts",
        paste0(
            current_season,
            "_",
            round_name
        )
    )
)

# run the elo model

source(
    here::here(
        "run", 
        "run.R"
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

# correct tips so far

afl_fixture_all %>% 
    filter(
        season == current_season
        & round %in% paste("Round", 1:rounds_so_far)
    ) %>% 
    select(-match_id) %>% 
    inner_join(
        afl_elo,
        by = c("season", "round", "home_team" = "team")
    ) %>% 
    mutate(
        correct_tip = if_else(
            (score_expected >= 0.5 & home_score - away_score >= 0)
            | (score_expected <= 0.5 & home_score - away_score <= 0),
            1,
            0
        )
    ) %>% 
    pull(correct_tip) %>% 
    sum()
