current_season <- 2022
rounds_so_far <- 13
finals_so_far <- c(
    # "Finals Week 1"
    # "Semi Finals", 
    # "Preliminary Finals", 
    # "Grand Final"
)
round_next <- rounds_so_far + 1
round_name <- paste("Round", round_next)

dir.create(
    paste0(
        here::here("files/charts/"),
        current_season,
        "_",
        round_name
    )
)

source(here::here("run/update.R"))
source(here::here("analysis/matchups.R"))
source(here::here("analysis/standard_outputs.R"))

# correct tips so far

correct_tips <- afl_fixture_all %>% 
    filter(season == current_season, 
           round %in% paste("Round", 1:rounds_so_far)) %>% 
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

correct_tips
