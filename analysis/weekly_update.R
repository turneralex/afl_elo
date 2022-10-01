# inputs
current_season <- 2022
rounds_so_far <- 18
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

# run elo

source(here::here("run/run.R"))

# tips

afl_elo %>% 
    filter(season == current_season, 
           round == paste("Round", rounds_so_far + 1)) %>% 
    group_by(match_id) %>%
    mutate(away_team = lead(team),
           away_elo = lead(start_elo)) %>% 
    slice(1) %>%
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
            T                 ~ hga_app * elo_par["hga_other"]
        ),
        home_team = team,
        home_elo = start_elo,
        elo_diff_hga = home_elo - away_elo + hga
    ) %>% 
    select(home_team,
           away_team,
           venue,
           hga,
           home_elo,
           away_elo,
           elo_diff_hga,
           score_expected)

# matchups

source(here::here("analysis/matchups.R"))

# trended & current elo ratings

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
