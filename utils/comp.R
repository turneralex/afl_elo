source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)
source(
    here::here(
        "utils", 
        "fixture scripts/fixture_all.R"
    )
)

library(dplyr)
library(ggplot2)

comp_par_file_name <- "elo_par_score_shots"
focus_seasons <- c("2021", "2022")

# read in parameters

elo_par <- readr::read_csv(
    here::here(
        "files", 
        "params",
        "archive", # remove if not in archive
        paste0(
            comp_par_file_name,
            ".csv"
        )
    )
) %>% 
    tibble::deframe() 

# run elo model

afl_elo <- afl_fixture_all %>% 
    filter(
        season %in% focus_seasons
    ) %>% 
    left_join(
        afl_venues_all %>% 
            select(venue, location) %>% 
            distinct(),
        by = "venue"
    ) %>% 
    mutate(
        match_id =  1:nrow(.),
        # share of scoring shots
        home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
        hga_app = purrr::pmap_int(
            .l = list(season, venue, home_team, away_team), 
            .f = is_home, 
            data_venues = afl_venues_all, 
            data_fixture = afl_fixture_all
        )
    ) %>% 
    select(
        season, 
        round, 
        match_id, 
        venue, 
        location, 
        home_team, 
        away_team, 
        home_score_adjusted, 
        hga_app
    ) %>% 
    tidyr::pivot_longer(
        cols = c("home_team", "away_team"), 
        names_to = c("home_away", "temp"), 
        names_sep = "_",
        values_to = "team"
    ) %>% 
    select(
        season, 
        round, 
        match_id, 
        venue, 
        location, 
        team, 
        home_away, 
        hga_app, 
        home_score_adjusted
    ) %>% 
    mutate(
        score_adjusted = if_else(home_away == "away", 1 - home_score_adjusted, home_score_adjusted),
        score_expected = numeric(nrow(.)),
        hga_app = if_else(home_away == "away", as.integer(hga_app * -1), hga_app),
        start_elo = 1500,
        new_elo = 1500
    ) %>% 
    select(
        -home_away, 
        -home_score_adjusted
    ) %>% 
    elo_run(
        k = elo_par["k"], 
        hga = elo_par[3:10], 
        regress = elo_par["regress"]
    )

# correct tips

afl_fixture_all %>% 
    filter(
        season %in% focus_seasons
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
