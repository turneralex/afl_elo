library(dplyr)
library(ggplot2)

start_season <- "1990" # ensure consistency with optim.R
current_season <- "2025"

source(
    here::here(
        "run",
        "run.R"
    )
)

afl_elo %>% 
    glimpse()

afl_elo %>% 
    skimr::skim()

# which venues are in VIC (the default)

afl_venues_all %>% 
    filter(
        location == "VIC"
    ) %>% 
    distinct(venue)

# which venues teams were home at
# contains some stadiums only used before current_season

purrr::map(
    .x = afl_venues_all %>% 
        distinct(venue) %>% 
        pull() %>% 
        sort(),
    ~ afl_venues_all %>% 
        tidyr::unnest(cols = data_teams) %>% 
        filter(
            venue == .x
        ) %>% 
        distinct(venue, location, team)
)

# according to the model

purrr::map(
    .x = afl_elo %>% 
        distinct(team) %>% 
        pull() %>% 
        sort(),
    ~ afl_elo %>% 
        filter(
            team == .x
            & hga_app == 1
        ) %>% 
        distinct(team, venue, location)
)

# check potential errors

team_to_check <- "Collingwood"
venue_to_check <- "Sydney Showground Stadium"

afl_fixture_all %>% 
    filter(
        home_team == team_to_check
        & venue == venue_to_check
    )

# total games per season

afl_elo %>% 
    group_by(season) %>% 
    summarise(
        games = n() / 2
    ) %>% 
    arrange(
        desc(season)
    )

# max & min team ratings

afl_elo %>% 
    filter(
        !is.na(start_elo)
    ) %>% 
    select(
        season, 
        round, 
        team, 
        start_elo
    ) %>% 
    mutate(
        rating_min = min(start_elo),
        rating_max = max(start_elo)
    ) %>% 
    filter(
        start_elo == rating_min
        | start_elo == rating_max
    ) %>% 
    select(
        -rating_min,
        -rating_max
    )

# histogram of ratings

afl_elo %>% 
    filter(
        !is.na(start_elo)
    ) %>% 
    ggplot(aes(x = start_elo)) +
    geom_histogram(binwidth = 1) 

# min & max score adjusted & expected

afl_elo %>% 
    filter(
        !is.na(score_expected)
    ) %>% 
    select(
        season, 
        round, 
        team, 
        score_adjusted, 
        score_expected
    ) %>% 
    mutate(
        score_adjusted_min = min(score_adjusted),
        score_expected_min = min(score_expected),
        score_adjusted_max = max(score_adjusted),
        score_expected_max = max(score_expected)
    ) %>% 
    filter(
        score_adjusted == score_adjusted_min
        | score_expected == score_expected_min
        | score_adjusted == score_adjusted_max
        | score_expected == score_expected_max
    ) %>% 
    select(
        -score_adjusted_min, 
        -score_expected_min, 
        -score_adjusted_max, 
        -score_expected_max
    )

# histogram of score adjusted & expected

afl_elo %>% 
    filter(
        !is.na(score_adjusted)
    ) %>% 
    select(score_adjusted, score_expected) %>% 
    tidyr::pivot_longer(
        cols = everything()
    ) %>% 
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 0.01) +
    facet_grid(name ~ .)

