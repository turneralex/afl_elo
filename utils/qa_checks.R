source(here::here("run/update.R"))

afl_elo

afl_elo %>% 
    skimr::skim()

# which venues teams were home at

map(
    afl_venues_all$venue %>% 
        unique() %>% 
        sort(),
    ~ afl_venues_all %>% 
        unnest(cols = teams) %>% 
        filter(
            venue == .x
            & !(year %in% c(2020, 2021)) 
        ) %>% 
        distinct(venue, location, team)
)

# check potential errors

afl_venues_all %>% 
    unnest(cols = teams) %>% 
    filter(
        venue == "Perth Stadium"
        & team == "Gold Coast"
    )

afl_venues_all %>% 
    unnest(cols = teams) %>% 
    filter(
        venue == "Cazaly's Stadium"
        & team == "Richmond"
    )

# unique locations

afl_elo %>% 
    distinct(location) %>% 
    arrange(location)

# total games per season

afl_elo %>% 
    group_by(season) %>% 
    summarise(
        games = n() / 2
    )

# team games per season

afl_elo %>% 
    group_by(season, team) %>% 
    summarise(
        games = n() 
    ) %>% 
    ungroup() %>% 
    distinct(season, games)

# max & min team ratings

afl_elo %>% 
    select(season, round, team, start_elo) %>% 
    mutate(
        rating_min = min(start_elo),
        rating_max = max(start_elo)
    ) %>% 
    filter(
        start_elo == rating_min
        | start_elo == rating_max
    )

# histogram of ratings

afl_elo %>% 
    ggplot(aes(start_elo)) +
    geom_histogram(binwidth = 1) 

# min & max score adjusted & expected

afl_elo %>% 
    select(season, round, team, score_adjusted, score_expected) %>% 
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
    select(-score_adjusted_min, -score_expected_min, -score_adjusted_max, -score_expected_max)

# histogram of score adjusted & expected

afl_elo %>% 
    select(score_adjusted, score_expected) %>% 
    pivot_longer(
        cols = everything()
    ) %>% 
    ggplot(aes(value)) +
    geom_histogram(binwidth = 0.01) +
    facet_grid(name ~ .)