library(dplyr)
source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)

season_first <- 1990
season_last <- 2024

# read in fixture

afl_fixture <- purrr::map_df(
    .x = seq(
        from = season_first, 
        to = season_last, 
        by = 1
    ),
    .f = ~ fitzRoy::fetch_fixture_squiggle(season = .x) 
) %>% 
    arrange(date) %>% 
    transmute(
        season = as.character(year),
        match_id = row_number(),
        round_number = round,
        round = paste("Round", round),
        round_name = roundname,
        finals_flag = if_else(is_final > 0, 1, 0),
        date = stringr::str_sub(date, start = 1, end = 10) %>% 
            lubridate::as_date(),
        venue = change_venue_name(venue),
        home_team = change_team_name(hteam),
        away_team = change_team_name(ateam),
        home_goals = hgoals,
        home_behinds = hbehinds,
        home_score = hscore,
        away_goals = agoals,
        away_behinds = abehinds,
        away_score = ascore
    )

# add location

afl_venues <- afl_fixture %>% 
    distinct(venue) %>% 
    mutate(
        location = venue_location(venue)
    )

# create table of teams & home venues

venues_team_season <- purrr::map(
    .x = afl_fixture %>% 
        distinct(venue) %>% 
        pull() %>% 
        sort(),
    ~ afl_fixture %>% 
        filter(
            venue == .x
            & finals_flag == 0
        ) %>% 
        select(
            season,
            team = home_team
        ) %>% 
        group_by(team) %>% 
        mutate(
            total_flag = if_else(
                n() >= 10,
                1,
                0
            )
        ) %>% 
        distinct() %>% 
        mutate(
            current_flag = if_else(
                lag(season, n = 1, default = "0000") %>% as.integer() == (season %>% as.integer()) - 1,
                1,
                0
            )
        ) %>% 
        ungroup()
) %>% 
    purrr::set_names(
        nm = afl_fixture %>% 
            distinct(venue) %>% 
            pull() %>% 
            sort()
    )  %>% 
    tibble::enframe(
        name = "venue", 
        value = "data_teams"
    ) %>% 
    mutate(venue = venue) 

# add location

afl_venues <- afl_venues %>% 
    left_join(
        venues_team_season, 
        by = "venue"
    ) 

# write files

afl_fixture %>%
    readr::write_csv(
        here::here(
            "files", 
            "fixtures",
            "afl_fixture_history.csv"
        )
    )

afl_venues %>%
    tidyr::unnest(cols = data_teams) %>%
    readr::write_csv(
        here::here(
            "files", 
            "venues", 
            "afl_venues_history.csv"
        )
    )
