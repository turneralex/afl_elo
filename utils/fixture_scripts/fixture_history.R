library(dplyr)
source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)

season_first <- 2011
season_last <- 2023

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

# available results check

if (exists("rounds_so_far")) {
    
    message(
        paste0(
            "results available for round ",
            rounds_so_far,
            ":"
        )
    )
    
    print(
        afl_fixture %>% 
            filter(
                round_number == rounds_so_far
            ) %>% 
            select(
                home_team,
                away_team
            )
    )
    
}

# add location

afl_venues <- afl_fixture %>% 
    distinct(venue) %>% 
    mutate(
        location = venue_location(venue)
    )

# create table of teams & home venues

venues_teams <- purrr::map(
    .x = afl_fixture %>% 
        distinct(venue) %>% 
        pull(),
    ~ afl_fixture %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    purrr::set_names(
        nm = afl_fixture %>% 
            distinct(venue) %>% 
            pull()
    ) %>% 
    tibble::enframe(
        name = "venue", 
        value = "teams"
    ) %>% 
    mutate(venue = venue)

# add location

afl_venues <- afl_venues %>% 
    left_join(
        venues_teams, 
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
    tidyr::unnest(cols = teams) %>%
    readr::write_csv(
        here::here(
            "files", 
            "venues", 
            "afl_venues_history.csv"
        )
    )
