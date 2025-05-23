library(dplyr)
source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)

# read in fixture

afl_fixture <- fitzRoy::fetch_fixture_squiggle(season = current_season) %>%
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
        home_score = if_else(
            is.na(hgoals),
            NA_integer_,
            hscore,
        ),
        away_goals = agoals,
        away_behinds = abehinds,
        away_score = if_else(
            is.na(agoals),
            NA_integer_,
            ascore
        ),
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
                & !is.na(home_score)
            ) %>% 
            select(
                home_team,
                away_team
            )
    )
    
}

# add location

afl_venues <- afl_fixture %>% 
    filter(
        !is.na(home_team)
    ) %>% 
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
        filter(
            !is.na(home_team)
            & venue == .x
        ) %>% 
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
            paste0(
                "afl_fixture_",
                current_season,
                ".csv"
            )
        )
    )

afl_venues %>%
    tidyr::unnest(cols = teams) %>%
    readr::write_csv(
        here::here(
            "files", 
            "venues", 
            paste0(
                "afl_venues_",
                current_season,
                ".csv"
            )
        )
    )
