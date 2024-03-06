library(dplyr)

seasons_include <- seq(
    from = as.integer(start_season), 
    to = as.integer(current_season), 
    by = 1
)

# read in fixtures

afl_fixture_all <- purrr::map_dfr(
    .x = seasons_include, 
    ~ readr::read_csv(
        here::here(
            "files",
            "fixtures",
            paste0(
                "afl_fixture_",
                .x, 
                ".csv"
            )
        ),
        col_types = "cicDccciiiiii"
    )
) %>% 
    mutate(
        match_id = 1:nrow(.),
        home_team = change_team_name(home_team),
        away_team = change_team_name(away_team)
    )

# available seasons check

message("seasons in afl_fixture_all:")

print(
    afl_fixture_all %>% 
        distinct(season) %>% 
        pull()
)

# read in venues 

afl_venues_all <- purrr::map_dfr(
    .x = seasons_include, 
    ~ readr::read_csv(
        here::here(
            "files",
            "venues",
            paste0(
                "afl_venues_", 
                .x, 
                ".csv"
            )
        ),
        col_types = "cccc"
    )
) %>% 
    mutate(
        team = change_team_name(team)
    ) %>% 
    tidyr::nest(teams = team)

# available seasons check

message("seasons in afl_venues_all:")

print(
    afl_venues_all %>% 
        distinct(year) %>% 
        pull()
)
