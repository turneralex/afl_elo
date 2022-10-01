library(tidyverse)

afl_fixture_all <- map_dfr(
    2010:2022, 
    ~ read_csv(
        paste0(
            here::here(), 
            "/files/fixtures/afl_fixture_", 
            .x, 
            ".csv"
        ),
        col_types = "cicDccciiiiii"
    )
) %>% 
    mutate(
        match_id = 1:nrow(.),
        home_team = case_when(
            home_team == "Greater Western Sydney" ~ "GWS",
            home_team == "Brisbane Lions"         ~ "Brisbane",
            T                                     ~ home_team
        ),
        away_team = case_when(
            away_team == "Greater Western Sydney" ~ "GWS",
            away_team == "Brisbane Lions"         ~ "Brisbane",
            T                                     ~ away_team
        )
    )

afl_venues_all <- map_dfr(
    2010:2022, 
    ~ read_csv(
        paste0(
            here::here(), 
            "/files/venues/afl_venues_", 
            .x, 
            ".csv"
        ),
        col_types = "cccc"
    )
) %>% 
    mutate(
        team = case_when(
            team == "Greater Western Sydney" ~ "GWS",
            team == "Brisbane Lions"         ~ "Brisbane",
            T                                ~ team
        )
    ) %>% 
    nest(teams = team)
