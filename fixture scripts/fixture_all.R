library(tidyverse)

afl_fixture_all <- map_dfr(
    2010:2019, 
    ~ read_csv(
        paste0(
            here::here(), 
            "/fixtures/afl_fixture_", 
            .x, 
            ".csv"
        ),
        col_types = "cicDccciiiiii"
    )
)

afl_venues_all <- map_dfr(
    2010:2020, 
    ~ read_csv(
        paste0(
            here::here(), 
            "/venues/afl_venues_", 
            .x, 
            ".csv"
        ),
        col_types = "cccc"
    )
) %>% 
    nest(teams = team)

read_csv(
    paste0(
        here::here(), 
        "/venues/afl_venues_", 
        "2020", 
        ".csv"
    ),
    col_types = "cccc"
)
