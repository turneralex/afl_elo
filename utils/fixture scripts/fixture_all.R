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
    mutate(match_id = 1:nrow(.))

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
    nest(teams = team)
