library(tidyverse)

wd <- getwd()

afl_fixture_all <- map_dfr(
    2014:2019, 
    ~ read_csv(
        paste0(wd, "/fixtures/afl_fixture_", .x, ".csv"),
        col_types = "cicDccciiiiii"
    )
)

afl_venues_all <- map_dfr(
    2014:2019, 
    ~ read_csv(
        paste0(wd, "/venues/afl_venues_", .x, ".csv"),
        col_types = "cccc"
    )
) %>% 
    nest(teams = team)
