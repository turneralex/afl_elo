library(tidyverse)

afl_fixture_all <- map_dfr(
    2010:2019, 
    ~ read_csv(
        paste0("./afl_fixture_", .x, ".csv"),
        col_types = "cicDcccii"
    )
)

afl_venues_all <- map_dfr(
    2010:2019, 
    ~ read_csv(
        paste0("./afl_venues_", .x, ".csv"),
        col_types = "cccc"
    )
) %>% 
    nest(team)
