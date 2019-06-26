library(tidyverse)

afl_fixture_all <- map_dfr(
    2014:2019, 
    ~ read_csv(paste0("./afl_fixture_", .x, ".csv"))
)

afl_fixture_all

afl_venues_all <- map_dfr(
    2014:2019, 
    ~ read_csv(paste0("./afl_venues_", .x, ".csv"))
) %>% 
    nest(team)

afl_venues_all
