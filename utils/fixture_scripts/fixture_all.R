library(dplyr)

if (!exists("start_season")) {
    
    warning(
        paste(
            "start_season not found, using default current_season:",
            current_season
        )
    )
    
    start_season <- current_season
    
} else if (start_season > current_season) {
    
    warning(
        paste(
            "start_season more recent than current_season, using default current_season:",
            current_season
        )
    )
    
    start_season <- current_season
    
}

seasons_include <- as.character(
    seq(
        from = as.integer(start_season), 
        to = as.integer(current_season), 
        by = 1
    )
)

# read in fixtures

afl_fixture_all <- bind_rows(
    readr::read_csv(
        here::here(
            "files",
            "fixtures",
            "afl_fixture_history.csv"
        ),
        col_types = "ciicciDccciiiiii"
    ),
    readr::read_csv(
        here::here(
            "files",
            "fixtures",
            paste0(
                "afl_fixture_",
                current_season, 
                ".csv"
            )
        ),
        col_types = "ciicciDccciiiiii"
    )
) %>% 
    filter(
        season %in% seasons_include
    ) %>% 
    mutate(
        match_id = 1:nrow(.)
    )

# available seasons check

message("seasons in afl_fixture_all:")

print(
    afl_fixture_all %>% 
        distinct(season) %>% 
        pull()
)

# read in venues 

afl_venues_history <- readr::read_csv(
    here::here(
        "files",
        "venues",
        "afl_venues_history.csv"
    ),
    col_types = "ccc"
) %>% 
    tidyr::nest(
        data_teams = c(team, total_flag, current_flag)
    )

afl_venues_current <- readr::read_csv(
    here::here(
        "files",
        "venues",
        paste0(
            "afl_venues_",
            current_season,
            ".csv"
        )
    ),
    col_types = "ccc"
) %>% 
    tidyr::nest(teams = team)
