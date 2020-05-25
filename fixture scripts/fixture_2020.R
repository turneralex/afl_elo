library(tidyverse)

afl_fixture_2020 <- fitzRoy::get_fixture(season = 2020) %>% 
    mutate(
        Date = lubridate::as_date(Date),
        Season = as.character(Season),
        match_id = Season.Game,
        Round = paste("Round", Round),
        Home.Team = case_when(
            Home.Team == "Footscray" ~ "Western Bulldogs",
            Home.Team == "GWS"       ~ "Greater Western Sydney",
            T                        ~ Home.Team
        ),
        Away.Team = case_when(
            Away.Team == "Footscray" ~ "Western Bulldogs",
            Away.Team == "GWS"       ~ "Greater Western Sydney",
            T                        ~ Away.Team
        ),
        home_score = NA,
        away_score = NA,
        home_goals = NA,
        away_goals = NA,
        home_behinds = NA,
        away_behinds = NA
    ) %>% 
    select(Season, match_id, Round, Date, Venue, Home.Team, Away.Team, home_score:away_behinds)

colnames(afl_fixture_2020) <- colnames(afl_fixture_2020) %>% 
    tolower() %>% 
    str_replace("\\.", "_")

afl_fixture_2020 <- afl_fixture_2020 %>% 
    mutate(
        venue = case_when(
            venue == "M.C.G."            ~ "MCG",
            venue == "Docklands"         ~ "Docklands Stadium",
            venue == "Sydney Showground" ~ "Sydney Showground Stadium",
            venue == "S.C.G."            ~ "SCG",
            venue == "Carrara"           ~ "Carrara Stadium",
            T                            ~ venue
        ) 
    ) 

afl_venues_2020 <- tibble(venue = afl_fixture_2020$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Adelaide Oval"                              ~ "SA",
            venue == "Gabba" | venue == "Carrara Stadium"         ~ "QLD",
            venue == "Manuka Oval"                                ~ "ACT",
            venue == "Perth Stadium"                              ~ "WA",
            venue == "SCG" | venue == "Sydney Showground Stadium" ~ "NSW",
            venue == "Bellerive Oval" | venue == "York Park"      ~ "TAS",
            venue == "Kardinia Park"                              ~ "GEE",
            venue == "Jiangwan Stadium"                           ~ "Other",
            venue == "Traeger Park" | venue == "Marrara Oval"     ~ "NT",
            T                                                     ~ "VIC"
        ) 
    )

venues_teams_2020 <- map(
    unique(afl_fixture_2020$venue),
    ~ afl_fixture_2020 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2020$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2020 <- afl_venues_2020 %>% 
    left_join(venues_teams_2020, by = "venue") %>% 
    mutate(year = "2020") %>% 
    select(year, everything())

# afl_fixture_2020 %>%
#     write_csv(here::here("fixtures", "afl_fixture_2020.csv"))

# afl_venues_2020 %>%
#     unnest(cols = teams) %>%
#     write_csv(here::here("venues", "afl_venues_2020.csv"))
