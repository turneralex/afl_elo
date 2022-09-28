# devtools::install_github("jimmyday12/fitzRoy")

library(tidyverse)

afl_fixture_2023 <- fitzRoy::fetch_fixture_footywire(season = 2023) %>% 
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
    ) %>% 
    select(Season, match_id, Round, Date, Venue, Home.Team, Away.Team)

colnames(afl_fixture_2023) <- colnames(afl_fixture_2023) %>% 
    tolower() %>% 
    str_replace("\\.", "_")

afl_fixture_2023 <- afl_fixture_2023 %>% 
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

results_2023 <- fitzRoy::fetch_results_afl(season = 2023) %>% 
    filter(
        !(round.name %in% c("Finals Week 1", "Semi Finals", "Preliminary Finals", "Grand Final"))
    )

afl_fixture_2023 <- afl_fixture_2023 %>% 
    mutate(season = "2023", match_id = 1:nrow(.)) %>% 
    left_join(
        results_2023 %>% 
            mutate(
                home_team = case_when(
                    match.homeTeam.name == "Geelong Cats"      ~ "Geelong",
                    match.homeTeam.name == "GWS Giants"        ~ "Greater Western Sydney",
                    match.homeTeam.name == "Gold Coast Suns"   ~ "Gold Coast",
                    match.homeTeam.name == "Sydney Swans"      ~ "Sydney",
                    match.homeTeam.name == "Adelaide Crows"    ~ "Adelaide",
                    match.homeTeam.name == "West Coast Eagles" ~ "West Coast",
                    T                                          ~ match.homeTeam.name
                )
            ) %>% 
            select(
                round = round.name,
                home_team,
                home_goals = homeTeamScore.matchScore.goals,
                home_behinds = homeTeamScore.matchScore.behinds,
                home_score = homeTeamScore.matchScore.totalScore,
                away_goals = awayTeamScore.matchScore.goals,
                away_behinds = awayTeamScore.matchScore.behinds,
                away_score = awayTeamScore.matchScore.totalScore
            ),
        by = c("round", "home_team")
    )

# results_2023 <- fitzRoy::fetch_results_afltables(season = 2023) %>%
#     filter(
#         !(round %in% c("EF", "QF", "SF", "PF", "GF"))
#     )
# 
# afl_fixture_2023 <- afl_fixture_2023 %>% 
#     mutate(season = "2023", match_id = 1:nrow(.)) %>% 
#     left_join(
#         results_2023 %>% 
#             mutate(
#                 home_team = case_when(
#                     Home.Team == "Footscray" ~ "Western Bulldogs",
#                     Home.Team == "GWS"       ~ "Greater Western Sydney",
#                     T                        ~ Home.Team
#                 ),
#                 round = paste("Round", str_replace(Round, "R", ""))
#             ) %>% 
#             select(
#                 round,
#                 home_team,
#                 home_goals = Home.Goals,
#                 home_behinds = Home.Behinds,
#                 home_score = Home.Points,
#                 away_goals = Away.Goals,
#                 away_behinds = Away.Behinds,
#                 away_score = Away.Points
#             ),
#         by = c("round", "home_team")
#     )

afl_venues_2023 <- tibble(venue = afl_fixture_2023$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Adelaide Oval"                                                             ~ "SA",
            venue == "Gabba" | venue == "Carrara Stadium" | venue == "Cazaly's Stadium"          ~ "QLD",
            venue == "Manuka Oval"                                                               ~ "ACT",
            venue == "Perth Stadium"                                                             ~ "WA",
            venue == "SCG" | venue == "Sydney Showground Stadium" | venue == "Stadium Australia" ~ "NSW",
            venue == "Bellerive Oval" | venue == "York Park"                                     ~ "TAS",
            venue == "Kardinia Park"                                                             ~ "GEE",
            venue == "Jiangwan Stadium"                                                          ~ "Other",
            venue == "Traeger Park" | venue == "Marrara Oval"                                    ~ "NT",
            T                                                                                    ~ "VIC"
        )  
    )

venues_teams_2023 <- map(
    unique(afl_fixture_2023$venue),
    ~ afl_fixture_2023 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2023$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2023 <- afl_venues_2023 %>% 
    left_join(venues_teams_2023, by = "venue") %>% 
    mutate(year = "2023") %>% 
    select(year, everything())

afl_fixture_2023 %>%
    write_csv(here::here("fixtures", "afl_fixture_2023.csv"))

afl_venues_2023 %>%
    unnest(cols = teams) %>%
    write_csv(here::here("venues", "afl_venues_2023.csv"))

rm(results_2023)
