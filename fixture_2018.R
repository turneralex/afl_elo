library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2018 <- "https://en.wikipedia.org/wiki/2018_AFL_season"

# import data
fixture_raw_2018 <- map(
    3:25,
    ~ fixture_url_2018 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(round = paste("Round", (.x - 2))) %>% 
        select(round, everything())
)

afl_fixture_2018 <- fixture_raw_2018 %>% 
    map_df(bind_rows)

rm(fixture_raw_2018)

afl_fixture_2018$X3 %>% 
    unique() %>% 
    magrittr::extract(nchar(.) <= 10)

afl_fixture_2018 <- afl_fixture_2018 %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with"))

afl_fixture_2018 <- afl_fixture_2018 %>% 
    select(round, date = X1, X2, X4, venue = X5) %>% 
    mutate(season = "2018",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2018") %>% 
               lubridate::dmy(),
           home_team = X2 %>% str_remove(" [:digit:].*") %>% str_trim(),
           away_team = X4 %>% str_remove(" [:digit:].*") %>% str_trim(),
           home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           venue = venue %>% str_remove(" [(].*")) %>% 
    select(season, match_id, round, date, venue:away_score)

afl_fixture_2018$venue %>% unique()

afl_fixture_2018 <- afl_fixture_2018 %>% 
    mutate(
        venue = case_when(
            venue == "Etihad Stadium"                 ~ "Docklands Stadium",
            venue == "UNSW Canberra Oval"             ~ "Manuka Oval",
            venue == "Optus Stadium"                  ~ "Perth Stadium",
            venue == "Blundstone Arena"               ~ "Bellerive Oval",
            venue == "GMHBA Stadium"                  ~ "Kardinia Park",
            venue == "Spotless Stadium"               ~ "Sydney Showground Stadium",
            venue == "University of Tasmania Stadium" ~ "York Park",
            venue == "Mars Stadium"                   ~ "Eureka Stadium",
            venue == "TIO Traeger Park"               ~ "Traeger Park",
            venue == "Metricon Stadium"               ~ "Carrara Stadium",
            venue == "TIO Stadium"                    ~ "Marrara Oval",
            T                                         ~ venue
        ) 
    ) 

afl_venues_2018 <- tibble(venue = afl_fixture_2018$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Adelaide Oval"                                                    ~ "SA",
            venue == "Cazaly's Stadium" | venue == "Gabba" | venue == "Carrara Stadium" ~ "QLD",
            venue == "Manuka Oval"                                                      ~ "ACT",
            venue == "Perth Stadium"                                                    ~ "WA",
            venue == "SCG" | venue == "Sydney Showground Stadium"                       ~ "NSW",
            venue == "Bellerive Oval" | venue == "York Park"                            ~ "TAS",
            venue == "Kardinia Park"                                                    ~ "GEE",
            venue == "Jiangwan Stadium"                                                 ~ "Other",
            venue == "Traeger Park" | venue == "Marrara Oval"                           ~ "NT",
            T                                                                           ~ "VIC"
        ) 
    )

venues_teams_2018 <- map(
    unique(afl_fixture_2018$venue),
    ~ afl_fixture_2018 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2018$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2018 <- afl_venues_2018 %>% 
    left_join(venues_teams_2018, by = "venue") %>% 
    mutate(year = "2018") %>% 
    select(year, everything())

rm(venues_teams_2018)

beepr::beep(0)
