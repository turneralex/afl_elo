library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2019 <- "https://en.wikipedia.org/wiki/2019_AFL_season"

# import data
fixture_raw_2019 <- map(
    3:25,
    ~ fixture_url_2019 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(round = paste("Round", (.x - 2))) %>% 
        select(round, everything())
)

afl_fixture_2019 <- fixture_raw_2019 %>% 
    map_df(bind_rows) %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with|vs\\.") & X1 %>% str_length() < 50) %>% 
    select(round, date = X1, X2, X4, venue = X5) %>% 
    mutate(season = "2019",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2019") %>% 
               lubridate::dmy(),
           home_team = X2 %>% str_remove(" [:digit:].*") %>% str_trim(),
           away_team = X4 %>% str_remove(" [:digit:].*") %>% str_trim(),
           home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           home_goals = X2 %>% str_remove_all("[:alpha:]* |\\..*") %>% as.integer(),
           away_goals = X4 %>% str_remove_all("[:alpha:]* |\\..*") %>% as.integer(),
           home_behinds = home_score - (home_goals * 6),
           away_behinds = away_score - (away_goals * 6),
           venue = venue %>% str_remove(" [(].*")) %>% 
    select(season, match_id, round, date, venue:away_behinds)

afl_fixture_2019 <- afl_fixture_2019 %>% 
    mutate(
        venue = case_when(
            venue == "Marvel Stadium"                                           ~ "Docklands Stadium",
            venue == "UNSW Canberra Oval"                                       ~ "Manuka Oval",
            venue == "Optus Stadium"                                            ~ "Perth Stadium",
            venue == "Blundstone Arena"                                         ~ "Bellerive Oval",
            venue == "GMHBA Stadium"                                            ~ "Kardinia Park",
            venue == "GIANTS Stadium"                                           ~ "Sydney Showground Stadium",
            venue == "University of Tasmania Stadium" | venue == "UTAS Stadium" ~ "York Park",
            venue == "Mars Stadium"                                             ~ "Eureka Stadium",
            venue == "TIO Traeger Park"                                         ~ "Traeger Park",
            venue == "Metricon Stadium"                                         ~ "Carrara Stadium",
            venue == "TIO Stadium"                                              ~ "Marrara Oval",
            venue == "The Gabba"                                                ~ "Gabba",
            T                                                                   ~ venue
        ) 
    ) 

afl_venues_2019 <- tibble(venue = afl_fixture_2019$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Adelaide Oval"                                                    ~ "SA",
            venue == "Riverway Stadium" | venue == "Gabba" | venue == "Carrara Stadium" ~ "QLD",
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

venues_teams_2019 <- map(
    unique(afl_fixture_2019$venue),
    ~ afl_fixture_2019 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2019$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2019 <- afl_venues_2019 %>% 
    left_join(venues_teams_2019, by = "venue") %>% 
    mutate(year = "2019") %>% 
    select(year, everything())

afl_fixture_2019 %>%
    write_csv(here::here("fixtures", "afl_fixture_2019.csv"))

afl_venues_2019 %>%
    unnest(cols = teams) %>%
    write_csv(here::here("venues", "afl_venues_2019.csv"))
