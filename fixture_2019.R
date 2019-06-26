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
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with|vs\\.")) %>% 
    filter(X1 %>% str_length() < 50) %>% # get rid of text row
    select(round, date = X1, home_team = X2, away_team = X4, venue = X5) %>% 
    mutate(season = "2019",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2019") %>% 
               lubridate::dmy(),
           home_team = home_team %>% str_remove(" [:digit:].*") %>% str_trim(),
           away_team = away_team %>% str_remove(" [:digit:].*") %>% str_trim(),
           venue = venue %>% str_remove(" [(].*"),
           home_score = NA,
           away_score = NA) %>% 
    select(season, match_id, round, date, venue, home_team:away_score) 

afl_fixture_2019 <- afl_fixture_2019 %>% 
    mutate(
        venue = case_when(
            venue == "Marvel Stadium"                                           ~ "Docklands Stadium",
            venue == "UNSW Canberra Oval"                                       ~ "Manuka Oval",
            venue == "Optus Stadium"                                            ~ "Perth Stadium",
            venue == "Blundstone Arena"                                         ~ "Bellerive Oval",
            venue == "GMHBA Stadium"                                            ~ "Kardinia Park",
            venue == "GIANTS Stadium"                                         ~ "Sydney Showground Stadium",
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
    write.csv(file = "afl_fixture_2019.csv", row.names = F)

afl_venues_2019 %>% 
    unnest() %>% 
    write.csv(file = "afl_venues_2019.csv", row.names = F)

# rm(
#     fixture_url_2019,
#     fixture_raw_2019,
#     venues_teams_2019,
#     afl_venues_2019,
# )
