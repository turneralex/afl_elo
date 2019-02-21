library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2016 <- "https://en.wikipedia.org/wiki/2016_AFL_season"

# import data
fixture_raw_2016 <- map(
    2:24,
    ~ fixture_url_2016 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(round = paste("Round", (.x - 1))) %>% 
        select(round, everything())
)

rm(fixture_url_2016)

afl_fixture_2016 <- fixture_raw_2016 %>% 
    map_df(bind_rows)

rm(fixture_raw_2016)

afl_fixture_2016$X3 %>% 
    unique() %>% 
    magrittr::extract(nchar(.) <= 10)

afl_fixture_2016 <- afl_fixture_2016 %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with"))

afl_fixture_2016 <- afl_fixture_2016 %>% 
    select(round, date = X1, X2, X4, venue = X5) %>% 
    mutate(season = "2016",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2016") %>% 
               lubridate::dmy(),
           home_team = X2 %>% str_remove(" [:digit:].*") %>% str_trim(),
           away_team = X4 %>% str_remove(" [:digit:].*") %>% str_trim(),
           home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           venue = venue %>% str_remove(" [(].*")) %>% 
    select(season, match_id, round, date, venue:away_score)

afl_fixture_2016$venue %>% unique() 

afl_fixture_2016 <- afl_fixture_2016 %>% 
    mutate(
        venue = case_when(
            venue == "Etihad Stadium"                 ~ "Docklands Stadium",
            venue == "Blundstone Arena"               ~ "Bellerive Oval",
            venue == "Simonds Stadium"                ~ "Kardinia Park",
            venue == "Spotless Stadium"               ~ "Sydney Showground Stadium",
            venue == "Aurora Stadium"                 ~ "York Park",
            venue == "TIO Traeger Park"               ~ "Traeger Park",
            venue == "Metricon Stadium"               ~ "Carrara Stadium",
            venue == "TIO Stadium"                    ~ "Marrara Oval",
            venue == "Domain Stadium"                 ~ "Subiaco Oval",
            T                                         ~ venue
        ) 
    ) 

afl_venues_2016 <- tibble(venue = afl_fixture_2016$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Adelaide Oval"                                                    ~ "SA",
            venue == "Cazaly's Stadium" | venue == "Gabba" | venue == "Carrara Stadium" ~ "QLD",
            venue == "Manuka Oval"                                                      ~ "ACT",
            venue == "Subiaco Oval"                                                     ~ "WA",
            venue == "SCG" | venue == "Sydney Showground Stadium"                       ~ "NSW",
            venue == "Bellerive Oval" | venue == "York Park"                            ~ "TAS",
            venue == "Kardinia Park"                                                    ~ "GEE",
            venue == "Traeger Park" | venue == "Marrara Oval"                           ~ "NT",
            T                                                                           ~ "VIC"
        ) 
    )

venues_teams_2016 <- map(
    unique(afl_fixture_2016$venue),
    ~ afl_fixture_2016 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2016$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2016 <- afl_venues_2016 %>% 
    left_join(venues_teams_2016, by = "venue") %>% 
    mutate(year = "2016") %>% 
    select(year, everything())

rm(venues_teams_2016)

beepr::beep(0)
