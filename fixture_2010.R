library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2010 <- "https://en.wikipedia.org/wiki/2010_AFL_season"

# import data
fixture_raw_2010 <- map(
    3:25,
    ~ fixture_url_2010 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(
            round = if_else(.x >= 16, paste("Round", (.x - 3)), paste("Round", (.x - 2)))
        ) %>% 
        select(round, everything())
    
)

afl_fixture_2010 <- fixture_raw_2010 %>% 
    map_df(bind_rows)

afl_fixture_2010 <- afl_fixture_2010 %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) 

afl_fixture_2010 <- afl_fixture_2010 %>% 
    select(round, date = X1, X2, X4, venue = X5) %>% 
    mutate(season = "2014",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2014") %>% 
               lubridate::dmy(),
           home_team = X2 %>% str_remove(" [:digit:].*") %>% str_trim(),
           away_team = X4 %>% str_remove(" [:digit:].*") %>% str_trim(),
           home_score = X2 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           away_score = X4 %>% str_remove_all(".*[(]|[)]") %>% as.integer(),
           venue = venue %>% str_remove(" [(].*")) %>% 
    select(season, match_id, round, date, venue:away_score)

afl_fixture_2010 <- afl_fixture_2010 %>% 
    mutate(
        venue = case_when(
            venue == "Etihad Stadium"  ~ "Docklands Stadium",
            venue == "Skilled Stadium" ~ "Kardinia Park",
            venue == "Aurora Stadium"  ~ "York Park",
            venue == "TIO Stadium"     ~ "Marrara Oval",
            venue == "ANZ Stadium"     ~ "Stadium Australia",
            venue == "AAMI Stadium"    ~ "Football Park",
            T                          ~ venue
        ) 
    ) 

afl_venues_2010 <- tibble(venue = afl_fixture_2010$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Football Park"                      ~ "SA",
            venue == "Gabba"                              ~ "QLD",
            venue == "Manuka Oval"                        ~ "ACT",
            venue == "Subiaco Oval"                       ~ "WA",
            venue == "SCG" | venue == "Stadium Australia" ~ "NSW",
            venue == "York Park"                          ~ "TAS",                 
            venue == "Kardinia Park"                      ~ "GEE",
            venue == "Marrara Oval"                       ~ "NT",
            T                                             ~ "VIC"
        ) 
    )

venues_teams_2010 <- map(
    unique(afl_fixture_2010$venue),
    ~ afl_fixture_2010 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2010$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2010 <- afl_venues_2010 %>% 
    left_join(venues_teams_2010, by = "venue") %>% 
    mutate(year = "2010") %>% 
    select(year, everything())

afl_fixture_2010 %>% 
    write.csv(file = "afl_fixture_2010.csv", row.names = F)

afl_venues_2010 %>% 
    unnest() %>% 
    write.csv(file = "afl_venues_2010.csv", row.names = F)
