library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2011 <- "https://en.wikipedia.org/wiki/2011_AFL_season"

# import data
fixture_raw_2011 <- map(
    3:26,
    ~ fixture_url_2011 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(round = paste("Round", (.x - 2))) %>% 
        select(round, everything())
    
)

afl_fixture_2011 <- fixture_raw_2011 %>% 
    map_df(bind_rows)

afl_fixture_2011 <- afl_fixture_2011 %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) 

afl_fixture_2011 <- afl_fixture_2011 %>% 
    slice(-17) %>% 
    select(round, date = X1, X2, X4, venue = X5) %>% 
    mutate(season = "2011",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2011") %>% 
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

afl_fixture_2011 <- afl_fixture_2011 %>% 
    mutate(
        venue = case_when(
            venue == "Etihad Stadium"    ~ "Docklands Stadium",
            venue == "Skilled Stadium"   ~ "Kardinia Park",
            venue == "Aurora Stadium"    ~ "York Park",
            venue == "Metricon Stadium"  ~ "Carrara Stadium",
            venue == "TIO Stadium"       ~ "Marrara Oval",
            venue == "Patersons Stadium" ~ "Subiaco Oval",
            venue == "ANZ Stadium"       ~ "Stadium Australia",
            venue == "AAMI Stadium"      ~ "Football Park",
            venue == "The Gabba"         ~ "Gabba",
            T                            ~ venue
        ) 
    ) 

afl_venues_2011 <- tibble(venue = afl_fixture_2011$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Football Park" | venue == "Adelaide Oval"                         ~ "SA",
            venue == "Cazaly's Stadium" | venue == "Gabba" | venue == "Carrara Stadium" ~ "QLD",
            venue == "Manuka Oval"                                                      ~ "ACT",
            venue == "Subiaco Oval"                                                     ~ "WA",
            venue == "SCG" | venue == "Stadium Australia"                               ~ "NSW",
            venue == "York Park"                                                        ~ "TAS",
            venue == "Kardinia Park"                                                    ~ "GEE",
            venue == "Marrara Oval"                                                     ~ "NT",
            T                                                                           ~ "VIC"
        ) 
    )

venues_teams_2011 <- map(
    unique(afl_fixture_2011$venue),
    ~ afl_fixture_2011 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2011$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2011 <- afl_venues_2011 %>% 
    left_join(venues_teams_2011, by = "venue") %>% 
    mutate(year = "2011") %>% 
    select(year, everything())

afl_fixture_2011 %>% 
    write_csv(here::here("fixtures", "afl_fixture_2011.csv"))

afl_venues_2011 %>% 
    unnest() %>% 
    write_csv(here::here("venues", "afl_venues_2011.csv"))
