library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2012 <- "https://en.wikipedia.org/wiki/2012_AFL_season"

# import data
fixture_raw_2012 <- map(
    3:25,
    ~ fixture_url_2012 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(round = paste("Round", (.x - 2))) %>% 
        select(round, everything())
    
)

afl_fixture_2012 <- fixture_raw_2012 %>% 
    map_df(bind_rows)

afl_fixture_2012 <- afl_fixture_2012 %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with")) 

afl_fixture_2012 <- afl_fixture_2012 %>% 
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

afl_fixture_2012 <- afl_fixture_2012 %>% 
    mutate(
        venue = case_when(
            venue == "Etihad Stadium"    ~ "Docklands Stadium",
            venue == "Blacktown ISP"     ~ "Blacktown International Sportspark",
            venue == "Blundstone Arena"  ~ "Bellerive Oval",
            venue == "Simonds Stadium"   ~ "Kardinia Park",
            venue == "Škoda Stadium"     ~ "Sydney Showground Stadium",
            venue == "Aurora Stadium"    ~ "York Park",
            venue == "Metricon Stadium"  ~ "Carrara Stadium",
            venue == "TIO Stadium"       ~ "Marrara Oval",
            venue == "Patersons Stadium" ~ "Subiaco Oval",
            venue == "ANZ Stadium"       ~ "Stadium Australia",
            venue == "AAMI Stadium"      ~ "Football Park",
            T                            ~ venue
        ) 
    ) 

afl_venues_2012 <- tibble(venue = afl_fixture_2012$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Football Park"                                                                                                             ~ "SA",
            venue == "Cazaly's Stadium" | venue == "Gabba" | venue == "Carrara Stadium"                                                          ~ "QLD",
            venue == "Manuka Oval"                                                                                                               ~ "ACT",
            venue == "Subiaco Oval"                                                                                                              ~ "WA",
            venue == "SCG" | venue == "Sydney Showground Stadium" | venue == "Stadium Australia" | venue == "Blacktown International Sportspark" ~ "NSW",
            venue == "Bellerive Oval" | venue == "York Park"                                                                                     ~ "TAS",
            venue == "Kardinia Park"                                                                                                             ~ "GEE",
            venue == "Marrara Oval"                                                                                                              ~ "NT",
            T                                                                                                                                    ~ "VIC"
        ) 
    )

venues_teams_2012 <- map(
    unique(afl_fixture_2012$venue),
    ~ afl_fixture_2012 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2012$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2012 <- afl_venues_2012 %>% 
    left_join(venues_teams_2012, by = "venue") %>% 
    mutate(year = "2012") %>% 
    select(year, everything())

afl_fixture_2012 %>% 
    write.csv(file = "afl_fixture_2012.csv", row.names = F)

afl_venues_2012 %>% 
    unnest() %>% 
    write.csv(file = "afl_venues_2012.csv", row.names = F)
