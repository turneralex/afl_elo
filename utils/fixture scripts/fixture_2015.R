library(tidyverse)
library(rvest)

# wikipedia url
fixture_url_2015 <- "https://en.wikipedia.org/wiki/2015_AFL_season"

# import data
fixture_raw_2015 <- map(
    3:25,
    ~ fixture_url_2015 %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble() %>% 
        mutate(round = paste("Round", (.x - 2))) %>% 
        select(round, everything())
)

afl_fixture_2015 <- fixture_raw_2015 %>% 
    map_df(bind_rows)

afl_fixture_2015 <- afl_fixture_2015 %>% 
    filter(X3 %>% str_detect("def\\.|def\\. by|drew with|vs.")) %>% 
    slice(-199)

afl_fixture_2015 <- afl_fixture_2015 %>% 
    select(round, date = X1, X2, X4, venue = X5) %>% 
    mutate(season = "2015",
           match_id = 1:nrow(.),
           date = date %>% 
               str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = "") %>% 
               str_remove(".*, ") %>% 
               paste(., "2015") %>% 
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

# replace values for abandoned match
afl_fixture_2015 <- afl_fixture_2015 %>% 
    mutate_at(
        c("home_score", "away_score", "home_goals", "away_goals", "home_behinds", "away_behinds"),
        ~ replace(.x, is.na(.x), 1)
    )

afl_fixture_2015 <- afl_fixture_2015 %>% 
    mutate(
        venue = case_when(
            venue == "Etihad Stadium"   ~ "Docklands Stadium",
            venue == "Blundstone Arena" ~ "Bellerive Oval",
            venue == "Simonds Stadium"  ~ "Kardinia Park",
            venue == "Spotless Stadium" ~ "Sydney Showground Stadium",
            venue == "Aurora Stadium"   ~ "York Park",
            venue == "TIO Traeger Park" ~ "Traeger Park",
            venue == "Metricon Stadium" ~ "Carrara Stadium",
            venue == "TIO Stadium"      ~ "Marrara Oval",
            venue == "Domain Stadium"   ~ "Subiaco Oval",
            venue == "ANZ Stadium"      ~ "Stadium Australia",
            venue == "StarTrack Oval"   ~ "Manuka Oval",
            venue == "Westpac Stadium"  ~ "Wellington Regional Stadium",
            T                           ~ venue
        ) 
    ) 

afl_venues_2015 <- tibble(venue = afl_fixture_2015$venue %>% unique()) %>%
    mutate(
        location = case_when(
            venue == "Adelaide Oval"                                                             ~ "SA",
            venue == "Cazaly's Stadium" | venue == "Gabba" | venue == "Carrara Stadium"          ~ "QLD",
            venue == "Manuka Oval"                                                               ~ "ACT",
            venue == "Subiaco Oval"                                                              ~ "WA",
            venue == "SCG" | venue == "Sydney Showground Stadium" | venue == "Stadium Australia" ~ "NSW",
            venue == "Bellerive Oval" | venue == "York Park"                                     ~ "TAS",
            venue == "Kardinia Park"                                                             ~ "GEE",
            venue == "Traeger Park" | venue == "Marrara Oval"                                    ~ "NT",
            venue == "Wellington Regional Stadium"                                               ~ "Other",
            T                                                                                    ~ "VIC"
        ) 
    )

venues_teams_2015 <- map(
    unique(afl_fixture_2015$venue),
    ~ afl_fixture_2015 %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    set_names(unique(afl_fixture_2015$venue)) %>% 
    enframe(name = "venue", value = "teams") %>% 
    mutate(venue = venue)

afl_venues_2015 <- afl_venues_2015 %>% 
    left_join(venues_teams_2015, by = "venue") %>% 
    mutate(year = "2015") %>% 
    select(year, everything())

afl_fixture_2015 %>% 
    write_csv(here::here("fixtures", "afl_fixture_2015.csv"))

afl_venues_2015 %>% 
    unnest() %>% 
    write_csv(here::here("venues", "afl_venues_2015.csv"))
