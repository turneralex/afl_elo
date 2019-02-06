library(tidyverse)
library(rvest)

# wikipedia url
fixture_url <- "https://en.wikipedia.org/wiki/2019_AFL_season"

# import data
fixture_raw <- map(
    3:25,
    ~ fixture_url %>% 
        read_html() %>% 
        html_nodes(xpath = paste("//*[@id='mw-content-text']/div/table[", .x, "]")) %>% 
        html_table() %>% 
        magrittr::extract2(1) %>% 
        as_tibble()
)

afl_fixture <- map_df(fixture_raw, bind_rows) %>% 
    filter(X1 != "") %>% 
    select(date = X1, home_team = X2, away_team = X4, venue = X5, X3) %>% 
    mutate(date = date %>% str_trunc(width = (nchar(.) - 10), side = "right", ellipsis = ""))

(18 * 22) / 2

afl_fixture <- afl_fixture %>% 
    filter(X3 == "vs.") %>% 
    select(-X3)

afl_fixture %>% 
    select(home_team:venue) %>% 
    map_int(~ unique(.) %>% length())

