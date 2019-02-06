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

test <- map_df(fixture_raw, bind_rows) %>% 
    filter(X1 != "")

