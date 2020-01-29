library(tidyverse)

teams <- afl_fixture_all %>% 
    select(home_team) %>% 
    pull() %>% 
    unique() %>% 
    sort()

afl_elo_all <- elo_model$elos %>% 
    as_tibble(.name_repair = "unique") %>% 
    select(3, 6, 7) 

col_names <- c("home_score_elo", "new_home_elo", "new_away_elo")

colnames(afl_elo_all) <- col_names

afl_elo_all <- afl_elo %>% 
    bind_cols(afl_elo_all) %>% 
    select(1:9, 11, 10, 12:14)

afl_elo_all

afl_elo_all %>% 
    filter(season == "2019") %>% 
    ggplot(aes(home_score_adjusted, home_score_elo)) +
    geom_point() 
