elo_model %>% 
    as_tibble() %>% 
    mutate(season = afl_elo$season) %>% 
    filter(season == "2018") %>% 
    mutate(
        correct = if_else(
            p.A > 0.5 & wins.A > 0.5 | p.A < 0.5 & wins.A < 0.5, 1, 0
        )
    ) %>% 
    pull(correct) %>% 
    sum()
