source(here::here("utils/fixture scripts/fixture_all.R"))
source(here::here("utils/functions_general.R"))

elo_par <- read_csv(
    here::here("files/params/archive/elo_par_2023.csv")
) %>% 
    deframe() 

afl_elo <- afl_fixture_all %>% 
    filter(
        season %in% c("2020", "2021")
    ) %>% 
    convert_elo_df() %>% 
    elo_run(k = elo_par["k"], hga = elo_par[3:10], regress = elo_par["regress"])

afl_elo

afl_elo %>% 
    mutate(
        round = paste(
            season,
            str_replace(round, "Round ", ""),
            sep = "_"
        ) %>% 
            fct_inorder()
    ) %>% 
    ggplot(aes(round, new_elo, group = 1)) +
    geom_hline(yintercept = 1500, colour = "firebrick1", alpha = 0.5) +
    geom_point() +
    geom_line() +
    facet_wrap(. ~ team) +
    labs(title = "Elo Rating by Round",
         # subtitle = paste("Season:", current_season),
         x = "Round",
         y = "Elo Rating") 

afl_elo %>% 
    mutate(
        correct_tip = if_else(
            (score_adjusted >= 0.5 & score_expected >= 0.5)
            | (score_adjusted <= 0.5 & score_expected <= 0.5),
            1,
            0
        )
    ) %>% 
    pull(correct_tip) %>% 
    sum()
