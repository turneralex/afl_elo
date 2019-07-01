source("./functions_general.R")

library(tidyverse)
library(elo)

afl_fixture_all <- map_dfr(
    2010:2019, 
    ~ read_csv(
        paste0("./afl_fixture_", .x, ".csv"),
        col_types = "cicDcccii"
    )
)

afl_venues_all <- map_dfr(
    2010:2019, 
    ~ read_csv(
        paste0("./afl_venues_", .x, ".csv"),
        col_types = "cccc"
    )
) %>% 
    nest(team)

rounds_so_far <- 1:15

afl_fixture_2019 <- map_dfr(
    rounds_so_far,
    ~ afl_fixture_2019 %>% 
        filter(round == paste("Round", .x)) %>% 
        mutate(
            home_score = replace(home_score, .$round == paste("Round", .x), get_round_scores(2019, round = .x) %>% pull(home_score)),
            away_score = replace(away_score, .$round == paste("Round", .x), get_round_scores(2019, round = .x) %>% pull(away_score))
        )
) %>% 
    bind_rows(
        afl_fixture_2019 %>% 
            filter(!(round %in% paste("Round", rounds_so_far)))
    )

afl_elo <- bind_rows(
    afl_elo,
    afl_fixture_2019 %>% 
        filter(round %in% paste("Round", rounds_so_far)) %>% 
        mutate(home_score_adjusted = home_score / (home_score + away_score),
               hga_app = pmap_int(list(season, venue, away_team), is_home, data = afl_venues_all))
) 

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 

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

afl_elo_all %>% 
    filter((home_team == "Fremantle" | away_team == "Fremantle" ) & season == "2018") %>% 
    select(season, match_id, home_team, away_team, new_home_elo, new_away_elo) %>% 
    unite("season_match_id", c("season", "match_id"), sep = "", remove = F) %>% 
    mutate(
        season_match_id = season_match_id %>% as.integer(),
        match_id = 1:nrow(.),
        elo_rating = if_else(
            home_team == "Fremantle", new_home_elo, new_away_elo
        )
    ) %>% 
    select(-new_home_elo, -new_away_elo) %>% 
    ggplot(aes(match_id, elo_rating)) +
    geom_line()

teams %>% 
    map(
        ~ afl_elo_all %>% 
            filter(home_team == .x | away_team == .x) %>% 
            select(season, match_id, home_team, away_team, new_home_elo, new_away_elo) %>% 
            unite("season_match_id", c("season", "match_id"), sep = "", remove = F) %>% 
            mutate(
                season_match_id = season_match_id %>% as.integer(),
                match_id = 1:nrow(.),
                elo_rating = if_else(
                    home_team == .x, new_home_elo, new_away_elo
                )
            ) %>% 
            select(-new_home_elo, -new_away_elo) %>% 
            ggplot(aes(match_id, elo_rating, colour = season)) +
            geom_line() +
            geom_point() +
            scale_y_continuous(limits = c(1375, 1625)) +
            labs(title = .x)
    )

afl_elo_all$new_home_elo %>% max()
afl_elo_all$new_home_elo %>% min()
afl_elo_all$new_away_elo %>% max()
afl_elo_all$new_away_elo %>% min()
