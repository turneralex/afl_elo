source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)
if (exists("current_season")) {
    
    source(
        here::here(
            "utils",
            "fixture scripts",
            paste0(
                "fixture_",
                current_season,
                ".R"
            )
        )
    )
    
}
source(
    here::here(
        "utils", 
        "fixture scripts", 
        "fixture_all.R"
    )
)

library(dplyr)

# get parameters

files <- list.files(
    path = "./files/params",
    pattern = "csv"
)

elo_par <- readr::read_csv(
    here::here(
        "files",
        "params",
        files[files != "archive"]
    )
) %>% 
    tibble::deframe() 

# create elo model output

seasons_exclude <- "2010"

afl_elo <- afl_fixture_all %>% 
    filter(
        !(season %in% seasons_exclude)
    ) %>%
    left_join(
        afl_venues_all %>% 
            select(venue, location) %>% 
            distinct(),
        by = "venue"
    ) %>% 
    mutate(
        match_id =  1:nrow(.),
        # share of scoring shots
        home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
        home_margin = home_score - away_score,
        hga_app = purrr::pmap_int(
            list(season, venue, home_team, away_team), 
            is_home, 
            data_venues = afl_venues_all, 
            data_fixture = afl_fixture_all
        )
    ) %>% 
    select(
        season, 
        round, 
        match_id, 
        venue, 
        location, 
        home_team, 
        away_team, 
        home_score_adjusted, 
        home_margin,
        hga_app
    ) %>% 
    tidyr::pivot_longer(
        cols = c("home_team", "away_team"), 
        names_to = c("home_away", "temp"), 
        names_sep = "_",
        values_to = "team"
    ) %>% 
    select(
        season, 
        round, 
        match_id, 
        venue, 
        location, 
        team, 
        home_away, 
        hga_app, 
        home_score_adjusted,
        home_margin
    ) %>% 
    mutate(
        score_adjusted = if_else(home_away == "away", 1 - home_score_adjusted, home_score_adjusted),
        score_expected = numeric(nrow(.)),
        margin = if_else(home_away == "away", -home_margin, home_margin),
        hga_app = if_else(home_away == "away", as.integer(-hga_app), hga_app),
        start_elo = 1500,
        new_elo = 1500
    ) %>% 
    select(
        -home_away, 
        -home_score_adjusted,
        -home_margin
    ) %>% 
    elo_run(
        k = elo_par["k"], 
        hga = elo_par[3:10], 
        regress = elo_par["regress"]
    )

# add prediction variables

# exclude covid seasons

seasons_exclude_pred <- c("2020", "2021")

# add game win % prediction

afl_win_prob_model <- afl_elo %>% 
    filter(
        !is.na(score_adjusted)
        & !(season %in% seasons_exclude_pred)
    ) %>% 
    mutate(
        win = if_else(
            score_adjusted >= 0.5, 
            1,
            0
        )
    ) %>% 
    glm(
        win ~ score_expected,
        family = binomial(link = "logit"),
        data = .
    )

# add margin prediction

afl_margin_model <- afl_elo %>% 
    filter(
        !is.na(score_adjusted)
        & !(season %in% seasons_exclude_pred)
    ) %>% 
    lm(
        margin ~ score_expected,
        data = .
    )
