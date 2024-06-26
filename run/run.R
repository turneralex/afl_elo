library(dplyr)

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
            "fixture_scripts",
            "fixture_current.R"
        )
    )
    
}
source(
    here::here(
        "utils", 
        "fixture_scripts", 
        "fixture_all.R"
    )
)

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
    ),
    show_col_types = F
) %>% 
    tibble::deframe() 

# create elo model output

seasons_exclude <- c("2010", "2011")

afl_elo <- afl_fixture_all %>% 
    filter(
        !(season %in% seasons_exclude)
        & !is.na(home_team)
    ) %>%
    left_join(
        afl_venues_all %>% 
            select(venue, location) %>% 
            distinct(),
        by = "venue"
    ) %>% 
    mutate(
        match_id =  seq(
            from = 1,
            to = n(),
            by = 1
        ),
        # share of scoring shots
        home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
        home_margin = home_score - away_score,
        hga_app = purrr::pmap_int(
            .l = list(
                venue, 
                home_team, 
                away_team
            ), 
            .f = is_home, 
            data_venues = afl_venues_all,
            data_fixture = afl_fixture_all
        )
    ) %>%
    select(
        season, 
        round_number,
        round,
        round_name,
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
    group_by(team) %>% 
    mutate(
        team_match_id = seq(
            from = 1,
            to = n(),
            by = 1
        )
    ) %>% 
    ungroup() %>% 
    select(
        season, 
        round_number,
        round,
        round_name,
        match_id, 
        team_match_id,
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
            score_adjusted > 0.5, 
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
