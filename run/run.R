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

elo_par <- readr::read_csv(
    here::here(
        "files",
        "params",
        "elo_par.csv"
    ),
    show_col_types = F
) %>% 
    tibble::deframe() 

# distinct combos of venue x location

afl_venues_locations_all <- bind_rows(
    afl_venues_current %>% 
        select(venue, location),
    afl_venues_history %>% 
        select(venue, location)
) %>% 
    distinct() %>% 
    arrange(location)

# create a master table of all historical HGA 
# & apply most recent HGA to current season

afl_venues_all <- bind_rows(
    afl_venues_current %>% 
        tidyr::unnest(cols = teams) %>% 
        mutate(
            season = current_season,
            total_flag = NA_real_,
            current_flag = NA_real_
        ) %>% 
        select(
            venue,
            location,
            season, 
            team,
            total_flag,
            current_flag
        ),
    afl_venues_history %>% 
        tidyr::unnest(cols = data_teams)
) %>% 
    group_by(team, venue) %>% 
    arrange(season) %>% 
    mutate(
        total_flag = if_else(
            season == current_season,
            lag(total_flag, n = 1, default = 0),
            total_flag
        ),
        current_flag = if_else(
            season == current_season,
            lag(current_flag, n = 1, default = 0),
            current_flag
        )
    ) %>% 
    ungroup() %>% 
    tidyr::nest(
        data_teams = c(team, total_flag, current_flag)
    )

# elo model input data

afl_elo_input <- afl_fixture_all %>% 
    filter(!is.na(home_team)) %>%
    left_join(
        afl_venues_locations_all %>% 
            select(venue, location) %>% 
            distinct(),
        by = "venue"
    ) %>% 
    mutate(
        # share of scoring shots
        home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
        home_margin = home_score - away_score,
        hga_app_flag = purrr::pmap_int(
            .l = list(
                venue, 
                home_team, 
                away_team,
                season
            ), 
            .f = is_home, 
            data_venues = afl_venues_all
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
        hga_app_flag
    ) %>% 
    tidyr::pivot_longer(
        cols = c("home_team", "away_team"), 
        names_to = c("home_away", "temp"), 
        names_sep = "_",
        values_to = "team"
    ) %>% 
    group_by(team) %>% 
    mutate(
        regress_app_flag = if_else(
            lead(season, n = 1, default = current_season) != season,
            1,
            0
        )
    ) %>% 
    ungroup() %>% 
    mutate(
        home_team_flag = if_else(
            home_away == "home",
            1,
            0
        ),
        score_adjusted = if_else(home_away == "away", 1 - home_score_adjusted, home_score_adjusted),
        score_expected = numeric(nrow(.)),
        margin = if_else(home_away == "away", -home_margin, home_margin),
        hga_app_flag = if_else(home_away == "away", as.integer(-hga_app_flag), hga_app_flag),
        hga = case_match(
            location,
            "VIC" ~ elo_par["hga_vic"] * hga_app_flag,
            "NSW" ~ elo_par["hga_nsw"] * hga_app_flag,
            "QLD" ~ elo_par["hga_qld"] * hga_app_flag,
            "SA"  ~ elo_par["hga_sa"] * hga_app_flag,
            "WA"  ~ elo_par["hga_wa"] * hga_app_flag,
            "GEE" ~ elo_par["hga_gee"] * hga_app_flag,
            "TAS" ~ elo_par["hga_tas"] * hga_app_flag,
            "ACT" ~ elo_par["hga_act"] * hga_app_flag,
            .default = 0
        ),
        start_elo = NA_real_,
        new_elo = NA_real_
    ) %>% 
    select(
        season, 
        round_number,
        round,
        round_name,
        match_id, 
        venue, 
        location, 
        team, 
        home_team_flag, 
        hga_app_flag, 
        hga,
        regress_app_flag,
        score_adjusted,
        score_expected,
        margin,
        start_elo,
        new_elo
    ) 

# run the elo model

afl_elo <- elo_run(
    afl_elo_input, 
    elo_par["k"], 
    elo_par["regress"]
)

# add prediction variables

# add game win % prediction

afl_win_prob_model <- afl_elo %>% 
    filter(!is.na(score_adjusted)) %>% 
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
    filter(!is.na(score_adjusted)) %>% 
    lm(
        margin ~ score_expected,
        data = .
    )
