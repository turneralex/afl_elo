# instructions:
# 1. shift elo_par.csv to archive
# 2. update seasons_exclude
# 3. run script
# 4. update params_input.txt

library(dplyr)

start_season <- "2012"
current_season <- "2023" # use previous season 

source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)
source(
    here::here(
        "utils", 
        "fixture scripts", 
        "fixture_all.R"
    )
)

par_file_name <- "elo_par"
seasons_exclude <- c("2010", "2011")
seasons_remove_hga <- c("2020", "2021")

afl_elo <- afl_fixture_all %>% 
    filter(
        !(season %in% seasons_exclude)
    ) %>%
    left_join(
        afl_venues_all %>% 
            select(
                venue, 
                location
            ) %>% 
            distinct(),
        by = "venue"
    ) %>% 
    mutate(
        match_id =  1:nrow(.),
        # share of scoring shots
        home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
        hga_app = purrr::pmap_int(
            .l = list(
                season, 
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
        round, 
        match_id, 
        venue, 
        location, 
        home_team, 
        away_team, 
        home_score_adjusted, 
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
        home_score_adjusted
    ) %>% 
    mutate(
        score_adjusted = if_else(
            home_away == "away", 
            1 - home_score_adjusted, 
            home_score_adjusted
        ),
        score_expected = numeric(nrow(.)),
        hga_app = if_else(
            home_away == "away", 
            as.integer(hga_app * -1), 
            hga_app
        ),
        start_elo = 1500,
        new_elo = 1500
    ) %>% 
    select(
        -home_away, 
        -home_score_adjusted
    ) %>% 
    # exclude certain seasons for home ground advantage
    mutate(
        hga_app = if_else(
            season %in% seasons_remove_hga,
            as.integer(0),
            hga_app
        )
    )

parameter_optim <- function(elo_df, par) {
    
    library(magrittr)
    
    all_games <- unique(elo_df$match_id)
    
    for (i in 1:(length(all_games))) {
        
        game <- elo_df[elo_df$match_id == i, ] 
        
        current_season <- game$season[1]
        
        row_id <- which(elo_df$match_id == i)[1]
        
        next_season <- data.table::as.data.table(elo_df) %>% 
            .[row_id:nrow(.)] %>% 
            .[, season := lead(season, n = 1, default = current_season), by = team] %>% 
            .[team == game[1, "team"]] %>% 
            .$season %>% 
            .[1]
        
        if (current_season == next_season) {
            regress_app <- 0
        } else {
            regress_app <- 1
        }
        
        location <- game$location[1]
        
        if (location == "VIC") {
            hga <- par["hga_vic"]
        } else if (location == "NSW") {
            hga <- par["hga_nsw"]
        } else if (location == "QLD") {
            hga <- par["hga_qld"]
        } else if (location == "SA") {
            hga <- par["hga_sa"]
        } else if (location == "WA") {
            hga <- par["hga_wa"]
        } else if (location == "GEE") {
            hga <- par["hga_gee"]
        } else if (location == "TAS") {
            hga <- par["hga_tas"]
        } else {
            hga <- par["hga_other"]
        }
        
        score_expected_1 <- score_expected(
            game[1, "start_elo"], 
            game[2, "start_elo"], 
            hga_app = game[1, "hga_app"], 
            hga = hga
        )
        
        score_expected_2 <- score_expected(
            game[2, "start_elo"], 
            game[1, "start_elo"], 
            hga_app = game[2, "hga_app"], 
            hga = hga
        )
        
        elo_df[elo_df$match_id == i, "score_expected"] <- c(score_expected_1, score_expected_2) %>%
            unlist()
        
        new_elo_1 <- elo_update(
            elo = game[1, "start_elo"],
            elo_opp = game[2, "start_elo"],
            score_adjusted = game[1, "score_adjusted"],
            score_expected = score_expected_1,
            k = par[1],
            regress = par[2],
            regress_app = regress_app
        )
        
        new_elo_2 <- elo_update(
            elo = game[2, "start_elo"],
            elo_opp = game[1, "start_elo"],
            score_adjusted = game[2, "score_adjusted"],
            score_expected = score_expected_2,
            k = par[1],
            regress = par[2],
            regress_app = regress_app
        )
        
        elo_df[elo_df$match_id == i, "new_elo"] <- c(new_elo_1, new_elo_2) %>%
            unlist()
        
        elo_df <- data.table::as.data.table(elo_df) %>%
            .[, start_elo := lag(new_elo, n = 1, default = 1500), by = team]
        
    }
    
    elo_df %>% 
        mutate(diff = abs(score_adjusted - score_expected)) %>% 
        pull(diff) %>% 
        sum()
    
}

elo_par <- optim(
    par = c(50, 0.5, rep(50, 8)),
    lower = rep(0, 11),
    upper = c(100, 1, rep(100, 8)),
    parameter_optim, 
    elo_df = afl_elo,
    method = "L-BFGS-B"
) %>% 
    purrr::pluck("par") %>% 
    purrr::set_names(
        nm = c(
            "k", 
            "regress", 
            "hga_vic", 
            "hga_nsw", 
            "hga_qld", 
            "hga_sa", 
            "hga_wa", 
            "hga_gee", 
            "hga_tas", 
            "hga_other"
        )
    )

elo_par %>%     
    tibble::enframe() %>% 
    rename(param = name) %>% 
    readr::write_csv(
        here::here(
            "files",
            "params", 
            paste0(
                par_file_name,
                ".csv"
            )
        )
    )
