# instructions:
# 1. shift elo_par.csv to archive
# 2. run script
# 3. update params_input.txt

library(dplyr)
library(foreach)
library(doParallel)

source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)

score_expected <- function(elo, elo_opp, hga_app, hga) {
    x <- -(elo - elo_opp + (hga_app * hga))
    y <- 1 / (1 + 10^(x / 400))
    
    return(y)
}

par_file_name <- "elo_par_new"

afl_venues_history <- readr::read_csv(
    here::here(
        "files",
        "venues",
        "afl_venues_history.csv"
    ),
    col_types = "ccc"
) %>% 
    tidyr::nest(
        data_teams = c(team, total_flag, current_flag)
    )

afl_fixture_history <- readr::read_csv(
    here::here(
        "files",
        "fixtures",
        "afl_fixture_history.csv"
    ),
    col_types = "ciicciDccciiiiii"
) %>% 
    mutate(match_id = 1:nrow(.))

afl_elo <- afl_fixture_history %>% 
    left_join(
        afl_venues_history %>% 
            distinct(
                venue,
                location
            ),
        by = "venue"
    ) %>% 
    mutate(
        match_id =  1:nrow(.),
        # share of scoring shots
        home_score_adjusted = (home_goals + home_behinds) / (home_goals + home_behinds + away_goals + away_behinds),
        hga_app = purrr::pmap_int(
            .l = list(
                venue, 
                home_team, 
                away_team,
                season
            ), 
            .f = is_home, 
            data_venues = afl_venues_history
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
    ) 

registerDoParallel(
    makeCluster(
        detectCores() - 1
    )
)

parameter_optim <- function(elo_df, par) {
    
    library(magrittr)
    
    all_games <- unique(elo_df$match_id)
    
    foreach(i = 1:(length(all_games))) %do% {
        
        game <- elo_df[elo_df$match_id == i, ] 
        
        current_season <- game$season[1]
        
        row_id <- which(elo_df$match_id == i)[1]
        
        next_season_home <- data.table::as.data.table(elo_df) %>% 
            .[row_id:nrow(.)] %>% 
            .[, season := lead(season, n = 1, default = current_season), by = team] %>% 
            .[team == game[1, "team"]] %>% 
            .$season %>% 
            .[1]
        
        if (current_season == next_season_home) {
            regress_app_home <- 0
        } else {
            regress_app_home <- 1
        }
        
        next_season_away <- data.table::as.data.table(elo_df) %>% 
            .[row_id:nrow(.)] %>% 
            .[, season := lead(season, n = 1, default = current_season), by = team] %>% 
            .[team == game[2, "team"]] %>% 
            .$season %>% 
            .[1]
        
        if (current_season == next_season_away) {
            regress_app_away <- 0
        } else {
            regress_app_away <- 1
        }
        
        location <- game$location[1]
        
        if (location == "VIC") {
            hga <- par[3]
        } else if (location == "NSW") {
            hga <- par[4]
        } else if (location == "QLD") {
            hga <- par[5]
        } else if (location == "SA") {
            hga <- par[6]
        } else if (location == "WA") {
            hga <- par[7]
        } else if (location == "GEE") {
            hga <- par[8]
        } else if (location == "TAS") {
            hga <- par[9]
        } else if (location == "ACT") {
            hga <- par[10]
        } else {
            hga <- 0
        }
        
        score_expected_home <- score_expected(
            game[1, "start_elo"], 
            game[2, "start_elo"], 
            hga_app = game[1, "hga_app"], 
            hga = hga
        )
        
        score_expected_away <- score_expected(
            game[2, "start_elo"], 
            game[1, "start_elo"], 
            hga_app = game[2, "hga_app"], 
            hga = hga
        )
        
        elo_df[elo_df$match_id == i, "score_expected"] <- c(score_expected_home, score_expected_away) %>%
            unlist()
        
        new_elo_home <- elo_update(
            elo = game[1, "start_elo"],
            score_adjusted = game[1, "score_adjusted"],
            score_expected = score_expected_home,
            k = par[1],
            regress = par[2],
            regress_app = regress_app_home
        )
        
        new_elo_away <- elo_update(
            elo = game[2, "start_elo"],
            score_adjusted = game[2, "score_adjusted"],
            score_expected = score_expected_away,
            k = par[1],
            regress = par[2],
            regress_app = regress_app_away
        )
        
        elo_df[elo_df$match_id == i, "new_elo"] <- c(new_elo_home, new_elo_away) %>%
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
    par = c(50, 0.5, rep(10, 8)),
    lower = rep(0, 12),
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
            "hga_act"
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
