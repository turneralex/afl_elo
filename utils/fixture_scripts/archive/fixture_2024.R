library(dplyr)
source(
    here::here(
        "utils", 
        "functions_general.R"
    )
)

focus_season <- "2024"

# read in fixture

afl_fixture <- fitzRoy::fetch_fixture(season = focus_season) %>%
    transmute(
        season = as.character(compSeason.year),
        match_id = row_number(),
        round = paste("Round", round.roundNumber),
        date = stringr::str_sub(utcStartTime, start = 1, end = 10) %>% 
            lubridate::as_date(),
        venue = change_venue_name(venue.name),
        home_team = change_team_name(home.team.name),
        away_team = change_team_name(away.team.name)
    ) 

# get results for current season

afl_results <- fitzRoy::fetch_results_afl(season = focus_season) 

# available results check

if (!is.null(afl_results) & exists("rounds_so_far")) {
    
    message(
        paste0(
            "results available for round ",
            rounds_so_far,
            ":"
        )
    )
    
    print(
        afl_results %>% 
            filter(
                round.roundNumber == rounds_so_far
            ) %>% 
            select(match.name)
    )
    
}

# add results, if available (won't be to start a new season)

if (!is.null(afl_results)) {
    
    # add results
    
    afl_fixture <- afl_fixture %>% 
        mutate(
            season = focus_season, 
            match_id = 1:nrow(.)
        ) %>% 
        left_join(
            afl_results %>% 
                mutate(
                    home_team = change_team_name(match.homeTeam.name),
                    round = paste("Round", round.roundNumber),
                ) %>% 
                select(
                    round,
                    home_team,
                    home_goals = homeTeamScore.matchScore.goals,
                    home_behinds = homeTeamScore.matchScore.behinds,
                    home_score = homeTeamScore.matchScore.totalScore,
                    away_goals = awayTeamScore.matchScore.goals,
                    away_behinds = awayTeamScore.matchScore.behinds,
                    away_score = awayTeamScore.matchScore.totalScore
                ),
            by = c("round", "home_team")
        )
    
} else {
    
    afl_fixture <- afl_fixture %>% 
        mutate(
            home_goals = NA,
            home_behinds = NA,
            home_score = NA,
            away_goals = NA,
            away_behinds = NA,
            away_score = NA
        )
    
}

# add location

afl_venues <- afl_fixture %>% 
    distinct(venue) %>% 
    mutate(
        location = venue_location(venue)
    )

# create table of teams & home venues

venues_teams <- purrr::map(
    .x = afl_fixture %>% 
        distinct(venue) %>% 
        pull(),
    ~ afl_fixture %>% 
        filter(venue == .x) %>% 
        distinct(home_team) %>% 
        select(team = home_team)
) %>% 
    purrr::set_names(
        nm = afl_fixture %>% 
            distinct(venue) %>% 
            pull()
    ) %>% 
    tibble::enframe(
        name = "venue", 
        value = "teams"
    ) %>% 
    mutate(venue = venue)

# add location

afl_venues <- afl_venues %>% 
    left_join(
        venues_teams, 
        by = "venue"
    ) %>% 
    mutate(year = focus_season) %>% 
    select(year, everything())

# write files

afl_fixture %>%
    readr::write_csv(
        here::here(
            "files", 
            "fixtures", 
            paste0(
                "afl_fixture_",
                focus_season,
                ".csv"
            )
        )
    )

afl_venues %>%
    tidyr::unnest(cols = teams) %>%
    readr::write_csv(
        here::here(
            "files", 
            "venues", 
            paste0(
                "afl_venues_",
                focus_season,
                ".csv"
            )
        )
    )

rm(afl_results)
