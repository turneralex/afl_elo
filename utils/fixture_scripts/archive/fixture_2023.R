library(dplyr)

focus_season <- "2023"

# read in fixture

afl_fixture <- fitzRoy::fetch_fixture_footywire(season = focus_season) %>% 
    mutate(
        Date = lubridate::as_date(Date),
        Season = as.character(Season),
        match_id = Season.Game,
        Round = paste("Round", Round),
        Home.Team = change_team_name(Home.Team),
        Away.Team = change_team_name(Away.Team),
    ) %>% 
    select(
        Season, 
        match_id, 
        Round, 
        Date, 
        Venue, 
        Home.Team, 
        Away.Team
    )

# update column names

afl_fixture <- afl_fixture %>% 
    rename_with(
        .fn = ~ .x %>% 
            tolower() %>% 
            stringr::str_replace(
                pattern = "\\.", 
                replacement = "_"
            )
    )

# update venue names

afl_fixture <- afl_fixture %>% 
    mutate(
        venue = change_venue_name(venue)
    ) 

# get results for current season

afl_results <- fitzRoy::fetch_results_afl(season = focus_season) 

# available results check

if (exists("rounds_so_far")) {
    
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
