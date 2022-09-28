library(tidyverse)
library(scales)
library(ggradar)
library(skimr)
library(fitzRoy)

season <- 2022
round_start <- 1
round_end <- 10
finals_exclude <- c(
    # "Finals Week 1", 
    "Semi Finals", 
    "Preliminary Finals", 
    "Grand Final"
)
round_current <- 11
round_name <- "Round 11"

afl_stats_all <- fetch_player_stats_afl(season = season) 

afl_stats_all %>% 
    glimpse()

colnames(afl_stats_all) <- colnames(afl_stats_all) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

afl_stats_all %>% 
    skim()

afl_stats_all 

matchups <- afl_stats_all %>% 
    filter(
        if (is.numeric(round_current)) {
            round_roundnumber == round_current
        } else {
            round_name == round_current
        }
    ) %>% 
    select(
        home_team_club_name,
        away_team_club_name
    ) %>% 
    distinct()

matchups

team_stats <- afl_stats_all %>% 
    filter(
        round_roundnumber %in% round_start:round_end
        & !(round_name %in% finals_exclude)
    ) %>% 
    group_by(round_roundnumber, home_team_club_name, away_team_club_name) %>% 
    mutate(
        score_shots_home = if_else(
            team_name == home_team_club_name,
            goals + behinds,
            0
        ) %>% 
            sum(),
        score_shots_away = if_else(
            team_name == away_team_club_name,
            goals + behinds,
            0
        ) %>% 
            sum(),
        i50_home = if_else(
            team_name == home_team_club_name,
            inside50s,
            0
        ) %>% 
            sum(),
        i50_away = if_else(
            team_name == away_team_club_name,
            inside50s,
            0
        ) %>% 
            sum(),
        mi50_home = if_else(
            team_name == home_team_club_name,
            marksinside50,
            0
        ) %>% 
            sum(),
        mi50_away = if_else(
            team_name == away_team_club_name,
            marksinside50,
            0
        ) %>% 
            sum(),
        turn_home = if_else(
            team_name == home_team_club_name,
            turnovers,
            0
        ) %>% 
            sum(),
        turn_away = if_else(
            team_name == away_team_club_name,
            turnovers,
            0
        ) %>% 
            sum(),
        cp_home = if_else(
            team_name == home_team_club_name,
            contestedpossessions,
            0
        ) %>% 
            sum(),
        cp_away = if_else(
            team_name == away_team_club_name,
            contestedpossessions,
            0
        ) %>% 
            sum(),
        pa_home = if_else(
            team_name == home_team_club_name,
            extendedstats_pressureacts,
            0
        ) %>% 
            sum(),
        pa_away = if_else(
            team_name == away_team_club_name,
            extendedstats_pressureacts,
            0
        ) %>% 
            sum()
    ) %>% 
    group_by(round_name, team_name) %>% 
    summarise(
        score_shots_total = sum(goals + behinds),
        score_shots_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                score_shots_away,
                score_shots_home
            )
        ),
        score_shots_diff = score_shots_total - score_shots_opp_total,
        i50_total = sum(inside50s),
        i50_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                i50_away,
                i50_home
            )
        ),
        i50_diff = i50_total - i50_opp_total,
        
        mi50_total = sum(marksinside50),
        mi50_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                mi50_away,
                mi50_home
            )
        ),
        mi50_diff = mi50_total - mi50_opp_total,
        
        turn_total = sum(turnovers),
        turn_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                turn_away,
                turn_home
            )
        ),
        turn_diff = turn_total - turn_opp_total,
        
        cp_total = sum(contestedpossessions),
        cp_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                cp_away,
                cp_home
            )
        ),
        cp_diff = cp_total - cp_opp_total,
        
        pa_total = sum(extendedstats_pressureacts),
        pa_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                pa_away,
                pa_home
            )
        ),
        pa_diff = pa_total - pa_opp_total
    ) %>% 
    rename(team = team_name) %>% 
    group_by(team) %>% 
    summarise(
        score_shots_diff_mean = mean(score_shots_diff),
        i50_diff_mean = mean(i50_diff),
        mi50_diff_mean = mean(mi50_diff),
        turn_diff_mean = mean(turn_diff),
        cp_diff_mean = mean(cp_diff),
        pa_diff_mean = mean(pa_diff)
    ) %>% 
    mutate(
        team = case_when(
            team == "Adelaide Crows"    ~ "Adelaide",
            team == "Western Bulldogs"  ~ "Footscray",
            team == "Geelong Cats"      ~ "Geelong",
            team == "Gold Coast Suns"   ~ "Gold Coast",
            team == "GWS Giants"        ~ "GWS",
            team == "Sydney Swans"      ~ "Sydney",
            team == "West Coast Eagles" ~ "West Coast",
            T                           ~ team
        )
    ) %>% 
    mutate_at(vars(-team), rescale)

team_stats %>% 
    head()

afl_ladder_all <- fetch_ladder_afltables(season = season)

afl_ladder_all %>% 
    glimpse()

colnames(afl_ladder_all) <- colnames(afl_ladder_all) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

afl_ladder_all %>% 
    skim()

afl_ladder <- afl_ladder_all %>% 
    filter(
        round_number == round_end
    ) %>% 
    select(team, ladder_position) 

afl_ladder

team_stats <- team_stats %>% 
    inner_join(
        afl_ladder,
        by = "team"
    ) %>% 
    mutate(
        team_position = case_when(
            ladder_position == 1 ~ paste0(ladder_position, "st: ", team),
            ladder_position == 2 ~ paste0(ladder_position, "nd: ", team),
            ladder_position == 3 ~ paste0(ladder_position, "rd: ", team),
            T ~                    paste0(ladder_position, "th: ", team)
        )
    ) 

game <- paste(
    season,
    round_name,
    sep = ": "
)

game

map(
    1:nrow(matchups),
    ~ team_stats %>% 
        inner_join(
            matchups %>% 
                slice(.x) %>% 
                pivot_longer(
                    cols = 1:2,
                    names_to = "home_away",
                    values_to = "team"
                ) %>% 
                select(team),
            by = "team"
        ) %>% 
        select(
            Team = team_position, 
            `Scoring shots` = score_shots_diff_mean, 
            `Inside 50s` = i50_diff_mean, 
            `Marks inside 50` = mi50_diff_mean,
            `Turnovers` = turn_diff_mean, 
            `Contested possessions` = cp_diff_mean, 
            `Pressure acts` = pa_diff_mean
        ) %>% 
        ggradar(values.radar = c(NA, NA, NA),
                axis.label.size = 5,
                axis.label.offset = 1.1,
                group.line.width = 2,
                group.point.size = 8) +
        scale_colour_brewer(palette = "Set2") +
        labs(title = paste0(
            as.character(matchups[.x, 1]),
            " vs. ",
            as.character(matchups[.x, 2])
           ),
            subtitle = paste0(
                game,
                "\nPosition indicates relative strength vs. all other teams in the competition*"
            ),
            caption = "*Closer to the outside indicates greater strength. Touching the outer circle represents top of the league, touching the inner circle represents bottom of the league
                        Source: AFL website. Created by: footycharts") +
        theme(plot.title = element_text(size = 20), 
              plot.subtitle = element_text(size = 15),
              legend.position = "bottom",
              plot.caption = element_text(size = 10)) 
)
