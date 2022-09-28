library(tidyverse)
library(scales)
library(ggradar)
library(skimr)
library(fitzRoy)

source(here::here("run/update.R"))

season <- 2022
rounds_so_far <- 10
finals_so_far <- c(
    # "Finals Week 1"
    # "Semi Finals", 
    # "Preliminary Finals", 
    # "Grand Final"
)
round_next <- 11
round_name <- "Round 11"

afl_stats_all <- fetch_player_stats_afl(season = season) 

afl_stats_all %>% 
    glimpse()

colnames(afl_stats_all) <- colnames(afl_stats_all) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

afl_stats_all %>% 
    skim()

afl_stats_all %>% 
    head()

team_stats <- afl_stats_all %>% 
    filter(
        round_roundnumber %in% 1:rounds_so_far
        & !(round_name %in% finals_so_far)
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
        turn_diff_mean = mean(-turn_diff),
        cp_diff_mean = mean(cp_diff),
        pa_diff_mean = mean(pa_diff)
    ) %>% 
    mutate(
        team_afl_tables = case_when(
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
    mutate_at(
        vars(score_shots_diff_mean:pa_diff_mean), 
        rescale
    )

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
        round_number == rounds_so_far
    ) %>% 
    select(team, ladder_position) 

afl_ladder

team_stats <- team_stats %>% 
    inner_join(
        afl_ladder,
        by = c("team_afl_tables" = "team")
    ) %>% 
    mutate(
        team_position = case_when(
            ladder_position == 1 ~ paste0(ladder_position, "st: ", team),
            ladder_position == 2 ~ paste0(ladder_position, "nd: ", team),
            ladder_position == 3 ~ paste0(ladder_position, "rd: ", team),
            T ~                    paste0(ladder_position, "th: ", team)
        )
    ) 

afl_elo_pred <- afl_elo %>% 
    filter(season == current_season, 
           round == paste("Round", rounds_so_far + 1)) %>% 
    mutate(
        team_afl_tables = case_when(
            team == "Western Bulldogs"       ~ "Footscray",
            team == "Greater Western Sydney" ~ "GWS",
            T                                ~ team
        )
    ) %>% 
    inner_join(
        afl_ladder,
        by = c("team_afl_tables" = "team")
    ) %>% 
    mutate(
        team_chart = team,
        team = case_when(
            team == "Adelaide"               ~ "Adelaide Crows",
            team == "Geelong"                ~ "Geelong Cats",
            team == "Gold Coast"             ~ "Gold Coast Suns",
            team == "Greater Western Sydney" ~ "GWS Giants",
            team == "Sydney"                 ~ "Sydney Swans",
            team == "West Coast"             ~ "West Coast Eagles",
            T                                ~ team
        ),
        elo_rank = case_when(
            rank(-start_elo) == 1 ~ paste0(rank(-start_elo), "st"),
            rank(-start_elo) == 2 ~ paste0(rank(-start_elo), "nd"),
            rank(-start_elo) == 3 ~ paste0(rank(-start_elo), "rd"),
            T ~                     paste0(rank(-start_elo), "th")
        ),
        ladder_position = case_when(
            ladder_position == 1 ~ paste0(ladder_position, "st"),
            ladder_position == 2 ~ paste0(ladder_position, "nd"),
            ladder_position == 3 ~ paste0(ladder_position, "rd"),
            T ~                    paste0(ladder_position, "th")
        )
    ) %>% 
    group_by(match_id) %>%
    mutate(
        away_team_chart = lead(team_chart),
        away_team = lead(team),
        away_elo = lead(start_elo),
        away_ladder_position = lead(ladder_position),
        away_elo_rank = lead(elo_rank)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    select(
        home_team_chart = team_chart,
        away_team_chart = away_team_chart,
        location, 
        home_team = team, 
        away_team, 
        venue, 
        hga_app, 
        home_elo = start_elo, 
        away_elo, 
        score_expected,
        home_elo_rank = elo_rank,
        away_elo_rank,
        home_ladder_position = ladder_position,
        away_ladder_position
    )  %>% 
    mutate(
        game_id = row_number(),
        hga = case_when(
            location == "VIC" ~ hga_app * elo_par["hga_vic"],
            location == "NSW" ~ hga_app * elo_par["hga_nsw"],
            location == "QLD" ~ hga_app * elo_par["hga_qld"],
            location == "SA"  ~ hga_app * elo_par["hga_sa"],
            location == "WA"  ~ hga_app * elo_par["hga_wa"],
            location == "GEE" ~ hga_app * elo_par["hga_gee"],
            location == "TAS" ~ hga_app * elo_par["hga_tas"],
            T                 ~ hga_app * elo_par["hga_other"]
        ),
        elo_diff_hga = home_elo - away_elo + hga,
        pred_winner = if_else(
            elo_diff_hga > 0,
            home_team,
            away_team
        ),
        matchup = paste0(
            "Ladder position\n",
            home_ladder_position, " vs. ", away_ladder_position,
            "\n\nElo ranking\n",
            home_elo_rank, " vs. ", away_elo_rank,
            "\n\nElo rating\n",
            round(home_elo), " vs. ", round(away_elo), 
            "\n\nHome ground advantage: +", round(hga),
            "\nPredicted winner: ",
            pred_winner
        )
    ) %>% 
    select(home_team_chart, away_team_chart, home_team, away_team, venue, matchup)

afl_elo_pred

dir.create(
    paste0(
        here::here("files/charts/"),
        season,
        "_",
        round_name
    )
)

charts <- map(
    1:nrow(afl_elo_pred),
    ~ team_stats %>% 
        inner_join(
            afl_elo_pred %>% 
                slice(.x) %>% 
                select(3:4) %>% 
                pivot_longer(
                    cols = 1:2,
                    names_to = "home_away",
                    values_to = "team"
                ) %>% 
                select(team) %>% 
                mutate(
                    team_home = 0:1
                ),
            by = "team"
        ) %>%
        mutate(
            team = case_when(
                team == "Adelaide Crows"    ~ "Adelaide",
                team == "Geelong Cats"      ~ "Geelong",
                team == "Gold Coast Suns"   ~ "Gold Coast",
                team == "GWS Giants"        ~ "Greater Western Sydney",
                team == "Sydney Swans"      ~ "Sydney",
                team == "West Coast Eagles" ~ "West Coast",
                T                           ~ team
            ),
            team = fct_reorder(
                team,
                team_home
            )
        ) %>% 
        select(
            Team = team, 
            `Scoring shots` = score_shots_diff_mean, 
            `Inside 50s` = i50_diff_mean, 
            `Marks inside 50` = mi50_diff_mean,
            `Turnovers` = turn_diff_mean, 
            `Contested\npossessions` = cp_diff_mean, 
            `Pressure acts` = pa_diff_mean
        ) %>% 
        ggradar(values.radar = c(NA, NA, NA),
                axis.label.size = 5,
                axis.label.offset = 1.1,
                group.line.width = 2,
                group.point.size = 8) +
        scale_colour_brewer(palette = "Set2") +
        labs(title = paste0(
            round_name,
            ": ",
            as.character(afl_elo_pred[.x, 1]),
            " vs. ",
            as.character(afl_elo_pred[.x, 2]),
            " @ ",
            as.character(afl_elo_pred[.x, 5])
           ),
            subtitle = paste0(
                afl_elo_pred[.x, 6],
                "\n\nTeam strengths & weaknesses*"
            ),
            caption = "*Closer to the outside indicates greater strength
                       *Touching the outer circle represents top of the league, touching the inner circle represents bottom of the league
                       Source: AFL website. Created by: footycharts") +
        theme(plot.title = element_text(size = 20, hjust = 0.5), 
              plot.subtitle = element_text(size = 15, hjust = 0.5),
              legend.position = "bottom",
              plot.caption = element_text(size = 10)) 
)

charts

map2(
    charts,
    1:nrow(afl_elo_pred),
    ~ ggsave(
        plot = .x,
        filename = paste0(
            round_name,
            ": ",
            as.character(afl_elo_pred[.y, 1]),
            " vs. ",
            as.character(afl_elo_pred[.y, 2]),
            ".png"
        ),
        path = paste0(
            here::here("files/charts/"),
            season,
            "_",
            round_name
        ),
        width = 1080,
        height = 1080,
        units = "mm"
    )
)
