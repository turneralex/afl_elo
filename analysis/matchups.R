library(tidyverse)
library(scales)
library(ggradar)
library(skimr)
library(fitzRoy)

afl_stats_all <- fetch_player_stats_afl(season = current_season) 

colnames(afl_stats_all) <- colnames(afl_stats_all) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

afl_stats_all %>% 
    skim() %>% 
    print()

team_stats <- afl_stats_all %>% 
    filter(
        round_roundnumber %in% 1:rounds_so_far
        & !(round_name %in% finals_so_far)
    ) %>% 
    group_by(round_roundnumber, home_team_club_name, away_team_club_name) %>% 
    mutate(
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
        clear_home = if_else(
            team_name == home_team_club_name,
            clearances_totalclearances,
            0
        ) %>% 
            sum(),
        clear_away = if_else(
            team_name == away_team_club_name,
            clearances_totalclearances,
            0
        ) %>% 
            sum(),
        tackles_home = if_else(
            team_name == home_team_club_name,
            tackles,
            0
        ) %>% 
            sum(),
        tackles_away = if_else(
            team_name == away_team_club_name,
            tackles,
            0
        ) %>% 
            sum()
    ) %>% 
    group_by(round_name, team_name) %>% 
    summarise(
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
        
        clear_total = sum(clearances_totalclearances),
        clear_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                clear_away,
                clear_home
            )
        ),
        clear_diff = clear_total - clear_opp_total,
        
        tackles_total = sum(tackles),
        tackles_opp_total = mean(
            if_else(
                team_name == home_team_club_name,
                tackles_away,
                tackles_home
            )
        ),
        tackles_diff = tackles_total - tackles_opp_total
    ) %>% 
    rename(team = team_name) %>% 
    group_by(team) %>% 
    summarise(
        i50_diff_mean = mean(i50_diff),
        mi50_diff_mean = mean(mi50_diff),
        turn_diff_mean = mean(-turn_diff),
        cp_diff_mean = mean(cp_diff),
        clear_diff_mean = mean(clear_diff),
        tackles_diff_mean = mean(tackles_diff)
    ) %>% 
    mutate(
        team = change_team_name(team)
    ) %>% 
    mutate_at(
        vars(i50_diff_mean:tackles_diff_mean), 
        rescale
    )

afl_ladder_all <- fetch_ladder_afltables(season = current_season)

colnames(afl_ladder_all) <- colnames(afl_ladder_all) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

afl_ladder_all %>% 
    skim() %>% 
    print()

paste(
    "latest round available for afltables ladder:",
    afl_ladder_all %>% 
        pull(round_number) %>% 
        max()
) 

afl_ladder <- afl_ladder_all %>% 
    mutate(
        team = change_team_name(team)
    ) %>% 
    filter(
        round_number == rounds_so_far
    ) %>% 
    select(team, ladder_position) 

afl_ladder %>% 
    print()

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
            T                    ~ paste0(ladder_position, "th: ", team)
        )
    ) 

afl_elo_pred <- afl_elo %>% 
    filter(season == current_season, 
           round == paste("Round", rounds_so_far + 1)) %>% 
    inner_join(
        afl_ladder,
        by = "team"
    ) %>% 
    mutate(
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
        away_team = lead(team),
        away_elo = lead(start_elo),
        away_ladder_position = lead(ladder_position),
        away_elo_rank = lead(elo_rank)
    ) %>% 
    slice(1) %>%
    ungroup() %>%
    select(
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
            home_elo_rank, " (", round(home_elo), ") vs. ", away_elo_rank, " (", round(away_elo), ")",
            "\nHGA: +", round(hga), ", predicted winner: ", pred_winner
        )
    ) %>% 
    select(home_team, away_team, venue, matchup)

charts <- map(
    1:nrow(afl_elo_pred),
    ~ team_stats %>% 
        inner_join(
            afl_elo_pred %>% 
                slice(.x) %>% 
                select(home_team:away_team) %>% 
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
            team = fct_reorder(
                team,
                team_home
            )
        ) %>%
        select(
            Team = team, 
            `Inside 50s` = i50_diff_mean, 
            `Marks inside 50` = mi50_diff_mean, 
            `Turnovers` = turn_diff_mean, 
            `Contested possessions` = cp_diff_mean, 
            `Clearances` = clear_diff_mean, 
            `Tackles` = tackles_diff_mean
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
            as.character(afl_elo_pred[.x, 3])
           ),
            subtitle = paste0(
                afl_elo_pred[.x, 4],
                "\n\nTeam differentials vs. opponents - closer to the outside indicates greater strength"
            ),
            caption = "Created by: footycharts. Source: AFL website") +
        theme(plot.title = element_text(size = 20, hjust = 0.5), 
              plot.subtitle = element_text(size = 15, hjust = 0.5),
              legend.position = "bottom",
              plot.caption = element_text(size = 10)) 
)

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
            current_season,
            "_",
            round_name
        ),
        width = 25,
        height = 25,
        units = "cm"
    )
)
