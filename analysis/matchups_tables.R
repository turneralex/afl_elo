library(dplyr)
library(gt)

# team logos

team_logos <- afl_elo %>% 
    distinct(team) %>% 
    mutate(
        logo = here::here(
            "files",
            "team_logos",
            paste0(
                team %>% 
                    stringr::str_remove_all(pattern = " ") %>% 
                    tolower(),
                ".png"
            )
        )
    )

# matchups tables

afl_elo_matchups <- afl_elo_pred %>% 
    inner_join(
        team_logos %>% 
            rename(home_logo = logo),
        by = c(
            "home_team" = "team"
        )
    ) %>% 
    inner_join(
        team_logos %>% 
            rename(away_logo = logo),
        by = c(
            "away_team" = "team"
        )
    ) %>% 
    mutate(
        matchup = paste0(
            home_team,
            " vs. ",
            away_team,
            " @ ",
            venue
        ),
        round_season = paste0(
            round,
            ", ",
            season
        ),
        home_elo = paste0(
            home_elo_rank,
            ": ",
            round(home_elo)
        ),
        away_elo = paste0(
            away_elo_rank,
            ": ",
            round(away_elo)
        ),
        pred_winner_win_prob = paste0(
            pred_winner_win_prob * 100,
            "%"
        )
    ) %>% 
    select(
        matchup,
        round_season,
        home_elo_rank,
        away_elo_rank,
        pred_winner,
        pred_winner_win_prob,
        pred_winner_margin,
        home_logo,
        away_logo
    )

matchups_tables <- purrr::map(
    .x = seq(
        from = 1,
        to = nrow(afl_elo_matchups),
        by = 1
    ),
    ~ afl_elo_matchups %>% 
        slice(.x) %>% 
        select(
            -matchup,
            -round_season,
            -home_logo,
            -away_logo
        ) %>% 
        gt() %>% 
        tab_header(
            title = afl_elo_matchups %>% 
                slice(.x) %>% 
                pull(matchup) %>% 
                paste0(
                    "**",
                    .,
                    "**"
                ) %>% 
                md(),
            subtitle = afl_elo_matchups %>% 
                slice(.x) %>% 
                pull(round_season) %>% 
                paste0(
                    "*",
                    .,
                    "*"
                ) %>% 
                md()
        ) %>% 
        tab_spanner(
            label = "Rankings",
            columns = c(
                home_elo_rank, 
                away_elo_rank
            )
        ) %>% 
        tab_spanner(
            label = "Match prediction",
            columns = c(
                pred_winner, 
                pred_winner_win_prob,
                pred_winner_margin
            )
        ) %>% 
        cols_label(
            home_elo_rank = html(
                local_image(
                    afl_elo_matchups %>% 
                        slice(.x) %>% 
                        pull(home_logo)
                )
            ),
            away_elo_rank = html(
                local_image(
                    afl_elo_matchups %>% 
                        slice(.x) %>% 
                        pull(away_logo)
                )
            ),
            pred_winner = "Winner", 
            pred_winner_win_prob = "Probability",
            pred_winner_margin = "Margin"
        ) %>% 
        cols_align(align = "center") %>% 
        tab_style(
            style = "vertical-align:middle",
            locations = cells_column_labels()
        ) 
)

# save tables

purrr::map2(
    .x = matchups_tables,
    .y = seq(
        from = 1,
        to = nrow(afl_elo_matchups),
        by = 1
    ),
    ~ gtsave(
        data = .x,
        filename = paste0(
            afl_elo_pred %>% 
                select(home_team) %>% 
                slice(.y) %>% 
                pull(),
            " vs. ",
            afl_elo_pred %>% 
                select(away_team) %>% 
                slice(.y) %>% 
                pull(),
            ".png"
        ),
        path = paste0(
            round_path,
            "/matchups"
        )
    )
)
