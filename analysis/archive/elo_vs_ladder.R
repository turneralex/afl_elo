source(here::here("run/run.R"))

library(viridis)

rounds_so_far <- 23

# current ladder position

afl_ladder <- fitzRoy::fetch_ladder_afltables(season = as.integer(current_season))

colnames(afl_ladder) <- colnames(afl_ladder) %>% 
    tolower() %>% 
    str_replace_all("[.]", "_")

afl_ladder <- afl_ladder %>% 
    filter(round_number == rounds_so_far) %>% 
    mutate(
        team = case_when(
            team == "Footscray" ~ "Western Bulldogs",
            team == "GWS"       ~ "Greater Western Sydney",
            T                   ~ team
        )
    ) %>%
    select(team, ladder_position)

afl_ladder

# current elo position

afl_elo_rank <- afl_elo %>% 
    filter(season == current_season,
           round %in% paste("Round", 1:rounds_so_far)) %>% 
    group_by(team) %>% 
    slice(n()) %>% 
    ungroup() %>% 
    select(team, new_elo) %>% 
    arrange(-new_elo) %>% 
    mutate(
        plus_minus = new_elo - 1500,
        elo_position = row_number(),
        elo_position_team = paste0(
            elo_position,
            ": ",
            team
        )
    ) 

afl_elo_rank

# elo positon vs. ladder position

afl_ladder %>%
    inner_join(
        afl_elo_rank,
        by = "team"
    ) %>%
    select(team, ladder_position, elo_position) %>%
    arrange(elo_position) %>%
    mutate(rank = row_number()) %>%
    pivot_longer(
        cols = c(
            "ladder_position",
            "elo_position"
        )
    ) %>%
    mutate(
        name = if_else(
            name == "ladder_position",
            "Ladder position",
            "Elo rank"
        ),
        value = abs(value - 19)
    ) %>%
    ggplot(aes(fct_reorder(team, -rank), value, fill = name, label = abs(value - 19))) +
    geom_vline(xintercept = factor("Carlton"), alpha = 0.6) +
    geom_label(size = 10) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Final team elo rank vs. ladder position",
         subtitle = paste("Season:", current_season),
         x = "Team",
         caption = "Created by: footycharts") +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 20),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
          legend.text = element_text(size = 15),
          legend.position = "bottom") +
    guides(
        fill = guide_legend(
            title = "",
            override.aes = aes(label = "")
        )
    )
