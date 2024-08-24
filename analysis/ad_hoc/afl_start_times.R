library(dplyr)
library(ggplot2)

# set start & finish seasons

season_first <- 2022
season_last <- 2024

# import data

afl_fixture <- purrr::map_df(
  .x = seq(
    from = season_first, 
    to = season_last, 
    by = 1
  ),
  .f = ~ fitzRoy::fetch_fixture_squiggle(season = .x) 
)

# create data frame of start time & count

afl_start_times_base <- afl_fixture %>% 
  filter(
    !(year == 2024 & round > 23)
    & !(year == 2023 & round > 24)
    & !(year == 2022 & round > 23)
  ) %>% 
  select(
    year, 
    round,
    date
  ) %>% 
  tidyr::separate(
    col = date,
    into = c("date", "time"),
    sep = " "
  ) %>% 
  mutate(
    time = stringr::str_sub(
      time,
      start = 1,
      end = 5
    )
  ) 

afl_start_times <- afl_start_times_base %>% 
  group_by(year) %>% 
  count(time) %>% 
  ungroup() 

# additional analysis

# new & gone in 2024

afl_start_times %>% 
  tidyr::pivot_wider(
    names_from = year,
    values_from = n
  ) %>% 
  tidyr::replace_na(
    list(
      `2022` = 0,
      `2023` = 0,
      `2024` = 0
    )
  ) %>% 
  mutate(
    time_classification = case_when(
      `2024` > 0 & `2023` == 0 & `2022` == 0 ~ "new in 2024",
      `2024` == 0 & `2023` > 0 & `2022` > 0  ~ "gone in 2024",
      `2024` > 0 & `2023` > 0 & `2022` > 0   ~ "appear in all seasons",
      T                                      ~ "other"
    )
  ) %>% 
  filter(time_classification == "new in 2024")
  # group_by(time_classification) %>% 
  # count() %>% 
  # ungroup()

afl_start_times_base %>% 
    filter(
        time == "20:30"
    )

# create chart

plot_title <- "AFL game start time frequency"
plot_subtitle <- "Regular season games only"
x_axis_title <- "Start time*"
y_axis_title <- "Number of games^"
plot_caption <- "*All times in AEST / AEDT
                ^2024 excludes the Round 24 floating fixture
                Created by: footycharts"

order_by_time <- F

afl_start_times %>% 
  ggplot(
    if (order_by_time) {
      aes(
        x = forcats::fct_rev(time), 
        y = n, 
        fill = factor(year)
      )
    } else {
      aes(
        x = forcats::fct_reorder(time, .x = n),
        y = n, 
        fill = factor(year)
      )
    }
  ) +
  geom_col(colour = "black") +
  geom_hline(yintercept = 0, alpha = 0.4) +
  facet_grid(. ~ year) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = x_axis_title,
    y = y_axis_title,
    caption = plot_caption
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20), 
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 15), 
    plot.caption = element_text(size = 10),
    strip.text = element_text(size = 15),
    legend.position = "none"
  )
