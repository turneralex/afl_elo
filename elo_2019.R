source("./functions_general.R")

library(tidyverse)
library(elo)

elo_model <- elo.run(
    home_score_adjusted ~ adjust(home_team, hga_app * elo_par["hga"]) + away_team + regress(season, 1500, elo_par["regress"]), 
    k = elo_par["k"],
    data = afl_elo
) 
