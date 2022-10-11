# source(here::here("utils/fixture scripts/fixture_2023.R"))
source(here::here("utils/fixture scripts/fixture_all.R"))
source(here::here("utils/functions_general.R"))

elo_par <- read_csv(
    here::here(
        "files",
        "params",
        list.files("./files/params")
    )
) %>% 
    deframe() 

afl_elo <- afl_fixture_all %>% 
    convert_elo_df() %>% 
    elo_run(
        k = elo_par["k"], 
        hga = elo_par[3:10], 
        regress = elo_par["regress"]
    )
