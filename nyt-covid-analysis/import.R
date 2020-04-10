library(tidyverse)

## submodule directory name:
dir_name <- "covid-19-data"

## directory to hold our data tables:
if (!dir.exists("data")) {
  dir.create("data")
}

## states ----
## csv files are one level up from user_area:
nyt_states <- readr::read_csv(paste0(dir_name, "/us-states.csv"))
save(nyt_states, file = "data/nyt_states.rda")

## counties ----

nyt_counties <- readr::read_csv(paste0(dir_name, "/us-counties.csv"))
save(nyt_counties, file = "data/nyt_counties.rda")

