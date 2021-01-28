## Compiles a panel dataset that contains citations by date for each census tract in Travis County
## 
## Note that you need a Google API key to use this (see ggmap's github page)
library(tidyverse)
library(janitor)
library(ggmap)
library(magrittr)
library(crul)
library(rjson)
register_google(key = read_file("../api_secret.txt"))

# Read these from Texas Observer's list and compile the unique offense descriptions used by AMC and DACC
offense_descriptions <- read_csv("../data/tx_observer_data.csv") %>% 
  drop_na %>% 
  clean_names %>% 
  group_by(offense_description) %>% 
  summarise(offense_code = unique(offense_code))

# the CSV has streets and cross street, they need to be normed
make_google_search <- function(street, cross_street) {
  normed_street <- ifelse(
    is.na(cross_street), 
    street, 
    paste(street, "and", cross_street))
  
  paste(normed_street, "Austin, TX")
}

# given a normed street name as column "google_search" attach a census tract
# uses the FCC's API to go from lat/lon to block FIPS (from which we can extract census tract)
add_census_tract <- function(df) {
  df %<>% mutate_geocode(google_search)
  urls <- paste0("https://geo.fcc.gov/api/census/area?lat=",df$lat, "&lon=",df$lon)
  tracts <- Async$new(urls=urls)$get() %>% sapply(function(resp){
    resp$parse() %>% fromJSON %>% {.$results[[1]]$block_fips} %>% substr(6, 11)
  })
  cbind(df, census_tract = tracts)
}



