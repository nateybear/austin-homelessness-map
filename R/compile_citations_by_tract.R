## Compiles a panel dataset that contains citations by date for each census tract in Travis County
##
## Note that you need a Google API key to use this (see ggmap's github page)
library(ggmap)
register_google(key = read_file("api_secret.txt"))

library(tidyverse)
library(janitor)
library(magrittr)
library(crul)
library(rjson)


TX_OBSERVER_PATH <- "data/tx_observer_data.csv"
FCC_API_URL <- "https://geo.fcc.gov/api/census/area"
DACC_DATA_PREFIX <- "data/DACC_"
DACC_YEARS <- 2017:2019
AMC_DATA_PREFIX <- "data/AMC_"
AMC_YEARS <- 2015:2019


# Read these from Texas Observer's list and compile the unique offense descriptions used by AMC and DACC
offense_descriptions <-
  read_csv(TX_OBSERVER_PATH) %>%
  drop_na %>%
  clean_names %>%
  group_by(offense_description) %>%
  summarise(offense_code = unique(offense_code))


make_google_search <- function(street, cross_street) {
  normed_street <- ifelse(is.na(cross_street),
                          street,
                          paste(street, "and", cross_street))
  
  paste(normed_street, "Austin, TX")
}




# given a normed street name as column "google_search" attach a census tract
# uses the FCC's API to go from lat/lon to block FIPS (from which we can extract census tract)
# BLOCK FIPS STRUCTURE: 2-digit state, 3-digit county, 6-digit census tract, 4-digit block
add_census_tract <- function(df) {
  
  df %<>% mutate_geocode(google_search)
  
  urls <- paste0(FCC_API_URL, "?lat=", df$lat, "&lon=", df$lon)
  
  responses <- Async$new(urls = urls)$get()
  
  parsed_responses <- responses %>% sapply(function(resp) {
    resp$parse() %>% fromJSON
  })
  
  census_tracts <- parsed_responses %>% sapply(function(resp) {
    resp$results[[1]]$block_fips %>% substr(6, 11)
  })
  
  cbind(df, census_tract = census_tracts)
}

clean_dacc <- function(year) {
  filename <- paste0(DACC_DATA_PREFIX, year, ".csv")
  read_csv(filename) %>% 
    clean_names %>%
    mutate(date = strptime(offense_date, "%m/%d/%Y")) %>%
    rename(
      offense_description = charges_description,
      street = offense_street_name,
      cross_street = offense_cross_street
    ) 
}

clean_amc <- function(year) {
  filename <- paste0(AMC_DATA_PREFIX, year, ".csv")
  read_csv(filename) %>% 
    clean_names %>%
    mutate(date = strptime(offense_date, "%m/%d/%Y %H:%M:%S")) %>%
    rename(
      offense_description = offense_charge_description,
      street = offense_street_name,
      cross_street = offense_cross_street
    ) 
}

#' Title
#'
#' @param dataframe 
#'
#' @return
#' @export
#'
#' @examples
geocode_and_filter <- . %>%
  inner_join(offense_descriptions, c("offense_description")) %>%
  mutate(google_search = make_google_search(street, cross_street)) %>%
  add_census_tract %>%
  select(date, offense_description, offense_code, census_tract)

parse_dacc <- . %>% clean_dacc %>% geocode_and_filter
parse_amc <- . %>% clean_amc %>% geocode_and_filter

# preparing arguments for do.call (fs and years)
fs <- rep(
  c(parse_dacc, parse_amc), 
  c(length(DACC_YEARS), length(AMC_YEARS))
)
years <- c(DACC_YEARS, AMC_YEARS) %>% lapply(list)

output_df <- mapply(do.call, fs, years) %>% Reduce(rbind, .)

write_csv(output_df, "data/citations_by_tract.csv")