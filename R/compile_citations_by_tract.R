## Compiles a panel dataset that contains citations by date for each census tract in Travis County
##
## Note that you need a Google API key to use this (see ggmap's github page).
## Be careful, as this could incur charges! This is why we use caches in the form of csv files
library(ggmap)
library(tidyverse)
library(janitor)
library(magrittr)
library(crul)
library(rjson)

register_google(key = read_file("api_secret.txt"))

TX_OBSERVER_PATH <- "data/tx_observer_data.csv"
FCC_API_URL <- "https://geo.fcc.gov/api/census/area"
GOOGLE_CACHE_PATH <- "data/google_cache.csv"
CENSUS_CACHE_PATH <- "data/census_cache.csv"
DACC_DATA_PREFIX <- "data/DACC_"
DACC_YEARS <- 2017:2019
AMC_DATA_PREFIX <- "data/AMC_"
AMC_YEARS <- 2015:2019




########### HELPER DATA AND FUNCTIONS #############


# Going to use Texas Observer's data as a Source Of Truth for which citations we're looking for
offense_descriptions <-
  read_csv(TX_OBSERVER_PATH, col_types = cols()) %>%
  drop_na %>%
  clean_names %>%
  group_by(offense_description) %>%
  summarise(offense_code = unique(offense_code))


# give me street and cross streets from your dataset and I'll give you something you can Google search
make_google_search <- function(street, cross_street) {
  normed_street <- 
    ifelse(is.na(cross_street),
      street,
      paste(street, "and", cross_street))
  
  paste(normed_street, "Austin, TX")
}

# cache a long-running operation at the given path.
# ... represents the columns that determine the cache hit.
cached <- function(mutation, path, ...) {
  function(df) {
    cache <- read_csv(path, col_types = cols())
    new_entries <- anti_join(df, cache, ...) %>% select(...) %>% distinct
    
    if (nrow(new_entries) > 0) {
      new_entries %<>% mutation
      
      # warn about NAs from the mutation. drop them and exclude from cache. but tell user we're doing that
      NAs <- apply(is.na(new_entries), 1, any)
      if (any(NAs)) {
        warning("The following entries returned NAs. THEY WILL BE DROPPED\n", new_entries[NAs,])
        new_entries %<>% drop_na
      }
      
      cache %<>% rbind(new_entries)
      write_csv(cache, path)
    }
    
    return(inner_join(df, cache, c(...)))
  }
}

add_lat_lon <- cached(. %>% mutate_geocode(google_search), GOOGLE_CACHE_PATH, "google_search")


# give me a dataframe with latitude and longitude, and
# 1. use the lat/lon to make an HTTP request to FCC's Block FIPS API
# 2. parse the returned 15-digit block FIPS for the 6-digit census tract
# 3. return the original dataset with a new column of census tract #s
.add_census_tract <- function(df) {
  urls <- paste0(FCC_API_URL, "?lat=", df$lat, "&lon=", df$lon)
  
  responses <- Async$new(urls = urls)$get()
  
  parsed_responses <- responses %>% lapply(function(resp) {
    resp$parse() %>% fromJSON
  })
  
  census_tracts <- parsed_responses %>% sapply(function(resp) {
    if (is.null(resp$status))
      resp$results[[1]]$block_fips %>% substr(6, 11)
    else
      NA
  })
  
  cbind(df, census_tract = census_tracts)
}

add_census_tract <- cached(.add_census_tract, CENSUS_CACHE_PATH, "lat", "lon")






######## PIPELINE #############


# give me a year for a DACC dataset and I'll open the CSV and clean to standardized format
clean_dacc <- function(year) {
  filename <- paste0(DACC_DATA_PREFIX, year, ".csv")
  read_csv(filename, col_types = cols()) %>%
    clean_names %>%
    mutate(date = strptime(offense_date, "%m/%d/%y")) %>%
    rename(
      offense_description = charges_description,
      street = offense_street_name,
      cross_street = offense_cross_street
    )
}


# give me a year for an AMC dataset and I'll open the CSV and clean to standardized format
clean_amc <- function(year) {
  filename <- paste0(AMC_DATA_PREFIX, year, ".csv")
  read_csv(filename, col_types = cols()) %>%
    clean_names %>%
    mutate(date = strptime(offense_date, "%m/%d/%Y %H:%M:%S %p")) %>%
    rename(
      offense_description = offense_charge_description,
      street = offense_street_name,
      cross_street = offense_cross_street
    )
}


# with a cleaned dataframe,
# 1. cross-match with Texas Observer data to narrow to anti-homeless citations,
# 2. use Google Maps API and FCC data to attach the census tract,
# 3. select the columns of interest
geocode_and_filter <- . %>%
  inner_join(offense_descriptions, c("offense_description")) %>%
  mutate(google_search = make_google_search(street, cross_street)) %>%
  add_lat_lon %>%
  add_census_tract %>%
  select(date, offense_description, offense_code, census_tract, street, cross_street, lat, lon)


# Now we prepare the pipeline with a functional programming mindset
parse_dacc <- . %>% clean_dacc %>% geocode_and_filter
parse_amc <- . %>% clean_amc %>% geocode_and_filter


# creating arguments for invoke_map
fs <- rep(c(parse_dacc, parse_amc),
          c(length(DACC_YEARS), length(AMC_YEARS)))
years <- c(DACC_YEARS, AMC_YEARS)


# the pipeline
output_df <- invoke_map(fs, years) %>% reduce(rbind)

write_csv(output_df, "data/citations_by_tract.csv")