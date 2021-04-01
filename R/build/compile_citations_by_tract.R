## Compiles a panel dataset that contains citations by date for each census tract in Travis County
##
## Note that you need a Google API key to use this (see ggmap's github page).
## Be careful, as this could incur charges! This is why we use caches in the form of csv files
here::i_am("R/build/compile_citations_by_tract.R")

TX_OBSERVER_PATH <- here("data/tx_observer_data.csv.gz")
DACC_DATA_PREFIX <- "data/DACC_"
DACC_YEARS <- 2015:2019
AMC_DATA_PREFIX <- "data/AMC_"
AMC_YEARS <- 2015:2019

source(here("R/utils/geometry_utils.R"), local = T)


########### HELPER DATA AND FUNCTIONS #############


# Going to use Texas Observer's data as a Source Of Truth for which citations we're looking for
offense_descriptions <-
  vroom(TX_OBSERVER_PATH) %>%
  drop_na %>%
  clean_names %>%
  group_by(offense_description) %>%
  summarise(offense_code = unique(offense_code))


# give me street and cross streets from your dataset and I'll give you something you can Google search
make_address <- function(street, cross_street) {
  normed_street <-
    ifelse(is.na(cross_street),
      street,
      glue("{street} and {cross_street}"))
  
  glue("{normed_street} Austin, TX")
}



######## PIPELINE #############


# give me a DACC dataset and I'll clean it
clean_dacc <- . %>%
    clean_names %>%
    mutate(date = mdy(offense_date)) %>%
    rename(
      offense_description = charges_description,
      street = offense_street_name,
      cross_street = offense_cross_street
    )


# give me an AMC dataset and I'll clean it
clean_amc <- . %>%
    clean_names %>%
    mutate(date = mdy_hms(offense_date)) %>%
    rename(
      offense_description = offense_charge_description,
      street = offense_street_name,
      cross_street = offense_cross_street
    )

# use ludbridate's interval notation to clip overlapping data
trim_year <- function(df, year) {
  jan_1st <- ymd(glue("{year}-01-01"))
  dec_31st <- ymd(glue("{year}-12-31"))
  
  year <- jan_1st %--% dec_31st
  
  df %>% dplyr::filter(date %within% year)
}

# with a cleaned dataframe,
# 1. cross-match with Texas Observer data to narrow to anti-homeless citations,
# 2. use Google Maps API and FCC data to attach the census tract,
# 3. select the columns of interest
geocode_and_filter <- . %>%
  inner_join(offense_descriptions, c("offense_description")) %>%
  mutate(address = make_address(street, cross_street)) %>%
  add_lat_lon %>%
  add_census_tract %>%
  select(date, offense_description, offense_code, census_tract, street, cross_street, lat, lon, address)

# compile the citations for DACC
dacc_citations_by_tract <- 
  map_df(DACC_YEARS, ~{
    glue("data/DACC_{.x}.csv.gz") %>%
      here %>%
      vroom %>% 
      clean_dacc %>%
      trim_year(.x) %>%
      geocode_and_filter
  })

# compile the citations for DACC
amc_citations_by_tract <- 
  map_df(AMC_YEARS, ~{
    glue("data/AMC_{.x}.csv.gz") %>%
      here %>%
      vroom %>% 
      clean_amc %>%
      trim_year(.x) %>%
      geocode_and_filter
  })

citations_by_tract <- bind_rows(dacc_citations_by_tract, amc_citations_by_tract)


vroom_write(citations_by_tract, "data/citations_by_tract.csv.gz")
