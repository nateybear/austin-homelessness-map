# performs the cleaning of the APD crime reports dataset from
# https://data.austintexas.gov/Public-Safety/Crime-Reports/fdj4-gpfu
# and writes 

crime <- vroom(here("data/crime_reports_raw.csv.gz"))

date_range <- ymd(20150101) %--% ymd(20191231)

crime %>%
  
  # filter on robberies or thefts that occurred in 2015-2019
  dplyr::filter(category_description %in% c("Robbery", "Theft")) %>%
  dplyr::filter(!highest_offense_code %in% c(1109, 1201)) %>%
  dplyr::filter(occurred_date_time %within% date_range) %>%
  
  # select relevant variables
  select(
    offense_description = highest_offense_description,
    offense_code = highest_offense_code,
    address,
    date = occurred_date_time,
    latitude,
    longitude
  ) %>%
  
  # many already have lat/lon included. we need to geocode <1% of the sample.
  # that's what this group_split and map_df business is doing
  group_split(needs_geocoding = is.na(latitude) | is.na(longitude)) %>%
  map_df(function(df) {
    if (any(df$needs_geocoding)) {
      df %>%
        mutate(address = glue("{address} Austin, TX")) %>%
        add_lat_lon
    } else {
      df %>% mutate(lat = latitude, lon = longitude)
    }
  }) %>%
  
  # quick way to batch this add_census_tract function instead of running 130k plus requests at once!
  group_split(batch = row_number() %% 100) %>%
  map_df(add_census_tract) %>%
  
  select(-batch, -needs_geocoding, -latitude, -longitude) %>%
  
  dplyr::filter(within_austin(lat, lon)) %>%
  
  vroom_write(here("data/crime_reports.csv.gz"))
  

