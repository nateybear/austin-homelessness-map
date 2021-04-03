# performs the cleaning of the APD crime reports dataset from
# https://data.austintexas.gov/Public-Safety/Crime-Reports/fdj4-gpfu
# and writes 

CRIME_REPORTS_PATH <- here("data/crime_reports.csv.gz")

crime_reports <- 
  vroom(CRIME_REPORTS_PATH) %>% 
  clean_names %>%
  mutate(across(ends_with("date") & where(is.character), mdy)) %>%
  mutate(across(ends_with("date_time") & where(is.character), mdy_hms))

dates_of_interest <- ymd(20150101) %--% ymd(20191231)

crime_reports %<>% 
  dplyr::filter(occurred_date %within% dates_of_interest) %>%
  mutate(address = glue("{address} Austin, TX")) %>%
  batched(add_lat_lon) %>%
  batched(add_census_tract)

