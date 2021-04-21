# loads our packages and does any setup we need
source("R/include.R", local = TRUE)
for (file in Sys.glob("R/utils/**.R")) {
  source(file, local = TRUE)
}

# define global variables for our analysis
GLOBALS <- env()
GLOBALS$cutoff <- ymd(20180628)

crime_data <- vroom(here("data/crime_reports.csv.gz")) %>% aggregate_event_data() %>% rename(reports = citations)
citation_data <- vroom(here("data/citations_by_tract.csv.gz")) %>% aggregate_event_data()

crime_data %<>% 
  semi_join(citation_data, c("census_tract"))
citation_data %<>% 
  semi_join(crime_data, c("census_tract"))

GLOBALS$crime <- crime_data
GLOBALS$citations <- citation_data
