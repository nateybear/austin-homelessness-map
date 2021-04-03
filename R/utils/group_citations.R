# Take event-level data and aggregate by time delta (default monthly)
# 
# Option to only use no sit/no lie and public camping citations, which
# appear to be the types of citations that changed after city council policy changed.

.citations_by_tract <- vroom("data/citations_by_tract.csv.gz")

group_citations <- function(by = "month", filter = TRUE) {
  filter_func <-
    if (filter)
      . %>% dplyr::filter(offense_code %in% c(62615, 62611)) # no sit/no lie and camping codes
    else
      identity
  
  .citations_by_tract %>%
    filter_func %>% 
    mutate(group_date = floor_date(date, by)) %>%
    group_by(group_date, census_tract) %>%
    summarise(citations = n())  %>%
    ungroup %>%
    complete(group_date, census_tract, fill = list(citations = 0))
}