# Take event-level data and aggregate by time delta (default monthly)
aggregate_event_data <- function(dataset, by = "month", date_column = date, tract_column = census_tract) {
    dataset %>%
      mutate(group_date = floor_date({{date_column}}, by)) %>%
      group_by(group_date, {{tract_column}}) %>%
      summarise(citations = n())  %>%
      ungroup %>%
      complete(group_date, {{tract_column}}, fill = list(citations = 0))
  }