# This script calculates a propensity score given a tibble of treatment assignments,
# based on on monthly crime data going back to the beginning of 2015. Return a tibble
# with the census tract, treatment, and propensity score. Regression forest does a
# pretty good job of estimating with a large number of predictors.

.default_treatment_tract <- function() {
  source(here("R/analyze/assign_treatment.R"), local = TRUE)
  assign_treatment()
}
.default_crime_tract_month <- function() vroom(here("data/crime_reports.csv.gz")) %>% aggregate_event_data()

calc_propensity_score <- function(treatment_tract = .default_treatment_tract(), crime_tract_month = .default_crime_tract_month()) {
  date_cutoff <- ymd(20180628)
  model_data <- crime_tract_month %>%
    dplyr::filter(group_date < date_cutoff) %>%
    pivot_wider(names_from = group_date,
                values_from = citations,
                names_glue = "citations_{format(group_date, '%b_%Y')}") %>%
    inner_join(treatment_tract, c("census_tract"))
  
  # let's try estimating this with a random forest. a linear model would overfit with so many predictors.
  # yes, randomForest gives a nice warning that we are doing regression for a model with fewer than five
  # unique outcomes. Comparing 0/1 outcome to the vote share from a classification setup yields the same answers!
  model <-
    randomForest(
      D ~ . - census_tract,
      data = model_data,
      mtry = 7,
      ntree = 5000
    )

  model_data %>% 
    select(census_tract, D) %>% 
    mutate(propensity = model$predicted)
}