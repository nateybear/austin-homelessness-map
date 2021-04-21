source(here("R/analyze/assign_treatment.R"), local = TRUE)

newey_ate <- function(crimes, citations) {
  
  propensity_scores <-
    assign_treatment(citations_tract_month = citations) %>%
    calc_propensity_score(crime_tract_month = crimes)
  
  cutoff <- ymd(20180628)
  outcome_data <- crimes %>% 
    dplyr::filter(group_date > cutoff) %>%
    inner_join(propensity_scores, c("census_tract")) %>%
    rename(p = propensity,
           y = citations,
           d = group_date)
  
  treated_outcomes <- outcome_data %>% dplyr::filter(D) %>% select(p, d ,y)
  control_outcomes <- outcome_data %>% dplyr::filter(!D) %>% select(p, d, y)
  
  treated_comparisons <- pmap_dbl(treated_outcomes, ~{
    control_outcomes %>%
      dplyr::filter(d == .y) %>%
      mutate(w = (p - .x)^-2) %$%
      weighted.mean(y, w)
  })
  
  tibble(
    ate = mean(treated_outcomes$y - treated_comparisons)
  )
  
}

did <- function(crime, citations) {
  treatments <- assign_treatment(citations)
  model_data <- crime %>% 
    inner_join(treatments, c("census_tract")) %>%
    mutate(D = as.numeric(D),
           t = as.numeric(group_date > GLOBALS$cutoff))
  
  lm(reports ~ D*t + census_tract, data = model_data)
}
