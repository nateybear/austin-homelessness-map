crime_data <- vroom(here("data/crime_reports.csv.gz")) %>% aggregate_event_data()
citation_data <- vroom(here("data/citations_by_tract.csv.gz")) %>% aggregate_event_data()

crime_data %<>% 
  semi_join(citation_data, c("census_tract")) # %>%
  # group_by(census_tract) %>%
  # mutate(citations = citations - mean(citations)) %>%
  # ungroup()

citation_data %<>% 
  semi_join(crime_data, c("census_tract")) #%>%
  # group_by(census_tract) %>%
  # mutate(citations = citations - mean(citations)) %>%
  # ungroup()

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
  cutoff <- ymd(20180628)
  model_data <- crime %>% 
    inner_join(treatments, c("census_tract")) %>%
    mutate(post = as.numeric(group_date > cutoff), D = as.numeric(D))
  
  tibble(att = coef(lm(citations ~ D*post + census_tract, data = model_data))["D:post"])
}

print(glue("Simple difference in means ATE is {newey_ate(crime_data, citation_data)['ate']}"))

# do 1000 bootstrap replicates of this process. cluster at the census_tract level
B <- 100

bootstrap_ate <- map_dfr(1:B, ~{
  tract_resamples <- crime_data %>% 
    distinct(census_tract) %$% 
    sample(census_tract, length(census_tract), replace = TRUE)
  
  # if we sample the same tract twice, treat it as "different" tracts in terms of grouping
  tract_recoding <- tibble(census_tract = tract_resamples, id = 1:length(tract_resamples))
  
  crime_resample <- pmap_df(tract_recoding, ~ {
    crime_data %>%
      dplyr::filter(census_tract == .x) %>%
      mutate(census_tract = .y)
  })
  
  citation_resample <- pmap_df(tract_recoding, ~ {
    citation_data %>%
      dplyr::filter(census_tract == .x) %>%
      mutate(census_tract = .y)
  })
  
  did(crime_resample, citation_resample)
})

