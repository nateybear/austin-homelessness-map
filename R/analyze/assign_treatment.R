# This script calculates the treatment based on number of citations and
# returns a tibble that contains the census tract and treatment status

assign_treatment <- function(citations_tract_month = GLOBALS$citations) {
  date_cutoff <- as_datetime(GLOBALS$cutoff)
  
  # mean citations by tract both before and after treatment
  citations_tract_pre_post <- citations_tract_month %>%
    mutate(pre_treatment = group_date < date_cutoff) %>%
    group_by(census_tract, pre_treatment) %>%
    summarise(citations = mean(citations))
  
  # drop in mean citations after treatment
  citations_tract_change <- citations_tract_pre_post %>% group_modify(function(group_data, group_key) {
    
    citations_before <- group_data %>% dplyr::filter(pre_treatment) %$% citations
    
    citations_after <- group_data %>% dplyr::filter(!pre_treatment) %$% citations
    
    tibble(change = citations_before)
  })
  
  # take census tracts with the highest drop as treatment group
  cutoff <- citations_tract_change %$% quantile(change, .9)
  
  # return tibble with census_tract and D
  citations_tract_change %>%
    mutate(D = change > cutoff) %>%
    select(-change)
}