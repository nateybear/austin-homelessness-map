# This script builds a treatment variable as the decile with the greatest percent reduction in mean citations per month.
# Then it uses a random forest to try and estimate propensity scores

citations_tract_month <- here("data/citations_by_tract.csv.gz") %>%
  vroom() %>%
  aggregate_event_data()

# The day of the city council meeting
date_cutoff <- as_datetime(ymd("20180628"))

citations_tract_month %<>% mutate(pre_treatment = group_date < date_cutoff)

# mean citations by tract both before and after treatment
citations_tract_pre_post <- citations_tract_month %>%
  group_by(census_tract, pre_treatment) %>%
  summarise(sd_citations = sd(citations),
            citations = mean(citations))

# drop in citations after treatment by number of standard deviations
citations_tract_change <-
  citations_tract_pre_post %>% group_modify(function(group_data, group_key) {
    sd_before <-
      group_data$sd_citations[which(group_data$pre_treatment)]
    
    citations_before <-
      group_data$citations[which(group_data$pre_treatment)]
    
    citations_after <-
      group_data$citations[which(!group_data$pre_treatment)]
    
    tibble(
      abs_change = (citations_before - citations_after),
      sd_change = (citations_before - citations_after) / sd_before
    )
  })

q_absolute <- quantile(citations_tract_change$abs_change, .9)
q_relative <- quantile(citations_tract_change$sd_change, .9)
citations_tract_change %<>% mutate(D_abs = abs_change > q_absolute, D_sd = sd_change > q_relative)

