citations <- vroom(here("data/citations_by_tract.csv.gz")) %>% aggregate_event_data(tract_column = offense_description)

ggplot(citations, aes(x = group_date, y = citations)) + 
  geom_line(color = muted("blue")) + 
  facet_wrap(~offense_description, nrow = 1) +
  geom_vline(xintercept = as_datetime(ymd(20180628)), color = muted("red")) +
  geom_vline(xintercept = as_datetime(ymd(20190629)), color = muted("green")) +
  labs(y = "Citations per Month", x = "") +
  theme_calc()

ggsave(here("figures/citations_over_time.png"))