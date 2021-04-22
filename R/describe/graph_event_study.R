event_study <- GLOBALS$citations %>% 
  assign_treatment() %>% 
  inner_join(GLOBALS$crime, c("census_tract")) %>%
  mutate(event_date = floor_date(group_date, "quarter")) %>%
  mutate(S = glue("Q{quarter(group_date)} {format(group_date, '%y')}"), D = as.numeric(D)) %>%
  ungroup()

model <- lm(reports ~ D*S + census_tract, data = event_study)
coefs <- coef(model)
coefs <- coefs[str_starts(names(coefs), "D:S")]
coefnames <- str_split(names(coefs), "D:S") %>% map(~paste0(.x, collapse = "")) %>% unlist
coefs <- tibble(estimate = coefs, S = coefnames)

vcov. <- sandwich::vcovBS(model, cluster = ~census_tract)

confints <- lmtest::coefci(model, vcov. = vcov.)
confints <- confints[str_starts(rownames(confints), "D:S"),]
rnames <- str_split(rownames(confints), "D:S") %>% map(~paste0(.x, collapse = "")) %>% unlist
confints <- tibble(lower = confints[,1], upper = confints[,2], S = rnames)

plot_data <- event_study %>% distinct(event_date, S) %>% inner_join(coefs, c("S")) %>% inner_join(confints, c("S"))

ggplot(plot_data, aes(x = event_date)) + 
  geom_line(aes(y = estimate)) + 
  geom_point(aes(y=estimate)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  scale_x_datetime(breaks = plot_data$event_date, labels = plot_data$S) +
  labs(x = "", y = "") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = -45)) +
  geom_vline(xintercept = as_datetime(ymd(20180601)), color = muted("red")) +
  geom_vline(xintercept = as_datetime(ymd(20190601)), color = muted("green")) +
  geom_hline(yintercept = 0)

ggsave(here("figures/event_study.png"))
