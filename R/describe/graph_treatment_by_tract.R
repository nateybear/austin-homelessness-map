# Reads in the citations by tract CSV and compiles
# 1. Static per-month plots of citations by tract
# 2. GIF animating the per-month plots
source(here("R/analyze/assign_treatment.R"), local = TRUE)
crime_data <- vroom(here("data/crime_reports.csv.gz")) %>% aggregate_event_data()
citation_data <- vroom(here("data/citations_by_tract.csv.gz")) %>% aggregate_event_data()

citation_data %<>% 
  semi_join(crime_data, c("census_tract")) #%>%
# group_by(census_tract) %>%
# mutate(citations = citations - mean(citations)) %>%
# ungroup()

##### Cleaning and Shaping #####

# This shapefile has a different coordinate system, and I really just
# want the convex hull so I can intersect the statewide census tract
# file to get the Austin-area census tracts
city_boundaries <-
  read_sf("data/austin_city_limits.shp") %>%
  st_transform("NAD83") %>%
  st_union() %>%
  st_convex_hull()

census_tracts <-
  read_sf("data/texas_census_tracts.shp") %>%
  st_intersection(city_boundaries)

# Take our citation by tract data and
# 1. add the "group_date" which is the granularity we want to aggregate by
# 2. group and summarise number of citations by census tract per aggregation period
# 3. ungroup and run complete to ensure that every tract has a datum for every period
# 4. join on census tract to attach the sf geometry to our dataset
# 5. select only the columns we care about
citations_by_tract_grouped <- citation_data %>%
  assign_treatment() %>%
  inner_join(census_tracts, c("census_tract" = "TRACTCE")) %>%
  select(census_tract, D, geometry) %>%
  st_sf


##### Graphing #######

map_data <- st_bbox(citations_by_tract_grouped) %>% as.numeric() %>% get_stamenmap(maptype = "toner", zoom = 13)

ggmap(map_data) +
  geom_sf(
    data = citations_by_tract_grouped,
    aes(
      fill = D,
      geometry = geometry,
      group = census_tract
    ),
    inherit.aes = F,
    alpha = .3
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(x = "", y = "", fill = "") +
  scale_fill_discrete(name = "", labels = c("Control", "Treatment"))

ggsave(here("figures/treatment_by_tract.png"))
