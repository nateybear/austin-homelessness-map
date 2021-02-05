# Reads in the citations by tract CSV and compiles
# 1. Static per-month plots of citations by tract
# 2. GIF animating the per-month plots


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
citations_by_tract_grouped <- vroom("data/citations_by_tract.csv.gz") %>%
  mutate(group_date = floor_date(date, "month")) %>%
  group_by(census_tract, group_date) %>%
  summarise(citations = n())  %>%
  ungroup %>%
  complete(census_tract, group_date, fill = list(citations = 0)) %>%
  inner_join(census_tracts, c("census_tract" = "TRACTCE")) %>%
  select(citations, census_tract, group_date, geometry) %>%
  st_sf



##### Static Plot #####

# set limits for the fill scale so that colors represent the same thing for every plot
.lims <- with(citations_by_tract_grouped, range(citations))
lims(fill = .lims)

# ggmap data
map_data <- st_bbox(citations_by_tract_grouped) %>% as.numeric() %>% get_stamenmap(maptype = "toner", zoom = 13)

# function to plot a static subset of the data
plot_citations <- function(d)
  ggmap(map_data, aes(alpha = 0.5)) +
  geom_sf(data = d, aes(
    fill = citations,
    geometry = geometry,
    group = census_tract
  ), inherit.aes = F) +
  scale_fill_gradient(
    low = rgb(0,0,0,0),
    high = "red",
    name = "Citations per month",
    guide = FALSE,
    trans = "log1p"
  ) +
  theme_economist_white()

# For each aggregation period, make a static plot
citations_by_tract_grouped %>%
  group_by(group_date) %>%
  group_walk(function(group_data, group_key) {
    date <- group_key$group_date
    plot_citations(group_data) + ggtitle(glue("Anti-Homeless Citations, {format(date, '%B %Y')}"))
    ggsave(glue("figures/citations_by_tract_{format(date, '%Y_%m')}.png"),
           device = "png")
  })

# Zip those static plots to save space. Note tar = "tar" b/c Sys.getenv("tar") was blank for me
Sys.glob("figures/citations_by_tract_*.png") %T>%
  tar(
    tarfile = "figures/citations_by_tract.tar.gz",
    files = .,
    compression = "gzip",
    tar = "tar"
  ) %T>%
  file.remove


##### Animated Plot #####

# let's also make an animated plot with gganimate
gif_plot <- citations_by_tract_grouped %>% plot_citations +
  transition_time(group_date) +
  labs(title = "Austin Homeless Citations by Census Tract",
       subtitle = '{format(frame_time, "%B %Y")}') +
  ease_aes(fill = "sine-in")

# two plots per second. How does that look?
animate(gif_plot,
        renderer = gifski_renderer(),
        duration = 30,
        fps = 10,
        width = 1200,
        height = 900)

anim_save("figures/citations_by_tract.gif")
