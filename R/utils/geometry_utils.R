# This file contains operations to help work with the sf package and do the various
# geocoding necessary for this project
# 

GOOGLE_CACHE_PATH <- here("data/google_cache.csv.gz")
CENSUS_CACHE_PATH <- here("data/census_cache.csv.gz")
FCC_API_URL <- "https://geo.fcc.gov/api/census/area"

##### Point geometry #######

austin_city_limits <- read_sf("data/austin_city_limits.shp") %>% 
  clean_names %>% 
  st_union %>% 
  st_convex_hull

within_austin <- function(lat, lon) {
  out <- suppressMessages(map2_lgl(lon, lat, ~st_point(c(.x, .y)) %>% st_intersects(austin_city_limits, F) %>% as.logical))
  gc()
  return(out)
}

### Geocoding utils ###

# cache a long-running operation at the given path.
# ... represents the columns that determine the cache hit.
.cached <- function(mutation, path, ...) {
  
  cache <- vroom(path)
  
  # .cached is a higher-order function. it returns a function
  function(df) {
    new_entries <- anti_join(df, cache, c(...)) %>% select(...) %>% distinct
    
    if (nrow(new_entries) > 0) {
      new_entries %<>% mutation
      
      # warn about NAs from the mutation. drop them and exclude from cache. but tell user we're doing that
      NAs <- apply(is.na(new_entries), 1, any)
      if (any(NAs)) {
        warning("The following entries returned NAs. THEY WILL BE DROPPED\n", new_entries[NAs,])
        new_entries %<>% drop_na
      }
      
      cache <<- bind_rows(cache, new_entries) # Need to use scoping assignment to access closure's outer scope!
      vroom_write(cache, path)
      message(glue("Updated cache with {nrow(new_entries)} entries"))
    }
    
    return(inner_join(df, cache, c(...)))
  }
}
source("R/utils/function_logger.R", local = T)
.add_lat_long <- . %>% mutate_geocode(address)
add_lat_lon <- .cached(withLogging(.add_lat_long), GOOGLE_CACHE_PATH, "address")


# give me a dataframe with latitude and longitude, and
# 1. use the lat/lon to make an HTTP request to FCC's Block FIPS API
# 2. parse the returned 15-digit block FIPS for the 6-digit census tract
# 3. return the original dataset with a new column of census tract #s
.add_census_tract <- function(df) {
  urls <- glue("{FCC_API_URL}?lat={df$lat}&lon={df$lon}")
  
  responses <- Async$new(urls = urls)$get()
  
  parsed_responses <- responses %>% map(function(resp) {
    resp$parse() %>% fromJSON
  })
  
  census_tracts <- parsed_responses %>% map_chr(function(resp) {
    if (is.null(resp$status))
      resp$results$block_fips[1] %>% substr(6, 11)
    else
      NA
  })
  
  return(cbind(df, census_tract = census_tracts))
}

add_census_tract <- .cached(withLogging(.add_census_tract), CENSUS_CACHE_PATH, "lat", "lon")

# batch an operation over a dataframe. accepts a purrr-style formula
# e.g.
# iris %>% batched(~filter(.x, Species == "setosa"))
batched <- function(df, f, batchSize = 100) {
  if (is_formula(f)) {
    f <- as_mapper(f)
  }
  map_df(0:floor(nrow(df) / batchSize), function(i) {
    start <- i * batchSize + 1
    end <- min((i + 1) * batchSize, nrow(df))
    f(slice(df, start:end))
  })
}