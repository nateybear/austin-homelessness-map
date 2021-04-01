# use librarian to manage this project and make it shareable
if (!("librarian" %in% rownames(utils::installed.packages()))) {
  install.packages("librarian")
}

librarian::shelf( 
  cran_repo = "https://cran.microsoft.com/", # Dallas, TX
  quiet = TRUE,
  tidymodels,
  units,
  here,
  kableExtra,
  logging,
  rlang,
  ggmap,
  gganimate,
  gifski,
  ggthemes,
  jsonlite,
  tidyverse,
  janitor,
  magrittr,
  glue,
  lubridate,
  crul,
  vroom,
  sf,
  transformr
)

# try to mask stats::filter
library(dplyr)

logging::basicConfig()

# register API key for ggmap
ggmap::register_google(key = read_file(here("api_secret.txt")))

# compress any CSV files I get so that I save storage space :)
.compress_csv <- function(file) {
  file <- here(glue("data/{file}.csv"))
  vroom(file) %>% vroom_write(glue("{file}.gz"))
  file.remove(file)
}

.f <- as_mapper