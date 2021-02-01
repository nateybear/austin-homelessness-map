library(ggmap)
library(gganimate)
library(gifski)
library(ggthemes)
library(jsonlite)
library(tidyverse)
library(janitor)
library(magrittr)
library(glue)
library(lubridate)
library(crul)
library(vroom)
library(sf)
library(transformr)

# register API key for ggmap
register_google(key = read_file("api_secret.txt"))

# compress any CSV files I get so that I save storage space :)
.compress_csv <- function(file) {
  file <- glue("data/{file}.csv")
  vroom(file) %>% vroom_write(glue("{file}.gz"))
  file.remove(file)
}