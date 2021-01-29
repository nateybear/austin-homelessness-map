library(ggmap)
library(tidyverse)
library(janitor)
library(magrittr)
library(crul)
library(vroom)

# register API key for ggmap
register_google(key = read_file("api_secret.txt"))

# read csv implementation of my choosing, with option to suppress output of column type guessing
csv <- function(file, suppress = F, ...) if (suppress) suppressMessages(vroom(file, ...)) else vroom(file, ...)

# compress any CSV files I get so that I save storage space :)
compress_csv <- function(file) {
  file <- paste0("data/",file,".csv")
  vroom(file) %>% vroom_write(paste0(file,".gz"))
  file.remove(file)
}