## code to prepare `address_abbreviations` dataset goes here

# Directions
directions <- tribble(
  ~short, ~long,
  "N", "NORTH",
  "S", "SOUTH",
  "E", "EAST",
  "W", "WEST",
  "NE", "NORTHEAST",
  "NW", "NORTHWEST",
  "SE", "SOUTHEAST",
  "SW", "SOUTHWEST"
) |> mutate(type = "directions")

# USPS Street Endings
library(readr)
library(tidyr)

if (!file.exists("data-raw/usps-street-endings.csv")) {
  library(rvest)
  html <- read_html("https://pe.usps.com/text/pub28/28apc_002.htm")
  street_endings_raw <- html |> html_elements(".Basic_no_title") |> html_table(header = TRUE)
  street_endings_raw <- street_endings_raw[[1]] |> janitor::clean_names()
  write_csv(street_endings_raw, "data-raw/usps-street-endings.csv")
}

street_endings_raw <- read_csv("data-raw/usps-street-endings.csv")

all_street_endings <- street_endings_raw |>
  pivot_longer(1:2, names_to = NULL, values_to = "value") |>
  distinct() |>
  rename(short = 1, long = 2) |>
  filter(short != long) |>
  mutate(type = "all_street_ends")

official_endings <- street_endings_raw |>
  select(short = 3, long = 1) |> distinct() |>
  filter(short != long) |>
  mutate(type = "official_street_ends")

address_abbreviations <- bind_rows(directions, all_street_endings, official_endings)

usethis::use_data(address_abbreviations, overwrite = TRUE)
