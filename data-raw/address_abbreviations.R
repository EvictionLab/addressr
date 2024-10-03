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

# USPS Street suffix
library(readr)
library(tidyr)

if (!file.exists("data-raw/usps-street-suffix.csv")) {
  library(rvest)
  html <- read_html("https://pe.usps.com/text/pub28/28apc_002.htm")
  street_suffix_raw <- html |> html_elements(".Basic_no_title") |> html_table(header = TRUE)
  street_suffix_raw <- street_suffix_raw[[1]] |> janitor::clean_names()
  write_csv(street_suffix_raw, "data-raw/usps-street-suffix.csv")
}

street_suffix_raw <- read_csv("data-raw/usps-street-suffix.csv")

all_street_suffix <- street_suffix_raw |>
  pivot_longer(1:2, names_to = NULL, values_to = "value") |>
  distinct() |>
  rename(short = 1, long = 2) |>
  filter(short != long) |>
  mutate(type = "all_street_suffix")

official_suffix <- street_suffix_raw |>
  select(short = 3, long = 1) |> distinct() |>
  filter(short != long) |>
  mutate(type = "official_street_suffix")

add_suffix <- tribble(
  ~short, ~long,
  "BV", "BLVD",
  "B LVD", "BLVD",
  "BLV", "BLVD",
  "BVD", "BLVD",
  "CI", "CIR",
  "EX", "EXPRESSWAY",
  "HY", "HWY",
  "PY", "PKWY",
  "TE", "TERRACE",
  "TR", "TRACE",
  "MOBILE HOME PARK", "MHP",
  "MOBILE HOME PK", "MHP",
  "MOBILE HOME DEV", "MHP",
  "MOBILE HOME", "MHP",
  "TRAILER PARK", "MHP",
  "TRAILER PRK", "MHP",
  "TRAILER PK", "MHP",
  "TRL PARK", "MHP",
  "TRL PK", "MHP"
) |>
  mutate(type = "all_street_suffix")

# Ordinal Number Streets
ordinals <- tribble(
  ~short, ~long,
  "1ST", "FIRST",
  "2ND", "SECOND",
  "3RD", "THIRD",
  "4TH", "FOURTH",
  "5TH", "FIFTH",
  "6TH", "SIXTH",
  "7TH", "SEVENTH",
  "8TH", "EIGHTH",
  "9TH", "NINTH",
  "10TH", "TENTH"
) |> mutate(type = "numbered_street_name")

# Unit Types
unit_types <- tribble(
  ~short, ~long,
  "APT", "APARTMENT",
  "UNIT", "UNIT",
  "STE", "SUITE",
  "FL", "FLOOR",
  "BLDG", "BUILDING",
  "RM", "ROOM",
  "PH", "PENTHOUSE",
  "DEPT", "DEPARTMENT",
  "LOT", "LOT",
  "OFFICE", "OFFICE",
  "#", "#",
) |> mutate(type = "unit")

# Special Units
special_units <- tribble(
  ~short, ~long,
  "UPPER FRONT", "UPPER FRONT",
  "UPPER REAR", "UPPER REAR",
  "LOWER FRONT", "LOWER FRONT",
  "LOWER REAR", "LOWER REAR",
  "REAR ATTIC", "REAR ATTIC",
  "UPPR", "UPPER",
  "LWR", "LOWER",
  "FRONT", "FRONT",
  "REAR", "REAR",
  "ATTIC", "ATTIC",
) |> mutate(type = "special_units")

address_abbreviations <- bind_rows(directions, all_street_suffix, add_suffix, official_suffix, ordinals, unit_types, special_units)

usethis::use_data(address_abbreviations, overwrite = TRUE)
