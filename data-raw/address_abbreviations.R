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
  "B LVD", "BLVD",
  "BVD", "BLVD",
  "BV", "BLVD",
  "BLV", "BLVD",
  "CI", "CIR",
  "EX", "EXPRESSWAY",
  "HY", "HWY",
  "LA", "LANE",
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
  "10TH", "TENTH",
  "11TH", "ELEVENTH",
  "12TH", "TWELFTH",
  "13TH", "THIRTEENTH",
  "14TH", "FOURTEENTH",
  "15TH", "FIFTEENTH",
  "16TH", "SIXTEENTH",
  "17TH", "SEVENTEENTH",
  "18TH", "EIGHTEENTH",
  "19TH", "NINETEENTH",
  "20TH", "TWENTIETH",
  "21ST", "TWENTY-FIRST",
  "22ND", "TWENTY-SECOND",
  "23RD", "TWENTY-THIRD",
  "24TH", "TWENTY-FOURTH",
  "25TH", "TWENTY-FIFTH",
  "26TH", "TWENTY-SIXTH",
  "27TH", "TWENTY-SEVENTH",
  "28TH", "TWENTY-EIGHTH",
  "29TH", "TWENTY-NINTH",
  "30TH", "THIRTIETH",
  "31ST", "THIRTY-FIRST",
  "32ND", "THIRTY-SECOND",
  "33RD", "THIRTY-THIRD",
  "34TH", "THIRTY-FOURTH",
  "35TH", "THIRTY-FIFTH",
  "36TH", "THIRTY-SIXTH",
  "37TH", "THIRTY-SEVENTH",
  "38TH", "THIRTY-EIGHTH",
  "39TH", "THIRTY-NINTH",
  "40TH", "FORTIETH",
  "41ST", "FORTY-FIRST",
  "42ND", "FORTY-SECOND",
  "43RD", "FORTY-THIRD",
  "44TH", "FORTY-FOURTH",
  "45TH", "FORTY-FIFTH",
  "46TH", "FORTY-SIXTH",
  "47TH", "FORTY-SEVENTH",
  "48TH", "FORTY-EIGHTH",
  "49TH", "FORTY-NINTH",
  "50TH", "FIFTIETH",
  "51ST", "FIFTY-FIRST",
  "52ND", "FIFTY-SECOND",
  "53RD", "FIFTY-THIRD",
  "54TH", "FIFTY-FOURTH",
  "55TH", "FIFTY-FIFTH",
  "56TH", "FIFTY-SIXTH",
  "57TH", "FIFTY-SEVENTH",
  "58TH", "FIFTY-EIGHTH",
  "59TH", "FIFTY-NINTH",
  "60TH", "SIXTIETH",
  "61ST", "SIXTY-FIRST",
  "62ND", "SIXTY-SECOND",
  "63RD", "SIXTY-THIRD",
  "64TH", "SIXTY-FOURTH",
  "65TH", "SIXTY-FIFTH",
  "66TH", "SIXTY-SIXTH",
  "67TH", "SIXTY-SEVENTH",
  "68TH", "SIXTY-EIGHTH",
  "69TH", "SIXTY-NINTH",
  "70TH", "SEVENTIETH",
  "71ST", "SEVENTY-FIRST",
  "72ND", "SEVENTY-SECOND",
  "73RD", "SEVENTY-THIRD",
  "74TH", "SEVENTY-FOURTH",
  "75TH", "SEVENTY-FIFTH",
  "76TH", "SEVENTY-SIXTH",
  "77TH", "SEVENTY-SEVENTH",
  "78TH", "SEVENTY-EIGHTH",
  "79TH", "SEVENTY-NINTH",
  "80TH", "EIGHTIETH",
  "81ST", "EIGHTY-FIRST",
  "82ND", "EIGHTY-SECOND",
  "83RD", "EIGHTY-THIRD",
  "84TH", "EIGHTY-FOURTH",
  "85TH", "EIGHTY-FIFTH",
  "86TH", "EIGHTY-SIXTH",
  "87TH", "EIGHTY-SEVENTH",
  "88TH", "EIGHTY-EIGHTH",
  "89TH", "EIGHTY-NINTH",
  "90TH", "NINETIETH",
  "91ST", "NINETY-FIRST",
  "92ND", "NINETY-SECOND",
  "93RD", "NINETY-THIRD",
  "94TH", "NINETY-FOURTH",
  "95TH", "NINETY-FIFTH",
  "96TH", "NINETY-SIXTH",
  "97TH", "NINETY-SEVENTH",
  "98TH", "NINETY-EIGHTH",
  "99TH", "NINETY-NINTH",
  "100TH", "ONE HUNDREDTH",
  "101ST", "ONE HUNDRED FIRST",
  "102ND", "ONE HUNDRED SECOND",
  "103RD", "ONE HUNDRED THIRD",
  "104TH", "ONE HUNDRED FOURTH",
  "105TH", "ONE HUNDRED FIFTH",
  "106TH", "ONE HUNDRED SIXTH",
  "107TH", "ONE HUNDRED SEVENTH",
  "108TH", "ONE HUNDRED EIGHTH",
  "109TH", "ONE HUNDRED NINTH",
  "110TH", "ONE HUNDRED TENTH",
  "111TH", "ONE HUNDRED ELEVENTH",
  "112TH", "ONE HUNDRED TWELFTH",
  "113TH", "ONE HUNDRED THIRTEENTH",
  "114TH", "ONE HUNDRED FOURTEENTH",
  "115TH", "ONE HUNDRED FIFTEENTH",
  "116TH", "ONE HUNDRED SIXTEENTH",
  "117TH", "ONE HUNDRED SEVENTEENTH",
  "118TH", "ONE HUNDRED EIGHTEENTH",
  "119TH", "ONE HUNDRED NINETEENTH",
  "120TH", "ONE HUNDRED TWENTIETH",
  "121ST", "ONE HUNDRED TWENTY-FIRST",
  "122ND", "ONE HUNDRED TWENTY-SECOND",
  "123RD", "ONE HUNDRED TWENTY-THIRD",
  "124TH", "ONE HUNDRED TWENTY-FOURTH",
) |> mutate(type = "ordinal")

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
  "DUPLEX", "DUPLEX",
  "ATTIC", "ATTIC",
  "LOT", "LOT",
  "LVL", "LEVEL",
  "OFC", "OFFICE",
  "#", "#",
) |> mutate(type = "unit")

# Special Units
special_unit <- tribble(
  ~short, ~long,
  "UPPR FRONT", "UPPER FRONT",
  "UPPER/FRONT", "UPPER FRONT",
  "FRNT UPPER", "UPPER FRONT",
  "UPPR REAR", "UPPER REAR",
  "LOWER/FRONT", "LOWER FRONT",
  "LOWR FRONT", "LOWER FRONT",
  "FRNT LOWER", "LOWER FRONT",
  "LOWER/REAR", "LOWER REAR",
  "LOWR REAR", "LOWER REAR",
  "LOWER BACK", "LOWER REAR",
  "LOWR BACK", "LOWER REAR",
  "UPPR", "UPPER",
  "LOWR", "LOWER",
  "LWR", "LOWER",
  "FRNT", "FRONT",
  "REAR", "REAR",
  "BACK", "BACK",
) |> mutate(type = "special_unit")

address_abbreviations <- bind_rows(directions, all_street_suffix, add_suffix, official_suffix, ordinals, unit_types, special_unit)

addr_abbr <- address_abbreviations

usethis::use_data(address_abbreviations, overwrite = TRUE)
