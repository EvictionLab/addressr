## This page contains code to prepare the following datasets:
#' `address_abbreviations`/`addr_abbr` (for internal use): lookup tables w/
#' common address abbr. Do not include regex in these tables.
#' `address_regex`: A single string of regex
#'

## a list of all letters excluding N, S, E, W, and O
# letter_no_NSEWO <- str_collapse_bound(LETTERS[!LETTERS %in% c("N", "S", "E", "W", "O")])

# main address reference
address_regex <- tribble(
  ~address_part, ~regex,
  "street_number", "^\\d+\\b",
  "street_number_multi", "^(\\d+\\b(\\W|AND)+)+(?! [STNDRH]{2})",
  "street_number_range", "^\\d+(\\s+)?(\\W+|AND)(\\s+)?\\d+\\b(?! [STNDRH]{2})",
  "street_number_range_db", "^\\d+(\\s+)?(-|/| )(\\s+)?\\d+\\b",
  "street_number_fraction", "\\d/\\d\\b",
  "building", "^\\d+[A-Z]\\b|^[A-Z]\\d+\\b|^(A|B|C|D|F|G|H|I|J|K|L|M|P|Q|R|T|U|V|X|Y|Z)\\b",
  "po_box", "(P( )?O )?BOX \\w+",
  "dr_king", "((DR|DOCTOR)(\\W+)?)?M(ARTIN)?(\\W+)?L(UTHER)?(\\W+)?K(ING)?(\\W+(JR|JUNIOR))?",
)

special_street_names <- tribble(
  ~regex, ~output,
  "((DR|DOCTOR)\\W*)?M(ARTIN)?\\W*L(UTHER)?\\W*K(ING)?(\\W+(JR|JUNIOR))?", "MARTIN LUTHER KING",
  "^ST", "SAINT",
  "MT", "MOUNT",
)

# directions
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

official_street_suffixes <- street_suffix_raw |>
  select(short = 3, long = 1) |> distinct() |>
  filter(short != long) |>
  mutate(type = "official_street_suffixes")

common_suffixes <- c("DR", "LN", "LANE", "AVE", "RD", "ST", "CIR", "CT", "PL", "WAY", "BLVD", "STRA", "CV")
most_common_suffixes <- official_street_suffixes |>
  filter(short %in% common_suffixes)

all_street_suffixes_1 <- street_suffix_raw |>
  pivot_longer(1:2, names_to = NULL, values_to = "value") |>
  distinct() |>
  rename(short = 1, long = 2) |>
  filter(short != long) |>
  mutate(type = "all_street_suffixes")

all_street_suffixes_2 <- tribble(
  ~short, ~long,
  "BLVD", "B LVD",
  "BLVD", "BVD",
  "BLVD", "BV",
  "BLVD", "BLV",
  "CIR", "CI",
  "EXPRESSWAY", "EX",
  "HWY", "HY",
  "LANE", "LA",
  "PKWY", "PY",
  "TERRACE", "TE",
  "TRACE", "TR",
  "MHP", "MOBILE HOME PARK",
  "MHP", "MOBILE HOME PK",
  "MHP", "MOBILE HOME DEV",
  "MHP", "MOBILE HOME",
  "MHP", "TRAILER PARK",
  "MHP", "TRAILER PRK",
  "MHP", "TRAILER PK",
  "MHP", "TRL PARK",
  "MHP", "TRL PK",
) |>
  mutate(type = "all_street_suffixes")

all_street_suffixes <- bind_rows(all_street_suffixes_1, all_street_suffixes_2)

least_common_suffixes <- all_street_suffixes |>
  filter(!short %in% common_suffixes & !long %in% common_suffixes)

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
  "21ST", "TWENTYFIRST",
  "22ND", "TWENTYSECOND",
  "23RD", "TWENTYTHIRD",
  "24TH", "TWENTYFOURTH",
  "25TH", "TWENTYFIFTH",
  "26TH", "TWENTYSIXTH",
  "27TH", "TWENTYSEVENTH",
  "28TH", "TWENTYEIGHTH",
  "29TH", "TWENTYNINTH",
  "30TH", "THIRTIETH",
  "31ST", "THIRTYFIRST",
  "32ND", "THIRTYSECOND",
  "33RD", "THIRTYTHIRD",
  "34TH", "THIRTYFOURTH",
  "35TH", "THIRTYFIFTH",
  "36TH", "THIRTYSIXTH",
  "37TH", "THIRTYSEVENTH",
  "38TH", "THIRTYEIGHTH",
  "39TH", "THIRTYNINTH",
  "40TH", "FORTIETH",
  "41ST", "FORTYFIRST",
  "42ND", "FORTYSECOND",
  "43RD", "FORTYTHIRD",
  "44TH", "FORTYFOURTH",
  "45TH", "FORTYFIFTH",
  "46TH", "FORTYSIXTH",
  "47TH", "FORTYSEVENTH",
  "48TH", "FORTYEIGHTH",
  "49TH", "FORTYNINTH",
  "50TH", "FIFTIETH",
  "51ST", "FIFTYFIRST",
  "52ND", "FIFTYSECOND",
  "53RD", "FIFTYTHIRD",
  "54TH", "FIFTYFOURTH",
  "55TH", "FIFTYFIFTH",
  "56TH", "FIFTYSIXTH",
  "57TH", "FIFTYSEVENTH",
  "58TH", "FIFTYEIGHTH",
  "59TH", "FIFTYNINTH",
  "60TH", "SIXTIETH",
  "61ST", "SIXTYFIRST",
  "62ND", "SIXTYSECOND",
  "63RD", "SIXTYTHIRD",
  "64TH", "SIXTYFOURTH",
  "65TH", "SIXTYFIFTH",
  "66TH", "SIXTYSIXTH",
  "67TH", "SIXTYSEVENTH",
  "68TH", "SIXTYEIGHTH",
  "69TH", "SIXTYNINTH",
  "70TH", "SEVENTIETH",
  "71ST", "SEVENTYFIRST",
  "72ND", "SEVENTYSECOND",
  "73RD", "SEVENTYTHIRD",
  "74TH", "SEVENTYFOURTH",
  "75TH", "SEVENTYFIFTH",
  "76TH", "SEVENTYSIXTH",
  "77TH", "SEVENTYSEVENTH",
  "78TH", "SEVENTYEIGHTH",
  "79TH", "SEVENTYNINTH",
  "80TH", "EIGHTIETH",
  "81ST", "EIGHTYFIRST",
  "82ND", "EIGHTYSECOND",
  "83RD", "EIGHTYTHIRD",
  "84TH", "EIGHTYFOURTH",
  "85TH", "EIGHTYFIFTH",
  "86TH", "EIGHTYSIXTH",
  "87TH", "EIGHTYSEVENTH",
  "88TH", "EIGHTYEIGHTH",
  "89TH", "EIGHTYNINTH",
  "90TH", "NINETIETH",
  "91ST", "NINETYFIRST",
  "92ND", "NINETYSECOND",
  "93RD", "NINETYTHIRD",
  "94TH", "NINETYFOURTH",
  "95TH", "NINETYFIFTH",
  "96TH", "NINETYSIXTH",
  "97TH", "NINETYSEVENTH",
  "98TH", "NINETYEIGHTH",
  "99TH", "NINETYNINTH",
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
  "121ST", "ONE HUNDRED TWENTYFIRST",
  "122ND", "ONE HUNDRED TWENTYSECOND",
  "123RD", "ONE HUNDRED TWENTYTHIRD",
  "124TH", "ONE HUNDRED TWENTYFOURTH",
) |> mutate(type = "ordinals")

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
) |> mutate(type = "unit_types")

# Special Units
special_units <- tribble(
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
) |> mutate(type = "special_units")

address_abbreviations <- bind_rows(directions, all_street_suffixes, official_street_suffixes, ordinals, unit_types, special_units)

addr_abbr <- address_abbreviations

usethis::use_data(address_abbreviations, overwrite = TRUE)

usethis::use_data(special_street_names, most_common_suffixes, least_common_suffixes, address_regex, addr_abbr, directions, all_street_suffixes, official_street_suffixes, ordinals, unit_types, special_units, overwrite = TRUE, internal = TRUE)
