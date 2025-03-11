## This page contains code to prepare the following datasets:
#' `address_abbreviations`/`addr_abbr` (for internal use): lookup tables w/
#' common address abbr. Do not include regex in these tables.
#' `address_regex`: A single string of regex
#'

## a list of all letters excluding N, S, E, W, and O (O'Reily, etc)
# letter_no_NSEWO <- str_collapse_bound(LETTERS[!LETTERS %in% c("N", "S", "E", "W", "O")])

# main address reference
address_regex <- tribble(
  ~address_part, ~regex,
  "street_number", "^\\d+\\b",
  "street_number_multi", "^(\\d+[A-Z]?\\b(\\W|AND[^A-Z]|OR[^A-Z])+)+(?! [STNDRH]{2})",
  "street_number_range", "^\\d+(\\s+)?(\\W+|AND)(\\s+)?\\d+\\b(?! [STNDRH]{2})",
  "street_number_range_db", "^\\d+(\\s+)?(-|/| )(\\s+)?\\d+\\b",
  "street_number_fraction", "[1-9]/\\d\\b",
  "street_name_fraction", "(?<=\\d ([NSEW] )?)\\d{1,2} (\\d{1,2}|(ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE|THIRTEEN|FOURTEEN|FIFTEEN))[ /](\\d{1,2}(THS?)?|(HALF|FOURTH|EIGHTH|SIXTEENTH)S?)(?= (\\d{1,3}[SNRT][TDH] )?(ST|AV|MILE\\b))",
  "street_number_coords", "(\\b[NSEW]\\s?\\d+\\W?[NSEW]\\s?\\d+)|(\\d{3,}\\s?[NSEW]\\s?\\d{3,}\\b)|([NSEW]\\s?\\d{3,}\\b)",
  "building", "^\\d+[A-Z]\\b|^[A-Z]\\d+\\b|^(A|B|C|D|F|G|H|I|J|K|L|M|P|Q|R|T|U|V|X|Y|Z)\\b",
  "po_box", "\\b((P( )?O )?BOX \\d+|P( )?O BOX ?\\w+)",
  "dr_king", "((DR|DOCTOR)(\\W+)?)?M(ARTIN)?(\\W+)?L(UTHER)?(\\W+)?K(ING)?(\\W+(JR|JUNIOR))?",
  "special_units_regex", "\\b((UP+[ER]+|LO*WE*R*|FR+O*N*T|REAR|B[AC]+K)\\W?)+(N[ORTH]+|S(O|[OUTH]{2,})?|E[AST]+|W[EST]+)*$"
)

special_street_names <- tribble(
  ~regex, ~output,
  # "((DR|DOCTOR)\\W*)?M(ARTIN)?\\W*L(UTHER)?\\W*K(ING)?(\\W+(JR|JUNIOR))?", "MARTIN LUTHER KING",
  "MT", "MOUNT",
  "^ST(?!$)", "SAINT",
  "^(O)\\W([A-Z]{3,})", "\\1\\2",
  "^(MC)\\W([A-Z]{3,})", "\\1\\2",
  "P( )?TREE", "PEACHTREE",
  "CONT(L)? C[OL]+NY", "CONTINENTAL COLONY",
  "C[EASR]+( E)? CHAVEZ", "CESAR CHAVEZ",
  "ATL", "ATLANTA",
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
  filter(short != long & short != "HWY") |>
  mutate(type = "all_street_suffixes")

# manual common fixes. short = official abbreviation, long = manual abbr
all_street_suffixes_2 <- tribble(
  ~short, ~long,
  "BLVD", "B LVD",
  "BLVD", "BVD",
  "BLVD", "BV",
  "BLVD", "BLV",
  "CIR", "CI",
  "CT", "CRT",
  "EXPY", "EX",
  "EXPY", "EXPWY",
  "IS", "ISLD",
  # "HWY", "HY",
  "LANE", "LA",
  "PKWY", "PY",
  "PKWY", "PARK WAY",
  "TER", "TE",
  "TRCE", "TR",
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
ordinal_suffix <- function(number) {
  last_digit <- number %% 10
  last_two_digits <- number %% 100

  if (last_two_digits %in% c(11, 12, 13)) {
    return("TH")
  } else if (last_digit == 1) {
    return("ST")
  } else if (last_digit == 2) {
    return("ND")
  } else if (last_digit == 3) {
    return("RD")
  } else {
    return("TH")
  }
}

ordinals <- tibble(
  number = 1:999,
  short = paste0(number, sapply(number, ordinal_suffix)),
  long = str_remove_all(str_to_upper(english::ordinal(number)), "-"),
  long_number = str_remove_all(str_to_upper(english::english(number)), "-")
)

fractions <- tribble(
  ~short, ~long,
  "2", "HALF",
  "4", "FOURTHS",
  "8", "EIGHTHS",
  "16", "SIXTEENTHS",
) |> mutate(type = "fractions")

# Unit Types
unit_types <- tribble(
  ~short, ~long,
  "APT", "APARTMENT",
  "UNIT", "UNIT",
  "STE", "SUITE",
  "FL", "FLOOR",
  "FLT", "FLAT",
  "BLDG", "BUILDING",
  "RM", "ROOM",
  "PH", "PENTHOUSE",
  "DEPT", "DEPARTMENT",
  "DUPLEX", "DUPLEX",
  "ATTIC", "ATTIC",
  "BSMT", "BASEMENT",
  "LOT", "LOT",
  "LVL", "LEVEL",
  "OFC", "OFFICE",
  "NUM", "NUMBER",
  "NO", "NUMBER",
  "#", "#",
) |> mutate(type = "unit_types")

# Special Units
special_units <- tribble(
  ~short, ~long,
  "UPPR", "UPPER",
  "LOWR", "LOWER",
  "LWR", "LOWER",
  "LW", "LOWER",
  "FRNT", "FRONT",
  "REAR", "REAR",
  "BACK", "BACK",
) |> mutate(type = "special_units")

highways <- tribble(
  ~short, ~long,
  "(STATE|ST) (HIGHWAY|HWY|ROAD|RD|TRUNK|TRNK|TRK|TK)", "STATE HIGHWAY",
  "STH", "STATE HIGHWAY",
  "(COUNTY|CNTY|CTY|CO) (HIGHWAY|HWY|ROAD|RD|TRUNK|TRNK|TRK|TK)", "COUNTY HIGHWAY",
  "COUNTY", "COUNTY HIGHWAY",
  "CTHY?", "COUNTY HIGHWAY",
  "CTY", "COUNTY HIGHWAY",
  "CO", "COUNTY HIGHWAY",
  "HIGHWAY", "HIGHWAY",
  "HWY", "HIGHWAY",
  "HY", "HIGHWAY",
  "U\\.? ?S\\.? ?", "HIGHWAY",
) |> mutate(type = "highways")

address_abbreviations <- bind_rows(directions, all_street_suffixes, official_street_suffixes, unit_types, special_units, highways)

addr_abbr <- address_abbreviations

usethis::use_data(address_abbreviations, overwrite = TRUE)

usethis::use_data(special_street_names, most_common_suffixes, least_common_suffixes, address_regex, addr_abbr, directions, fractions, all_street_suffixes, official_street_suffixes, ordinals, unit_types, special_units, highways, overwrite = TRUE, internal = TRUE)
