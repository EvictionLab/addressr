## code to prepare `dd_address` dataset goes here

# --------------------------------------------------------------- functions ----
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

narrow_table <- function(.data, prefix) {
  .data |>
    select(starts_with(prefix)) |>
    rename_with(~ str_remove(., str_glue("{prefix}_"))) |>
    mutate(type = str_glue("{prefix}"), .before = 1)
}

# ------------------------------------------------------------- definitions ----
d_common_suffix <- c("DRIVE", "LANE", "AVENUE", "ROAD", "STREET", "CIRCLE", "COURT", "PLACE", "WAY", "BOULEVARD", "STRAVENUE", "COVE", "LOOP")
d_highway <- c("HIGHWAY", "HWY", "HW", "RTE", "ROAD", "RD", "TRUNK", "TRNK", "TRK", "TK")
d_na <- c("N/?A", "NULL", "NAN", "MISSING", "NONE", "UN ?KNOWN")

# ----------------------------------------------------------- abbreviations ----
## ------------------------------------------------------- number & ordinal ----
abbr_numbers <- tibble(
  number_short = 1:999,
  ordinal_short = paste0(number_short, sapply(number_short, ordinal_suffix)),
  ordinal_long = str_remove_all(str_to_upper(english::ordinal(number_short)), "-"),
  number_long = str_remove_all(str_to_upper(english::english(number_short)), "-")
)
abbr_ordinal <- abbr_numbers |> narrow_table("ordinal")
abbr_number <- abbr_numbers |> narrow_table("number")

## --------------------------------------------------------------- fraction ----
abbr_fraction <- tribble(
  ~short, ~long,
  "2", "HALF",
  "4", "FOURTHS",
  "8", "EIGHTHS",
  "16", "SIXTEENTHS",
) |> mutate(type = "fraction", .before = 1)

## -------------------------------------------------------------- direction ----
abbr_direction <- tribble(
  ~short, ~long, ~short_type,
  "NE", "NORTHEAST", "abbr",
  "N\\W?E", "NORTHEAST", "regex",
  "NW", "NORTHWEST", "abbr",
  "N\\W?W", "NORTHWEST", "regex",
  "SE", "SOUTHEAST", "abbr",
  "S\\W?E", "SOUTHEAST", "regex",
  "SW", "SOUTHWEST", "abbr",
  "S\\W?W", "SOUTHWEST", "regex",
  "N", "NORTH", "abbr",
  "S", "SOUTH", "abbr",
  "E", "EAST", "abbr",
  "W", "WEST", "abbr",
  "WST", "WEST", "regex",
  "SO", "SOUTH", "regex",
) |> mutate(type = "direction", .before = 1)

## ----------------------------------------------------------------- suffix ----
library(readr)
library(tidyr)

if (!file.exists("data-raw/usps-street-suffix.csv")) {
  # scrape USPS street suffix table
  library(rvest)
  html <- read_html("https://pe.usps.com/text/pub28/28apc_002.htm")
  raw_suffix <- html |> html_elements(".Basic_no_title") |> html_table(header = TRUE)
  raw_suffix <- raw_suffix[[1]] |> janitor::clean_names()
  write_csv(raw_suffix, "data-raw/usps-street-suffix.csv")
} else raw_suffix <- read_csv("data-raw/usps-street-suffix.csv")

### ------------------------------------------------------------------- raw ----
abbr_raw_suffix <- raw_suffix |>
  select(usps_short = 3, usps_long = 1, common_short = 2) |>
  # trailer is used in unit instead
  filter(usps_long != "TRAILER") |>
  mutate(usps_short = case_when(
    # rename conflicting abbreviations
    usps_short %in% c("PARK", "WALK", "SPUR") & usps_long %in% c("PARKS", "WALKS", "SPURS") ~ str_c(usps_short, "S"),
    .default = usps_short))

### ------------------------------------------------------------------ usps ----
abbr_usps_suffix <- abbr_raw_suffix |> narrow_table("usps") |> distinct() |> mutate(type = "usps_suffix", .before = 1)

### ------------------------------------------------------------------- all ----
abbr_all_suffix <- abbr_raw_suffix |>
  pivot_longer(ends_with("_short"), names_to = NULL, values_to = "short") |>
  distinct() |> rename(long = usps_long)

abbr_more_suffix <- tribble(
  ~long, ~short,
  # "BOULEVARD", "B[LVD]{1,4}",
  "BOULEVARD", "BVD",
  "BOULEVARD", "BV",
  "BOULEVARD", "BLV",
  "BOULEVARD", "BL",
  "CIRCLE", "CI",
  "COURT", "CRT",
  "EXPRESSWAY", "EX",
  "EXPRESSWAY", "EXPWY",
  "EXTENSION", "EXN",
  "ISLAND", "ISLD",
  "HIGHWAY", "HY",
  "LANE", "LA",
  "PARKWAY", "PY",
  "PARKWAY", "PARK WAY",
  "PARKWAY", "PKW",
  "TERRACE", "TE",
  "TRACE", "TR",
  "PARK",  "PK",
  "BAYWAY", "BAYWAY",
  "PLACE", "PLC",
  "AVENUE", "AE",
  "STREET", "STR[ET]{1,4}",
  "DRIVE", "DIRVE",
  "FRONTAGE", "FRONTAGE",
)

abbr_all_suffix <- abbr_all_suffix |> bind_rows(abbr_more_suffix) |> mutate(type = "all_suffix", .before = 1)

### --------------------------------------------------------- common & rare ----
abbr_common_suffix <- abbr_all_suffix |> filter(long %in% d_common_suffix) |> mutate(type = "common_suffix", .before = 1)

abbr_rare_suffix <- abbr_all_suffix |> filter(!long %in% d_common_suffix) |> mutate(type = "rare_suffix", .before = 1)

## ------------------------------------------------------- mobile home park ----
abbr_mobile_home <- tribble(
  ~short, ~long,
  "MOBILE HOME PK", "MOBILE HOME PARK",
  "MOBILE HOME DEV", "MOBILE HOME PARK",
  "MOBILE HOME", "MOBILE HOME PARK",
  "TRAILER PARK", "MOBILE HOME PARK",
  "TRAILER PRK", "MOBILE HOME PARK",
  "TRAILER PK", "MOBILE HOME PARK",
  "TRL PARK", "MOBILE HOME PARK",
  "TRL PK", "MOBILE HOME PARK",
  "MHP", "MOBILE HOME PARK",
) |> mutate(type = "mobile_home", .before = 1)

## ------------------------------------------------------------------- unit ----
### ------------------------------------------------------------------ type ----
abbr_unit_type <- tribble(
  ~short, ~long,
  "APT", "APARTMENT",
  "UNIT", "UNIT",
  "STE", "SUITE",
  "FL", "FLOOR",
  "FLT", "FLAT",
  "BLDG?", "BUILDING",
  "RM", "ROOM",
  "(?:MASTER )?BE?D(?:ROOM|RM)", "ROOM",
  "PH", "PENTHOUSE",
  "TOWNHOUSE", "TOWNHOUSE",
  "DEPT", "DEPARTMENT",
  "DUPLEX", "DUPLEX",
  "ATTIC", "ATTIC",
  "BSMT", "BASEMENT",
  "LOT", "LOT",
  "LVL", "LEVEL",
  "OFC", "OFFICE",
  "NUM", "NUMBER",
  "NO", "NUMBER",
  "HSE?", "HOUSE",
  "STAIR", "STAIRS",
  "GARAGE", "GARAGE",
  "ENTRAN[CE]*", "ENTRANCE",
  "DOOR", "DOOR",
  "CONDO", "CONDO",
  "TRLRS?", "TRAILER",
  # "COTTAGE", "COTTAGE",
  "#", "#",
) |> mutate(type = "unit_type", .before = 1)

### -------------------------------------------------------------- location ----
abbr_unit_location <- tribble(
  ~short, ~long,
  "UP+R", "UPPER",
  "UP", "UPPER",
  "LOWR", "LOWER",
  "LWR", "LOWER",
  "LW", "LOWER",
  "FRNT", "FRONT",
  "FRT", "FRONT",
  "REAR", "REAR",
  "BACK", "BACK",
  "MID", "MIDDLE",
  "ENTIRE", "ENTIRE",
  "WHOLE", "WHOLE",
  "SING[LE]+ FAM(ILY)?", "SINGLE FAMILY",
  "SINGLE", "SINGLE",
  "DOWN", "DOWN",
  "RIGHT", "RIGHT",
  "LEFT", "LEFT",
  "DOWNSTAIRS", "DOWNSTAIRS",
  "UPSTAIRS", "UPSTAIRS",
  "UF", "UPPER FRONT",
  "UR", "UPPER R(IGHT|EAR)",
  "DF", "DOWN FRONT",
  "SIDE", "SIDE",
) |> mutate(type = "unit_location", .before = 1)

## ---- state ----
abbr_state <- tibble(
  short = str_to_upper(c(state.abb, "DC")),
  long = str_to_upper(c(state.name, "DISTRICT OF COLUMBIA"))
) |> mutate(type = "state", .before = 1)

abbr_address <- bind_rows(
  abbr_usps_suffix, abbr_all_suffix, abbr_common_suffix, abbr_rare_suffix,
  abbr_mobile_home, abbr_unit_type, abbr_unit_location, abbr_direction,
  abbr_fraction, abbr_state)

# ----------------------------------------------------------------- strings ----
flatten_abbr <- function(.data, bound = FALSE, exclude = NULL, abbr_cols = c("short", "long")) {
  out <- .data |>
    pivot_longer(all_of(abbr_cols), names_to = NULL) |>
    distinct()

  if (!is.null(exclude)) {
    out <- out |> filter(!value %in% exclude)
  }

  out <- out |>
    mutate(n = str_remove_all(value, "(\\{.*\\})|(\\[.*\\])|(\\+)|(\\([^\\W]\\))|(\\()|(\\|.*\\W?)|(\\*)|(\\?)") |> str_length()) |>
    arrange(desc(n)) |>
    pull(value)

  if (bound) {
    out |> str_collapse_bound()
  } else out |> str_flatten("|")

}

r_hwy <- str_c("(", str_flatten(d_highway, collapse = "|"), ")")
r_na <- d_na |> str_collapse_bound()
r_ordinal <- "\\d+[SNRT][TDH]"
nb_1_20 <- abbr_number |> filter(short %in% c(1:20)) |> flatten_abbr(abbr_cols = "long")
b_1_20 <- abbr_number |> filter(short %in% c(1:20)) |> flatten_abbr(bound = TRUE, abbr_cols = "long")
nb_ord_1_10 <- abbr_ordinal[1:10, ] |> flatten_abbr(abbr_cols = "long")
nb_fraction <- abbr_fraction |> pull() |> str_flatten("|")
b_dir <- abbr_direction |> flatten_abbr(bound = TRUE)
nb_dir <- abbr_direction |> flatten_abbr(bound = FALSE)
nb_dir_letters <- abbr_direction |> flatten_abbr(bound = FALSE, abbr_cols = "short")
nb_dir_words <- abbr_direction |> flatten_abbr(bound = FALSE, abbr_cols = "long")
nb_no_dir <- LETTERS[!LETTERS %in% c("N", "S", "E", "W")] |> str_flatten("|")
nb_no_diro <- LETTERS[!LETTERS %in% c("N", "S", "E", "W", "O")] |> str_flatten("|")
b_all_suffix <- abbr_all_suffix |> flatten_abbr(bound = TRUE, exclude = c("FLAT", "LF", "FLT"))
nb_all_suffix <- abbr_all_suffix |> flatten_abbr(bound = FALSE, exclude = c("FLAT", "LF", "FLT"))
b_common_suffix <- abbr_common_suffix |> flatten_abbr(bound = TRUE)
nb_common_suffix <- abbr_common_suffix |> flatten_abbr(bound = FALSE)
nb_common_suffix_no_bd <- abbr_common_suffix |> flatten_abbr(bound = FALSE, exclude = c("BD"))
nb_common_suffix_no_ord <- abbr_common_suffix |> flatten_abbr(bound = FALSE, exclude = c("RD", "ST"))
nb_rare_suffix <- abbr_rare_suffix |> flatten_abbr(bound = FALSE, exclude = c("FLAT", "LF", "FLT"))
nb_ext_suffix <- abbr_rare_suffix |> filter(str_detect(long, "PARKWAY|FREEWAY|HIGHWAY|EXPRESSWAY|EXTENSION|FRONTAGE")) |> flatten_abbr(bound = FALSE)
nb_unit_type <- abbr_unit_type |> flatten_abbr()
nb_unit_type_no_hse <- abbr_unit_type |> flatten_abbr(exclude = c("HOUSE", "TOWNHOUSE"))
b_unit_type <- abbr_unit_type |> flatten_abbr(bound = TRUE)
nb_unit_location <- abbr_unit_location |> flatten_abbr()
b_unit_location <- abbr_unit_location |> flatten_abbr(bound = TRUE)
b_state <- abbr_state |> flatten_abbr(bound = TRUE)
nb_state <- abbr_state |> flatten_abbr(bound = FALSE)

# regex_patterns <- c(
#   # "extra_front" = str_glue("^((([A-Z\\W]+ )+(?:({b_dir}) \\d))|([A-Z\\W]+ )+(?:\\d))"),
#   # "address" = str_glue("[NSEW]?\\d+ [\\w\\s]+ {b_all_suffix}"),
#   # "unit" = str_glue("\\b((({nb_unit_type})\\W*)?(\\d+\\W*[A-Z]|[A-Z]\\d+|{nb_no_diro}\\W*\\d+|\\d+)([\\s\\-&]+#?(\\d+\\W*[A-Z]|[A-Z]\\W*\\d+|\\d+|[A-Z]|({nb_unit_location})|({nb_unit_type})))+)\\b")
#   "extra_front" = "[A-Z\\W]*?",
#   "street_number" = "[NSEW]?\\d+(?:\\s[NSEW]\\d+)?(?:\\W\\d+)*\\b",
#   "pre_direction" = str_glue("\\b(?:{nb_dir})\\b"),
#   "street_name" = "(?:[A-Z0-9]+ )+",
#   "suffix" = str_glue("\\b(?:{nb_common_suffix})\\b"),
#   "unit" = "#\\d+"
# )

# ---------------------------------------------------------------------- dd ----
# delete this?
dd_components <- tribble(
  ~label, ~match,
  "ordinal", "\\d+\\s?[SNRT][TDH]",
  "unit_words", str_glue("\\b({nb_unit_type}|{nb_unit_location})\\b"),
  "only_num_name", "^(((\\d+)\\W*([A-Z]+))|((\\d+)\\W(\\w+)))$",
  "only_simple_address", str_glue("^(\\d+)\\W+(\\w+)\\W+\\b({nb_common_suffix})$"),
  "only_simple_address_w_unit", str_glue("^(\\d+)\\W*({nb_dir})\\W*(\\w+)\\W*\\b({nb_common_suffix})?\\W+?\\b(({nb_unit_type}|{nb_unit_location})\\W*)*?([A-Z]?\\W*\\d+\\W*[A-Z]?)*?$"),
  "only_simple_predir_address", str_glue("^(\\d+)\\W*({nb_dir})\\W*(\\w+)\\W*\\b({nb_common_suffix})$"),
  "simple_predir_address", str_glue("(\\d+)\\W+?({nb_dir})\\W+?(\\w+)\\W+?\\b({nb_common_suffix})?"),
  "simple_address", str_glue("(\\d+)\\W+?(\\w+)\\W+?\\b({nb_common_suffix})+?"),
  "dir_name_suffix", str_glue("\\b({nb_dir})\\W*((?:\\w+)(?:\\W\\w+)*?)+\\W*({nb_all_suffix})+?"),
) |>
  mutate(label = str_glue("match_{label}"), regex = match)
## ----------------------------------------------------------------- change ----
dd_warn_0 <- tribble(
  ~label, ~match, ~standard,
  "na_address_1", str_glue("(?<!(\\d|{nb_dir}) ){r_na}(\\W*ADDRESS)?(?!\\W\\d+)"), "",
  "na_address_2", "(NO\\W*ADDRESS)|^NO$", "",
) |> mutate(
  label = str_glue("change_{label}"),
  regex = match
)

# pre-check: change temp address
dd_warn_1 <- tribble(
  ~label, ~match, ~standard,
  # change
  "move_unit_to_number",    str_glue("^([^\\d]+)({nb_unit_type})\\W*(\\d+)$"), "\\3 \\1",
  "move_number_to_front",    str_glue("^([^\\d]+)\\W*(\\d+(?:-\\d+)?)$"), "\\2 \\1",
  "move_unit_to_number_num_street_name",    str_glue("^((?:(?:[NSEW]? ?\\d+[SNRT][TDH])|(?:(?:{nb_dir}) \\d+))\\W+(?:{nb_common_suffix})\\W+)({nb_unit_type})\\W*(\\d+)$"), "\\3 \\1 \\2",
  # "move_unit_to_back",      str_glue("^(\\d+)\\W+((?:{nb_unit_type})\\W+(?:\\d+|[A-Z]+|{nb_unit_location}))\\W+(.*)"), "\\1 \\3 \\2",
  "unstick_suffix_unit",    str_glue("\\b({nb_common_suffix_no_bd})({nb_unit_type})\\b"), "\\1 \\2",
  "unstick_ordinal_suffix", str_glue("(\\d+[SNRT][TDH])({nb_common_suffix})"), "\\1 \\2",
  "unstick_direction_ordinal", str_glue("({nb_dir})(\\d+[SNRT][TDH])"), "\\1 \\2",
  "replace_abbr_front_unit",         str_glue("((?:{nb_unit_type})(?:\\W*(?:{nb_unit_location}) )?)\\W?FR?T\\b"), "\\1 FRONT",
  "st_to_saint", str_glue("(^\\d+(?: ?[A-Z])? ?(?:{nb_dir})) ST (?!(?:{nb_unit_location}|{nb_unit_type}|{nb_all_suffix}))([A-Z]{{3,20}})"), "\\1 SAINT \\3",
  "pull_suffix_from_unit",       str_glue("(?<!\\b({nb_common_suffix})\\s?)((?:\\b(?:(?:{nb_unit_location})\\W+)?(?:{nb_unit_type_no_hse})(?:\\W+(?:{nb_unit_location}))?)|#)(?:\\W+|\\b)({nb_common_suffix})\\b"), "\\5 \\2",
  "typo_north", "^(\\d+) ((N(?!ORTH\\b)[ORTH ]{1,6}H)|(NORT))\\b", "\\1 NORTH",
  "typo_south", "^(\\d+) ((S(?!OUTH\\b)[OUTH ]{1,6}H)|(SOUT))\\b", "\\1 SOUTH",
  "typo_east", "^(\\d+) ((E(?!AST\\b)[AST ]{1,5}T)|(EAS))\\b", "\\1 EAST",
  "typo_west", "^(\\d+) ((W(?!EST\\b)[EST ]{1,5}T)|(EST))\\b", "\\1 WEST",
  "replace_0_o", "([A-Z]+)0([A-Z]+)", "\\1O\\2",
  "add_ordinal_suffix", "^(\\d+ [NSEW]) (\\d+)$", "\\1 \\2TH",
  "add_ordinal_suffix_unit", "^(\\d+ [NSEW]) (\\d+) (#\\w+)$", "\\1 \\2TH \\3",
  # "add_ordinal_suffix_bldg", "^(\\d+ ?[A-Z] [NSEW]) (\\d+)$", "\\1 \\2TH \\3",
) |> mutate(
  label = str_glue("change_precheck_{label}"),
  regex = match
)

# mid-check: change street names
dd_warn_2 <- tribble(
  ~label,               ~match,                            ~standard,
  "mt_to_mount",        "\\bMT\\b",                        "MOUNT",
  "ft_to_fort",         "\\bFT\\b",                        "FORT",
  "st_to_saint",        "^\\W*ST\\b(?= [A-Z]{3,})",        "SAINT",
  "pt_to_port",         "^\\W*PT\\b(?= [A-Z]{3,})",        "PORT",
  "squish_o",           "^\\W*(O)\\W([A-Z]{3,})\\b",       "\\1\\2",
  "squish_mc",          "^\\W*(MC)\\W([A-Z]{3,})\\b",      "\\1\\2",
  "ave_to_avenue",      "^AVE\\b",                         "AVENUE",
  "peachtree",          "\\bP( )?TREE\\b",                 "PEACHTREE",
  "cont_colony",        "\\bCONT(L)? C[OL]+NY\\b",         "CONTINENTAL COLONY",
  "cesear_chavez",      "\\bC[EA][EASR]+( E)? CH[AE]V[A-Z]{1,3}Z\\b",   "CESAR CHAVEZ",
  "natl_to_national",   "\\bNATL\\b",                      "NATIONAL",
  "atl_to_atlanta",     "\\bATL\\b",                       "ATLANTA",
  "add_ordinal",        "^(\\d{1,3})$",                    "\\1ST",
) |> mutate(
  label = str_glue("change_name_{label}"),
  regex = match
)

## ------------------------------------------------------------------- warn ----
dd_warn_3 <- tribble(
  ~label,               ~match,
  "letter_dir",         str_glue("^({nb_dir_letters})$"),
  "letter",             str_glue("^({nb_no_dir})$"),
  "dir",                str_glue("^({nb_dir_words})$"),
  "rare_suffix",        str_glue("^({nb_rare_suffix})$"),
  "common_suffix",      str_glue("^({nb_common_suffix})$"),
  "unit_type",          str_glue("^({nb_unit_type})$"),
  "number",             str_glue("^\\d+$"),
) |> mutate(
  label = str_glue("warn_name_{label}"),
  regex = match
)

# "(\\d{2,}) (\\d-?[A-Z]|[A-Z]-?\\d|APT \\w+|APARTMENT \\w+|NUM \\d+|UNIT \\d) ([\\w\\s]+) (ST|AVE|DR|R(OA)?D|LN|LANE|CIR|CT|COURT|PL|WAY|BLVD|BOU|STRA|CV|COVE)" = "\\1 \\3 \\4 \\2 "

## ------------------------------------------------------------------ extra ----
dd_extra <- tribble(
  ~label,                  ~match, ~standard,
  "extra_front",           str_glue("^((([A-Z\\W]+ )+(?=({b_dir}) \\d))|([A-Z\\W]+ )+(?=\\d))"), NA_character_,
  "temp_back_1",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)(?:\\b|>)({nb_common_suffix})\\b.*$"), NA_character_,
  "temp_back_2",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?( {b_dir} )?)({b_all_suffix}).*$"), NA_character_,
  "temp_back_3",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)({nb_ord_1_10})\\W*FLOOR$"), NA_character_,
  "temp_back_4",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)( {b_dir} )({nb_unit_type})$"), NA_character_,
  "temp_back_5",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)({b_unit_type}|{b_unit_location}|(\\b(\\d+\\W*({nb_no_diro}))\\b)|(\\b({nb_no_diro})\\W*\\d+\\b)|(#\\s?\\w+)).*$"), NA_character_,
  "temp_back_6",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)\\b({nb_unit_type}|{nb_unit_location})\\d+.*$"), NA_character_,
  "temp_back_7",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)\\b\\d+$"), NA_character_,
  "temp_back_8",           str_glue("(?<!street_number[a-z_]{{1,25}}>\\s?)\\b({nb_all_suffix})(\\d+.*)$"), "\\1 \\2",
  "temp_suffix",           str_glue("(.*\\b({nb_common_suffix})\\b( {b_all_suffix})?)|(.*\\b({nb_all_suffix})\\b)"), NA_character_,
  "temp_part",             "<[a-z_]+>", NA_character_,
) |> mutate(regex = match)

## ------------------------------------------------------------ street name ----
### --------------------------------------------------------------- special ----
dd_other <- tribble(
  ~label,                  ~match,                                                                                      ~standard,                       ~names,
  "po_box_number",         "\\bP\\W*O\\W+?BOX\\W*(\\d+)\\b",                                                            "PO BOX \\1",                    "po_box",
  "po_box_word",           "\\bP\\W*O\\W+?BOX\\W+(\\w+)\\b",                                                            "PO BOX \\1",                    "po_box",
  "po_box_no_box",         "\\bP\\W*O\\W+?(\\d+)\\b",                                                                   "PO BOX \\1",                    "po_box",
  "city_state_zip_only",   str_glue("^([A-Z][A-Z ]+ ?){{1,4}}\\W+{b_state}\\W+(\\d{{5}}(?:\\W\\d{{4}})?)(?: US)?$"),      "\\1, \\2 \\3",                  c("city", "state", "zip"),
  "city_state_zip_end",    str_glue(", ([A-Z][A-Z ]+)\\W+{b_state}\\W+(\\d{{5}}(?:\\W\\d{{4}})?)(?: US)?$"),            "\\1, \\2 \\3",                  c("city", "state", "zip"),
  "city_state_only",       str_glue("^([A-Z][A-Z ]+ ?){{1,4}}\\W+({nb_state})$"),                                       "\\1, \\2",                      c("city", "state"),
  "city_state_end",        str_glue(", ([A-Z][A-Z ]+ ?){{1,4}}\\W+({nb_state})$"),                                      "\\1, \\2",                      c("city", "state"),
  "state",                 str_glue("^({nb_state})$"),                                                                  "\\1",                           "state",
  # "parenth",               "\\((.*)\\)(.*)", " \\2 \\1", "parenthesis",
) |> mutate(regex = match)

dd_special_street_name <- tribble(
  ~label,            ~match,                                                                                                      ~standard,
  "dr_king",         "\\b(REV[ERND]+\\W*)?((DOCTOR|DRIVE|DR)\\W*)?((M(ARTIN|AR[TIN]{1,4})?\\W*L(UTHER|U[A-Z]+THER)?\\W*K(ING|[ING]+|IN[A-Z])?)|(MARTIN\\W*(KING\\W*)+))(\\W+(JR|JUNIOR|J[A-Z0-9]|J))?\\b",  "MARTIN LUTHER KING",
  "lbj",             "\\bL(YNDON)?\\W*B\\W*J(OHNSON)?\\b", "LYNDON B JOHNSON",
  "no_number",       "(?<=^)[A-Z0-9 ]+(?=<(suffix|unit|post_direction))",                                                         NA_character_,
  "prefix",          str_glue("(?<=^\\d{{1,6}} (?:(?:{nb_dir}) )?)(AVENUE|AVE) ([A-Z]+)"),                                        "AVENUE \\2",
  "simple",          "(?<=(street_number[a-z_]{1,25}|pre_direction)>)[A-Z0-9\\- ]+(?=($|<(suffix|unit|post_direction|city_state)))", NA_character_,
) |> mutate(
  label = str_glue("street_name_{label}"),
  regex = match
)

### --------------------------------------------------------------- highway ----
dd_highway <- tribble(
  ~label,            ~match,                                        ~standard,
  "state_highway",   str_glue("(STATE|ST)\\W*{r_hwy}"),             "STATE HIGHWAY",
  "county_highway",  str_glue("(COUNTY|CNTY|CT[HY]|CO)\\W*{r_hwy}"),   "COUNTY HIGHWAY",
  "us_highway",      str_glue("(U\\s?S\\W*{r_hwy})|(U\\s?S)|(INTERSTATE)|(I)"),  "US HIGHWAY",
  "highway",         "HIGHWAY|HWY|HW|HY|TRUNK|TRK",                    "HIGHWAY",
) |> mutate(
  label = str_glue("street_name_{label}"),
  regex = str_glue("(?<!(([A-Z]{{3,15}} ){{2,4}})|([A-Z]{{6,15}} )|({nb_common_suffix}))\\b({r_hwy} )?({match})+[ #-]+(?!{nb_common_suffix})(\\d{{1,3}}|[A-Z]{{1,2}})( (AND|&) \\d{{2,3}})?\\b")
)

### -------------------------------------------------------------- numbered ----
dd_number_street_name <- tribble(
  ~label,             ~match,
  "fraction_ordinal", str_glue("\\d{{1,2}} (\\d{{1,2}}|({b_1_20}))[ /](\\d{{1,2}}(THS?)?|({nb_fraction})S?) \\d{{1,3}}[SNRT][TDH](?= (ST|AV|MILE\\b))"),
  "fraction",         str_glue("\\d{{1,2}} (\\d{{1,2}}|({b_1_20}))[ /](\\d{{1,2}}(THS?)?|({nb_fraction})S?)( MILE)?(?= ({nb_common_suffix}))"),
  "mile",             "\\d+( \\d{1,2}[ /]\\d{1,2})? MILE\\b",
  "ordinal",          str_glue("((\\d{{1,3}}\\s*[SNRT][TDH])|(\\d{{1,3}}\\s?[SNRTDH]{{1,3}}\\b)|(\\d{{1,3}}))(?=((\\s*({nb_common_suffix_no_ord}|MILE)\\b)|\\s({nb_common_suffix}|MILE)\\b))"),
) |> mutate(
  label = str_glue("street_name_{label}"),
  regex = str_glue("(?<=\\d\\s?[A-Z]? ({b_dir} )?)({match})")
)

## ---------------------------------------------------------- street number ----
dd_street_number <- tribble(
  ~label,                ~match,                                                       ~standard,
  "phone",               "(\\d{3})\\W?(\\d{3})\\W(\\d{4})",                            "\\1-\\2-\\3",
  "coords_two_building", "([NSEW])\\W?(\\d+)\\W?([NSEW])\\W?(\\d+)([A-Z])",            "\\1\\2 \\3\\4 \\5",
  "coords_two",          "([NSEW])\\W?(\\d+)\\W?([NSEW])\\W?(\\d+)",                   "\\1\\2 \\3\\4",
  "coords_num_one",      str_glue("(\\d+)\\W?([NSEW])\\W?(\\d+)(?! ?($|{nb_unit_type}|{nb_unit_location}|{nb_common_suffix}))"),          "\\1 \\2\\3",
  "coords_one_building", "([NSEW])\\W?(\\d+)([A-Z])",                                  "\\1\\2 \\3",
  "coords_one",          "([NSEW])\\W?(\\d+)",                                         "\\1\\2",
  "fraction_half",       "(\\d+)(1/2\\b| HALF)",                                       "\\1 1/2",
  "fraction",            "\\d+ [1-9]/[1-9]",                                           NA_character_,
  "multi",               "\\d+((\\s?(,|AND|&|OR|-| |/)\\s?)\\b\\d+){1,}",              NA_character_,
  "building_unit",       str_glue("(\\d+)\\W*([A-Z])\\W*(\\d+)(?! ?($|{nb_unit_type}|{nb_unit_location}|{nb_common_suffix}))"),                      "\\1 \\2\\3",
  "multi_building",      str_glue("\\d+\\s?[A-Z]?((\\s?(,|AND|&|OR|-| |/)\\s?)\\b\\d+\\s?[A-Z]?){{1,}}(?! ?($|{nb_unit_type}|{nb_unit_location}|{nb_common_suffix}))"),  NA_character_,
  "unit_building",       str_glue("(\\d+)\\W(\\d+)\\W*({nb_no_diro})"),                "\\1 \\3\\2",
  "building_simple",     "(\\d+)(?:-|(?:\\W*#\\W*))?([A-Z])",                                            "\\1 \\2",
  "building_nodiro",     str_glue("(\\d+) ({nb_no_diro})(?! ({nb_common_suffix}|{nb_unit_location})\\b)"),      "\\1 \\2",
  "building_dir",        str_glue("(\\d+)({nb_dir})(?= {b_dir})(?! {nb_common_suffix})"),                    "\\1 \\2",
  "building_num",        "([A-Z])-?(\\d+)",                                            "\\2 \\1",
  "building_num_nodiro", str_glue("({nb_no_diro}) (\\d+)(?! ?($|{nb_unit_type}|{nb_unit_location}|{nb_common_suffix}))"),                            "\\2 \\1",
  "simple",              "\\d+",                                                       NA_character_,
) |> mutate(
  label = str_glue("street_number_{label}"),
  regex = str_glue("(?<=^)({match})\\b")
  )

## ------------------------------------------------------------------- unit ----
dd_unit <- tribble(
  ~label,            ~match,
  "multi",      str_glue("\\b((({nb_unit_type})\\W*)?(\\d+\\W*[A-Z]|[A-Z]\\d+|{nb_no_diro}\\W*\\d+|\\d+)([\\s\\-&]+#?(\\d+\\W*[A-Z]|[A-Z]\\W*\\d+|\\d+|[A-Z]|({nb_unit_location})|({nb_unit_type})))+)\\b"),
  "floor",      str_glue("(#|\\b)(((\\d+|{nb_ord_1_10})\\s*([RSNT][DTH])?\\W*FL([OR]+)?)|(FL([OR]+)? \\d+\\s*[RNST][DTH])|(FL([OR]+)?\\W*({nb_unit_location}))|(#?(FLOOR)))\\b"),
  "cottage",    str_glue("\\b(({nb_unit_location})\\W*)?COT+A[GE]+\\b"),
  "type_building", str_glue("(#|\\b)({nb_unit_type})\\W*\\d+\\W*({nb_unit_location})\\W\\d+$"),
  "location",   str_glue("((#|\\b)((\\d+\\W*({nb_unit_location}))|(\\d*[^>A-Z0-9]?([LUFR]{{2}}))|(({nb_unit_type})\\s*({nb_unit_location}|{nb_dir}))|({b_dir}\\s*({nb_unit_type}|\\d+)$)|(({nb_unit_location})\\W*({nb_unit_location}|{nb_dir}|{nb_unit_type}))|({nb_unit_location})|([LUFR]{{2}})))\\b|(#\\s*(({nb_dir})\\b|([A-Z]+$)))"),
  "location_word", str_glue("(#|\\b)({nb_unit_location}) [A-Z]+"),
  "type_1_multi",     str_glue("(\\b|#\\W*)({nb_unit_type})\\W*(\\d+(?:\\W*[A-Z])?)(?:[\\-\\s]+| AND )\\3(\\b|$)"),
  "type_2_multi",     str_glue("(\\b|#\\W*)({nb_unit_type})\\W+(({nb_1_20}|[A-Z])(?:\\W*\\d+)?)(?:[\\-\\s]+| AND )\\3(\\b|$)"),
  "type_1",           str_glue("(\\b|#\\W*)({nb_unit_type})\\W*(\\d+(\\W*[A-Z])?)(\\b|$)"),
  "type_2",           str_glue("(\\b|#\\W*)({nb_unit_type})\\W+(({nb_1_20}|[A-Z])(\\W*\\d+)?)(\\b|$)"),
  "type_none",        str_glue("(\\b|#\\W*)({nb_unit_type})\\W*$"),
  "number_word",      str_glue("(\\b|#\\W*)({nb_1_20})\\W*$"),
  # "type",       str_glue("(\\b(({nb_unit_type})\\W((([A-Z]\\d+|\\d+[A-Z]|[A-Z]|\\d+|{nb_1_20}|$)([\\-\\s]?\\w)?)|(\\d+\\W*[A-Z]\\d)))\\b)|(#\\W*({nb_unit_type}|{nb_1_20}))|(\\b({nb_unit_type}(?=$)))"),
  "building",   str_glue("\\b((\\d+\\W?({nb_no_diro}))|(({nb_no_diro})\\s*\\d+))\\b"),
  "simple",     str_glue("([\\-\\/#\\s]+|^\\W*)(((\\d+\\W*[A-Z]\\d)|(\\d+\\W*[A-Z])|(({nb_no_diro})\\W*\\d+)|([A-Z]\\d+)|(\\d+)|({nb_no_diro}))(\\b|$))|(\\b[A-Z]\\W*$)"),
  "end_number", "\\d+$",
  "end_letter", "\\b[A-Z]$",
) |>
  mutate(
    label = str_glue("unit_{label}"),
    regex = match
  )

dd_check <- t

## -------------------------------------------------------------- direction ----
dd_direction <- tribble(
  ~label,            ~match,
  "pre_direction",   str_glue("(?<=(^|^<street_number[a-z_]{{1,25}}>\\s?)){b_dir}(?!($|\\s?HIGHWAY|\\s?HWY|\\s?<temp_back>|\\s?<(unit|suffix)[a-z_]{{1,15}}>))"),
  "post_direction_temp",  str_glue("(?<=temp_suffix>\\s?)({nb_dir})(\\b|$)"),
  "post_direction_input",  str_glue("((?<!<street_number[a-z_]{{1,25}}>\\s?){b_dir}(?=($|\\s?<(unit_[a-z_]{{1,15}})>$)))|((?<=(suffix_[a-z_]{{1,15}})>\\s?){b_dir})"),
  "direction",       str_glue("{b_dir}")
) |> mutate(regex = match)

## ----------------------------------------------------------------- suffix ----
dd_suffix <- tribble(
  ~label,            ~match,
  "common_common",   str_glue("\\b({nb_common_suffix}) ({nb_common_suffix})(\\b|$)"),
  "common",          str_glue("\\b({nb_common_suffix})(?=(\\s?<unit_location>$|$))"),
  "common_ext",      str_glue("\\b({nb_common_suffix}) ({nb_ext_suffix})(?=(\\s?<unit_location>$|$))"),
  "common_rare",     str_glue("{b_common_suffix} ({nb_rare_suffix})(?=(\\s?<unit_location>$|$))"),
  "rare",            str_glue("\\b({nb_rare_suffix})(?=(\\s?<unit_location>$|$))"),
) |>
  mutate(
    label = str_glue("suffix_{label}"),
    regex = match
  )

dd_address <- bind_rows(dd_special_street_name, dd_highway, dd_number_street_name, dd_street_number, dd_unit, dd_direction, dd_extra, dd_suffix, dd_other, dd_warn_1, dd_warn_2, dd_components, dd_warn_3, dd_warn_0)

usethis::use_data(abbr_address, dd_address, abbr_numbers, overwrite = TRUE)
