#' Check the address pattern
#'
#' @param pattern The string to be extracted and removed.
#'
#' @return A string to use in pattern matching.
check_pattern <- function(pattern) {

  pat <- pattern

  # reference table: address_abbreviations
  abbr_types <- unique(addr_abbr$type)
  # reference table: address_regex
  regex_tbl <- address_regex
  regex_types <- unique(regex_tbl$address_part)

  # both pre and post-directions use the "directions" table
  if (str_detect(pattern, "direction")) {
    addr_abbr <- addr_abbr[addr_abbr$type == "directions", ]
    pat <- str_collapse_bound(unique(c(addr_abbr$short, addr_abbr$long)))

    # pre-directions must be at the string start
    if (pattern == "pre_direction") {
      pat <- paste0("^", pat, "(?!$)")
    }

    # pre-directions must be at the string start
    if (pattern == "pre_direction_db") {
      pat <- paste0("^", pat)
    }

  }

  if (pattern %in% abbr_types) {

    # filter for address pattern type, get unique from each column
    addr_abbr <- addr_abbr[addr_abbr$type == pattern, ]
    pat <- str_collapse_bound(unique(c(addr_abbr$short, addr_abbr$long)))

  }

  if (pattern == "post_direction") {
    pat <- paste0("(?<!^)", pat, "$")
  }

  if (pattern == "post_direction_db") {
    pat <- paste0(pat, "$")
  }

  # address parts that appear at the end
  if (pattern %in% c("all_street_suffix", "special_unit")) {
    pat <- paste0(pat, "$")
  }

  # capture items following the unit
  if (pattern == "unit") {
    letter_unit <- str_collapse_bound(LETTERS[!LETTERS %in% c("N", "S", "E", "W")])

    pat <- paste0(pat, "(.*)?$|#.*|\\b\\d+(\\W)?[:alpha:]$|(?<!^)\\b[:alpha:](\\W)?(\\d+)?$|(?<!^)\\d+$|", letter_unit, "$|\\bCOTTAGE$")
  }

  if (pattern == "unit_db") {
    pat <- paste0(pat, "(.*)?$|#.*|\\d+$")
  }

  if (pattern == "unit_type") {
    addr_abbr <- addr_abbr[addr_abbr$type == "unit", ]
    pat <- str_collapse_bound(unique(c(addr_abbr$long, addr_abbr$long, "COTTAGE")))
  }

  # if it's in the regex table, it's already ready to go
  if (pattern %in% regex_types) {
    addr_abbr <- regex_tbl[regex_tbl$address_part == pattern, ]
    pat <- addr_abbr$regex

  }

  pat

}

# If a street number range contains the same number twice, change it to a singular street number
check_street_range <- function(.data, street_number_range, street_number, addressr_id) {

  df_ranges <- .data |> filter(!is.na({{ street_number_range }}))

  df <- .data |> anti_join(df_ranges, by = "addressr_id")

  if (nrow(df_ranges) != 0) {

    range_1 <- sym("range_1")
    range_2 <- sym("range_2")

    df_ranges <- df_ranges |>
      mutate(street_number_range = str_replace_all(street_number_range, "\\W|AND", " ") |> str_squish()) |>
      separate_wider_delim({{ street_number_range }}, delim = " ", names = c("range_1", "range_2"), too_few = "align_start", too_many = "merge") |>
      mutate(
        # some street number ranges are the same number twice (105-105). only keep unique ranges
        {{ street_number_range }} := if_else(range_1 != range_2, paste0(range_1, "-", range_2), NA_character_),
        # for the ranges with duplicate numbers, save the number in the street_number col
        {{ street_number }} := if_else(!is.na(range_1) & range_1 == range_2, range_1, street_number)
      ) |>
      select(-c(range_1, range_2))

    df <- bind_rows(df, df_ranges) |> arrange(addressr_id)

  }

  df

}

check_unit <- function(.data, unit, street_number, street_suffix, addressr_id) {

  df_unit <- .data |> filter(!is.na({{ unit }}))

  df <- .data |> anti_join(df_unit, by = "addressr_id")

  if (nrow(df_unit) != 0) {

    abbr_street_suffix <- addr_abbr[addr_abbr$type == "all_street_suffix", ]
    abbr_street_suffix <- unique(c(abbr_street_suffix$short, abbr_street_suffix$long))

    df_unit <- df_unit |>
      mutate(
        # remove all non-word characters
        {{ unit }} := str_remove_all(unit, "\\W"),
        # check unit for street suffix
        {{ street_suffix }} := if_else(is.na({{ street_suffix }}) & ({{ unit }} %in% abbr_street_suffix), {{ unit }}, {{ street_suffix }}),
        # check if unit duplicates street number
        {{ unit }} := if_else({{ unit }} == {{ street_number }} | {{ unit }} == {{ street_suffix }}, NA_character_, {{ unit }})
      )

    df <- bind_rows(df, df_unit) |> arrange(addressr_id)

  }

  df

}

check_building <- function(.data, street_number, street_number_range, building, addressr_id) {

  # if the street number is missing & there's a number in the building, extract it.
  df_bldg <- .data |>
    filter(is.na({{ street_number }}) & is.na({{ street_number_range }}) & !is.na({{ building }}) & str_detect({{ building }}, "\\d"))

  df <- .data |> anti_join(df_bldg, by = "addressr_id")

  if (nrow(df_bldg) != 0) {

    df_bldg <- df_bldg |> extract_remove_squish({{ building }}, "street_number", "\\d+")

    df <- bind_rows(df, df_bldg) |> arrange(addressr_id)

  }

  df

}
