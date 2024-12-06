#' Check the address pattern
#'
#' @param pattern The string to be extracted and removed.
#'
#' @return A string to use in pattern matching.
check_pattern <- function(pattern) {

  pat <- pattern

  # reference table: address_abbreviations
  abbr_tbl <- addressr::address_abbreviations
  abbr_types <- unique(abbr_tbl$type)
  # reference table: address_regex
  regex_tbl <- address_regex
  regex_types <- unique(regex_tbl$address_part)

  # both pre and post-directions use the "directions" table
  if (str_detect(pattern, "direction")) {
    abbr_tbl <- abbr_tbl[abbr_tbl$type == "directions", ]
    pat <- str_collapse_bound(unique(c(abbr_tbl$short, abbr_tbl$long)))

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
    abbr_tbl <- abbr_tbl[abbr_tbl$type == pattern, ]
    pat <- str_collapse_bound(unique(c(abbr_tbl$short, abbr_tbl$long)))

  }

  # address parts that appear at the end
  if (pattern %in% c("post_direction", "all_street_suffix", "special_units")) {
    pat <- paste0(pat, "$")
  }

  # capture items following the unit
  if (pattern == "unit") {
    pat <- paste0(pat, "(.*)?$|#.*|\\d+$")
  }

  if (pattern == "unit_type") {
    abbr_tbl <- abbr_tbl[abbr_tbl$type == "unit", ]
    pat <- str_collapse_bound(unique(c(abbr_tbl$short, abbr_tbl$long)))
  }

  # if it's in the regex table, it's already ready to go
  if (pattern %in% regex_types) {
    abbr_tbl <- regex_tbl[regex_tbl$address_part == pattern, ]
    pat <- abbr_tbl$regex

  }

  pat

}

# If a street number range contains the same number twice, change it to a singular street number
check_street_range <- function(.data, street_number_range, street_number) {

  addressr_id <- sym("addressr_id")
  df <- .data |> mutate(addressr_id = row_number())

  df_ranges <- df |> filter(!is.na({{ street_number_range }}))

  df <- df |> anti_join(df_ranges, by = "addressr_id")

  if (nrow(df_ranges) != 0) {

    range_name_1 <- sym(paste0(street_number_range, "_1"))
    range_name_2 <- sym(paste0(street_number_range, "_2"))

    df_ranges <- df_ranges |>
      mutate(street_number_range = str_remove_all(street_number_range, "\\s")) |>
      separate_wider_delim({{ street_number_range }}, delim = "-", names = c("1", "2"), names_sep = "_") |>
      mutate(
        # some street number ranges are the same number twice (105-105). only keep unique ranges
        {{ street_number_range }} := if_else({{ range_name_1 }} != {{ range_name_2 }}, paste0({{ range_name_1 }}, "-", {{ range_name_2 }}), NA_character_),
        # for the ranges with duplicate numbers, save the number in the street_number col
        {{ street_number }} := if_else(!is.na({{ range_name_1 }}) & {{ range_name_1 }} == {{ range_name_2 }}, {{ range_name_1 }}, street_number)
      ) |>
      select(-c(range_name_1, range_name_2))

    df <- bind_rows(df, df_ranges)

  }

  df <- df |> select(-addressr_id)

}

check_unit <- function(.data, unit, street_number, all_street_suffix) {

  addressr_id <- sym("addressr_id")
  df <- .data |> mutate(addressr_id = row_number())

  df_unit <- df |> filter(!is.na({{ unit }}))

  df <- df |> anti_join(df_unit, by = "addressr_id")

  if (nrow(df_unit) != 0) {

    addr_abbr <- addressr::address_abbreviations
    street_suffix <- addr_abbr[addr_abbr$type == "all_street_suffix", ]
    street_suffix <- unique(c(street_suffix$short, street_suffix$long))

    df_unit <- df_unit |>
      mutate(
        # remove all non-word characters
        {{ unit }} := str_remove_all(unit, "\\W"),
        # check unit for street suffix
        {{ all_street_suffix }} := if_else(is.na({{ all_street_suffix }}) & ({{ unit }} %in% street_suffix), {{ unit }}, {{ all_street_suffix }}),
        # check if unit duplicates street number
        {{ unit }} := if_else({{ unit }} == {{ street_number }} | {{ unit }} == {{ all_street_suffix }}, NA_character_, {{ unit }})
      )

    df <- bind_rows(df, df_unit) |> arrange(addressr_id)

  }

  df <- df |> select(-addressr_id)
}
