#' Flatten and bound a vector for use in regex
#'
#' @param vector A vector or something that can be coerced to one.
#'
#' @return A flat bound string: `\\b(str1|str2)\\b`
#' @export
str_collapse_bound <- function(vector) {
  str_c("\\b(", str_flatten(vector, collapse = "|"), ")\\b")
}

#' Replace a string with another string
#'
#' @param string A string (or vector, column, etc) to be altered
#' @param input Pattern to find in the existing string
#' @param output Replacement for the matched pattern
#'
#' @return A modified version of the original string
#' @export
str_replace_names <- function(string, input, output) {
  # bound the string to not capture fragments within words
  input <- str_c("\\b", input, "\\b")
  # attach the input and output
  names(output) <- input
  # replace string
  str_replace_all(string, output)
}

#' Replace a string using the address_abbreviations table
#'
#' @param string A string (or vector, column, etc) to be altered
#' @param type The category of strings to filter for from `address_abbreviations$type`
#' @param method `"long-to-short"` (the default) to abbreviate the string, or `"short-to-long"` to un-abbreviate.
#'
#' @return A modified version of the original string
#' @export
switch_abbreviation <- function(string, type, method = "long-to-short") {

  adr_abbr <- addressr::address_abbreviations
  # make a vector of the available abbreviation types
  abbr_types <- unique(adr_abbr$type)

  # catch errors
  if (!type %in% abbr_types) {
    type_message <- paste0("type must be one of the following:", str_flatten_comma(abbr_types, last = ", or "))
    stop(type_message)
  }
  if (!method %in% c("short-to-long", "long-to-short")) {
    stop("method must be 'short-to-long' or 'long-to-short'")
  }

  # filter abbreviations to the selected type
  df <- adr_abbr[adr_abbr$type == type, ]

  # switch abbreviations based on method
  if (method == "short-to-long") {
    str_replace_names(string, df$short, df$long)
  } else {
    str_replace_names(string, df$long, df$short)
  }

}

#' Replace a string in a dataframe using the address_abbreviations table
#'
#' @param .data the dataframe
#' @param column The column to be altered
#' @param type The category of strings to filter for from `address_abbreviations$type`
#' @param method `"long-to-short"` (the default) to abbreviate the string, or `"short-to-long"` to un-abbreviate.
#'
#' @return A modified version of the original string
#' @export
switch_abbreviation_db <- function(.data, column, type, method = "long-to-short") {
  adr_abbr <- addressr::address_abbreviations
  abbr_types <- unique(adr_abbr$type)

  # catch errors
  if (!type %in% abbr_types) {
    type_message <- paste0("type must be one of the following:", str_flatten_comma(abbr_types, last = ", or "))
    stop(type_message)
  }
  if (!method %in% c("short-to-long", "long-to-short")) {
    stop("method must be 'short-to-long' or 'long-to-short'")
  }

  # filter abbreviations to the selected type
  lookup <- adr_abbr[adr_abbr$type == type, ]

  # switch abbreviations based on method
  if (method == "short-to-long") {

      pattern <- lookup$short
      replacement <- lookup$long

  } else {

      pattern <- lookup$long
      replacement <- lookup$short

  }

  replace_statements <- map2(pattern, replacement, ~ glue::glue("regexp_replace( '\\b{.x}\\b', '{.y}', 'g')"))
  replace_regex <- paste0(column, ".", paste0(replace_statements, collapse = "."))

  .data |>
    mutate({{ column }} := sql(replace_regex))
}

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
