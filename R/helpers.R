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

  # make a vector of the available abbreviation types
  abbr_types <- unique(addr_abbr$type)

  # catch errors
  if (!type %in% abbr_types) {
    type_message <- paste0("type must be one of the following:", str_flatten_comma(abbr_types, last = ", or "))
    stop(type_message)
  }
  if (!method %in% c("short-to-long", "long-to-short")) {
    stop("method must be 'short-to-long' or 'long-to-short'")
  }

  # filter abbreviations to the selected type
  df <- addr_abbr[addr_abbr$type == type, ]

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
  abbr_types <- unique(addr_abbr$type)

  # catch errors
  if (!type %in% abbr_types) {
    type_message <- paste0("type must be one of the following:", str_flatten_comma(abbr_types, last = ", or "))
    stop(type_message)
  }
  if (!method %in% c("short-to-long", "long-to-short")) {
    stop("method must be 'short-to-long' or 'long-to-short'")
  }

  # filter abbreviations to the selected type
  lookup <- addr_abbr[addr_abbr$type == type, ]

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

prep_address <- function(string) {
  x <- string |>
    str_to_upper() |>
    str_remove_all("\\.") |>
    str_replace_all(c(
      "," = " ",
      "(\\d+)([ABCDEFGHIJKLMOPQUVWXYZ][ABCEFGIJKLMNOPQRSUVWXYZ][A-Z]*)" = "\\1 \\2"
      )) |>
    str_squish()
}
