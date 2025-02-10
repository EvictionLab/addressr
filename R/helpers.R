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
    str_replace_all(c(
      "(?<!\\d),(?! \\d)" = " ",
      "\\bN\\W?A\\b" = " ",
      "&AMP;" = "&",
      "(\\w)\\.(\\w)" = "\\1 \\2",
      "(\\d+)([ABCDEFGHIJKLMOPQUVWXYZ][ABCEFGIJKLMNOPQRSUVWXYZ][A-Z]*)" = "\\1 \\2",
      "(\\d) (ST) (RD|ROAD) #? ?(\\d{2})" = "\\1 STATE \\3 \\4",
      "(\\d)\\s?([SNRT][TDH])\\s?(ST|AVE|DR|R(OA)?D|LN|LANE|CIR|CT|COURT|PL|WAY|BLVD|BOU|STRA|CV|COVE)" = "\\1\\2 \\3",
      "(\\d)\\s?([RTDH])\\s?(ST|AVE|DR|R(OA)?D|LN|LANE|CIR|CT|COURT|PL|WAY|BLVD|BOU|STRA|CV|COVE)" = "\\1TH \\3",
      "((DR|DOCTOR)\\W*)?M(ARTIN)?\\W*L(UTHER)?\\W*K(ING)?(\\W+(JR|JUNIOR))?" = "MARTIN LUTHER KING",
      "(\\d{2,}) (\\d-?[A-Z]|[A-Z]-?\\d) ([\\w\\s]+) (ST|AVE|DR|R(OA)?D|LN|LANE|CIR|CT|COURT|PL|WAY|BLVD|BOU|STRA|CV|COVE)" = "\\1 \\3 \\4 \\2 "
      )) |>
    str_remove_all(c("\\.|'")) |>
    str_replace_all("(?<!/)\\b\\d{1,2}(?= MILE)", replace_number) |>
    str_replace_all("(?<=\\d[A-Z]? [NSEW] )\\d{1,3}(?= (ST|AVE|DR|R(OA)?D|LN|LANE|CIR|CT|COURT|PL|WAY|BLVD|BOU|STRA|CV|COVE))", replace_ordinals) |>
    str_squish()
}

replace_ordinals <- function(string) {
  number <- as.numeric(str_extract(string, "\\d+"))
  x <- ordinals[ordinals$number == number, "long"]
  as.character(x)
}

replace_number <- function(string) {
  if (str_detect(string, "\\d")) {
    number <- as.numeric(str_extract(string, "\\d+"))
    x <- ordinals[ordinals$number == number, "long_number"]
    x <- as.character(x)
  } else {
    x <- string
  }
  x
}

replace_fraction <- function(string) {
  num_1 <- str_extract(string, "^\\d+")
  num_1 <- replace_number(num_1)
  num_2 <- str_extract(string, " (\\d+|\\w+)[\\s/]")
  num_2 <- replace_number(num_2)
  num_3 <- str_extract(string, "(\\d+|HALF|FOUR|EIGH|SIXTEEN)(?=(THS?)?$)")

  if (str_detect(num_3, "\\d")) {
    num_3 <- fractions[fractions$short == num_3, "long"]
  } else if (num_3 %in% c("FOUR", "EIGH", "SIXTEEN")) {
    num_3 <- paste0(num_3, "THS")
  }

  paste(num_1, "AND", num_2, num_3)
}
