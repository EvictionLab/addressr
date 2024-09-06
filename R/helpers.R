str_collapse_bound <- function(vector) {
  str_c("\\b(", str_flatten(vector, collapse = "|"), ")\\b")
}

str_replace_names <- function(string, input, output) {
  # bound the string to not capture fragments within words
  input <- str_c("\\b", input, "\\b")
  # attach the input and output
  names(output) <- input
  # replace string
  str_replace_all(string, output)
}

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
