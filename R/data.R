#' Address abbreviations
#'
#' Commonly used abbreviations in addresses.
#'
#' @format ## `address_abbreviations`
#' A data frame with 552 rows and 3 columns:
#' \describe{
#'  \item{short}{Abbreviation}
#'  \item{long}{Full word}
#'  \item{type}{Category of the abbreviation}
#' }
"address_abbreviations"

#' Address corrections
#'
#' Corrections to address errors
#'
#' @format ## `address_corrections`
#' A dataframe with 17 rows and 4 columns:
#' \describe{
#'   \item{input}{The original string found in the data}
#'   \item{output}{The string to output}
#'   \item{city}{The city/place for the correction}
#'   \item{state}{The state for the correction}
#' }
"address_corrections"
