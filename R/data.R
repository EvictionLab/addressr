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

#' Table reference
#'
#' A reference for commonly used tables
#'
#' @format ## `table_reference`
#' A dataframe with 78 rows and 5 columns
#' \describe{
#'   \item{table_name}{The name of the table}
#'   \item{original_column}{The name of the column as originally found in the data}
#'   \item{rename_column}{A column name to replace the original name}
#'   \item{data_category}{The category of the data column}
#'   \item{data_type}{The data type of the column}
#' }
"table_reference"
