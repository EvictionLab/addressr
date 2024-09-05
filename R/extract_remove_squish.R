#' Extract, remove, and squish a string
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param original_column The column from which the string should be extracted, then removed, then squished to remove extra whitespace.
#' @param new_column The name of the column to output the extracted strings.
#' @param pattern The string to be extracted and removed.
#'
#' @return An object of the same type as .data, with the following properties:
#'    * A modified original column, from which the pattern was removed and whitespace was trimmed.
#'    * A new column containing the extracted string.
#' @export
#'
#' @examples
#' df <- dplyr::tibble(row_id = 1, address = "123 Main St")
#' df |> extract_remove_squish(address, street_number, "^\\d+")
extract_remove_squish <- function(.data, original_column, new_column, pattern) {
  .data |>
    mutate(
      {{ new_column }} := str_extract({{ original_column }}, pattern),
      {{ original_column }} := str_remove({{ original_column }}, pattern) |> str_squish()
    )
}
