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

  pattern <- check_pattern(pattern)

  .data |>
    mutate(
      {{ new_column }} := str_extract({{ original_column }}, pattern),
      {{ original_column }} := str_remove({{ original_column }}, pattern) |> str_remove_all("^\\W+|\\W+$") |> str_squish() |> na_if("")
    )
}

#' Extract, remove, and squish a string in a dataframe
#'
#' @inheritParams extract_remove_squish
#'
#' @return An object of the same type as .data, with the following properties:
#'    * A modified original column, from which the pattern was removed and whitespace was trimmed.
#'    * A new column containing the extracted string.
#' @export
extract_remove_squish_db <- function(.data, original_column, new_column, pattern) {

  pattern <- check_pattern(pattern)

  extract_sql <- paste0("regexp_extract(", original_column, ", '", pattern, "')")
  remove_squish_sql <- paste0("trim(regexp_replace(", original_column, ", '", pattern, "', ''))")
  remove_messy_sql <- paste0("regexp_replace(", original_column, ", '(^\\W+|\\W+$)', '', 'g')")

  .data |>
    mutate(
      {{ new_column }} := sql(extract_sql),
      {{ original_column }} := sql(remove_squish_sql)
    ) |>
    mutate(
      {{ original_column }} := sql(remove_messy_sql)
    )
}
