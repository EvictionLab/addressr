#' Clean Addresses
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param dataset The dataset to clean. Currently "lexis" is only available.
#' @param input_column The column from which the string should be extracted, then removed, then squished to remove extra whitespace.
#'
#' @return An object of the same type as .data, with the following properties:
#'    * A modified original column, from which the pattern was removed and whitespace was trimmed.
#'    * New column containing the extracted strings.
#' @export
clean_address <- function(.data, dataset, input_column) {

  if (dataset == "lexis") {

    .data |>
      extract_remove_squish({{ input_column }}, "lexis_street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "lexis_street_number_range", "street_number_range") |>
      extract_remove_squish({{ input_column }}, "lexis_street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "lexis_unit", "unit") |>
      extract_remove_squish({{ input_column }}, "lexis_pre_direction", "pre_direction") |>
      extract_remove_squish({{ input_column }}, "lexis_post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "lexis_all_street_suffix", "all_street_suffix")

  } else if (dataset == "lexis_db") {

    .data |>
      extract_remove_squish_db({{ input_column }}, "lexis_street_number_fraction", "street_number_fraction") |>
      extract_remove_squish_db({{ input_column }}, "lexis_street_number_range", "street_number_range") |>
      extract_remove_squish_db({{ input_column }}, "lexis_street_number", "street_number") |>
      extract_remove_squish_db({{ input_column }}, "lexis_unit", "unit") |>
      extract_remove_squish_db({{ input_column }}, "lexis_pre_direction", "pre_direction") |>
      extract_remove_squish_db({{ input_column }}, "lexis_post_direction", "post_direction") |>
      extract_remove_squish_db({{ input_column }}, "lexis_all_street_suffix", "all_street_suffix")

  }

}
