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
clean_address <- function(.data, input_column, dataset = "default") {

  if (dataset == "default") {
    raw_address <- sym("raw_address")

    df <- .data |>
      mutate({{ input_column }} := str_to_upper({{ input_column }})) |>
      mutate({{ raw_address }} := {{ input_column }}, .before = {{ input_column }}) |>
      extract_remove_squish({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "street_number_range", "street_number_range") |>
      extract_remove_squish({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "unit", "unit") |>
      extract_remove_squish({{ input_column }}, "special_unit", "special_unit") |>
      extract_remove_squish({{ input_column }}, "building", "building") |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction") |>
      extract_remove_squish({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "all_street_suffix", "all_street_suffix") |>
      mutate({{ input_column }} := switch_abbreviation({{ input_column }}, "ordinal", "short-to-long"))

    df <- df |> check_street_range("street_number_range", "street_number")

  } else if (dataset == "default_db") {

    df <- .data |>
      extract_remove_squish_db({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish_db({{ input_column }}, "street_number_range", "street_number_range") |>
      extract_remove_squish_db({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish_db({{ input_column }}, "unit", "unit") |>
      extract_remove_squish_db({{ input_column }}, "special_unit", "special_unit") |>
      extract_remove_squish_db({{ input_column }}, "building", "building") |>
      extract_remove_squish_db({{ input_column }}, "pre_direction", "pre_direction") |>
      extract_remove_squish_db({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish_db({{ input_column }}, "all_street_suffix", "all_street_suffix") |>
      compute()

    df <- df |>
      switch_abbreviation_db({{ input_column }}, "ordinal", "short-to-long")

  }

}
