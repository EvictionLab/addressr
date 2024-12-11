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
  # column names. these prevent global variable warnings
  addressr_id <- sym("addressr_id")
  clean_address <- sym("clean_address")
  raw_address <- sym("raw_address")
  unit <- sym("unit")
  street_number <- sym("street_number")
  street_number_range <- sym("street_number_range")
  all_street_suffix <- sym("all_street_suffix")
  street_suffix <- sym("street_suffix")
  pre_direction <- sym("pre_direction")
  post_direction <- sym("post_direction")
  building <- sym("building")

  if (dataset == "quick") {
    df <- .data |>
      mutate({{ raw_address }} := {{ input_column }}, {{ addressr_id }} := row_number(), .before = {{ input_column }}) |>
      mutate({{ input_column }} := str_to_upper({{ input_column }})) |>
      extract_remove_squish({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "street_number_multi", "street_number_multi") |>
      extract_remove_squish({{ input_column }}, "street_number_range", "street_number_range") |>
      extract_remove_squish({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "unit", "unit") |>
      extract_remove_squish({{ unit }}, "unit_type", "unit_type") |>
      extract_remove_squish({{ input_column }}, "special_unit", "special_unit") |>
      extract_remove_squish({{ input_column }}, "building", "building") |>
      extract_remove_squish({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "street_suffix", "all_street_suffix") |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction")

    # tidy up for output
    df <- df |>
      mutate({{ input_column }} := na_if({{ input_column }}, "")) |>
      rename("street_name" = {{ input_column }}) |>
      unite("clean_address", c("street_number", "street_number_range", "street_number_fraction", "pre_direction", "street_name", "street_suffix", "post_direction"), sep = " ", remove = FALSE, na.rm = TRUE)
  }

  if (dataset == "default") {

    # the big separation
    df <- .data |>
      mutate({{ raw_address }} := {{ input_column }}, {{ addressr_id }} := row_number(), .before = {{ input_column }}) |>
      mutate({{ input_column }} := str_to_upper({{ input_column }})) |>
      extract_remove_squish({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "street_number_multi", "street_number_multi") |>
      extract_remove_squish({{ input_column }}, "street_number_range", "street_number_range") |>
      extract_remove_squish({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "unit", "unit") |>
      extract_remove_squish({{ unit }}, "unit_type", "unit_type") |>
      extract_remove_squish({{ input_column }}, "special_unit", "special_unit") |>
      extract_remove_squish({{ input_column }}, "building", "building") |>
      extract_remove_squish({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "street_suffix", "all_street_suffix") |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction") |>
      mutate({{ input_column }} := switch_abbreviation({{ input_column }}, "ordinals", "short-to-long")) |>
      mutate({{ street_suffix }} := switch_abbreviation({{ street_suffix }}, "official_street_suffixes", "long-to-short")) |>
      mutate(across(c({{ pre_direction }}, {{ post_direction }}), ~ switch_abbreviation(., "directions", "long-to-short")))

    # see checkers.r for these functions
    # check street number ranges
    df <- df |> check_street_range(street_number_range, street_number, addressr_id)

    # check units
    df <- df |>
      check_unit(unit, street_number, street_suffix, addressr_id) |>
      unite({{ unit }}, c("unit", "special_unit"), sep = " ", na.rm = TRUE) |>
      mutate({{ unit }} := str_squish({{ unit }}) |> na_if(""))

    # check for missing street numbers in building column
    df <- df |> check_building(street_number, street_number_range, building, addressr_id)

    # tidy up for output
    df <- df |>
      mutate({{ input_column }} := na_if({{ input_column }}, "")) |>
      rename("street_name" = {{ input_column }}) |>
      unite("clean_address", c("street_number", "street_number_range", "street_number_fraction", "pre_direction", "street_name", "street_suffix", "post_direction"), sep = " ", remove = FALSE, na.rm = TRUE)

  } else if (dataset == "default_db") {

    df <- .data |>
      extract_remove_squish_db({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish_db({{ input_column }}, "street_number_range", "street_number_range_db") |>
      extract_remove_squish_db({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish_db({{ input_column }}, "unit", "unit_db") |>
      extract_remove_squish_db({{ input_column }}, "special_unit", "special_unit") |>
      extract_remove_squish_db({{ input_column }}, "building", "building") |>
      extract_remove_squish_db({{ input_column }}, "pre_direction", "pre_direction_db") |>
      extract_remove_squish_db({{ input_column }}, "post_direction", "post_direction_db") |>
      extract_remove_squish_db({{ input_column }}, "all_street_suffix", "all_street_suffix") |>
      compute()

    df <- df |>
      switch_abbreviation_db({{ input_column }}, "ordinal", "short-to-long")

  }

}
