#' Clean Addresses
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param method The method for address cleaning. Either `"default"`, `"quick"`, or `"default_db"`
#' @param output Address columns to output. Defaults to "everything", which outputs all original columns in the dataset, row id columns, all address components, and the "clean_address". Can also provide a character vector of columns, including the following aggregate columns:
#'    * "clean_address": street number + pre direction + street name + street suffix + post direction
#'    * "short_address": street number + street name
#'
#'    And/or individual address components:
#'    * "po_box"
#'    * "street_number_coords": A coordinate-style street number (N123 E456 Main St)
#'    * "street_number": NOTE if only one column starting with "street_number" is included, all columns starting with "street_number" are united into one "street_number" column.
#'    * "street_number_multi": Street number containing multiple numbers
#'    * "street_number_fraction": A fraction included in the street number
#'    * "pre_direction"
#'    * "street_name"
#'    * "street_suffix"
#'    * "post_direction"
#'    * "unit"
#'    * "unit_type": the type of unit, such as "apt", "#", etc.
#'    * "building"
#'    * "extra": Extra characters which were not sorted into any column. Can also be broken into "extra_front", "extra_back", and "extra_unit"
#' @param address_column The column from which the string should be extracted, then removed, then squished to remove extra whitespace.
#' @param separate_street_range Should street numbers with multiple numbers be pivoted into individual rows? Default is FALSE
#' @param separate_multi_address Should rows with multiple addresses be pivoted into individual rows? Default is FALSE
#'
#' @return An object of the same type as .data, with the following properties:
#'    * A modified original column, from which the pattern was removed and whitespace was trimmed.
#'    * New column containing the extracted strings.
#' @export
clean_address <- function(.data, address_column, method = "default", output = "everything", separate_street_range = FALSE, separate_multi_address = FALSE) {

  # column names. these prevent global variable warnings
  input_column <- sym("input_column")
  addressr_id <- sym("addressr_id")
  clean_address <- sym("clean_address")
  raw_address <- sym("raw_address")
  unit <- sym("unit")
  unit_type <- sym("unit_type")
  street_number <- sym("street_number")
  street_number_coords <- sym("street_number_coords")
  street_number_range <- sym("street_number_range")
  street_number_multi <- sym("street_number_multi")
  street_number_fraction <- sym("street_number_fraction")
  all_street_suffix <- sym("all_street_suffix")
  street_suffix <- sym("street_suffix")
  street_suffix_2 <- sym("street_suffix_2")
  street_suffix_3 <- sym("street_suffix_3")
  pre_direction <- sym("pre_direction")
  post_direction <- sym("post_direction")
  building <- sym("building")
  extra_back <- sym("extra_back")
  special_unit <- sym("special_unit")
  special_unit_2 <- sym("special_unit_2")
  street_name <- sym("street_name")
  po_box <- sym("po_box")

  df_names <- names(.data)

  df <- .data |> mutate({{ input_column }} := {{ address_column }})

  v_clean_address <- c("po_box", "street_number_coords", "street_number", "street_number_multi", "street_number_fraction", "pre_direction", "street_name", "street_suffix", "post_direction")

  if (!method %in% c("quick", "default", "default_db")) stop("Invalid method argument")

  if (method == "quick") {

    tic("quick clean address.")

    df <- df |>
      mutate({{ addressr_id }} := row_number(), .before = {{ input_column }}) |>
      mutate({{ input_column }} := str_to_upper({{ input_column }})) |>
      extract_remove_squish({{ input_column }}, "po_box", "po_box") |>
      extract_remove_squish({{ input_column }}, "extra_front", "^([A-Z\\W]+ )+(?=\\d+)") |>
      extract_remove_squish({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "street_number_multi", "street_number_multi") |>
      extract_remove_squish({{ input_column }}, "street_number_range", "street_number_range") |>
      extract_remove_squish({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "unit", "unit") |>
      extract_remove_squish({{ unit }}, "unit_type", "unit_type") |>
      extract_remove_squish({{ input_column }}, "special_unit", "special_unit_regex") |>
      extract_remove_squish({{ input_column }}, "building", "building") |>
      extract_remove_squish({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "street_suffix", "all_street_suffix") |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction")

    # tidy up for output
    df <- df |>
      mutate({{ input_column }} := na_if({{ input_column }}, "")) |>
      rename("street_name" = {{ input_column }}) |>
      unite("clean_address", any_of(v_clean_address), sep = " ", remove = FALSE, na.rm = TRUE)

    toc()

    df

  }

  else if (method == "default") {

    tic("total clean time")

    # step 1: preserve original input
    tic("preserve original data")
    original_row_id <- sym("original_row_id")
    df <- df |>
      mutate({{ original_row_id }} := row_number(),
             {{ addressr_id }} := as.character({{ original_row_id }}),
             .before = {{ input_column }})

    df_null <- df |> filter(is.na({{ input_column }}))
    df <- df |>
      filter(!is.na({{ input_column }})) |>
      mutate({{ input_column }} := prep_address({{ input_column }}))

    toc()

    # step 1.5: check issue-causing street names with numbers
    # see checkers.r for any check_.*() function
    df <- df |> check_fractional_names({{ input_column }})

    df <- df |> check_highways({{ input_column }})

    # step 2: separate out multiple addresses
    all_suffix_regex <- str_collapse_bound(unique(c(all_street_suffixes$long, all_street_suffixes$short)))

    if (separate_multi_address) {

      tic("separate multiple addresses")
      df <- df |> check_multi_address({{ input_column }}, addressr_id, all_suffix_regex)
      toc()

    }

    # step 2.5: find PO Boxes (maybe add other weird addresses here (highways, 13 colony mall))
    tic("extract address parts")
    df_box <- df |>
      extract_remove_squish({{ input_column }}, "po_box", "po_box") |>
      mutate({{ po_box }} := str_replace_all({{ po_box }}, "(P?.*O?.*BOX)\\W(\\w+)", "PO BOX \\2"),
             {{ extra_back }} := {{ input_column }},
             {{ input_column }} := NA_character_) |>
      filter(!is.na(po_box))

    df <- df |> anti_join(df_box, by = "addressr_id")


    # step 3: separate out address components from each address & standardize spellings
    common_suffix_regex <- str_collapse_bound(unique(c(most_common_suffixes$long, most_common_suffixes$short, "AV", "BV", "CI")))
    uncommon_suffix_regex <- str_collapse_bound(unique(c(least_common_suffixes$long, least_common_suffixes$short)))
    pre_direction_regex <- str_collapse_bound(unique(c(directions$long, directions$short)))

    # TODO: rework the first part & improve street numbers, units, and buildings together
    df <- df |>
      extract_remove_squish({{ input_column }}, "extra_front", "^([A-Z\\W]+ )+(?!\\d+\\W?[NSEW]\\s?\\d+?)") |>
      extract_remove_squish({{ input_column }}, "street_number_coords", "street_number_coords") |>
      extract_remove_squish({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "street_number_multi", "street_number_multi") |>
      extract_remove_squish({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "unit", "unit") |>
      extract_remove_squish({{ unit }}, "unit_type", "unit_type") |>
      extract_remove_squish({{ unit }}, "special_unit_2", "special_units_regex") |>
      extract_remove_squish({{ input_column }}, "special_unit", "special_units_regex") |>
      extract_remove_squish({{ input_column }}, "building", "building") |>
      extract_remove_squish({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "extra_back", str_glue("(?<!^({pre_direction_regex} )?){all_suffix_regex}.*|(?<=^{pre_direction_regex} ){common_suffix_regex}$")) |>
      extract_remove_squish({{ extra_back }}, "street_suffix_2", str_glue(".*{common_suffix_regex}|.*?{all_suffix_regex}")) |>
      extract_remove_squish({{ extra_back }}, "street_suffix_3", str_glue("^{all_suffix_regex}")) |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction")

    toc()


    tic("standardize street suffix, directions & ordinals")

    replace_coords <- c(
      "([NSEW])\\s?(\\d+)\\W?([NSEW])\\s?(\\d+)" = "\\1\\2 \\3\\4",
      "^(\\d{3})\\s?([NSEW])\\s?(\\d+)" = "\\1 \\2\\3",
      "^([NSEW])\\s?(\\d+)$" = "\\1\\2"
    )

    df <- df |>
      mutate(across(c({{ street_number }}, {{ street_number_multi}}), ~ str_remove_all(., "^0+"))) |>
      # ordinals
      mutate({{ input_column }} := str_replace_all({{ input_column }}, "\\b\\d{1,3}[RSTN][DTH]\\b", replace_ordinals)) |>
      # street number coords
      mutate({{ street_number_coords }} := str_replace_all({{ street_number_coords }}, replace_coords)) |>
      # street suffixes
      mutate(across(c({{ street_suffix_2 }}, {{ street_suffix_3 }}), ~ switch_abbreviation(., "all_street_suffixes", "long-to-short")),
             across(c({{ street_suffix_2 }}, {{ street_suffix_3 }}), ~ switch_abbreviation(., "official_street_suffixes", "short-to-long"))) |>
      # mutate({{ street_suffix_2 }} := switch_abbreviation({{ street_suffix_2 }}, "all_street_suffixes", "long-to-short")) |>
      # mutate({{ street_suffix_2 }} := switch_abbreviation({{ street_suffix_2 }}, "official_street_suffixes", "short-to-long")) |>
      mutate({{ street_suffix_2 }} := str_replace({{ street_suffix_2 }}, str_glue("({common_suffix_regex}) \\1"), "\\1")) |>
      extract_remove_squish({{ street_suffix_2 }}, "street_suffix", str_glue("{common_suffix_regex}$")) |>
      unite("street_suffix", any_of(c("street_suffix", "street_suffix_3")), na.rm = TRUE, sep = " ") |>
      mutate({{ street_suffix }} := na_if({{ street_suffix }}, "")) |>
      # directions
      mutate(across(c({{ pre_direction }}, {{ post_direction }}), ~ switch_abbreviation(., "directions", "long-to-short")),
             {{ post_direction }} := if_else((!is.na({{ pre_direction }}) & {{ post_direction }} == {{ pre_direction }}), NA_character_, {{ post_direction }}))
    toc()

    # check street number ranges
    tic("check street numbers, units, and buildings")

    if (separate_street_range) {

      df <- df |> check_street_range(street_number_multi, street_number, addressr_id, building)

    } else {

      df <- df |> mutate(
        {{ street_number_multi }} := str_remove_all({{ street_number_multi }}, "\\s+(?=\\-)|(?<=\\-)\\s+"),
        across(c(street_number_multi, street_number), str_squish))

    }

    df <- df |> check_unit({{ input_column }}, unit, unit_type, special_unit, special_unit_2, street_number, street_suffix, building, addressr_id)

    df <- df |> check_missing_number({{ input_column }}, street_number, street_number_multi, street_number_coords, building, addressr_id)

    toc()

    # tidy up for output
    tic("tidy output")
    # add back P.O. Boxes
    if (nrow(df_box) != 0) {
      df <- bind_rows(df, df_box)
    }

    if (nrow(df_null) != 0) {
      df <- bind_rows(df, df_null)
    }

    df <- df |>
      mutate({{ input_column }} := na_if({{ input_column }}, "")) |>
      mutate({{ input_column }} := str_replace_names({{ input_column }}, special_street_names$regex, special_street_names$output)) |>
      rename("street_name" = {{ input_column }}) |>
      unite({{ street_name }}, any_of(c("street_name", "street_suffix_2")), sep = " ", na.rm = TRUE) |>
      mutate({{ street_name }} := str_squish({{ street_name }}) |> na_if("")) |>
      arrange({{ original_row_id }}, {{ addressr_id }}) |>
      unite("clean_address", any_of(v_clean_address), sep = " ", remove = FALSE, na.rm = TRUE)
    toc()

    toc()

    df

  } else if (method == "default_db") {

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

  # part 3: output
  full_output <- str_flatten_comma(output)

  if (nrow(.data) != nrow(df)) df_names <- c(df_names, "original_row_id", "addressr_id")

  if (length(output) == 1) {

    if (output == "everything") df_names <- c(df_names, "clean_address", v_clean_address, "building", "unit_type", "unit", "extra_front", "extra_back", "extra_unit")

  } else {

    if ("short_address" %in% output) {

      df <- df |>
        unite(street_number, any_of(c("street_number_multi", "street_number_coords", "street_number_range", "street_number")), na.rm = TRUE, sep = " ") |>
        unite("short_address", c(street_number, street_name, po_box), na.rm = TRUE, sep = " ", remove = FALSE) |>
        mutate(across(c("short_address"), ~ str_squish(.) |> na_if("")))

    }

    if ("extra" %in% output) {

      df <- df |>
        unite("extra", any_of(starts_with("extra_")), sep = " ", remove = FALSE, na.rm = TRUE) |>
        mutate(across(c("extra"), ~ str_squish(.) |> na_if("")))

    }

    if ("street_number" %in% output & str_count(full_output, "street_number") == 1) {

      df <- df |>
        unite(street_number, any_of(c("street_number_multi", "street_number_coords", "street_number_range", "street_number")), na.rm = TRUE, sep = " ") |>
        mutate(across(c("street_number"), ~ str_squish(.) |> na_if("")))

    }

    df_names <- c(df_names, output)

  }

  df |> select(any_of(df_names))

}
