#' Clean Addresses
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param dataset The dataset to clean. Either "default", "quick", or "default_db"
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
  street_number_multi <- sym("street_number_multi")
  all_street_suffix <- sym("all_street_suffix")
  street_suffix <- sym("street_suffix")
  street_suffix_2 <- sym("street_suffix_2")
  pre_direction <- sym("pre_direction")
  post_direction <- sym("post_direction")
  building <- sym("building")
  extra_back <- sym("extra_back")
  street_name <- sym("street_name")
  po_box <- sym("po_box")

  if (dataset == "quick") {

    tic("quick clean address.")

    df <- .data |>
      mutate({{ raw_address }} := {{ input_column }}, {{ addressr_id }} := row_number(), .before = {{ input_column }}) |>
      mutate({{ input_column }} := str_to_upper({{ input_column }})) |>
      extract_remove_squish({{ input_column }}, "po_box", "po_box") |>
      extract_remove_squish({{ input_column }}, "extra_front", "^([A-Z\\W]+ )+(?=\\d+)") |>
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

    toc()

  }

  if (dataset == "default") {

    # step 1: preserve original input
    tic("preserve original data")
    original_row_id <- sym("original_row_id")
    df <- .data |>
      mutate({{ raw_address }} := {{ input_column }},
             {{ original_row_id }} := row_number(),
             {{ addressr_id }} := as.character({{ original_row_id }}),
             .before = {{ input_column }}) |>
      mutate({{ input_column }} := prep_address({{ input_column }}))
    toc()

    # step 2: separate out multiple addresses
    tic("separate multiple addresses")
    # current logic to delim: (etc + street suffix) + [punctuation, and, or space] + (numbers + etc + street suffix)
    all_suffix_regex <- str_collapse_bound(unique(c(all_street_suffixes$long, all_street_suffixes$short)))
    longer_regex <- paste0("(?<=", all_suffix_regex, ")\\s*([:punct:]| AND |\\s)\\s*(?=\\d+\\b.+", all_suffix_regex, ")")

    df_multi <- df |> filter(str_detect({{ input_column }}, longer_regex))
    df <- df |> anti_join(df_multi, by = "addressr_id")

    if (nrow(df_multi) != 0) {
      addressr_addr_id <- sym("addressr_addr_id")

      df_multi <- df_multi |>
        separate_longer_delim({{ input_column }}, delim = stringr::regex(longer_regex)) |>
        distinct() |>
        mutate({{ addressr_addr_id }} := row_number(), .by = "addressr_id") |>
        unite({{ addressr_id }}, c("addressr_id", "addressr_addr_id"), sep = "-A", remove = FALSE) |>
        select(-addressr_addr_id)

      df <- bind_rows(df, df_multi)

    }
    toc()

    # step 2.5: find PO Boxes (maybe add other weird addresses here)
    tic("extract p.o. boxes")
    df_box <- df |>
      extract_remove_squish({{ input_column }}, "po_box", "po_box") |>
      filter(!is.na(po_box))

    df <- df |> anti_join(df_box, by = "addressr_id")
    toc()

    # step 3: separate out address components from each address & standardize spellings
    tic("extract address parts")
    extra_regex <- str_glue("{all_suffix_regex}.*")
    # extra_regex <- paste0("(?<=(", all_suffix_regex, ")?.*", all_suffix_regex, ").*")
    # idea: rework the first part & improve street numbers, units, and buildings together
    df <- df |>
      extract_remove_squish({{ input_column }}, "po_box", "po_box") |>
      extract_remove_squish({{ input_column }}, "extra_front", "^([A-Z\\W]+ )+(?=\\d+)") |>
      extract_remove_squish({{ input_column }}, "street_number_fraction", "street_number_fraction") |>
      extract_remove_squish({{ input_column }}, "street_number_multi", "street_number_multi") |>
      extract_remove_squish({{ input_column }}, "street_number", "street_number") |>
      extract_remove_squish({{ input_column }}, "unit", "unit") |>
      extract_remove_squish({{ unit }}, "unit_type", "unit_type") |>
      extract_remove_squish({{ input_column }}, "special_unit", "special_unit") |>
      extract_remove_squish({{ input_column }}, "building", "building") |>
      extract_remove_squish({{ input_column }}, "post_direction", "post_direction") |>
      extract_remove_squish({{ input_column }}, "extra_back", extra_regex) |>
      extract_remove_squish({{ extra_back }}, "street_suffix", str_glue(".*{all_suffix_regex}")) |>
      extract_remove_squish({{ street_suffix }}, "street_suffix_2", str_glue(".*(?={all_suffix_regex}$)")) |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction")

    toc()


    tic("standardize street suffix, directions & ordinals")

    df <- df |>
      mutate({{ input_column }} := switch_abbreviation({{ input_column }}, "ordinals", "short-to-long")) |>
      mutate({{ street_suffix }} := switch_abbreviation({{ street_suffix }}, "official_street_suffixes", "long-to-short")) |>
      mutate(across(c({{ pre_direction }}, {{ post_direction }}), ~ switch_abbreviation(., "directions", "long-to-short")))
    toc()

    # check street number ranges
    # see checkers.r for these functions
    tic("check street ranges")
    df <- df |> check_street_range(street_number_multi, street_number, addressr_id)
    toc()

    # check units
    tic("check units")
    df <- df |>
      check_unit(unit, street_number, street_suffix, addressr_id) |>
      unite({{ unit }}, c("unit", "special_unit"), sep = " ", na.rm = TRUE) |>
      mutate({{ unit }} := str_squish({{ unit }}) |> na_if(""))
    toc()

    # check for missing street numbers in building column
    tic("check buildings")
    df <- df |> check_building(street_number, street_number_multi, building, addressr_id)
    toc()

    df_num <- df |> filter(is.na({{street_number}}) & str_starts({{input_column}}, "\\d+"))
    df <- df |> anti_join(df_num, by = "addressr_id")
    if (nrow(df_num) != 0) {
      df_num <- df_num |>
        extract_remove_squish({{input_column}}, "street_number", "^\\d+")
      df <- bind_rows(df, df_num)
    }

    # tidy up for output
    tic("tidy output")
    # add back P.O. Boxes
    if (nrow(df_box) != 0) {
      df <- bind_rows(df, df_box)
    }

    df <- df |>
      mutate({{ input_column }} := na_if({{ input_column }}, "")) |>
      rename("street_name" = {{ input_column }}) |>
      unite({{ street_name }}, c("street_name", "street_suffix_2"), sep = " ", na.rm = TRUE) |>
      mutate({{ street_name }} := str_squish({{ street_name }}) |> na_if("")) |>
      arrange({{ original_row_id }}, {{ addressr_id }}) |>
      unite("clean_address", c("po_box", "street_number", "street_number_multi", "street_number_fraction", "pre_direction", "street_name", "street_suffix", "post_direction"), sep = " ", remove = FALSE, na.rm = TRUE)
    toc()

    df

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
