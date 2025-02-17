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
  unit_type <- sym("unit_type")
  street_number <- sym("street_number")
  street_number_coords <- sym("street_number_coords")
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

  v_clean_address <- c("po_box", "street_number_coords", "street_number", "street_number_multi", "street_number_fraction", "pre_direction", "street_name", "street_suffix", "post_direction")

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

  else if (dataset == "default") {

    tic("total clean time")

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

    # step 1.5: change fractional street names
    regex_frac <- check_pattern("street_name_fraction")
    df_frac <- df |> filter(str_detect({{ input_column }}, regex_frac))
    df <- df |> anti_join(df_frac, by = "addressr_id")

    if (nrow(df_frac) != 0) {
      df_frac <- df_frac |>
        mutate({{ input_column }} := str_replace_all({{ input_column }}, regex_frac, replace_fraction))
      df <- bind_rows(df, df_frac)
    }

    df <- df |> check_highways({{ input_column }})

    # step 2: separate out multiple addresses
    tic("separate multiple addresses")
    # current logic to delim: (etc + street suffix) + [punctuation, and, or space] + (numbers + etc + street suffix)
    all_suffix_regex <- str_collapse_bound(unique(c(all_street_suffixes$long, all_street_suffixes$short)))
    longer_regex <- paste0("(?<=", all_suffix_regex, ")\\s*([:punct:]| AND |\\s)\\s*(?=\\d+\\b.+", all_suffix_regex, ")")
    # second logic to delim: (numbers + word 4-20 letters) + [punctuation, and, or space] + (numbers + same word)
    longer_regex_2 <- "(?<=\\d (([NSEW] )?\\w{4,20}))\\s*([:punct:]| AND |\\s)\\s*(?=\\d+(\\W\\d+)? \\1)"

    df_multi <- df |> filter(str_detect({{ input_column }}, longer_regex) | str_detect({{ input_column }}, longer_regex_2))
    df <- df |> anti_join(df_multi, by = "addressr_id")

    if (nrow(df_multi) != 0) {
      addressr_addr_id <- sym("addressr_addr_id")

      df_multi <- df_multi |>
        separate_longer_delim({{ input_column }}, delim = stringr::regex(longer_regex)) |>
        separate_longer_delim({{ input_column }}, delim = stringr::regex(longer_regex_2)) |>
        distinct() |>
        mutate({{ addressr_addr_id }} := row_number(), .by = "addressr_id") |>
        unite({{ addressr_id }}, c("addressr_id", "addressr_addr_id"), sep = "-A", remove = FALSE) |>
        select(-addressr_addr_id)

      df <- bind_rows(df, df_multi)

    }
    toc()

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


    common_suffix_regex <- str_collapse_bound(unique(c(most_common_suffixes$long, most_common_suffixes$short)))
    uncommon_suffix_regex <- str_collapse_bound(unique(c(least_common_suffixes$long, least_common_suffixes$short)))
    pre_direction_regex <- str_collapse_bound(unique(c(directions$long, directions$short)))

    # TODO: rework the first part & improve street numbers, units, and buildings together
    df <- df |>
      extract_remove_squish({{ input_column }}, "extra_front", "^([A-Z\\W]+ )+(?!\\d+\\W?[NSEW]\\s?\\d+)") |>
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
      extract_remove_squish({{ extra_back }}, "street_suffix", str_glue("({all_suffix_regex} )?{all_suffix_regex}")) |>
      extract_remove_squish({{ input_column }}, "pre_direction", "pre_direction")

    toc()


    tic("standardize street suffix, directions & ordinals")

    replace_coords <- c(
      "([NSEW])\\s?(\\d+)\\W?([NSEW])\\s?(\\d+)" = "\\1\\2 \\3\\4",
      "^(\\d{3})\\s?([NSEW])\\s?(\\d+)" = "\\1 \\2\\3"
    )

    df <- df |>
      # ordinals
      mutate(
        # {{ input_column }} := str_replace_names({{ input_column}}, ordinals$short, ordinals$long),
        {{ input_column }} := str_replace_all({{ input_column }}, "\\b\\d{1,3}[RSTN][DTH]\\b", replace_ordinals)
        ) |>
      # street number coords
      mutate({{ street_number_coords }} := str_replace_all({{ street_number_coords }}, replace_coords)) |>
      # street suffixes
      mutate({{ street_suffix }} := switch_abbreviation({{ street_suffix }}, "all_street_suffixes", "long-to-short")) |>
      mutate({{ street_suffix }} := switch_abbreviation({{ street_suffix }}, "official_street_suffixes", "short-to-long")) |>
      extract_remove_squish({{ street_suffix }}, "street_suffix_2", str_glue("^({uncommon_suffix_regex} *)+")) |>
      mutate({{ street_suffix }} := str_replace({{ street_suffix }}, str_glue("({common_suffix_regex}) \\1"), "\\1")) |>
      # directions
      mutate(across(c({{ pre_direction }}, {{ post_direction }}), ~ switch_abbreviation(., "directions", "long-to-short")),
             {{ post_direction }} := if_else((!is.na({{ pre_direction }}) & {{ post_direction }} == {{ pre_direction }}), NA_character_, {{ post_direction }}))
    toc()

    # check street number ranges
    # see checkers.r for these functions
    tic("check street numbers, units, and buildings")
    df <- df |> check_street_range(street_number_multi, street_number, addressr_id)

    # check units
    df <- df |>
      check_unit(unit, unit_type, street_number, street_suffix, building, addressr_id) |>
      unite({{ unit }}, c("unit", "special_unit", "special_unit_2"), sep = " ", na.rm = TRUE) |>
      mutate({{ unit }} := switch_abbreviation({{ unit }}, "special_units", "short-to-long"),
             {{ unit }} := str_squish({{ unit }}) |> na_if(""))

    # check for missing street numbers in building column
    df <- df |> check_building(street_number, street_number_multi, building, addressr_id)

    df_num <- df |> filter(is.na({{street_number}}) & str_starts({{input_column}}, "\\d+"))
    df <- df |> anti_join(df_num, by = "addressr_id")
    if (nrow(df_num) != 0) {
      df_num <- df_num |>
        extract_remove_squish({{input_column}}, "street_number", "^\\d+")
      df <- bind_rows(df, df_num)
    }
    toc()

    # tidy up for output
    tic("tidy output")
    # add back P.O. Boxes
    if (nrow(df_box) != 0) {
      df <- bind_rows(df, df_box)
    }

    df <- df |>
      mutate({{ input_column }} := na_if({{ input_column }}, "")) |>
      mutate({{ input_column }} := str_replace_names({{ input_column }}, special_street_names$regex, special_street_names$output)) |>
      rename("street_name" = {{ input_column }}) |>
      unite({{ street_name }}, c("street_name", "street_suffix_2"), sep = " ", na.rm = TRUE) |>
      mutate({{ street_name }} := str_squish({{ street_name }}) |> na_if("")) |>
      arrange({{ original_row_id }}, {{ addressr_id }}) |>
      unite("clean_address", any_of(v_clean_address), sep = " ", remove = FALSE, na.rm = TRUE)
    toc()

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
