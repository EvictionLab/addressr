#' Check the address pattern
#'
#' @param pattern The string to be extracted and removed.
#'
#' @return A string to use in pattern matching.
check_pattern <- function(pattern) {

  pat <- pattern

  # list of available address abbreviations
  abbr_types <- unique(addr_abbr$type)
  # reference table: address_regex
  regex_tbl <- address_regex
  regex_types <- unique(regex_tbl$address_part)

  # both pre and post-directions use the "directions" table
  if (str_detect(pattern, "direction")) {
    pat <- str_collapse_bound(unique(c(directions$short, directions$long)))

    # pre-directions must be at the string start, but if at the start and end,
    # probably a street name
    if (pattern == "pre_direction") {
      pat <- paste0("^", pat, "(?!$)")
    }

    if (pattern == "pre_direction_db") {
      pat <- paste0("^", pat)
    }

    if (pattern == "post_direction") {
      pat <- paste0("(?<!^)", pat, "$")
    }

    if (pattern == "post_direction_db") {
      pat <- paste0(pat, "$")
    }

  }

  if (pattern %in% c("all_street_suffix")) {
    pat <- str_collapse_bound(unique(c(all_street_suffixes$short, all_street_suffixes$long)))
    pat <- paste0(pat, "$")
  }

  if (pattern == "unit_type") {
    pat <- str_collapse_bound(unique(c(unit_types$short, unit_types$long, "COTTAGE$")))
  }

  # IMPORTANT: unit_types will capture anything following the unit_type.
  # use pattern = "unit_type" for only the unit_type
  if (pattern == "unit") {
    pat <- str_collapse_bound(unique(c(unit_types$short, unit_types$long)))
    letter_unit <- str_collapse_bound(LETTERS[!LETTERS %in% c("N", "S", "E", "W")])

    pat <- paste0("(?<!^)\\W*(", pat, ".*$|\\b\\d+(\\W)?\\w$|", letter_unit, "((\\W)+?\\d)?$|\\d+$|\\bCOTTAGE$|#.*$)")
  }

  if (pattern == "unit_db") {
    pat <- str_collapse_bound(unique(c(unit_types$short, unit_types$long)))
    pat <- paste0(pat, "(.*)?$|#.*|\\d+$")
  }

  # if it's in the regex table, it's already ready to go
  if (pattern %in% regex_types) {
    addr_regx <- regex_tbl[regex_tbl$address_part == pattern, ]
    pat <- addr_regx$regex
  }

  pat

}

check_street_range <- function(.data, street_number_multi, street_number, addressr_id) {

  street_number_and <- sym("street_number_and")
  street_number_first <- sym("street_number_first")
  street_number_n <- sym("street_number_n")
  street_number_id <- sym("street_number_id")
  street_number_min <- sym("street_number_min")
  street_number_max <- sym("street_number_max")
  street_number_diff <- sym("street_number_diff")
  street_number_first_length <- sym("street_number_first_length")
  street_number_length <- sym("street_number_length")
  street_number_logic <- sym("street_number_logic")

  df_ranges <- .data |> filter(!is.na(street_number_multi))

  df <- .data |> anti_join(df_ranges, by = "addressr_id")

  if (nrow(df_ranges) != 0) {

    df_ranges <- df_ranges |>
      mutate(
        street_number_and = str_extract(street_number_multi, "AND|&|,|OR"),
        street_number_multi = str_replace_all(street_number_multi, "\\W|AND|OR", " ") |> str_squish(),
        street_number_first = str_extract(street_number_multi, "^\\d+\\b")
        ) |>
      separate_longer_delim(street_number_multi, delim = " ") |>
      extract_remove_squish({{ street_number_multi }}, building, "[A-Z]$") |>
      distinct() |>
      mutate(
        street_number_id = row_number(),
        street_number_first_length = str_length(street_number_first),
        street_number_length = str_length(street_number_multi),
        .by = "addressr_id")

    # Correct for street numbers ranges with inconsistent digits like "123 25 27"
    df_length <- df_ranges |> filter(street_number_length < street_number_first_length)

    df_ranges <- df_ranges |> anti_join(df_length, by = c("addressr_id", "street_number_id"))

    if (nrow(df_length) != 0) {

      df_length <- df_length |>
        mutate(
          {{street_number_multi}} := paste0(str_sub(street_number_first, end = (street_number_first_length - street_number_length)), street_number_multi),
        )

      df_ranges <- bind_rows(df_ranges, df_length) |> arrange(addressr_id, street_number_id)

    }

    df_ranges <- df_ranges |>
      mutate(street_number_n = n(),
             street_number_min = min(as.numeric(street_number_multi)),
             street_number_max = max(as.numeric(street_number_multi)),
             street_number_diff = street_number_max - street_number_min,
             street_number_logic = case_when(
               street_number_n == 1 ~ "street_number",
               street_number_n > 2 ~ "ready",
               street_number_n == 2 & (street_number_diff == 2 | !is.na(street_number_and)) ~ "ready",
               street_number_n == 2 & street_number_diff > 2 & street_number_diff <= 20 ~ "seq_along",
               .default = "error"
             ),
             .by = "addressr_id")

    # Repair 1: If there's only one distinct street_number, place it in the street_number column and finish.
    df_ranges_one <- df_ranges |> filter(street_number_logic == "street_number")

    df_ranges <- df_ranges |> anti_join(df_ranges_one, by = "addressr_id")

    if (nrow(df_ranges_one) != 0) {

      df_ranges_one <- df_ranges_one |>
        extract_remove_squish(street_number_multi, street_number, "\\d+") |>
        select(-c(street_number_and, street_number_first, street_number_n, street_number_id, street_number_min, street_number_max, street_number_diff, street_number_first_length, street_number_length, street_number_logic))

      df <- bind_rows(df, df_ranges_one)

    }

    # Repair 2: If there's 3+ numbers of the same length or two numbers next to each other, place in the street_number column and finish.
    df_ranges_two <- df_ranges |> filter(street_number_logic == "ready")

    df_ranges <- df_ranges |> anti_join(df_ranges_two, by = "addressr_id")

    if (nrow(df_ranges_two) != 0) {

      df_ranges_two <- df_ranges_two |>
        extract_remove_squish(street_number_multi, street_number, "\\d+") |>
        unite({{ addressr_id }}, c("addressr_id", "street_number_id"), sep = "-N", remove = TRUE) |>
        select(-c(street_number_and, street_number_first, street_number_n, street_number_min, street_number_max, street_number_diff, street_number_first_length, street_number_length, street_number_logic))

      df <- bind_rows(df, df_ranges_two)

    }

    # Repair 3: If there's two numbers and they're between 2 and 20 digits apart, find each number in the sequence.
    df_ranges_three <- df_ranges |> filter(street_number_logic == "seq_along")

    df_ranges <- df_ranges |> anti_join(df_ranges_three, by = "addressr_id")

    if (nrow(df_ranges_three) != 0) {

      df_ranges_three <- df_ranges_three |>
        filter(street_number_id == 1) |>
        mutate({{ street_number }} := map2(street_number_min, street_number_max, ~ seq(.x, .y, by = 2))) |>
        unnest_longer({{ street_number }}) |>
        mutate({{ street_number }} := as.character({{ street_number }}),
               {{ street_number_id }} := row_number(),
               {{ street_number_multi }} := NA_character_,
               .by = "addressr_id") |>
        unite({{ addressr_id }}, c("addressr_id", "street_number_id"), sep = "-N", remove = TRUE) |>
        select(-c(street_number_and, street_number_first, street_number_n, street_number_min, street_number_max, street_number_diff, street_number_first_length, street_number_length, street_number_logic))

      df <- bind_rows(df, df_ranges_three)

    }

    # Catch all
    df_ranges_four <- df_ranges |> filter(!street_number_logic %in% c("street_number", "ready", "seq_along"))

    df_ranges <- df_ranges |> anti_join(df_ranges_four, by = "addressr_id")

    if (nrow(df_ranges_four) != 0) {

      df_ranges_four <- df_ranges_four |>
        select(-c(street_number_and, street_number_first, street_number_n, street_number_id, street_number_min, street_number_max, street_number_diff, street_number_first_length, street_number_length, street_number_logic))

      df <- bind_rows(df, df_ranges_four)

    }

    df_ranges <- df_ranges |>
      select(-c(street_number_and, street_number_first, street_number_n, street_number_id, street_number_min, street_number_max, street_number_diff, street_number_first_length, street_number_length, street_number_logic))

    df <- bind_rows(df, df_ranges)

  }

  df

}

#' Check the unit
#' @inheritParams clean_address
#' @param unit The unit number or letter
#' @param unit_type The type of unit (apt, #, etc)
#' @param street_number The street number (<123> N Main St)
#' @param street_suffix The street suffix (123 N Main <St>)
#' @param building The building number or letter
#' @param addressr_id Unique row_id
check_unit <- function(.data, unit, unit_type, street_number, street_suffix, building, addressr_id) {

  df_unit <- .data |> filter(!is.na({{ unit }}))

  df <- .data |> anti_join(df_unit, by = "addressr_id")

  if (nrow(df_unit) != 0) {

    unit_number <- sym("unit_number")
    unit_text <- sym("unit_text")
    extra_unit <- sym("extra_unit")

    #' 1. remove all non-word characters
    #' 2. extract numerics, compare with street number
    #' 4. if a unit contains text that is more than 2 characters, put it in "extra_unit" to fix later
    #' 3. extract words, compare with street suffix & building
    #' 5. combine unit_number and unit_text
    df_unit <- df_unit |>
      extract_remove_squish({{ unit }}, "unit_number", "\\d+") |>
      extract_remove_squish({{ unit }}, "extra_unit", "([A-Z]{2,}\\W?)+") |>
      extract_remove_squish({{ unit }}, "unit_text", "[A-Z]{1}") |>
      mutate(
        {{ unit_number }} := str_remove({{ unit_number }}, "^0+"),
        {{ unit_number }} := if_else(((!is.na({{ street_number }}) & {{ unit_number }} == {{ street_number }}) | unit_number == ""), NA_character_, {{ unit_number }}),
        {{ unit_text }} := if_else(((!is.na({{ street_suffix }}) & {{ unit_text }} == {{ street_suffix }}) | (!is.na({{ building }}) & {{ unit_text }} == {{ building }})), NA_character_, {{ unit_text }}),
        {{ extra_unit }} := if_else((!is.na({{ unit_type }}) & {{ unit_text }} == {{ unit_type }}), NA_character_, extra_unit)
      ) |>
      unite(extra_unit, c(unit, extra_unit), sep = "", na.rm = TRUE) |>
      unite(unit, c(unit_number, unit_text), sep = "", na.rm = TRUE) |>
      mutate(extra_unit := na_if(extra_unit, ""))

    df <- bind_rows(df, df_unit)

  }

  df

}

check_building <- function(.data, street_number, street_number_multi, building, addressr_id) {

  # if the street number is missing & there's a number in the building, extract it.
  df_bldg <- .data |>
    filter(is.na({{ street_number }}) & is.na({{ street_number_multi }}) & !is.na({{ building }}) & str_detect({{ building }}, "\\d"))

  df <- .data |> anti_join(df_bldg, by = "addressr_id")

  if (nrow(df_bldg) != 0) {

    df_bldg <- df_bldg |>
      extract_remove_squish({{ building }}, "street_number", "\\d+") |>
      mutate({{ street_number }} := str_remove({{ street_number }}, "^0+"))

    df <- bind_rows(df, df_bldg)

  }

  df

}
