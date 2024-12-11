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

  # address parts that appear at the end
  if (pattern %in% c("special_unit")) {
    pat <- str_collapse_bound(unique(c(special_units$short, special_units$long)))
    pat <- paste0(pat, "$")
  }

  if (pattern == "unit_type") {
    pat <- str_collapse_bound(unique(c(unit_types$short, unit_types$long, "COTTAGE$")))
  }

  # IMPORTANT: unit captures anything following the unit. use unit_type for only the unit_type
  if (pattern == "unit") {
    pat <- str_collapse_bound(unique(c(unit_types$short, unit_types$long)))
    letter_unit <- str_collapse_bound(LETTERS[!LETTERS %in% c("N", "S", "E", "W")])

    pat <- paste0(pat, "(.*)?$|\\b\\d+(\\W)?[:alpha:]$|(?<!^)\\b[:alpha:](\\W)?(\\d+)?$|(?<!^)\\d+$|", letter_unit, "$|\\bCOTTAGE$")
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

# If a street number range contains the same number twice, change it to a singular street number
check_street_range <- function(.data, street_number_multi, street_number, addressr_id) {

  street_number_id <- sym("street_number_id")

  df_ranges <- .data |> filter(!is.na(street_number_multi))

  df <- .data |> anti_join(df_ranges, by = "addressr_id")

  if (nrow(df_ranges) != 0) {

    df_ranges <- df_ranges |>
      mutate(street_number_multi = str_replace_all(street_number_multi, "\\W|AND", " ") |> str_squish()) |>
      separate_longer_delim(street_number_multi, delim = " ") |>
      distinct() |>
      mutate(street_number_n = n(),
             street_number_id = row_number(),
             street_number_min = min(as.numeric(street_number_multi)),
             street_number_max = max(as.numeric(street_number_multi)),
             street_number_diff = street_number_max - street_number_min,
             street_number_max_length = str_length(street_number_max),
             street_number_min_length = str_length(street_number_min),
             street_number_logic = case_when(
               street_number_n == 1 ~ "street_number",
               street_number_n > 2 ~ "ready",
               street_number_n == 2 & street_number_diff == 2 ~ "ready",
               street_number_n == 2 & street_number_diff > 2 & street_number_diff <= 20 ~ "seq_along",
               street_number_n == 2 & street_number_diff > 20 ~ "check_length",
               .default = "error"
             ),
             .by = "addressr_id")

    df_ranges_one <- df_ranges |> filter(street_number_logic == "street_number")

    df_ranges <- df_ranges |> anti_join(df_ranges_one, by = "addressr_id")

    if (nrow(df_ranges_one) != 0) {

      df_ranges_one <- df_ranges_one |>
        extract_remove_squish(street_number_multi, street_number, "\\d+") |>
        select(-c(street_number_n, street_number_id, street_number_min, street_number_max, street_number_diff, street_number_max_length, street_number_min_length, street_number_logic))

      df <- bind_rows(df, df_ranges_one)

    }

    df_ranges_two <- df_ranges |> filter(street_number_logic == "ready")

    df_ranges <- df_ranges |> anti_join(df_ranges_two, by = "addressr_id")

    if (nrow(df_ranges_two) != 0) {

      df_ranges_two <- df_ranges_two |>
        extract_remove_squish(street_number_multi, street_number, "\\d+") |>
        unite({{ addressr_id }}, c("addressr_id", "street_number_id"), sep = "-N", remove = TRUE) |>
        select(-c(street_number_n, street_number_min, street_number_max, street_number_diff, street_number_max_length, street_number_min_length, street_number_logic))

      df <- bind_rows(df, df_ranges_two)

    }

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
        select(-c(street_number_n, street_number_min, street_number_max, street_number_diff, street_number_max_length, street_number_min_length, street_number_logic))

      df <- bind_rows(df, df_ranges_three)

    }

    df_ranges_four <- df_ranges |> filter(!street_number_logic %in% c("street_number", "ready", "seq_along"))

    df_ranges <- df_ranges |> anti_join(df_ranges_four, by = "addressr_id")

    if (nrow(df_ranges_four) != 0) {

      df_ranges_four <- df_ranges_four |>
        select(-c(street_number_n, street_number_id, street_number_min, street_number_max, street_number_diff, street_number_max_length, street_number_min_length, street_number_logic))

      df <- bind_rows(df, df_ranges_four)

    }

    df_ranges <- df_ranges |>
      select(-c(street_number_n, street_number_id, street_number_min, street_number_max, street_number_diff, street_number_max_length, street_number_min_length, street_number_logic))

    df <- bind_rows(df, df_ranges)

  }

  df

}

check_unit <- function(.data, unit, street_number, street_suffix, addressr_id) {

  df_unit <- .data |> filter(!is.na({{ unit }}))

  df <- .data |> anti_join(df_unit, by = "addressr_id")

  if (nrow(df_unit) != 0) {

    abbr_street_suffix <- unique(c(all_street_suffixes$short, all_street_suffixes$long))

    df_unit <- df_unit |>
      mutate(
        # remove all non-word characters
        {{ unit }} := str_remove_all(unit, "\\W"),
        # check unit for street suffix
        {{ street_suffix }} := if_else(is.na({{ street_suffix }}) & ({{ unit }} %in% abbr_street_suffix), {{ unit }}, {{ street_suffix }}),
        # check if unit duplicates street number
        {{ unit }} := if_else(!is.na({{ street_number }}) & ({{ unit }} == {{ street_number }} | {{ unit }} == {{ street_suffix }}), NA_character_, {{ unit }})
      )

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

    df_bldg <- df_bldg |> extract_remove_squish({{ building }}, "street_number", "\\d+")

    df <- bind_rows(df, df_bldg)

  }

  df

}
