#' Check the address pattern
#'
#' @param pattern The string to be extracted and removed.
#'
#' @return A string to use in pattern matching.
#'
#' @export
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


  # use pattern = "unit_type" for only the unit_type
  if (pattern %in% c("unit", "unit_no_anchor", "unit_stricter")) {
    pat <- str_flatten(unique(c(unit_types$short, unit_types$long)), collapse = "|")
    letter_unit <- str_flatten(LETTERS[!LETTERS %in% c("N", "S", "E", "W")], collapse = "|")

    # unit rules:
    unit_pat <- c(
      # 1. unit_type + anything
      str_glue("({pat})(\\W\\w+|\\d+|$)(-\\w+| \\w)?"),
      # 2. digits + non-word character? + letter (not NSEW)
      str_glue("\\d+\\W?({letter_unit})"),
      # 3. not HIGHWAY + letter (not NSEW) + non-word character ? + number ?
      str_glue("(?<!HIGHWAY )({letter_unit})(\\W*\\d)?"),
      # 4. letter + number
      str_glue("({letter_unit})\\d+"),
      # 5. numbers + - ? + numbers
      "(\\d+(\\s?-\\s?))?\\d+",
      # 6. COTTAGE
      "COTTAGE",
      # 7. boundary or number + L or U + F or R
      "\\d?[LU][FR]",
      # 8. ordinal number + floor
      "\\d+[RSNT][DTH] FL(OOR)?"
    )

    if (pattern == "unit") {

      # 1. unit_type + anything
      # unit_pat[1] <- str_glue("({pat}).*")
      # 2. digits + non-word character? + word character
      unit_pat[2] <- "\\d+\\W?\\w"
      # not at start, anchor to end
      pat <- paste0("(?<!^)\\W*\\b(", str_flatten(unit_pat, collapse = "|"), ")$|#.*$")

    } else if (pattern == "unit_no_anchor") {

      pat <- str_collapse_bound(unit_pat)

    } else if (pattern == "unit_stricter") {

      # 3. not HIGHWAY + letter (not NSEW) + non-word character ? + number
      unit_pat[3] <- str_glue("(?<!HIGHWAY )({letter_unit})\\W*\\d")
      # no stand alone numbers or COTTAGE
      unit_pat <- unit_pat[-c(5, 6)]
      pat <- str_collapse_bound(unit_pat)

    }

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

check_street_range <- function(.data, street_number_multi, street_number, addressr_id, building) {

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
  building_2 <- sym("building_2")

  df_ranges <- .data |> filter(!is.na(street_number_multi))

  df <- .data |> anti_join(df_ranges, by = "addressr_id")

  if (nrow(df_ranges) != 0) {

    df_ranges <- df_ranges |>
      mutate(
        street_number_and = str_extract(street_number_multi, "AND|&|,|OR"),
        street_number_multi = str_replace_all(street_number_multi, "\\W|AND|OR", " ") |> str_squish(),
        street_number_first = str_extract(street_number_multi, "^\\d+")
        ) |>
      separate_longer_delim(street_number_multi, delim = " ") |>
      extract_remove_squish({{ street_number_multi }}, building_2, "[A-Z]$") |>
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
               street_number_min == street_number_max ~ "ready",
               street_number_n == 2 & (street_number_diff == 2 | !is.na(street_number_and)) ~ "ready",
               street_number_n == 2 & street_number_diff > 2 & street_number_diff > 20 ~ "ready",
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
        unite({{ addressr_id }}, any_of(c("addressr_id", "street_number_id")), sep = "-N", remove = TRUE) |>
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
        unite({{ addressr_id }}, any_of(c("addressr_id", "street_number_id")), sep = "-N", remove = TRUE) |>
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

  df <- df |>
    unite({{ building }}, any_of(c("building", "building_2")), sep = " ", na.rm = TRUE) |>
    mutate(building := na_if(building, ""))

}

#' Check the unit
#' @inheritParams clean_address
#' @param input_column address column
#' @param unit The unit number or letter
#' @param unit_type The type of unit (apt, #, etc)
#' @param street_number The street number (<123> N Main St)
#' @param street_suffix The street suffix (123 N Main <St>)
#' @param special_unit Special units
#' @param special_unit_2 Extra special units
#' @param building The building number or letter
#' @param addressr_id Unique row_id
check_unit <- function(.data, input_column, unit, unit_type, special_unit, special_unit_2, street_number, street_suffix, building, addressr_id) {

  extra_unit <- sym("extra_unit")
  df_unit <- .data |> filter(!is.na({{ unit }}))

  df <- .data |> anti_join(df_unit, by = "addressr_id")

  if (nrow(df_unit) != 0) {

    unit_number <- sym("unit_number")
    unit_text <- sym("unit_text")

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
        {{ extra_unit }} := if_else((!is.na({{ unit_type }}) & {{ unit_text }} == {{ unit_type }}), NA_character_, {{ extra_unit }}, missing = NA_character_)
      ) |>
      unite({{ extra_unit }}, any_of(c("unit", "extra_unit")), sep = "", na.rm = TRUE) |>
      unite({{ unit }}, any_of(c("unit_number", "unit_text")), sep = "", na.rm = TRUE) |>
      mutate({{ extra_unit }} := na_if({{ extra_unit }}, ""))

    df <- bind_rows(df, df_unit)

  }

  df |>
    extract_remove_squish({{ input_column }}, "extra_unit_2", "unit_stricter") |>
    unite({{ extra_unit }}, any_of(c("extra_unit", "extra_unit_2")), sep = " ", na.rm = TRUE) |>
    unite({{ unit }}, any_of(c("unit", "special_unit", "special_unit_2")), sep = " ", na.rm = TRUE) |>
    mutate({{ unit }} := switch_abbreviation({{ unit }}, "special_units", "short-to-long"),
           {{ unit }} := str_squish({{ unit }}) |> na_if(""),
           {{ extra_unit }} := na_if({{ extra_unit }}, "")
    )

}

check_missing_number <- function(.data, input_column, street_number, street_number_multi, street_number_coords, building, addressr_id) {

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

  # if the street number is missing & the input column starts with a number, extract it
  df_num <- df |> filter(is.na({{ street_number }}) & str_starts({{ input_column }}, "\\d+"))
  df <- df |> anti_join(df_num, by = "addressr_id")

  if (nrow(df_num) != 0) {

    df_num <- df_num |>
      extract_remove_squish({{ input_column }}, "street_number", "^\\d+")
    df <- bind_rows(df, df_num)
  }

  df

}

check_highways <- function(.data, input_column) {

  regex_hwy <- paste0("(", str_collapse_bound(unique(highways$short, highways$long)), " )+(?!(ST|AV|CV|LN|CI|PL|BV|WY))(\\d{2,3}|[A-Z]{1,2})( (AND|&) \\d{2,3})?\\b")

  df_hwy <- .data |> filter(str_detect({{ input_column }}, regex_hwy))
  df <- .data |> anti_join(df_hwy, by = "addressr_id")

  if (nrow(df_hwy) != 0) {
    highway <- sym("highway")
    highway_num <- sym("highway_num")

    df_hwy <- df_hwy |>
      mutate({{ highway }} := str_extract({{ input_column }}, regex_hwy)) |>
      extract_remove_squish({{ highway }}, "highway_num", "(\\d{2,3}|[A-Z]{1,2})( (AND|&) \\d{2,3})?$") |>
      mutate(
        {{ highway }} := case_when(
          str_detect({{ highway }}, "\\b(COUNTY|CNTY|CTY|CO|CTHY?)\\b") ~ "COUNTY HIGHWAY",
          str_detect({{ highway }}, "\\b(STATE|STH?)\\b") ~ "STATE HIGHWAY",
          .default = "HIGHWAY"
        ),
        {{ highway_num }} := str_replace_all({{ highway_num }}, "\\d{2,3}", replace_number),
        {{ highway_num }} := str_replace_all({{ highway_num }}, "&", "AND")
      ) |>
      unite({{ highway }}, any_of(c("highway", "highway_num")), sep = " ", na.rm = TRUE) |>
      mutate({{ input_column }} := str_replace_all({{ input_column }}, regex_hwy, {{ highway }})) |>
      select(-highway)
    df <- bind_rows(df, df_hwy)
  }

  df
}

check_fractional_names <- function(.data, input_column) {

  regex_frac <- check_pattern("street_name_fraction")

  df_frac <- .data |> filter(str_detect({{ input_column }}, regex_frac))
  df <- .data |> anti_join(df_frac, by = "addressr_id")

  if (nrow(df_frac) != 0) {
    df_frac <- df_frac |>
      mutate({{ input_column }} := str_replace_all({{ input_column }}, regex_frac, replace_fraction))
    df <- bind_rows(df, df_frac)
  }

  df
}

check_multi_address <- function(.data, input_column, addressr_id, all_suffix_regex) {

  # current logic to delim: (etc + street suffix) + [punctuation, and, or space] + (numbers + etc + street suffix)
  longer_regex <- paste0("(?<=", all_suffix_regex, ")\\s*([:punct:]| AND |\\s)\\s*(?=\\d+\\b.+", all_suffix_regex, ")")
  # second logic to delim: (numbers + word 4-20 letters) + [punctuation, and, or space] + (numbers + same word)
  longer_regex_2 <- "(?<=\\d (([NSEW] )?\\w{4,20}))\\s*([:punct:]| AND |\\s)\\s*(?=\\d+(\\W\\d+)? \\1)"

  df_multi <- .data |> filter(str_detect({{ input_column }}, longer_regex) | str_detect({{ input_column }}, longer_regex_2))
  df <- .data |> anti_join(df_multi, by = "addressr_id")

  if (nrow(df_multi) != 0) {
    addressr_addr_id <- sym("addressr_addr_id")

    df_multi <- df_multi |>
      separate_longer_delim({{ input_column }}, delim = stringr::regex(longer_regex)) |>
      separate_longer_delim({{ input_column }}, delim = stringr::regex(longer_regex_2)) |>
      distinct() |>
      mutate({{ addressr_addr_id }} := row_number(), .by = "addressr_id") |>
      unite({{ addressr_id }}, any_of(c("addressr_id", "addressr_addr_id")), sep = "-A", remove = FALSE) |>
      select(-addressr_addr_id)

    df <- bind_rows(df, df_multi)

  }

  df
}
