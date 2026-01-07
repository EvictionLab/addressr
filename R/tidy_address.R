#' Tidy an address column
#'
#' @param .data data
#' @param address_column address column
#' @param output Address columns to output. Defaults to "everything", which outputs all original columns in the dataset, row id columns, all address components, and the "clean_address".
#' @param separate_multi_address separate multi address
#'
#' @returns An object of the same type as .data
#' @export
tidy_address <- function(.data, address_column, output = "everything", separate_multi_address = FALSE) {

  tic("total tidy time")

  input_id <- sym("input_id")
  addressr_id <- sym("addressr_id")
  addressr_methods <- sym("addressr_methods")
  addressr_errors <- sym("addressr_errors")
  addressr_extra <- sym("addressr_extra")
  temp_address <- sym("temp_address")

  clean_address <- sym("clean_address")
  street_number <- sym("street_number")
  pre_direction <- sym("pre_direction")
  street_name <- sym("street_name")
  post_direction <- sym("post_direction")
  street_suffix <- sym("street_suffix")
  unit <- sym("unit")
  city_state_zip <- sym("city_state_zip")

  # TODO: archive
  extra_front <- sym("extra_front")
  extra_name <- sym("extra_name")
  city_state <- sym("city_state")
  method_address <- sym("method_address")

  # ---------------------------------------------------------- prep address ----
  tic("preserve input")
  df <- .data |>
    mutate(
      {{ input_id }} := row_number(),
      {{ addressr_id }} := as.character({{ input_id }}),
      {{ temp_address }} := str_to_upper({{ address_column }}),
      {{ addressr_errors }} := NA_character_
      )
  toc()

  # ------------------------------------------------------------ na address ----

  df <- df |>
    tidy_part(temp_address, "change_na_") |>
    mutate({{ temp_address }} := str_squish({{ temp_address }}) |> na_if(""))

  df_na <- df |>
    filter(is.na({{ temp_address }})) |>
    mutate({{ addressr_errors }} := "na_address")

  # ---------------------------------------------------------- prep address ----
  df <- df |>
    anti_join(df_na, by = "input_id") |>
    mutate({{ temp_address }} := prepare_address({{ temp_address }}))

  # ------------------------------------------------ non-address components ----
  ## ----------------------------------------------------- city, state, zip ----
  df <- df |>
    tidy_part(temp_address, "state") |>
    tidy_part(temp_address, "city_state") |>
    mutate({{ temp_address }} := str_replace_all({{ temp_address }}, "(?<!\\d)\\W*,\\W*(?!\\d)", " "))
    # remove commas, except when separating numbers

  ## --------------------------------------------------------------- po box ----
  df <- tidy_part(df, temp_address, "po_box")

  # --------------------------------------------------------- multi address ----
  if (separate_multi_address) {

    check_multi_address <- function(.data, temp_address, addressr_id) {

      r_dir <- abbr_regex("direction", bound = TRUE)
      r_all_suffix <- abbr_regex("all_suffix", bound = TRUE)

      # current logic to delim: (etc + street suffix) + [punctuation, and, or space] + (numbers + etc + street suffix)
      r_longer <- paste0(str_glue("(?<={r_all_suffix})\\s*([:punct:]| AND |\\s)\\s*(?=\\d+\\b.+{r_all_suffix})"))
      # second logic to delim: (numbers + word 4-20 letters) + [punctuation, and, or space] + (numbers + same word)
      r_longer_2 <- paste0(str_glue("(?<=\\d (({r_dir} )?\\w{{4,20}}))\\s*([:punct:]| AND |\\s)\\s*(?=\\d+(\\W\\d+)? \\1)"))

      df_multi <- .data |> filter(str_detect({{ temp_address }}, r_longer) | str_detect({{ temp_address }}, r_longer_2))
      df <- .data |> anti_join(df_multi, by = "addressr_id")

      if (nrow(df_multi) != 0) {
        addressr_addr_id <- sym("addressr_addr_id")

        df_multi <- df_multi |>
          separate_longer_delim({{ temp_address }}, delim = stringr::regex(r_longer)) |>
          separate_longer_delim({{ temp_address }}, delim = stringr::regex(r_longer_2)) |>
          distinct() |>
          mutate({{ addressr_addr_id }} := row_number(), .by = "addressr_id") |>
          unite({{ addressr_id }}, any_of(c("addressr_id", "addressr_addr_id")), sep = "-A", remove = FALSE) |>
          select(-addressr_addr_id)

        df <- bind_rows(df, df_multi)

      }

      df
    }

    tic("separate multiple addresses")
    df <- df |> check_multi_address({{ temp_address }}, {{ addressr_id }})
    toc()

  }

  # ------------------------------------------------------- extract address ----
  # replace some street names (mostly numerical)
  df <- df |>
    tidy_part(temp_address, "street_name") |>
    tidy_part(temp_address, "change_precheck") |>
    tidy_part(temp_address, "extra_front") |>
    tidy_part(temp_address, "street_number") |>
    tidy_part(temp_address, "temp_back") |>
    tidy_part(temp_back, "temp_suffix") |>
    tidy_part(temp_suffix, "suffix") |>
    tidy_part(temp_back, "unit") |>
    tidy_part(temp_back, "post_direction_temp")

  tic("clean up suffix, pre-direction, street name")
  all_suffix <- get_abbr("all_suffix")
  df <- df |>
  mutate(temp_parts := str_extract_all(temp_back, "<[a-z0-9_]+>"),
         temp_back := str_remove_all(temp_back, "<[a-z0-9_]+>") |> str_remove_all("[^A-Z0-9 ]") |> str_squish() |> na_if("")) |>
  rename("extra_back" = "temp_back") |>
  unnest_wider(temp_parts, names_sep = "tmp") |>
  unite(temp_parts, any_of(starts_with("temp_parts")), na.rm = TRUE, sep = "") |>
  extract_replace(temp_suffix, "temp_street_name", "^\\W*(ST|LA|DR|PT)\\W+[A-Z]+\\b") |>
  mutate(
    temp_parts := na_if(temp_parts, ""),
    across(c(suffix, temp_suffix), ~ switch_abbr(., "all_suffix"))
  ) |>
  mutate(
    temp_suffix := case_when(str_detect(temp_suffix, suffix) ~ str_remove(temp_suffix, suffix), .default = temp_suffix) |> str_squish(),
    temp_suffix := case_when(!is.na(temp_street_name) ~ str_replace(temp_suffix, "<temp_street_name>", temp_street_name), .default = temp_suffix),
    temp_address := case_when(!is.na(temp_parts) ~ str_replace(temp_address, "<temp_back_\\d>", temp_parts), .default = temp_address),
    temp_address := case_when(!is.na(temp_suffix) ~ str_replace(temp_address, "<temp_suffix>", temp_suffix), .default = temp_address)
    ) |>
  select(-any_of(c("temp_parts", "temp_suffix", "temp_street_name")))

  # 10. Pre-direction
  df <- df |>
    tidy_part(temp_address, "pre_direction") |>
    tidy_part(temp_address, "post_direction_input") |>
    unite("post_direction", any_of(starts_with("post_direction")), na.rm = TRUE, sep = "; ") |>
    mutate(
      post_direction := str_squish(post_direction) |> na_if(""),
      temp_address := str_replace_all(temp_address, c("([a-z]>)\\W+(<[a-z])" = "\\1\\2", "([a-z]>)[\\s,]+([A-Z])" = "\\1\\2", "([A-Z])\\s(<[a-z])" = "\\1\\2"))
    )

  toc()

  tic("standardize")
  df <- df |>
    tidy_part(temp_address, "street_name") |>
    tidy_part(street_name, "change_name")

  df_replace <- df |>
    filter(!is.na(street_name)) |>
    mutate(street_name := case_when(
      method_street_name == "street_name_ordinal" ~ str_replace_all(street_name, "\\d{1,3}\\s?[A-Z]*", replace_ordinals),
      method_street_name == "street_name_fraction" ~ str_replace_all(street_name, "^\\d+ [A-Z0-9]+[ /][A-Z0-9]+", replace_fraction),
      method_street_name == "street_name_fraction_ordinal" ~ str_replace_all(street_name, "^\\d+ [A-Z0-9]+[ /][A-Z0-9]+", replace_fraction) |> str_replace_all("\\d{1,3}[A-Z]*", replace_ordinals),
      method_street_name == "street_name_mile" ~ str_replace_all(street_name, "\\d+", replace_number),
      str_detect(method_street_name, "highway") ~ str_replace_all(street_name, c("[:punct:]|[:symbol:]" = " ", "^.*?(?=(STATE|COUNTY|US)? ?HIGHWAY)" = "")) |> str_replace_all("\\d{1,3}", replace_number),
      .default = street_name
    ),
    street_name := str_replace_all(street_name, "\\b(\\d{1,3}[SNRT][TDH]\\b)", replace_ordinals),
    street_name := str_remove_all(street_name, "^\\W+|\\W+$"),
    across(ends_with("direction"), ~ switch_abbr(., "direction") |> switch_abbr("direction", "long-to-short"))
    )

  df <- df |> anti_join(df_replace, by = "addressr_id") |> bind_rows(df_replace)

  df <- df |>
    mutate(
      {{ temp_address }} := str_replace_all(temp_address, c("([a-z0-9]>)\\W+(<[a-z])" = "\\1\\2", "([a-z0-9]>)\\s+([^\\s])" = "\\1\\2", "([^\\s])\\s+(<[a-z])" = "\\1\\2")),
      {{ extra_name }} := str_remove_all(temp_address, "<[a-z0-9_]+>") |> str_squish() |> na_if(""),
      across(any_of(c("street_name", "street_number", "suffix", "unit", "pre_direction", "post_direction", "extra_back", "extra_front", "extra_name")), ~ str_squish(.) |> na_if(""))
    )
  toc()

  df <- tidy_part(df, street_name, "warn_name")

  r_unit_types <- abbr_regex("unit_type", bound = FALSE)

  df <- df |>
    extract_remove_squish(unit, unit_type, r_unit_types) |>
    extract_remove_squish(street_number, building, "(?<=\\d )[A-Z]$") |>
    unite(unit, any_of(c("unit", "building")), na.rm = TRUE, sep = " ") |>
    mutate(
      unit = str_squish(unit) |> na_if(""),
      unit_type = switch_abbr(unit_type, "unit_type"),
      unit = switch_abbr(unit, "unit_location"),
      unit = str_replace_all(unit, c(
        "\\b([A-Z])\\W*(\\d+)\\b" = "\\2 \\1",
        "\\b(\\d+)\\W*([A-Z])\\b" = "\\1 \\2"
        )),
      street_number = str_remove(street_number, "^0+"))

  # Return rows with null address to table
  if (nrow(df_na) != 0) df <- bind_rows(df, df_na)

  df <- df |> arrange(input_id) |> select(-c(any_of(starts_with("method")), input_id))

  df_names <- names(.data)
  v_clean_address <- c("po_box", "street_number", "pre_direction", "street_name", "suffix", "post_direction")

  df <- df |>
    unite("clean_address", any_of(v_clean_address), sep = " ", remove = FALSE, na.rm = TRUE) |>
    mutate({{ clean_address }} := str_squish({{ clean_address }}) |> na_if(""))

  toc()

  # ---------------------------------------------------------------- output ----
  full_output <- str_flatten_comma(output)

  if (nrow(.data) != nrow(df)) df_names <- c(df_names, "addressr_id")

  if (length(output) == 1 & str_detect(full_output, "everything")) {

    df_names <- c(df_names, "addressr_errors", "clean_address", v_clean_address, "unit_type", "unit", "extra_front", "extra_back", "extra_name", "temp_address", "city_state")

  } else {

    if ("short_address" %in% output) {

      df <- df |>
        unite("short_address", any_of(c("street_number", "street_name", "po_box")), na.rm = TRUE, sep = " ", remove = FALSE) |>
        mutate(across(c("short_address"), ~ str_squish(.) |> na_if("")))

    }

    if ("extra" %in% output) {

      df <- df |>
        unite("extra", any_of(starts_with("extra_")), sep = " ", remove = FALSE, na.rm = TRUE) |>
        mutate(across(c("extra"), ~ str_squish(.) |> na_if("")))

    }

    df_names <- c(df_names, output)

  }

  df |> select(any_of(df_names))

}

prepare_address <- function(string) {
  x <- str_replace_all(string, c(
      # fix bad ampersand
      "&AMP;" = "&",
      # dedupe non-word characters ("ST..JUDE" = "ST.JUDE")
      "(\\W)\\1+" = "\\1",
      # replace period surrounded by letters/numbers with space ("ST.JUDE" = "ST JUDE")
      "([^\\s])\\.([^\\s])" = "\\1 \\2",
      # remove spaces around hyphens
      "([A-Z0-9])\\s*-\\s*([A-Z0-9])" = "\\1-\\2",
      # add space between number and letters
      "^(\\d+)([A-Z]+)" = "\\1 \\2",
      # remove space between first direction and number
      "^([NSEW]) (\\d+)\\b" = "\\1\\2",
      # remove pound sign before ordinal
      "#(\\d+[RNTS][DHT])" = "\\1",
      # add dash to interstate
      "(?<=^\\d (?:(?:N|S|E|W) )?)\\b(I|US)(\\d+)\\b" = "\\1-\\2",
      # replace word + & + word with AND
      "([A-Z])\\s*&\\s*([A-Z])" = "\\1 AND \\2",
      # replace random / with space
      "(?<!\\d)/(?!\\d)" = " ",
      "\\W+$" = ""
    )) |>
    str_remove_all(c("\\.|'|;|>|<|\\$|\\(|\\)")) |>
    str_remove_all(c('"')) |>
    str_squish()
}

#' @export
get_abbr <- function(address_part) {
  abbr_address[abbr_address$type == address_part, ] |> select(-type)
}

#' @export
get_dd <- function(address_part, dd_col = "regex") {
  dd_out <- dd_address |> filter(str_starts(label, address_part)) |> pull(dd_col)

  if (dd_col == "names") dd_out <- dd_out[[1]]

  dd_out

}

abbr_regex <- function(address_part, bound = FALSE) {

  the_abbr <- get_abbr(address_part)
  the_abbr <- the_abbr |>
    pivot_longer(all_of(c("short", "long")), names_to = NULL) |>
    distinct() |>
    mutate(n = str_remove(value, "\\{.*\\}") |> str_length()) |>
    arrange(desc(n)) |>
    pull(value)
  if (bound) {
    the_abbr |> str_collapse_bound()
  } else the_abbr |> str_flatten("|")

}

extract_replace <- function(.data, temp_address, output_column, pattern) {

  .data |>
    mutate(
      {{ output_column }} := str_extract({{ temp_address }}, pattern) |> str_squish(),
      {{ temp_address }} := str_replace({{ temp_address }}, pattern, str_glue("<{output_column}>")) |> str_squish()
      )

}

#' @export
tidy_part <- function(.data, temp_address, address_part) {

  dd_part <- dd_address |> filter(str_starts(label, address_part))
  dd_label <- dd_part$label
  dd_regex <- dd_part$regex

  address_method <- str_glue("method_{address_part}")
  address_method <- ensym(address_method)
  address_name <- ensym(address_part)
  addressr_errors <- sym("addressr_errors")

  df <- .data
  n_df <- names(df)

  if (!address_part %in% n_df & !str_starts(address_part, "(warn|change)_")) df <- df |> mutate({{ address_name }} := NA_character_)


  df <- purrr::reduce2(
    .x = dd_regex,
    .y = dd_label,
    .init = df,
    .f = function(df, regex, label) {
      tidy_regex(df, regex, label, {{ temp_address }}, {{ address_name }}, {{ address_method }}, {{ addressr_errors }})
    }
  )


}


#' @export
tidy_regex <- function(.data, regex, label, temp_address, address_name, address_method, addressr_errors, ...) {

  df_out <- .data
  df_part <- df_out |>
    filter(str_detect({{ temp_address }}, regex))

  n_part <- nrow(df_part)

  if (n_part != 0) {

    tic(str_glue("Found {label}: {n_part} matches"))

    the_part <- dd_address[dd_address$label == label, ]
    dd_standard <- the_part |> pull(standard)
    dd_match <- the_part |> pull(match)
    address_components <- c("street_(number|name)|(pre|post)_direction|unit|suffix|temp")


    if (str_starts(label, "warn")) {

      df_part <- df_part |>
        mutate(
          {{ addressr_errors }} := case_when(!is.na({{addressr_errors}}) ~ paste0({{addressr_errors}}, ", ", label), .default = label)
        )

    } else if (str_starts(label, "change")) {

      df_part <- df_part |>
        mutate(
          {{ addressr_errors }} := case_when(!is.na({{addressr_errors}}) ~ paste0({{addressr_errors}}, ", ", label), .default = label),
          {{ temp_address }} := str_replace({{ temp_address }}, dd_match, dd_standard)
        )

    } else {

      df_part <- df_part |>
        filter(is.na({{ address_name }})) |>
        mutate(
          {{ address_method }} := label,
          {{ address_name }} := str_extract({{ temp_address }}, regex),
          {{ address_name }} := if (!is.na(dd_standard)) str_replace({{ address_name }}, dd_match, dd_standard) else {{ address_name }},
          {{ temp_address }} := if (str_starts(label, address_components)) str_replace({{ temp_address }}, regex, str_glue("<{label}>")) else str_remove({{ temp_address }}, regex) |> str_squish() |> na_if("")
        )

    }

    df_out <- df_out |> anti_join(df_part, by = "addressr_id") |> bind_rows(df_part)

    toc()

  }

  df_out

}

#' @export
switch_abbr <- function(string, type, method = "short-to-long") {

  # make a vector of the available abbreviation types
  abbr_types <- unique(abbr_address$type)

  # catch errors
  if (!type %in% abbr_types) {
    type_message <- paste0("type must be one of the following:", str_flatten_comma(abbr_types, last = ", or "))
    stop(type_message)
  }
  if (!method %in% c("short-to-long", "long-to-short")) {
    stop("method must be 'short-to-long' or 'long-to-short'")
  }

  # filter abbreviations to the selected type
  df <- abbr_address[abbr_address$type == type, ]

  # switch abbreviations based on method
  if (method == "short-to-long") {
    str_replace_names(string, df$short, df$long)
  } else {
    df <- df[(is.na(df$short_type) | df$short_type == "abbr"), ]
    str_replace_names(string, df$long, df$short)
  }

}

standardize_street_name <- function(.data, address_column) {

  df <- .data |>
    extract_remove_squish({{ address_column }}, pre_direction, "pre_direction") |>
    extract_remove_squish({{ address_column }}, post_direction, "post_direction") |>
    tidy_part()

}


match_part <- function(address_column, address_part) {

  regex_to_match <- get_dd(address_part)
  regex_to_match <- paste0("^(.*)", regex_to_match)
  regex_names <- get_dd(address_part, dd_col = "names")
  regex_names <- c("address_column_new", regex_names)

  regex_match <- str_match(address_column, regex_to_match)
  regex_matches <- regex_match[, -1, drop = FALSE]
  out <- as_tibble(regex_matches, .name_repair = "none")
  colnames(out) <- regex_names
  address_column <- ifelse(is.na(regex_matches[, 1]), address_column, regex_matches[, 1])
  out[["address"]] <- address_column

  out

}

match_all_parts <- function(.data, address_column, address_part) {

  all_parts <- get_dd(address_part, "label")

  remainder <- .data
  out <- tibble()

  for (i in all_parts) {
    addr <- remainder |> pull({{address_column}})
    match <- match_part(addr, i)
    remainder <- match |> filter(is.na(address_column_new))
    matches <- match |> filter(!is.na(address_column_new)) |> mutate(label = i)
    out <- bind_rows(out, matches)
    if (i == last(all_parts)) {
      out <- bind_rows(out, remainder) |> select(-address_column_new)
    }
  }

  out

}

map_unpack <- function(
    data,
    cols,
    fun,
    names_sep,
    names_repair,
    error_call = caller_env()
) {
  cols <- tidyselect::eval_select(
    enquo(cols),
    data = data,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )
  col_names <- names(cols)

  for (col in col_names) {
    data[[col]] <- fun(data[[col]], col)
  }

  unpack(
    data = data,
    cols = all_of(col_names),
    names_sep = names_sep,
    names_repair = names_repair,
    error_call = error_call
  )
}

separate_address <- function(.data, address_column) {

  df <- .data |> mutate(input_column = {{address_column}})

  df <- df |>
    separate_wider_regex(
      input_column,
      patterns = c(
        input_column = ".*",
        ", ",
        city = "[A-Z ]+",
        ",? ",
        state = str_glue("{the_state}"),
        ",? ",
        zip = "\\d{5}(?:-\\d{4})?"
      ),
      too_few = "align_start"
    )

  df <- df |>
    separate_wider_regex(
      input_column,
      patterns = c(
        input_column = ".*?",
        " ",
        temp_back = str_glue("(?:\\b(?:{the_suffixes}|{the_unit_types})\\b|#).*")
      ),
      too_few = "align_start"
    )

  # df |>
  #   separate_wider_regex(
  #     input_column,
  #     patterns = c(
  #       extra_front = "[^\\dNSEW ]*",
  #       street_number = "\\d+(?:\\W*\\d)?",
  #       " ",
  #       pre_direction = str_glue("(?:\\b(?:{the_directions})\\b)*"),
  #       " ?",
  #       street_name = "[A-Z0-9 \\-]+",
  #       " ",
  #       suffix = the_suffixes,
  #       " ",
  #       post_direction = str_glue("(?:{the_directions})*")
  #     ),
  #     too_few = "debug"
  #   )
}
