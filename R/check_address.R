#' Check the validity of an address
#'
#' @inheritParams clean_address
#' @param row_id A unique row_id for the dataset. If NULL, one will be created.
#' @param city A column containing the name of the city/town/place.
#' @param state A column containing the state. Long or abbreviated.
#' @param zip_code A column containing the zip code.
#'
#' @returns An object of the same type as .data, with the following properties:
#'    * New column `address_error` containing a list of errors for the address.
#' @export
#'
#' @examples
#' test_addresses |> check_address(address)
check_address <- function(.data, address_column, row_id = NULL, city = NULL, state = NULL, zip_code = NULL) {

  checked_address <- sym("checked_address")
  addressr_id <- sym("addressr_id")
  row_id <- enquo(row_id)
  state <- enquo(state)
  city <- enquo(city)
  zip_code <- enquo(zip_code)

  df <- .data |> mutate({{ checked_address }} := {{ address_column }})
  n_df <- nrow(df)

  message(str_glue("total rows: {n_df}"))

  if (quo_is_null(row_id)) {

    row_id <- "row_id"
    df <- df |> mutate({{ row_id }} := row_number(), .before = 1)

  }

  df_distinct <- df |>
    select({{ address_column }}) |>
    distinct()

  n_distinct <- nrow(df_distinct)
  message(str_glue("distinct addresses: {n_distinct}"))

  regex_fix <- c(
    "\\b(\\W){2,}\\b" = "\\1",
    "([A-Z0-9]+)\\s+-\\s+([A-Z0-9]+)" = "\\1-\\2",
    "(\\d+)\\s*,\\s*(\\d+)" = "\\1, \\2",
    "(\\d+)\\s*(&|AND)\\s*(\\d+)" = "\\1 & \\3"
  )

  df <- df |>
    mutate({{ checked_address }} := str_to_upper({{ checked_address }}) |> str_remove_all("\\.|'") |> str_replace_all(regex_fix) |> str_squish())

  address_errors <- sym("address_errors")

  df_na <- df |>
    filter(is.na({{ checked_address }}) | {{ checked_address }} %in% c("N\\W?A", "NULL", "NAN", "(MISSING|NONE|NO|UNKNOWN) ADDRESS", "")) |>
    mutate(address_errors = "na_address")
  n_na <- nrow(df_na)
  message(str_glue("na_address: {n_na}"))

  df <- df |> anti_join(df_na, by = {{ row_id }})

  n_errors <- sym("n_errors")

  regex_zip <- "\\b\\d{5}(\\-\\d{4})?\\b"
  regex_states <- str_collapse_bound(c(the_states$long, the_states$short))

  checks <- c(
    only_state = str_glue("^({regex_states})$"),
    only_zip_state = str_glue("^({regex_zip}),? ({regex_states})$"),
    has_zip_state = str_glue("(({regex_zip}),? ({regex_states})|(({regex_states}) ({regex_zip})))"),
    possible_zip = str_glue("{regex_zip}$"),
    no_number = "^[^\\d]+$",
    no_words = "^[^A-Z]+$",
    no_spaces = "^[^ ]+$",
    non_address_words = "\\b(BEDROOM|KITCHEN|BATHROOM|LAUNDRY|RENT)\\b",
    has_punct = "[:symbol:]|[;\\?\\!\\/\\*@#\\-_\\'\\[\\]\\{\\}\\:]",
    only_zeros = "\\b0+\\b",
    leading_zeros = "(?<!-)\\b0+[1-9]+\\b"
  )

  check_names <- names(checks)
  address_errors <- sym("address_errors")

  check_string <- function(.data, string, pattern, error_name, row_id) {

    df <- .data |> select({{ row_id }}, {{ string }}) |> filter(str_detect({{ string }}, pattern))
    n_string <- nrow(df)

    if (n_string != 0) {

      message(str_glue("{error_name}: {n_string}"))

      df <- df |>
        mutate({{ address_errors }} := paste0(error_name))

    }

    df

  }

  all_checks <- purrr::map2(checks, check_names, ~ check_string(df, {{ checked_address }}, .x, .y, {{ row_id }})) |> purrr::list_rbind()
  all_checks <- all_checks |> summarise(address_errors = str_flatten_comma(unique(address_errors), na.rm = TRUE), .by = {{ row_id }})

  df <- df |> left_join(all_checks, by = {{ row_id }}) |> bind_rows(df_na)

  # n_spaces <- sym("n_spaces")
  # n_let_grps <- sym("n_let_grps")
  # n_num_grps <- sym("n_num_grps")
  spaceless_address <- sym("spaceless_address")
  n_alnum_grps <- sym("n_alnum_grps")
  first_number <- sym("first_number")
  address_length <- sym("address_length")
  address_pattern <- sym("address_pattern")

  address_patterns <- c(
    po_box = check_pattern("po_box"),
    extra_front = "^([A-Z\\W]+ )+(?!\\d+\\W?[NSEW]\\s?\\d+?)",
    building = paste0(check_pattern("building"), "|\\b\\d+-?[ABCDFGHIJKLMPQRTUVWXYZ]\\b"),
    street_name_fraction = check_pattern("street_name_fraction"),
    fraction = "1/2\\b",
    number_range = "\\b\\d+-\\d+\\b",
    number_multi = "\\b\\d+([,& ]+\\d+)+",
    street_number = "^\\d+\\b",
    city_state_zip = str_glue("(?<=(, |^))[A-Z ]+,? {regex_states},? {regex_zip}"),
    street_suffix = str_remove(check_pattern("all_street_suffix"), "\\$"),
    highway = "\\b((STATE|COUNTY|CTH?Y|U ?S|ST|CO) )?(HIGHWAY|HWY|HY)\\b",
    zip_code = regex_zip,
    state = regex_states,
    ordinal = "\\b\\d+[SNRT][TDH]\\b",
    special_units_regex = check_pattern("special_units_regex"),
    unit_type = paste0(check_pattern("unit_type"), "|#"),
    street_number_coords = check_pattern("street_number_coords"),
    directions = check_pattern("direction")
  )

  address_pat_names <- str_c("{", names(address_patterns), "}")

  df <- df |>
    mutate(
      {{ spaceless_address }} := str_remove_all({{ checked_address }}, "[^A-Z0-9]"),
      {{ address_length }} := str_length({{ checked_address }}),
      # {{ n_spaces }} := str_count({{ checked_address }}, "\\s"),
      # {{ n_let_grps }} := str_count({{ checked_address }}, "\\b([A-Z]+)\\b"),
      # {{ n_num_grps }} := str_count({{ checked_address }}, "\\b(\\d+)\\b"),
      {{ n_alnum_grps }} := str_count({{ checked_address }}, "\\b([A-Z]+[0-9]+)|([0-9]+[A-Z]+)\\b"),
      {{ first_number }} := str_locate({{ checked_address }}, "\\d")[, "start"],
      {{ address_pattern }} := str_replace_names({{ checked_address }}, address_patterns, address_pat_names)
    )

  if (nrow(all_checks) == 0) {

    message("No address errors detected!")

  }

  df |> select(-checked_address)

}
