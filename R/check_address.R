#' Check the validity of an address
#'
#' @inheritParams clean_address
#' @param row_id A unique row_id for the dataset. If NULL, one will be created.
#' @param city A column containing the name of the city/town/place.
#' @param state A column containing the state. Long or abbreviated.
#' @param zip_code A column containing the zip code.
#'
#' @returns An object of the same type as .data, with the following properties:
#'    * New column `checked_address` containing the recommended address.
#'    * New column `error` containing a list of errors for the address.
#' @export
#'
#' @examples
#' test_addresses |> check_address(address)
#'
#' test_addresses |> check_address(address, "addressr_id")
check_address <- function(.data, address_column, row_id = "row_id", city = NULL, state = NULL, zip_code = NULL) {

  checked_address <- sym("checked_address")
  checked_state <- sym("checked_state")

  df <- .data |>
    mutate(
      {{ checked_address }} := {{ address_column }},
      {{ checked_state }} := {{ state }}
      )

  if (row_id == "row_id") {

    df <- df |> mutate({{ row_id }} := row_number(), .before = 1)

  }

  # --------------------------------------------------- error 1: NA address ----
  error_1 <- sym("error_1")
  error_na <- df |>
    filter(is.na({{ address_column }}) | {{ address_column }} %in% c("N\\W?A", "NULL", "(MISSING|NONE|NO|UNKNOWN) ADDRESS"))

  n_error_na <- nrow(error_na)

  if (nrow(error_na) != 0) {

    message(str_glue("missing address: {n_error_na}"))

    error_na <- error_na |>
      mutate({{ error_1 }} := "missing_address",
             {{ checked_address }} := NA_character_)

    df <- df |>
      anti_join(error_na, by = {{ row_id }}) |>
      bind_rows(error_na)

  }


  if (!is.null(checked_state)) {
    # ------------------------------------------------------ error 2: state ----
    error_2 <- sym("error_2")

    the_state_names <- c(state.name, "DISTRICT OF COLUMBIA") |> str_to_upper()
    the_state_abbr <- c(state.abb, "DC")

    df <- df |>
      mutate(
        {{ checked_state }} := str_to_upper({{ checked_state }}) |> str_remove_all("\\."),
        {{ checked_state }} := str_replace_names({{ checked_state }}, the_state_names, the_state_abbr),
        {{ error_2 }} := case_when(
          is.na(checked_state) ~ "missing_state",
          !{{ checked_state }} %in% the_state_abbr ~ "unknown_state",
          .default = NA_character_
        )
      )

    n_error_state_na <- df |> filter(error_2 == "missing_state") |> nrow()
    n_error_state_idk <- df |> filter(error_2 == "unknown_state") |> nrow()

    if (n_error_state_na != 0) message(str_glue("missing state: {n_error_state_na}"))
    if (n_error_state_idk != 0) message(str_glue("unknown state: {n_error_state_idk}"))

  }

  # ---------------------------------------------------------------- output ----
  address_errors <- sym("address_errors")

  error_names <- names(df) |> str_subset("error_\\d")

  if (length(error_names) > 0) {

    df <- df |>
      unite({{ address_errors }}, starts_with("error"), sep = "; ", na.rm = TRUE) |>
      mutate({{ address_errors }} := str_squish({{ address_errors }}) |> na_if("")) |>
      arrange(row_id)

  } else message("No address errors detected!")

  df

}
