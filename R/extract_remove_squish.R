extract_remove_squish <- function(.data, original_column, new_column, pattern) {
  .data |>
    mutate(
      {{ new_column }} := str_extract({{ original_column }}, pattern),
      {{ original_column }} := str_remove({{ original_column }}, pattern) |> str_squish()
    )
}
