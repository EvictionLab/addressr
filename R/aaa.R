
narrow_table <- function(.data, prefix) {
  .data |>
    select(starts_with(prefix)) |>
    rename_with(~ str_remove(., str_glue("{prefix}_"))) |>
    mutate(type = str_glue("{prefix}"), .before = 1)
}

# abbr_numbers <- addressr::abbr_numbers
# abbr_address <- addressr::abbr_address

# abbr_usps <- abbr_address |> filter(type == "usps_suffix")
# abbr_ordinal <- abbr_numbers |> narrow_table("ordinal")
# abbr_all_suffix <- abbr_address |> filter(type == "all_suffix")
# abbr_common_suffix <- abbr_address |> filter(type == "common_suffix")
# abbr_rare_suffix <- abbr_address |> filter(type == "rare_suffix")
#
# abbr_direction <- abbr_address |> filter(type == "direction")


flatten_abbr <- function(.data, bound = FALSE, exclude = NULL, abbr_cols = c("short", "long")) {
  out <- .data |>
    pivot_longer(all_of(abbr_cols), names_to = NULL) |>
    distinct()

  if (!is.null(exclude)) {
    out <- out |> filter(!value %in% exclude)
  }

  out <- out |>
    mutate(n = str_remove(value, "\\{.*\\}") |> str_length()) |>
    arrange(desc(n)) |>
    pull(value)

  if (bound) {
    out |> str_collapse_bound()
  } else out |> str_flatten("|")

}

