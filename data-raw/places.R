## code to prepare `places` dataset goes here
library(readr)
# library(dplyr)
# library(tidyr)
library(sf)
library(tigris)
# library(stringr)
library(purrr)

the_states <- state.abb

if (!file.exists("data-raw/places.csv")) {

  get_places <- function(state) {

    the_places <- places({{ state }}) |> st_drop_geometry() |> select(geoid = GEOID, city = NAME) |> mutate(state = {{ state }}, .after = 1)
    write_csv(the_places, str_glue("data-raw/places/{state}.csv"))

  }

  walk(the_states, get_places)

  all_places <- map(the_states, ~ read_csv(str_glue("data-raw/places/{.}.csv"), col_types = "ccc")) |> list_rbind()
  all_places <- all_places |>
    mutate(city = str_to_upper(city) |> str_remove_all("'|\\.| \\(balance\\)"))

  write_csv(all_places, "data-raw/places.csv")

} else all_places <- read_csv("data-raw/places.csv")

nonword_places <- all_places |>
  filter(str_detect(city, "[:punct:]|[:symbol:]")) |>
  mutate(city)
shelby_ranges <- tigris::address_ranges("TN", "Shelby")
the_ranges <- shelby_ranges |>
  st_drop_geometry() |>
  select(street_name = FULLNAME, start_left = LFROMHN, end_left = LTOHN, start_right = RFROMHN, end_right = RTOHN, zip_left = ZIPL, zip_right = ZIPR) |>
  pivot_longer(
    cols = c("start_left", "end_left", "start_right", "end_right", "zip_left", "zip_right"),
    names_pattern = "(.*)_(.*)",
    names_to = c(".value", "street_side")
  ) |>
  filter(if_all(c("start", "end"), ~ !is.na(.))) |>
  arrange(street_name, zip, street_side, start) |>
  mutate(
    row_id = row_number(),
    across(c(start, end), ~ as.numeric(.))) |>
  mutate(
    min = min(start, end),
    max = max(start, end),
    .by = "row_id"
  ) |>
  mutate(
    cont = if_else(max + 2 == lead(min), "go", "stop", "stop"),
    .by = c("street_name", "street_side", "zip")
    )


the_ranges_2 <- the_ranges |>
  mutate(
    true_start = case_when(
      row_id == 1 ~ min,
      cont == "stop" & lag(cont) == "stop" ~ min,
      cont == "stop" & lag(cont) == "go" ~ NA,
      cont == "go" & lag(cont) != "go" ~ min,
      cont == "go" & lag(cont) == "go" ~ NA,
      .default = 0
    ),
    true_end = case_when(
      row_id == max(row_id) ~ max,
      cont == "stop" & lag(cont) == "stop" ~ max,
      cont == "stop" & lag(cont) == "go" ~ max,
      cont == "go" ~ NA,
      .default = 0
    )
  ) |>
  fill(true_start, .direction = "down") |>
  fill(true_end, .direction = "up") |>
  select(street_name, street_side, zip, true_start, true_end) |>
  distinct() |>
  mutate(street_side = case_when(
    str_ends(true_start, "[02468]") & str_ends(true_end, "[02468]") ~ "even",
    str_ends(true_start, "[13579]") & str_ends(true_end, "[13579]") ~ "odd",
    .default = "weird"
  )) |>
  mutate(street_name = str_to_upper(street_name)) |>
  extract_remove_squish(street_name, post_direction, "post_direction") |>
  extract_remove_squish(street_name, street_suffix_ext, "\\b(EXD|RMP)\\b") |>
  extract_remove_squish(street_name, street_suffix_etc, "\\b(CR|BL)\\b") |>
  extract_remove_squish(street_name, street_suffix, "all_street_suffix") |>
  extract_remove_squish(street_name, pre_direction, "pre_direction") |>
  mutate(street_name = str_replace_all(street_name, "\\b\\d{1,3}[RSTN][DTH]\\b", replace_ordinals),
         street_name = str_replace_all(street_name, regex_street_names)) |>
  extract_remove_squish(street_name, street_suffix_2, "all_street_suffix") |>
  mutate(street_name = case_when(
    !is.na(street_name) & street_suffix == "ARC" ~ paste(street_name, street_suffix),
    !is.na(street_name) & street_suffix_2 == "ARC" ~ paste(street_name, street_suffix_2),
    is.na(street_name) & !is.na(street_suffix_2) ~ street_suffix_2,
    is.na(street_name) & !is.na(street_suffix) ~ street_suffix,
    .default = street_name
    ),
    street_suffix_2 = case_when(street_suffix_2 == street_name ~ NA_character_, street_suffix == "ARC" ~ NA_character_, .default = street_suffix_2),
    street_suffix = case_when(street_suffix == street_name ~ NA_character_, street_suffix == "ARC" ~ NA_character_, .default = street_suffix),
    street_suffix = switch_abbreviation(street_suffix, "all_street_suffixes", "long-to-short"),
    street_suffix = switch_abbreviation(street_suffix, "official_street_suffixes", "short-to-long"),
    street_suffix_2 = switch_abbreviation(street_suffix_2, "all_street_suffixes", "long-to-short"),
    street_suffix_2 = switch_abbreviation(street_suffix_2, "official_street_suffixes", "short-to-long"),
  )

example_maf <- the_ranges_2 |>
  rename(street_number_start = true_start, street_number_end = true_end, street_suffix_1 = street_suffix_2) |>
  select(zip, street_number_start, street_number_end, pre_direction, street_name, street_suffix_1, street_suffix_2 = street_suffix, street_suffix_ext, post_direction, street_side)



regex_street_names <- c(
  " N'([A-Z]+)" = " N \\1",
  "'" = "",
  "&" = "AND",
  "-" = " ",
  "\\." = "",
  "^ST " = "SAINT ",
  "^MT " = "MOUNT ",
  "^FT " = "FORT "
)

street_names <- example_maf |> select(zip, pre_direction:post_direction) |> distinct()
street_names <- the_ranges_2 |> summarise(pre_direction = str_flatten_comma(unique(pre_direction)), street_suffix = str_flatten_comma(unique(street_suffix)), n = n(), .by = c("street_name", "true_start"))

usethis::use_data(places, overwrite = TRUE)
