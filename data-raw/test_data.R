## code to prepare `test_data` dataset goes here

test_data <- tribble(
  ~address,
  "123 Main St",
  "123 N Main St",
  "123 N Main St Apt 2E",
  "123 N Main St FL LOWER",
  "123 N Main St LOWER",
  "123 N Main St DEPT 2",
  "123 N Main St FLOOD",
  "123 N Main St - DEPT 2",
  "123 1st st apt 7B"
) |>
  clean_address(input_column = address) |>
  janitor::remove_empty("cols")

# usethis::use_data(test_data, overwrite = TRUE)
