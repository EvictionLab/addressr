## code to prepare `test_data` dataset goes here

test_data <- tribble(
  ~address,
  "123 and 125 East Main St 123-A",
  "123 East Main St 123-A",
  "123 East Main St A",
  "123 East Main St #123A #",
  "123 East Main St N",
  "123 East Main St - 248W",
  "123 East Main St",
  "123 East 97th St",
  "123 East St",
  "123 N",
  "123 Main St",
  "123 Cottage St",
  "123 Main St Rear Cottage",
  "123 N Main St",
  "123 N Main St Apt 2E",
  "123 N Main St FL LOWR",
  "123 N Main St LOWER",
  "123 N Main St UNIT LOWR",
  "123 N Main St LOWR UNIT",
  "123 N Main St DEPT 2",
  "123 N Main St FLOOR",
  "123 N Main St - DEPT 2",
  "123 1st st apt 7B",
  "123 - 123 N Main St Apt 32",
  "123 - 125 N Main St Apt 32",
  "123-125 N Main St Apt 32",
  "121 123 125 N Main St",
  "121-125 N Main St And 187 S Main St",
  "121-125 N Main St / 187 S Main St",
  "123 125 N Main St Apt 32",
  "123A Main Street South",
  "123 A Main Street South",
  "12135, 12215, 12205 AND 12221 W Main Ave",
  "12135, 12215, 12205 & 12221 W Main Ave",
) |>
  clean_address(address) |>
  janitor::remove_empty("cols")

# usethis::use_data(test_data, overwrite = TRUE)
