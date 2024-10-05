## code to prepare `address_regex` dataset goes here

address_regex <- tribble(
  ~address_part, ~regex,
  "street_number", "^\\d+\\b",
  "street_number_range", "^\\d+-\\d+\\b",
  "street_number_fraction", "\\d/\\d\\b",
  "po_box", "(P( )?O )?BOX \\w+",
  "building", "^\\d+[A-Z]\\b|^[A-Z]\\d\\b"
)

usethis::use_data(address_regex, overwrite = TRUE, internal = TRUE)
