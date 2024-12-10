## code to prepare `address_regex` dataset goes here

# letter_no_NSEWO <- str_collapse_bound(LETTERS[!LETTERS %in% c("N", "S", "E", "W", "O")])

address_regex <- tribble(
  ~address_part, ~regex,
  "street_number", "^\\d+\\b",
  "street_number_multi", "^\\d+\\b(\\W)+(\\d+\\b(\\W|AND)+){2,}(?! [STNDRH]{2})",
  "street_number_range", "^\\d+(\\s+)?(\\W+|AND)(\\s+)?\\d+\\b(?! [STNDRH]{2})",
  "street_number_range_db", "^\\d+(\\s+)?(-|/| )(\\s+)?\\d+\\b",
  "street_number_fraction", "\\d/\\d\\b",
  "po_box", "(P( )?O )?BOX \\w+",
  "building", "^\\d+[A-Z]\\b|^[A-Z]\\d\\b|^(A|B|C|D|F|G|H|I|J|K|L|M|P|Q|R|T|U|V|X|Y|Z)\\b"
)

usethis::use_data(address_regex, addr_abbr, overwrite = TRUE, internal = TRUE)
