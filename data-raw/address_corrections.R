## code to prepare `address_corrections` dataset goes here

address_corrections <- tribble(
  ~input, ~output, ~city, ~state,
  "P( )?TREE", "PEACHTREE", NA_character_, "GA",
  "ATL", "ATLANTA", NA_character_, "GA",
  "CONT(L)? C[OL]+NY", "CONTINENTAL COLONY", "ATLANTA", "GA",
)

usethis::use_data(address_corrections, overwrite = TRUE)
