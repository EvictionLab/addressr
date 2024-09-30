## code to prepare `address_corrections` dataset goes here

address_corrections <- tribble(
  ~input, ~output, ~city, ~state,
  "P( )?TREE", "PEACHTREE", NA_character_, "GA",
  "ATL", "ATLANTA", NA_character_, "GA",
  "AANDB( MHP)?", "A AND B MHP", "GRIFFIN", "GA",
  "(DR )?M(ARTIN)?( )?L(UTHER)?( )?K(ING)?( JR)?", "MARTIN LUTHER KING JR", NA_character_, "all",
  "M L K[LI]{2}NG JR", "MARTIN LUTHER KING JR", NA_character_, "all",
  "M L KING FR", "MARTIN LUTHER KING JR", NA_character_, "all",
  "MKL", "MARTIN LUTHER KING JR", NA_character_, "all",
  "GA", "GEORGIA", NA_character_, "GA",
  "TIND[AE]LL( (HEIGHT(S)?|HGHTS|HGTS|HOMES))?", "1850 TINDALL AVE", "MACON", "GA",
  "PENDLETON( HOMES)?", "3401 HOUSTON AVE", "MACON", "GA",
  "BOWDEN( HOMES)?", "2301 HOUSTON AVE", "MACON", "GA",
  "FELTON( HOMES)?", "2035 FELTON AVE", "MACON", "GA",
  "([A-D] )?RIVERWALK", "5327 RIVERWALK DR", "COLLEGE PARK", "GA",
  "BOLTON", "2108 BOLTON RD NW", "ATLANTA", "GA",
  "SUMMERGLEN", "6425 OAKLEY RD", "UNION CITY", "GA",
  "CONT COLONY", "CONTINENTAL COLONY", "ATLANTA", "GA",
  "CONTL CLNY", "CONTINENTAL COLONY", "ATLANTA", "GA",
)

usethis::use_data(address_corrections, overwrite = TRUE)
