
<!-- README.md is generated from README.Rmd. Please edit that file -->

# addressr

<!-- badges: start -->
<!-- badges: end -->

The goal of addressr is to standardize address cleaning for various
datasets used by the Eviction Lab.

## Installation

You can install the development version of addressr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("EvictionLab/addressr")
```

## Usage

Addresses can be difficult to work with and messy in many different
ways.

Here is an example of a relatively clean table:

``` r
address_table <- dplyr::tribble(
  ~address, ~city, ~state,
  "456 Jersey Avenue #102", "Montclair", "NJ",
  "123-125 N Street Rd", "Cincinnati", "OH",
  "928-928 S Montgomery Ave 1500-1502 1550 Ptree Rd", "Atlanta", "GA",
  "789 Pirate Cv East", "Memphis", "TN",
  "3548 1ST ST FL 1", "St. Louis", "MO"
)
```

Geocoders can be thrown off by various things, such as address ranges,
unit numbers, directionals, and street endings.

The `clean_address()` function streamlines address cleaning and outputs
data into a standard format

``` r
library(addressr)
library(dplyr)

cleaned_addresses <- address_table |> clean_address(address)
#> preserve original data: 0.01 sec elapsed
#> separate multiple addresses: 0.01 sec elapsed
#> extract address parts: 0.012 sec elapsed
#> standardize street suffix, directions & ordinals: 0.023 sec elapsed
#> check street ranges: 0.014 sec elapsed
#> check units: 0.009 sec elapsed
#> check buildings: 0.001 sec elapsed
#> tidy output: 0.007 sec elapsed
#> total clean time: 0.088 sec elapsed

cleaned_addresses |> janitor::remove_empty("cols")
#> # A tibble: 9 × 13
#>   raw_address  original_row_id addressr_id clean_address street_name city  state
#>   <chr>                  <int> <chr>       <chr>         <chr>       <chr> <chr>
#> 1 456 Jersey …               1 1           456 JERSEY A… JERSEY      Mont… NJ   
#> 2 123-125 N S…               2 2-N1        123 N STREET… N           Cinc… OH   
#> 3 123-125 N S…               2 2-N2        125 N STREET… N           Cinc… OH   
#> 4 928-928 S M…               3 3-A1        928 S MONTGO… MONTGOMERY  Atla… GA   
#> 5 928-928 S M…               3 3-A2-N1     1500 PEACHTR… PEACHTREE   Atla… GA   
#> 6 928-928 S M…               3 3-A2-N2     1502 PEACHTR… PEACHTREE   Atla… GA   
#> 7 928-928 S M…               3 3-A2-N3     1550 PEACHTR… PEACHTREE   Atla… GA   
#> 8 789 Pirate …               4 4           789 PIRATE C… PIRATE      Memp… TN   
#> 9 3548 1ST ST…               5 5           3548 FIRST S… FIRST       St. … MO   
#> # ℹ 6 more variables: street_number <chr>, unit <chr>, unit_type <chr>,
#> #   post_direction <chr>, street_suffix <chr>, pre_direction <chr>
```

If there is a common pattern that is not removed with the function, you
can use `extract_remove_squish()`, to pre-clean the data.

``` r
address_table |> 
  add_row(address = "246 S Bend St Unit 530 3 Bedroom", 
          city = "Princeton", 
          state = "NJ") |> 
  extract_remove_squish(address, other, "\\d Bedroom")
#> # A tibble: 6 × 4
#>   address                                          city       state other    
#>   <chr>                                            <chr>      <chr> <chr>    
#> 1 456 Jersey Avenue #102                           Montclair  NJ    <NA>     
#> 2 123-125 N Street Rd                              Cincinnati OH    <NA>     
#> 3 928-928 S Montgomery Ave 1500-1502 1550 Ptree Rd Atlanta    GA    <NA>     
#> 4 789 Pirate Cv East                               Memphis    TN    <NA>     
#> 5 3548 1ST ST FL 1                                 St. Louis  MO    <NA>     
#> 6 246 S Bend St Unit 530                           Princeton  NJ    3 Bedroom
```

To switch a column from the abbreviated spelling to the long format,
there are two functions:

- `switch_abbreviation()` for abbreviations included in the
  `address_abbreviations` dataset: directions, all_street_suffixes,
  official_street_suffixes, ordinals, unit_types, and special_units

``` r
cleaned_addresses |> 
  mutate(
    post_direction_long = switch_abbreviation(post_direction, "directions", "short-to-long"),
    street_suffix_short = switch_abbreviation(street_suffix, "official_street_suffixes", "long-to-short"),
    .keep = "used"
    )
#> # A tibble: 9 × 4
#>   post_direction street_suffix post_direction_long street_suffix_short
#>   <chr>          <chr>         <chr>               <chr>              
#> 1 <NA>           AVENUE        <NA>                AVE                
#> 2 <NA>           STREET ROAD   <NA>                ST RD              
#> 3 <NA>           STREET ROAD   <NA>                ST RD              
#> 4 <NA>           AVENUE        <NA>                AVE                
#> 5 <NA>           ROAD          <NA>                RD                 
#> 6 <NA>           ROAD          <NA>                RD                 
#> 7 <NA>           ROAD          <NA>                RD                 
#> 8 E              COVE          EAST                CV                 
#> 9 <NA>           STREET        <NA>                ST
```

- `str_replace_names()` to replace any vector with another vector of the
  same length

``` r
address_table |> 
  mutate(state = str_replace_names(state, state.abb, state.name))
#> # A tibble: 5 × 3
#>   address                                          city       state     
#>   <chr>                                            <chr>      <chr>     
#> 1 456 Jersey Avenue #102                           Montclair  New Jersey
#> 2 123-125 N Street Rd                              Cincinnati Ohio      
#> 3 928-928 S Montgomery Ave 1500-1502 1550 Ptree Rd Atlanta    Georgia   
#> 4 789 Pirate Cv East                               Memphis    Tennessee 
#> 5 3548 1ST ST FL 1                                 St. Louis  Missouri
```
