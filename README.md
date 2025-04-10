
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
library(dplyr)

address_table <- dplyr::tribble(
  ~address, ~city, ~state,
  "456 Jersey Avenue #102", "Montclair", "NJ",
  "123-125 N Street Rd", "Cincinnati", "OH",
  "789 Pirate Cv East DBA ELAB LLC", "Memphis", "TN",
  "3548 1ST ST FL 1", "St. Louis", "MO",
)
```

Geocoders can be thrown off by various things, such as address ranges,
unit numbers, directionals, and street endings.

The `clean_address()` function streamlines address cleaning and outputs
data into a standard format

``` r
library(addressr)

cleaned_addresses <- address_table |> clean_address(address) |> janitor::remove_empty("cols")
#> preserve original data: 0.007 sec elapsed
#> extract address parts: 0.014 sec elapsed
#> standardize street suffix, directions & ordinals: 0.034 sec elapsed
#> check street numbers, units, and buildings: 0.01 sec elapsed
#> tidy output: 0.003 sec elapsed
#> total clean time: 0.072 sec elapsed

cleaned_addresses
#> # A tibble: 4 × 11
#>   address            city  state clean_address street_number_multi pre_direction
#>   <chr>              <chr> <chr> <chr>         <chr>               <chr>        
#> 1 456 Jersey Avenue… Mont… NJ    456 JERSEY A… 456                 <NA>         
#> 2 123-125 N Street … Cinc… OH    123-125 N ST… 123-125             N            
#> 3 789 Pirate Cv Eas… Memp… TN    789 PIRATE C… 789                 <NA>         
#> 4 3548 1ST ST FL 1   St. … MO    3548 FIRST S… 3548                <NA>         
#> # ℹ 5 more variables: street_name <chr>, street_suffix <chr>, unit_type <chr>,
#> #   unit <chr>, extra_back <chr>
```

By default, the function returns all address components. You can also
select the output:

``` r
address_table |> clean_address(address, output = c("clean_address", "short_address", "street_number", "unit", "extra"))
#> preserve original data: 0.002 sec elapsed
#> extract address parts: 0.013 sec elapsed
#> standardize street suffix, directions & ordinals: 0.03 sec elapsed
#> check street numbers, units, and buildings: 0.009 sec elapsed
#> tidy output: 0.004 sec elapsed
#> total clean time: 0.06 sec elapsed
#> # A tibble: 4 × 8
#>   address      city  state clean_address short_address street_number unit  extra
#>   <chr>        <chr> <chr> <chr>         <chr>         <chr>         <chr> <chr>
#> 1 456 Jersey … Mont… NJ    456 JERSEY A… 456 JERSEY    456           102   <NA> 
#> 2 123-125 N S… Cinc… OH    123-125 N ST… 123-125 STRE… 123-125       <NA>  <NA> 
#> 3 789 Pirate … Memp… TN    789 PIRATE C… 789 PIRATE    789           <NA>  EAST…
#> 4 3548 1ST ST… St. … MO    3548 FIRST S… 3548 FIRST    3548          1     <NA>
```

`clean_address` will return the street number, pre-direction, street
name, street suffix and post-direction. `short_address` will return the
street number and street name.

The package can also separate rows with a street range or multiple
addresses:

``` r
address_table <- address_table |> 
  add_row(address = "928-928 S Montgomery Ave 1500 Jefferson Rd", city = "New York", state = "NY") |> 
  add_row(address = "1500-1502 1550 Ptree Rd", city = "Atlanta", state = "GA")

address_table |> clean_address(address, separate_street_range = TRUE, separate_multi_address = TRUE)
#> preserve original data: 0.002 sec elapsed
#> separate multiple addresses: 0.007 sec elapsed
#> extract address parts: 0.011 sec elapsed
#> standardize street suffix, directions & ordinals: 0.03 sec elapsed
#> check street numbers, units, and buildings: 0.021 sec elapsed
#> tidy output: 0.003 sec elapsed
#> total clean time: 0.075 sec elapsed
#> # A tibble: 10 × 20
#>    address                 city  state original_row_id addressr_id clean_address
#>    <chr>                   <chr> <chr>           <int> <chr>       <chr>        
#>  1 456 Jersey Avenue #102  Mont… NJ                  1 1           456 JERSEY A…
#>  2 123-125 N Street Rd     Cinc… OH                  2 2-N1        123 N STREET…
#>  3 123-125 N Street Rd     Cinc… OH                  2 2-N2        125 N STREET…
#>  4 789 Pirate Cv East DBA… Memp… TN                  3 3           789 PIRATE C…
#>  5 3548 1ST ST FL 1        St. … MO                  4 4           3548 FIRST S…
#>  6 928-928 S Montgomery A… New … NY                  5 5-A1        928 S MONTGO…
#>  7 928-928 S Montgomery A… New … NY                  5 5-A2        1500 JEFFERS…
#>  8 1500-1502 1550 Ptree Rd Atla… GA                  6 6-N1        1500 PEACHTR…
#>  9 1500-1502 1550 Ptree Rd Atla… GA                  6 6-N2        1502 PEACHTR…
#> 10 1500-1502 1550 Ptree Rd Atla… GA                  6 6-N3        1550 PEACHTR…
#> # ℹ 14 more variables: street_number_coords <chr>, street_number <chr>,
#> #   street_number_multi <chr>, street_number_fraction <chr>,
#> #   pre_direction <chr>, street_name <chr>, street_suffix <chr>,
#> #   post_direction <chr>, building <chr>, unit_type <chr>, unit <chr>,
#> #   extra_front <chr>, extra_back <chr>, extra_unit <chr>
```

If there is a common pattern that is not removed with the function, you
can use `extract_remove_squish()`, to pre-clean the data.

``` r
address_table |> 
  add_row(address = "246 S Bend St Unit 530 3 Bedroom") |> 
  extract_remove_squish(address, other, "\\d Bedroom")
#> # A tibble: 7 × 4
#>   address                                    city       state other    
#>   <chr>                                      <chr>      <chr> <chr>    
#> 1 456 Jersey Avenue #102                     Montclair  NJ    <NA>     
#> 2 123-125 N Street Rd                        Cincinnati OH    <NA>     
#> 3 789 Pirate Cv East DBA ELAB LLC            Memphis    TN    <NA>     
#> 4 3548 1ST ST FL 1                           St. Louis  MO    <NA>     
#> 5 928-928 S Montgomery Ave 1500 Jefferson Rd New York   NY    <NA>     
#> 6 1500-1502 1550 Ptree Rd                    Atlanta    GA    <NA>     
#> 7 246 S Bend St Unit 530                     <NA>       <NA>  3 Bedroom
```

To switch a column from the abbreviated spelling to the long format,
there are two functions: `switch_abbreviation` and `str_replace_names`

- `switch_abbreviation()` for abbreviations included in the
  `address_abbreviations` dataset: directions, all_street_suffixes,
  official_street_suffixes, unit_types, special_units, and highways.
  - `all_street_suffixes` has a one-to-many relationship and should only
    be used `long-to-short` to standardize spellings into the same
    official abbreviation (AV, AVE, AVN to AVE)
  - `official_street_suffixes` has a one-to-one relationship between
    short and long spellings. It can be used either `short-to-long` or
    `long-to-short` (assuming all endings are in the official format).

``` r
address_table |> 
  clean_address(address) |> 
  mutate(
    street_suffix_short = switch_abbreviation(street_suffix, "official_street_suffixes", "long-to-short"),
    .keep = "used"
    )
#> preserve original data: 0.004 sec elapsed
#> extract address parts: 0.011 sec elapsed
#> standardize street suffix, directions & ordinals: 0.039 sec elapsed
#> check street numbers, units, and buildings: 0.008 sec elapsed
#> tidy output: 0.003 sec elapsed
#> total clean time: 0.066 sec elapsed
#> # A tibble: 6 × 2
#>   street_suffix street_suffix_short
#>   <chr>         <chr>              
#> 1 AVENUE        AVE                
#> 2 ROAD          RD                 
#> 3 COVE          CV                 
#> 4 STREET        ST                 
#> 5 ROAD          RD                 
#> 6 ROAD          RD
```

- `str_replace_names()` to replace any vector with another vector of the
  same length

``` r
address_table |> 
  mutate(state = str_replace_names(state, state.abb, state.name))
#> # A tibble: 6 × 3
#>   address                                    city       state     
#>   <chr>                                      <chr>      <chr>     
#> 1 456 Jersey Avenue #102                     Montclair  New Jersey
#> 2 123-125 N Street Rd                        Cincinnati Ohio      
#> 3 789 Pirate Cv East DBA ELAB LLC            Memphis    Tennessee 
#> 4 3548 1ST ST FL 1                           St. Louis  Missouri  
#> 5 928-928 S Montgomery Ave 1500 Jefferson Rd New York   New York  
#> 6 1500-1502 1550 Ptree Rd                    Atlanta    Georgia
```
