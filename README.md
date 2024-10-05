
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
  "789 Pirate Cv East", "Memphis", "TN",
  "3548 1ST ST FL 1", "St. Louis", "MO"
)
```

Geocoders can be thrown off by various things, such as address ranges,
unit numbers, directionals, and street endings.

The `clean_address()` function streamlines address cleaning in a
standardized pattern:

- Convert all characters to uppercase
- Tweeze apart the address column into components
- Rename ordinal street names (“1ST” = “FIRST”)

``` r
library(addressr)
library(dplyr)

address_out <- address_table[, 1] |> 
  clean_address(address) |> 
  # clean_address(address, dataset = "default_db") |>   # for duckdb tbls
  select(street_number, street_number_range, pre_direction, 
         street_name = address, all_street_suffix, post_direction, unit)

address_out
#> # A tibble: 4 × 7
#>   street_number street_number_range pre_direction street_name all_street_suffix
#>   <chr>         <chr>               <chr>         <chr>       <chr>            
#> 1 456           <NA>                <NA>          JERSEY      AVENUE           
#> 2 <NA>          123-125             N             STREET      RD               
#> 3 789           <NA>                <NA>          PIRATE      CV               
#> 4 3548          <NA>                <NA>          FIRST       ST               
#> # ℹ 2 more variables: post_direction <chr>, unit <chr>
```

Behind the function is `extract_remove_squish()`, which will

- input `.data` with a column of address data
- extract a regex pattern from the `original_column`
- output the extracted string into a `new_column`
- trim the original column of extra whitespace or non-word characters
  (`\\W+`)
- the `pattern` may be a string or a named address component, from one
  of the tables mentioned above

To use the function, call
`extract_remove_squish(original_column, new_column, pattern)`.

``` r
address_table |>
  extract_remove_squish(address, unit, "unit") |>
  # extract_remove_squish_db("original_column", "new_column", "pattern") # duckdb
  select(address, unit, everything())
#> # A tibble: 4 × 4
#>   address             unit  city       state
#>   <chr>               <chr> <chr>      <chr>
#> 1 456 Jersey Avenue   #102  Montclair  NJ   
#> 2 123-125 N Street Rd <NA>  Cincinnati OH   
#> 3 789 Pirate Cv East  <NA>  Memphis    TN   
#> 4 3548 1ST ST         FL 1  St. Louis  MO
```

Address components are defined in `data-raw`:

- `address_abbreviations` are common groups of address-related words,
  including official USPS street endings.
  - Note: The abbreviations table calls `str_collapse_bound()`, to
    flatten values into a bounded regex,
    e.g. `\\b(N|S|E|W|NORTH|SOUTH...)\\b`. Vectors call
    `str_collapse_bound()` by default; regex and character strings will
    not.
- `address_regex` defines address components, e.g. `^\\d+\\b` for
  `street_number`.
- `address_corrections` is available but not currently in use.

Combine `switch_abbreviation()` with the `address_abbreviations` table
to alternate between short and long spellings of words. By default, the
function abbreviates words (`"long-to-short"`).

``` r
address_out |> 
  # switch_abbreviation_db(column, type, method = "long-to-short") |> 
  mutate(post_direction = switch_abbreviation(post_direction, "directions")) |> 
  mutate(all_street_suffix = switch_abbreviation(all_street_suffix, "official_street_suffix", "short-to-long"))
#> # A tibble: 4 × 7
#>   street_number street_number_range pre_direction street_name all_street_suffix
#>   <chr>         <chr>               <chr>         <chr>       <chr>            
#> 1 456           <NA>                <NA>          JERSEY      AVENUE           
#> 2 <NA>          123-125             N             STREET      ROAD             
#> 3 789           <NA>                <NA>          PIRATE      COVE             
#> 4 3548          <NA>                <NA>          FIRST       STREET           
#> # ℹ 2 more variables: post_direction <chr>, unit <chr>
```

To use your own input and output, use
`str_replace_names(string, input, output)`.

``` r
address_table |> 
  mutate(state_name = str_replace_names(state, state.abb, state.name)) |>
  select(city, state, state_name)
#> # A tibble: 4 × 3
#>   city       state state_name
#>   <chr>      <chr> <chr>     
#> 1 Montclair  NJ    New Jersey
#> 2 Cincinnati OH    Ohio      
#> 3 Memphis    TN    Tennessee 
#> 4 St. Louis  MO    Missouri
```

More to come. Please report issues, suggest/commit changes, open a
discussion.
