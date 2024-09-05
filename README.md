
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
  "123-125 N Main St", "Cincinnati", "OH",
  "789 Pirate Cv East", "Memphis", "TN"
)
```

Geocoders can be thrown off by various things, such as address ranges,
unit numbers, directionals, and street endings.

One method of standardizing addresses is to piece apart an address
column and put it back together in friendlier format for geocoders.

The `extract_remove_squish()` allows you to tweeze out parts of the
address using regex. The pattern will be removed from one column, placed
into a new column, and the original column will be trimmed of
whitespace. Trimming whitespace ensures we can chain certain regex
patterns, notably `^` or `$` to indicate the start or end of the string.

To use the function, call
`extract_remove_squish(original_column, new_column, pattern)` on a
table.

``` r
library(addressr)

address_table |>
  extract_remove_squish(address, street_number_range, "^\\d+-\\d+\\b") |>
  extract_remove_squish(address, street_number, "^\\d+\\b") |>
  extract_remove_squish(address, unit, "#.*") |>
  dplyr::select(street_number,
                street_number_range,
                street_name = address,
                unit,
                everything())
#> # A tibble: 3 × 6
#>   street_number street_number_range street_name    unit  city       state
#>   <chr>         <chr>               <chr>          <chr> <chr>      <chr>
#> 1 456           <NA>                Jersey Avenue  #102  Montclair  NJ   
#> 2 <NA>          123-125             N Main St      <NA>  Cincinnati OH   
#> 3 789           <NA>                Pirate Cv East <NA>  Memphis    TN
```

This method is flexible and allows you to: - Adjust the order of
extractions - Edit the regex easily, or declare the regex separately and
call it within the function.

More to come.
