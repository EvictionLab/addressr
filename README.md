
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
`extract_remove_squish(original_column, new_column, pattern)`.

``` r
library(dplyr)
library(addressr)

address_table <- address_table |>
  extract_remove_squish(address, street_number_range, "^\\d+-\\d+\\b") |>
  extract_remove_squish(address, street_number, "^\\d+\\b") |>
  extract_remove_squish(address, unit, "#.*") |>
  select(street_number, street_number_range, street_name = address, unit, everything())

address_table
#> # A tibble: 3 × 6
#>   street_number street_number_range street_name    unit  city       state
#>   <chr>         <chr>               <chr>          <chr> <chr>      <chr>
#> 1 456           <NA>                Jersey Avenue  #102  Montclair  NJ   
#> 2 <NA>          123-125             N Street Rd    <NA>  Cincinnati OH   
#> 3 789           <NA>                Pirate Cv East <NA>  Memphis    TN
```

This method is flexible and allows you to:

- Adjust the order of extractions
- Edit the regex easily, or declare the regex separately and call it
  within the function.

### `address_abbreviations` dataset

One common issue with address cleaning is abbreviation of directions,
street names, and street endings. The `address_abbreviations` dataset
assists by providing a resource of common abbreviations.

``` r
slice_head(address_abbreviations, n = 3, by = type)
#> # A tibble: 12 × 3
#>    short long   type                  
#>    <chr> <chr>  <chr>                 
#>  1 N     NORTH  directions            
#>  2 S     SOUTH  directions            
#>  3 E     EAST   directions            
#>  4 ALY   ALLEY  all_street_suffix     
#>  5 ALY   ALLEE  all_street_suffix     
#>  6 ALY   ALLY   all_street_suffix     
#>  7 ALY   ALLEY  official_street_suffix
#>  8 ANX   ANEX   official_street_suffix
#>  9 ARC   ARCADE official_street_suffix
#> 10 1ST   FIRST  numbered_street_name  
#> 11 2ND   SECOND numbered_street_name  
#> 12 3RD   THIRD  numbered_street_name
```

The table has three columns: `short` contains the abbreviation, `long`
contains the full word, and `type` is the category of the
word/abbreviation. There are currently four categories:

- `directions` for N, S, E, W, NE, NW, SE, and SW
- `numbered_street_name` for ordinal street names like 1st, 2nd, or 3rd
  St.
- `all_street_suffix` and `official_street_suffix` are derived primarily
  from the [USPS’s list of officially accepted street
  endings](https://pe.usps.com/text/pub28/28apc_002.htm). Both contain
  the same abbreviated values. For the code behind this chunk, see
  `data-raw/address_abbreviations.R`.
  - `all_street_suffix` contains all accepted spellings of the street
    suffix (one-to-many). Good for catching varied spellings of street
    endings and reducing them to a common abbreviation.
  - `official_street_suffix` contains only the official spellings of the
    un-abbreviated street suffix (one-to-one). Good for expanding
    abbreviated street endings to a common spelling.

Planned future additions: Commonly abbreviated street names (MLK Jr
Blvd).

### `str_collapse_bound()` to create a regex string

Extract directions from an address by using the values in
`address_abbreviations`. To use `str_extract()` or
`extract_remove_squish()`, all directionals need to be flattened into
one string.

The `str_collapse_bound()` function:

- Flattens strings, separating each with an OR (`|`) operator. ex:
  `N|S|E|W`
- Places word boundaries around the strings. Adding the boundaries
  ensures random letters are not extracted from the middle of words. ex:
  `\\b(N|S|E|W)\\b`

To catch both short and long spellings of directions, include both
columns in the function.

``` r
directions <- address_abbreviations |> filter(type == "directions")
directions <- str_collapse_bound(c(directions$short, directions$long))

# values in address_abbreviations are exclusively upper case
address_table <- address_table |> mutate(street_name = toupper(street_name))

address_table |> 
  extract_remove_squish(street_name, direction, directions) |> 
  select(direction, street_name)
#> # A tibble: 3 × 2
#>   direction street_name  
#>   <chr>     <chr>        
#> 1 <NA>      JERSEY AVENUE
#> 2 N         STREET RD    
#> 3 EAST      PIRATE CV
```

The function worked, but pre-directions and post-directionals should
ideally be in separate columns. We can edit the regex pattern to specify
which direction to extract.

``` r
regex_pre_direction <- paste0("^", directions)
regex_post_direction <- paste0(directions, "$")

address_table <- address_table |> 
  extract_remove_squish(street_name, pre_direction, regex_pre_direction) |> 
  extract_remove_squish(street_name, post_direction, regex_post_direction)

address_table |> select(pre_direction, street_name, post_direction)
#> # A tibble: 3 × 3
#>   pre_direction street_name   post_direction
#>   <chr>         <chr>         <chr>         
#> 1 <NA>          JERSEY AVENUE <NA>          
#> 2 N             STREET RD     <NA>          
#> 3 <NA>          PIRATE CV     EAST
```

### Alter strings with `switch_abbreviations()` and `str_replace_names()`

The `switch_abbreviation()` function can be used to abbreviate or
un-abbreviate strings. By default, the function abbreviates strings.

``` r
address_table <- address_table |> 
  mutate(post_direction = switch_abbreviation(post_direction, "directions"))

address_table |> select(pre_direction, street_name, post_direction)
#> # A tibble: 3 × 3
#>   pre_direction street_name   post_direction
#>   <chr>         <chr>         <chr>         
#> 1 <NA>          JERSEY AVENUE <NA>          
#> 2 N             STREET RD     <NA>          
#> 3 <NA>          PIRATE CV     E
```

Street endings can also be extracted using the `address_abbreviations`
dataset. To expand abbreviations, change `method = "short-to-long"`.

``` r
street_suffixes <- address_abbreviations |> filter(type == "all_street_suffix")
street_suffixes <- paste0("(?<!^)", str_collapse_bound(unique(c(street_suffixes$short, street_suffixes$long))), "$")

address_table <- address_table |> 
  extract_remove_squish(street_name, street_suffix, street_suffixes) |> 
  mutate(street_suffix = switch_abbreviation(street_suffix, "all_street_suffix", "short-to-long"))

address_table |> select(street_name, street_suffix)
#> # A tibble: 3 × 2
#>   street_name street_suffix
#>   <chr>       <chr>        
#> 1 JERSEY      AVENUE       
#> 2 STREET      ROAD         
#> 3 PIRATE      COVE
```

The `switch_abbreviation()` function was made to be used with the
`address_abbreviations` table. To use your own input and output, use
`str_replace_names(string, input, output)`.

``` r
address_table |> 
  mutate(state_long = str_replace_names(state, state.abb, state.name)) |>
  select(city, state_short = state, state_long)
#> # A tibble: 3 × 3
#>   city       state_short state_long
#>   <chr>      <chr>       <chr>     
#> 1 Montclair  NJ          New Jersey
#> 2 Cincinnati OH          Ohio      
#> 3 Memphis    TN          Tennessee
```

More to come.
