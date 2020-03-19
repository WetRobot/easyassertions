
# easyassertions

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/WetRobot/easyassertions.svg?branch=master)](https://travis-ci.org/WetRobot/easyassertions)
<!-- badges: end -->

easyassertions enables you to write simple, one-line and one-function-call 
assertions.

## Installation

``` r
devtools::install_github("WetRobot/easyassertions")
```

## Example

You would probably use this package to write assertions for your own functions
and possibly your own package. I recommend the following style of using
easyassertion functions:

``` r
# function outside a packge
my_fun <- function(x) {
  requireNamespace("easyassertions")
  easyassertions::assert_is_integer_ltezero_vector(x)
}


# function within a package
#' @importFrom easyassertions assert_is_integer_ltezero_vector
my_fun <- function(x) {
  easyassertions::assert_is_integer_ltezero_vector(x)
}
```

