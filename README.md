
# degSets

<!-- badges: start -->
[![R-CMD-check](https://github.com/CECADBioinformaticsCoreFacility/degSets/workflows/R-CMD-check/badge.svg)](https://github.com/CECADBioinformaticsCoreFacility/degSets/actions)
[![Codecov test coverage](https://codecov.io/gh/CECADBioinformaticsCoreFacility/degSets/branch/master/graph/badge.svg)](https://app.codecov.io/gh/CECADBioinformaticsCoreFacility/degSets?branch=master)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of degSets is to ...

## Installation

You can install the development version of degSets from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("CECADBioinformaticsCoreFacility/degSets")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(degSets)
degSets::run_app()
```

## Hosted Instance

You can find an example instance of degSets hosted at [shinyapps.io](https://richardjacton.shinyapps.io/degSets/)


## TODO

- [ ] BUG upset selection and highlighting not working clicking one sometimes selects another, venn selection works fine but subsets are not highlighted. (following change to distinct sets)

- [ ] update on button press not immediately when p-value, LFC thresholds are changed
- [ ] better solution to white/black font colour change in cell background colour
- [ ] test coverage for other main functions in modsets functions
