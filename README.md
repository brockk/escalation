dose
================
Kritian Brock

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/brockk/dose.svg?branch=master)](https://travis-ci.org/brockk/dose)
<!-- badges: end -->

# Overview

`dose` provides a grammar for dose-finding clinical trials.

It starts by providing functions to use dose-escalation methodologies
like CRM, BOIN, and 3+3. Largely, model-fitting code is imported from
existing packages:

  - `get_dfcrm()` uses `dfcrm`
  - `get_boin()` uses `BOIN`
  - there is also `get_three_plus_three()`

These approaches are then fit to trial outcomes to produce dose selector
objects that support a common interface. The two most important methods
are `recommended_dose()` to get the current dose selection, and
`continue()` to learn whether the model advocates continuing patient
recruitment.

With a little bit of plumbing, a dose-selection function could be
imported from practically any dose-finding package in R and made to act
the same way.

`dose` then adds optional embellishments to provide extra desirable
behaviour:

  - `dont_skip_doses()`
  - `stop_at_n()`
  - `stop_when_n_at_dose()`
  - `stop_when_too_toxic()`
  - `demand_n_at_dose()`

Each of these functions overrides the way doses are selected or when a
design decides to stop. A powerful feature of `dose` is that all of
these objects supports exactly the same interface, so they can be
daisy-chained together using `%>%` operator from the `magrittr` package
to create dose-selectors that perform exactly how you want.

Furthermore, having defined this flexible interface for creating
dose-finding designs, it is simple to run simulations or calculate
dose-pathways for future cohorts of patients.

See Examples.

# Installation

``` r
# Install the latest official version from CRAN with:
install.packages("dose")

# Alternatively, install the latest code from GitHub:
devtools::install_github("brockk/dose")
```

# Usage

``` r
library(dose)
```

    ## Loading required package: magrittr

TODO

# Getting help

TODO
