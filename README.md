
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chemunits

<!-- badges: start -->

[![R-CMD-check](https://github.com/KopfLab/chemunits/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KopfLab/chemunits/actions/workfl%20ows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KopfLab/chemunits/graph/badge.svg)](https://app.codecov.io/gh/KopfLab/chemunits)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://chemunits.kopflab.org/)
<!-- badges: end -->

## About

The **chemunits** package expands the functionality of the
**[units](https://r-quantities.github.io/units/)** package for routine
calculations in chemical/biochemical/geochemical applications by
implementing two additional features:

- automatic convertion of

The **[microbialkitchen](https://microbialkitchen.kopflab.org/)**
package is a collection of tools to simplify working with the chemical
composition and speciation of defined culture media for microbial
physiology and environmental microbiology research. It includes a wide
range of general purpose functionality for chemical applications
including built-in, data-frame-compatible [chemical
quantities](https://microbialkitchen.kopflab.org/articles/quantities.html)
(volume, mass, molarity, temperature, pressure, etc.) that automatically
keep track of their units and metric scaling, as well as more
specialized tools for the assembly and comparison of culturing media
recipes, pH buffering strategies and aqueous speciation. All basic data
types and operations are fully implemented, documented and ready to use.
However, since the package is still in active development and some
syntax and function names may change.

<https://r-quantities.github.io/units/>

<https://github.com/Unidata/UDUNITS-2>

The goal of chemunits is to …

NOTE: this package has some strong opinions about what the important
default units are for chemical applications and which units should be
auto_scaled but this is very easy to change with the
`chemunits_options()` if you have different preferences

## Installation

You can install the development version of chemunits from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("KopfLab/chemunits")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(chemunits)
#> Loading required package: units
#> udunits database from /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/units/share/udunits/udunits2.xml
## basic example code
```
