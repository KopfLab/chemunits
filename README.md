
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chemunits

<!-- badges: start -->

[![R-CMD-check](https://github.com/KopfLab/chemunits/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KopfLab/chemunits/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KopfLab/chemunits/graph/badge.svg)](https://app.codecov.io/gh/KopfLab/chemunits)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://chemunits.kopflab.org/)
<!-- badges: end -->

## About

The **chemunits** package extends the functionality of the
**[units](https://r-quantities.github.io/units/)** package, which makes
the incredibly powerful
[udunits-2](https://github.com/Unidata/UDUNITS-2) API available in R, by
implementing two additional features:

- automatic conversion of units generated from arithmetic
  (multiplication, division, etc.) to your preferred units
- automatic scaling of numeric values with units to their ideal SI
  prefixes (nano-, milli-, kilo-, etc.)

This is helpful in all kinds of situations involving units of physical
quantities and is particularly powerful in routine calculations for
chemical/biochemical/geochemical applications (hence the name
“chemunits”). In addition to these features, chemtools also defines
molarity (`M = mol/L`) which is not part of the default
[udunits-2](https://github.com/Unidata/UDUNITS-2) database given its
widespread use in chemical labs and calculations.

Check out the examples below and explore the use of chemunits further in
[this vignette](https://chemunits.kopflab.org/articles/explore.html).

## Installation

You can install the development version of chemunits from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("KopfLab/chemunits")
```

## Show me some code

``` r
library(chemunits)

# let chemunits know which units you prefer for different types of quantities
# (you can always reuse the same list, just set it once at the beginning of a session)
my_units <- c(
  "m", "g", "mol", "g/mol", # length, mass, amount, molecular weight
  "L", "M", "bar", "M/bar", # volume, molarity, pressure, solubility
  "J", "J/mol" # energy, gibbs free energy
)
chemunits_options(preferred_units = my_units, auto_scale_units = my_units)

# create a tibble and run some fun calculations
library(dplyr)
df <- 
  # define 2 compounds
  tibble(
    compound = c("NaOH", "theobromine"),
    mw = set_cu(c(40, 180), "g/mol")
  ) |>
  # set a set of weights for each
  cross_join(
    tibble(weight = set_cu(c(0.01, 100, 10000), "mg"))
  ) |>
  # calculate the amount and concentration if dissolved
  mutate(
    amount = weight / mw,
    vol = set_cu(200, "mL"),
    conc = amount / vol,
    bla = as.numeric(amount)
  )

# see what this looks like in different print formats
options(digits = 3)
df # default tibble output
#> # A tibble: 6 × 7
#>   compound         mw   weight      amount  vol        conc         bla
#>   <chr>       [g/mol]     [mg]      [mmol] [mL]        [mM]       <dbl>
#> 1 NaOH             40     0.01   0.00025    200    0.00125    0.00025  
#> 2 NaOH             40   100      2.5        200   12.5        2.5      
#> 3 NaOH             40 10000    250          200 1250        250        
#> 4 theobromine     180     0.01   0.0000556  200    0.000278   0.0000556
#> 5 theobromine     180   100      0.556      200    2.78       0.556    
#> 6 theobromine     180 10000     55.6        200  278.        55.6

# nicely formatted wih units with the values
df |> knitr::kable()
```

| compound    |            mw |     weight |        amount |        vol |        conc |     bla |
|:------------|--------------:|-----------:|--------------:|-----------:|------------:|--------:|
| NaOH        |  40 \[g/mol\] |  10 \[µg\] |  250 \[nmol\] | 200 \[mL\] | 1.25 \[µM\] |   0.000 |
| NaOH        |  40 \[g/mol\] | 100 \[mg\] |  2.5 \[mmol\] | 200 \[mL\] | 12.5 \[mM\] |   2.500 |
| NaOH        |  40 \[g/mol\] |   10 \[g\] |  250 \[mmol\] | 200 \[mL\] |  1.25 \[M\] | 250.000 |
| theobromine | 180 \[g/mol\] |  10 \[µg\] | 55.6 \[nmol\] | 200 \[mL\] |  278 \[nM\] |   0.000 |
| theobromine | 180 \[g/mol\] | 100 \[mg\] |  556 \[µmol\] | 200 \[mL\] | 2.78 \[mM\] |   0.556 |
| theobromine | 180 \[g/mol\] |   10 \[g\] | 55.6 \[mmol\] | 200 \[mL\] |  278 \[mM\] |  55.556 |

``` r
# with units in the header
df |> make_units_explicit() |> knitr::kable() 
```

| compound    | mw \[g/mol\] | weight \[mg\] | amount \[mmol\] | vol \[mL\] | conc \[mM\] |     bla |
|:------------|-------------:|--------------:|----------------:|-----------:|------------:|--------:|
| NaOH        |           40 |         1e-02 |           0.000 |        200 |       0.001 |   0.000 |
| NaOH        |           40 |         1e+02 |           2.500 |        200 |      12.500 |   2.500 |
| NaOH        |           40 |         1e+04 |         250.000 |        200 |    1250.000 | 250.000 |
| theobromine |          180 |         1e-02 |           0.000 |        200 |       0.000 |   0.000 |
| theobromine |          180 |         1e+02 |           0.556 |        200 |       2.778 |   0.556 |
| theobromine |          180 |         1e+04 |          55.556 |        200 |     277.778 |  55.556 |
