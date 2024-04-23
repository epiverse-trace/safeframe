
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->

# *datatagr*: Generic Data Tagging and Validating <img src="man/figures/logo.svg" align="right" width="120" alt="Logo for datatagr" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![R-CMD-check](https://github.com/epiverse-trace/datatagr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/datatagr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/datatagr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/datatagr?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)

<!-- badges: end -->

**datatagr** provides functions to tag, validate, and safeguard data of
any kind. datatagr is an abstraction from **linelist**, which applies
these principles for epidemiological data. The original proposal for
this package can be found on [the Discussion
board](https://github.com/orgs/epiverse-trace/discussions/221).

> \![INFO\] For our project management and roadmap, please [see the
> relevant GitHub
> Project](https://github.com/orgs/epiverse-trace/projects/41).

## Installation

You can install the development version of datatagr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("epiverse-trace/datatagr")
```

## Example

These examples illustrate some of the current functionalities

## Development

### Lifecycle

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/datatagr/pulls).

### Related projects

This project is related to other existing projects in R or other
languages, but also differs from them in the following aspects:

### Code of Conduct

Please note that the datatagr project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
