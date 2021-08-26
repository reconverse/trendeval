
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/trendeval)](https://CRAN.R-project.org/package=trendeval)
[![Codecov test
coverage](https://codecov.io/gh/reconverse/trendeval/branch/master/graph/badge.svg)](https://codecov.io/gh/reconverse/trendeval?branch=master)
[![R build
status](https://github.com/reconverse/trendeval/workflows/R-CMD-check/badge.svg)](https://github.com/reconverse/trendeval/actions)
<!-- badges: end -->

# Trendeval

*trendeval* aims to provide a coherent interface for evaluating models
fit with the [trending](https://github.com/reconverse/trending) package.
Whilst it is useful in an interactive context, it’s main focus is to
provide an intuitive interface on which other packages can be developed
(e.g. [*trendbreaker*](https://github.com/reconhub/trendbreaker)).

## Installing the package

You can install the stable version of this package from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("trendeval")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("reconverse/trendeval", build_vignettes = TRUE)
```

# Resources

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue* system](https://github.com/reconverse/trendeval/issues). All
other questions should be posted on the **RECON** slack channel see
<https://www.repidemicsconsortium.org/forum/> for details on how to
join.
