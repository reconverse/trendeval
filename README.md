
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/trendeval)](https://CRAN.R-project.org/package=trendeval)
[![Codecov test
coverage](https://codecov.io/gh/reconhub/trendeval/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/trendeval?branch=master)
[![R build
status](https://github.com/reconhub/trendeval/workflows/R-CMD-check/badge.svg)](https://github.com/reconhub/trendeval/actions)
<!-- badges: end -->

<br> **<span style="color: red;">Disclaimer</span>**

This package is a work in progress. Please reach out to the authors
before using.

# Trendeval

*trendeval* aims to provide a coherent interface for evaluating models
fit with the [trending](https://github.com/reconhub/trending) package.
Whilst it is useful in an interactive context, it’s main focus is to
provide an intuitive interface on which other packages can be developed
(e.g. [*trendbreaker*](https://github.com/reconhub/trendbreaker)).

## Installing the package

Once it is released on [CRAN](https://CRAN.R-project.org), you will be
able to install the stable version of the package with:

``` r
install.packages("trendeval")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("reconhub/trendeval", build_vignettes = TRUE)
```

## Model selection example

``` r
library(dplyr)      # for data manipulation
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(outbreaks)  # for data
library(trending)   # for trend fitting
library(trendeval)  # for model selection

# load data
data(covid19_england_nhscalls_2020)

# define a model
models  <- list(
  simple = lm_model(count ~ day),
  glm_poisson = glm_model(count ~ day, family = "poisson"),
  glm_negbin = glm_nb_model(count ~ day + weekday),
  will_error = glm_nb_model(count ~ day + nonexistant)
)

# select 6 weeks of data (from a period when the prevalence was decreasing)
last_date <- as.Date("2020-05-28")
first_date <- last_date - 8*7
pathways_recent <-
  covid19_england_nhscalls_2020 %>%
  filter(date >= first_date, date <= last_date) %>%
  group_by(date, day, weekday) %>%
  summarise(count = sum(count), .groups = "drop")

# split data for fitting and prediction
dat <-
  pathways_recent %>%
  group_by(date <= first_date + 6*7) %>%
  group_split()
fitting_data <- dat[[2]]
pred_data <- select(dat[[1]], date, day, weekday)


out <- capture.output( # no log output in readme :)
  results <- evaluate_models(
    fitting_data, 
    models,
    method = evaluate_resampling,
    metrics = list(yardstick::rmse, yardstick::huber_loss, yardstick::mae)
  )
)

results
#> # A tibble: 10 x 7
#>    model_name  model       data            metric    score warning    error     
#>    <chr>       <named lis> <list>          <chr>     <dbl> <named li> <named li>
#>  1 simple      <trndng_l>  <tibble [43 × … huber_lo… 6902. <NULL>     <NULL>    
#>  2 simple      <trndng_l>  <tibble [43 × … mae       6903. <NULL>     <NULL>    
#>  3 simple      <trndng_l>  <tibble [43 × … rmse      6903. <NULL>     <NULL>    
#>  4 glm_poisson <trndng_g>  <tibble [43 × … huber_lo… 5193. <NULL>     <NULL>    
#>  5 glm_poisson <trndng_g>  <tibble [43 × … mae       5193. <NULL>     <NULL>    
#>  6 glm_poisson <trndng_g>  <tibble [43 × … rmse      5193. <NULL>     <NULL>    
#>  7 glm_negbin  <trndng__>  <tibble [43 × … huber_lo… 5223. <NULL>     <NULL>    
#>  8 glm_negbin  <trndng__>  <tibble [43 × … mae       5224. <NULL>     <NULL>    
#>  9 glm_negbin  <trndng__>  <tibble [43 × … rmse      5224. <NULL>     <NULL>    
#> 10 will_error  <trndng__>  <tibble [43 × … <NA>        NA  <NULL>     <chr [1]>
```

# Resources

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue* system](https://github.com/reconhub/trendeval/issues). All
other questions should be posted on the **RECON** slack channel see
<https://www.repidemicsconsortium.org/forum/> for details on how to
join.
