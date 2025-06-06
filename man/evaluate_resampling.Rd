% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_resampling.R
\name{evaluate_resampling}
\alias{evaluate_resampling}
\alias{evaluate_resampling.default}
\alias{evaluate_resampling.trending_model}
\alias{evaluate_resampling.list}
\title{Resampling approach for model evaluation}
\usage{
evaluate_resampling(x, ...)

\method{evaluate_resampling}{default}(x, ...)

\method{evaluate_resampling}{trending_model}(
  x,
  data,
  metric = c("rmse", "rsq", "mae"),
  metric_arguments = list(na.rm = TRUE),
  v = 5,
  repeats = 1,
  ...
)

\method{evaluate_resampling}{list}(
  x,
  data,
  metric = c("rmse", "rsq", "mae"),
  metric_arguments = list(na.rm = TRUE),
  v = 5,
  repeats = 1,
  ...
)
}
\arguments{
\item{x}{An \R object.}

\item{...}{Not currently used.}

\item{data}{a \code{data.frame} containing data (including the response variable
and all predictors) used in the specified model.}

\item{metric}{One of "rmse" (see \link{calculate_rmse}), "mae" (see
\link{calculate_mae}) and "rsq" (see \link{calculate_rsq}).}

\item{metric_arguments}{A named list of arguments passed to the underlying
functions that calculate the metrics.}

\item{v}{the number of equally sized data partitions to be used for K-fold
cross-validation; \code{v} cross-validations will be performed, each using \code{v - 1} partition as training set, and the remaining partition as testing set.
Defaults to the number of row in data, so that the method uses
leave-one-out cross validation, akin to Jackknife except that the testing
set (and not the training set) is used to compute the fit statistics.}

\item{repeats}{the number of times the random K-fold cross validation should
be repeated for; defaults to 1; larger values are likely to yield more
reliable / stable results, at the expense of computational time}
}
\description{
\code{\link[=evaluate_resampling]{evaluate_resampling()}} uses repeated K-fold cross-validation and
the Root Mean Square Error (RMSE) of testing sets to measure the predictive
power of a single model. Methods are provided for
\code{\link[trending:trending_model]{trending::trending_model}} (and lists of these) objects.
}
\details{
These functions wrap around existing functions from several
packages.  \code{\link[=evaluate_resampling.trending_model]{evaluate_resampling.trending_model()}} and
\code{\link[=evaluate_resampling.list]{evaluate_resampling.list()}} both use \code{\link[rsample:vfold_cv]{rsample::vfold_cv()}} for sampling
and, for the calculating the different metrics, the
\href{https://yardstick.tidymodels.org/}{yardstick} package.
}
\examples{
x <- rnorm(100, mean = 0)
y <- rpois(n = 100, lambda = exp(x + 1))
dat <- data.frame(x = x, y = y)
model <- trending::glm_model(y ~ x, poisson)
models <- list(
  poisson_model = trending::glm_model(y ~ x, poisson),
  linear_model = trending::lm_model(y ~ x)
)

evaluate_resampling(model, dat)
evaluate_resampling(models, dat)

}
\seealso{
\code{\link[=calculate_aic]{calculate_aic()}}, \code{\link[=calculate_rmse]{calculate_rmse()}}, \code{\link[=calculate_mae]{calculate_mae()}} and
\code{\link[=calculate_rsq]{calculate_rsq()}}.
}
