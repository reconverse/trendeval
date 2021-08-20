#' Generic for calculating the AIC
#'
#' Generic `calculate_aic()` returns the Akaike's 'An Information Criterion' for
#'   the given input.
#'
#' @param x An \R object.
#' @param ... Not currently used.
#'
#' @details Specific methods are given for [`trending_fit`][trending::fit()]
#'   objects and lists of these models. The default method applies
#'   [stats::AIC()] directly.
#'
#' @return For a single [`trending_fit`][trending::fit()] input, the object
#'   returned will be a list with entries:
#'
#'   - metric: "AIC"
#'   - result: the resulting AIC value fit (NULL if the calculation failed)
#'   - warnings: any warnings generated during calculation
#'   - errors: any errors generated during calculation
#'
#'   For a list of trending fit inputs a [tibble][tibble::tibble()] with one
#'   row for each fitted model and corresponding to output generated with a
#'   single model input.
#'
#' @author Tim Taylor
#'
#' #' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#'
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm.nb_model(y ~ x)
#' fitted_model <- fit(poisson_model, dat)
#' fitted_models <- lapply(c(poisson_model, negbin_model), fit, data = dat)
#'
#' calculate_aic(poisson_model, dat)
#' calculate_aic(fitted_models)
#'
#' calculate_aic(fitted_model)
#'
#' @export
calculate_aic <- function(x, ...) {
  UseMethod("calculate_aic")
}

# -------------------------------------------------------------------------

#' @aliases calculate_aic.default
#' @rdname calculate_aic
#' @export
calculate_aic.default <- function(x, ...) {
  stats::AIC(x)
}

# -------------------------------------------------------------------------


#' @aliases calculate_aic.trending_model
#' @rdname calculate_aic
#' @export
calculate_aic.trending_fit <- function(x, ...) {
  fitted_model <- trending::get_fitted_model(x)
  f <- make_catcher(stats::AIC)
  res <- f(fitted_model)
  append(list(metric = "aic"), res)
}


#' @aliases calculate_aic.list
#' @rdname calculate_aic
#' @export
calculate_aic.list <- function(x, data, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_fit"))) {
    stop("list entrys should be `trending_fit` objects", call. = FALSE)
  }
  res <- lapply(x, calculate_aic.trending_fit)
  res <- lapply(res, function(x) tibble::tibble(x[[1]], list(x[[2]]), list(x[[3]]), list(x[[4]])))
  res <- do.call(rbind, res)
  res <- setNames(res, c("metric", "result", "warnings", "errors"))
  model_name <- names(x)
  if (!is.null(model_name)) {
    res <- cbind(model_name, res)
  }
  tidyr::unnest(res, cols = c(metric, result))
}


