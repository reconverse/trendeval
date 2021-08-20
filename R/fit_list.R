#' Fit a list of trending models
#'
#' `fit_list()` will return the output from fitting a list of models to the
#'   specified data.
#'
#' @param models A list of [`trending_model`] object (i.e. a list of the output
#'   from functions `lm_model`, `glm_model`, `glm.nb_model`, or `brm_model`).
#' @param data A data frame containing the data to fit.
#' @param ... Not currently used.
#'
#' @return A `trending_fit_list` object which is a [`tibble`][tibble::tibble()]
#'   subclass with with one row for each fitted model and columns:
#'
#'   - model_name (optional - only if input `models` is a named list)
#'
#'   - result: the resulting fit from calling the underlying model
#'     directly, i.e.
#'
#'       - `lm_model`: a fitted model object of class [`lm`][stats::lm()]
#'
#'       - `glm_model`: a fitted model object of class [`glm`][stats::glm()]
#'
#'       - `glm.nb_model`: a fitted model object of class [`negbin`][MASS::glm.nb()]
#'
#'       - `brm_model`: An object of class [`brmsfit`][brms::brm()]
#'
#'     `NULL` if fitting fails.
#'
#'   - warnings: any warnings generated during fitting
#'
#'   - errors: any errors generated during fitting
#'
#'
#' @author Tim Taylor
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#'
#' poisson_model <- glm_model(y ~ x , family = poisson)
#' negbin_model <- glm.nb_model(y ~ x)
#'
#' fit_list(list(poisson_model, negbin_model), dat)
#' fit_list(list(pm = poisson_model, nm = negbin_model), dat)
#'
#' @export
fit_list<- function(models, data) {
  stopifnot(is.list(models))
  if (!all(vapply(models, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  qfun <- bquote(lapply(models, fit, data = .(substitute(data))))
  res <- eval(qfun)
  res <- lapply(seq_along(res[[1]]), function(x) lapply(res, "[[", x))
  res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
  nms <- names(models)
  model_name <- NULL
  if (!is.null(nms)) {
    res <- cbind(tibble(model_name = nms), res)
    model_name <- "model_name"
  }
  res <- new_tibble(
    res,
    model_name = model_name,
    result = "result",
    warnings = "warnings",
    errors = "errors",
    nrow = nrow(res),
    class = "trending_fit_list"
  )
  validate_tibble(res)
}
