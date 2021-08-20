#' @export
calculate_yardstick_metric <- function(x, ...) {
  UseMethod("calculate_yardstick_metric")
}

# -------------------------------------------------------------------------

#' @aliases calculate_yardstick_metric.default
#' @rdname calculate_yardstick_metric
#' @export
calculate_yardstick_metric.default <- function(x, ...) {
  not_implemented(x)
}

# -------------------------------------------------------------------------


#' @aliases calculate_yardstick_metric.trending_model
#' @rdname calculate_yardstick_metric
#' @export
calculate_yardstick_metric.trending_predict <- function(x, metrics = c("rmse", "mae", "rsq"), ...) {
  metrics <- match.arg(metrix, several.ok = TRUE)
  pred <- trending::get_result(x)
  f <- make_catcher(stats::AIC)
  res <- f(fitted_model)
  append(list(metric = "aic"), res)
}


calculate_yardstick <- function(x, truth, estimate, metric, na.rm) {
  truth <- x[[truth]]
  estimate <- x[[estimate]]
  result <- switch (
    metric,
    rmse = yardstick::rmse_vec(truth = truth, estimate = estimate, na.rm = na.rm),
    mae = yardstick::mae_vec(truth = truth, estimate = estimate, na.rm = na.rm),
    rsq = yardstick::rsq_vec(truth = truth, estimate = estimate, na.rm = na.rm),
    stop("Invalid metric")
  )
  tibble::tibble(metric, result)
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

