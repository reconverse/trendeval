#' @export
calculate_rmse <- function(x, ...) UseMethod("calculate_rmse")

#' @aliases calculate_rmse.default
#' @rdname calculate_rmse
#' @export
calculate_rmse.default <- function(x, ...) not_implemented(x)

#' @aliases calculate_rmse.trending_model
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_model <- function(x, data, na.rm = TRUE, as_tibble = TRUE, ...) {
  fitt <- fit(x, data)
  calculate_rmse(fitt, new_data = data, na.rm = na.rm, as_tibble = as_tibble)
}

#' @aliases calculate_rmse.trending_fit
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_fit <- function(x, new_data, na.rm = TRUE, as_tibble = TRUE, ...) {
  if (missing(new_data)) new_data <- get_fitted_data(x)
  pred <- predict(x, new_data, add_pi = FALSE)
  result <- get_result(pred)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "rmse_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_rmse.trending_fit_tbl
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_fit_tbl <- function(x, new_data, na.rm = TRUE, ...) {
  pred <- predict(x, new_data, add_pi = FALSE)
  result <- get_result(pred)
  res <- .mapply(
    FUN = calculate_yardstick,
    dots = list(
      x = result,
      truth = get_response(x)
    ),
    MoreArgs = list(
      estimate = "estimate",
      metric = "rmse_vec",
      na.rm = TRUE,
      as_tibble = TRUE
    )
  )
  do.call(rbind, res)
}

#' @aliases calculate_rmse.trending_predict
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "rmse_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_rmse.trending_predict_tbl
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_predict_tbl <- function(x, na.rm = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "rmse_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_rmse.trending_prediction
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_prediction <- function(x, na.rm = TRUE, ...) {
  calculate_yardstick(
    x,
    truth = x[[get_response(x)]],
    estimate = x[[get_estimate(x)]],
    metric = "rmse_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}


# -------------------------------------------------------------------------

#' @export
calculate_mae <- function(x, ...) UseMethod("calculate_mae")

#' @aliases calculate_mae.default
#' @rdname calculate_mae
#' @export
calculate_mae.default <- function(x, ...) not_implemented(x)

#' @aliases calculate_mae.trending_predict
#' @rdname calculate_mae
#' @export
calculate_mae.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = attr(result, "response"),
    estimate = attr(result, "estimate"),
    metric = "mae_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

# -------------------------------------------------------------------------

#' @export
calculate_rsq <- function(x, ...) UseMethod("calculate_rsq")

#' @aliases calculate_rsq.default
#' @rdname calculate_rsq
#' @export
calculate_rsq.default <- function(x, ...) not_implemented(x)

#' @aliases calculate_rsq.trending_predict
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = attr(result, "response"),
    estimate = attr(result, "estimate"),
    metric = "rsq_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

# -------------------------------------------------------------------------

calculate_yardstick <- function(x, truth, estimate, metric, na.rm, as_tibble) {
  fun <- match.fun(metric)
  f <- make_catcher(fun)
  metric <- sub("_vec", "", metric, perl = TRUE)
  truth <- x[[truth]]
  estimate <- x[[estimate]]
  res <- f(truth = truth, estimate = estimate, na.rm = na.rm)
  if (as_tibble) {
    result <- res$result
    if (is.null(result)) result <- NA_real_
    out <- tibble(
      metric = metric,
      result = result,
      warnings = list(res$warnings),
      errors = list(res$errors)
    )
  } else {
    out <- append(list(metric = metric), res)
  }
  out
}
