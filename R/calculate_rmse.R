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
  calculate_yardstick_trending_model(
    x = x,
    data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rmse_vec"
  )
}

#' @aliases calculate_rmse.trending_fit
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_fit <- function(x, new_data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_fit(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rmse_vec"
  )
}

#' @aliases calculate_rmse.trending_fit_tbl
#' @rdname calculate_rmse
#' @export
calculate_rmse.trending_fit_tbl <- function(x, new_data, na.rm = TRUE, ...) {
  calculate_yardstick_trending_fit_tbl(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    metric = "rmse_vec"
  )
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
calculate_rmse.trending_prediction <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick(
    x,
    truth = get_response(x),
    estimate = get_estimate(x),
    metric = "rmse_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}
