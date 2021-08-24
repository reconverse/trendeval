#' @export
calculate_mae <- function(x, ...) UseMethod("calculate_mae")

#' @aliases calculate_mae.default
#' @rdname calculate_mae
#' @export
calculate_mae.default <- function(x, ...) not_implemented(x)

#' @aliases calculate_mae.trending_model
#' @rdname calculate_mae
#' @export
calculate_mae.trending_model <- function(x, data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_model(
    x = x,
    data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "mae_vec"
  )
}

#' @aliases calculate_mae.trending_fit
#' @rdname calculate_mae
#' @export
calculate_mae.trending_fit <- function(x, new_data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_fit(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "mae_vec"
  )
}

#' @aliases calculate_mae.trending_fit_tbl
#' @rdname calculate_mae
#' @export
calculate_mae.trending_fit_tbl <- function(x, new_data, na.rm = TRUE, ...) {
  calculate_yardstick_trending_fit_tbl(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    metric = "mae_vec"
  )
}

#' @aliases calculate_mae.trending_predict
#' @rdname calculate_mae
#' @export
calculate_mae.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "mae_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_mae.trending_predict_tbl
#' @rdname calculate_mae
#' @export
calculate_mae.trending_predict_tbl <- function(x, na.rm = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "mae_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_mae.trending_prediction
#' @rdname calculate_mae
#' @export
calculate_mae.trending_prediction <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick(
    x,
    truth = get_response(x),
    estimate = get_estimate(x),
    metric = "mae_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}
