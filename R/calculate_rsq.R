#' @export
calculate_rsq <- function(x, ...) UseMethod("calculate_rsq")

#' @aliases calculate_rsq.default
#' @rdname calculate_rsq
#' @export
calculate_rsq.default <- function(x, ...) not_implemented(x)

#' @aliases calculate_rsq.trending_model
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_model <- function(x, data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_model(
    x = x,
    data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rsq_vec"
  )
}

#' @aliases calculate_rsq.trending_fit
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_fit <- function(x, new_data, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick_trending_fit(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = "rsq_vec"
  )
}

#' @aliases calculate_rsq.trending_fit_tbl
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_fit_tbl <- function(x, new_data, na.rm = TRUE, ...) {
  calculate_yardstick_trending_fit_tbl(
    x = x,
    new_data = new_data,
    na.rm = na.rm,
    metric = "rsq_vec"
  )
}

#' @aliases calculate_rsq.trending_predict
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_predict <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "rsq_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_rsq.trending_predict_tbl
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_predict_tbl <- function(x, na.rm = TRUE, ...) {
  result <- trending::get_result(x)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = "rsq_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}

#' @aliases calculate_rsq.trending_prediction
#' @rdname calculate_rsq
#' @export
calculate_rsq.trending_prediction <- function(x, na.rm = TRUE, as_tibble = TRUE, ...) {
  calculate_yardstick(
    x,
    truth = get_response(x),
    estimate = get_estimate(x),
    metric = "rsq_vec",
    na.rm = TRUE,
    as_tibble = as_tibble
  )
}
