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

# -------------------------------------------------------------------------

calculate_yardstick_trending_fit <- function(x, new_data, na.rm, as_tibble, metric) {
  if (missing(new_data)) new_data <- get_fitted_data(x)
  pred <- predict(x, new_data, add_pi = FALSE)
  result <- get_result(pred)
  calculate_yardstick(
    x = result,
    truth = get_response(result),
    estimate = get_estimate(result),
    metric = metric,
    na.rm = na.rm,
    as_tibble = as_tibble
  )
}

# -------------------------------------------------------------------------

calculate_yardstick_trending_fit_tbl <- function(x, new_data, na.rm, metric, ...) {
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
      metric = metric,
      na.rm = na.rm,
      as_tibble = TRUE
    )
  )
  do.call(rbind, res)
}

# -------------------------------------------------------------------------

calculate_yardstick_trending_model <- function(x, data, na.rm, as_tibble, metric) {
  fitt <- fit(x, data)
  calculate_yardstick_trending_fit(
    fitt,
    new_data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = metric)
}
