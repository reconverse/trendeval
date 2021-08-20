# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_fit_list",
    method = dplyr_reconstruct_trending_fit_list
  )
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_predict_list",
    method = dplyr_reconstruct_trending_predict_list
  )
  invisible()
}
# nocov end
