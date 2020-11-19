test_that("evaluate_models returns with expected structure", {
  model_constant <- trending::lm_model(hp ~ 1)
  model1 <- trending::glm_model(hp ~ 1 + cyl, poisson)
  model2 <- trending::lm_model(hp ~ 1 + cyl)
  model3 <- trending::glm_nb_model(hp ~ 1 + cyl)
  models <- list(
    null = model_constant,
    glm_poisson = model1,
    lm_trend = model2,
    negbin = model3
  )
  training_data <- mtcars
  results <- evaluate_models(
    models,
    training_data,
    evaluate_resampling, 
    metrics = list(yardstick::rmse, yardstick::huber_loss), 
    v = 2, 
    repeats = 1
  )

  expect_equal(
    colnames(results),
    c("model_name", "model", "data", "warning", "error", "huber_loss", "rmse")
  )
  
  expect_setequal(results$model_name, names(models))


})