testthat::test_that("observe_multiple_comb", {
  library(dplyr)
  x <-
  observe_multiple_comb(x = mtcars,
                        response = matches("mpg|disp|drat|hp|qsec"),
                        explanatory = matches("vs|am"), stat = "diff in means") %>%
    select(outcome, predictor, estimate1, estimate2, p.value)
  testthat::expect_equal(dim(x), c(10, 5))
})
