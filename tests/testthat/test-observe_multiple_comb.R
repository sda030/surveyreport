testthat::test_that("t_test_multiple_comb", {
  library(dplyr)
  x <-
    t_test_multiple_comb(x = mtcars,
                        response = matches("mpg|disp"),
                        explanatory = matches("vs"),
                        # null = "point",
                        mu = 0,
                        # stat = "diff in means",
                        order = c("1", "0"),
                        alternative = "two-sided",
                        conf_level = .95#,
                        # ci_type = "se"
                        )
  testthat::expect_equal(dim(x), c(2, 9))
})
