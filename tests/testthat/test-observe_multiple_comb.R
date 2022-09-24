testthat::test_that("t_test_multiple_comb", {
  library(dplyr)
  x <-
    mtcars %>%
    # mutate(carb = as.factor(carb)) %>%
    t_test_multiple_comb(x = ,
                        response = matches("mpg|disp"),
                        explanatory = matches("vs"),
                        mu = 0,
                        order = c("1", "0"),
                        alternative = "two-sided",
                        conf_level = .95,
                        test = "t-test two-sample"
                        )
  testthat::expect_equal(dim(x), c(2, 9))
})

testthat::test_that("t_test_multiple_comb", {
  library(dplyr)
  x <-
    mtcars %>%
    mutate(across(matches("carb|am|cyl"), ~as.factor(.x))) %>%
    t_test_multiple_comb(x = ,
                         response = matches("carb|am"),
                         explanatory = matches("cyl"),
                         mu = 0,
                         order = c("1", "0"),
                         alternative = "two-sided",
                         conf_level = .95,
                         test = "chi-square"
    )
  testthat::expect_equal(dim(x), c(2, 5))
})
