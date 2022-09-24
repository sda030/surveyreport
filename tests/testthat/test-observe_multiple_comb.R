testthat::test_that("test_multiple_comb t_test mtcars", {
  library(dplyr)
  x <-
    mtcars %>%
    test_multiple_comb(x = ,
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

testthat::test_that("test_multiple_comb chisq mtcars", {
  library(dplyr)
  x <-
    mtcars %>%
    mutate(across(matches("carb|am|cyl"), ~as.factor(.x))) %>%
    test_multiple_comb(x = ,
                         response = matches("carb|am"),
                         explanatory = matches("cyl"),
                         mu = 0,
                         order = c("1", "0"),
                         alternative = "two-sided",
                         conf_level = .95,
                         test = "chisq"
    )
  testthat::expect_equal(dim(x), c(2, 5))
})

testthat::test_that("test_multiple_comb t_test ex_survey1", {
  library(dplyr)
  x <-
    ex_survey1 %>%
    test_multiple_comb(x = ,
                         response = matches("c_[1-2]"),
                         explanatory = matches("x1_sex"),
                         mu = 0,
                         order = c("Males", "Females"),
                         alternative = "two-sided",
                         conf_level = .95,
                         test = "t-test two-sample"
    )
  testthat::expect_equal(dim(x), c(2, 9))
})
