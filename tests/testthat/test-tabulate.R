testthat::test_that("apa formatter", {
  library(dplyr)

  testthat::expect_s3_class(object = {

    test <-
      crosstable::crosstable(ex_survey1, b_1:b_3, percent_pattern = "{p_col}", percent_digits = 0) %>%
        # crosstable::pivot_crosstable() %>%
      crosstable_to_apa(label_separator=" - ", font_family = "Times New Roman", topcaption = F)
      }, class = "rdocx", exact = TRUE)

    # x<-withr::with_tempfile(new = "test", code = {
    #   print(test, target = "test.docx")
    # }, fileext = ".docx")

})
