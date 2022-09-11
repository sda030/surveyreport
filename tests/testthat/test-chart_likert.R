test_that("report_chart_likert errors", {
  library(dplyr)
  library(labelled)
  library(tibble)
  library(readxl)


  docx_template <- system.file("template","NIFUmal_tom.docx", package = "surveyreport", mustWork = TRUE)
  colour_palette <-
    readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx", package = "surveyreport", mustWork = TRUE),
                       sheet = "NIFUblue") %>%
    dplyr::pull(hex)
  chart_format <-
  readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx", package = "surveyreport", mustWork = TRUE), sheet = 1) %>%
    dplyr::filter(surveyreport_style == "figure") %>%
    dplyr::pull(template_style)

  testthat::expect_s3_class(object = {
  test_docx8 <-
    ex_survey1 %>%
    dplyr::select(b_1:b_3) %>%
    report_chart_likert(cols = tidyselect::everything(),
                        docx_template = docx_template,
                        colour_palette = colour_palette,
                        chart_formatting = chart_format,
                        height_per_col = .3,
                        height_fixed = 1)
  }, class = "rdocx", exact = TRUE)

  testthat::expect_error(object = report_chart_likert(mtcars, cols = c(cyl, vs, gear, carb)),
                         regexp = "Column `cyl` and column `vs` lack common categories")
  testthat::expect_error(object = report_chart_likert(ex_survey1, cols = tidyselect::matches("^[ab]")),
                         regexp = "Column `a_1` and column `b_1` lack common categories")
  ex_survey1 %>%
    select(a_1:a_9) %>%
    mutate(across(.fns = ~dplyr::recode_factor(.x, .missing = "NA"))) %>%
    report_chart_likert() %>%
    officer:::print.rdocx(target = "tmp.docx")

  # testthat::expect_output_file(object =
  #                                officer:::print.rdocx(test_docx8, target = "test8.docx"),
  # file = system.file("template","test8.docx", package = "surveyreport", mustWork = TRUE))
})
