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
  test_docx_b13 <-
    ex_survey1 %>%
    report_chart_likert(cols = b_1:b_3,
                        docx_template = docx_template,
                        colour_palette = colour_palette,
                        chart_formatting = chart_format,
                        height_per_col = .3,
                        height_fixed = 1)
  }, class = "rdocx", exact = TRUE)
  officer:::print.rdocx(test_docx_b13, target = "test_docx_b13.docx")
  file.remove("test_docx_b13.docx")

  testthat::expect_error(object = report_chart_likert(mtcars, cols = c(cyl, vs, gear, carb)),
                         regexp = "Column `cyl` and column `vs` lack common categories")
  testthat::expect_error(object = report_chart_likert(ex_survey1, cols = tidyselect::matches("^[ab]")),
                         regexp = "Column `a_1` and column `b_1` lack common categories")
  test_docx_a19 <-
    ex_survey1 %>%
    report_chart_likert(cols = a_1:a_9)
    officer:::print.rdocx(test_docx_a19, target = "test_docx_a19.docx")
    file.remove("test_docx_a19.docx")

    test_docx_p14_p4NA <-
      ex_survey1 %>%
      mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly disagree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4)
    officer:::print.rdocx(test_docx_p14_p4NA, target = "test_docx_p14_p4NA.docx")
    file.remove("test_docx_p14_p4NA.docx")


    test_docx_p14_p1NA <- # Th dangerous one
      ex_survey1 %>%
      mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Somewhat disagree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4,
                          docx_template = docx_template,
                          colour_palette = colour_palette,
                          chart_formatting = chart_format,
                          height_per_col = .3,
                          height_fixed = 1)
    officer:::print.rdocx(test_docx_p14_p1NA, target = "test_docx_p14_p1NA.docx")
    file.remove("test_docx_p14_p1NA.docx")


    test_docx_p14_p4NA2 <-
      ex_survey1 %>%
      mutate(across(p_4, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4)
    officer:::print.rdocx(test_docx_p14_p4NA2, target = "test_docx_p14_p4NA2.docx")
    file.remove("test_docx_p14_p4NA2.docx")


    test_docx_p14_p1NA2 <-
      ex_survey1 %>%
      mutate(across(p_1, ~forcats::fct_recode(.x, NULL = "Strongly agree"))) %>%
      labelled::copy_labels_from(from = ex_survey1) %>%
      report_chart_likert(cols = p_1:p_4)
    officer:::print.rdocx(test_docx_p14_p1NA2, target = "test_docx_p14_p1NA2.docx")
    file.remove("test_docx_p14_p1NA2.docx")


  # testthat::expect_output_file(object =
  #                                officer:::print.rdocx(test_docx8, target = "test8.docx"),
  # file = system.file("template","test8.docx", package = "surveyreport", mustWork = TRUE))
})