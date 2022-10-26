test_that("prepare_perc_for_mschart", {
  testthat::expect_equal(
    surveyreport:::prepare_perc_for_mschart(
      data = ex_survey1[, paste0("a_", 1:9)],
      digits = 0, percent_sign = FALSE)[1,"data_label"],
    expected = "49")

  testthat::expect_equal(
    surveyreport:::prepare_perc_for_mschart(
      data = ex_survey1[, paste0("a_", 1:9)],
      sort_by = "value", desc = T)[1,4],
    expected = 60)

  testthat::expect_equal(
    surveyreport:::prepare_perc_for_mschart(
      data = ex_survey1[, paste0("b_", 1:3)],
      sort_by = "A bit", desc = FALSE)[5,4],
    expected = 42)

  testthat::expect_equal(
    surveyreport:::prepare_perc_for_mschart(
      data = ex_survey1[, paste0("b_", 1:3)],
      sort_by = c("A bit", "A lot"), desc = FALSE)[9,"sum_value"],
    expected = 60)
})

test_that("prepare_freq_for_mschart", {
  testthat::expect_equal(surveyreport:::prepare_freq_for_mschart(
    data = ex_survey1[, paste0("a_", 1:9)], showNA = "no")[1,"data_label"],
    "49")

  testthat::expect_equal(
    surveyreport:::prepare_perc_for_mschart(
      data = ex_survey1[, paste0("a_", 1:9)],
      sort_by = "value", desc = T)[1,4],
    expected = 60)
})

test_that("report_chart_likert errors", {
  suppressMessages(library(dplyr))
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

    test_docx_a19_no_NA <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, showNA = "no")
    officer:::print.rdocx(test_docx_a19_no_NA, target = "test_docx_a19_no_NA.docx")
    file.remove("test_docx_a19_no_NA.docx")

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


    test_docx_a19_sporring <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, digits = 0L, percent_sign = FALSE, font_family = "Calibri")
    officer:::print.rdocx(test_docx_a19_sporring, target = "test_docx_a19_sporring.docx")
    file.remove("test_docx_a19_sporring.docx")


    test_docx_a19_value_sort <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, sort_by = "value", desc=FALSE, vertical=FALSE, showNA = "no")
    officer:::print.rdocx(test_docx_a19_value_sort, target = "test_docx_a19_value_sort.docx")
    file.remove("test_docx_a19_value_sort.docx")



    test_freq_a19_value_sort <-
      ex_survey1 %>%
      report_chart_likert(cols = a_1:a_9, sort_by = "value", desc=T, vertical=FALSE, showNA = "no",
                          what = "fre")
    officer:::print.rdocx(test_freq_a19_value_sort, target = "test_freq_a19_value_sort.docx")
    file.remove("test_freq_a19_value_sort.docx")


    test_freq_b13_value_sort <-
      ex_survey1 %>%
      report_chart_likert(cols = b_1:b_3, sort_by = "value", desc=T, vertical=FALSE, showNA = "no",
                          what = "fre")
    officer:::print.rdocx(test_freq_b13_value_sort, target = "test_freq_b13_value_sort.docx")

    file.remove("test_freq_b13_value_sort.docx")

    test_b13_value_sort_a_bit <-
      ex_survey1 %>%
      report_chart_likert(cols = b_1:b_3, sort_by = "A bit", desc=FALSE, vertical=FALSE, showNA = "no")
    officer:::print.rdocx(test_b13_value_sort_a_bit, target = "test_b13_value_sort_a_bit.docx")
    file.remove("test_b13_value_sort_a_bit.docx")

    test_b13_value_sort_double <-
      ex_survey1 %>%
      report_chart_likert(cols = b_1:b_3, sort_by = c("A bit", "A lot"), desc=FALSE, vertical=FALSE, showNA = "no")
    officer:::print.rdocx(test_b13_value_sort_double, target = "test_b13_value_sort_double.docx")
    file.remove("test_b13_value_sort_double.docx")

  # testthat::expect_output_file(object =
  #                                officer:::print.rdocx(test_docx8, target = "test8.docx"),
  # file = system.file("template","test8.docx", package = "surveyreport", mustWork = TRUE))
})
