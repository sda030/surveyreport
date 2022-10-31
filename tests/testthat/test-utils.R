testthat::test_that("get_colour_set", {
  testthat::expect_warning(
    x <-
    get_colour_set(n_colours_needed = 4,
                   user_colour_set = c("#444444", "#dddddd", "#123414"),
                   seed = 1),
    regexp = "Fewer colours provided in colour palette than needed. Defaulting to RColorBrewer")
  testthat::expect_equal(x, c("#CAB2D6", "#33A02C", "#FDBF6F", "#A6CEE3"))

  testthat::expect_no_message(
    x <-
      get_colour_set(n_colours_needed = 3,
                     user_colour_set = c("#444444", "#dddddd", "#123414"),
                     seed = 1))
  testthat::expect_equal(x, c("#444444", "#dddddd", "#123414"))


  testthat::expect_warning(
    x <-
      get_colour_set(n_colours_needed = 14,
                     user_colour_set = c("#444444", "#dddddd", "#123414"),
                     seed = 1),
    regexp = "Fewer colours provided in colour palette than needed. Defaulting to RColorBrewer")
  testthat::expect_equal(x, c("#440154FF", "#481D6FFF", "#453581FF", "#3D4D8AFF",
                              "#34618DFF", "#2B748EFF", "#24878EFF", "#1F998AFF",
                              "#25AC82FF", "#40BC72FF", "#67CC5CFF", "#97D83FFF",
                              "#CBE11EFF", "#FDE725FF"))

})
