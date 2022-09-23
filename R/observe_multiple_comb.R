
#' Extention to the infer package for multiple testing
#'
#' @inheritParams infer::t_test
#'
#' @importFrom dplyr mutate across
#' @importFrom tidyselect eval_select all_of
#' @importFrom tidyr drop_na
#' @importFrom labelled remove_val_labels
#' @importFrom broom tidy
#' @importFrom purrr map_dfr
#' @importFrom rlang set_names enquo
#' @importFrom infer t_test
#' @return A tibble containing for each response-explanatory row combination
#' columns summarizing the test statistic as obtained from broom::tidy().
#' @export
#'
#' @examples
#' library(dplyr)
#' x <- t_test_multiple_comb(x = mtcars,
#'                      response = matches("mpg|disp|drat|hp|qsec"),
#'                      explanatory = matches("vs|am"))
t_test_multiple_comb <-
  function(x,
           formula = NULL,
           response = NULL,
           explanatory = NULL,
           order = NULL,
           alternative = "two-sided",
           mu = 0,
           conf_int = TRUE,
           conf_level = 0.95
  ) {

    response_cols <- tidyselect::eval_select(rlang::enquo(response), x)

    explanatory_cols <- tidyselect::eval_select(rlang::enquo(explanatory), x)

    x <-
      dplyr::mutate(x, across(c({{response}}, {{explanatory}}),
                              ~labelled::remove_val_labels(.x)))

    resp_cols <- names(response_cols)
    resp_cols <- rlang::set_names(resp_cols)
    expl_cols <- names(explanatory_cols)
    expl_cols <- rlang::set_names(expl_cols)

    purrr::map_dfr(.x = resp_cols,
                   .id = "response",
                   .f = function(response) {

                     resp_col <- rlang::sym(response)

                     purrr::map_dfr(.x = expl_cols,
                     .id = "explanatory",
                     .f = function(explanatory) {

                       expl_col <- rlang::sym(explanatory)
                       # x <-
                       #   tidyr::drop_na(x, response_cols, explanatory_cols)

        infer::t_test(x = x,
                      response = !!resp_col,
                      explanatory = !!expl_col,
                      order = order,
                      alternative = alternative,
                      mu = mu,
                      conf_int = conf_int,
                      conf_level = conf_level)
          })
      })
  }
