
#' Extention to the infer package for multiple testing
#'
#' @inheritParams infer::specify
#' @inheritParams infer::hypothesize
#' @inheritParams infer::calculate
#' @importFrom dplyr mutate across
#' @importFrom tidyselect eval_select
#' @importFrom tidyr drop_na
#' @importFrom labelled remove_val_labels
#' @importFrom broom tidy
#' @importFrom purrr map_dfr
#' @importFrom rlang set_names enquo
#' @importFrom stats t.test
#' @return A tibble containing for each response-explanatory row combination
#' columns summarizing the test statistic as obtained from broom::tidy().
#' @export
#'
#' @examples
observe_multiple_comb <-
  function(x,
           # specify arguments

           formula = NULL,
           response = NULL,
           explanatory = NULL,
           success = NULL,
           null = NULL,
           p = NULL,
           mu = NULL,
           med = NULL,
           sigma = NULL,
           stat=
             c("mean", "median", "sum", "sd", "prop", "count", "diff in means",
               "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z",
               "ratio of props", "odds ratio")
  ) {

    response_cols <- tidyselect::eval_select(enquo(response), x)
    explanatory_cols <- tidyselect::eval_select(enquo(explanatory), x)
    x <-
      tidyr::drop_na(x, response_cols, explanatory_cols)
    x <-
      dplyr::mutate(x, across(c({{response}}, {{explanatory}}),
                              ~labelled::remove_val_labels(.x)))
    if(
      stat %in% c("means",
                  "diff in medians", "diff in props", "Chisq", "ratio of props")) {
      x <-
        dplyr::mutate(x, across({{explanatory}}, ~as.factor(.x)))
    }

    out <-
    names(response_cols)
    out <- rlang::set_names(out)
    purrr::map_dfr(out, .id = "outcome", .f = function(outcome) {
      out <- names(explanatory_cols)
      out <- rlang::set_names(out)
      purrr::map_dfr(out, .id = "predictor", .f = function(predictor) {
        out <-
            t.test(x[[outcome]], x[[predictor]])
              broom::tidy(out)
          })
      })
  }
