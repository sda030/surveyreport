#' Return character vector of manually picked data columns.
#'
#' @param data A dataset; tibble or data frame.
#' @param to_clipboard Boolean. Defaults to FALSE. If TRUE, overwrites what you already have copied. Use with caution.
#'
#' @return Character vector. Prints to console.
#' @importFrom utils select.list
#' @importFrom clipr write_clip
#' @export
#'
#' @examples if(interactive()) handpick(mtcars)
handpick <- function(data, to_clipboard = FALSE) {
  x <-
    select.list(colnames(data),
                   multiple=TRUE,
                   title='Pick columns in dataset',
                   graphics=TRUE)
  y <- dput(x)
  if(to_clipboard) {
    clipr::write_clip(content = as.character(deparse(x)), object_type = "character")
    }
  x
}
