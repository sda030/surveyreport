#' Rename Dataset Columns by Labels.
#'
#' Occasionally dataframe columns have not been named logically and consistent in the
#' software where the data originates. This function renames variable names
#' based on patterns in the variable labels, after ignoring some stop words.
#'
#' @param data Dataset.
#' @param label_sep The separator between group part and unique part of label.
#' @param sort_var When numbering variables within a group, what to sort by? pos is original position in dataset,
#' @param new_var_sep When creating new variables, how to glue together variable group name prefix and numbering?
#' @param stop_words Words to ignore in label when abbreviating label to name.
#'
#' @return Data with renamed variable names.
#' @importFrom dplyr arrange group_by ungroup mutate rename_with pull n
#' @importFrom vctrs as_list_of
#' @importFrom labelled lookfor
#' @importFrom tidyr separate unite
#' @importFrom purrr map_chr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' rename_by_labels(ex_survey1)
rename_by_labels <-
	function(data,
			 label_sep=" - ",
			 sort_var=c("pos", "variable", "label"),
			 new_var_sep="_",
			 stop_words=unique(c(stopwords[["en"]], "where", "is", "do", "which", "how", "to"))) {
		sort_var <- rlang::arg_match(sort_var)

		df_labels <- labelled::lookfor(data = data, details = FALSE)
		df_labels <- tidyr::separate(data = df_labels, col = .data$label, sep = label_sep, fill = "right",
									 into = c("label_pre", "label_suf"), remove = FALSE)
		df_labels$label_pre <- tolower(df_labels$label_pre)
		df_labels$label_pre_str <- vctrs::as_list_of(strsplit(x = df_labels$label_pre, split = "[[:space:][:punct:]]"))
		df_labels$label_pre2 <-
			purrr::map_chr(df_labels$label_pre_str, .f=function(.x) {
				out <- .x[!.x %in% stop_words]
				out <- if(length(out) > 0L) paste0(out, collapse=" ") else paste0(.x, collapse=" ")
				out <- paste0(out, collapse=" ")
			})
		df_labels$label_pre3 <- abbreviate(names.arg = df_labels$label_pre2,
										   named = TRUE, minlength = 2L, dot = FALSE, method = "both")
		df_labels <- dplyr::arrange(df_labels, .data$label_pre, .data[[sort_var]])
		df_labels <- dplyr::group_by(df_labels, .data$label_pre)
		df_labels <- dplyr::mutate(df_labels,
								   label_suf_no = if(dplyr::n()==1L) NA_character_ else if(n()<10L) sprintf("%01d", seq_len(n())) else if(dplyr::n()>=10L) sprintf("%02d", seq_len(dplyr::n())))
		df_labels <- dplyr::ungroup(df_labels)
		df_labels <- tidyr::unite(df_labels, col = "variable_new", c(.data$label_pre3, .data$label_suf_no), sep = new_var_sep, na.rm = TRUE)
		data <- dplyr::rename_with(.data = data,
								   .fn = ~purrr::map_chr(.x = .,
								   					  .f = ~dplyr::pull(df_labels[df_labels$variable == .x, "variable_new"])))
		data
	}


#' Swap Dataset Columns and Labels
#'
#' Columns not containing labels will remain unaffected, and warning given.
#'
#' @param data Data frame or tibble.
#'
#' @return Data.frame/tibble.
#' @export
#'
#' @examples swap_label_colnames(mtcars)
swap_label_colnames <-
  function(data) {
    cls <- class(data)
    if(!inherits(data, "data.frame")) cli::cli_abort("{.arg data} must be a {.cls data.frame}.")

    colname <- colnames(data)
    data <- remove_special_chars_in_labels(data)
    lacking_labels <- c()
    label <- lapply(colname, function(.x) {
      l <- attr(data[[.x]], "label")
      if(is.null(l)) {
        assign(x = "lacking_labels",
               value = c(lacking_labels, .x),
               envir = rlang::env_parent())
        .x
      } else l
    })
    label <- as.character(unlist(label))
    if(length(lacking_labels)>0L) {
      cli::cli_warn(c(i="Missing labels for columns: {.var {lacking_labels}}.",
                      i="Leaving column names untouched for these."))
    }

    data <-
      lapply(seq_along(data), function(.x) {
        if(!is.null(colname[.x])) {
          attr(data[[.x]], "label") <- colname[[.x]]
        }
      data[[.x]]
    })
    data <- as.data.frame(data)
    colnames(data) <- label
    class(data) <- cls
    data
  }
