
#' Remove Special Characters (<,>) in Variable Labels
#'
#' @param df Data frame
#'
#' @return A data frame
#'

.remove_special_chars_in_labels <- 
	function(df) {
	labelled::val_labels(df) <-
		labelled::val_labels(df) %>%
		purrr::map2(.x = ., .y=names(.), .f=function(x, y) {
			if(!is.null(x) && any(grepl("<|>", names(x)))) {
				rlang::warn(paste0("Current version of function doesn't handle special characters `<` or `>` in labels.",
								   "Will remove these in ", y))
				names(x)<- gsub("<|>", "", names(x))
			}
			x
		})	
	df
}
