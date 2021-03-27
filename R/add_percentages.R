

add_percentages <- function(df, design_frame, set="upper", percent=TRUE, digits=0) {
	purrr::pmap_dfr(.l = design_frame, .f=function(Y, X, ...) {
		df %>%
			dplyr::select(y=tidyselect::all_of(Y), x=tidyselect::all_of(X)) %>%
			dplyr::filter(!is.na(x), !is.na(y)) %>%
			dplyr::mutate(x_cat = labelled::to_character(x),
						  x_val = as.integer(haven::as_factor(x)),
						  x_val2 = x_val,
						  y_val = as.integer(haven::as_factor(y)),
						  include = y_val %in% subset_vector(sort(unique(y_val)), set=set),
						  y_val=NULL) %>%
			dplyr::count(x_cat, x_val, x_val2, include) %>%
			dplyr::group_by(x_cat, x_val) %>%
			dplyr::mutate(p = round(n/sum(n)*(if(percent) 100 else 1), digits=digits)) %>%
			dplyr::ungroup() %>%
			dplyr::filter(include) %>%
			dplyr::select(-include, -n) %>%
			tidyr::pivot_wider(names_from = c(x_val, x_val2), values_from = c(p, x_cat)) %>%
			dplyr::mutate(Y=Y, X=X) %>%
			dplyr::rename_with(.cols = dplyr::matches("_[0-9]*$"), .fn = ~gsub("_[0-9]*$", "", .))
	}) %>%
		dplyr::full_join(x = design_frame, y=., by=c("X", "Y")) %>%
		dplyr::mutate(dplyr::across(dplyr::matches("^p_[0-9]*$"), ~ifelse(is.na(.), 0, .)))
}
