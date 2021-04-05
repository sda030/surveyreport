#' Add Table
#'
#' Can add one of two tables:
#'
#' @param obj A list containing df, var_frame and design_frame, as created by
#'   create_infoframe().
#' @param prefix_number Whether to add the underlying number index for the
#'   categories as a prefix in the printed chart and table labels. Useful for
#'   troubleshooting.
#' @param round_digits Integer, defaults to 1. Number of decimals in charts and
#'   tables. Currently not working for charts.
#' @param show_n Logical, defaults to FALSE. Should number of cases in a
#'   category/group be added to axis labels? NA means TRUE where there is
#'   variation across groups/categories, FALSE otherwise.
#' @param show_p Logical, defaults to TRUE. Should percentage symbol (%) be
#'   added to data labels?
#' @param sorting Which sorting variable to use? NULL equals data as it is.
#' @param drop_na_y,drop_na_x Logical, defaults to TRUE. Whether to drop missing
#'   as a category.
#' @param remove_empty_y_var Logical, defaults to FALSE. As `reporter()` cannot
#'   handle rows in design_frame with missing y_var, will drop these rows if
#'   FALSE (NOT YET IMPLEMENTED). Otherwise aborts.
#' @param type Integer, either 1 or 2. 1 corresponds to a regular percent table
#'   whereas 2 corresponds to average per x_var category.
#'
#' @return obj, with the barcharts attached in design_frame under column
#'   "barchart1".
#' @importFrom dplyr select all_of case_when filter rename across n group_by
#'   ungroup summarize mutate if_else count add_count n_distinct arrange
#'   rename_with %>%
#' @importFrom labelled set_variable_labels var_label to_character val_label
#' @importFrom rlang abort warn .data .env  arg_match set_names %|% quo_text
#' @importFrom haven as_factor
#' @importFrom tidyr pivot_longer unite
#' @importFrom purrr map2 pmap map_lgl
#' @export
#'
#' @examples
#' ex_survey1_inf_new <- add_table(ex_survey1_inf, type=1)
#' ex_survey1_inf_new <- add_table(ex_survey1_inf_new, type=2)

add_table <- 
	function(obj, 
			 # Model specific arguments
			 prefix_number=FALSE, 
			 round_digits=1, 
			 show_n=FALSE, 
			 show_p=TRUE, 
			 sorting = NULL, drop_na_y=TRUE, drop_na_x=TRUE,
			 # Global arguments
			 type=1,
			 remove_empty_y_var=FALSE) {
		
		show_n_options <- c(T,F,NA)
		sorting_options <- c("y_pos", "x_pos", 
							 "top1", "average", "sum_high_cat", "sum_low_cat", "est", "pval")
		
		
		obj <- assert_valid_infoframe(obj=obj)
		design_frame <- check_options(df = design_frame, df_var = "drop_na_y", global_default = drop_na_y, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "drop_na_x", global_default = drop_na_x, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "prefix_number", global_default = prefix_number, options = c(TRUE,FALSE))
		design_frame <- check_options(df = design_frame, df_var = "round_digits", global_default = round_digits, options = -3:3)
		design_frame <- check_options(df = design_frame, df_var = "show_n", global_default = show_n, options = show_n_options)
		design_frame <- check_options(df = design_frame, df_var = "show_p", global_default = show_n, options = show_n_options)
		
		added_parts <- c()
		
		
		if(is.null(design_frame[["sort_order"]])) {
			if(!is.null(sorting) && (length(sorting)!=1L || !sorting %in% sorting_options)) {
				rlang::abort(c(i="Global argument `sorting` must be a string length 1, and one of ", 
							   rlang::quo_text(sorting_options), 
							   i=" or NULL when column 'sort_order' in design_frame is not provided."))
			}
			design_frame[["sort_order"]] <- if(!is.null(sorting)) sorting else "y_var"
			added_parts <- c(added_parts, "sort_order")
		}
		
		
		if(length(added_parts)>0L) rlang::warn(c("Added parts that were missing in design_frame: ", 
												 rlang::quo_text(added_parts)))
		

		
		
		create_table <- 
			function(y_var, y_label, y_group, y_group_label, y_colour_set, drop_na_y,
					 x_var, x_label, x_group, x_group_label, x_colour_set, drop_na_x,
					 sort_order,
					 prefix_number, hide_label_if_less_than, round_digits, show_n, show_p, ...) {
				
				if(is.null(x_var)) x_var <- NA_character_
				if(!is.na(x_var)) {
					
					if(!is.character(x_var) | !length(x_var) %in% 0:1) {
						
						rlang::abort(c("Current version only accepts x_var as a single string per row.",
									   i=paste0("For the row when y_var is ", paste0(y_var, collapse=","), ","),
									   x=paste0("there is a problem with x_var: ", paste0(x_var, collapse=",")))) 
					}
					if(any(!x_var %in% names(df))) {
						rlang::abort(message = paste0("Following x_var not found in df:", x_var))
					}
				}
				
				tab <-
					df %>%
					dplyr::select(dplyr::all_of(c(.env$y_var), if(!is.na(.env$x_var)) .env$x_var)) %>%
					labelled::set_variable_labels(.labels = rlang::set_names(.env$y_label, nm=unname(.env$y_var)) %>% as.list()) %>%
					rlang::set_names(nm = ifelse(nchar(labelled::var_label(., unlist = T))==0L | colnames(.) %in% .env$x_var,
												 colnames(.), labelled::var_label(., unlist = T)))
				
				if(type==1L) {
					if(!is.na(x_var) && length(y_var)>1) {
						rlang::warn(c("When x_var is specified, table type1 is currently limited to a single-item chart.",
									  i = paste0("Skipping for ", rlang::quo_text(y_var), " and ", x_var)))
						return()
						
					} else {
						
						tab_long <-
							tab %>%
							{if(is.na(x_var)) tidyr::pivot_longer(., cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val") else {
								dplyr::rename(., val={y_label}, var={x_var})}} %>%
							dplyr::mutate(var = labelled::to_character(.data$var),
										  category = labelled::to_character(.data$val),
										  val = as.integer(haven::as_factor(.data$val))) %>% # Problem here if first variable lacks a category
							{if(drop_na_y) dplyr::filter(., !is.na(.data$category)) else .} %>%
							{if(!drop_na_y) dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>")) else .} %>%
							{if(drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
							{if(drop_na_x & !is.na(x_var)) dplyr::filter(., !is.na(.data$var)) else .} %>%
							{if(!drop_na_x | is.na(x_var)) dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>")) else .} %>%
							dplyr::add_count(.data$var, name = "n_per_var") %>%
							dplyr::count(.data$var, .data$n_per_var, .data$val, .data$category, name = "n_per_var_val") %>%
							dplyr::mutate(percent = round(.data$n_per_var_val/.data$n_per_var, 3)) %>%
							{if(show_n |
								(is.na(show_n) &
								 dplyr::n_distinct(.[["n_per_var"]]) > 1L)) tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
									dplyr::mutate(var=paste0(.data$var, ")")) else .} %>%
							{if(prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
							dplyr::arrange(.data$val) %>%
							dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = TRUE)) %>% # Problem here if first variable lacks a category
							dplyr::arrange(.data$var, .data$val)
						
						table <-
							tab_long %>%
							dplyr::select(-dplyr::all_of("val", "n_per_var_val")) %>%
							dplyr::mutate(percent = .data$percent*100) %>%
							{if(show_n) dplyr::select(., -dplyr::all_of("n_per_var")) else dplyr::rename(., N=.data$n_per_var)} %>%
							dplyr::rename_with(.cols = dplyr::all_of("var"), .fn = ~ y_group_label) %>%
							tidyr::pivot_wider(names_from = all_of("category"), values_from = all_of("percent")) %>%
							{if(show_p) dplyr::rename_with(., .cols = -1, ~ paste0(., " (%)")) else .}
					}
					
				} else if(type==2 && !is.na(x_var)) {
					
					table <-
						tab %>%
						tidyr::pivot_longer(cols = dplyr::all_of(.env$y_label), names_to = "var", values_to = "val") %>%
						dplyr::mutate(var = labelled::to_character(.data$var),
									  category = labelled::to_character(.data$val),
									  val = as.integer(haven::as_factor(.data$val))) %>% # Problem here if first variable lacks a category
						{if(.env$drop_na_y) dplyr::filter(., !is.na(.data$category)) else dplyr::mutate(., category = rlang::`%|%`(.data$category, "<NA>"))} %>%
						{if(.env$drop_na_y) dplyr::filter(., !is.na(.data$val)) else .} %>%
						{if(.env$drop_na_x & !is.na(.env$x_var)) dplyr::filter(., !is.na(.data$var)) else dplyr::mutate(., var = rlang::`%|%`(.data$var, "<NA>"))} %>%
						dplyr::add_count(.data$var, .data[[x_var]], name = "n_per_var_var2") %>%
						dplyr::count(.data$var, .data[[x_var]], .data$n_per_var_var2, .data$val, .data$category, name = "n_per_var_val_var2") %>%
						dplyr::mutate(percent = round(.data$n_per_var_val_var2/.data$n_per_var_var2*100, round_digits)) %>%
						{if(.env$show_n | (is.na(.env$show_n) & dplyr::n_distinct(.[["n_per_var_var2"]]) > 1L)) {
							tidyr::unite(., col = "var", c(.data$var, .data$n_per_var), sep = " (N=", remove = TRUE, na.rm = TRUE) %>%
								dplyr::mutate(var=paste0(.data$var, ")"))} else .} %>%
						{if(.env$prefix_number) tidyr::unite(., col = "category", c(.data$val, .data$category), sep = ": ", remove = FALSE) else .} %>%
						dplyr::arrange(.data$val) %>%
						dplyr::mutate(category = factor(.data$category, levels = unique(.data$category), ordered = T)) %>% # Problem here if first variable lacks a category
						{if(.env$show_p) dplyr::mutate(., category = paste0(.data$category, " (%)")) else .} %>%
						{if(.env$show_n) dplyr::rename(., N = .data$n_per_var_var2) else dplyr::select(., -dplyr::all_of("n_per_var_var2"))} %>%
						dplyr::arrange(dplyr::all_of(x_var), .data$var, .data$val) %>%
						dplyr::select(-dplyr::all_of(c("n_per_var_val_var2", "val")))  %>%
						tidyr::pivot_wider(names_from = .data$category, values_from = .data$percent) %>%
						dplyr::relocate(dplyr::all_of(c(x_var, "var"))) %>%
						dplyr::rename_with(.cols = dplyr::all_of("var"), .fn = ~ y_group_label)
				}
				
				
				
				# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
				# {if(sort=="sum_upper_categories") dplyr::group_by(., var) %>%
				# 		dplyr::mutate(sort_var = ifelse(category = )) %>%
				# 		dplyr::arrange(if(desc) dplyr::desc(sort_var) else sort_var, val) else .} %>%
				# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
				# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
				#"average", "sum_upper_categories", "alphabetical", "sum_lower_categories", "significance",
				table
			}
		
		df <- .remove_special_chars_in_labels(obj$df)
		
		design_frame <-
			design_frame %>%
			dplyr::arrange(.data[["sort_order"]])
		
		out <-
			purrr::pmap(.l = design_frame, .f = create_table)
		
		list(df=obj$df, var_frame=obj$var_frame, design_frame=cbind(obj$design_frame, out))
				
	}