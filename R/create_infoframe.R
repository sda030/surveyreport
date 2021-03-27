# TODO: Better tests and better diagnostics if tests fail: Return tibble
# TODO: Replace separators with patterns? Or character vector patterns?
# TODO: The check of corresponding group and group_label should rather be checking if all groups have the same group_label, not the other way around

# TODO:  create expand.grid-based function.
# TODO: nested data-frame

#' @param df A labelled data frame containing only the variables one wishes to create item groups for.
#' @param var_group_item_sep Separator between item group identifier prefix and item suffix. Default="_" is currently the only that works well for separating. Set "$" if each item is to be considered a group.
#' @param varlabel_group_item_sep Same as for `var_group_item_sep`, but for separating labels into group and item specific.
#' @param list To return a data frame or a list. Default is FALSE.
#' @description Will create a data frame or a list for functions such as `tablemaker()` and `all_mplus_analyses()`
#' @return A data.frame, possibly nested if multiple variables in x_var and y_var
#' \item{group} The item variable group. If list=TRUE, each group is the name of the returned list.
#' \item{item} The original item variable. If list=TRUE, each item are grouped within each group.
#' \item{group_label} The group variable label. If list=TRUE, these are returned as an attribute of the list object.
#' \item{item_label} The original item variable label. If list=TRUE, these are names of the item character vectors.
#'
#' @author Stephan Daus



# If is.character(var_group_item_sep), then rename variables to fit label split? As separate prior function?
create_infoframe <-
	function(df,
			 var_group_item_sep="_",
			 varlabel_group_item_sep=" - ",
			 y_var = grep("^x", colnames(df), value = T, invert = TRUE),
			 x_var = grep("^x", colnames(df), value = T, invert = FALSE),
			 med_var = NULL,
			 ordinal_var=NULL,
			 nominal_var=NULL,
			 interval_var=NULL,
			 text_var=NULL,
			 add_constructs=TRUE,
			 add_x_univariates=FALSE, add_y_univariates=FALSE, add_m_univariates=FALSE,
			 ordinal_if_fewer_than = 5,
			 vars_as_labels = FALSE,
			 colour_set_ordinal = c("#E8B0B7", "#DE919A", "#D5727D", "#C84957", "#BC3848", "#9D2F3C", "#7E2630"), # Red
			 colour_set_nominal = c(red="#C82D49",
			 					   black = "#363636",
			 					   beige="#EDE2D2",
			 					   blue="#2D8E9F",
			 					   purple="#DBD2E0")) {
		arg_call <- match.call(expand.dots = T)

		type_checker <- function(var, data=df) {
			purrr::map_chr(var, function(v) {
				dplyr::case_when(dplyr::n_distinct(data[[v]], na.rm = T) == 1L ~ "singular",
								 v %in% text_var ~ "text",
								 v %in% nominal_var ~ "nominal",
								 v %in% ordinal_var |
								 	(dplyr::n_distinct(data[[v]], na.rm = T) >= 2L &
								 	 	dplyr::n_distinct(data[[v]], na.rm = T) < ordinal_if_fewer_than) ~ "ordinal",
								 v %in% interval_var | (dplyr::n_distinct(data[[v]], na.rm = T) >= ordinal_if_fewer_than) ~ "interval")
			})
		}
		colour_picker <- function(var) {
			purrr::map(var, function(v) {
				if(is.na(v)) NA_character_ else if(v=="ordinal") colour_set_ordinal else if(v=="nominal") colour_set_nominal else NA_character_
				### NEED TO ADD INTERVAL
			})}
		add_univariates <- function(.data) {
			bind_rows(.data,
					  dplyr::filter(var_frame, role=="y") %>%
					  	dplyr::select(-role) %>%
					  	dplyr::rename_with(.cols = dplyr::everything(), .fn = ~paste0("y_", .)))
		}
		if(vars_as_labels) {
			lapply(c(y_var, x_var), function(var) {
				if(!haven::is.labelled(df[[var]])) {
					rlang::warn(paste0(var, " is not labelled. Using variable as label."))
					df[[var]] <- labelled::to_labelled(df[[var]])
				}
			})
		}
		if(!all(y_var %in% colnames(df))) rlang::abort(paste0("Following y_var not found in df: c(", paste0(y_var[!y_var %in% colnames(df)], collapse=", "), ")"))
		if(!all(x_var %in% colnames(df))) rlang::abort(paste0("Following x_var not found in df: c(", paste0(x_var[!x_var %in% colnames(df)], collapse=", "), ")"))

		y_lacks_sep <- grep(var_group_item_sep, y_var, value = T, invert = T)
		if(length(y_lacks_sep)>0L) rlang::warn(paste0("The following y_var could not be split: c(", paste0(y_lacks_sep, collapse=", "), ")"))
		x_lacks_sep <- grep(var_group_item_sep, x_var, value = T, invert = T)
		if(length(x_lacks_sep)>0L) rlang::warn(paste0("The following x_var could not be split: c(", paste0(x_lacks_sep, collapse=", "), ")"))



		var_frame <-
			labelled::look_for(df, details=FALSE) %>%
			tidyr::separate(col=variable, into=c("group", NA), sep=var_group_item_sep, remove = F, convert = F, extra = "merge") %>%
			tidyr::separate(col=label, into=c("group_label", "label"), sep=varlabel_group_item_sep, convert = F, extra = "merge") %>%
			dplyr::rename(var = variable) %>%
			dplyr::mutate(label = dplyr::if_else(is.na(label) | nchar(label)==0L, group_label, label),
						  role = dplyr::if_else(var %in% y_var, "y", dplyr::if_else(var %in% x_var, "x", dplyr::if_else(var %in% med_var, "m", NA_character_))),
						  type = type_checker(var=var),
						  colour_set = colour_picker(var=type)) %>%
			dplyr::arrange(group, group_label, var, label)

		## Error checking
		n_group_label <- var_frame %>% dplyr::distinct(group, group_label) %>% nrow()
		n_group <- var_frame %>% dplyr::distinct(group) %>% nrow()
		n_group_aux <- var_frame %>% dplyr::distinct(group, role, type, colour_set, .keep_all = T) %>% nrow()

		if(n_group_label != n_group) {
			View(var_frame %>% dplyr::distinct(group, group_label, .keep_all = T) %>% dplyr::add_count(group) %>% dplyr::filter(n>1)) # Only show what is relevant
			error_msg <- "You need to check that your battery-item pattern is valid, and that names and labels match accordingly.\n There is a mismatch between "
			rlang::abort(paste0(error_msg, "group names (", n_group, ") and labels (", n_group_label, "). "))
		}
		if(n_group_aux != n_group) {
			View(var_frame %>% dplyr::distinct(group, role, type, colour_set, .keep_all = T) %>% dplyr::add_count(group) %>% dplyr::filter(n>1)) # Only show what is relevant
			error_msg <- "Please ensure all variables in a group have the same role, type and colour_set. Mismatch between "
			rlang::abort(paste0(error_msg, "groups (", n_group, ") and role-type-colourset (", n_group_aux, "). "))
		}

		if(add_constructs) {
			var_frame <-
				dplyr::bind_rows(var_frame %>%
								 	dplyr::mutate(var = vctrs::as_list_of(as.list(var)),
								 		   label = vctrs::as_list_of(as.list(label))),
					  var_frame %>%
					  	dplyr::select(-pos) %>%
					  	dplyr::filter(group != var) %>%
					  	tidyr::chop(cols = c(var, label)))
		}

		tmp <-
			c("y", "m", "x") %>%
			setNames(., nm=.) %>%
			purrr::map(., function(e) {
				var_frame %>%
					dplyr::filter(role==e) %>%
					dplyr::select(-role) %>%
					dplyr::rename_with(.cols = dplyr::everything(), .fn = ~paste0(e, "_", .)) %>%
					tidyr::pack(!!e := dplyr::everything())
			}) %>%
			.[purrr::map_lgl(., .f = ~nrow(.)>0L)]
		design_frame <-
			tidyr::expand_grid(tmp[["y"]], tmp[["m"]], tmp[["x"]]) %>%
			tidyr::unpack(cols = names(tmp)) %>%
			{if(add_y_univariates) add_univariates(.data=.) else .} %>%
			{if(add_m_univariates) add_univariates(.data=.) else .} %>%
			{if(add_x_univariates) add_univariates(.data=.) else .} %>%
			unique()


		list(df=df, var_frame=var_frame, design_frame=design_frame, call=as.list(arg_call))
	}
