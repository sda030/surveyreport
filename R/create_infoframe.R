
#' Create An Infoframe
#' 
#' Will create a list with the original data, a data column frame (codebook), 
#'     and a design frame for y-x analyses, tables, figures and reports. 
#'     Defaults in the design frame can be modified later. This function should
#'     be run if the original data has been changed.
#'     
#' @param df A labelled data frame containing only the data columns one wishes 
#'     to create item groups for.
#' @param var_group_item_sep Separator between item group identifier prefix 
#'     and item suffix. Default="_" is currently the only that works well for 
#'     separating. Set "$" if each item is to be considered a group.
#' @param varlabel_group_item_sep Same as for `var_group_item_sep`, but for 
#'     separating labels into group and item specific.
#' @param y_var character vector with dependent data columns.
#' @param x_var character vector with independent data columns.
#' @param med_var character vector with mediator data columns.
#' @param ordinal_var character vector with data columns having ordinal meaning.
#' @param nominal_var character vector with data columns having nominal meaning.
#' @param interval_var character vector with data columns having interval meaning.
#' @param text_var character vector with data columns having text meaning 
#'     (will be excluded from figures, tables, analyses, but will be reported 
#'     in wordclouds and as-is.
#' @param add_constructs Constructs consist of multiple data columns measuring 
#'     essentially the same. Add these as new construct based on group-variable?
#' @param add_y_univariates,add_x_univariates,add_m_univariates Whether or not 
#'     to report univariates for dependent/independent/mediator data columns.
#' @param ordinal_if_fewer_than ???
#' @param vars_as_labels Logical, TRUE (default) or FALSE). If TRUE, will use 
#'     data column names as labels if labels are missing. 
#' @param colour_set_ordinal A character vector with hex-colours used for 
#'     ordinal data columns in charts/plots.
#' @param colour_set_nominal A character vector with hex-colours used for 
#'     nominal data columns in charts/plots.
#'
#' @return A list consisting of the original data, a var_frame (codebook for 
#'     each data column), and a design_frame.
#' \itemize{
#' \item{var_frame}{The var_frame (codebook).}
#' \itemize{
#' \item{group}{Data column group.}
#' \item{item}{Original data column.}
#' \item{group_label}{Group variable label.}
#' \item{item_label}{Original data column label.}
#' \item{type}{Data column type: ordinal, nominal, text, interval, 
#'     singular (only one value)}
#' }
#' \item{design_frame}
#' \itemize{
#' \item{y_var}
#' \item{y_label}
#' \item{y_group}
#' \item{y_group_label}
#' \item{x_var}
#' \item{x_label}
#' \item{x_group}
#' \item{x_group_label}
#' \item{y_type}
#' \item{x_type}
#' }
#' }
#' @importFrom purrr map_chr map map_lgl
#' @import dplyr 
#' @importFrom haven is.labelled
#' @importFrom labelled to_labelled look_for
#' @importFrom rlang warn abort := set_names .data .env
#' @importFrom tidyr separate chop pack unpack expand_grid
#' @importFrom vctrs as_list_of
#' @importFrom utils View
#' @export
#'
#' @examples
#' ex_data <- readRDS(system.file("extdata", "ex_survey.RDS", 
#'                    package="surveyreport", mustWork=TRUE))
#' obj <- create_infoframe(df=ex_data, 
#'                         y_var=grep("^[abdefg]_", names(ex_data), value=TRUE),
#'                         x_var=grep("^x", names(ex_data), value=TRUE),
#'                         ordinal_var=grep("^[abdefg]_", names(ex_data), value=TRUE),
#'                         nominal_var=c("xsex", "xhuman", "h_1", "h_2", "h_3", "h_4"),
#'                         add_x_univariates=TRUE,
#'                         add_y_univariates=TRUE)
#' obj$var_frame
#' obj$design_frame

create_infoframe <-
	function(df,
			 var_group_item_sep="_",
			 varlabel_group_item_sep=" - ",
			 y_var = grep("^x", colnames(df), value = TRUE, invert = TRUE),
			 x_var = NULL,
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
		add_univariates <- function(data, e="y") {
			dplyr::bind_rows(data,
							 dplyr::filter(var_frame, .data$role==e) %>%
							 	dplyr::select(-all_of("role")) %>%
							 	dplyr::rename_with(.cols = dplyr::everything(), .fn = ~paste0("y_", .)))
		}
		if(vars_as_labels) {
			lapply(c(y_var, x_var), function(var) {
				if(!haven::is.labelled(df[[var]])) {
					rlang::warn(paste0(var, " is not labelled. Using data column as label."))
					df[[var]] <- labelled::to_labelled(df[[var]])
				}
			})
		}
		if(!all(y_var %in% colnames(df))) rlang::abort(paste0("Following y_var not found in df: c(", paste0(y_var[!y_var %in% colnames(df)], collapse=", "), ")"))
		if(!all(x_var %in% colnames(df))) rlang::abort(paste0("Following x_var not found in df: c(", paste0(x_var[!x_var %in% colnames(df)], collapse=", "), ")"))

		y_lacks_sep <- grep(var_group_item_sep, y_var, value = TRUE, invert = TRUE)
		if(length(y_lacks_sep)>0L) rlang::warn(paste0("The following y_var could not be split: c(", paste0(y_lacks_sep, collapse=", "), ")"))
		x_lacks_sep <- grep(var_group_item_sep, x_var, value = TRUE, invert = TRUE)
		if(length(x_lacks_sep)>0L) rlang::warn(paste0("The following x_var could not be split: c(", paste0(x_lacks_sep, collapse=", "), ")"))



		var_frame <-
			labelled::look_for(df, details=FALSE) %>%
			tidyr::separate(col=.data$variable, into=c("group", NA), sep=var_group_item_sep, remove = FALSE, convert = FALSE, extra = "merge") %>%
			tidyr::separate(col=.data$label, into=c("group_label", "label"), sep=varlabel_group_item_sep, convert = FALSE, extra = "merge") %>%
			dplyr::rename(var = all_of("variable")) %>%
			dplyr::mutate(label = dplyr::if_else(is.na(.data$label) | nchar(.data$label)==0L, .data$group_label, .data$label),
						  role = dplyr::if_else(.data$var %in% .env$y_var, "y", 
						  					  dplyr::if_else(.data$var %in% .env$x_var, "x", 
						  					  			   dplyr::if_else(.data$var %in% .env$med_var, "m", NA_character_))),
						  type = type_checker(var=.data$var),
						  colour_set = colour_picker(var=.data$type)) %>%
			dplyr::arrange(.data$group, .data$group_label, .data$var, .data$label)


		## Error checking
		n_group_label <- 
			var_frame %>% 
			dplyr::distinct(.data$group, .data$group_label) %>% 
			nrow()
		n_group <- 
			var_frame %>% 
			dplyr::distinct(.data$group) %>% 
			nrow()
		n_group_aux <- 
			var_frame %>% 
			dplyr::distinct(.data$group, .data$role, .data$type, .data$colour_set, .keep_all = T) %>% 
			nrow()

		if(n_group_label != n_group) {
			var_frame %>% 
				dplyr::distinct(.data$group, .data$group_label, .keep_all = T) %>% 
				dplyr::add_count(.data$group) %>% 
				dplyr::filter(.data$n > 1) %>% 
				utils::View() # Only show what is relevant
			error_msg <- "You need to check that your battery-item pattern is valid, and that names and labels match accordingly.\n There is a mismatch between "
			rlang::abort(paste0(error_msg, "group names (", n_group, ") and labels (", n_group_label, "). "))
		}
		if(n_group_aux != n_group) {
			var_frame %>% 
				dplyr::distinct(.data$group, .data$role, .data$type, .data$colour_set, .keep_all = T) %>% 
				dplyr::add_count(.data$group) %>% 
				dplyr::filter(.data$n>1) %>%
				utils::View() # Only show what is relevant
			error_msg <- "Please ensure all data columns in a group have the same role, type and colour_set. Mismatch between "
			rlang::abort(paste0(error_msg, "groups (", n_group, ") and role-type-colourset (", n_group_aux, "). "))
		}

		if(add_constructs) {
			var_frame <-
				dplyr::bind_rows(
					var_frame %>% # For the regular variables, make them list-columns.
						dplyr::mutate(var = vctrs::as_list_of(as.list(.data$var)),
									  label = vctrs::as_list_of(as.list(.data$label))),
					var_frame %>% # For the constructs, chop up related variables.
						dplyr::select(-dplyr::all_of("pos")) %>%
						dplyr::filter(.data$group != .data$var) %>%
						tidyr::chop(cols = c(.data$var, .data$label)))
		}
		tmp <-
			c("y", "m", "x") %>%
			rlang::set_names() %>%
			purrr::map(., function(e) {
				var_frame %>%
					dplyr::filter(.data$role == .env$e) %>%
					dplyr::select(-dplyr::all_of("role")) %>%
					dplyr::rename_with(.cols = dplyr::everything(), .fn = ~paste0(e, "_", .)) %>%
					tidyr::pack(!!e := dplyr::everything())
			}) %>%
			.[purrr::map_lgl(., .f = ~nrow(.)>0L)]

		design_frame <-
			tidyr::expand_grid(tmp[["y"]], tmp[["m"]], tmp[["x"]]) %>%
			tidyr::unpack(cols = names(.env$tmp)) %>%
			{if(add_y_univariates) add_univariates(data=., e="y") else .} %>%
			{if(add_m_univariates) add_univariates(data=., e="m") else .} %>%
			{if(add_x_univariates) add_univariates(data=., e="x") else .} %>%
			unique()


		list(df=df, var_frame=var_frame, design_frame=design_frame) #, call=as.list(arg_call)
	}
