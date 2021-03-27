# TODO: If first variable in a y_var has no response in a category, category order is messed up.
# TODO: Sorting of variables both within section, but also within a graph/table!
# TODO: Make function general enough so that it can be used in a pmap-loop for school reports. section1_new_file=TRUE for school reports
# TODO: Make tables in doc prettier (header formating, left align first column, etc)


# Version 2.0
# TODO: Scatter+trend line plots for linear/binary logistic regression models where x_focus is specified
# TODO: Forest plot for regression model results
# TODO: Same plots for ggplot2 as for mschart
# TODO: Method chapter: response rate (opened/completed), broken down by x_var, representativeness given pop_var, map for gsiid
# TODO: Also export Rmarkdown/html-file?


#### Modify xml-files:
# TODO: Hide axis titles.
# TODO: Right-align axis text by modifying xml-raw data
# TODO: Drop data labels if smaller than 5%
# Insert this: dLbl><c:idx val=X_value/><c:delete val="1"/><c:extLst><c:ext uri="{CE6537A1-D6FC-4f65-9D91-7224C49458BB}" xmlns:c15="http://schemas.microsoft.com/office/drawing/2012/chart"/><c:ext uri="{C3380CC4-5D6E-409C-BE32-E72D297353CC}" xmlns:c16="http://schemas.microsoft.com/office/drawing/2014/chart"><c16:uniqueId val="{00000000-BC21-437A-B275-29C6E34D2EC4}"/></c:ext></c:extLst></c:dLbl><c:
# After this: </a:srgbClr></a:solidFill></a:ln><a:effectLst/></c:spPr><c:invertIfNegative val="0"/><c:dLbls><c:

# TODO: Strip off all unnecessary dependencies
# TODO: Documentation
# TODO: Excel template

# out of my control:
# TODO: Excel output, table and chart into Excel
# TODO: Crashes if file is open (https://github.com/davidgohel/officer/issues/349)
# TODO: Also file doesn't open if a crashed Word app is open...
# TODO: Non-ASCII letters (https://github.com/ardata-fr/mschart/issues/57), but workaround works.


#' @param df A dataframe with raw responses (one row is a respondent, columns are variables)
#' @param design_frame A dataframe (or tibble) with the variable combinations to run. For instance based on output from all_mplus_analyses, indicating which combinations of X and Y to allow. data.frame(X=c(), Y=c(), group=c()). If not given, produce all combinations found in y_var
#' @examples


reporter <-
	function(df, design_frame,  var_frame,

			 section_divider1 = NULL,
			 section_divider2 = NULL,
			 sorting = NULL,
			 y_colour_set=NULL, drop_na_y=TRUE, drop_na_x=TRUE, prefix_number=FALSE, remove_empty_y_var=FALSE, drop_duplicates=TRUE,
			 hide_label_if_less_than=0, font_size=8, round_digits=1, show_n_chart="caption", show_n_table="caption",
			 show_p_table = "axis",
			 create_barplot1=TRUE, create_barplot2=FALSE, create_table1=TRUE, create_table2=FALSE,

			 docx=TRUE, pptx=FALSE, xlsx=FALSE,
			 str_pos = " <+> ", str_neg = " <-> ", str_not = " <0> ", str_blank = 0, str_table_tag="Tabell ", str_figure_tag="Figur ",

			 path=getwd(), file_prefix="uni",
			 docx_template_path = "C:/Users/py128/OneDrive - NIFU/AuxR/tidyreport/NIFUmal_tom.docx",
			 pptx_template_path = "C:/Users/py128/OneDrive - NIFU/AuxR/tidyreport/NIFUmal_tom.pptx",
			 log_path=paste0(getwd(), "log.txt")) {

	library(dplyr)
	library(tidyr)
	library(labelled)
	library(officer)
	library(mschart)
	library(progress)



	built_in_colour_set <-
		c(red="#C82D49", black = "#363636", beige="#EDE2D2", blue="#2D8E9F", purple="#DBD2E0")
	section_options <- c('y_group_label', 'y_group', 'y_var', 'y_label', 'y_type',
						 'x_group_label', 'x_group', 'x_var', 'x_label', 'x_type', 'section1', 'section2')
	sorting_options <- c(section_options, "y_pos", "x_pos",
						 "top1", "average", "sum_high_cat", "sum_low_cat", "est", "pval")
	show_n_options <- c("axis", "caption", "auto", "none")
	added_parts <- c()

	# Early abort, warnings and messages.
	{
	if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	if(!is.data.frame(design_frame)) rlang::abort("design_frame is not a data.frame")
	if(nrow(design_frame)==0L) rlang::abort("Why do you give me an empty design_frame?")
	if(is.null(design_frame[["y_var"]])) rlang::abort("design_frame must have at least a 'y_var' variable")

	empty_y_var <- design_frame[["y_var"]] %>% purrr::map_int(.f = ~length(.))
	if(!remove_empty_y_var & any(!empty_y_var)) {
		rlang::abort("Seems there are empty 'y_var' entries. Please remove these first using:
					 `my_design_frame %>% filter(pull(y_var) %>% lapply(length) %>% unlist()>0)`")
	}
	if(drop_duplicates) {
		design_frame <- unique(design_frame)
	}

	if(is.null(design_frame[["y_label"]])) {
		design_frame[["y_label"]] <- design_frame[["y_var"]]
		added_parts <- c(added_parts, "y_label")
	}
	if(is.null(design_frame[["y_group"]])) {
		design_frame[["y_group"]] <- design_frame[["y_var"]] %>% purrr::map(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "y_group")
	}
	if(is.null(design_frame[["y_group_label"]])) {
		design_frame[["y_group_label"]] <- design_frame[["y_var"]] %>% purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "y_group_label")
	}
	if(is.null(design_frame[["x_var"]])) {
		design_frame[["x_var"]] <- NA_character_
		added_parts <- c(added_parts, "x_var")
	}
	if(is.null(design_frame[["x_label"]])) {
		design_frame[["x_label"]] <- design_frame[["x_var"]]
		added_parts <- c(added_parts, "x_label")
	}
	if(is.null(design_frame[["x_group"]])) {
		design_frame[["x_group"]] <- design_frame[["x_var"]] %>% purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "x_group")
	}
	if(is.null(design_frame[["x_group_label"]])) {
		design_frame[["x_group_label"]] <- design_frame[["x_var"]] %>% purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "x_group_label")
	}




	if(is.null(design_frame[["section1"]])) {
		if(!is.null(section_divider1) && (length(section_divider1) != 1L || !section_divider1 %in% section_options)) {
			rlang::abort(paste0("Global argument `section_divider1=`", " must be a string length 1, and one of c(", paste0(section_options, collapse=","), ") or NULL when variable 'section1' in design_frame is not provided."))
		}
		design_frame[["section1"]] <- if(!is.null(section_divider1)) design_frame[[section_divider1]] else NA_character_
		added_parts <- c(added_parts, "section1")
	}
	if(is.null(design_frame[["section2"]])) {
		if(!is.null(section_divider2) && (length(section_divider2) != 1L || !section_divider2 %in% section_options)) {
			rlang::abort(paste0("Global argument `section_divider2=`", " must be a string length 1, and one of c(", paste0(section_options, collapse=","), ") or NULL when variable 'section2' in design_frame is not provided."))
		}
		design_frame[["section2"]] <- if(!is.null(section_divider2)) design_frame[[section_divider2]] else NA_character_
		added_parts <- c(added_parts, "section2")
	}


	if(is.null(design_frame[["sort_order"]])) {
		if(!is.null(sorting) && (length(sorting)!=1L || !sorting %in% sorting_options)) {
			rlang::abort(paste0("Global argument `sorting=`", " must be a string length 1, and one of c(", paste0(sorting_options, collapse = ", "), ") or NULL when variable 'sort_order' in design_frame is not provided."))
		}
		design_frame[["sort_order"]] <- if(!is.null(sorting)) sorting else "y_var"
		added_parts <- c(added_parts, "sort_order")
	}

	if(is.null(design_frame[["y_colour_set"]])) {
		if(!is.null(y_colour_set) && !all(areColors(y_colour_set))) {
			rlang::abort(paste0("Global argument `y_colour_set=`", " must be a set of hex-colours, or NULL when variable 'y_colour_set' in design_frame is not provided."))
		}
		y_colour_set_replacement <- if(!is.null(y_colour_set)) y_colour_set else get_colour_set(n_colours_needed=20, user_colour_set=built_in_colour_set)
		design_frame[["y_colour_set"]] <- purrr::map(1:nrow(design_frame), function(i) y_colour_set_replacement)
		added_parts <- c(added_parts, "y_colour_set")
	}
	if(is.null(design_frame[["x_colour_set"]])) {
		if(!is.null(x_colour_set) && !all(areColors(x_colour_set))) {
			rlang::abort(paste0("Global argument `x_colour_set=`", " must be a set of hex-colours, or NULL when variable 'x_colour_set' in design_frame is not provided."))
		}
		x_colour_set_replacement <- if(!is.null(x_colour_set)) x_colour_set else get_colour_set(n_colours_needed=20, user_colour_set=built_in_colour_set)
		design_frame[["x_colour_set"]] <- purrr::map(1:nrow(design_frame), function(i) x_colour_set_replacement)
		added_parts <- c(added_parts, "x_colour_set")
	}
	# Return warnings
	if(length(added_parts)>0L) rlang::warn(paste0("Added parts that were missing in design_frame: ", paste0(added_parts, collapse=",")))

	design_frame <- check_options(df = design_frame, df_var = "est", global_default = NA_real_, options = numeric())
	design_frame <- check_options(df = design_frame, df_var = "pval", global_default = NA_real_, options = numeric())
	design_frame <- check_options(df = design_frame, df_var = "drop_na_y", global_default = drop_na_y, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "drop_na_x", global_default = drop_na_x, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "prefix_number", global_default = prefix_number, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "hide_label_if_less_than", global_default = hide_label_if_less_than, options = numeric())
	design_frame <- check_options(df = design_frame, df_var = "font_size", global_default = font_size, options = 0:72)
	design_frame <- check_options(df = design_frame, df_var = "round_digits", global_default = round_digits, options = -3:3)
	design_frame <- check_options(df = design_frame, df_var = "show_n_chart", global_default = show_n_chart, options = show_n_options)
	design_frame <- check_options(df = design_frame, df_var = "show_p_table", global_default = show_p_table, options = show_n_options)
	design_frame <- check_options(df = design_frame, df_var = "create_barplot1", global_default = create_barplot1, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "create_barplot2", global_default = create_barplot2, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "create_table1", global_default = create_table1, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "create_table2", global_default = create_table2, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "pptx", global_default = pptx, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "docx", global_default = docx, options = c(T,F))
	design_frame <- check_options(df = design_frame, df_var = "xlsx", global_default = xlsx, options = c(T,F))

	}

	val_labels(df) <-
		val_labels(df) %>%
		purrr::map2(.x = ., .y=names(.), .f=function(x, y) {
			if(!is.null(x) && any(grepl("<|>", names(x)))) {
				rlang::warn(paste0("Current version doesn't handle special characters `<` or `>` in labels. Will remove these in ", y))
				names(x)<- gsub("<|>", "", names(x))
			}
			x
		})



	## Create template files. SHOULD THIS BE GENERALIZED SO THAT OTHER TEMPLATES ARE POSSIBLE? THEN SUBMIT DOCUMENT TO FUNCTION.
	# doc <- tempfile(pattern = "doc_", fileext = ".docx")
	# download.file("https://nifu.no/nifu/rapportmal/NIFUmal_tom.docx", destfile = doc, mode = "wb", quiet = T)
	doc <- officer::read_docx(path = docx_template_path)
	doc_dim <- officer::docx_dim(doc)
	img_width <- doc_dim$page[["width"]] - doc_dim$margins[["left"]] - doc_dim$margins[["right"]]
	img_height_max <- doc_dim$page[["height"]] - doc_dim$margins[["top"]] - doc_dim$margins[["bottom"]] - 2

	# ppt <- tempfile(pattern = "ppt_", fileext = ".pptx")
	# download.file("https://nifu.no/nifu/rapportmal/NIFUmal_tom.pptx", destfile = ppt, mode = "wb", quiet = T)
	ppt <- officer::read_pptx(path = pptx_template_path)
	xls <- officer::read_xlsx(path = xlsx_template_path)


	design_frame <-
		design_frame %>%
		dplyr::arrange(.data[["section1"]], .data[["section2"]], .data[["sort_order"]]) %>%
		dplyr::mutate(rownumber = 1:nrow(.))

	pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(design_frame))
	pb$tick(0)


	doc <<-
		officer::body_add_par(x = doc, value = "Results", style = "NIFU_Overskrift 1") %>%
		officer::body_add_par(x = ., value = "", style = "NIFU_Normal første")

	out <- purrr::map(unique(design_frame[["section1"]]), .f = function(sec1) {
		if(!is.na(sec1)) {
		doc <<-
			officer::body_add_par(x = doc, value = if(!is.na(sec1)) sec1 else "", style = "NIFU_Overskrift 2 nummerert") %>%
			officer::body_add_par(x = ., value = "", style = "NIFU_Normal første")
		}

		purrr::map(unique(design_frame[design_frame[["section1"]] == sec1, "section2"]), .f = function(sec2) {
			if(!is.na(sec2)) {
				doc <<-
					officer::body_add_par(x = doc, value = if(!is.na(sec2)) sec2 else "", style = "NIFU_Overskrift 3 nummerert") %>%
					officer::body_add_par(x = ., value = "", style = "NIFU_Normal første")
			}
			input <-
				design_frame %>%
				{if(!is.na(sec1)) dplyr::filter(., section1 == sec1) else dplyr::filter(., is.na(section1))} %>%
				{if(!is.na(sec2)) dplyr::filter(., section2 == sec2) else dplyr::filter(., is.na(section2))}

			purrr::pmap(.l = input, .f = create_report_entry)
		})
	})
	if(pptx) print(x = ppt, target = file.path(path, paste0(file_prefix, ".pptx")))
	if(xlsx) print(x = xls, target = file.path(path, paste0(file_prefix, ".xlsx")))
	if(docx) print2.rdocx(x=doc, target = file.path(path, paste0(file_prefix, ".docx")))
	list(df=df, var_frame=var_frame, design_frame=design_frame, report=out)
}

areColors <- function(x) {
	sapply(x, function(X) { # Avoid sapply
		tryCatch(is.matrix(col2rgb(X)),
				 error = function(e) FALSE)
	})
}

hex_bw <- function(hex_code) {

	myrgb <- as.integer(col2rgb(hex_code))

	rgb_conv <- lapply(myrgb, function(x) {
		i <- x / 255
		if (i <= 0.03928) i / 12.92 else ((i + 0.055) / 1.055) ^ 2.4
	})
	rgb_calc <- (0.2126*rgb_conv[[1]]) + (0.7152*rgb_conv[[2]]) + (0.0722*rgb_conv[[3]])

	if (rgb_calc > 0.179) return("#000000") else return("#ffffff")

}

get_colour_set <- function(n_colours_needed, user_colour_set) {


	x <- 1:length(user_colour_set)

	if(!is.null(user_colour_set) &&
	   length(user_colour_set) >= n_colours_needed &&
	   all(areColors(user_colour_set))) {
		if(length(user_colour_set)==7L) {

			if(n_colours_needed==7L) return(user_colour_set)
			if(n_colours_needed==6L) return(user_colour_set[c(1:3, 5:length(x))])
			if(n_colours_needed==5L) return(user_colour_set[c(1:2, median(x), 6:length(x))])
			if(n_colours_needed==4L) return(user_colour_set[c(1,3, 5,length(x))])
			if(n_colours_needed==3L) return(user_colour_set[c(1, median(x),length(x))])
			if(n_colours_needed==2L) return(user_colour_set[c(1, length(x))])
		} else return(user_colour_set)

	} else {
		if(length(built_in_colour_set) >= n_colours_needed) {
			return(built_in_colour_set)
		} else {
			library(RColorBrewer)
			set.seed(1)
			qual_col_pals <- RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual' & brewer.pal.info$colorblind,]
			mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)) %>%
				unlist() %>%
				sample(., n_colours_needed)
		}
	}

}

ms_percentbar <- function(df, y_string, x_string, group_string="category", user_colours="#000000", f_size=8, means=FALSE) {
	if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	if(!(is.character(y_string) & length(y_string)==1L & !is.na(y_string) & y_string %in% colnames(df))) rlang::abort("y_string must be a single, non-NA, string and a valid column name in df.")
	if(!(is.character(x_string) & length(x_string)==1L & !is.na(x_string) & x_string %in% colnames(df))) rlang::abort("x_string must be a non-NA string and a valid column name in df.")
	if(!(is.character(group_string) & length(group_string)==1L & !is.na(group_string) & x_string %in% colnames(df))) rlang::abort("group_string must be a single non-NA string and a valid column name in df.")
	if(!means %in% c(T, F)) rlang::abort("means must be either TRUE or FALSE.")
	if(!all(areColours(user_colours))) rlang::abort("colours must be a character vector of valid colours in hex-format (e.g. #000000).")

	colour_palette <-
		get_colour_set(n_colours_needed = dplyr::n_distinct(df[["category"]]),
					   user_colour_set = user_colours) %>%
		setNames(nm=unique(df[["category"]])) %>% # Could this be part of the function?
		.[1:dplyr::n_distinct(df[["category"]])] # Could this be part of the function?
	fp_text_settings <-
		lapply(colour_palette, function(color) {
			officer::fp_text(font.size=f_size, color=hex_bw(color))}) %>%
		.[1:dplyr::n_distinct(df[["category"]])]

	fp_b <- officer::fp_border(style = "none")
	fp_t <- officer::fp_text(font.size = f_size)
	mschart::ms_barchart(data = df, y = y_string, x=x_string, group = group_string) %>%
		mschart::chart_data_labels(x=., position = "ctr", show_val = T, num_fmt = if(!means) "0%" else NA) %>% # Control rounding as argument
		mschart::chart_settings(x = ., grouping = if(!means) "stacked" else "standard",
								overlap = if(!means) 100 else 0, gap_width= if(!means) 50 else 100, dir="horizontal") %>%
		mschart::chart_labels(x = ., title = "", xlab = "", ylab = "") %>% # Add title here by default?
		mschart::chart_labels_text(x = ., values=fp_text_settings) %>%
		mschart::chart_theme(x = .,
							 axis_title_x = officer::fp_text(font.size = 1),
							 axis_title_y = officer::fp_text(font.size = 1),
							 main_title = officer::fp_text(font.size = 1), # Can be dropped
							 legend_position = "b",
							 axis_text_x = fp_t,
							 axis_ticks_y = fp_b,
							 axis_ticks_x = fp_b,
							 grid_major_line_x = fp_b,
							 grid_major_line_y = fp_b,
							 grid_minor_line_x = fp_b,
							 grid_minor_line_y = fp_b,
							 legend_text = fp_t) %>%
		mschart::chart_data_fill(x=., values=colour_palette) %>%
		mschart::chart_data_stroke(x=., values=colour_palette) %>%
		mschart::chart_ax_y(x = ., limit_min = 0, limit_max = if(!means) 1 else ceiling(max(df[[y_string]])), num_fmt = if(!means) "0%%" else NA)  %>%
		mschart::chart_ax_x(x = ., major_tick_mark = "none", minor_tick_mark = "none")
}


create_report_entry <- function(rownumber,
								y_var, y_label, y_group, y_group_label, y_colour_set, drop_na_y,
								x_var, x_label, x_group, x_group_label, x_colour_set, drop_na_x,
								est, pval, section1, section2, sort_order,
								prefix_number, hide_label_if_less_than, font_size, round_digits, show_n_chart,
								create_barplot1, create_barplot2, create_table1, create_table2,	pptx, docx, xlsx, ...) {


	arg_call <- as.list(match.call(expand.dots = T))
	pb$tick()

	# Early abort, warnings and messages. These could be moved even further up front?
	y_var_unavail <- y_var[!y_var %in% colnames(df)]
	if(length(y_var_unavail)>0L) {
		View(arg_call)
		rlang::abort(message = paste0("Following y_var not found in df", paste0(y_var_unavail, collapse=",")))
	}

	problem_singular <- dplyr::select(df, dplyr::all_of(y_var)) %>% dplyr::select(tidyselect:::where(~all(is.na(.)))) %>% colnames()
	if(length(problem_singular)>0L) {
		rlang::warn(paste0("There is only NA in ", paste0(problem_singular, collapse=","), ". Will skip it."))
		return()
	}



	if(is.null(x_var)) x_var <- NA_character_
	if(!is.na(x_var)) {

		if((!is.character(x_var) | !length(x_var) %in% 0:1)) {
			View(arg_call)
			rlang::abort(message = paste0("Current version only accepts x_var as a single string per row. Problem with: ", paste0(x_var, collapse=",")))
		}
		if(any(!x_var %in% colnames(df))) {
			View(arg_call)
			rlang::abort(message = paste0("Following x_var not found in df:", x_var))
		}

	}

	if(is.na(y_colour_set) || !all(areColors(y_colour_set))) {
		rlang::warn("No valid colours provided. Using default.\n")
	}



	# Create title
	if(length(y_var) == 1L & length(y_label) == 1L) y_group_label <- paste0(y_group_label, " - ", y_label)
	title <- dplyr::case_when(is.na(x_var) || is.na(est) || is.na(pval) ~ y_group_label,
					   est > 0 & pval<.05 ~ paste0(y_group_label, str_pos, paste0(x_label, collapse="_")),
					   est < 0 & pval<.05 ~ paste0(y_group_label, str_neg, paste0(x_label, collapse="_")),
					   pval>=.05 | est == 0 ~ paste0(y_group_label, str_not, paste0(x_label, collapse="_")))

	tab <-
		df %>%
		dplyr::select(all_of(c(unname(y_var), if(!is.na(x_var)) x_var))) %>%
		labelled::set_variable_labels(.labels = setNames(y_label, nm=unname(y_var)) %>% as.list()) %>%
		setNames(nm = ifelse(nchar(var_label(., unlist = T))==0L | colnames(.) %in% x_var,
							 colnames(.), var_label(., unlist = T)))

	if(!is.na(x_var) && length(y_var)>1) {
		rlang::warn("When x_var is specified, barcharts are currently limited to single-item batteries. Omitting barchart1")
	} else {


		tab_long1 <-
			tab %>%
			{if(is.na(x_var)) tidyr::pivot_longer(., cols = dplyr::all_of(y_label), names_to = "var", values_to = "val") else {
				dplyr::rename(., val={y_label}, var={x_var})}} %>%
			dplyr::mutate(var = labelled::to_character(var),
						  category = labelled::to_character(val),
						  val = as.integer(haven::as_factor(val))) %>% # Problem here if first variable lacks a category
			{if(drop_na_y) dplyr::filter(., !is.na(category)) else dplyr::mutate(., category = dplyr::if_else(is.na(category), "<NA>", category))} %>%
			{if(drop_na_y) dplyr::filter(., !is.na(val)) else .} %>%
			{if(drop_na_x & !is.na(x_var)) dplyr::filter(., !is.na(var)) else dplyr::mutate(., var = dplyr::if_else(is.na(var), "<NA>", var))} %>%
			dplyr::add_count(var, name = "n_per_var") %>%
			dplyr::count(var, n_per_var, val, category, name = "n_per_var_val") %>%
			dplyr::mutate(percent = round(n_per_var_val/n_per_var, 3)) %>%
			{if(show_n_chart=="axis" |
				(show_n_chart=="auto" &
				 dplyr::n_distinct(.[["n_per_var"]]) > 1L)) tidyr::unite(., col = "var", c(var, n_per_var), sep = " (N=", remove = T, na.rm = T) %>%
					dplyr::mutate(var=paste0(var, ")")) else .} %>%
			{if(prefix_number) tidyr::unite(., col = "category", c(val, category), sep = ": ", remove = F) else .} %>%
			dplyr::arrange(val) %>%
			dplyr::mutate(category = forcats::fct_inorder(category, ordered = T)) %>% # Problem here if first variable lacks a category
			dplyr::arrange(var, val)


		### THIS WILL FAIL IF NO tab_long1 HAS BEEN CREATED BUT NEEDED LATER!
		caption_suffix <- paste0(". ", title,
								 if(show_n_chart=="caption" |
								    (show_n_chart == "auto" & dplyr::n_distinct(range(tab_long1$n_per_var))==1L)) {
								 	paste0(" N=[", paste0(unique(range(tab_long1$n_per_var)), collapse="-"), "]")
								 })

		# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
		# {if(sort=="sum_upper_categories") dplyr::group_by(., var) %>%
		# 		dplyr::mutate(sort_var = ifelse(category = )) %>%
		# 		dplyr::arrange(if(desc) desc(sort_var) else sort_var, val) else .} %>%
		# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
		# {if(sort=="alphabetical") dplyr::arrange(., if(desc) desc(var) else var, val) else .} %>%
		#"average", "sum_upper_categories", "alphabetical", "sum_lower_categories", "significance",

		if(create_barplot1) {


			barplot1 <- ms_percentbar(df = tab_long1, y_string = "percent", x_string="var",
									  means = FALSE, f_size = font_size, user_colours=y_colour_set)

			if(docx) {
				n_vars1 <- length(unique(barplot1[["data"]][["var"]]))
				doc <<-
					mschart::body_add_chart(x = doc, width = img_width, height = min(img_height_max, n_vars1*7/25+150/100), style = "NIFU_Figuranker_bred",
											chart = mschart::chart_labels_text(x = barplot1, values=officer::fp_text(font.size=5))) %>%
					officer::body_add_par(x = ., value = paste0(str_figure_tag, rownumber, if(create_barplot2) "a", caption_suffix), style = "NIFU_Figur tittel") %>%
					officer::body_add_par(x = ., value = "", style = "NIFU_Normal første")
			}

			if(pptx) {
				ppt <<-
					officer::add_slide(ppt, layout = "Tittel og innhold", master = "NIFU_ppt_NO") %>%
					officer::ph_with(x = ., value = barplot1, location = officer::ph_location_fullsize()) %>%
					officer::ph_with(x = ., value = paste0(str_figure_tag, rownumber, if(create_barplot2) "a", caption_suffix),
									 location = officer::ph_location_type(type = "ftr"))
			}
		}


		if(create_table1) {
			table1 <-
				tab_long1 %>%
				dplyr::select(-val, -n_per_var_val) %>%
				dplyr::mutate(percent = percent*100) %>%
				{if(show_n_table != "axis") select(., -n_per_var) else dplyr::rename(., N=n_per_var)} %>%
				dplyr::rename_with(.cols = var, .fn = ~ y_group_label) %>%
				tidyr::pivot_wider(names_from = category, values_from = percent) %>%
				{if(show_p_table == "axis") dplyr::rename_with(., .cols = -1, ~ paste0(., " (%)")) else .}

			if(xlsx) {
				table1 <<-
					officer::add_sheet(x = table1, label = paste0(y_var, if(!is.na(x_var)) x_var, collapse="_"))
			}

			df_display1 <-
				table1 %>%
				dplyr::mutate(dplyr::across(tidyselect:::where("is.numeric"),
											~gsub("\\.", ",", round(replace(., is.na(.), str_blank), digits = round_digits))))

			if(docx) {

				doc <<-
					officer::body_add_par(x = doc, value = paste0(str_table_tag, rownumber, if(create_table2) "a", caption_suffix, if(show_p_table=="caption") " (%)"), style = "NIFU_Tabell tittel") %>%
					officer::body_add_table(x = .,value = df_display1, style = "NIFU_Tabell 1", header = T, alignment = c("l", rep("r", ncol(table1)-1)),
											stylenames = officer::table_stylenames(stylenames = setNames(rep("NIFU_Tabell kropp", ncol(table1)), colnames(table1)))) %>%
					officer::body_add_par(x = ., value = "", style = "NIFU_Normal første")
			}
			if(pptx) {

				ppt <<-
					officer::add_slide(ppt, layout = "Tittel og innhold", master = "NIFU_ppt_NO") %>%
					officer::ph_with(x = ., value = df_display1,
									 location = officer::ph_location_type(), alignment = c("l", rep("c", ncol(table1)-1))) %>%
					officer::ph_with(x = ., value = paste0(str_table_tag, rownumber, if(create_table2) "a", caption_suffix, if(show_p_table=="caption") " (%)"),
									 location = officer::ph_location_type(type = "ftr"))
			}

		}
	}

	if(!is.na(x_var) & create_barplot2) {

		tab_long2 <-
			tab %>%
			dplyr::mutate(dplyr::across(.cols = -dplyr::all_of(x_var), .fns = ~as.integer(haven::as_factor(.)))) %>%
			dplyr::mutate(dplyr::across(.cols = dplyr::all_of(x_var), .fns = ~labelled::to_character(.))) %>%
			dplyr::group_by(category=.data[[x_var]]) %>%
			dplyr::summarize(across(.cols = dplyr::all_of(y_label), .fns = ~round(mean(., na.rm=T), digits = round_digits+1)), n_per_var=dplyr::n()) %>%
			dplyr::ungroup() %>%
			tidyr::pivot_longer(cols = dplyr::all_of(y_label), names_to = "var", values_to = "val")  %>%
			{if(drop_na_y) dplyr::filter(., !is.na(category)) else dplyr::mutate(., category = dplyr::if_else(is.na(category), "<NA>", category))} %>%
			{if(drop_na_y) dplyr::filter(., !is.na(val)) else .} %>%
			{if(drop_na_x) dplyr::filter(., !is.na(var)) else dplyr::mutate(., var = dplyr::if_else(is.na(var), "<NA>", var))} %>%
			{if(show_n_chart=="axis" | (show_n_chart=="auto" & dplyr::n_distinct(.[["n_per_var"]])>1L)) tidyr::unite(., col = "var", c(var, n_per_var), sep = " (N=", remove = T, na.rm = T) %>%
					dplyr::mutate(var=paste0(var, ")")) else .} %>%
			dplyr::arrange(var, category)

		caption_suffix <- paste0(". ", title,
								 if(show_n_chart=="caption" |
								    (show_n_chart == "auto" & dplyr::n_distinct(range(tab_long2$n_per_var))==1L)) {
								 	paste0(" N=[", paste0(unique(range(tab_long2$n_per_var)), collapse="-"), "]")
								 })


		barplot2 <- ms_percentbar(df = tab_long2, y_string = "val", x_string="var",
								  user_colours=x_colour_set, f_size = font_size, means=TRUE)

		if(docx) {
			n_vars2 <- dplyr::n_distinct(barplot2[["data"]][["var"]])
			doc <<-
				mschart::body_add_chart(x = doc, width = img_width, height = min(img_height_max, n_vars2*7/25+150/100), chart = barplot2, style = "NIFU_Figuranker_bred") %>%
				officer::body_add_par(x = ., value = paste0(str_figure_tag, rownumber, if(create_barplot1) "b", caption_suffix), style = "NIFU_Figur tittel")
		}

		if(pptx) {
			ppt <<-
				officer::add_slide(ppt, layout = "Tittel og innhold", master = "NIFU_ppt_NO") %>%
				officer::ph_with(x = ., value = barplot2, location = officer::ph_location_fullsize()) %>%
				officer::ph_with(x = ., value = paste0(str_figure_tag, rownumber, if(create_barplot1) "b", caption_suffix),
								 location = officer::ph_location_type(type = "ftr"))
		}
	}

	### Three-level table for layered chart (manually constructed)
	if(!is.na(x_var) & length(y_var)>1L & create_table2) {
		table2 <-
			tab %>%
			tidyr::pivot_longer(cols = dplyr::all_of(y_label), names_to = "var", values_to = "val") %>%
			dplyr::mutate(var = labelled::to_character(var),
						  category = labelled::to_character(val),
						  val = as.integer(haven::as_factor(val))) %>% # Problem here if first variable lacks a category
			{if(drop_na_y) dplyr::filter(., !is.na(category)) else dplyr::mutate(., category = dplyr::if_else(is.na(category), "<NA>", category))} %>%
			{if(drop_na_y) dplyr::filter(., !is.na(val)) else .} %>%
			{if(drop_na_x & !is.na(x_var)) dplyr::filter(., !is.na(var)) else dplyr::mutate(., var = dplyr::if_else(is.na(var), "<NA>", var))} %>%
			dplyr::add_count(var, .data[[x_var]], name = "n_per_var_var2") %>%
			dplyr::count(var, .data[[x_var]], n_per_var_var2, val, category, name = "n_per_var_val_var2") %>%
			dplyr::mutate(percent = round(n_per_var_val_var2/n_per_var_var2*100, round_digits)) %>%
			{if(show_n_chart=="axis" | (show_n_chart=="auto" & dplyr::n_distinct(.[["n_per_var_var2"]])>1L)) {
				tidyr::unite(., col = "var", c(var, n_per_var), sep = " (N=", remove = T, na.rm = T) %>%
					dplyr::mutate(var=paste0(var, ")"))} else .} %>%
			{if(prefix_number) tidyr::unite(., col = "category", c(val, category), sep = ": ", remove = F) else .} %>%
			dplyr::arrange(val) %>%
			dplyr::mutate(category = forcats::fct_inorder(category, ordered = T)) %>% # Problem here if first variable lacks a category
			{if(show_p_table == "axis") dplyr::mutate(., category = paste0(category, " (%)")) else .} %>%
			{if(show_n_table == "axis") dplyr::rename(., N=n_per_var_var2) else dplyr::select(., -n_per_var_var2)} %>%
			dplyr::arrange(dplyr::all_of(x_var), var, val) %>%
			dplyr::select(-n_per_var_val_var2, -val)  %>%
			tidyr::pivot_wider(names_from = category, values_from = percent) %>%
			dplyr::relocate(dplyr::all_of(c(x_var, "var"))) %>%
			dplyr::rename_with(.cols = var, .fn = ~ y_group_label)

		if(docx) {
			## Can this line be generalized?
			df_display2 <- dplyr::mutate(table2, dplyr::across(tidyselect:::where("is.numeric"), ~gsub("\\.", ",", round(replace(., is.na(.), str_blank), digits = round_digits))))

			doc <<-
				officer::body_add_par(x = doc, value = paste0(str_table_tag, rownumber, if(create_table2) "b", ". ", title, if(show_p_table=="caption") " (%)"), style = "NIFU_Tabell tittel") %>%
				officer::body_add_table(x = .,value = df_display2, style = "NIFU_Tabell 1", header = T, alignment = c("l", rep("r", ncol(table2)-1)),
										stylenames = officer::table_stylenames(stylenames = setNames(rep("NIFU_Tabell kropp", ncol(table2)), colnames(table2)))) %>%
				officer::body_add_par(x = ., value = "", style = "NIFU_Normal første")
		}

		if(xlsx) {
			xls <<-
				officer::add_sheet(x = xls, label = paste0(rownumber, paste0(y_var, collapse="_"), if(!is.na(x_var)) x_var, sep="x"))
		}

		# colour_palette3 <-
		# 	get_colour_set(n_colours_needed = n_distinct(tab_long3$category),
		# 					 user_colour_set = if(battery %in% nominal_vars) y_colour_set_nominal else y_colour_set) %>%
		# 	setNames(nm=unique(tab_long3$category)) %>%
		# 	.[1:dplyr::n_distinct(tab_long3$category)]
		#
		# fp_text_settings3 <-
		# 	lapply(colour_palette3, function(color) {
		# 		officer::fp_text(font.size=font_size, color=hex_bw(color))}) %>%
		# 	.[1:dplyr::n_distinct(tab_long3$category)]

		# chart3 <-
		# 	mschart::ms_barchart(data = tab_long2, y = "val", x="var", group = "category") %>%
		# 	mschart::chart_data_labels(x=., position = "ctr", show_val = T) %>%
		# 	mschart::chart_settings(x = .,   dir="horizontal") %>%
		# 	mschart::chart_labels(x = ., title = "", xlab = "", ylab = "") %>%
		# 	mschart::chart_labels_text(x = ., values=fp_text_settings2) %>%
		# 	mschart::chart_theme(x = ., legend_position = "b",
		# 						 axis_text_x = officer::fp_text(font.size = font_size),
		# 						 axis_ticks_y = officer::fp_border(style = "none"),
		# 						 axis_ticks_x = officer::fp_border(style = "none"),
		# 						 grid_major_line_x = officer::fp_border(style = "none"),
		# 						 grid_major_line_y = officer::fp_border(style = "none"),
		# 						 grid_minor_line_x = officer::fp_border(style = "none"),
		# 						 grid_minor_line_y = officer::fp_border(style = "none"),
		# 						 legend_text = officer::fp_text(font.size = font_size)) %>%
		# 	mschart::chart_data_fill(x=., values=colour_palette2) %>%
		# 	mschart::chart_data_stroke(x=., values=colour_palette2) %>%
		# 	mschart::chart_ax_y(x = ., limit_min = 0) %>%
		# 	mschart::chart_ax_x(x = ., minor_tick_mark = "none")
	}
	mget(x = ls())
}



# What is this for?
reverse_unlabeling <- function(var) {
	val <- as.integer(gsub("\\[([0-9]*)\\].*", "\\1", var))
	label <- stringr::str_trim(gsub("\\[[0-9]*\\] (.*)", "\\1", var))
	print(unique(val))
	print(unique(label))

	factor(val, levels = unique(val)[!is.na(unique(val))], labels=unique(label)[!is.na(unique(label))])
}

print2.rdocx <- function(x, target=file.path(getwd(), "tmp.docx")) {
	# Unzip everything in temp, recode chart files to UTF-8 and return
	current_wd <- getwd()
	dir.create(tmp_zip_dir <- tempfile())
	tmp_zip <- tempfile(fileext = ".docx")
	print(doc, target = tmp_zip)

	utils::unzip(zipfile = tmp_zip, exdir = tmp_zip_dir) %>%
		grep(pattern = "charts\\/.*\\.xml$", x = ., value = T) %>%
		purrr::map_chr(., function(xml_file) {
			xml_content <-
				readr::read_file(xml_file) %>%
				iconv(x = ., from = "latin1", to = "UTF-8") %>%
				gsub('<c:title xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">\r\n      <c:tx>\r\n        <c:rich>\r\n          <a:bodyPr/>.*<c:autoTitleDeleted val=\"0\"/>',
					 replacement = '<c:autoTitleDeleted val="1"/>', x = .)
			readr::write_file(x = xml_content, file = xml_file, append = F)
		})

	#### Replace with officer::pack_folder(folder=tmp_zip_dir, target=file.path(path, paste0(file_prefix, ".docx")))
	setwd(tmp_zip_dir)
	list.files(path = tmp_zip_dir, all.files = T, recursive = T, include.dirs = F) %>%
		utils::zip(files=., zipfile = tmp_zip)
	setwd(current_wd)
	file.copy(from = tmp_zip, to = target, overwrite = T, copy.date = T)
	target
}
