
#' Are All Colours in Vector Valid Colours
#' 
#' As title says. From: http://stackoverflow.com/a/13290832/3315962
#' 
#' @param x Character vector of colours in hex-format.
#'
#' @return Logical, or error.
#' @export
#'
#' @examples
#' is_colour(c("#ff00ff", "#010101"))
is_colour <- function(x) {
	sapply(x, function(X) { # Avoid sapply
		tryCatch(is.matrix(col2rgb(X)),
				 error = function(e) FALSE)
	})
}

#' Identify Suitable Font Given Background Hex Colour
#' 
#' Code is taken from XXX.
#' 
#' @param hex_code Colour in hex-format.
#'
#' @return Colours in hex-format, either black or white.
#' @importFrom grDevices col2rgb
#' @export
#'
#' @examples
#' hex_bw("#0dadfd")
hex_bw <- function(hex_code) {
	
	myrgb <- as.integer(col2rgb(hex_code))
	
	rgb_conv <- lapply(myrgb, function(x) {
		i <- x / 255
		if (i <= 0.03928) i / 12.92 else ((i + 0.055) / 1.055) ^ 2.4
	})
	rgb_calc <- (0.2126*rgb_conv[[1]]) + (0.7152*rgb_conv[[2]]) + (0.0722*rgb_conv[[3]])
	
	if (rgb_calc > 0.179) return("#000000") else return("#ffffff")
	
}

#' Provide A Colour Set for A Number of Requested Colours
#' 
#' Possibly using user_colour_set if available. If not sufficient, uses a set
#'     palette from RColorBrewer.
#'
#' @param n_colours_needed Number of colours needed.
#' @param user_colour_set User-supplied default palette.
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom stats median
#' @return A colour set as character vector.
#' @export
#' @examples
#' get_colour_set(n_colours_needed=4)
get_colour_set <- function(n_colours_needed, user_colour_set=NULL) {

	if(!is.null(user_colour_set) &&
	   length(user_colour_set) >= n_colours_needed &&
	   all(is_colour(user_colour_set))) {
		if(length(user_colour_set)==7L) {
			x <- 1:length(user_colour_set)
			if(n_colours_needed==7L) return(user_colour_set)
			if(n_colours_needed==6L) return(user_colour_set[c(1:3, 5:length(x))])
			if(n_colours_needed==5L) return(user_colour_set[c(1:2, stats::median(x), 6:length(x))])
			if(n_colours_needed==4L) return(user_colour_set[c(1,3, 5,length(x))])
			if(n_colours_needed==3L) return(user_colour_set[c(1, stats::median(x),length(x))])
			if(n_colours_needed==2L) return(user_colour_set[c(1, length(x))])
		} else return(user_colour_set)
	} else {
			set.seed(1)
			colour_map <- RColorBrewer::brewer.pal.info
			qual_col_pals <- colour_map[colour_map$category == 'qual' & colour_map$colorblind,]
			out <- mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))
			out <- sample(unlist(out), n_colours_needed)
	}
}

#' Create a MS Chart Horizontal Percent Barplot
#' 
#' Mostly an internal function.
#' 
#' @param df Data frame
#' @param y_var,x_var Variables for y and x axes. 
#' @param group_string Category to group on.
#' @param user_colours Character vector with hex colours.
#' @param f_size Font size, numeric value.
#' @param means Logical, default is FALSE. Whether to show means or percent.
#' @importFrom rlang abort warn set_names
#' @importFrom dplyr n_distinct %>%
#' @importFrom officer fp_text fp_border
#' @importFrom mschart ms_barchart chart_data_labels chart_settings chart_labels chart_labels_text chart_theme chart_ax_y chart_ax_x chart_data_stroke chart_data_fill
#' @return A mschart object for a rdocx.
#' @export
#' @examples
ms_percentbar <- function(df, y_var, x_var, group_string="category", user_colours="#000000", f_size=8, means=FALSE) {
	if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	if(!(is.character(y_var) & length(y_var)==1L & !is.na(y_var) & y_var %in% colnames(df))) rlang::abort("y_var must be a single, non-NA, string and a valid column name in df.")
	if(!(is.character(x_var) & length(x_var)==1L & !is.na(x_var) & x_var %in% colnames(df))) rlang::abort("x_var must be a non-NA string and a valid column name in df.")
	if(!(is.character(group_string) & length(group_string)==1L & !is.na(group_string) & x_var %in% colnames(df))) rlang::abort("group_string must be a single non-NA string and a valid column name in df.")
	if(!means %in% c(TRUE, FALSE)) rlang::abort("means must be either TRUE or FALSE.")
	if(!all(is_colour(user_colours))) rlang::abort("colours must be a character vector of valid colours in hex-format (e.g. #000000).")
	
	colour_palette <-
		get_colour_set(n_colours_needed = dplyr::n_distinct(df[["category"]]),
					   user_colour_set = user_colours) %>%
		rlang::set_names(nm=unique(df[["category"]])) %>% # Could this be part of the function?
		.[1:dplyr::n_distinct(df[["category"]])] # Could this be part of the function?
	fp_text_settings <-
		lapply(colour_palette, function(color) {
			officer::fp_text(font.size=f_size, color=hex_bw(color))}) %>%
		.[1:dplyr::n_distinct(df[["category"]])]
	
	fp_b <- officer::fp_border(style = "none")
	fp_t <- officer::fp_text(font.size = f_size)
	mschart::ms_barchart(data = df, y = y_var, x=x_var, group = group_string) %>%
		mschart::chart_data_labels(x=., position = "ctr", show_val = TRUE, num_fmt = if(!means) "0%" else NA) %>% # Control rounding as argument
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
		mschart::chart_ax_y(x = ., limit_min = 0, limit_max = if(!means) 1 else ceiling(max(df[[y_var]])), num_fmt = if(!means) "0%%" else NA)  %>%
		mschart::chart_ax_x(x = ., major_tick_mark = "none", minor_tick_mark = "none")
}