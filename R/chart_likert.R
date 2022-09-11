
chart_likert <- 
	function(.data,
			 cols,
			 by = NULL,
			 docx_template = NULL,
			 font_size = 8,
			 user_colours = NULL) {
		
		if(missing(cols)) cols <- tidyselect::everything()
		cols_enq <- rlang::enquo(cols)
		cols_pos <- tidyselect::eval_select(cols_enq, data = .data)
		by_enq <- rlang::enquo(by)
		by_pos <- tidyselect::eval_select(by_enq, data = .data)
		
		
		if(!inherits(.data, what = "data.frame")) {
			cli::cli_abort(".data must be a {.cls data.frame} or {.cls tibble}.")
		}
		###  Check that all pairs of cols share at least one observed response category
		check_category_pairs <-
			function(cols_pos, call = rlang::caller_env()) {
				purrr::walk2(.x = unname(cols_pos),
							 .y = names(cols_pos),
							 .f = ~{
							 	cols_rest <- 
							 		cols_pos[-c(1:match(.y, names(cols_pos)))]
							 	purrr::walk2(.x = unname(cols_rest),
							 				 .y = names(cols_rest),
							 				 .f = function(.x2, .y2) {
							 				 	common <-
							 				 		dplyr::intersect(
							 				 			levels(.data[[.y]]),
							 				 			levels(.data[[.y2]]))
							 				 	if(length(common)==0L) {
							 				 		cli::cli_abort(
							 				 			c("Unequal variables.",
							 				 			  "!" = "All variables must share at least one common category.",
							 				 			  "i" = "Column {.var {.y}} and column {.var {.y2}} lack common categories."),
							 				 			call = call)
							 				 	}
							 				 })
							 })
			}
		check_category_pairs(cols_pos)
		
		if(!is.null(user_colours) & !all(is_colour(user_colours))) {
			cli::cli_abort(c("Invalid user-specified colours.", 
							 "{.arg user_colours} must be a character vector of valid colours in hex-format (e.g. #000000)."))
		}
							 
							 if(is.null(unlist(user_colours))) {
							 	
							 	colour_palette <-
							 		get_colour_set(n_colours_needed = dplyr::n_distinct(df[["category"]]))
							 } else {
							 	colour_palette <- user_colours
							 }
							 colour_palette <-
							 	rlang::set_names(colour_palette, nm=unique(df[["category"]])) 
							 
							 
							 fp_text_settings <-
							 	lapply(colour_palette, function(color) {
							 		officer::fp_text(font.size=f_size, color=hex_bw(color))}) %>%
							 	.[seq_len(dplyr::n_distinct(df[["category"]]))]
							 
							 
							 add_chart_likert <- 
							 	function(cols_pos) {
							 		.data %>%
							 			tidyr::pivot_longer(cols = cols) %>%
							 			count(name, value) %>%
							 			group_by(name) %>%
							 			mutate(n = n/sum(n, na.rm=TRUE)*100,
							 				   label = sprintf("%.1f%%", n)) %>%
							 			ungroup() %>%
							 			mschart::ms_barchart(data = ., y = "n", x = "name", group = "value", labels = "label") %>%
							 			mschart::as_bar_stack(dir = "horizontal", percent = T) %>%
							 			mschart::chart_labels_text(values = officer::fp_text(font.size = font_size)) %>%
							 			mschart::chart_data_fill(values = colour_palette) %>%
							 			mschart::chart_data_stroke(values = colour_palette) %>%
							 			mschart::chart_labels(ylab = NULL, xlab = NULL, title = NULL) %>%
							 			mschart::chart_ax_x(major_tick_mark = "none") %>%
							 			mschart::chart_ax_y(num_fmt = "0%%") %>%
							 			mschart::chart_theme(grid_major_line_x = officer::fp_border(style = "none"),
							 								 grid_major_line_y = officer::fp_border(style = "none"),
							 								 grid_minor_line_x = officer::fp_border(style = "none"),
							 								 grid_minor_line_y = officer::fp_border(style = "none"))
							 	}
							 
							 add_chart_likert_by <-
							 	function(by_pos) {
							 		
							 	}
							 
							 if(!is.null(docx_template)) {
							 	if(is.character(docx_template) && fs::file_exists(docx_template)) {
							 		docx_file <- officer::read_docx(path = docx_template)
							 	} else docx_file <- docx_template
							 } else docx_file <- officer::read_docx()
							 
							 mschart::body_add_chart(x = docx_file, pos = "after", width = 5.6, height = .7*length(cols_pos),
							 						chart = add_chart_likert(cols_pos=cols_pos))
							 
							 if(!is.null(by)) {
							 	purrr::walk2(.x = unname(by_pos),
							 				 .y = names(by_pos),
							 				 .f = ~{
							 				 	add_chart_likert_by(by_pos = .x)
							 				 })
							 }
							 docx_file
		}
		
		
		tmp <-
			tibble::tibble(
				a1 = as.factor(sample(c("a","b","c","d"), size = 200, replace = TRUE)),
				a2 = as.factor(sample(c("a","b","c","d"), size = 200, replace = TRUE)),
				a3 = as.factor(sample(c("a","b","c","d"), size = 200, replace = TRUE)),
				a4 = as.factor(sample(c("e", "f"), size = 200, replace = TRUE)))
		
		testthat::expect_error(object = chart_likert(mtcars, cols = c(cyl, vs, gear, carb)), 
							   regexp = "Unequal variables")
		testthat::expect_error(object = chart_likert(tmp, cols = matches("a")), 
							   regexp = "Unequal variables")
		
		chart_likert(tmp, cols = matches("a[123]")) %>%
			officer:::print.rdocx(target = "test.docx")
		
		
		