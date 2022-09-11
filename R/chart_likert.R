#' Create Likert Chart from Descriptives Data Frame
#'
#' @param data Data frame or tibble.
#' @param y [\code{character(1)}]\cr Name of column in data for .
#' @param x [\code{character(1)}]\cr Name of column in data for
#' @param group [\code{character(1)}]\cr Name of column in data for
#' @param labels [\code{character(1)}]\cr Name of column in data for labels.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' @param colour_palette [\code{character()}]\cr
#' @param seed Optional random seed for selection of colours in blender.
#' Must contain at least the number of unique values (incl. missing) in the data set.
#'
#' @return mschart-object. Can be added to an rdocx, rpptx or rxlsx object.
#' @export
#'
#' @examples
#' data(ex_survey1)
#' dat <- ex_survey1[,c("b_1", "b_2", "b_3")]
#' dat_freq <- prepare_freq_data(dat)
#' create_chart_likert(dat_freq)
create_chart_likert <-
  function(data,
           y="n", x="name", group="value", labels="label",
           label_font_size = 10,
           main_font_size = 8,
           colour_palette = NULL,
           seed=1) {

    coll <- checkmate::makeAssertCollection()
    checkmate::assert_string(y, add = coll)
    checkmate::assert_string(x, add = coll)
    checkmate::assert_string(group, add = coll)
    checkmate::assert_string(labels, add = coll)
    checkmate::assert_string(labels, add = coll)
    checkmate::assert_number(label_font_size, lower = 0, upper = 72, finite = TRUE, add = coll)
    checkmate::assert_number(main_font_size, lower = 0, upper = 72, finite = TRUE, add = coll)
    checkmate::assert_data_frame(data, add = coll)
    checkmate::assert_subset(x = colnames(data), choices = c(y,x,group,labels), add = coll)
    if(!is.null(colour_palette) & !all(is_colour(colour_palette))) {
      cli::cli_abort(
        c("Invalid user-specified colours.",
          i="{.arg colour_palette} must be a character vector of valid colours in hex-format (e.g. #000000)."))
    }
    checkmate::reportAssertions(coll)

    colour_palette <-
        get_colour_set(n_colours_needed = dplyr::n_distinct(data[[group]], na.rm = FALSE),
                       user_colour_set = colour_palette,
                       seed = seed)
    colour_palette <-
      rlang::set_names(colour_palette, nm=unique(data[[group]]))


    fp_text_settings <-
      lapply(colour_palette,
             function(color) {
               officer::fp_text(font.size=label_font_size, color=hex_bw(color))
             })
    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(data[[group]]))]

    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(font.size = main_font_size)

    m <- mschart::ms_barchart(data = data,
                              y = y, x = x, group = group, labels = labels)
    m <- mschart::as_bar_stack(x = m, dir = "horizontal", percent = T)
    m <- mschart::chart_data_fill(x = m, values = colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = colour_palette)
    m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")
    m <- mschart::chart_ax_y(x = m, num_fmt = "0%%")
    m <- mschart::chart_theme(x = m,
                              legend_text = main_text,
                              axis_text_x = main_text,
                              axis_text_y = main_text,
                              grid_major_line_x = blank_border,
                              grid_major_line_y = blank_border,
                              grid_minor_line_x = blank_border,
                              grid_minor_line_y = blank_border)
    m
  }




#' Create Word Report with Univariates for Categorical Columns Sharing Same Categories
#'
#' @param data Data frame or tibble.
#' @param cols <tidy-select> Columns to select for reporting.
#' @param docx_template  [\code{character(1) || officer::read_docx()}]\cr
#' Either a filepath to a template file, or a rdocx-object.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' @param colour_palette [\code{character()}]\cr
#' Must contain at least the number of unique values (incl. missing) in the data set.
#' @param chart_formatting [\code{integer(1)}]\cr
#' Which template style to be used for formatting chart?
#' @param height_per_col [\code{numeric(1)>0}]\cr Height in cm per chart entry.
#' @param height_fixed [\code{numeric(1)>0}]\cr Fixed height in cm.
#' @param seed Optional random seed for selection of colours in blender.
#'
#' @return
#' @export
#'
#' @examples
report_chart_likert <-
	function(data,
			 cols,
			 docx_template = NULL,
			 label_font_size = 8,
			 colour_palette = NULL,
			 chart_formatting = NULL,
			 height_per_col = .3,
			 height_fixed = 1,
			 main_font_size = 8,
			 seed = 1) {

	  if(!inherits(data, what = "data.frame")) {
	    cli::cli_abort("data must be a {.cls data.frame} or {.cls tibble}.")
	  }

		cols_enq <- rlang::enquo(cols)
		cols_pos <- tidyselect::eval_select(cols_enq, data = data)


		check_category_pairs(data = data, cols_pos = cols_pos)



		prepared_data <- data[, cols_pos]
		prepared_data <-
		  prepare_freq_data(data = data)

		docx_file <- use_docx(docx_template = docx_template)
		docx_dims <- officer::docx_dim(docx_file)
		docx_dims <- c(w =
		                 docx_dims$page[["width"]] -
		                 docx_dims$margins[["left"]] -
		                 docx_dims$margins[["right"]],
		               h =
		                 docx_dims$page[["height"]] -
		                 docx_dims$margins[["top"]] -
		                 docx_dims$margins[["bottom"]])

		chart <-
		  create_chart_likert(data = prepared_data,
		                      y = "n", x = "name", group = "value", labels = "label",
		                      label_font_size = label_font_size,
		                      colour_palette = colour_palette,
		                      main_font_size = main_font_size,
		                      seed = seed)

		determine_height <-
		  min(c(height_fixed + height_per_col*length(cols_pos),
		        docx_dims[["h"]]))

		mschart::body_add_chart(x = docx_file,
		                        chart = chart,
		                        style = chart_formatting,
		                        pos = "after",
		                        width = docx_dims[["w"]],
		                        height = determine_height)

		docx_file
	}
