#' Create Likert Chart from Descriptives Data Frame
#'
#' @param data Data frame or tibble.
#' @param y [\code{character(1)}]\cr Name of column in data for .
#' @param x [\code{character(1)}]\cr Name of column in data for
#' @param group [\code{character(1)}]\cr Name of column in data for
#' @param labels [\code{character(1)}]\cr Name of column in data for labels.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' Must contain at least the number of unique values (excl. missing) in the data set.
#' @param colour_palette [\code{character()}]\cr
#' Must contain at least the number of unique values (incl. missing) in the data set.
#' @param colour_na [\code{character(1)}]\cr Colour as a single string.
#' @param colour_2nd_binary_cat [\code{character(1)}]\cr Colour for second category in binary variables. Often useful to hide this.
#' @param font_family Word font family. See officer::fp_text
#' @param vertical Logical. If FALSE (default), then horizontal.
#' @param what [\code{character(1)}] Either "percent" or "frequency". Supports partial matching.
#' @param seed Optional random seed for selection of colours in blender.
#'
#' @importFrom crosstable crosstable
#'
#' @return mschart-object. Can be added to an rdocx, rpptx or rxlsx object.
#'
#' @examples
#' #create_chart_likert(prepare_data_for_mschart(ex_survey1[paste0("b_", 1:3)]))
create_chart_likert <-
  function(data,
           y="value", x="label", group="variable", labels="data_label",
           label_font_size = 10,
           main_font_size = 8,
           font_family = "Calibri",
           colour_palette = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           vertical = FALSE,
           what = "percent",
           seed = 1) {

    coll <- checkmate::makeAssertCollection()
    checkmate::assert_string(y, add = coll)
    checkmate::assert_string(x, add = coll)
    checkmate::assert_string(group, add = coll)
    checkmate::assert_string(labels, add = coll)
    checkmate::assert_character(colour_palette, null.ok = TRUE, add = coll)
    checkmate::assert_string(colour_na, na.ok = TRUE, null.ok = TRUE, add = coll)
    checkmate::assert_number(label_font_size, lower = 0, upper = 72, finite = TRUE, add = coll)
    checkmate::assert_number(main_font_size, lower = 0, upper = 72, finite = TRUE, add = coll)
    checkmate::assert_number(seed, lower = 1, finite = TRUE, add = coll)
    checkmate::assert_data_frame(data, add = coll)
    checkmate::assert_logical(vertical, len = 1, any.missing = FALSE, add = coll)
    checkmate::assert_subset(x = colnames(data), choices = c(y,x,group,labels, ".id", "cat_id", "sum_value"), add = coll)
    if(!is.null(colour_palette) & !all(is_colour(colour_palette))) {
      cli::cli_abort(
        c("Invalid user-specified colours.",
          i="{.arg colour_palette} must be a character vector of valid colours in hex-format (e.g. #000000)."))
    }

    checkmate::reportAssertions(coll)

    colour_palette <-
      get_colour_set(n_colours_needed = length(levels(data[[group]])),
                     user_colour_set = colour_palette,
                     seed = seed)

    colour_palette <-
      rlang::set_names(colour_palette, nm=levels(data[[group]]))

    if(!is.null(colour_na) && !is.na(colour_na)) {
      colour_palette[names(colour_palette)=="NA"] <- colour_na
    }

    if(length(levels(data[[group]]))==2L &&
       !is.null(colour_2nd_binary_cat)
       && is_colour(colour_2nd_binary_cat)) {
      colour_palette[2] <- colour_2nd_binary_cat
    }



    fp_text_settings <-
      lapply(colour_palette,
             function(color) {
               officer::fp_text(font.size = label_font_size,
                                color = hex_bw(color),
                                font.family = font_family)
             })
    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(data[[group]]))]

    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(font.size = main_font_size, font.family = font_family)

    m <- mschart::ms_barchart(data = data, y = y, x = x,
                              group = group, labels = labels)

    if(grepl("per", what)) {
      m <- mschart::as_bar_stack(x = m,
                                 dir = if(vertical) "vertical" else "horizontal",
                                 percent = TRUE)
    }
    m <- mschart::chart_data_fill(x = m, values = colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = colour_palette)
    m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")
    if(grepl("per", what)) m <- mschart::chart_ax_y(x = m, num_fmt = "0%%")
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
#' @param showNA Whether to show NA in categorical variables (one of c("ifany", "always", "no"), like in table()).
#' @param docx_template  [\code{character(1) || officer::read_docx()}]\cr
#' Either a filepath to a template file, or a rdocx-object.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' @param font_family Office font family. Defaults to "Arial". See ?officer::fp_text() for options.
#' @param colour_palette [\code{character()}]\cr
#' Must contain at least the number of unique values (incl. missing) in the data set.
#' @param colour_na [\code{character(1)}]\cr Colour as a single string, for NA-values.
#' @param colour_2nd_binary_cat [\code{character(1)}]\cr Colour for second category in binary variables. Often useful to hide this.
#' @param chart_formatting [\code{integer(1)}]\cr
#' Which template style to be used for formatting chart?
#' @param height_per_col [\code{numeric(1)>0}]\cr Height in cm per chart entry.
#' @param height_fixed [\code{numeric(1)>0}]\cr Fixed height in cm.
#' @param what [\code{character(1)}] Either "percent" or "frequency". Supports partial matching.
#' @param digits Number of decimal places as integer.
#' @param percent_sign Logical, whether to include percentage symbol on chart.
#' @param sort_by String, sort by value or label?
#' @param desc Loical, sort in descending order?
#' @param vertical Logical. If FALSE (default), then horizontal.
#' @param seed Optional random seed for selection of colours in blender.
#'
#' @importFrom crosstable crosstable
#' @importFrom tidyselect everything
#' @importFrom stats ave
#' @return rdocx object, which can be saved with officer:::print.rdocx()
#' @export
#'
#' @examples
#' library(dplyr) # For piping
#' library(officer) # To save the rdocx object to disk
#' ex_survey1 %>%
#'   report_chart_likert(cols = a_1:a_9) %>%
#'   print(target = "test_docx_a19.docx")
#' file.remove("test_docx_a19.docx")
#'
#'
#'   docx_template <-
#'       system.file("template","NIFUmal_tom.docx",
#'                    package = "surveyreport", mustWork = TRUE)
#'   colour_palette <-
#'     readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx",
#'                                    package = "surveyreport", mustWork = TRUE),
#'                        sheet = "NIFUblue") %>%
#'     dplyr::pull(hex)
#' chart_format <-
#'  system.file("template", "NIFUmal_stiler.xlsx",
#'              package = "surveyreport", mustWork = TRUE) %>%
#'  readxl::read_excel(., sheet = 1) %>%
#'  dplyr::filter(surveyreport_style == "figure") %>%
#'  dplyr::pull(template_style)
#'
#'  test_docx_b13 <-
#'    ex_survey1 %>%
#'    report_chart_likert(cols = b_1:b_3,
#'                        docx_template = docx_template,
#'                        colour_palette = colour_palette,
#'                        chart_formatting = chart_format,
#'                        height_per_col = .3,
#'                        height_fixed = 1)
#' print(test_docx_b13, target = "test_docx_b13.docx")
#' file.remove("test_docx_b13.docx")

report_chart_likert <-
  function(data,
           cols = everything(),
           showNA = "ifany",
           docx_template = NULL,
           label_font_size = 8,
           main_font_size = 9,
           font_family = "Calibri",
           colour_palette = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           height_per_col = .3,
           height_fixed = 1,
           chart_formatting = NULL,
           what = "percent",
           digits = 1,
           percent_sign = TRUE,
           sort_by = NULL,
           vertical = FALSE,
           desc = FALSE,
           seed = 1) {

    if(!inherits(data, what = "data.frame")) {
      cli::cli_abort("data must be a {.cls data.frame} or {.cls tibble}.")
    }

    cols_enq <- rlang::enquo(cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data)

    check_category_pairs(data = data, cols_pos = cols_pos)



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




    if(grepl("^per", what)) {

      data <- prepare_perc_for_mschart(data[, cols_pos],
                                       showNA = showNA,
                                       percent_sign = percent_sign,
                                       digits = digits,
                                       sort_by = sort_by,
                                       desc = desc)
    } else if(grepl("^fre", what)) {
      data <- prepare_freq_for_mschart(data[, cols_pos],
                                       showNA = showNA,
                                       sort_by = sort_by,
                                       desc = desc)
    } else cli::cli_abort("{.arg what} must be either {.var percent} or {.var frequency}.")

    chart <-
      create_chart_likert(data = data,
                          label_font_size = label_font_size,
                          main_font_size = main_font_size,
                          colour_palette = colour_palette,
                          colour_na = colour_na,
                          colour_2nd_binary_cat = colour_2nd_binary_cat,
                          font_family = font_family,
                          vertical = vertical,
                          what = what,
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


#' Helper Function to Prepare Data for create_chart_likert
#'
#' @param data Dataset
#' @param showNA Whether to show NA in categorical variables (one of c("ifany", "always", "no"), like in table()).
#' @param digits Number of decimal places as integer.
#' @param percent_sign Logical, whether to include percentage symbol on chart.
#' @param sort_by String, sort by either "value", "label", ".id" (variable name) or NULL (default).
#' @param desc Reverse sorting of sort_by
#' @param call Error call function, usually not needed.
#'
#' @importFrom crosstable crosstable
#' @importFrom rlang arg_match caller_env is_integerish
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange desc
#'
#' @return Dataset
prepare_perc_for_mschart <-
  function(data,
           showNA = "ifany",
           call = rlang::caller_env(),
           digits = 1,
           percent_sign = TRUE,
           sort_by = NULL,
           desc = FALSE) {
    rlang::arg_match(showNA, values = c("ifany", "always", "no"), multiple = FALSE, error_call = call)
    if(!rlang::is_integerish(digits)) cli::cli_abort("{.arg digits} must be {.cls {integer(1)}}.")
    data <- cross_n(data, showNA)

    data$data_label <-
      ave(x = data$value,
          data$label,
          FUN = function(x) {
            fmt <- paste0("%.",digits, "f", if(percent_sign) "%%")
            sprintf(fmt = fmt, x/sum(x, na.rm = T)*100)
          })
    data <- sorter(data, sort_by=sort_by, desc=desc)
    data
  }


#' Helper Function to Prepare Frequency Data for create_chart_likert
#'
#' @param data Dataset
#' @param showNA Whether to show NA in categorical variables (one of c("ifany", "always", "no"), like in table()).
#' @param sort_by String, sort by either "value", "label", ".id" (variable name) or NULL (default).
#' @param desc Reverse sorting of sort_by
#' @param call Error call function, usually not needed.
#'
#' @importFrom crosstable crosstable
#' @importFrom rlang arg_match caller_env is_integerish
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange desc
#'
#' @return Dataset
prepare_freq_for_mschart <-
  function(data,
           showNA = "ifany",
           call = rlang::caller_env(),
           sort_by = NULL,
           desc = FALSE) {
    rlang::arg_match(showNA, values = c("ifany", "always", "no"), multiple = FALSE, error_call = call)
    data <- cross_n(data, showNA)

    data$data_label <- as.character(data$value)
    data <- sorter(data=data, sort_by=sort_by, desc=desc)

    data
  }

cross_n <- function(data, showNA="ifany") {
  data <- crosstable::crosstable(data = data,
                                 percent_pattern = "{n}",
                                 showNA = showNA,
                                 label = TRUE)

  data <- as.data.frame(data)
  data$value <- as.integer(data$value)
  data
}


sorter <- function(data, sort_by=NULL, desc=FALSE) {
  data$n_unique <- ave(x = data$variable, data$label,
                       FUN = function(x) length(unique(x)))
  fct_max <- max(data$n_unique, na.rm = TRUE)
  fct_uniques <- unique(data[data$n_unique == fct_max, "variable"])
  data$n_unique <- NULL
  data$variable <- factor(data$variable, levels = fct_uniques)
  data$cat_id <- data$variable %in% sort_by
  if(!is.null(sort_by)) {

    if(all(sort_by %in% names(data))) {
      sort_col <- sort_by
    } else if(all(sort_by %in% unique(data$variable))) {
      sort_col <- "sum_value"
    }
    data <- dplyr::group_by(data, .data$label, .data$cat_id)
    data <- dplyr::mutate(data,
                          sum_value = ifelse(.data$cat_id,
                                             sum(as.numeric(.data$value), na.rm=TRUE),
                                             NA))
    data <- dplyr::ungroup(data)
    data <-
      dplyr::arrange(data,
                     dplyr::desc(.data$cat_id),
                     if(desc) dplyr::desc(.data[[sort_col]]) else .data[[sort_col]])
  }
  data$label <- factor(data$label,
                       levels = unique(as.character(data$label)),
                       ordered = TRUE)
  data <- dplyr::arrange(data, as.integer(.data$label), .data$variable)
  as.data.frame(data)
}
