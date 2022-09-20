

#' Given Ordered Integer Vector, Return Requested Set.
#'
#' Useful for identifying which categories are to be collected.
#'
#' @param vec A vector of any type.
#' @param set A character string, one of c("top", "upper", "mid_upper", "lower",
#'   "mid_lower", "bottom")
#' @param maximize_n The number of values to extract when set is "maximize".
#' @importFrom stats median
#' @importFrom rlang arg_match
#' @return Selected set of vector.
#' @export
#' @examples
#' subset_vector(vec=1:7, set="mid_lower")

subset_vector <- function(vec, set=c("top", "upper", "mid_upper",
									 "lower", "mid_lower", "bottom", "maximize"),
						  maximize_n=NULL) {
	set <- rlang::arg_match(set)
	n <- length(vec)
	if(n %in% 0:1) {
		vec
	} else if(set=="top") {
		vec[n]
	} else if(set=="bottom") {
		vec[1]
	} else if(n %% 2 == 0L & set!="maximize") {
		if(set %in% c("mid_upper", "upper")) {
			vec[(n/2+1):n]
		} else if(set %in% c("mid_lower", "lower")) {
			vec[1:(n/2)]
		}
	} else {
		m <- median(seq_len(n))
		if(set=="upper") {
			vec[(m+1):n]
		} else if(set=="lower") {
			vec[1:(m-1)]
		} else if(set=="mid_upper") {
			vec[m:n]
		} else if(set=="mid_lower") {
			vec[1:m]
		} else if(set=="maximize") {
			if(n == maximize_n) {
				vec
			} else {
				max_set <- c()
				if(n %% 2 != 0L) {
					if(maximize_n == 1L) {
						m
					}
					if(maximize_n %% 2 != 0L) {
						max_set <- c(max_set, m)
					}
					if(maximize_n > 1L) {
						max_set <- c(max_set, 1L, n)
					}
					if(maximize_n > 4L) {
						max_set <- c(max_set, 2L, n-1L)
					}
					if(maximize_n > 5L | maximize_n == 4) {
						max_set <- c(max_set, 3L, n-2L)
					}
					if(maximize_n > 6L) {
						max_set <- c(max_set, 4L, n-3L)
					}
				} else if(n %% 2L == 0L) {
					if(maximize_n > 1L) {
						max_set <- c(max_set, 1L, n)
					}
					if(maximize_n > 3L & n <= 6) {
						max_set <- c(max_set, 2L, n-1L)
					}
					if(maximize_n > 4L | (maximize_n>3L & n > 6)) {
						max_set <- c(max_set, 3L, n-2L)
					}
					if(maximize_n %% 2 != 0L) {
						m <- round(median(seq_len(n)))
						max_set <- c(max_set, m)
					}
				}
				vec[sort(max_set)]
			}
		}
	}
}


#' Recode Missing By Type of Missingness
#'
#' Useful for item difficulty estimation according to Mislevy's recommendation.
#'     Also allowing for escaping rows with all missingess (typically
#'     not administered).
#'
#' @param df Data frame, or vector. Must be a dataframe, not a matrix, in this
#'   function. Only include item variables.
#' @param accept_vector Handles vectors if accept_vector=TRUE. Set to false to
#'   avoid accidents when using function per block and there is just one item in
#'   the block.
#' @param skipped What to replace skipped values with
#' @param not_administered What to replace not administered values with.
#' @param all_missing What to replace values in rows with all missing with.
#' @importFrom rlang set_names warn abort
#' @return A data.frame (or vector, if input is vector and accept_vector=TRUE)
#'   with recoded cells.
#' @export
#'
#' @examples
#' # Original data
#' input <- setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	NA,0,1,0,1, # First missing
#' 	NA,NA,1,0,1, # First two missing
#' 	1,0,NA,0,1, # One in middle missing
#' 	1,NA,NA,NA,1, # All in the middle missing
#' 	1,0,1,0,NA, # Last one missing
#' 	1,0,1,NA,NA, # Last two missing
#' 	1,0,NA,NA,NA, # Last three missing
#' 	NA,NA,NA,NA,NA # All missing
#' ), nrow = 9, byrow = TRUE)), nm=paste0("X", 1:5))
#' # What should be the output for item estimation according to Mislevy
#' # Skipped=> 0, not_administered=>NA, all_missing=>NA
#' y_i <-  stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	0,0,1,0,1, # First missing
#' 	0,0,1,0,1, # First two missing
#' 	1,0,0,0,1, # One in middle missing
#' 	1,0,0,0,1, # All in the middle missing
#' 	1,0,1,0,0, # Last one missing
#' 	1,0,1,0,NA, # Last two missing
#' 	1,0,0,NA,NA, # Last three missing
#' 	NA,NA,NA,NA,NA # All missing
#' ), nrow = 9, byrow = TRUE)), nm=paste0("X", 1:5))
#'
#' # What should be the output for person estimation according to Mislevy
#' # Skipped=> 0, not_administered=>NA, all_missing=>NA
#' y_p <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	0,0,1,0,1, # First missing
#' 	0,0,1,0,1, # First two missing
#' 	1,0,0,0,1, # One in middle missing
#' 	1,0,0,0,1, # All in the middle missing
#' 	1,0,1,0,0, # Last one missing
#' 	1,0,1,0,0, # Last two missing
#' 	1,0,0,0,0, # Last three missing
#' 	0,0,0,0,0 # All missing
#' ), nrow = 9, byrow = TRUE)), nm=paste0("X", 1:5))
#' # Recoding for counting skipped, not_administered, all_missing, etc
#' # Skipped=> 99, not_administered=>999, all_missing=>9999
#' y_info <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	99,0,1,0,1, # First missing
#' 	99,99,1,0,1, # First two missing
#' 	1,0,99,0,1, # One in middle missing
#' 	1,99,99,99,1, # All in the middle missing
#' 	1,0,1,0,99, # Last one missing
#' 	1,0,1,99,999, # Last two missing
#' 	1,0,99,999,999, # Last three missing
#' 	9999,9999,9999,9999,9999 # All missing
#' ), nrow = 9, byrow = TRUE)), nm=paste0("X", 1:5))
#'
#' y_i2 <- omitted_recoder_df(input) #Mislevy item estimation
#' y_p2 <- omitted_recoder_df(input, skipped = 0L, #Mislevy person estimation
#'                            not_administered = 0L, all_missing = 0L)
#' y_info2 <- omitted_recoder_df(input, skipped = 99,
#'                               not_administered = 999, all_missing = 9999)
#' identical(y_i, y_i2)
#' identical(y_p, y_p2)
#' identical(y_info, y_info2)
#' \dontrun{
#' omitted_recoder_df(input[,4]) # Should fail
#' }
#' identical(omitted_recoder_df(input[,4], accept_vector=TRUE),
#'          c(0,0,0,0,0,0,0,NA,NA))
#' identical(omitted_recoder_df(input[,4, drop=FALSE]),
#'           input[,4, drop=FALSE]) # Output should equal input
#'
omitted_recoder_df <- function(df, accept_vector=FALSE, skipped=0L,
							   not_administered=NA_integer_,
							   all_missing=NA_integer_) {
	omittedRecoderVec <- function(vec) {
		vec_new <- vec
		N <- length(vec)
		if(all(is.na(vec))) {
			vec_new <- rep(all_missing, times=N)
		} else {
			for(i in N:1L) { # Going backwards on all a person's responses,

				if(is.na(vec[i])) { # if the response is blank AND either
					if((any(!is.na(vec[min(c(i+1L, N)):N]))) ||  #  1) any responses after this to the end are present OR  #i==1L && it is the first response AND
					   (i!=1L && !is.na(vec[i-1L]) && all(is.na(vec[min(c(i+1L, N)):N]))) ## or the prior response is PRESENT (if not first response)
					) {
						vec_new[i] <- skipped   # Then set this response as 'skipped'
					} else { # OR if the response is blank AND
						if((i == 1L || is.na(vec[i-1L])) && # 1) it is the first response or the prior response is MISSING AND
						   all(is.na(vec[i:N]))) { # 2) All responses from this and to the end are all blank
							vec_new[i] <- not_administered # Recode as not administered.
						}
					}
				}
			}
		}
		vec_new
	}
	if(is.atomic(df)) {
		if(!accept_vector) {
			rlang::abort("Vectors not accepted.")
		} else if(is.atomic(df)) omittedRecoderVec(df)
	} else {
		if(ncol(df)==1) {
			rlang::warn("Unable to recode single-column data.frame without knowing context.")
			df
		} else {
			rlang::set_names(as.data.frame(t(apply(df, 1, omittedRecoderVec))),
							 nm=colnames(df))
		}
	}
}







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
#' @param seed Random seed for sampling.
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom stats median
#' @return A colour set as character vector.
#' @export
#' @examples
#' get_colour_set(n_colours_needed=4)
# get_colour_set <- function(n_colours_needed, user_colour_set=NULL) {
#
# 	if(!is.null(user_colour_set) &&
# 	   length(user_colour_set) >= n_colours_needed &&
# 	   all(is_colour(user_colour_set))) {
# 		if(length(user_colour_set)==7L) {
#
# 			x <- 1:length(user_colour_set)
# 			if(n_colours_needed==7L) return(user_colour_set)
# 			if(n_colours_needed==6L) return(user_colour_set[c(1:3, 5:length(x))])
# 			if(n_colours_needed==5L) return(user_colour_set[c(1:2, stats::median(x), 6:length(x))])
# 			if(n_colours_needed==4L) return(user_colour_set[c(1,3, 5,length(x))])
# 			if(n_colours_needed==3L) return(user_colour_set[c(1, stats::median(x),length(x))])
# 			if(n_colours_needed==2L) return(user_colour_set[c(1, length(x))])
# 		} else return(user_colour_set[1:n_colours_needed])
# 	} else {
# 		set.seed(1)
# 		colour_map <- RColorBrewer::brewer.pal.info
# 		qual_col_pals <- colour_map[colour_map$category == 'qual' & colour_map$colorblind,]
# 		out <- mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))
# 		sample(unlist(out), n_colours_needed)
# 	}
# }
get_colour_set <-
  function(n_colours_needed, user_colour_set=NULL, seed=1) {

	if(!is.null(user_colour_set) &&
	   length(user_colour_set) >= n_colours_needed &&
	   all(is_colour(user_colour_set))) {
			x <- seq_along(user_colour_set)
			subset_vector(vec = user_colour_set, set = "maximize", maximize_n = n_colours_needed)
	} else if(n_colours_needed <= 12) {
		set.seed(seed)
		sample(x = RColorBrewer::brewer.pal(n = 12, name = "Paired"), size = n_colours_needed)
	} else viridisLite::viridis(n = n_colours_needed)
}


#' Get Colour Palette
#'
#' Give two
#'
#' @param type Character vector of variable types ("ordinal", "nominal",
#'   "interval").
#' @param unique_set_group Character vector of unique values across the battery.
#' @param unique_set Character vector of unique values within the variable.
#' @param colour_set_ordinal,colour_set_nominal Character vector with hex
#'   colours. Must be provided.
#'
#' @return Named character vector of hex colours for each element of unique_set.
#' @importFrom rlang abort
#' @importFrom vctrs vec_assert
#' @importFrom purrr map2
#'
colour_picker <-
  function(type,
           unique_set_group,
           unique_set,
           colour_set_ordinal,
           colour_set_nominal) {
	vctrs::vec_assert(x = type, ptype = character())
	vctrs::vec_assert(x = unique_set_group, ptype = list())
	vctrs::vec_assert(x = unique_set, ptype = list())

	if(length(type) != length(unique_set_group)) {
		rlang::abort(c("type and unique_set_group are not of equal length.",
					   x=paste0("type is of length ", length(type),
					   		 " whereas unique_set_group is of length ", length(unique_set_group))))
	}
	if(length(unique_set) != length(unique_set_group)) {
		rlang::abort(c("unique_set and unique_set_group are not of equal length.",
					   x=paste0("unique_set is of length ", length(unique_set),
					   		 " whereas unique_set_group is of length ", length(unique_set_group))))
	}
	lengths_comparisons <- lengths(unique_set) <= lengths(unique_set_group)
	if(!all(lengths_comparisons)) {
		rlang::abort(c("unique_set and unique_set_group contain vectors of pairwise unequal lengths.",
					   x=paste0("Problem(s) at row ", which(!lengths_comparisons),
					   		 " when unique_set is ", lengths(unique_set)[!lengths_comparisons],
					   		 " and unique_set_group is ", lengths(unique_set_group)[!lengths_comparisons])))
	}
	# out <-
		purrr::map(.x = seq_along(type),# .y = unique_set,
				function(i) {
					vctrs::vec_assert(x = unique_set_group[[i]], ptype = character())
					vctrs::vec_assert(x = unique_set[[i]], ptype = character())

					n_unique_set_i <- length(unique_set[[i]])
					n_unique_set_group_i <- length(unique_set_group[[i]])

					if(!is.na(type[i]) && type[i] %in% c("ordinal", "interval")) {
						rlang::set_names(
							x = unname(get_colour_set(n_colours_needed = n_unique_set_group_i,
												  user_colour_set = colour_set_ordinal)),
							nm = unique_set_group[[i]])[unique_set[[i]]]

					} else if(!is.na(type[i]) && type[i] == "nominal" &&
							  n_unique_set_group_i <= 12) { # Why this limit here only?
						rlang::set_names(
							x = unname(get_colour_set(n_colours_needed = n_unique_set_group_i,
											  user_colour_set = colour_set_nominal)),
							nm = unique_set_group[[i]])[unique_set[[i]]]
					} else NA_character_
				})
	# out
}



#' Remove Special Characters (<,>) in Variable Labels
#'
#' @param df Data frame
#'
#' @return A data frame
#' @export

remove_special_chars_in_labels <-
	function(df) {
	  z <-
			labelled::val_labels(df)
	  z <-
			purrr::map2(.x = z,
			            .y = names(z),
			            .f = function(x, y) {
				if(!is.null(x) && any(grepl("<|>", names(x)))) {
					cli::cli_warn(c(
					  "Current version of function doesn't handle special characters `<` or `>` in labels.",
					  i="Will remove these in {{y}}"))
					names(x)<- gsub("<|>", "", names(x))
				}
				x
			})
	  labelled::val_labels(df) <- z
		df
	}


#' Create All Possible Combinations of Vector Elements with Minimum A and
#' Maximum B.
#'
#' @param vec Vector
#' @param n_min Minimum number of elements
#' @param n_max Maximum number of elements. Defaults to length of vec.
#'
#' @importFrom utils combn
#' @return A data frame
#' @export
#' @examples combn_upto()
combn_upto <-
  function(vec=c("a", "b", "c", "d", "e", "f", "g"),
           n_min=6L,
           n_max=length(vec)) {
	stopifnot(purrr::is_integer(as.integer(n_min)))
	stopifnot(n_max<=length(vec))
	x <-
	  unlist(lapply(n_min:n_max, function(x) utils::combn(x = vec, m = x, simplify = F)), recursive = FALSE)
	x <- rlang::set_names(x, x)
	rev(x)
}


#' Center String Vector
#'
#' @param string String vector
#' @param maxwidth Maximum width
#' @return String vector
#' @export
#' @examples center_string(string=c("This is a very long label for a graph.",
#' "But this one is even longer due to superfluous and verbose way of writing"),
#'  maxwidth=20)
center_string <- function(string, maxwidth=50) {
		sapply(string, USE.NAMES = F, function(x) {
			maxw <- median(nchar(string))
			maxw <- maxwidth
			if(nchar(x)<maxw) {
				x
			} else {
				if(nchar(x)>=maxw & nchar(x)<2*maxw) {
					stringr::str_wrap(x, nchar(x)/2)
				} else {
					if(nchar(x)>=2*maxw) {
						stringr::str_wrap(x, nchar(x)/3)
					}
				}
			}
		})
	}


###  Check that all pairs of cols share at least one observed response category
check_category_pairs <-
  function(data, cols_pos, call = rlang::caller_env()) {
    purrr::walk2(.x = unname(cols_pos),
                 .y = names(cols_pos),
                 .f = function(x, y) {
                   cols_rest <-
                     cols_pos[-c(1:match(y, names(cols_pos)))]
                   purrr::walk2(.x = unname(cols_rest),
                                .y = names(cols_rest),
                                .f = function(x2, y2) {
                                  common <-
                                    dplyr::intersect(
                                      levels(data[[y]]),
                                      levels(data[[y2]]))
                                  if(length(common) == 0L) {
                                    cli::cli_abort(
                                      c("Unequal variables.",
                                        "!" = "All variables must share at least one common category.",
                                        "i" = "Column {.var {y}} and column {.var {y2}} lack common categories."
                                      ),
                                      call = call)
                                  }
                                })
                 })
  }

use_docx <- function(docx_template = NULL) {
  if(!is.null(docx_template)) {
    if(is.character(docx_template) &&
       length(docx_template) == 1L &&
       file.exists(docx_template)) {
      officer::read_docx(path = docx_template)
    } else docx_template
  } else officer::read_docx()
}
