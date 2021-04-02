
#' Assert Infoframe As Valid Object
#'
#' Checks that an object is a valid infoframe object. Currently does not return
#'     anything.
#'
#' @param obj An infoframe object as created by create_infoframe()
#' @export
#' @return Nothing.
#' @examples
assert_valid_infoframe <- function(obj) {
	if(!rlang::is_list(obj, n = 3L) ||
	   !all(c("df", "var_frame", "design_frame") %in% names(obj)) ||
	   !is.data.frame(obj[["df"]]) || 
	   !is.data.frame(obj[["var_frame"]]) || 
	   !is.data.frame(obj[["design_frame"]])) {

		X <- paste0("`obj` is a ", class(obj)[[1]], " of length ", length(obj),
					 " with elements of class \n", 
					 rlang::quo_text(unname(purrr::map_chr(obj, ~class(.x)[[1]]))))
		rlang::abort(c("Invalid `obj`:",
					   i = "`obj` must be a list with three elements.",
					   i = "The elements must be named `df`, `var_frame` and `design_frame`.",
					   i = "Each element must consist of a data frame.",
					   x = X))
	}
	X <- unique(unlist(obj$var_frame$var))[!unique(unlist(obj$var_frame$var)) %in% names(obj$df)]
	if(length(X) > 0L) {
		rlang::abort(c("Inconsistent variable names:", 
					   i = "Each entry in `obj$var_frame$var` should exist in `obj$df`.",
					   x = "Following variables were not found in `df`: ", 
					   rlang::quo_text(X)))
	}
	if(nrow(obj$design_frame)==0L) {
		rlang::abort(c("Empty design_frame:",
					 i = "design_frame must contain at least one row.",
					 x = "design_frame contains 0 zeros."))
	}
}



#' Given Ordered Integer Vector, Return Requested Set.
#' 
#' Useful for identifying which categories are to be collected.
#' 
#' @param vec A vector of any type.
#' @param set A character string, one of c("top", "upper", "mid_upper", "lower",
#'   "mid_lower", "bottom")
#' @importFrom stats median
#' @importFrom rlang arg_match
#' @return Selected set of vector.
#' @export
#' @examples
#' subset_vector(vec=1:7, set="mid_lower")

subset_vector <- function(vec, set=c("top", "upper", "mid_upper", 
									 "lower", "mid_lower", "bottom")) {
	set <- rlang::arg_match(set)
	n <- length(vec)
	if(n %in% 0:1) {
		vec
	} else if(set=="top") {
		vec[n]
	} else if(set=="bottom") {
		vec[1]
	} else if(n %% 2 == 0L) {
		if(set %in% c("mid_upper", "upper")) {
			vec[(n/2+1):n]
		} else if(set %in% c("mid_lower", "lower")) {
			vec[1:(n/2)]
		}
	} else {
		m <- median(vec)
		if(set=="upper") {
			vec[(m+1):n]
		} else if(set=="lower") {
			vec[1:(m-1)]
		} else if(set=="mid_upper") {
			vec[m:n]
		} else if(set=="mid_lower") {
			vec[1:m]
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





#' Mostly an internal function to check whether options provided are valid
#'
#' Not much else to say.
#'
#' @param df Typically a design frame or a var_frame.
#' @param df_var Character string for variable name.
#' @param global_default If missing, what to set as default value.
#' @param options A type (character()) or a character vector with valid options.
#'
#' @importFrom rlang inform abort
#' @return Data frame, optionally with the missing variable added, set with the
#'   global_default if it is among the options.
#'
#' @examples
check_options <- function(df, df_var, global_default, options) {
	if(is.null(df[[df_var]])) {
		df[[df_var]] <- global_default
		rlang::inform(paste0('Variable `', df_var, '` does not exist in design frame. Adding and using global default: `', global_default, "`"))
	} else {
		if(!is.numeric(options) && !is.numeric(df[[df_var]])) {
			invalid <- unique(df[[df_var]][!df[[df_var]] %in% options])
			if(length(invalid) > 0L) {
				rlang::abort(c("Incorrect options:",
							 x=paste0('Variable `', df_var, '` in design frame contains invalid options'),
								rlang::quo_text(invalid), 
							i='Please use one of:', 
							rlang::quo_text(options)))
			}
		} else if(class(options) != class(df[[df_var]])) {
			rlang::abort(c(x=paste0('Variable `', df_var, '` in design frame is of class ', class(df[[df_var]])),
						   i=paste0('Options expect ', class(options))))
		} else if(any(is.na(df[[df_var]]))) {
			df[is.na(df[[df_var]]), df_var] <- global_default
			rlang::inform(paste0('Variable `', df_var, '` in design frame contains NA. Using global default: `', global_default, "`"))
		}
	}
	df
}
