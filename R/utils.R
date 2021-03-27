
subset_vector <- function(vec, set="upper") { # set=c("top", "upper", "mid_upper", "lower", "mid_lower", "bottom")
	# Alternativelt: lowest,lower,mid_lower,mid_higher,higher,highest
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

### Fix:
# Colnames kept
# Weird notAdministered not recoded
omitted_recoder_df <- function(df, acceptVector=FALSE, skipped=0L, notAdministered=NA_integer_, allMissing=NA_integer_) {
	# Purpose: Recoding missingness depending on the type of missingness: skipped or not administered.
	# Also allowing for escaping rows with all missingess (typically not administered).
	# Useful for item difficulty estimation according to Mislevy's recommendation.
	# Arguments:
	# df = must be a dataframe, not a matrix, in this function. Only include item variables.
	# Handles vectors if acceptVector=TRUE. Set to false to avoid accidents when using function per block and there is just one item in the block.
	# skipped, notAdminsitered and allMissing: Values to replace with.

	omittedRecoderVec <- function(vec) {
		vec_new <- vec
		N <- length(vec)
		if(all(is.na(vec))) {
			vec_new <- rep(allMissing, times=N)
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
							vec_new[i] <- notAdministered # Recode as not administered.
						}
					}
				}
			}
		}
		vec_new
	}
	if(is.atomic(df)) {
		if(!acceptVector) {
			stop("Vectors not accepted")
		} else if(is.atomic(df)) omittedRecoderVec(df)
	} else {
		if(ncol(df)==1) {
			warning("Unable to recode single-column data.frame without knowing context.")
			df
		} else {
			setNames(as.data.frame(t(apply(df, 1, omittedRecoderVec))), nm=colnames(df))
		}
	}
}





check_options <- function(df, df_var, global_default, options) {
	if(is.null(df[[df_var]])) {
		df[[df_var]] <- global_default
		rlang::inform(paste0('Variable `', df_var, '` does not exist in design frame. Adding and using global default: `', global_default, "`"))
	} else {
		if(!is.numeric(options) && !is.numeric(df[[df_var]])) {
			invalid <- unique(df[[df_var]][!df[[df_var]] %in% options])
			if(length(invalid) > 0L) {
				rlang::abort(message = paste0('Variable `', df_var,
											  '` in design frame contains invalid options c(',
											  paste0(invalid, collapse=","),
											  '). Please use one of: `c(',
											  paste0(options, collapse=", "), ')'))
			}
		} else if(class(options) != class(df[[df_var]])) {
			rlang::abort(message = paste0('Variable `', df_var, '` in design frame is of class ',
										  class(df[[df_var]]),
										  'whereas options expect ', class(options)))
		} else if(any(is.na(df[[df_var]]))) {
			df[is.na(df[[df_var]]), df_var] <- global_default
			rlang::inform(paste0('Variable `', df_var, '` in design frame contains NA. Using global default: `', global_default, "`"))
		}
	}
	df
}
