

#' @param df The data.frame or tibble to find variables from.
#' @param y_var A list where each element is a character string or vector of dependent variables for a specific model. If a character vector is provided, each element will be used for a single model. All character vector elements can be named for convenience in output. Otherwise vector elements will be used as names. Use helper function `create_item_groups()` for easily creating such lists. List names will be shortened to 6 characters to comply with Mplus limitations.
#' @param x_var Principally the same as for y_var. However, list names will be ignored. Models will be run for all y_var X x_var combinations. If x_var is NULL or "", a CFA model will be run.
#' @param cluster Character string identifying the variable used as cluster-identifier.
#' @return A data frame with model specifications in simplified form (y_var=Y, independent=>X), estimate of the regression coefficient if any, and model fit indices if any.
#' @author Stephan Daus
run_analyses <- function(df, var_frame, design_frame,
						 cluster = NA,
						 engine="mplus", estimator="wlsmv", loading_cutoff=.4, ignore_x_type=FALSE, ignore_dummy01=FALSE, ignore_low_n=FALSE,
						 remove_text_var = TRUE,
						 remove_empty_y_var=TRUE, drop_duplicates=TRUE, path=getwd()) {
	library(MplusAutomation)
	library(purrr)
	library(dplyr)
	prepare_mplus_model <- function(df, y_var, x_var, y_group, x_group, y_type, x_type, cluster, estimator) {

		x_syntax <- paste0(" ON ", paste0(x_var, collapse="\n"), ";")

		y_group_short <- gsub("^([[:alnum:]_]{,6}).*", "\\1", y_group)
		y_syntax <- paste0(paste0(y_var, collapse="\n"))
		if(length(y_var)>1L) {
			model_syntax <- paste0(paste0(y_group_short, " BY ", y_syntax, ";\n"),
								   if(all(!is.null(x_var) && !is.na(x_var))) paste0(y_group_short, " ", x_syntax))
		} else {
			model_syntax <- if(all(!is.null(x_var) && !is.na(x_var))) paste0(y_syntax,  x_syntax)
		}

		y_variable_syntax <- paste0(if(y_type == "ordinal") "CATEGORICAL = " else "NOMINAL = ",
									paste0(y_var, collapse="\n"), ";\n")
		cluster_syntax <- if(!is.na(cluster)) paste0("CLUSTER = ", cluster,";\n")
		title <- paste0(paste0(y_var, collapse=", "), "\n",
						if(all(!is.null(x_var) && !is.na(x_var))) " ON ",
						if(all(!is.null(x_var) && !is.na(x_var))) paste(x_var, collapse=" "), ";")
		variable <- paste0(y_variable_syntax, cluster_syntax)
		analysis <- paste0(if(estimator == "wlsmv") "ESTIMATOR=WLSMV;\n" else "ESTIMATOR=MLR;\n",
						   if(!is.na(cluster)) "TYPE = COMPLEX;")
		file_path <- file.path(path, paste0(y_group_short, if(all(!is.null(x_var) && !is.na(x_var))) x_group, ".inp"))

		df %>%
			dplyr::select(dplyr::all_of(c(y_var, if(all(!is.null(x_var) && !is.na(x_var))) x_var, if(!is.na(cluster)) cluster))) %>%
			MplusAutomation::mplusObject(TITLE = title, VARIABLE = variable, ANALYSIS = analysis, MODEL = model_syntax,
										 OUTPUT = "STD;", usevariables = colnames(.), rdata = .) %>%
			MplusAutomation::mplusModeler(object = ., modelout = file_path, run = 1L, hashfilename = F, varwarnings = T, writeData = "always")
	}
	extract_mplus_error <- function(mod) {
		paste0(unlist(mod[["results"]][["errors"]]), collapse=". ")
	}
	extract_mplus_model <- function(mod, y_var, y_group, x_var, x_group) {
		mod_out <- data.frame(error=extract_mplus_error(mod))
		mod_available <- !is.null(mod[["results"]][["parameters"]][["unstandardized"]][["pval"]])
		if(mod_available) {

			if(all(!is.null(x_var) && !is.na(x_var))) {

				header_on <- if(length(y_var)>1L) paste0(toupper(y_group), "\\.ON") else paste0(toupper(y_var), "\\.ON")
				mod_par <-
					mod %>%
					.[["results"]] %>%
					.[["parameters"]] %>%
					.[["unstandardized"]] %>%
					dplyr::filter(grepl(header_on, paramHeader)) %>%
					dplyr::select(-paramHeader, -se, -est_se) %>%
					dplyr::rename(term=param, estimate=est, p.value=pval) %>%
					tidyr::chop(cols = c(term, estimate, p.value))
				if(nrow(mod_par)==1L) mod_out <- cbind(mod_out, mod_par)
			}

			if(length(y_var)>1L) {
				mod_load <-
					mod %>%
					.[["results"]] %>%
					.[["parameters"]] %>%
					.[["unstandardized"]] %>%
					dplyr::filter(grepl(paste0(toupper(y_group), "\\.BY") , paramHeader), !(est==1.000 & se==999.000)) %>%
					dplyr::select(-paramHeader, -se, -est_se) %>%
					dplyr::mutate(n_load_good = est>=loading_cutoff & pval<.05, n_load_bad = est<loading_cutoff | pval>.05) %>%
					dplyr::summarize(n_load_good=sum(n_load_good), n_load_bad=sum(n_load_bad))
				if(nrow(mod_load)==1L) mod_out <- cbind(mod_out, mod_load)

			}

			if(!is.null(mod[["results"]][["summaries"]])) {
				mod_sum <-
					mod %>%
					.[["results"]] %>%
					.[["summaries"]] %>%
					dplyr::select(-Mplus.version, -Title, -AnalysisType, -DataType,
						   -Parameters, -Estimator, -NGroups,
						   -NContinuousLatentVars, -dplyr::matches("Baseline"))
				if(nrow(mod_sum)==1L) mod_out <- cbind(mod_out, mod_sum)
				if(all(!is.null(x_var) && !is.na(x_var))) {
					mod_r2 <-
						mod %>%
						.[["results"]] %>%
						.[["parameters"]] %>%
						.[["r2"]] %>%
						dplyr::filter(param== if(length(y_var)>1L) toupper(y_group) else toupper(y_var)) %>%
						dplyr::mutate(r2=as.character(est), r2_p=as.character(pval)) %>%
						dplyr::select(r2, r2_p)
					if(nrow(mod_r2)==1L) mod_out <- cbind(mod_out, mod_r2)
				}
			}
		}
		mod_out
	}
	run_mplus <- function(df, y_var, y_group, x_var, x_group, y_type, x_type, cluster, estimator) {
		mod <- prepare_mplus_model(df=df, y_var=y_var, y_group=y_group, x_var=x_var, x_group=x_group, y_type=y_type, x_type=x_type, cluster=cluster, estimator=estimator)
		extract_mplus_model(mod, y_var=y_var, x_var=x_var, y_group=y_group, x_group=x_group)
	}
	run_lavaan <- function(df, y_var, y_group, x_var, x_group, y_type, x_type, cluster, estimator, ...) {

	}
	run_lm <- function(df, y_var, x_var, ...) {
		mod <- lm(formula = as.formula(paste0(y_var, "~", paste0(x_var, collapse=" + "))), data = df)
	}

	run_single_analysis <- function(
		y_var, y_group, y_type,
		x_var, x_group, x_type, x_ref_cat,
		cluster, engine, ...) {


		arg_call <- match.call(expand.dots = T)
		pb$tick()


		# Move all these checks to global scope
		y_var_unavail <- y_var[!y_var %in% colnames(df)]
		if(length(y_var_unavail)>0L) {
			View(arg_call)
			rlang::abort(message = paste0("Following y_var not found in df", paste0(y_var_unavail, collapse=",")))
		}

		problem_singular <- dplyr::select(df, tidyselect::all_of(y_var)) %>% dplyr::select(tidyselect:::where(~all(is.na(.)))) %>% colnames()
		if(length(problem_singular)>0L) {
			rlang::warn(paste0("There is only NA in ", paste0(problem_singular, collapse=","), ". Will skip it."))
			return()
		}

		if(length(cluster)>1L) {
			View(arg_call)
			rlang::abort(message = paste0("Max 1 `cluster` is allowed. Problem with ", paste0(cluster, collapse=",")))
		}
		if(!is.na(cluster) && !cluster %in% colnames(df)) {
			View(arg_call)
			rlang::abort(message = paste0("Following `cluster` not found in df: ", cluster))
		}

		# Slim down df temporarily
		df <- dplyr::select(df, tidyselect::all_of(c(y_var, x_var)))

		### Generate dummy variables
		if(x_type %in% c("ordinal", "nominal") & dplyr::n_distinct(df[[x_var]], na.rm = T)>2L) {
			if(is.null(x_ref_cat) || is.na(x_ref_cat)) x_ref_cat <-
					dplyr::count(df, .data[[x_var]]) %>%
					dplyr::arrange(desc(n)) %>%
					dplyr::slice(1) %>%
					dplyr::pull(.data[[x_var]])
			df <-
				df %>%
				dplyr::mutate(dummy_variable=1L, ROW_IDENTIFIER=seq_len(nrow(.)),
							  "{x_var}" := gsub("[æøåÆØÅ\\.]", "", .data[[x_var]])) %>%
				tidyr::pivot_wider(names_from = all_of(x_var), values_from = dummy_variable,
								   values_fill = 0L, names_glue = paste0( "{.name}__", x_var)) %>%
				dplyr::select(-tidyselect::all_of(paste0(x_ref_cat, "__", x_var)), -ROW_IDENTIFIER)

			x_var <- grep(pattern = paste0("__", x_var), x = names(df), value = T)

			print(x_var)
		}

		if(engine=="mplus") {
			run_mplus(df=df, y_var = y_var, y_group = y_group, y_type=y_type, x_var=x_var, x_group=x_group, cluster=cluster, estimator=estimator)
		} else if(engine=="lavaan") {
			run_lavaan(df=df, y_var = y_var, y_group = y_group,y_type=y_type, x_var=x_var, x_group=x_group, cluster=cluster, estimator=estimator)
		} else if(engine=="lm") {
			if(!is.na(cluster)) rlang::warn("Cluster ignored for lm()")
			if(!length(y_var)>1L) rlang::abort("lm() requires a single y_var string.")
			run_lm(df=df, y_var = y_var, x_var=x_var, y_type=y_type)
		}
	}
	# Early fail.
	{
	engine_options <- c("mplus", "lavaan", "lm")
	estimator_options <- c("wlsmv", "mlr")
	type_options <- c("nominal", "ordinal", "interval")
	cluster_options <- c(colnames(df), NA_character_)
	added_parts <- c()

	df <- labelled::remove_labels(x = df, user_na_to_na = T, keep_var_label = F)
	if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	if(!is.data.frame(design_frame)) rlang::abort("design_frame is not a data.frame")
	if(nrow(design_frame)==0L) rlang::abort("Why do you give me an empty design_frame?")
	if(is.null(design_frame[["y_var"]])) rlang::abort("design_frame must have at least a 'y_var' variable")

	if(remove_text_var) {
		design_frame <- design_frame %>% dplyr::filter(y_type != "text", x_type != "text")
	} else if(design_frame %>% dplyr::filter(y_type == "text", x_type == "text") %>% nrow() >0L) {
		rlang::abort("Found character-variables. Please drop these or set remove_text_var=TRUE.")
	}

	empty_y_var <- design_frame[["y_var"]] %>% purrr::map_lgl(FUN = function(x) length(x)==0)
	if(!remove_empty_y_var && any(empty_y_var)) {
		rlang::abort("Seems there are empty 'y_var' entries. Please remove these first using:
					 `my_design_frame %>% filter(pull(y_var) %>% lapply(length) %>% unlist()>0)`")
	}
	if(drop_duplicates) {
		design_frame <- unique(design_frame)
	}

	if(is.null(design_frame[["y_group"]])) {
		design_frame[["y_group"]] <- design_frame[["y_var"]] %>% purrr::map(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "y_group")
	}
	if(is.null(design_frame[["x_var"]])) {
		design_frame[["x_var"]] <- NA_character_
		added_parts <- c(added_parts, "x_var")
	}
	if(is.null(design_frame[["x_group"]])) {
		design_frame[["x_group"]] <- design_frame[["x_var"]] %>% purrr::map_chr(.x = ., .f = ~paste0(.,collapse=","))
		added_parts <- c(added_parts, "x_group")
	}
	if(is.null(design_frame[["x_ref_cat"]])) {
		design_frame[["x_ref_cat"]] <- NA_character_
		added_parts <- c(added_parts, "x_ref_cat")
	}
	design_frame %>%
		dplyr::distinct(x_var, x_type) %>%
		dplyr::filter(!is.na(x_var)) %>%
		purrr::pwalk(function(x_var, x_type) {
			if(!x_var %in% colnames(df)) {
				rlang::abort(message = paste0("Following x_var not found in df: c(", paste0(x_var, collapse=","), ")"))
			}
			if(x_type %in% c("ordinal", "nominal")) {
				if(!ignore_x_type &&
				   dplyr::n_distinct(df[[x_var]], na.rm = T)>2L) {
					rlang::abort(paste0("For x_var=='", x_var, "', x_type is '", x_type, "' and there are over 2 categories.\n",
										"Use function prep_data to automatically generate dummy variables.\n",
										"Set `ignore_x_type=TRUE` to omit this check."))
				} else if(!ignore_dummy01 &&
						  dplyr::n_distinct(df[[x_var]], na.rm = T)==2L &&
						  min(df[[x_var]], na.rm = T) > 0L) {
					rlang::abort(paste0("For x_var=='", x_var, "', x_type is '", x_type, "' and there are 2 categories where lowest value is above 0.\n",
										"Use function prep_data to automatically adjust dummy variables.\n",
										"Set `ignore_dummy01 = TRUE` to omit this check."))
				} else if(!ignore_low_n &&
						  dplyr::count(df, .data[[x_var]]) %>%
						  dplyr::filter(!is.na(.data[[x_var]]), n<10) %>%
						  nrow() > 0L) {
					rlang::warn(paste0("For x_var=='", x_var, "', x_type is '", x_type, "' and at least one category has fewer than 10 observations.\n",
										"Use function prep_data to automatically combine categories with small n before creating dummy variables.\n",
										"Set `ignore_low_n = TRUE` to omit this check."))
				}
			}
		})


	design_frame <- check_options(df = design_frame, df_var = "y_type", global_default = "interval", options = type_options)
	if(!ignore_x_type) design_frame <- check_options(df = design_frame, df_var = "x_type", global_default = "interval", options = type_options)
	design_frame <- check_options(df = design_frame, df_var = "cluster", global_default = cluster, options = cluster_options)
	design_frame <- check_options(df = design_frame, df_var = "engine", global_default = engine, options = engine_options)
	design_frame <- check_options(df = design_frame, df_var = "estimator", global_default = estimator, options = estimator_options)
	# Return warnings
	if(length(added_parts)>0L) rlang::warn(paste0("Added parts that were missing in design_frame: ", paste0(added_parts, collapse=",")))

	}
	pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(design_frame))
	pb$tick(0)

	design_frame <-
		cbind(design_frame, purrr::pmap_dfr(.l = design_frame, .f = run_single_analysis)) %>%
		tibble::as_tibble()
	list(df=df, var_frame=var_frame, design_frame=design_frame)
}

# prep_data <- function(df, var_frame, design_frame, drop_unlisted=FALSE, drop_duplicates=TRUE) {
	# if(!is.data.frame(df)) rlang::abort("df is not a data.frame")
	# if(!is.data.frame(var_frame)) rlang::abort("var_frame is not a data.frame")
	# if(nrow(var_frame)==0L) rlang::abort("Why do you give me an empty var_frame?")
	# if(is.null(var_frame[["var"]]) | is.null(var_frame[["role"]]) | is.null(var_frame[["type"]])) {
	# 	rlang::abort("var_frame must have at least the variables: var, role, type")
	# }
	#
	# empty_var <- var_frame[["var"]] %>% purrr::map_int(.f = ~length)
	# if(any(!empty_var)) {
	# 	rlang::abort("Seems there are empty 'var' entries. Please remove these first using:
	# 				 `my_design_frame %>% filter(pull(var) %>% lapply(length) %>% unlist()>0)`")
	# }
	# if(drop_duplicates) {
	# 	var_frame <- unique(var_frame)
	# }
	# dummy_code <- function(x) {
	# 	sapply(levels(x), function(y) as.integer(x == y)) %>% tibble::as_tibble()
	# }
	###
	# for(i in 1:nrow(var_frame)) {
	# 	if(var_frame[i, "role"] == "x" &&
	# 	   var_frame[i, "type"] %in% c("ordinal", "nominal") &&
	# 	   n_distinct(df[[var_frame[i, "var"]]], na.rm = T) > 2L) {
	# 		var_frame[["modelled_by"]] <-
	# 		df[[var_frame[i, "var"]]]

		# }
	# }

	# if(drop_unlisted) {

	# }


	# Check upfront if there is insufficient variance
	# for(i in 1:length(x_var)) {
	# 	x_n_unique <- length(unique(df[[x_var[i]]])[!is.na(unique(df[[x_var[i]]]))])
	# 	if(x_n_unique<2) {
	# 		return(tibble(error="n_distinct(x_var)<2"))
	# 	} else if(x_n_unique >= 2L && x_n_unique < ordinal_if_fewer_than) {
	# 		df[[x_var[i]]] <- as.factor(df[[x_var[i]]])
	# 		df2 <- model.matrix.default(data = df, as.formula("~", x_var[i]))
	# 		df <- cbind(df, df2)
	# 		x_var <- c(x_var, colnames(df2))
	# 	} else if(x_n_unique == 2L) {
	# 		df[[x_var[i]]] <- as.integer(as.factor(df[[x_var[i]]])) - 1L
	# 	}
	# }
	# list(df=df, var_frame=var_frame)
# }

#' Title
#'
#' @param obj
#' @param cutoff_chisq
#' @param cutoff_CFI
#' @param cutoff_TLI
#' @param cutoff_RMSEA
#' @param cutoff_RMSEA_p05
#' @param cutoff_SRMR
#' @param drop_raw_fit
#'
#' @return
#' @export
#'
#' @examples
assess_fit <- function(obj, cutoff_chisq=.05,
								  cutoff_CFI=0.95, cutoff_TLI=0.95, cutoff_RMSEA=0.05, cutoff_RMSEA_p05=.05,
								  cutoff_SRMR=.05,
								  drop_raw_fit=TRUE) {
	if(!is.null(obj[["design_frame"]][["CFI"]])) {
	obj[["design_frame"]][["fit"]] <-
		ifelse((obj[["design_frame"]]$NDependentVars >= 3 &
					obj[["design_frame"]]$NIndependentVars > 0L) |
						obj[["design_frame"]]$NDependentVars >= 4L,
					 ifelse(obj[["design_frame"]]$ChiSqM_PValue >= cutoff_chisq, "Perfect",
					 	   ifelse(obj[["design_frame"]]$CFI >= cutoff_CFI &
					 	   	   	obj[["design_frame"]]$TLI >= cutoff_TLI &
					 	   	   	obj[["design_frame"]]$RMSEA_Estimate <= cutoff_RMSEA &
					 	   	   	obj[["design_frame"]]$RMSEA_pLT05 >= cutoff_RMSEA_p05 &
					 	   	   	obj[["design_frame"]]$SRMR <= cutoff_SRMR,
					 	   	   "Acceptable", "Low")), "NA")
	if(drop_raw_fit) {
		obj[["design_frame"]]$ChiSqM_Value <-
			obj[["design_frame"]]$ChiSqM_DF <-
			obj[["design_frame"]]$ChiSqM_PValue <-
			obj[["design_frame"]]$CFI <-
			obj[["design_frame"]]$TLI <-
			obj[["design_frame"]]$RMSEA_Estimate <-
			obj[["design_frame"]]$RMSEA_90CI_LB <-
			obj[["design_frame"]]$RMSEA_90CI_UB <-
			obj[["design_frame"]]$RMSEA_pLT05 <-
			obj[["design_frame"]]$SRMR <- NULL
	}
	}
	obj
}

add_effectsize <- function(obj, cutoff_sig=0.05, cutoff_r2_p = .05, cutoff_r2=.01,
						  effect_size_lower_cutoff = c("Low" = 0.1, "Medium" = .3, "High" = .5),
								  drop_raw_est=TRUE) {
	if(!is.null(obj[["design_frame"]][["r2"]])) {
		obj[["design_frame"]][["sig"]] <- ifelse()

	}
	obj
}

add_mi_diff <- function(model, group_name="FOCUS",
								 cutoff_chisq = .05,
								 cutoff_cfi = .95,
								 cutoff_tli = .95,
								 cutoff_rmsea = .08,
								 cutoff_srmr = .05, return_class="tbl") {
	if(!is.null(model$results$invariance_testing$models)) {
		a <- model$results$summaries %>%
			select(Model, ChiSqM_PValue, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, SRMR)
		b <- tibble(Model="SCALAR MODEL", ChiSq_ScalarEqConfigural = model$results$invariance_testing$compared[,"Pvalue"])

		a <- left_join(a,b, by="Model")

		if(!is.null(model$results$parameters$unstandardized$SCALAR.MODEL)) {
			c <- model$results$parameters$unstandardized$SCALAR.MODEL %>%
				filter(grepl("Means", paramHeader), Group==group_name) %>%
				select(est, pval) %>%
				mutate(Model="SCALAR MODEL", est =as.numeric(est), pval=as.numeric(pval))
			a <- left_join(a, c, by = "Model") %>%
				mutate(Model = gsub(" MODEL", "", Model),
					   Fit = if_else(ChiSqM_PValue >= cutoff_chisq & CFI >= cutoff_cfi & TLI >= cutoff_tli & RMSEA_Estimate <= cutoff_rmsea & SRMR <= cutoff_srmr, "perfect",
					   			  if_else(ChiSqM_PValue < cutoff_chisq & CFI > cutoff_cfi & TLI > cutoff_tli & RMSEA_Estimate < cutoff_rmsea & SRMR < cutoff_srmr, "acceptable",
					   			  		if_else(ChiSqM_PValue > cutoff_chisq & (CFI > cutoff_cfi | TLI > cutoff_tli | RMSEA_Estimate < cutoff_rmsea | SRMR < cutoff_srmr), "low", "poor"))))
		}
		if(return_class %in% c("tbl", "tibble") && require("tibble")) as_tibble(a) else as.data.frame(a)
	}
}

