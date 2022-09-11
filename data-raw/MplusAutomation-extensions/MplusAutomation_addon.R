#' Create syntactically valid and unique variable names for Mplus
#' 
#' Works best if you are not very concerned about having perfect names. You
#' just need something that works for Mplus.
#'     
#' @param x Character vector with original variable names.
#'
#' @return Character vector with new variable names with old names as names.
#' @export
#' @examples 
#' make_mplus_names(c("q_12_seserøjæklj3432ê", "_q_23_asrhbæløiejrè", 
#' "æøer32ü", "akrjh3uøo83aeh38æk æwerj", "q123234234234", 
#' "sæøæser333", "æHihHD"))

make_mplus_names <- function(x) {
	if(!is.character(x)) rlang::abort("argument l must contain a list of character vectors")
	
	y <- x
	y <- stringi::stri_trans_toupper(y)
	y <- stringi::stri_trans_general(y, "latin-ascii")
	# y <- iconv(as.character(y), "", "ASCII", "byte")
	y <- gsub("^[^[:alpha:]]*", "", y)
	y <- gsub("[\\.\\?\\-]*|[[:space:]]*", "", y)
	y <- stringi::stri_sub(str = y, to = 5)
	y <- make.unique(y, sep = "")
	names(y) <- x
	y
}

#' Internal function that reverts the names changed with make_mplus_names(). 
#' 
#' Currently does not work.
#'
#' @param res Results from Mplus object.
#' @param changes Changes made to names.
#'
#' @return Results, with original names instead of 
#'

revert_mplus_renaming <- function(res, changes) {
	res$term
	res$load_term
}

#' Rename variable names in data frame to align with Mplus.
#' 
#' make_mplus_names() does most of the work.
#'     
#' @param df data.frame or tibble.
#'
#' @return List with df being the newly modified data.fram and old_names.
#' @export
#' @examples 
#' data <- data.frame(`q_12_seserøjæklj3432ê`=1,
#' `_q_23_asrhbæløiejrè` = 0,
#' `æøer32ü` = 1, `akrjh3uøo83aeh38æk æwerj`=0,
#' `q123234234234`=1, `Sæøæser333` = 1,
#' `æHihHD` = 0)
#' 
#' 
#' out <- rename_mplus_df(df=data)
#' out$data

rename_mplus_df <- function(df) {
	if(!is.data.frame(df)) rlang::abort("argument df must be a data.frame or tibble")
	new_names <- make_mplus_names(names(df))
	names(df) <- unname(new_names)
	list(old_names=names(new_names), 
		 data=df)
}




#' Prepare Mplus Model Syntax and Run Model
#' 
#' Internal function. Runs a simple Y <- X model with Y and X being latent or manifest.
#'
#' @param df Data frame.
#' @param y_var String. 
#' @param x_var String. 
#' @param y_group String. 
#' @param x_group String. 
#' @param y_type String. 
#' @param x_type String. 
#' @param cluster String. Optional.
#' @param estimator String. One of "wlsmv", "mlr", or NA
#' @param path Path to Mplus files. 
#'
#' @return Data frame row.
#' @importFrom MplusAutomation mplusObject mplusModeler
#' @importFrom rlang arg_match

prepare_mplus_model <- 
	function(df, y_var, x_var, y_group, x_group, y_type, x_type, 
			 cluster, estimator=c("wlsmv", "mlr", NA), path=tempdir()) {
		estimator <- rlang::arg_match(estimator)
		
		x_syntax <- paste0(" ON ", paste0(x_var, collapse="\n"), ";")
		
		y_group_short <- gsub("^([[:alnum:]_]{,6}).*", "\\1", y_group)
		y_syntax <- paste0(paste0(y_var, collapse="\n"))
		if(length(y_var)>1L) {
			model_syntax <- paste0(paste0(y_group_short, " BY ", y_syntax, ";\n"),
								   if(all(!is.null(x_var) && !is.na(x_var))) paste0(y_group_short, " ", x_syntax))
		} else {
			model_syntax <- if(all(!is.null(x_var) && !is.na(x_var))) paste0(y_syntax,  x_syntax)
		}
		
		cluster_syntax <- if(!is.na(cluster)) paste0("CLUSTER = ", cluster,";\n")

		if(y_type != "interval") {
			y_variable_syntax_suffix <- paste0(y_var, collapse="\n")
			if(y_type == "ordinal") {
				y_variable_syntax <- paste0("CATEGORICAL = ", y_variable_syntax_suffix, ";\n")
			} else if(y_type == "nominal") {
				y_variable_syntax <- paste0("NOMINAL = ", y_variable_syntax_suffix, ";\n")
			} else rlang::abort("y_type must be one of interval, ordinal or nominal")
		} else y_variable_syntax <- ""
		
		title <- paste0(paste0(y_var, collapse=", "), "\n",
						if(all(!is.null(x_var) && !is.na(x_var))) " ON ",
						if(all(!is.null(x_var) && !is.na(x_var))) paste(x_var, collapse=" "), ";")
		variable <- paste0(y_variable_syntax, cluster_syntax)
		analysis <- paste0(if(estimator == "wlsmv") "ESTIMATOR=WLSMV;\n" else "ESTIMATOR=MLR;\n",
						   if(!is.na(cluster)) "TYPE = COMPLEX;")
		file_path <- file.path(path, paste0(y_group_short, if(all(!is.null(x_var) && !is.na(x_var))) x_group, ".inp"))
		
	 	MplusAutomation::mplusObject(TITLE = title, VARIABLE = variable, ANALYSIS = analysis, MODEL = model_syntax,
									 OUTPUT = "STD;", usevariables = colnames(df), rdata = df) %>%
			MplusAutomation::mplusModeler(object = ., modelout = file_path, run = 1L, hashfilename = FALSE, varwarnings = TRUE, writeData = "always")
	}

#' Extract Errors From MplusAutomation Model Object
#' 
#' First version of this is very crude, simply extracting every error and 
#'     returning a string.
#'     
#' @param mod MplusAutomation model object.
#'
#' @return String
#' @export 

extract_mplus_error <- function(mod) {
	paste0(unlist(mod[["results"]][["errors"]]), collapse=". ")
}

#' Extract MplusAutomation Results Into A Single Row
#' 
#' Takes a MplusAutomation object and names of variables to extract. If model 
#'     contains error, will attempt to extract these.
#' 
#' @param mod mplusAutomation.model object.
#' @param y_var,x_var Character vector with dependent/independent variables to
#'   extract.
#' @param y_group,x_group String with name of factor.
#' @importFrom dplyr filter select rename mutate summarize matches %>%
#' @importFrom rlang .data .env
#' @importFrom tidyr chop
#' @return A single data frame row with most important information.
#' @export

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
				dplyr::filter(grepl(header_on, .data$paramHeader)) %>%
				dplyr::select(-dplyr::all_of(c("paramHeader", "se", "est_se"))) %>%
				dplyr::rename(term = dplyr::all_of("param"), # Names aligned with broom
							  estimate = dplyr::all_of("est"), 
							  p.value = dplyr::all_of("pval")) %>%
				tidyr::chop(cols = c(.data$term, .data$estimate, .data$p.value))
			if(nrow(mod_par)==1L) mod_out <- cbind(mod_out, mod_par)
		}
		
		if(length(y_var)>1L) {
			mod_load <-
				mod %>%
				.[["results"]] %>%
				.[["parameters"]] %>%
				.[["unstandardized"]] %>%
				dplyr::filter(grepl(paste0(toupper(.env$y_group), "\\.BY") , .data$paramHeader), 
							  !(.data$est==1.000 & .data$se==999.000)) %>%
				dplyr::select(-dplyr::all_of(c("paramHeader", "se", "est_se"))) %>%
				dplyr::rename(load_term = dplyr::all_of("param"), 
							  load_est = dplyr::all_of("est"), 
							  load_p.value = dplyr::all_of("pval")) %>%
				tidyr::chop(cols = c(.data$load_term, .data$load_est, .data$load_p.value))
			if(nrow(mod_load)==1L) mod_out <- cbind(mod_out, mod_load)
			
		}
		
		if(!is.null(mod[["results"]][["summaries"]])) {
			mod_sum <-
				mod %>%
				.[["results"]] %>%
				.[["summaries"]] %>%
				dplyr::select(-all_of(c("Mplus.version", "Title", "AnalysisType", "DataType",
										"Parameters", "Estimator", "NGroups",
										"NContinuousLatentVars")), -dplyr::matches("Baseline"))
			if(nrow(mod_sum)==1L) mod_out <- cbind(mod_out, mod_sum)
			if(all(!is.null(x_var) && !is.na(x_var))) {
				mod_r2 <-
					mod %>%
					.[["results"]] %>%
					.[["parameters"]] %>%
					.[["r2"]] %>%
					dplyr::filter(.data$param== if(length(.env$y_var)>1L) toupper(.env$y_group) else toupper(.env$y_var)) %>%
					dplyr::mutate(r2=as.character(.data$est), r2_p=as.character(.data$pval)) %>%
					dplyr::select(all_of(c("r2", "r2_p")))
				if(nrow(mod_r2)==1L) mod_out <- cbind(mod_out, mod_r2)
			}
		}
	}
	mod_out
}

#' Extract Regression Coefficients from An Mplus Object
#' 
#' Major changes will follow. Should adapt to/merge with tidySEM-package.
#'
#' @param object Mplus object as returned from MplusAutomation::mplusModeler().
#'
#' @return data.frame with 
#' @importFrom stats p.adjust
#' @export
#'
#' @examples
extract_mplus_reg_coefs <- function(object) {
	std_res <- object$results$parameters$stdy.standardized
	std_ci <- object$results$parameters$ci.stdy.standardized
	orig_df <- object$rdata
	R2 <- object$results$parameters$r2[, c("param", "est")]
	names(R2) <- c("paramHeader", "r2")
	mod_n <- object$results$summaries[1,c("Observations")]
	
	std_res$padj <- round(stats::p.adjust(std_res$pval, method = "fdr"), 3)
	std_res$se <- NULL
	std_res$est_se <- NULL
	
	out <- merge(x = std_res, 
					 y= std_ci[, c("paramHeader", "param", "low2.5", "up2.5")])

	out$paramHeader <- gsub("\\.ON$", "", out$paramHeader)
	out$param <- tolower(out$param)
	out$x_group <- gsub("(.*)_.*", "\\1", out$param)
	out$sig <- out$padj < .05
	out <- merge(x=out, y=R2)

	out$paramHeader <- tolower(out$paramHeader)
	
	orig_df_lab <- lapply(orig_df, FUN=function(v) attr(v, "label"))
	orig_df_lab <- unlist(orig_df_lab)
	orig_df_lab <- data.frame(variable = names(orig_df_lab), 
							  y_label = unname(orig_df_lab))


	out <- merge(x = out, y=orig_df_lab,
				 by.x = "paramHeader", by.y="variable", all.x=TRUE, all.y=FALSE)
	
	names(orig_df_lab)[names(orig_df_lab) == "y_label"] <- "x_label"
	out <- merge(x = out, y=orig_df_lab,
				 by.x = "param", by.y="variable", all.x=TRUE, all.y=FALSE)
	
	names(orig_df_lab)[names(orig_df_lab) == "x_label"] <- "x_group_label"
	out <- merge(x = out, y=orig_df_lab,
				 by.x = "x_group", by.y="variable", all.x=TRUE, all.y=FALSE)
	
	names(out)[names(out) == "param"] <- "x"
	names(out)[names(out) == "paramHeader"] <- "y"
	
	out$n <- as.integer(mod_n)

	out
}


#' Run a Default CFA Model in MplusAutomation, optionally with Measurement Invariance
#'
#' @param df data.frame or tibble
#' @param group_var String, optional grouping variable for measurement invariance (not supporting alignment yet).
#' @param mplus_out_file String, name of *.inp file. Note, paths are not supported.
#' @param estimator String, if WLSMV, will assume ordinal/binary for all variables. Any estimator supported.
#' @param cluster_var Optional, string for cluster variable.
#' @param strata_var Optional, string for stratification variable.
#' @param mi Character vector of automatic measurement models to run. Defaults to configural, metric and scalar.
#' @param cluster_twolevel Logical, whether to run TYPE=TWOLEVEL instead of  TYPE=COMPLEX.
#'
#' @return Mplus object
#' @export
#'
#' @examples mplus_cfa_mi(labelled::remove_labels(ex_survey1[,paste0("a_", 1:9)]))
mplus_cfa_mi <- 
	function(df, group_var=NULL, mplus_out_file="cfa_mi_tmp.inp", estimator=NULL, 
			 cluster_var=NULL, strata_var=NULL, mi=c("CONFIGURAL", "METRIC", "SCALAR"), cluster_twolevel=FALSE) {
			
		df <- as.data.frame(lapply(df, function(.x) {
			attributes(.x) <- NULL
			.x
			}))  

		if(!is.null(cluster_var)) {
			if(is.null(df[[cluster_var]])) rlang::abort("Cannot find cluster_var in dataset.")
			do_cluster <- TRUE
		} else do_cluster <- FALSE
		
		if(!is.null(strata_var)) {
			if(is.null(df[[strata_var]])) rlang::abort("Cannot find strata_var in dataset.")
			do_strata <- TRUE
		} else do_strata <- FALSE
		
		if(!is.null(group_var)) {
			if(is.null(df[[group_var]])) rlang::abort("Cannot find group_var in dataset.")
			do_mi <- TRUE
		} else do_mi <- FALSE
		
		categories <- if(do_mi) sort(unique(df[[group_var]])[!is.na(unique(df[[group_var]]))])
		
		if(!cluster_twolevel) {
			model_str <- 
				paste0(
					"f BY ", 
					paste0(names(df)[!names(df) %in% c(if(do_mi) group_var, if(do_cluster) cluster_var, strata_var)], 
						   collapse = "\n"), ";\n", if(!do_mi) "[f@0];\n")
		} else {
			model_str <- 
				paste0("%BETWEEN%\n",
					   "f_b BY ", 
					   paste0(names(df)[!names(df) %in% c(if(do_mi) group_var, if(do_cluster) cluster_var, strata_var)], 
					   	   collapse = "\n"), ";\n[f@0];",
					   "%WITHIN%\n",
					   "f_w BY ",
					   paste0(names(df)[!names(df) %in% c(if(do_mi) group_var, if(do_cluster) cluster_var, strata_var)], 
					   	   collapse = "\n"), ";\n[f@0];")
		}
		variable_str <-
			paste0(
				if(!is.null(estimator) && estimator=="WLSMV") paste0("CATEGORICAL = ", 
											  paste0(names(df)[!names(df) %in% c(group_var, cluster_var, strata_var)], collapse = "\n"), 
											  ";\n"), 
				if(do_mi) paste0("GROUPING = ", group_var, "(", categories[1], "=REF ", categories[2], "=TARGET);\n"),
				if(do_cluster) paste0("CLUSTER = ", cluster_var, ";\n"),
				if(do_strata) paste0("STRATIFICATION = ", strata_var, ";\n"))
		analysis_str <-
			paste(sep = "\n", 
				  if(!is.null(estimator)) paste0("ESTIMATOR = ", estimator, ";"), 
				   if(do_mi) paste0("MODEL = ", paste0(mi, collapse=" "), ";"),
				   if(do_cluster | do_strata | cluster_twolevel) paste0("TYPE = ", if(do_cluster | do_strata) "COMPLEX", if(cluster_twolevel) " TWOLEVEL", ";"))
		out <-
			MplusAutomation::mplusObject(rdata = df, usevariables = names(df), autov = F,
										 VARIABLE = if(length(variable_str)>0) variable_str,
										 ANALYSIS = if(length(analysis_str)>0) analysis_str,
										 MODEL = model_str,
										 OUTPUT = paste0(if(!do_mi) "STDY MOD(ALL) ", "RESIDUAL TECH1 TECH2 TECH3 TECH4 TECH5;"),
										 PLOT = if(!do_mi) "TYPE = PLOT3;" else "TYPE = PLOT1;",
										 SAVEDATA = if(!do_mi) paste0("FILE = ", gsub("\\.inp", "_savedata.dat", mplus_out_file), ";\n",
										 				  "SAVE = FSCORES;\n",
										 				  if(estimator=="WLSMV") paste0("DIFFTEST = ", gsub("\\.inp", "_difftest.dat", mplus_out_file), ";")))
		MplusAutomation::mplusModeler(object = out, modelout = mplus_out_file, run = 1L, writeData = "always", hashfilename = FALSE)
	}

#TODO: BIC/AIC when available.
#TODO: Only chisquare when available.
#' Get Parameters from MplusObject, optionally with Measurement Invariance 
#' Parameters
#'
#' @param singlegroup_object Mplus model without MI setup (for comparison)
#' @param mi_object MplusAutomation results object with automated measurement invariance.
#' @param low_loading Lower limit for flagging bad loadings.
#' @param alpha Critical alpha level.
#'
#' @return data.frame with columns Model, Observations, NDependentVars, ChiSqM_Value,
#' CFI, TLI, RMSEA_Estimate, SRMR, Bad_std_loadings
#' @export
#'
#' @examples data <- labelled::remove_labels(ex_survey1[,c(paste0("a_", 1:9))])
#' data2 <- labelled::remove_labels(ex_survey1[,c("x1_sex", paste0("a_", 1:9))])
#' mod <- mplus_cfa_mi(data)
#' mod_mi <- mplus_cfa_mi(data2, group_var="x1_sex")
#' get_mi_pars(mod, mod_mi)
#' 
get_mi_pars <- function(singlegroup_object=NULL, mi_object=NULL, low_loading=.4, alpha=.05) {
	if(is.null(mi_object) & is.null(singlegroup_object)) {
		warning(message = "Must provide at least object or single_group_object.")
		return(data.frame())
	}
	extract_vars2 <- c("CFI", "TLI", "RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB", "SRMR") # 
	extract_vars2b <- c("AIC", "BIC", "aBIC", "AICC")
	extract_vars <- c("Observations", "NDependentVars", "ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue", extract_vars2)
	
	if(is.null(mi_object) && 
	   !is.null(singlegroup_object$results$summaries) && 
	   nrow(singlegroup_object$results$summaries) > 1L) {
		mi_object <- singlegroup_object
		singlegroup_object <- NULL
	}
	
	if(!is.null(mi_object)) {
		mi_mods <- cbind(mi_object$results$summaries[, c("Model", extract_vars)], 
						 Bad_std_loadings=NA)


		comp_df <- as.data.frame(mi_object$results$invariance_testing$compared)
		names(comp_df) <- c("ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue")
		comp_df$Model <- rownames(comp_df)
		comp_df$Bad_std_loadings <- NA_character_
		rownames(comp_df) <- NULL

		
		config_exists <- any(grepl("CONFIGURAL", comp_df[,"Model"], ignore.case = TRUE)) |  any(grepl("CONFIGURAL", mi_mods[,"Model"], ignore.case = TRUE))
		metric_exists <- any(grepl("METRIC", comp_df[,"Model"], ignore.case = TRUE)) |  any(grepl("METRIC", mi_mods[,"Model"], ignore.case = TRUE))
		scalar_exists <- any(grepl("SCALAR", comp_df[,"Model"], ignore.case = TRUE)) |  any(grepl("SCALAR", mi_mods[,"Model"], ignore.case = TRUE))

		tmp_df <- cbind(Observations=min(mi_mods$Observations), 
						NDependentVars = min(mi_mods$NDependentVars))
		
		if(config_exists && metric_exists) {
			met_con_df <- cbind(comp_df[comp_df$Model == "Metric against Configural", ],
								mi_mods[mi_mods$Model == "CONFIGURAL MODEL", extract_vars2] -
									mi_mods[mi_mods$Model == "METRIC MODEL", extract_vars2],
								tmp_df)
		}
		if(config_exists && scalar_exists) {
			sca_con_df <- cbind(comp_df[comp_df$Model == "Scalar against Configural", ],
								mi_mods[mi_mods$Model == "CONFIGURAL MODEL", extract_vars2] -
									mi_mods[mi_mods$Model == "SCALAR MODEL", extract_vars2],
								tmp_df)
		}
		if(metric_exists && scalar_exists) {
			sca_met_df <- cbind(comp_df[comp_df$Model == "Scalar against Metric", ],
								mi_mods[mi_mods$Model == "METRIC MODEL", extract_vars2] -
									mi_mods[mi_mods$Model == "SCALAR MODEL", extract_vars2],
								tmp_df)
		}
	}
	
	
	if(!is.null(singlegroup_object)) {
		sin_sum <- singlegroup_object$results$summaries
		if(!is.null(sin_sum$ChiSqM_Value)) {
			sin_mod <- sin_sum[, extract_vars]
			sin_par <- singlegroup_object$results$parameters$stdy.standardized
			bad_load <- sin_par[grepl("\\.BY$", sin_par$paramHeader) & 
									(sin_par$pval >= alpha | sin_par$est < low_loading), 
								c("param", "est")]
			bad_load <- 
				lapply(seq_len(nrow(bad_load)), function(i) {
					paste0(bad_load[i, "param"], "(", bad_load[i, "est"], ")")
				})
			sin_mod$Bad_std_loadings <- paste0(unlist(bad_load), collapse=",")
			
			sin_mod$Model <- "SingleGroup"
			
			if(!is.null(mi_object) && config_exists) {
				con_sin_df <- cbind(Model = "Configural against SingleGroup", 
									ChiSqM_Value = NA_real_, 
									ChiSqM_DF = NA_real_,  
									ChiSqM_PValue = NA_real_, 
									Bad_std_loadings = NA_character_,
									Observations = sin_sum$Observations, 
									NDependentVars = sin_sum$NDependentVars,
									mi_mods[mi_mods$Model == "CONFIGURAL MODEL", extract_vars2] - 
										sin_mod[, extract_vars2])
			}
		} else sin_mod <- data.frame(Observations = sin_sum$Observations, 
									 NDependentVars = sin_sum$NDependentVars)
	}

	out <-
		rbind.data.frame(
		if(exists("sin_mod")) sin_mod,
		if(exists("mi_mods")) mi_mods,
		if(exists("met_con_df")) met_con_df,
		if(exists("sca_con_df")) sca_con_df,
		if(exists("sca_met_df")) sca_met_df,
		if(exists("con_sin_df")) con_sin_df,
		make.row.names = FALSE, stringsAsFactors = FALSE)
	out[, c("Model", names(out)[names(out) != "Model"])]
}



#' Plot Conditional Scale Reliability from Observed Factor SE
#'
#' @param object Mplus object
#' @param latent_variable String, name of latent variable in Mplus model.
#' @import ggplot2
#' @return ggplot-object
#' @export
#'
#' @examples
mplus_condition_reliability_from_fscores <- function(object, latent_variable="F") {
	data <- object[["results"]][["savedata"]]
	data$TI <- data[[paste0(latent_variable, "_SE")]]^-2
	data$TI <- data[["TI"]]/(1+data[["TI"]])
	data$theta <- data[[latent_variable]]
	ggplot2::ggplot(data = data, ggplot2::aes_string(x="theta", y="TI")) +
		ggplot2::geom_smooth(method = "loess") +
		ggplot2::coord_cartesian(xlim = c(-3, 3), ylim = c(0, 1), expand = F) +
		ggplot2::scale_y_continuous(breaks = seq(from=0, to=1, by=.1)) +
		ggplot2::theme_classic() +
		ggplot2::theme(legend.position = "bottom",
					   panel.grid.major.y = ggplot2::element_line(colour = "gray90")) +
		ggplot2::guides(colour = ggplot2::guide_legend(nrow=1)) +
		ggplot2::labs(title=NULL, subtitle = NULL, x = expression(Level~of~latent~variable~(italic(theta)[p])),
					  colour=NULL, linetype=NULL,
					  y = "Conditional Scale Reliability")
}


# mplusDifferenceTest <- function(modList = list(), method=c("chisq", "ll")) {
#     lapply(modList, function(mod) {
#         if(mod$results$summaries$estimator %in% c("MLM", "MLR", "WLSM") & any(method %in% "chisq")) {
#             
#                 cd <- (mod$results$summaries$d0 * mod$results$summaries$c0 - 
#                            mod$results$summaries$d1*mod$results$summaries$c1)/
#                     (mod$results$summaries$d0 - mod$results$summaries$d1)
#                 
#                 TRd <- (mod$results$summaries$T0*mod$results$summaries$c0 - mod$results$summaries$T1*mod$results$summaries$c1)/cd
#                 
#         } else if(mod$results$summaries$estimator == "MLR" & any(method %in% "ll"))
#     })
# 
#     
# }
# 
