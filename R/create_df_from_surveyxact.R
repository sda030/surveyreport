# TODO: Remove plyr::dlply. Entire line seems strange
# TODO: Should ensure output is haven_labelled
# TODO: ******** Rename variable names to match labels? *********
#TODO: Omit everything on splitting up into groups, etc. Kept in create_item_groups()
#TODO: Flervalgskryss får ikke riktig label-suffiks?
#TODO: Get SX to fix the bug in structure
#TODO: Get SX to fix the bug in Excel-sheets being separated into dataset(1)(2), etc.
#TODO: Hardcode variable names as this is only for SX-datasets?



create_df_from_surveyxact <- # Old version
	function(filepath=c(dataset="dataset.csv",
						structure ="structure.csv",
						labels="labels.csv"),
			 variableName="variableName",
			 variableLabel="questionText",
			 variableType="subType",
			 variableQuestion="variableQuestion",
			 variableGroup="variableGroup",
			 variableChoiceText = "choiceText",
			 value="value",
			 valueLabel="valueLabel",
			 valueType="valueType",
			 fixStupidSXbug=TRUE, rawAsList=FALSE) {

		filepath <- unlist(filepath)
		if(length(filepath)==1L && grepl(".xlsx", filepath)) {
			df_data <-
				grep("Dataset\\(*1*\\)*.*", readxl::excel_sheets(filepath), value = T) %>%
				purrr::map(.f = ~readxl::read_excel(path = filepath, sheet=.x)) %>%
				purrr::reduce(cbind) %>%
				as.data.frame()

			df_vars <-  readxl::read_excel(path = filepath, sheet="Structure")
			df_vars <- as.data.frame(df_vars)
			df_labels <- suppressMessages(readxl::read_excel(path = filepath, sheet="Labels", col_names = !fixStupidSXbug))
			df_labels <- as.data.frame(df_labels)
			colnames(df_labels) <- c(variableName, value, valueLabel)
		} else if(length(filepath)==3L && all(grepl(".csv", filepath))) {
			df_data <- read.csv2(file = filepath[names(filepath)=="dataset"], row.names = F, stringsAsFactors = F)
			df_vars <- read.csv2(file = filepath[names(filepath)=="structure"], row.names = F, stringsAsFactors = F)
			df_labels <- read.csv2(file = filepath[names(filepath)=="labels"], row.names = F, stringsAsFactors = F, header = !fixStupidSXbug, col.names = c(variableName, value, valueLabel))
		} else stop("filepath must be either an xlsx-file or the three csv-files dataset.csv, structure.csv and labels.csv")


		if(any(!c(variableName, variableLabel) %in% colnames(df_vars))) stop(paste0("Could not find columns ", paste0(c(variableName, variableLabel), collapse=", "), " in Structure"))
		if(any(!c(variableName, value, valueLabel) %in% colnames(df_labels))) stop(paste0("Could not find columns ", paste0(c(variableName, value, valueLabel), collapse=", "), " in Labels", collapse=", "))

		# Deler opp variableLabel => variableGroup og variableQuestion
		df_vars[[variableGroup]] <- gsub("(.*) - .*", "\\1", df_vars[[variableLabel]])
		df_vars[[variableQuestion]] <- gsub(".* - (.*)", "\\1", df_vars[[variableLabel]])
		df_vars$questionName <- NULL
		df_vars[[variableGroup]] <- gsub("\n$", "", df_vars[[variableGroup]])
		df_vars[[variableQuestion]] <- gsub("\n$", "", df_vars[[variableQuestion]])
		df_vars[[variableLabel]] <- gsub("\n$", "", df_vars[[variableLabel]])
		df_vars[[variableChoiceText]] <- gsub("\n$", "", df_vars[[variableChoiceText]])
		df_labels[[valueLabel]] <- gsub("\n$", "", df_labels[[valueLabel]])
		df_vars[[variableLabel]] <- ifelse(df_vars[[variableType]] == "Multiple",
										   paste(df_vars[[variableLabel]], df_vars[[variableChoiceText]],sep=" - "), df_vars[[variableLabel]])


		if(rawAsList) {
			return(list(structure=df_vars, dataset=df_data, labels=df_labels))
		} else {
			df_vars <-
				df_vars %>%
				dplyr::select(variableName, questionText) %>%
				tibble::deframe() %>%
				as.list()
			df_labels <-
				plyr::dlply(.variables = "variableName",
							.fun = function(df) tibble::deframe(df[,c("valueLabel","value")]))
			df_data %>%
				labelled::set_variable_labels(.labels = df_vars) %>%
				labelled::set_value_labels(.labels = df_labels) %>%
				return()
		}
	}




create_df_from_surveyxact <-
	function(filepath=c(dataset="dataset.csv",
						structure ="structure.csv",
						labels="labels.csv"),
			 fixStupidSXbug=TRUE, rawAsList=FALSE) {

		filepath <- unlist(filepath)
		if(length(filepath)==1L && grepl(".xlsx", filepath)) {
			df_data <-
				grep("Dataset\\(*1*\\)*.*", readxl::excel_sheets(filepath), value = T) %>%
				purrr::map(.f = ~readxl::read_excel(path = filepath, sheet=.x)) %>%
				purrr::reduce(cbind) %>%
				as.data.frame()

			df_vars <-  readxl::read_excel(path = filepath, sheet="Structure")
			df_vars <- as.data.frame(df_vars)
			df_labels <- suppressMessages(readxl::read_excel(path = filepath, sheet="Labels", col_names = !fixStupidSXbug))
			df_labels <- as.data.frame(df_labels)
			colnames(df_labels) <- c("variableName", "value", "valueLabel")
		} else if(length(filepath)==3L && all(grepl(".csv", filepath))) {
			df_data <- read.csv2(file = filepath[names(filepath)=="dataset"], row.names = F, stringsAsFactors = F)
			df_vars <- read.csv2(file = filepath[names(filepath)=="structure"], row.names = F, stringsAsFactors = F)
			df_labels <- read.csv2(file = filepath[names(filepath)=="labels"], row.names = F, stringsAsFactors = F, header = !fixStupidSXbug, col.names = c("variableName", "value", "valueLabel"))
		} else stop("filepath must be either an xlsx-file or the three csv-files dataset.csv, structure.csv and labels.csv")


		if(any(!c("variableName", "questionText") %in% colnames(df_vars))) stop(paste0("Could not find columns ", paste0(c("variableName", "questionText"), collapse=", "), " in Structure"))
		if(any(!c("variableName", "value", "valueLabel") %in% colnames(df_labels))) stop(paste0("Could not find columns ", paste0(c("variableName", "value", "valueLabel"), collapse=", "), " in Labels", collapse=", "))

		# Deler opp "questionText" => variableGroup og variableQuestion
		df_vars[["variableGroup"]] <- gsub("(.*) - .*", "\\1", df_vars[["questionText"]])
		df_vars[["variableQuestion"]] <- gsub(".* - (.*)", "\\1", df_vars[["questionText"]])
		df_vars$questionName <- NULL
		df_vars[["variableGroup"]] <- gsub("\n$", "", df_vars[["variableGroup"]])
		df_vars[["variableQuestion"]] <- gsub("\n$", "", df_vars[["variableQuestion"]])
		df_vars[["questionText"]] <- gsub("\n$", "", df_vars[["questionText"]])
		df_vars[["choiceText"]] <- gsub("\n$", "", df_vars[["choiceText"]])
		df_labels[["valueLabel"]] <- gsub("\n$", "", df_labels[["valueLabel"]])
		df_vars[["questionText"]] <- ifelse(df_vars[["subType"]] == "Multiple",
											paste(df_vars[["questionText"]], df_vars[["choiceText"]],sep=" - "), df_vars[["questionText"]])


		if(rawAsList) {
			return(list(structure=df_vars, dataset=df_data, labels=df_labels))
		} else {
			df_vars <-
				df_vars %>%
				dplyr::select(variableName, questionText) %>%
				tibble::deframe() %>%
				as.list()
			df_labels <-
				plyr::dlply(.variables = "variableName",
							.fun = function(df) tibble::deframe(df[,c("valueLabel","value")]))
			df_data %>%
				labelled::set_variable_labels(.labels = df_vars) %>%
				labelled::set_value_labels(.labels = df_labels) %>%
				return()
		}
	}
