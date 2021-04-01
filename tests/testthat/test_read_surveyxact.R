testthat::test_that("can read Excel and CSV-files (tab, UTF-16)", {
	ex_survey2_xlsx <- 
		read_surveyxact(filepath = system.file("extdata", "ex_survey2.xlsx",
											   package = "surveyreport", mustWork = TRUE))
	
	dataset <- system.file("extdata", "ex_survey2_tab_utf16", "dataset.csv",
						   package = "surveyreport", mustWork = TRUE)
	labels <- system.file("extdata", "ex_survey2_tab_utf16", "labels.csv",
						  package = "surveyreport", mustWork = TRUE)
	structure <- system.file("extdata", "ex_survey2_tab_utf16", "structure.csv",
							 package = "surveyreport", mustWork = TRUE)
	ex_survey2_tab_utf16 <- read_surveyxact(filepath=c(dataset = dataset,
													   labels = labels,
													   structure = structure))
	testthat::expect_identical(ex_survey2_xlsx, ex_survey2_tab_utf16)
	# waldo::compare(ex_survey2_xlsx, ex_survey2_tab_utf16)
})


# ex_survey2_comma_ansi <- 
# 	read.csv("inst/extdata/ex_survey2_comma_ansi/complete.csv", sep = ",")
# any(grepl("[æøåÆØÅ]", ex_survey2_comma_ansi$Skole))
# ex_survey2_semicolon_ansi <- 
# 	read.csv("inst/extdata/ex_survey2_semicolon_ansi/complete.csv", fileEncoding = "iso8859-1", sep = ";")
# any(grepl("[æøåÆØÅ]", ex_survey2_semicolon_ansi$Skole))
# ex_survey2_tab_utf16 <- 
# 	read.csv("inst/extdata/ex_survey2_tab_utf16/complete.csv", sep = "\t", fileEncoding = "UTF-16")
# any(grepl("[æøåÆØÅ]", ex_survey2_tab_utf16$Skole))
# 
# identical(ex_survey2_comma_ansi$Skole, ex_survey2_semicolon_ansi$Skole)
# identical(ex_survey2_comma_ansi$Skole, ex_survey2_tab_utf16$Skole)
# waldo::compare(ex_survey2_comma_ansi$Skole, ex_survey2_semicolon_ansi$Skole)
# waldo::compare(ex_survey2_comma_ansi$Skole, ex_survey2_tab_utf16$Skole)
# waldo::compare(ex_survey2_semicolon_ansi$Skole, ex_survey2_tab_utf16$Skole)

