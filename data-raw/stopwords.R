## code to prepare `stopwords` dataset goes here
stopwords <- jsonlite::read_json(path = 
								 	system.file("inst", "extdata", "stopwords-iso.json", package = "surveyreport", mustWork = TRUE), 
								 simplifyVector = TRUE)
usethis::use_data(stopwords, overwrite = TRUE, internal = FALSE)
