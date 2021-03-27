#	Create/continue development of following functions that should be possible to run in the following sequence:
#	User checks that labels are correctly named and labelled in SX, with correct values. No modifications (dummies, cuts, centering, etc))
#	user runs preprocessing, either all up front manually or within recipes)
#	model_batch
#	User adds whatever is missing from or modifies model_batch output
#	parsnip
#	chart_data (only add labels, etc from df_vars after processing)
#	tab_data
#	chart_model
#	write_report_results



### Mal for NIFU-analyser av survey-data
## Disse R-pakkene trengs for å kjøre alt det nedenfor. 
install.packages(c("tidyr", "dplyr", "readr", "haven", "labelled", "mschart", "officer", "MplusAutomation", "purrr", 
				   "writexl", "readxl", "forcats", "tibble", "RColorBrewer", "plyr"))


