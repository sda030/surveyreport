
# tmp <- 
# 	data.frame(	   
# 		xsex = sample(x=c("M", "F"), replace = T, size = 100), 
# 		xhuman = sample(x=c(1, 0), replace = T, size = 100), 
# 		a_1=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_2=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_3=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_4=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_5=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_6=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_7=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
# 		a_8=sample(x=c("Yes", "No"), replace = T, size = 100),
# 		b_1=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
# 		b_2=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
# 		b_3=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
# 		b_4=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
# 		c_1=sample(x=c(pi, 4), replace = T, size = 100),
# 		d_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		d_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		d_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		e_1=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		e_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		e_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		e_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		f_1=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		f_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		f_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		f_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		g_1=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		g_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		g_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		g_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
# 		h_1=sample(x=paste0("University of ", LETTERS[1:2]), replace = T, size = 100),
# 		h_2=sample(x=paste0("University of ", LETTERS[3:4]), replace = T, size = 100),
# 		h_3=sample(x=paste0("University of ", LETTERS[5:6]), replace = T, size = 100),
# 		h_4=sample(x=paste0("University of ", LETTERS[7:8]), replace = T, size = 100)
# 	)
# 
# labelled::var_label(tmp) <- c("Gender", "Is human?", 
# 							  paste0("Survey question about ", 
# 							  	   gsub("^([[:alpha:]])_.$", "\\1", names(tmp))[-c(1:2)], 
# 							  	   " - Item text: ",
# 							  	   1:(ncol(tmp)-2)))
# tmp <- labelled::to_labelled(x = tmp)
saveRDS(object = tmp, file = "inst/extdata/ex_survey.RDS", compress = FALSE)