
source("C:/Users/py128/NIFU/Metode - General/Programvaremaler/R/create_item_groups.R")
source("C:/Users/py128/NIFU/Metode - General/Programvaremaler/R/tablemaker.R")
tmp <- 
	tibble(	   
		xsex = sample(x=c("M", "F"), replace = T, size = 100), 
		xhuman = sample(x=c(1, 0), replace = T, size = 100), 
		a_1=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_2=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_3=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_4=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_5=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_6=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_7=sample(x=c("Yes", "No", NA_character_), replace = T, size = 100),
		a_8=sample(x=c("Yes", "No"), replace = T, size = 100),
		b_1=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
		b_2=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
		b_3=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
		b_4=sample(x=c("Not at all", "A bit", "A lot (æøå)"), replace = T, size = 100),
		c_1=sample(x=c(pi, 4), replace = T, size = 100),
		d_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		d_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		d_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		e_1=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		e_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		e_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		e_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Neither disagree nor agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		f_1=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		f_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		f_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		f_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		g_1=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		g_2=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		g_3=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		g_4=sample(x=c("Strongly disagree", "Somewhat disagree", "Slightly disagree", "Neither disagree nor agree", "Slightly agree", "Somewhat agree", "Strongly agree"), replace = T, size = 100),
		h_1=sample(x=paste0("University of ", LETTERS[1:2]), replace = T, size = 100),
		h_2=sample(x=paste0("University of ", LETTERS[3:4]), replace = T, size = 100),
		h_3=sample(x=paste0("University of ", LETTERS[5:6]), replace = T, size = 100),
		h_4=sample(x=paste0("University of ", LETTERS[7:8]), replace = T, size = 100),
	)

labelled::var_label(tmp) <- c("Gender", "Is human?", 
							  paste0("Survey question about ", 
							  	   gsub("^([[:alpha:]])_.$", "\\1", names(tmp))[-c(1:2)], 
							  	   " - Item text: ",
							  	   1:(ncol(tmp)-2)))

tmp_uni <- 
	tablemaker(df=select(tmp, matches("^[^x]")), 
			   dependent_vars = select(tmp, matches("^[^x]")) %>%
					   	create_item_groups(df=.),
					   include_missing=F, 
			   colour_choice = c("#E8B0B7", "#DE919A", "#D5727D", "#C84957", "#BC3848", "#9D2F3C", "#7E2630"))
tmp_uni2 <- 
	tablemaker(df=select(tmp, matches("^[^x]")), 
			   dependent_vars = select(tmp, matches("^[^x]")) %>%
			   	create_item_groups(df=.),
			   include_missing=F, filepath_xlsx = "tmp2.xlsx", filepath_docx = "tmp2.docx", filepath_pptx = "tmp2.pptx", 
			   colour_choice = c("#90D4E0", "#70C7D7", "#50BBCE", "#36AABF", "#2D8E9F", "#24727F", "#1B555F"))
