
set.seed(4321)
library(dplyr)
library(tibble)
ex_survey1 <-
	tibble::tibble(
		x1_sex = labelled::labelled(sample(x=1:2, replace = T, size = 100), labels = c("Males"=1, "Females"=2)),
		x2_human = labelled::labelled(sample(x=0:1, replace = T, size = 100), labels = c("Robot?"=0, "Definitely humanoid"=1)))
ex_survey1[,c(paste0("a_", 1:9), paste0("b_", 1:3), paste0("c_", 1:2), paste0("d_", 1:4), paste0("e_", 1:4), paste0("p_", 1:4), "f_uni")] <- NA
ex_survey1 <- 
	ex_survey1 %>%
	dplyr::mutate(
		dplyr::across(dplyr::all_of(paste0("a_", 1:9)), 
					  ~labelled::labelled(sample(x=c(0, 1, NA_real_), replace = T, size = 100, prob=c(.45,.45,.1)),
					  					labels = c("Yes"=0, "No"=1))),
		dplyr::across(dplyr::all_of(paste0("b_", 1:3)), 
					  ~labelled::labelled(sample(x=0:2, replace = T, size = 100, prob=c(.45,.45,.1)),
					  					labels = c("Not at all"=0, "A bit"=1, "A lot"=2))),
		dplyr::across(dplyr::all_of(paste0("c_", 1:2)), 
					  ~round(rnorm(n = 100, mean = 20, sd = 5), 1)),
		dplyr::across(dplyr::all_of(paste0("d_", 1:4)), 
					  ~labelled::labelled(sample(x=0:10, replace = T, size = 100),
					  					labels = rlang::set_names(0:10, c("0\nCannot do at all", 1:4, "5\nModerately can do", 6:9, "10\nHighly certain can do")))),
		dplyr::across(dplyr::all_of(paste0("e_", 1:4)), 
					  ~labelled::labelled(sample(x=0:4, replace = T, size = 100),
					  					labels = rlang::set_names(0:4, c("Very rarely", "Rarely", "Sometimes", "Often", "Very often")))),
		dplyr::across(dplyr::all_of(paste0("p_", 1:4)), 
					  ~labelled::labelled(sample(x=0:3, replace = T, size = 100, prob = c(.2,.2,.2,.4)),
					  					labels = rlang::set_names(0:3, c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")))),
		f_uni=sample(x=paste0("University of ", LETTERS[1:5]), replace = T, size = 100),
		resp_status = sample(x=1:6, replace=TRUE, size=100, prob = c(.01, .1, .15, .7, .03, .01)))

labelled::var_label(ex_survey1) <- c(
	"Gender", 
	"Is respondent human?",
	paste0("Do you consent to the following? - Agreement #", 1:9),
	paste0("How much do you like living in - ", c("Bejing", "Brussels", "Budapest")),
	paste0("How many years of experience do you have in - ", c("Company A", "Company B")),
	paste0("Rate your degree of confidence doing the following - ", c("Driving", "Drinking", "Driving", "Dancing")),
	paste0("How often do you do the following? - ", c("Eat", "Eavesdrop", "Exercise", "Encourage someone whom you have only recently met and who struggles with simple tasks that they cannot achieve by themselves")),
	paste0("To what extent do you agree or disagree to the following policies - ", c("Red", "Green", "Yellow", "Blue"), " Party"),
	paste0("Which of the following universities would you prefer to study at?"),
	"Response status (1=Not sent out, 2=Unopened, 3=Some responses, 4=Completed, 5=Excused, 6=Not serious)")

usethis::use_data(ex_survey1, overwrite = TRUE)


