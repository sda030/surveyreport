
# TODO: Text reporting: Randomize among glue templates
# TODO: Text reporting: Commenting on fit.
# TODO: Text reporting: Adding adverbial clauses to spice up: If contrast to previous finding. 
# TODO: Text reporting: Summarize findings if all of the same pattern (X positively correlated with all of Y1, Y2, Y3, ... and detailed=FALSE)
# TODO: Text reporting: Distinguish between first-time mention of a variable within a section (where full x_label is mentioned "Vi finner for x_label at...)
# TODO: Text reporting: named vector/list with phrases to look for and reuse: list(list("enig", "er {str_more_less} enig i påstanden '{y_label}' enn")), list("liker", "liker {str_more_less} {y_label} enn", 

add_text <- function(design_frame,
					 str_agrees_sig_to = "{x_val_label_focus} ({value_focus}) er signifikant {compare} enig i påstanden {y_group_label} enn {x_val_label_ref} ({value_ref}).", 
					 str_construct_fit = "Samlemålet {y_group_label} har {fit} psykometriske egenskaper.", ) {
	str_glue_agrees_1_above_2 <- c("{x_cat_1} er mer enig enn {tolower(x_cat_2)} i påstanden {y_label} ({p_1} kontra {p_2} prosent, en forskjell på {p_1-p_2} prosentpoeng).",
								   "{x_cat_2} er mindre enig enn {tolower(x_cat_1)} i påstanden {y_label} ({p_2} kontra {p_1} prosent, en forskjell på {p_1-p_2} prosentpoeng).",
								   "Flere {tolower(x_cat_1)} enn {tolower(x_cat_2)} er enig i påstanden {y_label} ({p_1} kontra {p_2} prosent, en forskjell på {p_1-p_2} prosentpoeng).",
								   "Færre {tolower(x_cat_2)} enn {tolower(x_cat_1)} er enig i påstanden {y_label} ({p_2} kontra {p_1} prosent, en forskjell på {p_1-p_2} prosentpoeng).")
	str_glue_agrees_2_above_1 <- c("{x_cat_2} er mer enig enn {tolower(x_cat_1)} i påstanden {y_label} ({p_2} kontra {p_1} prosent, en forskjell på {p_2-p_1} prosentpoeng).",
								   "{x_cat_1} er mindre enig enn {tolower(x_cat_2)} i påstanden {y_label} ({p_1} kontra {p_2} prosent, en forskjell på {p_2-p_1} prosentpoeng).",
								   "Flere {tolower(x_cat_2)} enn {tolower(x_cat_1)} er enig i påstanden {y_label} ({p_2} kontra {p_1} prosent, en forskjell på {p_2-p_1} prosentpoeng).",
								   "Færre {tolower(x_cat_1)} enn {tolower(x_cat_2)} er enig i påstanden {y_label} ({p_1} kontra {p_2} prosent, en forskjell på {p_2-p_1} prosentpoeng).")
	
	str_glue_likes_1_above_2 <- c("{x_cat_1} liker {y_label} mer enn {tolower(x_cat_2)} gjør ({p_1} kontra {p_2} prosent, en forskjell på {p_1-p_2} prosentpoeng).",
								  "{x_cat_2} liker {y_label} mindre enn {tolower(x_cat_1)} gjør ({p_2} kontra {p_1} prosent, en forskjell på {p_1-p_2} prosentpoeng).",
								  "Flere {tolower(x_cat_1)} enn {tolower(x_cat_2)} liker {y_label} ({p_1} kontra {p_2} prosent, en forskjell på {p_1-p_2} prosentpoeng).",
								  "Færre {tolower(x_cat_2)} enn {tolower(x_cat_1)} liker {y_label} ({p_2} kontra {p_1} prosent, en forskjell på {p_1-p_2} prosentpoeng).")
	str_glue_likes_2_above_1 <- c("{x_cat_2} liker {y_label} mer enn {tolower(x_cat_1)} gjør ({p_2} kontra {p_1} prosent, en forskjell på {p_2-p_1} prosentpoeng).",
								  "{x_cat_1} liker {y_label} mindre enn {tolower(x_cat_2)} gjør ({p_1} kontra {p_2} prosent, en forskjell på {p_2-p_1} prosentpoeng).",
								  "Flere {tolower(x_cat_2)} enn {tolower(x_cat_1)} liker {y_label} ({p_2} kontra {p_1} prosent, en forskjell på {p_2-p_1} prosentpoeng).",
								  "Færre {tolower(x_cat_1)} enn {tolower(x_cat_2)} liker {y_label} ({p_1} kontra {p_2} prosent, en forskjell på {p_2-p_1} prosentpoeng).")
	
}
