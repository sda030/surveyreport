x<-xml2::read_xml("C:/Users/py128/NIFU/20750 Evalueringen av Tett på realfag - General/Spørreundersøkelser/Analyser/Stephans realfag/tabeller/vg2_uni_17/word/charts/chart1.xml")
# xml2::as_list(x2) %>%
# 	xml2::as_xml_document() %>%
# 	xml2::write_xml("C:/Users/py128/NIFU/20750 Evalueringen av Tett på realfag - General/Spørreundersøkelser/Analyser/Stephans realfag/tabeller/vg2_uni_17/word/charts/chart1b.xml")
# 
# x3<-xml2::read_xml("C:/Users/py128/NIFU/20750 Evalueringen av Tett på realfag - General/Spørreundersøkelser/Analyser/Stephans realfag/tabeller/vg2_uni_17/word/charts/chart1b.xml")
# waldo::compare(x2, x3)

y <-
	x %>% 
	xml_find_all(xpath = "/c:chartSpace/c:chart/c:plotArea/c:barChart/c:ser/c:val/c:numRef/c:numCache/c:pt")
y %>% xml_attr(attr = "idx") %>% as.integer() %>% .[y %>% xml_find_all(xpath = "c:v") %>% xml_double() < 0.05]
dLbls_paths <- 
	y %>% xml_path() %>% .[y %>% xml_find_all(xpath = "c:v") %>% xml_double() < 0.05] %>%
	gsub("/c\\:val/c\\:numRef/c\\:numCache/c\\:pt\\[*.*\\]*", "/c:dLbls", .) %>% unique()
y2 <- 
	x %>% 
	xml_find_all(paste0(dLbls_paths, collapse="|")) %>% 
	xml_child() %>%
	xml_add_child(.value = "c:dLbl", .where = 1L) %>% 
	xml_add_child(.value = "c:idx")
xml_set_attr(x = y2, attr = "val", value = "99")
y2 <- y2 %>% xml_parent() %>% xml_add_child(.value = "c:delete")
xml_set_attr(x = y2, attr = "val", value = "1")
y2 <- y2 %>% xml_parent() %>% xml_add_child(.value = "c:extLst") %>% xml_add_child(.value = "c:ext")
xml_set_attr(x = y2, attr = "val", value = "1")

x$chartSpace$chart$plotArea$barChart[names(x$chartSpace$chart$plotArea$barChart) == "ser"] <-
	x$chartSpace$chart$plotArea$barChart[names(x$chartSpace$chart$plotArea$barChart) == "ser"] %>%
	plyr::llply(., function(ser) {
		idx <-
			plyr::llply(ser$val$numRef$numCache[names(ser$val$numRef$numCache) == "pt"], function(pt) {
				if(as.numeric(pt$v)< .05) attr(pt, "idx")
			})  %>% unname() %>% unlist()
		ser$dLbls <-
			c(ser$dLbls,
			  lapply(idx, function(id) {
			  	list(dLbl = list(idx = magrittr::set_attr(x = list(), which="val", value = id),
			  					 delete = magrittr::set_attr(x = list(), which="val", value = "1"),
			  					 extLst = list(ext = 
			  					 			  	list() %>% 
			  					 			  	magrittr::set_attr(x = ., which="uri", value = "{CE6537A1-D6FC-4f65-9D91-7224C49458BB}") %>%
			  					 			  	magrittr::set_attr(x = ., which="xmlns:c15", value = "http://schemas.microsoft.com/office/drawing/2012/chart"),
			  					 			  ext = 
			  					 			  	list(
			  					 			  		uniqueId = magrittr::set_attr(x = list(), which="val", value = "{00000007-3733-4D7B-A2F5-E18D1D2E3D43}")) %>%
			  					 			  	magrittr::set_attr(x = ., which="uri", value = "{C3380CC4-5D6E-409C-BE32-E72D297353CC}") %>%
			  					 			  	magrittr::set_attr(x = ., which="xmlns:c16", value = "http://schemas.microsoft.com/office/drawing/2014/chart"))))
			  }))
		ser
	})
xml2::as_xml_document(x) %>%
	xml2::write_xml(., file = "C:/Users/py128/NIFU/20750 Evalueringen av Tett på realfag - General/Spørreundersøkelser/Analyser/Stephans realfag/tabeller/vg2_uni_17/word/charts/chart1_2.xml", option="require_xhtml")

w<-
	lapply(y[names(y) == "ser"], function(ser) {
		z <- ser$dLbls
		lapply(z[names(z) == "dLbl"], function(dLbl) {
			dLbl
		})
	}) %>% .[[3]] %>% .[[1]]
names(w)
