#' ex_survey1: Mockup dataset of a survey.
#'
#' A dataset containing fake respondents' answers to survey questions.
#' the first two, x_sex and x_human, are intended to be independent variables,
#' whereas the remaning are dependent. The separator between items and groups
#' is the underscore _ (i.e. a_1-a_9 => a + 1-9) whereas the separator for
#' labels is the hyphen. The latter corresponds with the default in SurveyXact. 
#'
#' @format A data frame with 100 rows and 29 variables:
#' \describe{
#'   \item{x1_sex}{Gender}
#'   \item{x2_human}{Is respondent human?}
#'   \item{a_1}{Do you consent to the following? - Agreement #1}
#'   \item{a_2}{Do you consent to the following? - Agreement #2}
#'   \item{a_3}{Do you consent to the following? - Agreement #3}
#'   \item{a_4}{Do you consent to the following? - Agreement #4}
#'   \item{a_5}{Do you consent to the following? - Agreement #5}
#'   \item{a_6}{Do you consent to the following? - Agreement #6}
#'   \item{a_7}{Do you consent to the following? - Agreement #7}
#'   \item{a_8}{Do you consent to the following? - Agreement #8}
#'   \item{a_9}{Do you consent to the following? - Agreement #9}
#'   \item{b_1}{How much do you like living in - Bejing}
#'   \item{b_2}{How much do you like living in - Brussels}
#'   \item{b_3}{How much do you like living in - Budapest}
#'   \item{c_1}{How many years of experience do you have in - Company A}
#'   \item{c_2}{How many years of experience do you have in - Company B}
#'   \item{d_1}{Rate your degree of confidence doing the following - Driving}
#'   \item{d_2}{Rate your degree of confidence doing the following - Drinking}
#'   \item{d_3}{Rate your degree of confidence doing the following - Driving}
#'   \item{d_4}{Rate your degree of confidence doing the following - Dancing}
#'   \item{e_1}{How often do you do the following? - Eat}
#'   \item{e_2}{How often do you do the following? - Eavesdrop}
#'   \item{e_3}{How often do you do the following? - Exercise}
#'   \item{e_4}{How often do you do the following? - Encourage someone whom you have only recently met and who struggles with simple tasks that they cannot achieve by themselves}
#'   \item{p_1}{To what extent do you agree or disagree to the 
#'   following policies - Red Party}
#'   \item{p_2}{To what extent do you agree or disagree to the 
#'   following policies - Green Party}
#'   \item{p_3}{To what extent do you agree or disagree to the 
#'   following policies - Yellow Party}
#'   \item{p_4}{To what extent do you agree or disagree to the 
#'   following policies - Blue Party}
#'   \item{f_uni}{Which of the following universities would you 
#'   prefer to study at?}
#'   \item{resp_status}{Response status}
#' }
#' @seealso ex_survey2
"ex_survey1"

#' ex_survey2: Anonymized snippet of a real survey.
#'
#' A dataset containing real respondents' answers to survey questions. The
#' separator between items and groups is the underscore _ (i.e. a_1-a_9 => a +
#' 1-9) whereas the separator for labels is the hyphen. The latter corresponds
#' with the default in SurveyXact. Note that _ separator will not work perfectly
#' for distinguishing all variables and groups.
#'
#' @format A data frame with 100 rows and 29 variables: 
#' \describe{
#'   \item{v_1_1}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Design og redesign} 
#'   \item{v_1_2}{Hvilke(t) valgfag hadde du p??
#'   ungdomsskolen? (flere kryss er mulig) - Forskning i praksis}
#'   \item{v_1_3}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Fysisk aktivitet og helse} 
#'   \item{v_1_4}{Hvilke(t) valgfag hadde du
#'   p?? ungdomsskolen? (flere kryss er mulig) - Natur, milj?? og friluftsliv}
#'   \item{v_1_5}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Produksjon av varer og tjenester} 
#'   \item{v_1_6}{Hvilke(t) valgfag
#'   hadde du p?? ungdomsskolen? (flere kryss er mulig) - Teknologi i praksis}
#'   \item{v_1_7}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Demokrati i praksis} 
#'   \item{v_1_8}{Hvilke(t) valgfag hadde du p??
#'   ungdomsskolen? (flere kryss er mulig) - Innsats for andre}
#'   \item{v_1_9}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Internasjonalt samarbeid} 
#'   \item{v_1_10}{Hvilke(t) valgfag hadde du
#'   p?? ungdomsskolen? (flere kryss er mulig) - Levande kulturarv}
#'   \item{v_1_11}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Medier og informasjon} 
#'   \item{v_1_12}{Hvilke(t) valgfag hadde du p??
#'   ungdomsskolen? (flere kryss er mulig) - Reiseliv} 
#'   \item{v_1_13}{Hvilke(t)
#'   valgfag hadde du p?? ungdomsskolen? (flere kryss er mulig) - Sal og scene}
#'   \item{v_1_14}{Hvilke(t) valgfag hadde du p?? ungdomsskolen? (flere kryss er
#'   mulig) - Programmering} 
#'   \item{v_1_15}{Hvilke(t) valgfag hadde du p??
#'   ungdomsskolen? (flere kryss er mulig) - Trafikk} 
#'   \item{v_1_16}{Hvilke(t)
#'   valgfag hadde du p?? ungdomsskolen? (flere kryss er mulig) - Annet,
#'   hvilke(t):} 
#'   \item{s_2}{Hvilket studieforberedende utdanningsprogram g??r du
#'   p???} 
#'   \item{s_4_1}{Hvilket programomr??de (linje) g??r du p??? - Realfag (for
#'   eksempel et eller flere av fagene: biologi, kjemi, fysikk, informatikk,
#'   matematikk)} 
#'   \item{s_4_2}{Hvilket programomr??de (linje) g??r du p??? -
#'   Spr??kfag, samfunnsfag og ??konomi} 
#'   \item{s_4_3}{Hvilket programomr??de
#'   (linje) g??r du p??? - Annet*:} 
#'   \item{s_6}{N??r bestemte du deg for
#'   programomr??det du har valgt?} 
#'   \item{m_1}{Hvilket matematikkfag hadde du p??
#'   Vg1?} 
#'   \item{s_11_1}{Hvilke fag har du valgt/skal du velge? (flere kryss er
#'   mulig) - Matematikk (S, R)} 
#'   \item{s_11_2}{Hvilke fag har du valgt/skal du
#'   velge? (flere kryss er mulig) - Biologi; Kjemi; Fysikk; Geofag}
#'   \item{s_11_3}{Hvilke fag har du valgt/skal du velge? (flere kryss er mulig)
#'   - Informasjonsteknologi; Teknologi og forskningsl??re} 
#'   \item{s_11_4}{Hvilke
#'   fag har du valgt/skal du velge? (flere kryss er mulig) - Spr??kfag (Engelsk;
#'   Fremmedspr??k; Latin; Gresk)} 
#'   \item{s_11_5}{Hvilke fag har du valgt/skal du
#'   velge? (flere kryss er mulig) - Markedsf??ring og ledelse; Medie- og
#'   informasjonskunnskap} 
#'   \item{s_11_6}{Hvilke fag har du valgt/skal du velge?
#'   (flere kryss er mulig) - Samfunns??konomi; Rettsl??re; Psykologi}
#'   \item{s_11_7}{Hvilke fag har du valgt/skal du velge? (flere kryss er mulig)
#'   - ??konomistyring; ??konomi og ledelse; Entrepren??rskap} 
#'   \item{s_11_8}{Hvilke
#'   fag har du valgt/skal du velge? (flere kryss er mulig) - Sosialkunnskap;
#'   Samfunnsgeografi; Sosiologi og sosialantropologi; Politikk og
#'   menneskerettigheter} 
#'   \item{s_11_9}{Hvilke fag har du valgt/skal du velge?
#'   (flere kryss er mulig) - Historie og filosofi; Antikkens kultur; Samisk
#'   historie og kultur} 
#'   \item{s_11_10}{Hvilke fag har du valgt/skal du velge?
#'   (flere kryss er mulig) - Reiseliv og spr??kfag; Kommunikasjon og kultur}
#'   \item{p_01}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Matematikk R1}
#'   \item{p_02}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Matematikk R2}
#'   \item{p_03}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Matematikk S1}
#'   \item{p_04}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Matematikk S2}
#'   \item{p_05}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Matematikk X}
#'   \item{p_06}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Programmering og modellering
#'   X} \item{p_07}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Biologi 1} 
#'   \item{p_08}{Kryss
#'   av for de programfagene du har valgt/skal velge (Kryss kun av p?? de fagene
#'   som er aktuelle for deg) - Biologi 2} 
#'   \item{p_09}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Fysikk 1} 
#'   \item{p_10}{Kryss av for de programfagene du
#'   har valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Fysikk 2} 
#'   \item{p_11}{Kryss av for de programfagene du har valgt/skal velge
#'   (Kryss kun av p?? de fagene som er aktuelle for deg) - Geofag X}
#'   \item{p_12}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Geofag 1} 
#'   \item{p_13}{Kryss
#'   av for de programfagene du har valgt/skal velge (Kryss kun av p?? de fagene
#'   som er aktuelle for deg) - Geofag 2} 
#'   \item{p_14}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Informasjonsteknologi 1} 
#'   \item{p_15}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Informasjonsteknologi 2} 
#'   \item{p_16}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Kjemi 1} 
#'   \item{p_17}{Kryss av for de programfagene du
#'   har valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Kjemi 2} 
#'   \item{p_18}{Kryss av for de programfagene du har valgt/skal velge
#'   (Kryss kun av p?? de fagene som er aktuelle for deg) - Teknologi og
#'   forskningsl??re X} 
#'   \item{p_19}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Teknologi og forskningsl??re 1} 
#'   \item{p_20}{Kryss av for de programfagene du
#'   har valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Teknologi og forskningsl??re 2} 
#'   \item{p_21}{Kryss av for de programfagene du
#'   har valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Antikkens kultur} 
#'   \item{p_22}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Latin 1 eller gresk 1} 
#'   \item{p_23}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Latin 2 eller gresk 2} 
#'   \item{p_24}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Internasjonal engelsk} 
#'   \item{p_25}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Samfunnsfaglig engelsk} 
#'   \item{p_26}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Engelskspr??klig litteratur og kultur} 
#'   \item{p_27}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Engelsk 1} 
#'   \item{p_28}{Kryss av for de programfagene du
#'   har valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Engelsk 2} 
#'   \item{p_29}{Kryss av for de programfagene du har valgt/skal
#'   velge (Kryss kun av p?? de fagene som er aktuelle for deg) - Entrepren??rskap
#'   og bedriftsutvikling 1} 
#'   \item{p_30}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Entrepren??rskap og bedriftsutvikling 2} 
#'   \item{p_31}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Fremmedspr??k niv?? I} \item{p_32}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Fremmedspr??k niv?? II} \item{p_33}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Fremmedspr??k niv?? III} \item{p_34}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Historie og filosofi 1} \item{p_35}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Historie og filosofi 2} \item{p_36}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Kommunikasjon og kultur 1} \item{p_37}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Kommunikasjon og kultur 2} \item{p_38}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Kommunikasjon og kultur 3} \item{p_39}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Markedsf??ring og ledelse 1} \item{p_40}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Markedsf??ring og ledelse 2} \item{p_41}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Medie- og informasjonskunnskap 1} \item{p_42}{Kryss av
#'   for de programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som
#'   er aktuelle for deg) - Medie- og informasjonskunnskap 2} \item{p_43}{Kryss
#'   av for de programfagene du har valgt/skal velge (Kryss kun av p?? de fagene
#'   som er aktuelle for deg) - ??konomistyring} \item{p_44}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - ??konomi og ledelse} \item{p_45}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Sosialkunnskap} \item{p_46}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Samfunnsgeografi} \item{p_47}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Sosiologi og sosialantropologi} \item{p_48}{Kryss av
#'   for de programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som
#'   er aktuelle for deg) - Politikk og menneskerettigheter} \item{p_49}{Kryss
#'   av for de programfagene du har valgt/skal velge (Kryss kun av p?? de fagene
#'   som er aktuelle for deg) - Psykologi 1} \item{p_50}{Kryss av for de
#'   programfagene du har valgt/skal velge (Kryss kun av p?? de fagene som er
#'   aktuelle for deg) - Psykologi 2} \item{p_51}{Kryss av for de programfagene
#'   du har valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg)
#'   - Reiseliv og spr??k 1} \item{p_52}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Reiseliv og spr??k 2} \item{p_53}{Kryss av for de programfagene du har
#'   valgt/skal velge (Kryss kun av p?? de fagene som er aktuelle for deg) -
#'   Rettsl??re 1} \item{p_54}{Kryss av for de programfagene du har valgt/skal
#'   velge (Kryss kun av p?? de fagene som er aktuelle for deg) - Rettsl??re 2}
#'   \item{p_55}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Samfunns??konomi 1}
#'   \item{p_56}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Samfunns??konomi 2}
#'   \item{p_57}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Samisk historie og samfunn
#'   1} \item{p_58}{Kryss av for de programfagene du har valgt/skal velge (Kryss
#'   kun av p?? de fagene som er aktuelle for deg) - Samisk historie og samfunn
#'   2} \item{a_p}{Andre fag (du trenger ikke ?? ta med obligatoriske fellesfag i
#'   ditt programomr??de som bl.a. norsk, historie, kropps??ving):}
#'   \item{pb_1}{Hvilket av f??lgende fag liker du best? (velg ett fag)}
#'   \item{i_01}{I hvilken grad har du f??tt inspirasjon eller motivasjon til
#'   ditt valg av programomr??de/programfag fra f??lgende?  - L??rere}
#'   \item{i_02}{I hvilken grad har du f??tt inspirasjon eller motivasjon til
#'   ditt valg av programomr??de/programfag fra f??lgende?  - Venner og /eller
#'   kj??reste} \item{i_03}{I hvilken grad har du f??tt inspirasjon eller
#'   motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   Foreldre/foresatte} \item{i_04}{I hvilken grad har du f??tt inspirasjon
#'   eller motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   S??sken} \item{i_05}{I hvilken grad har du f??tt inspirasjon eller motivasjon
#'   til ditt valg av programomr??de/programfag fra f??lgende?  - Andre kjente}
#'   \item{i_06}{I hvilken grad har du f??tt inspirasjon eller motivasjon til
#'   ditt valg av programomr??de/programfag fra f??lgende?  - R??dgiver p?? skolen}
#'   \item{i_07}{I hvilken grad har du f??tt inspirasjon eller motivasjon til
#'   ditt valg av programomr??de/programfag fra f??lgende?  - Fag fra
#'   ungdomskolen/Vg1} \item{i_08}{I hvilken grad har du f??tt inspirasjon eller
#'   motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  - ????pen
#'   dag?? p?? videreg??ende} \item{i_09}{I hvilken grad har du f??tt inspirasjon
#'   eller motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   Avisoppslag og -artikler} \item{i_10}{I hvilken grad har du f??tt
#'   inspirasjon eller motivasjon til ditt valg av programomr??de/programfag fra
#'   f??lgende?  - Popul??rvitenskapelige b??ker og blader} \item{i_11}{I hvilken
#'   grad har du f??tt inspirasjon eller motivasjon til ditt valg av
#'   programomr??de/programfag fra f??lgende?  - Andre b??ker og blader}
#'   \item{i_12}{I hvilken grad har du f??tt inspirasjon eller motivasjon til
#'   ditt valg av programomr??de/programfag fra f??lgende?  - Reklameplakater og
#'   annonser} \item{i_13}{I hvilken grad har du f??tt inspirasjon eller
#'   motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   Internett} \item{i_14}{I hvilken grad har du f??tt inspirasjon eller
#'   motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   Dataspill} \item{i_15}{I hvilken grad har du f??tt inspirasjon eller
#'   motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   Museum/vitensenter} \item{i_16}{I hvilken grad har du f??tt inspirasjon
#'   eller motivasjon til ditt valg av programomr??de/programfag fra f??lgende?  -
#'   Popul??rvitenskapelige kanaler/programmer (Discovery Channel, Newton, Animal
#'   planet, Myth busters osv.)} \item{i_17}{I hvilken grad har du f??tt
#'   inspirasjon eller motivasjon til ditt valg av programomr??de/programfag fra
#'   f??lgende?  - Popul??rvitenskapelige kanaler p?? YouTube, Snapchat, TikTok,
#'   osv.} \item{i_18}{I hvilken grad har du f??tt inspirasjon eller motivasjon
#'   til ditt valg av programomr??de/programfag fra f??lgende?  - Filmer og
#'   TV-serier} \item{fp_01}{Hvor viktig var f??lgende faktorer for deg da du
#'   valgte programomr??de/valgfrie programfag? - At jeg skulle f?? bruke mine
#'   talenter og evner} \item{fp_02}{Hvor viktig var f??lgende faktorer for deg
#'   da du valgte programomr??de/valgfrie programfag? - At jeg skulle f?? utvikle
#'   meg selv} \item{fp_03}{Hvor viktig var f??lgende faktorer for deg da du
#'   valgte programomr??de/valgfrie programfag? - At jeg skulle f?? arbeide
#'   kreativt} \item{fp_04}{Hvor viktig var f??lgende faktorer for deg da du
#'   valgte programomr??de/valgfrie programfag? - At jeg skulle ha det g??y med
#'   fagene} \item{fp_05}{Hvor viktig var f??lgende faktorer for deg da du valgte
#'   programomr??de/valgfrie programfag? - At jeg skulle f?? utfordringer ?? bryne
#'   meg p??} \item{fp_06}{Hvor viktig var f??lgende faktorer for deg da du valgte
#'   programomr??de/valgfrie programfag? - At jeg skulle f?? l??re om noe jeg er
#'   interessert i} \item{fp_07}{Hvor viktig var f??lgende faktorer for deg da du
#'   valgte programomr??de/valgfrie programfag? - At jeg skulle f?? l??re om noe
#'   jeg synes er viktig og meningsfylt} \item{fp_08}{Hvor viktig var f??lgende
#'   faktorer for deg da du valgte programomr??de/valgfrie programfag? - At jeg
#'   skulle f?? l??re om noe som stemmer med mine holdninger og verdier}
#'   \item{fp_09}{Hvor viktig var f??lgende faktorer for deg da du valgte
#'   programomr??de/valgfrie programfag? - At jeg skulle f?? l??re om noe som er
#'   viktig for samfunnet} \item{fp_10}{Hvor viktig var f??lgende faktorer for
#'   deg da du valgte programomr??de/valgfrie programfag? - Karakterene jeg hadde
#'   fra f??r} \item{fp_11}{Hvor viktig var f??lgende faktorer for deg da du
#'   valgte programomr??de/valgfrie programfag? - Mulighetene for gode karakterer
#'   videre} \item{fp_12}{Hvor viktig var f??lgende faktorer for deg da du valgte
#'   programomr??de/valgfrie programfag? - Opptakskrav til videre utdanning}
#'   \item{fp_13}{Hvor viktig var f??lgende faktorer for deg da du valgte
#'   programomr??de/valgfrie programfag? - At jeg skulle samle flest mulig
#'   realfagspoeng} \item{fp_14}{Hvor viktig var f??lgende faktorer for deg da du
#'   valgte programomr??de/valgfrie programfag? - At jeg skulle holde mange
#'   muligheter for videre studier ??pne} \item{fp_15}{Hvor viktig var f??lgende
#'   faktorer for deg da du valgte programomr??de/valgfrie programfag? - Hva
#'   vennene mine valgte} \item{fp_16}{Hvor viktig var f??lgende faktorer for deg
#'   da du valgte programomr??de/valgfrie programfag? - At programomr??det ikke
#'   skulle kreve for mye arbeid} \item{fp_17}{Hvor viktig var f??lgende faktorer
#'   for deg da du valgte programomr??de/valgfrie programfag? - At programomr??det
#'   ikke skulle v??re for vanskelig} \item{fp_18}{Hvor viktig var f??lgende
#'   faktorer for deg da du valgte programomr??de/valgfrie programfag? - At jeg
#'   skulle f?? tid til ?? gj??re andre ting enn skolearbeid} \item{fp_19}{Hvor
#'   viktig var f??lgende faktorer for deg da du valgte programomr??de/valgfrie
#'   programfag? - At programomr??det gir meg mulighet til ?? jobbe med det jeg
#'   vil i fremtiden} \item{a_fp}{Andre forhold, kommenter:} \item{up_01}{Hvor
#'   enig er du i f??lgende utsagn om deg og fagene p?? programomr??det/programfag
#'   du har valgt?  - Jeg er sv??rt motivert for fagene p?? programomr??det}
#'   \item{up_02}{Hvor enig er du i f??lgende utsagn om deg og fagene p??
#'   programomr??det/programfag du har valgt?  - Jeg er fortsatt usikker p?? om
#'   jeg har valgt det rette programomr??det} \item{up_03}{Hvor enig er du i
#'   f??lgende utsagn om deg og fagene p?? programomr??det/programfag du har valgt?
#'   - Jeg kommer til ?? trives med programomr??det jeg har valgt}
#'   \item{up_04}{Hvor enig er du i f??lgende utsagn om deg og fagene p??
#'   programomr??det/programfag du har valgt?  - Fagene kommer til ?? handle om
#'   noe jeg synes er spennende} \item{up_05}{Hvor enig er du i f??lgende utsagn
#'   om deg og fagene p?? programomr??det/programfag du har valgt?  - Fagene vil
#'   ta opp temaer jeg mener er meningsfylte og viktig} \item{up_06}{Hvor enig
#'   er du i f??lgende utsagn om deg og fagene p?? programomr??det/programfag du
#'   har valgt?  - Fagene vil gi meg mulighet til ?? studere det jeg vil}
#'   \item{up_07}{Hvor enig er du i f??lgende utsagn om deg og fagene p??
#'   programomr??det/programfag du har valgt?  - Jeg kommer til ?? v??re stolt over
#'   ?? ha g??tt p?? programomr??det} \item{up_08}{Hvor enig er du i f??lgende utsagn
#'   om deg og fagene p?? programomr??det/programfag du har valgt?  - Det betyr
#'   mye for meg ?? gj??re det bra i fagene} \item{up_09}{Hvor enig er du i
#'   f??lgende utsagn om deg og fagene p?? programomr??det/programfag du har valgt?
#'   - Jeg er flinkere enn de fleste andre som g??r p?? programomr??det}
#'   \item{up_10}{Hvor enig er du i f??lgende utsagn om deg og fagene p??
#'   programomr??det/programfag du har valgt?  - Jeg er flinkere i disse fagene
#'   enn i fag p?? andre programomr??der} \item{up_11}{Hvor enig er du i f??lgende
#'   utsagn om deg og fagene p?? programomr??det/programfag du har valgt?  - Jeg
#'   l??rer lett i fagene p?? dette programomr??det} \item{up_12}{Hvor enig er du i
#'   f??lgende utsagn om deg og fagene p?? programomr??det/programfag du har valgt?
#'   - Jeg er bekymret for at jeg ikke er flink nok i fagene} \item{up_13}{Hvor
#'   enig er du i f??lgende utsagn om deg og fagene p?? programomr??det/programfag
#'   du har valgt?  - Dette programomr??det vil koste meg mer tid og arbeid enn
#'   om jeg hadde valgt et annet programomr??de} \item{up_14}{Hvor enig er du i
#'   f??lgende utsagn om deg og fagene p?? programomr??det/programfag du har valgt?
#'   - Jeg vil f?? mindre fritid enn om jeg hadde valgt et annet programomr??de}
#'   \item{hm_1}{Hva synes du generelt om matematikkfaget slik du kjenner det
#'   fra skolen? - Matematikk er vanskeligere enn de fleste andre fag}
#'   \item{hm_2}{Hva synes du generelt om matematikkfaget slik du kjenner det
#'   fra skolen? - Matematikk er mer interessant enn de fleste andre fag}
#'   \item{hm_3}{Hva synes du generelt om matematikkfaget slik du kjenner det
#'   fra skolen? - En god karakter i matematikk krever mer arbeid enn i andre
#'   fag} \item{hm_4}{Hva synes du generelt om matematikkfaget slik du kjenner
#'   det fra skolen? - Jeg liker matematikk bedre enn de fleste andre fag}
#'   \item{hm_5}{Hva synes du generelt om matematikkfaget slik du kjenner det
#'   fra skolen? - I forhold til mange andre, er jeg flink i matematikk}
#'   \item{hm_6}{Hva synes du generelt om matematikkfaget slik du kjenner det
#'   fra skolen? - Matematikk er viktig og meningsfylt for meg} \item{hm_7}{Hva
#'   synes du generelt om matematikkfaget slik du kjenner det fra skolen? -
#'   Matematikk er viktig for samfunnet} \item{hn_1}{Hva synes du generelt om
#'   naturfaget slik du kjenner det fra skolen? - Naturfag er vanskeligere enn
#'   de fleste andre fag} \item{hn_2}{Hva synes du generelt om naturfaget slik
#'   du kjenner det fra skolen? - Naturfag er mer interessant enn de fleste
#'   andre fag} \item{hn_3}{Hva synes du generelt om naturfaget slik du kjenner
#'   det fra skolen? - En god karakter i naturfag krever mer arbeid enn i andre
#'   fag} \item{hn_4}{Hva synes du generelt om naturfaget slik du kjenner det
#'   fra skolen? - Jeg liker naturfag bedre enn de fleste andre fag}
#'   \item{hn_5}{Hva synes du generelt om naturfaget slik du kjenner det fra
#'   skolen? - I forhold til mange andre, er jeg flink i naturfag}
#'   \item{hn_6}{Hva synes du generelt om naturfaget slik du kjenner det fra
#'   skolen? - Naturfag er viktig og meningsfylt for meg} \item{hn_7}{Hva synes
#'   du generelt om naturfaget slik du kjenner det fra skolen? - Naturfag er
#'   viktig for samfunnet} \item{b_1}{Er du jente eller gutt?} \item{b_2}{Hva er
#'   den h??yeste utdanningen moren din (eller annen foresatt) har fullf??rt?}
#'   \item{b_3}{Hva er den h??yeste utdanningen faren din (eller annen foresatt)
#'   har fullf??rt?} \item{b_4}{Er moren din (eller annen foresatt) i jobb?}
#'   \item{b_5}{Er faren din (eller annen foresatt) i jobb?} \item{b_6_1}{- Hvor
#'   ble du f??dt?} \item{b_6_2}{- Hvor ble moren din (eller annen foresatt)
#'   f??dt?} \item{b_6_3}{- Hvor ble faren din (eller annen foresatt) f??dt?}
#'   \item{b_7_1}{Hvilke karakterer fikk du i f??lgende fag p?? slutten av Vg1?  -
#'   Matematikk 1T} \item{b_7_2}{Hvilke karakterer fikk du i f??lgende fag p??
#'   slutten av Vg1?  - Matematikk 1P} \item{b_7_3}{Hvilke karakterer fikk du i
#'   f??lgende fag p?? slutten av Vg1?  - Norsk} \item{b_7_4}{Hvilke karakterer
#'   fikk du i f??lgende fag p?? slutten av Vg1?  - Naturfag} \item{b_8_1}{Mitt
#'   prestasjonsm??l for f??lgende fag er ?? f?? standpunktkarakterene: - Matematikk
#'   2P} \item{b_8_2}{Mitt prestasjonsm??l for f??lgende fag er ?? f??
#'   standpunktkarakterene: - Samfunnsfag} \item{b_8_3}{Mitt prestasjonsm??l for
#'   f??lgende fag er ?? f?? standpunktkarakterene: - Geografi} \item{bp_00}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Matematikk 2P} \item{bp_01}{Mitt prestasjonsm??l for f??lgende programfag er
#'   ?? f?? standpunktkarakterene: - Matematikk R1} \item{bp_02}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Matematikk R2} \item{bp_03}{Mitt prestasjonsm??l for f??lgende programfag er
#'   ?? f?? standpunktkarakterene: - Matematikk S1} \item{bp_04}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Matematikk S2} \item{bp_05}{Mitt prestasjonsm??l for f??lgende programfag er
#'   ?? f?? standpunktkarakterene: - Matematikk X} \item{bp_06}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Programmering og modellering X} \item{bp_07}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Biologi 1}
#'   \item{bp_08}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Biologi 2} \item{bp_09}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Fysikk 1}
#'   \item{bp_10}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Fysikk 2} \item{bp_11}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Geofag X}
#'   \item{bp_12}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Geofag 1} \item{bp_13}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Geofag 2}
#'   \item{bp_14}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Informasjonsteknologi 1} \item{bp_15}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Informasjonsteknologi 2} \item{bp_16}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Kjemi 1} \item{bp_17}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Kjemi 2} \item{bp_18}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Teknologi og forskningsl??re X} \item{bp_19}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Teknologi og forskningsl??re 1} \item{bp_20}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Teknologi og
#'   forskningsl??re 2} \item{bp_21}{Mitt prestasjonsm??l for f??lgende programfag
#'   er ?? f?? standpunktkarakterene: - Antikkens kultur} \item{bp_22}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Latin 1 eller gresk 1} \item{bp_23}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Latin 2 eller gresk 2}
#'   \item{bp_24}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Internasjonal engelsk} \item{bp_25}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Samfunnsfaglig engelsk} \item{bp_26}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Engelskspr??klig litteratur og
#'   kultur} \item{bp_27}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Engelsk 1} \item{bp_28}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Engelsk 2}
#'   \item{bp_29}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Entrepren??rskap og bedriftsutvikling 1}
#'   \item{bp_30}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Entrepren??rskap og bedriftsutvikling 2}
#'   \item{bp_31}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Fremmedspr??k niv?? I} \item{bp_32}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Fremmedspr??k niv?? II} \item{bp_33}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Fremmedspr??k niv?? III}
#'   \item{bp_34}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Historie og filosofi 1} \item{bp_35}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Historie og filosofi 2} \item{bp_36}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Kommunikasjon og kultur 1}
#'   \item{bp_37}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Kommunikasjon og kultur 2} \item{bp_38}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Kommunikasjon og kultur 3} \item{bp_39}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Markedsf??ring og ledelse 1}
#'   \item{bp_40}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Markedsf??ring og ledelse 2} \item{bp_41}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Medie- og informasjonskunnskap 1} \item{bp_42}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Medie- og
#'   informasjonskunnskap 2} \item{bp_43}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - ??konomistyring}
#'   \item{bp_44}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - ??konomi og ledelse} \item{bp_45}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Sosialkunnskap} \item{bp_46}{Mitt prestasjonsm??l for f??lgende programfag er
#'   ?? f?? standpunktkarakterene: - Samfunnsgeografi} \item{bp_47}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Sosiologi og sosialantropologi} \item{bp_48}{Mitt prestasjonsm??l for
#'   f??lgende programfag er ?? f?? standpunktkarakterene: - Politikk og
#'   menneskerettigheter} \item{bp_49}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Psykologi 1} \item{bp_50}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Psykologi 2} \item{bp_51}{Mitt prestasjonsm??l for f??lgende programfag er ??
#'   f?? standpunktkarakterene: - Reiseliv og spr??k 1} \item{bp_52}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Reiseliv og spr??k 2} \item{bp_53}{Mitt prestasjonsm??l for f??lgende
#'   programfag er ?? f?? standpunktkarakterene: - Rettsl??re 1} \item{bp_54}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Rettsl??re 2} \item{bp_55}{Mitt prestasjonsm??l for f??lgende programfag er ??
#'   f?? standpunktkarakterene: - Samfunns??konomi 1} \item{bp_56}{Mitt
#'   prestasjonsm??l for f??lgende programfag er ?? f?? standpunktkarakterene: -
#'   Samfunns??konomi 2} \item{bp_57}{Mitt prestasjonsm??l for f??lgende programfag
#'   er ?? f?? standpunktkarakterene: - Samisk historie og samfunn 1}
#'   \item{bp_58}{Mitt prestasjonsm??l for f??lgende programfag er ?? f??
#'   standpunktkarakterene: - Samisk historie og samfunn 2}
#'   \item{id}{Kontaktopplysninger - id} \item{orgnr}{Kontaktopplysninger -
#'   orgnr} \item{b_1_1}{Skole} \item{stato_1}{Samlet status - Ny}
#'   \item{stato_2}{Samlet status - Distribuert} \item{stato_3}{Samlet status -
#'   Noen svar} \item{stato_4}{Samlet status - Gjennomf??rt}
#'   \item{stato_5}{Samlet status - Frafalt} \item{s_1}{Hvilke(t) valgfag hadde
#'   du p?? ungdomsskolen? (flere kryss er mulig) - Annet, hvilke(t):}
#'   \item{s_3}{Hvilket studieforberedende utdanningsprogram g??r du p??? - Annet,
#'   spesifiser:} \item{s_5}{Hvilket programomr??de (linje) g??r du p??? - Annet*:}
#'   \item{s_7}{N??r bestemte du deg for programomr??det du har valgt? - Annet,
#'   beskriv:} }
#' @seealso ex_survey1
"ex_survey2"



#' stopwords 
#' 
#' stopwords
#' 
#' @format stopwords
#' \describe{list}
"stopwords"

