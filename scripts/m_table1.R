#' ########################################
#' make Table 1 to display baseline (day 0) characteristics
#' ########################################
#' 
#' depends: subject_summary
#' 
library(tidyverse)
library(gt)
library(gtsummary)


subject_summary <- read_csv(file = "./tabs/subject_summary.csv")



#' table 1 - subglottic suction at start of mechanical ventilation
subject_summary %>%
  select(age, # demographics
         gender,
         race,
         # comorbidities
         copd,
         asthma,
         ild,
         lymphleuk,
         dm,
         chf,
         cirrhosis,
         # features of critical illness
         ss1,
         ss1_7,
         vent_duration,
         vae_date_rel,
         ivac_yn,
         pvap_yn,
         fever_7before_vent,
         respcx_7before_vent,
         contains("admit"),
         norepi_after0_1,
         epi_after0_1,
         pit_after0_1,
         dopa_after0_1,
         # vanco_before0_7,
         # metro_before0_7,
         # linez_before0_7,
         # cefaz_before0_7,
         # dapto_before0_7,
         # piptaz_before0_7,
         # cefep_before0_7,
         # mero_before0_7,
         contains('_before0_7'),
         ) %>%
  tbl_summary(
    data = .,
    #by = ivac_yn,
    #by = vae_late,
    by = ss1_7,
    type = list("admit_fio2" ~ "continuous", "admit_peep" ~ "continuous")
  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = "Table 1: Sputum Production at Onset of Mechanical Ventilation",
                 subtitle = "Greater Than 7 Subglottic Suction Episodes in First Calendar Day") -> t1_ss1

t1_ss1




# t1_ss1 %>%
#   gt::as_raw_html() %>%
#   write_lines(path = "./tabs/t1_ss1.html")










#
###
#####
###
#







