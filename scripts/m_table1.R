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
  mutate(ss1_7 = ifelse(ss1_7, "More than 7 Suction Episodes", "7 or Fewer Suction Episodes"),
         gender = ifelse(gender == "M", "Male", "Female"),
         race = stringr::str_to_title(string = race)) %>%
  rename(`Age (years)` = age,
         Gender = gender,
         Race = race,
         # comorbidities
         `Chronic Obstructive Pulmonary Disease (COPD)` = copd,
         Asthma = asthma,
         `Interstitial Lung Disease (ILD)` = ild,
         `Lymphoma or Leukemia` = lymphleuk,
         `Diabetes mellitus` = dm,
         `Congestive Heart Failure (CHF)` = chf,
         Cirrhosis = cirrhosis,
         # features of critical illness
         `Subglottic Suction Events at MV Onset` = ss1,
         `Duration of MV (days)` = vent_duration,
         `Ventilator Day of VAE` = vae_date_rel,
         IVAC = ivac_yn,
         PVAP = pvap_yn,
         `Fever Week Before MV Onset` = fever_7before_vent,
         `Respiratory Culture Order Week Before MV Onset` = respcx_7before_vent,
         `WBC at MV Onset (1e3 cells/mm3)` = admit_wbc,
         `Serum Cr at MV Onset (mg/dL)` = admit_cr,
         `AST at MV Onset (units/L)` = admit_ast,
         `ALT at MV Onset (units/L)` = admit_alt,
         `Maximum FiO2 at MV Onset (%)` = admit_fio2,
         `Maximum PEEP at MV Onset (cm H2O)` = admit_peep,
         `Norepinephrine at MV Onset` = norepi_after0_1,
         `Epinephrine at MV Onset` = epi_after0_1,
         `Vasopressin at MV Onset` = pit_after0_1,
         `Dopamine at MV Onset` = dopa_after0_1,
         `Vancomycin (IV) in Week Before MV Onset` = vanco_before0_7,
         `Metronidazole in Week Before MV Onset` = metro_before0_7,
         `Linezolid in Week Before MV Onset` = linez_before0_7,
         `Cefazolin in Week Before MV Onset` = cefaz_before0_7,
         `Daptomycin in Week Before MV Onset` = dapto_before0_7,
         `Piperacillin-tazobactam in Week Before MV Onset` = piptaz_before0_7,
         `Cefepime in Week Before MV Onset` = cefep_before0_7,
         `Meropenem in Week Before MV Onset` = mero_before0_7,
         ) %>%
  tbl_summary(
    data = .,
    #by = ivac_yn,
    #by = vae_late,
    by = ss1_7,
    type = list(`Maximum FiO2 at MV Onset (%)` ~ "continuous",
                `Maximum PEEP at MV Onset (cm H2O)` ~ "continuous")
  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = "Table 1: Characteristics of Enrolled Subjects",
                 #subtitle = "Exposure: Greater Than 7 Subglottic Suction Episodes in First Calendar Day",
                 ) -> t1_ss1

t1_ss1




# t1_ss1 %>%
#   gt::as_raw_html() %>%
#   write_lines(path = "./tabs/t1_ss1.html")










#
###
#####
###
#







