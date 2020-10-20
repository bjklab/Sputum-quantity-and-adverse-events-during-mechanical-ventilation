#' ########################################
#' subglottic suction summary statistics
#' ########################################
#' 
#' 
#' 

library(tidyverse)
library(tidybayes)
library(brms)


lm_colors <- colorspace::sequential_hcl(5, palette = "Grays")
#' custom color
#' lm_colors <- colorspace::sequential_hcl(5, palette = "Blues 3")




#' ##########################################################################
#' ##########################################################################
#' 
#' READ DATA
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_sameday <- read_csv(file = "./models/binomial/d_binomial_sameday.csv")
d_binomial_sameday

d_binomial_sameday %>%
  group_by(subject_id) %>%
  mutate(vae_type = ifelse(sum(pvap_yn) > 0, "PVAP", ifelse(sum(ivac_yn) > 0, "IVAC", "VAC"))) %>%
  ungroup() %>%
  identity() -> d_vae
d_vae


#' ##########################################################################
#' ##########################################################################
#' 
#' SUMMARY TABLE
#' 
#' ##########################################################################
#' ##########################################################################

d_vae %>%
  select(subject_id, d_rel_vae, vae_type, ss_daily_total) %>%
  filter(d_rel_vae <= 0) %>%
  filter(!is.na(ss_daily_total)) %>%
  #mutate(vae_type = replace(vae_type, vae_type %in% c("POVAP","PRVAP","PVAP"), "PVAP")) %>%
  mutate(day_definition = case_when(d_rel_vae == 0 ~ "Day of VAE",
                                    d_rel_vae == -1 ~ "1 Day Before VAE",
                                    d_rel_vae == -2 ~ "2 Days Before VAE",
                                    d_rel_vae < -2 ~ "More Than 2 Days Before VAE")) %>%
  mutate(day_definition = factor(day_definition, levels = c("More Than 2 Days Before VAE", "2 Days Before VAE", "1 Day Before VAE", "Day of VAE"))) %>%
  mutate(vae_type = factor(vae_type, levels = c("VAC", "IVAC", "PVAP"))) %>%
  pivot_wider(names_from = day_definition, values_from = ss_daily_total) %>%
  select(-subject_id, -d_rel_vae) %>%
  ungroup() %>%
  gtsummary::tbl_summary(by = vae_type, missing = "no") %>%
  gtsummary::add_p() %>%
  gtsummary::add_overall() %>%
  gtsummary::modify_header(label ~ "**Time Relative to VAE**") -> stab_time
stab_time

# stab_time %>%
#   gtsummary::as_gt() %>%
#   gt::as_raw_html() %>%
#   write_lines(file = "./tabs/st1_time.html")





d_vae %>%
  select(subject_id, d_rel_vae, vae_type, ss_daily_total) %>%
  filter(d_rel_vae <= 0) %>%
  filter(!is.na(ss_daily_total)) %>%
  #mutate(vae_type = replace(vae_type, vae_type %in% c("POVAP","PRVAP","PVAP"), "PVAP")) %>%
  mutate(day_definition = case_when(d_rel_vae == 0 ~ "Day of VAE",
                                    d_rel_vae == -1 ~ "1 Day Before VAE",
                                    d_rel_vae == -2 ~ "2 Days Before VAE",
                                    d_rel_vae < -2 ~ "More Than 2 Days Before VAE")) %>%
  mutate(day_definition = factor(day_definition, levels = c("More Than 2 Days Before VAE", "2 Days Before VAE", "1 Day Before VAE", "Day of VAE"))) %>%
  mutate(vae_type = factor(vae_type, levels = c("VAC", "IVAC", "PVAP"))) %>%
  pivot_wider(names_from = day_definition, values_from = ss_daily_total) %>%
  select(-subject_id, -d_rel_vae) %>%
  ungroup() %>%
  gtsummary::tbl_summary(by = vae_type, missing = "no") %>%
  #gtsummary::add_p() %>%
  gtsummary::add_overall() %>%
  gtsummary::modify_header(label ~ "**Time Relative to VAE**") %>%
  gtsummary::modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**VAE Type**") %>%
  identity() -> stab_simple
stab_simple

# stab_simple %>%
#   gtsummary::as_gt() %>%
#   gt::as_raw_html() %>%
#   write_lines(file = "./tabs/st1_simple.html")





d_vae %>%
  select(subject_id, d_rel_vae, vae_type, ss_daily_total) %>%
  filter(d_rel_vae <= 0) %>%
  filter(!is.na(ss_daily_total)) %>%
  #mutate(vae_type = replace(vae_type, vae_type %in% c("POVAP","PRVAP","PVAP"), "PVAP")) %>%
  mutate(day_definition = case_when(d_rel_vae == 0 ~ "Day of VAE",
                                    d_rel_vae == -1 ~ "1 Day Before VAE",
                                    d_rel_vae == -2 ~ "2 Days Before VAE",
                                    d_rel_vae < -2 ~ "More Than 2 Days Before VAE")) %>%
  mutate(day_definition = factor(day_definition, levels = c("More Than 2 Days Before VAE", "2 Days Before VAE", "1 Day Before VAE", "Day of VAE"))) %>%
  mutate(vae_type = factor(vae_type, levels = c("VAC", "IVAC", "PVAP"))) %>%
  pivot_wider(names_from = vae_type, values_from = ss_daily_total) %>%
  select(-subject_id, -d_rel_vae) %>%
  ungroup() %>%
  gtsummary::tbl_summary(by = day_definition, missing = "no") %>%
  gtsummary::add_p() %>%
  gtsummary::add_overall() %>%
  gtsummary::modify_header(label ~ "**VAE Type**") -> stab_type
stab_type

# stab_type %>%
#   gtsummary::as_gt() %>%
#   gt::as_raw_html() %>%
#   write_lines(file = "./tabs/st1_type.html")



