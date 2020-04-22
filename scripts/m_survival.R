#' ########################################
#' survival models
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


#' #############################################
#' 60-DAY SURVIVAL ~ ADMIT SS in BRMS
#' #############################################

d_surv <- read.csv(file = "./models/survival/d_surv.csv")
d_surv



#' #' check priors
#' 
#' d_surv %>%
#'   # filter(d_rel_vent < 60) %>% # filter to first two weeks on vent
#'   select(subject_id, d_rel_vent, vae_day_yn, ivac_yn, vae_date, vent_date, age, ss_daily_total) %>%
#'   group_by(subject_id) %>%
#'   summarise(vae_date_rel = unique(as.numeric(vae_date - vent_date) + 1),
#'             vae_date_rel = ifelse(vae_date_rel <= 60, vae_date_rel, 60),
#'             ivac_yn = unique(ivac_yn),
#'             age = unique(age),
#'             admit_ss = ss_daily_total[d_rel_vent == 0],
#'             admit_dss = ss_daily_total[d_rel_vent == 1] - ss_daily_total[d_rel_vent == 0],
#'             censored = as.numeric(vae_date_rel >= 60)) %>% # filter to first two weeks on vent
#'   ungroup() %>%
#'   select(vae_date_rel, censored, admit_ss, ivac_yn) %>%
#'   filter(complete.cases(.)) %>%
#'   get_prior(vae_date_rel | cens(censored) ~ 1 + admit_ss,
#'             data = ., family = weibull())
#' 
#' 
#' 
#' #' check prior predictive distribution
#' 
#' d_surv %>%
#'   # filter(d_rel_vent < 60) %>% # filter to first two weeks on vent
#'   select(subject_id, d_rel_vent, vae_day_yn, ivac_yn, vae_date, vent_date, age, ss_daily_total) %>%
#'   group_by(subject_id) %>%
#'   summarise(vae_date_rel = unique(as.numeric(vae_date - vent_date) + 1),
#'             vae_date_rel = ifelse(vae_date_rel <= 60, vae_date_rel, 60),
#'             ivac_yn = unique(ivac_yn),
#'             age = unique(age),
#'             admit_ss = ss_daily_total[d_rel_vent == 1],
#'             admit_dss = ss_daily_total[d_rel_vent == 2] - ss_daily_total[d_rel_vent == 1],
#'             censored = as.numeric(vae_date_rel >= 60)) %>% # filter to first two weeks on vent
#'   ungroup() %>%
#'   select(vae_date_rel, censored, admit_ss, ivac_yn) %>%
#'   filter(complete.cases(.)) %>%
#'   brm(vae_date_rel | cens(censored) ~ 1 + admit_ss,
#'       data = ., family = weibull(),
#'       prior = c(prior(student_t(3,0,1), class = Intercept),
#'                 prior(student_t(3,0,1), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(1), class = cor)
#'       ),
#'       sample_prior = "only",
#'       chains = 4,
#'       cores = 4,
#'       control = list(adapt_delta = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' d_prior
#' 
#' d_prior %>%
#'   pp_expect(new_data = tibble(admit_dss = rep(seq(-5,10),2), ivac_yn = rep(c(TRUE,FALSE),each=16))) %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   filter(pp_samples < 60) %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' #' run model
#' 
#' d_surv %>%
#'   # filter(d_rel_vent < 60) %>% # filter to first two weeks on vent
#'   select(subject_id, d_rel_vent, vae_day_yn, ivac_yn, vae_date, vent_date, age, ss_daily_total) %>%
#'   group_by(subject_id) %>%
#'   summarise(vae_date_rel = unique(as.numeric(vae_date - vent_date) + 1),
#'             vae_date_rel = ifelse(vae_date_rel <= 60, vae_date_rel, 60),
#'             ivac_yn = unique(ivac_yn),
#'             age = unique(age),
#'             admit_ss = ss_daily_total[d_rel_vent == 0],
#'             admit_dss = ss_daily_total[d_rel_vent == 1] - ss_daily_total[d_rel_vent == 0],
#'             censored = as.numeric(vae_date_rel >= 60)) %>% # filter to first two weeks on vent
#'   ungroup() %>%
#'   select(vae_date_rel, censored, admit_ss, ivac_yn) %>%
#'   filter(complete.cases(.)) %>%
#'   brm(vae_date_rel | cens(censored) ~ 1 + admit_ss,
#'       data = ., family = weibull(),
#'       prior = c(prior(student_t(3,0,1), class = Intercept),
#'                 prior(student_t(3,0,1), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(1), class = cor)
#'       ),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list(adapt_delta = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vaesurv60_ss_brms
#' 
#' 
#' 
#' m_vaesurv60_ss_brms %>% write_rds(path = "./models/survival/m_vaesurv60_ss_brms.rds.gz", compress = "gz")
#' m_vaesurv60_ss_brms$fit %>% write_rds(path = "./models/survival/m_vaesurv60_ss_brms_stanfit.rds.gz", compress = "gz")

m_vaesurv60_ss_brms <- read_rds(path = "./models/survival/m_vaesurv60_ss_brms.rds.gz")

m_vaesurv60_ss_brms$formula

pp_check(m_vaesurv60_ss_brms, nsamples = 100)
m_vaesurv60_ss_brms$fit -> m_vaesurv60_ss_stan
rstan::check_hmc_diagnostics(m_vaesurv60_ss_stan)


#' posterior predictive plot
m_vaesurv60_ss_brms %>%
  pp_expect(object = .,
            newdata = tibble(admit_ss = seq(0,20)),
            allow_new_levels = TRUE,
            re_formula = ~(1|condition)) %>%
  as_tibble() %>%
  gather(key = "sim_ss", value = "survtime") %>%
  mutate(admit_ss = structure(.Data = seq(0,20),
                              .Names = paste0("V",seq(21)))[sim_ss],
  ) %>%
  group_by(admit_ss) %>%
  select(survtime) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = admit_ss, y = survtime, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  scale_color_manual(aesthetics = c("fill", "color"),
                     # ordering to match guide_legend specified below
                     breaks = c("median","0.5", "0.8", "0.95"),
                     values = c(lm_colors[1:4]),
                     labels = c("median","50%", "80%", "95%")
  ) + 
  guides(fill = guide_legend(title = "Posterior Credible Interval",
                             override.aes = list(color = c(lm_colors[1],NA, NA, NA),
                                                 fill = c(NA,lm_colors[2:4]))),
         color = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 11),
        plot.title = element_text(size = 11),
        strip.background = element_blank(),
        plot.title.position = "plot") +
  labs(x = "Sputum Production at Mechanical Ventilation Onset\n(Subglottic Suction Events First Full Day on Ventilator)",
       y = "Survival Without VAE (days)"#,
       #colour = "",
       #title = "VAE Survival ~ Admission Subglottic Suctioning"#,
  ) -> p_surv
p_surv


# ggsave(plot = p_surv, filename = "./figs/supplement/weibull_survival_vae_vs_admit_ss.pdf", height = 4, width = 5, units = "in")
# ggsave(plot = p_surv, filename = "./figs/supplement/weibull_survival_vae_vs_admit_ss.svg", height = 4, width = 5, units = "in")
# ggsave(plot = p_surv, filename = "./figs/supplement/weibull_survival_vae_vs_admit_ss.png", height = 4, width = 5, units = "in", dpi = 600)







