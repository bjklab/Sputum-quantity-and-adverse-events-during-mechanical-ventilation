#' ########################################
#' random slope and intercept models
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
#' SAME DAY VAE SUBGLOTTIC MODELS
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_sameday <- read_csv(file = "./models/binomial/d_binomial_sameday.csv")
d_binomial_sameday







#' ##########################################################################
#' ##########################################################################
#' 
#' ALL VAC with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: SAME DAY SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_sameday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 848 data points from 87 subjects


d_binomial_sameday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss_subject_only_vac_brms
#' 
#' 
#' m_vae_ss_subject_only_vac_brms %>% write_rds(path = "./models/binomial/m_vae_ss_subject_only_vac_brms.rds.gz", compress = "gz")
#' m_vae_ss_subject_only_vac_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss_subject_only_vac_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss_subject_only_vac_brms <- read_rds(path = "./models/binomial/m_vae_ss_subject_only_vac_brms.rds.gz")

m_vae_ss_subject_only_vac_brms$formula


pp_check(m_vae_ss_subject_only_vac_brms)
m_vae_ss_subject_only_vac_brms$fit -> m_vae_ss_subject_only_vac_stan
rstan::check_hmc_diagnostics(m_vae_ss_subject_only_vac_stan)

m_vae_ss_subject_only_vac_brms %>%
  tidybayes::get_variables()

m_vae_ss_subject_only_vac_brms %>%
  tidybayes::recover_types()


m_vae_ss_subject_only_vac_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss_subject_only_vac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss_subject_only_vac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss_subject_only_vac_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                      max(scale(d_mod_ss)[,1]),
                                                      length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                    max(scale(d_mod_ss)[,1]),
                                                    length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
         ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of VAC Same Day"#,
       #colour = "",
       #title = "Posterior VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_vac_sameday
p_vac_sameday




#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC RESTRICTED with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: SAME DAY SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_sameday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  mutate(ivac_yn = ifelse(ivac_yn, "IVAC", "VAC")) %>%
  filter(ivac_yn == "IVAC") %>% # filter to IVAC and PVAP only
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 416 data points from 40 subjects


d_binomial_sameday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss_subject_only_ivac_brms
#' 
#' 
#' m_vae_ss_subject_only_ivac_brms %>% write_rds(path = "./models/binomial/m_vae_ss_subject_only_ivac_brms.rds.gz", compress = "gz")
#' m_vae_ss_subject_only_ivac_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss_subject_only_ivac_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss_subject_only_ivac_brms <- read_rds(path = "./models/binomial/m_vae_ss_subject_only_ivac_brms.rds.gz")

m_vae_ss_subject_only_ivac_brms$formula


pp_check(m_vae_ss_subject_only_ivac_brms)
m_vae_ss_subject_only_ivac_brms$fit -> m_vae_ss_subject_only_ivac_stan
rstan::check_hmc_diagnostics(m_vae_ss_subject_only_ivac_stan)

m_vae_ss_subject_only_ivac_brms %>%
  tidybayes::get_variables()

m_vae_ss_subject_only_ivac_brms %>%
  tidybayes::recover_types()


m_vae_ss_subject_only_ivac_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss_subject_only_ivac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss_subject_only_ivac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss_subject_only_ivac_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of IVAC Same Day"#,
       #colour = "",
       #title = "Posterior IVAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_sameday
p_ivac_sameday








#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP RESTRICTED with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: SAME DAY SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_sameday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  filter(pvap_yn == TRUE) %>% # filter to PVAP only
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 89 data points from 12 subjects


d_binomial_sameday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss_subject_only_pvap_brms
#' 
#' 
#' m_vae_ss_subject_only_pvap_brms %>% write_rds(path = "./models/binomial/m_vae_ss_subject_only_pvap_brms.rds.gz", compress = "gz")
#' m_vae_ss_subject_only_pvap_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss_subject_only_pvap_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss_subject_only_pvap_brms <- read_rds(path = "./models/binomial/m_vae_ss_subject_only_pvap_brms.rds.gz")

m_vae_ss_subject_only_pvap_brms$formula


pp_check(m_vae_ss_subject_only_pvap_brms)
m_vae_ss_subject_only_pvap_brms$fit -> m_vae_ss_subject_only_pvap_stan
rstan::check_hmc_diagnostics(m_vae_ss_subject_only_pvap_stan)

m_vae_ss_subject_only_pvap_brms %>%
  tidybayes::get_variables()

m_vae_ss_subject_only_pvap_brms %>%
  tidybayes::recover_types()


m_vae_ss_subject_only_pvap_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss_subject_only_pvap_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss_subject_only_pvap_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss_subject_only_pvap_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of PVAP Same Day"#,
       #colour = "",
       #title = "Posterior PVAP Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_sameday
p_pvap_sameday








#' ##########################################################################
#' ##########################################################################
#' 
#' DAY BEFORE VAE SUBGLOTTIC MODELS
#' 
#' ##########################################################################
#' ##########################################################################


d_binomial_oneday <- read_csv(file = "./models/binomial/d_binomial_oneday.csv")
d_binomial_oneday







#' ##########################################################################
#' ##########################################################################
#' 
#' ALL VAC with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: DAY BEFORE SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_oneday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 761 data points from 69 subjects


d_binomial_oneday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss1before_subject_only_vac_brms
#' 
#' 
#' m_vae_ss1before_subject_only_vac_brms %>% write_rds(path = "./models/binomial/m_vae_ss1before_subject_only_vac_brms.rds.gz", compress = "gz")
#' m_vae_ss1before_subject_only_vac_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss1before_subject_only_vac_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss1before_subject_only_vac_brms <- read_rds(path = "./models/binomial/m_vae_ss1before_subject_only_vac_brms.rds.gz")

m_vae_ss1before_subject_only_vac_brms$formula


pp_check(m_vae_ss1before_subject_only_vac_brms)
m_vae_ss1before_subject_only_vac_brms$fit -> m_vae_ss1before_subject_only_vac_stan
rstan::check_hmc_diagnostics(m_vae_ss1before_subject_only_vac_stan)

m_vae_ss1before_subject_only_vac_brms %>%
  tidybayes::get_variables()

m_vae_ss1before_subject_only_vac_brms %>%
  tidybayes::recover_types()


m_vae_ss1before_subject_only_vac_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss1before_subject_only_vac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss1before_subject_only_vac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss1before_subject_only_vac_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of VAC One Day Later"#,
       #colour = "",
       #title = "Posterior VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_vac_day1before
p_vac_day1before




#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC RESTRICTED with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: DAY BEFORE SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_oneday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  mutate(ivac_yn = ifelse(ivac_yn, "IVAC", "VAC")) %>%
  filter(ivac_yn == "IVAC") %>% # filter to IVAC and PVAP only
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 376 data points from 34 subjects


d_binomial_oneday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss1before_subject_only_ivac_brms
#' 
#' 
#' m_vae_ss1before_subject_only_ivac_brms %>% write_rds(path = "./models/binomial/m_vae_ss1before_subject_only_ivac_brms.rds.gz", compress = "gz")
#' m_vae_ss1before_subject_only_ivac_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss1before_subject_only_ivac_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss1before_subject_only_ivac_brms <- read_rds(path = "./models/binomial/m_vae_ss1before_subject_only_ivac_brms.rds.gz")

m_vae_ss1before_subject_only_ivac_brms$formula


pp_check(m_vae_ss1before_subject_only_ivac_brms)
m_vae_ss1before_subject_only_ivac_brms$fit -> m_vae_ss1before_subject_only_ivac_stan
rstan::check_hmc_diagnostics(m_vae_ss1before_subject_only_ivac_stan)

m_vae_ss1before_subject_only_ivac_brms %>%
  tidybayes::get_variables()

m_vae_ss1before_subject_only_ivac_brms %>%
  tidybayes::recover_types()


m_vae_ss1before_subject_only_ivac_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss1before_subject_only_ivac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss1before_subject_only_ivac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss1before_subject_only_ivac_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of IVAC One Day Later"#,
       #colour = "",
       #title = "Posterior IVAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_day1before
p_ivac_day1before








#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP RESTRICTED with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: DAY BEFORE SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_oneday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  filter(pvap_yn == TRUE) %>% # filter to PVAP only
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 77 data points from 11 subjects


d_binomial_oneday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss1before_subject_only_pvap_brms
#' 
#' 
#' m_vae_ss1before_subject_only_pvap_brms %>% write_rds(path = "./models/binomial/m_vae_ss1before_subject_only_pvap_brms.rds.gz", compress = "gz")
#' m_vae_ss1before_subject_only_pvap_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss1before_subject_only_pvap_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss1before_subject_only_pvap_brms <- read_rds(path = "./models/binomial/m_vae_ss1before_subject_only_pvap_brms.rds.gz")

m_vae_ss1before_subject_only_pvap_brms$formula


pp_check(m_vae_ss1before_subject_only_pvap_brms)
m_vae_ss1before_subject_only_pvap_brms$fit -> m_vae_ss1before_subject_only_pvap_stan
rstan::check_hmc_diagnostics(m_vae_ss1before_subject_only_pvap_stan)

m_vae_ss1before_subject_only_pvap_brms %>%
  tidybayes::get_variables()

m_vae_ss1before_subject_only_pvap_brms %>%
  tidybayes::recover_types()


m_vae_ss1before_subject_only_pvap_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss1before_subject_only_pvap_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss1before_subject_only_pvap_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss1before_subject_only_pvap_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of PVAP One Day Later"#,
       #colour = "",
       #title = "Posterior PVAP Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_day1before
p_pvap_day1before












#' ##########################################################################
#' ##########################################################################
#' 
#' DAY BEFORE VAE SUBGLOTTIC MODELS
#' 
#' ##########################################################################
#' ##########################################################################


d_binomial_twoday <- read_csv(file = "./models/binomial/d_binomial_twoday.csv")
d_binomial_twoday







#' ##########################################################################
#' ##########################################################################
#' 
#' ALL VAC with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: TWO DAYS BEFORE SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_twoday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 692 data points from 59 subjects


d_binomial_twoday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss2before_subject_only_vac_brms
#' 
#' 
#' m_vae_ss2before_subject_only_vac_brms %>% write_rds(path = "./models/binomial/m_vae_ss2before_subject_only_vac_brms.rds.gz", compress = "gz")
#' m_vae_ss2before_subject_only_vac_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss2before_subject_only_vac_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss2before_subject_only_vac_brms <- read_rds(path = "./models/binomial/m_vae_ss2before_subject_only_vac_brms.rds.gz")

m_vae_ss2before_subject_only_vac_brms$formula


pp_check(m_vae_ss2before_subject_only_vac_brms)
m_vae_ss2before_subject_only_vac_brms$fit -> m_vae_ss2before_subject_only_vac_stan
rstan::check_hmc_diagnostics(m_vae_ss2before_subject_only_vac_stan)

m_vae_ss2before_subject_only_vac_brms %>%
  tidybayes::get_variables()

m_vae_ss2before_subject_only_vac_brms %>%
  tidybayes::recover_types()


m_vae_ss2before_subject_only_vac_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss2before_subject_only_vac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss2before_subject_only_vac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss2before_subject_only_vac_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of VAC Two Days Later"#,
       #colour = "",
       #title = "Posterior VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_vac_day2before
p_vac_day2before




#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC RESTRICTED with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: TWO DAYS BEFORE SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_twoday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  mutate(ivac_yn = ifelse(ivac_yn, "IVAC", "VAC")) %>%
  filter(ivac_yn == "IVAC") %>% # filter to IVAC and PVAP only
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 342 data points from 31 subjects


d_binomial_twoday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss2before_subject_only_ivac_brms
#' 
#' 
#' m_vae_ss2before_subject_only_ivac_brms %>% write_rds(path = "./models/binomial/m_vae_ss2before_subject_only_ivac_brms.rds.gz", compress = "gz")
#' m_vae_ss2before_subject_only_ivac_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss2before_subject_only_ivac_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss2before_subject_only_ivac_brms <- read_rds(path = "./models/binomial/m_vae_ss2before_subject_only_ivac_brms.rds.gz")

m_vae_ss2before_subject_only_ivac_brms$formula


pp_check(m_vae_ss2before_subject_only_ivac_brms)
m_vae_ss2before_subject_only_ivac_brms$fit -> m_vae_ss2before_subject_only_ivac_stan
rstan::check_hmc_diagnostics(m_vae_ss2before_subject_only_ivac_stan)

m_vae_ss2before_subject_only_ivac_brms %>%
  tidybayes::get_variables()

m_vae_ss2before_subject_only_ivac_brms %>%
  tidybayes::recover_types()


m_vae_ss2before_subject_only_ivac_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss2before_subject_only_ivac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss2before_subject_only_ivac_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss2before_subject_only_ivac_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of IVAC Two Days Later"#,
       #colour = "",
       #title = "Posterior IVAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_day2before
p_ivac_day2before








#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP RESTRICTED with SUBJECT-LEVEL RANDOM SLOPE & INTERCEPT MODEL: TWO DAYS BEFORE SUBGLOTTIC
#' 
#' ##########################################################################
#' ##########################################################################

d_binomial_twoday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  filter(pvap_yn == TRUE) %>% # filter to PVAP only
  # format data for Stan
  select(-d_rel_vae) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 66 data points from 9 subjects


d_binomial_twoday %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       sample_prior = "only", # prior predictive
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> d_prior
#' 
#' d_prior %>%
#'   pp_expect() %>%
#'   as_tibble() %>%
#'   gather(key = "observations", value = "pp_samples") %>%
#'   qplot(data = ., x = pp_samples, geom = "density")
#' 
#' 
#' 
#' 
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(vae_day_yn = as.numeric(vae_day_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       vae_day_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_vae_ss2before_subject_only_pvap_brms
#' 
#' 
#' m_vae_ss2before_subject_only_pvap_brms %>% write_rds(path = "./models/binomial/m_vae_ss2before_subject_only_pvap_brms.rds.gz", compress = "gz")
#' m_vae_ss2before_subject_only_pvap_brms$fit %>% write_rds(path = "./models/binomial/m_vae_ss2before_subject_only_pvap_brms_stanfit.rds.gz", compress = "gz")
m_vae_ss2before_subject_only_pvap_brms <- read_rds(path = "./models/binomial/m_vae_ss2before_subject_only_pvap_brms.rds.gz")

m_vae_ss2before_subject_only_pvap_brms$formula


pp_check(m_vae_ss2before_subject_only_pvap_brms)
m_vae_ss2before_subject_only_pvap_brms$fit -> m_vae_ss2before_subject_only_pvap_stan
rstan::check_hmc_diagnostics(m_vae_ss2before_subject_only_pvap_stan)

m_vae_ss2before_subject_only_pvap_brms %>%
  tidybayes::get_variables()

m_vae_ss2before_subject_only_pvap_brms %>%
  tidybayes::recover_types()


m_vae_ss2before_subject_only_pvap_stan %>%
  rstan::summary(probs = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)) %>%
  `[[`(1) %>%
  as_tibble(rownames = "param") %>%
  filter(grepl("b_|^r_",param)) %>%
  mutate_at(.vars = vars(-param,-n_eff,-Rhat), .funs = list(exp = ~ exp(.x))) %>%
  select(param, contains("exp")) %>%
  #mutate(subject_integer = readr::parse_number(param)) %>%
  #left_join(d_key, by = "subject_integer") %>%
  ggplot(data = .) +
  geom_segment(aes(x = `5%_exp`, xend = `95%_exp`, y = param, yend = param)) +
  geom_point(aes(x = mean_exp, y = param))



#' use draws from the posterior 

# m_vae_ss2before_subject_only_pvap_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh() +
#   facet_wrap(facets = ~ condition, ncol = 1)
# 
# 
# m_vae_ss2before_subject_only_pvap_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() +
#   facet_wrap(facets = ~ condition, ncol = 1)




#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' posterior predictive plot
m_vae_ss2before_subject_only_pvap_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = seq(min(scale(d_mod_ss)[,1]),
                                                  max(scale(d_mod_ss)[,1]),
                                                  length.out = 100),
                             subject_id = rep("Subject 0100",100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_ss", value = "pp") %>%
  mutate(ss_daily_total = structure(.Data = seq(min(scale(d_mod_ss)[,1]),
                                                max(scale(d_mod_ss)[,1]),
                                                length.out = 100),
                                    .Names = paste0("V",seq(100)))[sim_ss],
         ss_range = ss_daily_total * sd(d_mod_ss, na.rm = TRUE) + mean(d_mod_ss, na.rm = TRUE),
  ) %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ condition, nrow = 1) +
  scale_y_continuous(limits = c(0,1)) +
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
  labs(x = "Subglottic Suctioning Events",
       y = "Posterior Probability of PVAP Two Days Later"#,
       #colour = "",
       #title = "Posterior PVAP Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_day2before
p_pvap_day2before

















#' ##########################################################################
#' ##########################################################################
#' 
#' COMBINE PLOTS
#' 
#' ##########################################################################
#' ##########################################################################

library(patchwork)

(p_vac_sameday + labs(y = "VAC Probability Same Day", x = "") + p_vac_day1before + labs(y = "VAC Probability Next Day", x = "24-Hour Subglottic Suction Events") + p_vac_day2before + labs(y = "VAC Probability Two Days Later", x = "")) /
  (p_ivac_sameday + labs(y = "IVAC Probability Same Day", x = "") + p_ivac_day1before + labs(y = "IVAC Probability Next Day", x = "24-Hour Subglottic Suction Events") + p_ivac_day2before + labs(y = "IVAC Probability Two Days Later", x= "")) +
  (p_pvap_sameday + labs(y = "PVAP Probability Same Day", x = "") + p_pvap_day1before + labs(y = "PVAP Probability Next Day", x = "24-Hour Subglottic Suction Events") + p_pvap_day2before + labs(y = "PVAP Probability Two Days Later", x= "")) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          text = element_text(size = 10),
          ) -> p_binomial_combined

p_binomial_combined

# ggsave(plot = p_binomial_combined, filename = "./figs/main/p_binomial_combined.pdf", height = 8.5, width = 8.5, units = "in")
# ggsave(plot = p_binomial_combined, filename = "./figs/main/p_binomial_combined.svg", height = 8.5, width = 8.5, units = "in")
# ggsave(plot = p_binomial_combined, filename = "./figs/main/p_binomial_combined.png", height = 8.5, width = 8.5, units = "in", dpi = 600)


p_binomial_combined &
  plot_annotation(title = "Results of Binomial Models: VAE ~ Sputum Quantity", theme = theme(plot.title.position = "plot"))


p_binomial_combined &
  scale_color_manual(aesthetics = c("fill", "color"),
                     # ordering to match guide_legend specified below
                     breaks = c("median","0.5", "0.8", "0.95"),
                     values = c(colorspace::sequential_hcl(5, palette = "Blues 3")[1:4]),
                     labels = c("median","50%", "80%", "95%")
  ) &
  guides(fill = guide_legend(title = "Posterior Credible Interval",
                             override.aes = list(color = c(colorspace::sequential_hcl(5, palette = "Blues 3")[1],NA, NA, NA),
                                                 fill = c(NA,colorspace::sequential_hcl(5, palette = "Blues 3")[2:4]))),
         color = FALSE)
  

p_binomial_combined &
  scale_color_manual(aesthetics = c("fill", "color"),
                     # ordering to match guide_legend specified below
                     breaks = c("median","0.5", "0.8", "0.95"),
                     values = c(colorspace::sequential_hcl(5, palette = "Greens 3")[1:4]),
                     labels = c("median","50%", "80%", "95%")
  ) &
  guides(fill = guide_legend(title = "Posterior Credible Interval",
                             override.aes = list(color = c(colorspace::sequential_hcl(5, palette = "Greens 3")[1],NA, NA, NA),
                                                 fill = c(NA,colorspace::sequential_hcl(5, palette = "Greens 3")[2:4]))),
         color = FALSE)


p_binomial_combined &
  scale_color_manual(aesthetics = c("fill", "color"),
                     # ordering to match guide_legend specified below
                     breaks = c("median","0.5", "0.8", "0.95"),
                     values = c(colorspace::sequential_hcl(5, palette = "Reds 3")[1:4]),
                     labels = c("median","50%", "80%", "95%")
  ) &
  guides(fill = guide_legend(title = "Posterior Credible Interval",
                             override.aes = list(color = c(colorspace::sequential_hcl(5, palette = "Reds 3")[1],NA, NA, NA),
                                                 fill = c(NA,colorspace::sequential_hcl(5, palette = "Reds 3")[2:4]))),
         color = FALSE)


p_binomial_combined &
  scale_color_manual(aesthetics = c("fill", "color"),
                     # ordering to match guide_legend specified below
                     breaks = c("median","0.5", "0.8", "0.95"),
                     values = c(colorspace::sequential_hcl(5, palette = "Purples 3")[1:4]),
                     labels = c("median","50%", "80%", "95%")
  ) &
  guides(fill = guide_legend(title = "Posterior Credible Interval",
                             override.aes = list(color = c(colorspace::sequential_hcl(5, palette = "Purples 3")[1],NA, NA, NA),
                                                 fill = c(NA,colorspace::sequential_hcl(5, palette = "Purples 3")[2:4]))),
         color = FALSE)





