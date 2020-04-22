#' ########################################
#' IVAC versus VAC models
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
#' IVAC versus VAC SUBGLOTTIC MODELS
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap <- read_csv(file = "./models/binomial/d_vac_ivac_pvap.csv")
d_vac_ivac_pvap

  
  
  




#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC: ALL DAYS RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id / d_rel_vae),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id / d_rel_vae),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_ss_subject_day_brms
#' 
#' 
#' m_ivac_ss_subject_day_brms %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_day_brms.rds.gz", compress = "gz")
#' m_ivac_ss_subject_day_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_day_brms_stanfit.rds.gz", compress = "gz")
m_ivac_ss_subject_day_brms <- read_rds(path = "./models/binomial/m_ivac_ss_subject_day_brms.rds.gz")

m_ivac_ss_subject_day_brms$formula


pp_check(m_ivac_ss_subject_day_brms)
m_ivac_ss_subject_day_brms$fit -> m_ivac_ss_subject_day_stan
rstan::check_hmc_diagnostics(m_ivac_ss_subject_day_stan)

m_ivac_ss_subject_day_brms %>%
  tidybayes::get_variables()

m_ivac_ss_subject_day_brms %>%
  tidybayes::recover_types()


m_ivac_ss_subject_day_stan %>%
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

# m_ivac_ss_subject_day_brms %>%
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
# m_ivac_ss_subject_day_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
#   #facet_wrap(facets = ~ condition, ncol = 1)
# 



#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_ivac_ss_subject_day_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                   subject_id = rep("Subject 0100",300),
                   d_rel_vae = rep(c(-2,-1,0), each = 100),
                   sim_var = paste0("V",seq(300)),
                   ss_range = rep(seq(0, 21, length.out = 100), 3)),
            by = "sim_var") %>%
  group_by(ss_range, d_rel_vae) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac
p_ivac_vs_vac












#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP: ALL DAYS RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id / d_rel_vae),
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
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id / d_rel_vae),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 2000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_pvap_ss_subject_day_brms
#' 
#' 
#' m_pvap_ss_subject_day_brms %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_day_brms.rds.gz", compress = "gz")
#' m_pvap_ss_subject_day_brms$fit %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_day_brms_stanfit.rds.gz", compress = "gz")
m_pvap_ss_subject_day_brms <- read_rds(path = "./models/binomial/m_pvap_ss_subject_day_brms.rds.gz")

m_pvap_ss_subject_day_brms$formula


pp_check(m_pvap_ss_subject_day_brms)
m_pvap_ss_subject_day_brms$fit -> m_pvap_ss_subject_day_stan
rstan::check_hmc_diagnostics(m_pvap_ss_subject_day_stan)

m_pvap_ss_subject_day_brms %>%
  tidybayes::get_variables()

m_pvap_ss_subject_day_brms %>%
  tidybayes::recover_types()


m_pvap_ss_subject_day_stan %>%
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

# m_pvap_ss_subject_day_brms %>%
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
# m_pvap_ss_subject_day_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
#   #facet_wrap(facets = ~ condition, ncol = 1)
# 



#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_pvap_ss_subject_day_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                   subject_id = rep("Subject 0100",300),
                   d_rel_vae = rep(c(-2,-1,0), each = 100),
                   sim_var = paste0("V",seq(300)),
                   ss_range = rep(seq(0, 21, length.out = 100), 3)),
            by = "sim_var") %>%
  group_by(ss_range, d_rel_vae) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of PVAP versus VAC/IVAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_vs_vac
p_pvap_vs_vac














#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC: SAME DAY RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(d_rel_vae == 0) %>% # same day only
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_ss_subject_brms
#' 
#' 
#' m_ivac_ss_subject_brms %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_brms.rds.gz", compress = "gz")
#' m_ivac_ss_subject_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_brms_stanfit.rds.gz", compress = "gz")
m_ivac_ss_subject_brms <- read_rds(path = "./models/binomial/m_ivac_ss_subject_brms.rds.gz")

m_ivac_ss_subject_brms$formula


pp_check(m_ivac_ss_subject_brms)
m_ivac_ss_subject_brms$fit -> m_ivac_ss_subject_stan
rstan::check_hmc_diagnostics(m_ivac_ss_subject_stan)

m_ivac_ss_subject_brms %>%
  tidybayes::get_variables()

m_ivac_ss_subject_brms %>%
  tidybayes::recover_types()


m_ivac_ss_subject_stan %>%
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

# m_ivac_ss_subject_brms %>%
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
# m_ivac_ss_subject_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
# #facet_wrap(facets = ~ condition, ncol = 1)
# 



#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_ivac_ss_subject_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = scale(seq(0, 21, length.out = 100))[,1],
                   subject_id = rep("Subject 0100",100),
                   sim_var = paste0("V",seq(100)),
                   ss_range = seq(0, 21, length.out = 100)),
            by = "sim_var") %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac_sameday
p_ivac_vs_vac_sameday








#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP: SAME DAY RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(d_rel_vae == 0) %>% # same day only
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
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
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_pvap_ss_subject_brms
#' 
#' 
#' m_pvap_ss_subject_brms %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_brms.rds.gz", compress = "gz")
#' m_pvap_ss_subject_brms$fit %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_brms_stanfit.rds.gz", compress = "gz")
m_pvap_ss_subject_brms <- read_rds(path = "./models/binomial/m_pvap_ss_subject_brms.rds.gz")

m_pvap_ss_subject_brms$formula


pp_check(m_pvap_ss_subject_brms)
m_pvap_ss_subject_brms$fit -> m_pvap_ss_subject_stan
rstan::check_hmc_diagnostics(m_pvap_ss_subject_stan)

m_pvap_ss_subject_brms %>%
  tidybayes::get_variables()

m_pvap_ss_subject_brms %>%
  tidybayes::recover_types()


m_pvap_ss_subject_stan %>%
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

# m_pvap_ss_subject_brms %>%
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
# m_pvap_ss_subject_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
# #facet_wrap(facets = ~ condition, ncol = 1)
# 



#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_pvap_ss_subject_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = scale(seq(0, 21, length.out = 100))[,1],
                   subject_id = rep("Subject 0100",100),
                   sim_var = paste0("V",seq(100)),
                   ss_range = seq(0, 21, length.out = 100)),
            by = "sim_var") %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of PVAP versus VAC/IVAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_vs_vac_sameday
p_pvap_vs_vac_sameday












#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC: NEXT DAY RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(d_rel_vae == -1) %>% # day before only
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_ss_subject_next_brms
#' 
#' 
#' m_ivac_ss_subject_next_brms %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_next_brms.rds.gz", compress = "gz")
#' m_ivac_ss_subject_next_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_next_brms_stanfit.rds.gz", compress = "gz")
m_ivac_ss_subject_next_brms <- read_rds(path = "./models/binomial/m_ivac_ss_subject_next_brms.rds.gz")

m_ivac_ss_subject_next_brms$formula


pp_check(m_ivac_ss_subject_next_brms)
m_ivac_ss_subject_next_brms$fit -> m_ivac_ss_subject_next_stan
rstan::check_hmc_diagnostics(m_ivac_ss_subject_next_stan)

m_ivac_ss_subject_next_brms %>%
  tidybayes::get_variables()

m_ivac_ss_subject_next_brms %>%
  tidybayes::recover_types()


m_ivac_ss_subject_next_stan %>%
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

# m_ivac_ss_subject_next_brms %>%
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
# m_ivac_ss_subject_next_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
# #facet_wrap(facets = ~ condition, ncol = 1)
# 
# 


#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_ivac_ss_subject_next_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = scale(seq(0, 21, length.out = 100))[,1],
                   subject_id = rep("Subject 0100",100),
                   sim_var = paste0("V",seq(100)),
                   ss_range = seq(0, 21, length.out = 100)),
            by = "sim_var") %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac_nextday
p_ivac_vs_vac_nextday














#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP: NEXT DAY RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(d_rel_vae == -1) %>% # day before only
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
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
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_pvap_ss_subject_next_brms
#' 
#' 
#' m_pvap_ss_subject_next_brms %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_next_brms.rds.gz", compress = "gz")
#' m_pvap_ss_subject_next_brms$fit %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_next_brms_stanfit.rds.gz", compress = "gz")
m_pvap_ss_subject_next_brms <- read_rds(path = "./models/binomial/m_pvap_ss_subject_next_brms.rds.gz")

m_pvap_ss_subject_next_brms$formula


pp_check(m_pvap_ss_subject_next_brms)
m_pvap_ss_subject_next_brms$fit -> m_pvap_ss_subject_next_stan
rstan::check_hmc_diagnostics(m_pvap_ss_subject_next_stan)

m_pvap_ss_subject_next_brms %>%
  tidybayes::get_variables()

m_pvap_ss_subject_next_brms %>%
  tidybayes::recover_types()


m_pvap_ss_subject_next_stan %>%
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

# m_pvap_ss_subject_next_brms %>%
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
# m_pvap_ss_subject_next_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
# #facet_wrap(facets = ~ condition, ncol = 1)
# 
# 


#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_pvap_ss_subject_next_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = scale(seq(0, 21, length.out = 100))[,1],
                   subject_id = rep("Subject 0100",100),
                   sim_var = paste0("V",seq(100)),
                   ss_range = seq(0, 21, length.out = 100)),
            by = "sim_var") %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of PVAP versus VAC/IVAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_vs_vac_nextday
p_pvap_vs_vac_nextday









#' ##########################################################################
#' ##########################################################################
#' 
#' IVAC: TWO DAYS LATER RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(d_rel_vae == -2) %>% # day before only
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_ss_subject_nextnext_brms
#' 
#' 
#' m_ivac_ss_subject_nextnext_brms %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_nextnext_brms.rds.gz", compress = "gz")
#' m_ivac_ss_subject_nextnext_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_ss_subject_nextnext_brms_stanfit.rds.gz", compress = "gz")
m_ivac_ss_subject_nextnext_brms <- read_rds(path = "./models/binomial/m_ivac_ss_subject_nextnext_brms.rds.gz")

m_ivac_ss_subject_nextnext_brms$formula


pp_check(m_ivac_ss_subject_nextnext_brms)
m_ivac_ss_subject_nextnext_brms$fit -> m_ivac_ss_subject_nextnext_stan
rstan::check_hmc_diagnostics(m_ivac_ss_subject_nextnext_stan)

m_ivac_ss_subject_nextnext_brms %>%
  tidybayes::get_variables()

m_ivac_ss_subject_nextnext_brms %>%
  tidybayes::recover_types()


m_ivac_ss_subject_nextnext_stan %>%
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

# m_ivac_ss_subject_nextnext_brms %>%
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
# m_ivac_ss_subject_nextnext_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
# #facet_wrap(facets = ~ condition, ncol = 1)
# 



#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_ivac_ss_subject_nextnext_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = scale(seq(0, 21, length.out = 100))[,1],
                   subject_id = rep("Subject 0100",100),
                   sim_var = paste0("V",seq(100)),
                   ss_range = seq(0, 21, length.out = 100)),
            by = "sim_var") %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac_nextnextday
p_ivac_vs_vac_nextnextday











#' ##########################################################################
#' ##########################################################################
#' 
#' PVAP: TWO DAYS LATER RANDOM SLOPE & INTERCEPT
#' 
#' ##########################################################################
#' ##########################################################################

d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -pvap_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn) %>%
  filter(d_rel_vae == -2) %>% # day before only
  filter(complete.cases(.)) -> d_mod
d_mod


d_vac_ivac_pvap %>%
  select(subject_id, d_rel_vae, vae_day_yn, pvap_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss


#' #' prior predictive plot
#' d_mod %>%
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
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
#'   mutate(pvap_yn = as.numeric(pvap_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       pvap_yn ~ 1 + ss_daily_total + (1 + ss_daily_total | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_pvap_ss_subject_nextnext_brms
#' 
#' 
#' m_pvap_ss_subject_nextnext_brms %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_nextnext_brms.rds.gz", compress = "gz")
#' m_pvap_ss_subject_nextnext_brms$fit %>% write_rds(path = "./models/binomial/m_pvap_ss_subject_nextnext_brms_stanfit.rds.gz", compress = "gz")
m_pvap_ss_subject_nextnext_brms <- read_rds(path = "./models/binomial/m_pvap_ss_subject_nextnext_brms.rds.gz")

m_pvap_ss_subject_nextnext_brms$formula


pp_check(m_pvap_ss_subject_nextnext_brms)
m_pvap_ss_subject_nextnext_brms$fit -> m_pvap_ss_subject_nextnext_stan
rstan::check_hmc_diagnostics(m_pvap_ss_subject_nextnext_stan)

m_pvap_ss_subject_nextnext_brms %>%
  tidybayes::get_variables()

m_pvap_ss_subject_nextnext_brms %>%
  tidybayes::recover_types()


m_pvap_ss_subject_nextnext_stan %>%
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

# m_pvap_ss_subject_nextnext_brms %>%
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
# m_pvap_ss_subject_nextnext_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, r_subject_id[condition, term]) %>%
#   spread(key = term, value = r_subject_id) %>%
#   #group_by(condition, .chain, .iteration, .draw) %>% mutate(pp = rethinking::inv_logit(b_Intercept))
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw, -condition) %>%
#   filter(param == "b_ss_daily_total" | param == "b_Intercept") %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh() #+
# #facet_wrap(facets = ~ condition, ncol = 1)
# 



#' use draws from the posterior to put on outcome scale
d_mod_ss %>% summary() 


#' Posterior Certainty plot
m_pvap_ss_subject_nextnext_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 3),
                             subject_id = rep("Subject 0100",300),
                             d_rel_vae = rep(c(-2,-1,0), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = scale(seq(0, 21, length.out = 100))[,1],
                   subject_id = rep("Subject 0100",100),
                   sim_var = paste0("V",seq(100)),
                   ss_range = seq(0, 21, length.out = 100)),
            by = "sim_var") %>%
  group_by(ss_range) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_range, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  #facet_wrap(facets = ~ d_rel_vae, nrow = 1) +
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
       y = "Probability of PVAP versus VAC/IVAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_pvap_vs_vac_nextnextday
p_pvap_vs_vac_nextnextday










#' ##########################################################################
#' ##########################################################################
#' 
#' COMBINE DISCRIMINATION PLOTS
#' 
#' ##########################################################################
#' ##########################################################################


library(patchwork)

((p_ivac_vs_vac_sameday +
    geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
    annotate(geom = "text", x = 10, y = 0.9, label = "Favors IVAC", color = "black", size = 3) +
    annotate(geom = "text", x = 10, y = 0.1, label = "Favors VAC", color = "black", size = 3) +
    labs(x = "24-Hour Subglottic Suction Events", y = "Probability IVAC versus VAC\nSame Day")) +
    (p_ivac_vs_vac_nextday +
       geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
       annotate(geom = "text", x = 10, y = 0.9, label = "Favors IVAC", color = "black", size = 3) +
       annotate(geom = "text", x = 10, y = 0.1, label = "Favors VAC", color = "black", size = 3) +
       labs(x = "24-Hour Subglottic Suction Events", y = "Next Day")) +
    (p_ivac_vs_vac_nextnextday +
       geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
       annotate(geom = "text", x = 10, y = 0.9, label = "Favors IVAC", color = "black", size = 3) +
       annotate(geom = "text", x = 10, y = 0.1, label = "Favors VAC", color = "black", size = 3) +
       labs(x = "24-Hour Subglottic Suction Events", y = "Two Days Later"))) /
  ((p_pvap_vs_vac_sameday +
      geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
      annotate(geom = "text", x = 10, y = 0.9, label = "Favors PVAP", color = "black", size = 3) +
      annotate(geom = "text", x = 10, y = 0.1, label = "Favors VAC/IVAC", color = "black", size = 3) +
      labs(x = "24-Hour Subglottic Suction Events", y = "Probability PVAP versus VAC/IVAC\nSame Day")) +
     (p_pvap_vs_vac_nextday +
        geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
        annotate(geom = "text", x = 10, y = 0.9, label = "Favors PVAP", color = "black", size = 3) +
        annotate(geom = "text", x = 10, y = 0.1, label = "Favors VAC/IVAC", color = "black", size = 3) +
        labs(x = "24-Hour Subglottic Suction Events", y = "Next Day")) +
     (p_pvap_vs_vac_nextnextday +
        geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
        annotate(geom = "text", x = 10, y = 0.9, label = "Favors PVAP", color = "black", size = 3) +
        annotate(geom = "text", x = 10, y = 0.1, label = "Favors VAC/IVAC", color = "black", size = 3) +
        labs(x = "24-Hour Subglottic Suction Events", y = "Two Days Later"))) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 10),
        ) -> p_ivac_yn_combined

p_ivac_yn_combined


# ggsave(plot = p_ivac_yn_combined, filename = "./figs/main/p_ivac_yn_combined.pdf", height = 6.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_yn_combined, filename = "./figs/main/p_ivac_yn_combined.svg", height = 6.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_yn_combined, filename = "./figs/main/p_ivac_yn_combined.png", height = 6.5, width = 8.5, units = "in", dpi = 600)










#' ##########################################################################
#' ##########################################################################
#' 
#' EXPLORATORY MULTIVARIABLE MODELS
#' 
#' ##########################################################################
#' ##########################################################################


d_explore <- read_csv(file = "./models/binomial/d_explore.csv")
d_explore






# sum model
d_explore %>%
  mutate(ast_alt = ast+alt) %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total, pressor_number, abx_b_number, ast_alt, creat, bnp) %>%
  # replace NA where NA = 0
  mutate_at(.vars = vars(pressor_number, abx_b_number), .funs = ~ replace(.x, is.na(.x), 0)) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn, -bnp) %>%
  filter(d_rel_vae == 0) %>% # same only
  filter(complete.cases(.)) -> d_mod
d_mod


d_explore %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss

#' 
#' #' which priors
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   get_prior(data = ., family = bernoulli,
#'             ivac_yn ~ ss_daily_total + pressor_number + abx_b_number + temp_f_max + wbc + ast_alt + creat + 1,
#'             seed = 16)
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ ss_daily_total + pressor_number + abx_b_number + ast_alt + creat + 1,
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(2), class = cor),
#'                 ),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ ss_daily_total + pressor_number + abx_b_number + ast_alt + creat + 1,
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b)#,
#'                 #prior(horseshoe(), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(2), class = cor),
#'                 ),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_yn_explore_brms
#' 
#' 
#' m_ivac_yn_explore_brms %>% write_rds(path = "./models/binomial/m_ivac_yn_explore_brms.rds.gz", compress = "gz")
#' m_ivac_yn_explore_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_yn_explore_brms_stanfit.rds.gz", compress = "gz")
m_ivac_yn_explore_brms <- read_rds(path = "./models/binomial/m_ivac_yn_explore_brms.rds.gz")

m_ivac_yn_explore_brms$formula


pp_check(m_ivac_yn_explore_brms)
m_ivac_yn_explore_brms$fit -> m_ivac_yn_explore_stan
rstan::check_hmc_diagnostics(m_ivac_yn_explore_stan)

m_ivac_yn_explore_brms %>%
  tidybayes::get_variables()

m_ivac_yn_explore_brms %>%
  tidybayes::recover_types()


m_ivac_yn_explore_stan %>%
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

# m_ivac_yn_explore_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, b_pressor_number, b_abx_b_number, b_ast_alt, b_creat) %>%
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw) %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh()
# 
# 
# m_ivac_yn_explore_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, b_pressor_number, b_abx_b_number, b_ast_alt, b_creat) %>%
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw) %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh()
# 


#' Posterior Certainty plot
m_ivac_yn_explore_brms %>%
  pp_expect(object = .,
            newdata = expand_grid(ast_alt = scale(seq(0,3000,50))[,1],
                                   pressor_number = c(0,1,2,3),
                                   ss_daily_total = 0, #scaled
                                   #wbc = 0, #scaled
                                   abx_b_number = c(0,1,2,3),
                                   #temp_f_max = 0, # scaled
                                   creat = 0, # scaled
            ),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  mutate(sim_var = as.character(sim_var)) %>%
  left_join(cbind(expand_grid(ast_alt = seq(0,3000,50),
                              pressor_number = c(0,1,2,3),
                              ss_daily_total = 0, #scaled
                              #wbc = 0, #scaled
                              abx_b_number = c(0,1,2,3),
                              #temp_f_max = 0, # scaled
                              creat = 0, # scaled
                              ),
                  sim_var = paste0("V",seq(976))),
            by = "sim_var") %>%
  mutate(pressor_number = paste0("Pressors: ",pressor_number),
         abx_b_number = paste0("Antibiotics: ",abx_b_number)) %>%
  group_by(ast_alt, pressor_number, abx_b_number) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ast_alt, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  geom_hline(yintercept = 0.5, color = "black", linetype = 2) +
  annotate(geom = "text", x = 1500, y = 0.9, label = "Favors IVAC", color = "black", size = 3) +
  annotate(geom = "text", x = 1500, y = 0.1, label = "Favors VAC", color = "black", size = 3) +
  facet_grid(rows = vars(abx_b_number), cols = vars(pressor_number)) +
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
  labs(x = "Serum Aspartate Aminotransferase (AST) + Alanine Aminotransferase (ALT) (units/L)",
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac_explore
p_ivac_vs_vac_explore


# ggsave(plot = p_ivac_vs_vac_explore, filename = "./figs/supplement/p_ivac_vs_vac_explore.pdf", height = 8.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_vs_vac_explore, filename = "./figs/supplement/p_ivac_vs_vac_explore.svg", height = 8.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_vs_vac_explore, filename = "./figs/supplement/p_ivac_vs_vac_explore.png", height = 8.5, width = 8.5, units = "in", dpi = 600)










# ALT model
d_explore %>%
  #mutate(ast_alt = ast+alt) %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total, pressor_number, abx_b_number, alt, creat, bnp) %>%
  # replace NA where NA = 0
  mutate_at(.vars = vars(pressor_number, abx_b_number), .funs = ~ replace(.x, is.na(.x), 0)) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn, -bnp) %>%
  filter(d_rel_vae == 0) %>% # same only
  filter(complete.cases(.)) -> d_mod
d_mod


d_explore %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss

#' 
#' #' which priors
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   get_prior(data = ., family = bernoulli,
#'             ivac_yn ~ ss_daily_total + alt + 1,
#'             seed = 16)
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ ss_daily_total + alt + 1,
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(2), class = cor),
#'       ),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ ss_daily_total + alt + 1,
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b)#,
#'                 #prior(horseshoe(), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(2), class = cor),
#'       ),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_yn_explore_alt_only_brms
#' 
#' 
#' m_ivac_yn_explore_alt_only_brms %>% write_rds(path = "./models/binomial/m_ivac_yn_explore_alt_only_brms.rds.gz", compress = "gz")
#' m_ivac_yn_explore_alt_only_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_yn_explore_alt_only_brms_stanfit.rds.gz", compress = "gz")
m_ivac_yn_explore_alt_only_brms <- read_rds(path = "./models/binomial/m_ivac_yn_explore_alt_only_brms.rds.gz")

m_ivac_yn_explore_alt_only_brms$formula


pp_check(m_ivac_yn_explore_alt_only_brms)
m_ivac_yn_explore_alt_only_brms$fit -> m_ivac_yn_explore_alt_only_stan
rstan::check_hmc_diagnostics(m_ivac_yn_explore_alt_only_stan)

m_ivac_yn_explore_alt_only_brms %>%
  tidybayes::get_variables()

m_ivac_yn_explore_alt_only_brms %>%
  tidybayes::recover_types()


m_ivac_yn_explore_alt_only_stan %>%
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

# m_ivac_yn_explore_alt_only_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, b_alt) %>%
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw) %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh()
# 
# 
# m_ivac_yn_explore_alt_only_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, b_alt) %>%
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw) %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh()
# 


#' posterior plot

#' custom color
#lm_colors <- colorspace::sequential_hcl(5, palette = "Blues 3")

m_ivac_yn_explore_alt_only_brms %>%
  pp_expect(object = .,
            newdata = expand_grid(alt = scale(seq(0,1000,50))[,1],
                                  #pressor_number = c(0,1,2,3),
                                  ss_daily_total = scale(seq(0,21,3))[,1], #scaled
                                  #wbc = 0, #scaled
                                  #abx_b_number = c(0,1,2,3),
                                  #temp_f_max = 0, # scaled
                                  #creat = 0, # scaled
            ),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  mutate(sim_var = as.character(sim_var)) %>%
  left_join(cbind(expand_grid(alt = seq(0,1000,50),
                              #pressor_number = c(0,1,2,3),
                              ss_daily_total = seq(0,21,3), #un-scaled
                              #wbc = 0, #scaled
                              #abx_b_number = c(0,1,2,3),
                              #temp_f_max = 0, # scaled
                              #creat = 0, # scaled
  ),
  sim_var = paste0("V",seq(168))),
  by = "sim_var") %>%
  mutate(ss_daily_total = paste0("Subglottic Suction\nEvents: ", stringr::str_pad(string = as.character(ss_daily_total), width = 2, side = "left", pad = "0"))) %>%
  group_by(alt, ss_daily_total) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = alt, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  facet_wrap(~ ss_daily_total, nrow = 2) +
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
  labs(x = "Serum Alanine Aminotransferase (ALT) (units/L)",
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac_explore_alt_only
p_ivac_vs_vac_explore_alt_only


# ggsave(plot = p_ivac_vs_vac_explore_alt_only, filename = "./figs/supplement/p_ivac_vs_vac_explore_alt_only.pdf", height = 6.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_vs_vac_explore_alt_only, filename = "./figs/supplement/p_ivac_vs_vac_explore_alt_only.svg", height = 6.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_vs_vac_explore_alt_only, filename = "./figs/supplement/p_ivac_vs_vac_explore_alt_only.png", height = 6.5, width = 8.5, units = "in", dpi = 600)









# AST model
d_explore %>%
  #mutate(ast_alt = ast+ast) %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total, pressor_number, abx_b_number, ast, creat, bnp) %>%
  # replace NA where NA = 0
  mutate_at(.vars = vars(pressor_number, abx_b_number), .funs = ~ replace(.x, is.na(.x), 0)) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -d_rel_vae, -vae_day_yn, -ivac_yn), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  select(-vae_day_yn, -bnp) %>%
  filter(d_rel_vae == 0) %>% # same only
  filter(complete.cases(.)) -> d_mod
d_mod


d_explore %>%
  select(subject_id, d_rel_vae, vae_day_yn, ivac_yn, ss_daily_total) %>%
  pull(ss_daily_total) -> d_mod_ss
d_mod_ss

#' 
#' #' which priors
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   get_prior(data = ., family = bernoulli,
#'             ivac_yn ~ ss_daily_total + ast + 1,
#'             seed = 16)
#' 
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ ss_daily_total + ast + 1,
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(2), class = cor),
#'       ),
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
#'   mutate(ivac_yn = as.numeric(ivac_yn)) %>%
#'   brm(data = ., family = bernoulli,
#'       ivac_yn ~ ss_daily_total + ast + 1,
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b)#,
#'                 #prior(horseshoe(), class = b)#,
#'                 #prior(exponential(1), class = sd),
#'                 #prior(lkj(2), class = cor),
#'       ),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_ivac_yn_explore_ast_only_brms
#' 
#' 
#' m_ivac_yn_explore_ast_only_brms %>% write_rds(path = "./models/binomial/m_ivac_yn_explore_ast_only_brms.rds.gz", compress = "gz")
#' m_ivac_yn_explore_ast_only_brms$fit %>% write_rds(path = "./models/binomial/m_ivac_yn_explore_ast_only_brms_stanfit.rds.gz", compress = "gz")
m_ivac_yn_explore_ast_only_brms <- read_rds(path = "./models/binomial/m_ivac_yn_explore_ast_only_brms.rds.gz")

m_ivac_yn_explore_ast_only_brms$formula


pp_check(m_ivac_yn_explore_ast_only_brms)
m_ivac_yn_explore_ast_only_brms$fit -> m_ivac_yn_explore_ast_only_stan
rstan::check_hmc_diagnostics(m_ivac_yn_explore_ast_only_stan)

m_ivac_yn_explore_ast_only_brms %>%
  tidybayes::get_variables()

m_ivac_yn_explore_ast_only_brms %>%
  tidybayes::recover_types()


m_ivac_yn_explore_ast_only_stan %>%
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

# m_ivac_yn_explore_ast_only_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, b_ast) %>%
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw) %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_eyeh()
# 
# 
# m_ivac_yn_explore_ast_only_brms %>%
#   #get_variables()
#   spread_draws(b_Intercept, b_ss_daily_total, b_ast) %>%
#   gather(key = "param", value = "posterior", -.chain, -.iteration, -.draw) %>%
#   mutate(posterior = exp(posterior)) %>% # put on odds ratio scale
#   ggplot(aes(x = posterior, y = param)) +
#   tidybayes::stat_dotsh() +
#   tidybayes::stat_pointintervalh()
# 


#' posterior  plot
#' custom color
#' lm_colors <- colorspace::sequential_hcl(5, palette = "Reds 3")

m_ivac_yn_explore_ast_only_brms %>%
  pp_expect(object = .,
            newdata = expand_grid(ast = scale(seq(0,1000,50))[,1],
                                  #pressor_number = c(0,1,2,3),
                                  ss_daily_total = scale(seq(0,21,3))[,1], #scaled
                                  #wbc = 0, #scaled
                                  #abx_b_number = c(0,1,2,3),
                                  #temp_f_max = 0, # scaled
                                  #creat = 0, # scaled
            ),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  mutate(sim_var = as.character(sim_var)) %>%
  left_join(cbind(expand_grid(ast = seq(0,1000,50),
                              #pressor_number = c(0,1,2,3),
                              ss_daily_total = seq(0,21,3), #un-scaled
                              #wbc = 0, #scaled
                              #abx_b_number = c(0,1,2,3),
                              #temp_f_max = 0, # scaled
                              #creat = 0, # scaled
  ),
  sim_var = paste0("V",seq(168))),
  by = "sim_var") %>%
  mutate(ss_daily_total = paste0("Subglottic Suction\nEvents: ", stringr::str_pad(string = as.character(ss_daily_total), width = 2, side = "left", pad = "0"))) %>%
  group_by(ast, ss_daily_total) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ast, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  facet_wrap(~ ss_daily_total, nrow = 2) +
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
  labs(x = "Serum Aspartate Aminotransferase (AST) (units/L)",
       y = "Probability of IVAC versus VAC"#,
       #colour = "",
       #title = "VAC Probability ~ Daily Subglottic Suctioning Events"#,
  ) -> p_ivac_vs_vac_explore_ast_only
p_ivac_vs_vac_explore_ast_only


# ggsave(plot = p_ivac_vs_vac_explore_ast_only, filename = "./figs/supplement/p_ivac_vs_vac_explore_ast_only.pdf", height = 6.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_vs_vac_explore_ast_only, filename = "./figs/supplement/p_ivac_vs_vac_explore_ast_only.svg", height = 6.5, width = 8.5, units = "in")
# ggsave(plot = p_ivac_vs_vac_explore_ast_only, filename = "./figs/supplement/p_ivac_vs_vac_explore_ast_only.png", height = 6.5, width = 8.5, units = "in", dpi = 600)




#' combine AST and ALT "only" model plots

library(patchwork)

(p_ivac_vs_vac_explore_ast_only / p_ivac_vs_vac_explore_alt_only) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 10),
  ) -> p_ast_alt_combined

p_ast_alt_combined



# ggsave(plot = p_ast_alt_combined, filename = "./figs/supplement/p_ast_alt_combined.pdf", height = 8.5, width = 8.5, units = "in")
# ggsave(plot = p_ast_alt_combined, filename = "./figs/supplement/p_ast_alt_combined.svg", height = 8.5, width = 8.5, units = "in")
# ggsave(plot = p_ast_alt_combined, filename = "./figs/supplement/p_ast_alt_combined.png", height = 8.5, width = 8.5, units = "in", dpi = 600)
















