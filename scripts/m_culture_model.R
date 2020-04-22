#' ########################################
#' culture models
#' ########################################
#' 
#' 
#' 

#' load libraries and data
library(tidyverse)
library(tidybayes)
library(brms)


lm_colors <- colorspace::sequential_hcl(5, palette = "Grays")
#' custom color
#' lm_colors <- colorspace::sequential_hcl(5, palette = "Blues 3")


d_culture <- read_csv(file = "./models/binomial/d_culture.csv")
d_culture



#' ##########################################################################
#' ##########################################################################
#' 
#' RANDOM EFFECTS MODEL: CULTURE vs SUBGLOTTIC and FEVER
#' 
#' ##########################################################################
#' ##########################################################################

d_culture %>%
  mutate(fever = temp_f_max > 100.4 | temp_f_min < 96.8) %>%
  select(subject_id, sputum_check, ss_daily_total, fever) %>%
  # scale exposure variables
  mutate_at(.vars = vars(-subject_id, -sputum_check, -fever), .funs = ~scale(.x)[,1]) %>%
  # format data for Stan
  filter(complete.cases(.)) -> d_mod
d_mod


d_mod %>%
  count(subject_id)
# 2111 observations from 87 subjects

#' 
#' #' which priors
#' d_mod %>%
#'   mutate(sputum_check = as.numeric(sputum_check)) %>%
#'   get_prior(data = ., family = bernoulli,
#'             sputum_check ~ 1 + ss_daily_total + fever + (1 + ss_daily_total + fever | subject_id))
#' 
#' #' prior predictive plot
#' d_mod %>%
#'   mutate(sputum_check = as.numeric(sputum_check)) %>%
#'   brm(data = ., family = bernoulli,
#'       sputum_check ~ 1 + ss_daily_total + fever + (1 + ss_daily_total + fever | subject_id),
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
#' #' non-centered with brms
#' d_mod %>%
#'   mutate(sputum_check = as.numeric(sputum_check)) %>%
#'   brm(data = ., family = bernoulli,
#'       sputum_check ~ 1 + ss_daily_total + fever + (1 + ss_daily_total + fever | subject_id),
#'       prior = c(prior(normal(0,0.5), class = Intercept),
#'                 prior(normal(0,0.5), class = b),
#'                 prior(exponential(1), class = sd),
#'                 prior(lkj(2), class = cor)),
#'       iter = 3000,
#'       warmup = 1000,
#'       chains = 4,
#'       cores = 4,
#'       control = list("adapt_delta" = 0.99, max_treedepth = 16),
#'       seed = 16) -> m_culture_ss_fever_subject_brms
#' 
#' 
#' m_culture_ss_fever_subject_brms %>% write_rds(path = "./models/binomial/m_culture_ss_fever_subject_brms.rds.gz", compress = "gz")
#' m_culture_ss_fever_subject_brms$fit %>% write_rds(path = "./models/binomial/m_culture_ss_fever_subject_brms_stanfit.rds.gz", compress = "gz")
m_culture_ss_fever_subject_brms <- read_rds(path = "./models/binomial/m_culture_ss_fever_subject_brms.rds.gz")

m_culture_ss_fever_subject_brms$formula


pp_check(m_culture_ss_fever_subject_brms)
m_culture_ss_fever_subject_brms$fit -> m_culture_ss_fever_subject_stan
rstan::check_hmc_diagnostics(m_culture_ss_fever_subject_stan)

m_culture_ss_fever_subject_brms %>%
  tidybayes::get_variables()

m_culture_ss_fever_subject_brms %>%
  tidybayes::recover_types()

m_culture_ss_fever_subject_brms %>%
  posterior_summary() %>%
  as_tibble(rownames = "param")
  
  


m_culture_ss_fever_subject_stan %>%
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
# 
# m_culture_ss_fever_subject_brms %>%
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
# m_culture_ss_fever_subject_brms %>%
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


#' posterior predictive plot
m_culture_ss_fever_subject_brms %>%
  pp_expect(object = .,
            newdata = tibble(ss_daily_total = rep(scale(seq(0, 21, length.out = 100))[,1], 2),
                             subject_id = rep("Subject 0100",200),
                             fever = rep(c(TRUE, FALSE), each = 100)),
            allow_new_levels = TRUE,
            re_formula = NA) %>%
  as_tibble() %>%
  #names()
  gather(key = "sim_var", value = "pp") %>%
  left_join(tibble(ss_daily_total = rep(seq(0, 21, length.out = 100), 2),
                   subject_id = rep("Subject 0100",200),
                   fever = rep(c(TRUE, FALSE), each = 100),
                   sim_var = paste0("V",seq(200))),
            by = "sim_var") %>%
  mutate(fever = ifelse(fever, "Fever", "No Fever")) %>%
  group_by(ss_daily_total, fever) %>%
  select(pp) %>%
  tidybayes::median_qi(.width = c(.95, .8, .5)) %>% 
  ungroup() %>%
  ggplot(data = .) + 
  geom_lineribbon(aes(x = ss_daily_total, y = pp, fill = factor(.width), ymin = .lower, ymax = .upper, color = .point), alpha = 0.8) + 
  facet_wrap(facets = ~ fever, nrow = 1) +
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
  labs(x = "Subglottic Suction Events",
       y = "Probability of Sputum Culture Order"#,
       #colour = "",
       #title = "Predicted VAC Probability ~ Daily Subglottic Suction Events"#,
  ) -> p_culture_ss_fever
p_culture_ss_fever


# ggsave(plot = p_culture_ss_fever, filename = "./figs/supplement/p_culture_ss_fever.pdf", height = 4, width = 6, units = "in")
# ggsave(plot = p_culture_ss_fever, filename = "./figs/supplement/p_culture_ss_fever.svg", height = 4, width = 6, units = "in")
# ggsave(plot = p_culture_ss_fever, filename = "./figs/supplement/p_culture_ss_fever.png", height = 4, width = 6, units = "in", dpi = 600)






