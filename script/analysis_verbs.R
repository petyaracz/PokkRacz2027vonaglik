# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

set.seed(1337)

# -- read -- #

d = read_tsv('dat/final_counts.tsv')
blm1 = read_rds('fits/blm1.rds')

# -- labs -- #
# nice labels for the suffix classes, csak tő (stem) first. 'odik' not 'Vdik' this round: the
# suffix-classification regex in subset_verbs.R was rewritten and renamed the category
suffix_labels = c(
  stem = 'csak tő',
  zik = '-zik',
  odik = '-odik/edik/ödik',
  szik = '-szik',
  lik = '-lik'
)

# -- bayes -- #

# blm1 = rstanarm::stan_glmer(cbind(k,m) ~ suffix + log_1sg + llfpm10 + (1|lemma), family = binomial, data = d, chains = 4, iter = 4000)
# 
# write_rds(blm1, 'fits/blm1.rds')

rstanarm::prior_summary(blm1)

# posterior draws of the per-suffix marginal log-odds, the bayesian equivalent of emmeans1 above
# (same emmeans() reference grid, but gather_emmeans_draws() keeps the full posterior instead of
# collapsing it to a point estimate + interval)
bayes_draws = emmeans::emmeans(blm1, 'suffix', data = d) |>
  tidybayes::gather_emmeans_draws()

broom.mixed::tidy(blm1, conf.int = T, conf.level = .95) |> 
  select(term,estimate,conf.low,conf.high) |> 
  filter(term != 'sd_(Intercept).lemma') |> 
  mutate_if(is.double, ~ round(., 2)) |> 
  write_tsv('dat/best_model_summary.tsv')

bayes_draws |> 
  summarise(
    q1 = list(posterior::quantile2(.value))[[1]][1],
    q2 = list(posterior::quantile2(.value))[[1]][2],
    p1 = plogis(q1),
    p2 = plogis(q2)
  ) 

bayes_draws |>
  mutate(suffix = factor(suffix, levels = rev(names(suffix_labels)), labels = rev(suffix_labels))) |>
  ggplot(aes(.value, suffix)) +
  ggdist::stat_halfeye() +
  scale_x_continuous(
    # limits = c(-2.5,4),
    breaks = c(-2, 0, 2, 4),  # wide enough to show how far out -lik sits
    sec.axis = sec_axis(transform = plogis, name = 'p(-k)', breaks = c(.25, .5, .75, .9))
  ) +
  labs(x = 'log (-k / -m)', y = NULL) +
  theme_bw()
ggsave('viz/bayes_halfeye_suffix.png', width = 5, height = 4)


# ---
library(posterior)

# all parameters: fixed effects, lemma intercepts, Sigma
diag = as.array(blm1) |>
  as_draws_array() |>
  summarise_draws(default_convergence_measures())  # rhat, ess_bulk, ess_tail

# headline numbers for the text
diag |>
  summarise(
    max_rhat = max(rhat),
    min_ess_bulk = min(ess_bulk),
    min_ess_tail = min(ess_tail)
  )

# anything over threshold
diag |>
  filter(rhat > 1.01 | ess_bulk < 400 | ess_tail < 400)

# divergent transitions
rstan::get_num_divergent(blm1$stanfit)

