# -- head -- #

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)

set.seed(1337)

# -- read -- #

d = read_tsv('dat/final_counts.tsv') |>
  mutate(suffix = fct_relevel(suffix, 'stem'))  # stem as reference level instead of the alphabetical default (lik)

# transitive_verb added as an additive predictor (no interaction) now that
# dat/lemma_transitivity_decisions.tsv's hand-checked calls are carried through to
# final_counts.tsv (script/tally_verbs.R): TRUE for lemmas confirmed to take a direct accusative
# object under some construction, FALSE for lemmas confirmed never to
# blm1 = rstanarm::stan_glmer(cbind(k,m) ~ suffix + log_1sg + llfpm10 + transitive_verb + (1|lemma), family = binomial, data = d, chains = 4, iter = 4000)

# write_rds(blm1, 'fits/blm1.rds')
blm1 = readRDS('fits/blm1.rds')

# -- labs -- #
# nice labels for the suffix classes, csak tő (stem) first. 'odik' not 'Vdik' this round: the
# suffix-classification regex in subset_verbs.R was rewritten and renamed the category
suffix_labels = c(
  stem = 'stem only',
  zik = '-zik',
  odik = '-odik/edik/ödik',
  szik = '-szik',
  lik = '-lik'
)

# -- bayes -- #

rstanarm::prior_summary(blm1)

# posterior draws of the per-suffix marginal log-odds, the bayesian equivalent of emmeans1 above
# (same emmeans() reference grid, but gather_emmeans_draws() keeps the full posterior instead of
# collapsing it to a point estimate + interval)
bayes_draws = emmeans::emmeans(blm1, 'suffix', data = d) |>
  tidybayes::gather_emmeans_draws()

model_summary = broom.mixed::tidy(blm1, conf.int = T, conf.level = .95) |>
  select(term,estimate,conf.low,conf.high) |>
  filter(term != 'sd_(Intercept).lemma') |>
  mutate_if(is.double, ~ round(., 2))

write_tsv(model_summary, 'dat/best_model_summary.tsv')

# booktabs table: every fixed-effect estimate (log-odds scale) with its 95% credible interval
model_summary |>
  rename(Term = term, Estimate = estimate, `2.5%` = conf.low, `97.5%` = conf.high) |>
  knitr::kable(
    format = 'latex', booktabs = TRUE,
    caption = 'Posterior estimates (log-odds scale) with 95\\% credible intervals.',
    label = 'model-estimates'
  ) |>
  print()

# booktabs table: posterior median p(-k) by suffix class, with 95% credible intervals
bayes_draws |>
  group_by(suffix) |>
  summarise(med = median(.value), lo = quantile(.value, .025), hi = quantile(.value, .975)) |>
  arrange(desc(med)) |>
  mutate(
    Suffix = factor(suffix, levels = names(suffix_labels), labels = suffix_labels) |> as.character(),
    `p(-k)` = round(plogis(med), 2),
    `95% CI` = sprintf('[%.2f, %.2f]', plogis(lo), plogis(hi))
  ) |>
  select(Suffix, `p(-k)`, `95% CI`) |>
  knitr::kable(
    format = 'latex', booktabs = TRUE,
    caption = 'Posterior median $p(\\text{-}k)$ by suffix class, with 95\\% credible intervals.',
    label = 'suffix-pk'
  ) |>
  print()

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
ggsave('viz/bayes_halfeye_suffix.pdf', width = 5, height = 4)

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

