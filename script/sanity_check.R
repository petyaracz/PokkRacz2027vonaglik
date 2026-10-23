# -- head -- #

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)

set.seed(1337)

# -- read -- #

d = read_tsv('dat/concordance_mnsz2_judged.tsv')
f = read_tsv('dat/final_counts.tsv')

# -- check -- #

f |> 
  filter(suffix == 'lik', m == 0)

# -- excl -- #

d |> 
  filter(str_detect(final_call, 'xclude')) |> 
  distinct(lemma)

# -- sample -- #

# one file, 50 definite + 50 indefinite -m hits, each stratum weighted by 1/n_lemma so no single
# high-frequency lemma dominates its half of the sample
d |>
  filter(ending == 'm', !str_detect(final_call, regex('exclude', ignore_case = TRUE))) |>
  add_count(lemma, name = 'n_lemma') |>
  mutate(
    weight = 1 / n_lemma,
    KWIC2 = str_replace(KWIC, '[oeöa]m$', 'ik'),
    Left2 = word(Left, -4, -1)
  ) |>
  group_by(judgement) |>
  slice_sample(n = 50, weight_by = weight) |>
  ungroup() |>
  select(lemma, Left2, KWIC, KWIC2, Right, judgement) |>
  write_tsv('dat/proba.tsv')
