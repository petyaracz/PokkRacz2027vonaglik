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

d |>
  filter(ending == 'm', !str_detect(final_call, regex('exclude', ignore_case = TRUE))) |>
  add_count(lemma, name = 'n_lemma') |>
  mutate(
    weight = 1 / n_lemma,
    KWIC2 = str_replace(KWIC, '[oeöa]m$', 'ik'),
    Left2 = word(Left, -4, -1)
  ) |>
  slice_sample(n = 150, weight_by = weight) |>
  select(lemma, Left2, KWIC, KWIC2, Right, judgement) |>
  write_tsv('dat/proba.tsv')


d |>
  filter(final_call == 'object', ending == 'm', !str_detect(final_call, regex('exclude', ignore_case = TRUE))) |>
  add_count(lemma, name = 'n_lemma') |>
  mutate(
    weight = 1 / n_lemma,
    KWIC2 = str_replace(KWIC, '[oeöa]m$', 'ik'),
    Left2 = word(Left, -4, -1)
  ) |>
  slice_sample(n = 150, weight_by = weight) |>
  select(lemma, Left2, KWIC, KWIC2, Right, judgement) |>
  write_tsv('dat/proba2.tsv')

# -- read in -- #

p1 = read_tsv('dat/proba.tsv')

p1 |> count(`...4`)
# 8/150

p2 = read_tsv('dat/proba2.tsv')
p2 |> count(`...3`)
# but I only counted to 50
# 2/50
