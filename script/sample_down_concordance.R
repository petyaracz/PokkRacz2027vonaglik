# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

set.seed(1337)

# -- read -- #

d = read_tsv('dat/sample_counts.tsv')
all = read_tsv('dat/verbs.tsv')

# -- combine -- #

ref = all |>
  filter(ik, xpostag == '[/V][Prs.NDef.1Sg]') |>
  summarise(
    freq = sum(freq),
    .by = c(lemma,lemma_freq,llfpm10,suffix)
  ) |> # we need this hack because of repeating things
  mutate(log_1sg = log(freq / lemma_freq), suffix = replace_na(suffix, 'stem')) |>
  select(lemma, log_1sg)

d2 = ref |>
  select(lemma, log_1sg) |>  # llfpm10 dropped here: d already carries it (identical
  inner_join(d, by = 'lemma')        # values, verified), joining both would collide into
# llfpm10.x/.y and silently break the model formula below



# -- sample down -- #

d2 |> count(suffix)

d3 = d2 |> 
  group_by(suffix) |> 
  sample_n(25)

# -- write -- #

write_tsv(d3, 'dat/final_counts.tsv')
