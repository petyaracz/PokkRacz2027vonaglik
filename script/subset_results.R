# -- head -- #

set.seed(1337)

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

# -- read -- #

# new MNSZ2-UD export format (differs from the old CSV): metadata header (4 lines) + a
# "Reference,Sentence" label line, then one line per hit as "doc#N | sentence text", the hit
# itself marked inline as <coll>...</coll>. no separate Left/KWIC/Right columns this time.
raw = read_lines('dat/concordance_mnsz2-ud_20261016162252.txt')
data_lines = raw[-(1:5)]

ref = read_tsv('dat/ik_sample.tsv') |> mutate(suffix = replace_na(suffix, 'stem'))

# -- def -- #

# split each line on the first " | " into reference and sentence, then pull Left/KWIC/Right
# out of the inline <coll> markup so the rest of the pipeline (classify_definiteness.R,
# tally_verbs.R) can keep using the same column names as before
split_idx = str_locate(data_lines, fixed(' | '))

d = tibble(
  Reference = str_sub(data_lines, 1, split_idx[, 1] - 1),
  Sentence = str_sub(data_lines, split_idx[, 2] + 1)
) |>
  mutate(
    KWIC = str_extract(Sentence, '(?<=<coll>).*?(?=</coll>)'),
    Left = str_extract(Sentence, '^.*(?=<coll>)'),
    Right = str_extract(Sentence, '(?<=</coll>).*$'),
    lemma = KWIC |> str_to_lower() |> str_replace_all('[oeö][km]$', 'ik')
  ) |>
  select(Reference, Left, KWIC, Right, lemma)

# -- run -- #

# no more per-class balancing: subset_verbs.R now takes every ik lemma with webcorpus freq > 10
# instead of a capped sample, so we just keep every lemma that got at least one corpus hit,
# unbalanced across suffix classes (the GLMM's per-lemma random intercept handles unequal n)
keep_verbs = inner_join(ref, d, by = 'lemma') |>
  distinct(lemma) |>
  pull(lemma)

d2 = d |>
  filter(lemma %in% keep_verbs)

# -- write -- #

write_tsv(d2, 'dat/concordance_mnsz2_tidy.tsv')
