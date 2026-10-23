# -- head -- #

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)

# -- read -- #

# per-row definite/indefinite calls for the -m rows (script/classify_definiteness.R), carries
# row_id so rows can be dropped safely even when duplicate (Reference,Left,KWIC,Right,lemma)
# tuples exist (genuine duplicate corpus hits, not a bug -- see script/classify_definiteness.R)
m = read_tsv('dat/objcap_rows_classified.tsv')
# every -k/-m hit, all originally-sampled lemmas, MNSZ2-UD concordance
all = read_tsv('dat/concordance_mnsz2_tidy.tsv') |> mutate(row_id = row_number())
# sample metadata (lemma, frequency, suffix class)
ref = read_tsv('dat/ik_sample.tsv') |> mutate(suffix = replace_na(suffix, 'stem'))
decisions = read_tsv('dat/lemma_transitivity_decisions.tsv') |>
  mutate(final_call = coalesce(na_if(your_call, ''), claude_call))

# -- def -- #

# lemmas excluded for data-quality reasons (ellik, illik, telik, íratik: homograph-contaminated,
# see dat/lemma_transitivity_decisions.tsv) never belonged in the tally at all
exclude_lemmas = decisions |> filter(str_detect(str_to_lower(final_call), 'exclude')) |> pull(lemma)

# hand-checked transitivity, collapsed to a boolean predictor: TRUE for lemmas confirmed to take
# a direct accusative object under some construction ('object' or 'object_capable'), FALSE for
# lemmas confirmed never to ('no_object'). excluded lemmas never reach this join (dropped from
# `d` below), so every remaining lemma resolves to exactly one of the two
transitivity = decisions |>
  mutate(transitive_verb = final_call %in% c('object', 'object_capable')) |>
  select(lemma, transitive_verb)

# rows the heuristic called genuinely definite: strip these out rather than tally them as -ik
# indefinite use. dropped by row_id, not by content columns: a handful of (Reference,Left,KWIC,
# Right,lemma) tuples are genuine duplicate corpus hits, so a content-based anti_join either
# drops too many or too few of them depending on join multiplicity
drop_ids = m |> filter(heuristic_call == 'definite') |> pull(row_id)

d = all |>
  filter(!lemma %in% exclude_lemmas, !row_id %in% drop_ids)

# -- run -- #

counts = d |>
  mutate(ending = if_else(str_detect(str_to_lower(KWIC), 'k$'), 'k', 'm')) |>
  count(lemma, ending) |>
  pivot_wider(names_from = ending, values_from = n, values_fill = 0) |>
  mutate(
    p_k = k / (k + m),
    lo_k_m = log((k + 1) / (m + 1))  # log odds of -k over -m, Laplace-smoothed
  )

# subset_verbs.R no longer deduplicates by lemma before sampling, so a handful of lemmas with
# two attested -k stem variants (syncopated/non-syncopated: igyekszem/igyekszek vs igyekezem/
# igyekezek, same alternation as cselekszik/cselekedik) got drawn twice. distinct() here so the
# join below doesn't silently double-count their k/m totals in the final tally
metadata = ref |>
  select(lemma, lemma_freq, corpus_size, llfpm10, lemma_syl_count, suffix) |>
  distinct(lemma, .keep_all = TRUE)

counts2 = left_join(metadata, counts, by = 'lemma') |>
  left_join(transitivity, by = 'lemma')

counts2 = counts2 |>
  filter(!is.na(k))  # lemmas with no surviving hits at all after exclusions

# -- write -- #

write_tsv(counts2, 'dat/sample_counts.tsv')

cat('n lemmas in final tally:', nrow(counts2), '\n')
cat('n lemmas with no transitivity decision (should be 0):', sum(is.na(counts2$transitive_verb)), '\n')
print(count(counts2, suffix))
print(count(counts2, transitive_verb))
