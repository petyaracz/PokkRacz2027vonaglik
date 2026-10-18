# -- head -- #

# full spot-check file: every original concordance row (both -k and -m, every lemma including
# excluded ones), with the definiteness judgement attached, for manual review of the whole
# pipeline rather than just the subset that triggered a "definite" call.

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)

# -- read -- #

conc = read_tsv('dat/concordance_mnsz2_tidy.tsv') |> mutate(row_id = row_number())  # stable id:
# some (Reference,Left,KWIC,Right,lemma) tuples are genuine duplicate corpus hits, not join-safe keys
judged = read_tsv('dat/objcap_rows_classified.tsv')  # -m rows only, post-exclusion, carries row_id
decisions = read_tsv('dat/lemma_transitivity_decisions.tsv') |>
  mutate(final_call = coalesce(na_if(your_call, ''), claude_call))

# -- def -- #

out = conc |>
  mutate(ending = if_else(str_detect(str_to_lower(KWIC), 'k$'), 'k', 'm')) |>
  left_join(judged |> select(row_id, def_judgement = heuristic_call), by = 'row_id') |>
  left_join(decisions |> select(lemma, final_call), by = 'lemma') |>
  mutate(
    judgement = case_when(
      str_detect(str_to_lower(final_call), 'exclude') ~ 'EXCLUDED (data quality, see final_call)',
      ending == 'k' ~ 'indefinite (unambiguous, -k never marks definite)',
      ending == 'm' ~ def_judgement
    )
  ) |>
  select(lemma, ending, judgement, final_call, Reference, Left, KWIC, Right)

cat('n total rows:', nrow(out), '\n')
cat('n rows with no judgement (should be 0):', sum(is.na(out$judgement)), '\n')
print(count(out, judgement))

# -- write -- #

write_tsv(out, 'dat/concordance_mnsz2_judged.tsv')
