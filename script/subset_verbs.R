# -- head -- #

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)

set.seed(1337)

# -- read -- #

# webcorpus2 frequency list; hunspell flags real dictionary words, screening out typos and junk
c = arrow::read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet') |>
  filter(hunspell)

# -- def -- #

# plural nouns, so we can drop verb/noun homographs below (e.g. horgászok is both "I fish" and "anglers")
plur = c |>
  filter(xpostag == '[/N][Pl][Nom]') |> 
  pull(form)

poss = c |>
  filter(xpostag == "[/N][Poss.1Sg][Nom]") |> 
  pull(form)

# big ik/nem ik set
verbs = c |> 
  filter(
    xpostag == '[/V][Prs.NDef.1Sg]' | xpostag == '[/V][Prs.NDef.3Sg]',
    !form %in% plur
         ) |> 
  mutate(
    ik = str_detect(lemma, 'ik$'),
    suffix = case_when(
      str_detect(lemma, '[oeö]dik$') ~ 'odik',
      str_detect(lemma, 'szik$') & form_syl_count > 2 ~ 'szik',
      str_detect(lemma, '[^l]lik$') & form_syl_count > 1 ~ 'lik',
      str_detect(lemma, '[^s]zik$') ~ 'zik',
      str_detect(lemma, '[oeö]d$') ~ 'od',
      str_detect(lemma, 'sz$') & form_syl_count > 2 ~ 'sz',
      str_detect(lemma, '[^l]l$') & form_syl_count > 1 ~ 'l',
      str_detect(lemma, '[^s]z$') & form_syl_count > 1 ~ 'z'
    ),
    class = case_when(
      suffix %in% c('odik','od') ~ 'd',
      suffix %in% c('szik','sz') ~ 'sz',
      suffix %in% c('lik','l') ~ 'l',
      suffix %in% c('zik','z') ~ 'z'
    )
  )

# candidate -ik verbs: 1sg indefinite -k forms. This tag is reliable, unlike the -m side of the
# alternation, which the parser routinely confuses with 1sg definite (see readme)
ik = verbs |>
  filter(
    freq > 10,
    !form %in% 'bányászok',
    str_detect(lemma, 'ik$'),
    xpostag == '[/V][Prs.NDef.1Sg]',
    str_detect(form, 'k$'),
    # screens out the -m side's own homograph risk, the same way !form %in% plur screens the -k
    # side above: horgászik's -m form "horgászom" ("I fish") is indistinguishable in Webcorpus2's
    # own tagging from the 1sg-possessive noun "horgászom" ("my fisherman"). 13 lemmas confirmed
    # affected this round (eszik, lakik, jelenik, fosik, adatik, fingik, rajzik, madarászik,
    # vadászik, telik, horgászik, gombászik, hajhászik) -- none happened to land in a prior
    # working sample, but it's a latent risk on any resample, so excluded at the source here
    # rather than left as a diagnostic-only check
    !str_replace(form, 'k$', 'm') %in% poss
  )

# -- run -- #

ik |>
  count(suffix)

ik = ik |>
  group_by(suffix) |>
  sample_n(40)

# build one regex from the -k forms, swapping the final k for a [km] class so a literal
# string search (run externally, against MNSZ2-UD) picks up both variants at once
ik_query = ik |>
  pull(form) |>
  paste(collapse = '|') |>
  str_replace_all('[km]\\|', '[km]|') |>  # ...k| -> ...[km]| for every item but the last
  str_replace('[km]$', '[km]')            # ...k  -> ...[km] for the last item, which has no trailing |

ik_query = glue::glue('({ik_query})')

# -- write -- #

write_tsv(ik, 'dat/ik_sample.tsv')
write_lines(ik_query, 'dat/ik_query.txt')
write_tsv(verbs, 'dat/verbs.tsv')
