# -- head -- #

setwd('~/Github/RaczPokk2027/')

library(tidyverse)

set.seed(1337)

# -- read -- #

d = read_tsv('dat/concordance_mnsz2_judged.tsv')

# -- check -- #

d |> 
  count(judgement)

d |> 
  filter(judgement == 'definite') |> 
  sample_n(10) |> 
  select(Left,KWIC,Right)

d |> 
  filter(judgement == 'indefinite') |> 
  sample_n(10) |> 
  select(Left,KWIC,Right)
