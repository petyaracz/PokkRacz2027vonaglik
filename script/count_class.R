# -- head -- #

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)
library(patchwork)

# -- read -- #

d = read_tsv('dat/sample_counts.tsv')
all = read_tsv('dat/verbs.tsv')

# -- counts -- #

all |> 
  mutate(class = fct_relevel(class, 'l', 'sz', 'd', 'z')) |> 
  filter(!is.na(class)) |> 
  count(class,ik)

p1 = all |> 
  mutate(class = fct_relevel(class, 'l', 'sz', 'd', 'z')) |> 
  filter(!is.na(class)) |> 
  ggplot(aes(class, fill = ik)) +
  geom_bar(position = position_dodge()) +
  coord_flip() +
  scale_x_discrete(labels = c(
    l  = '-l(ik)',
    sz = '-sz(ik)',
    d  = '-od(ik)/ed(ik)/ö(dik)',
    z  = '-z(ik)'
  )) +
  scale_fill_grey(name = 'verb ending', labels = c(`TRUE` = 'ik', `FALSE` = '0')) +
  theme_bw() +
  xlab('') +
  ylab('counts (webcorpus)')

ggsave('viz/counts.png', width = 6, height = 4, dpi = 900)
ggsave('viz/counts.pdf', width = 6, height = 4)

# -- moar conts -- #

# in all, the number of verbs that end in bare -ol/-el/-öl

all |> 
  filter(class == 'l', !ik) |>  # 4251
  filter(str_detect(lemma, '[oeöá]l$')) # 3324
3324/4251

all |> 
  filter(class == 'd',!ik) |> # 153
  filter(str_detect(lemma, 'k.d$')) # 32

32/153

all |> 
  filter(class == 'd',!ik) |> # 153
  filter(str_detect(lemma, 'ed$')) # 120

120/153

all |> 
  filter(class == 'd',ik) |> # 2116
  mutate(end = str_extract(lemma, '.dik$')) |> 
  count(end) |> 
  mutate(percent = n / 2116)


