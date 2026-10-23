# -- head -- #

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)
library(patchwork)

# -- read -- #

d = read_tsv('dat/sample_counts.tsv')
all = read_tsv('dat/verbs.tsv')

# -- counts -- #

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
  scale_fill_grey(name = 'igevégződés', labels = c(`TRUE` = 'ik', `FALSE` = '0')) +
  theme_bw() +
  xlab('') +
  ylab('mennyiség (webkorpusz)')

ggsave('viz/counts.png', width = 6, height = 4, dpi = 900)
ggsave('viz/counts.pdf', width = 6, height = 4)
