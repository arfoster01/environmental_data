# Code Template
rm(list = ls())

require(here)

rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)

n_obs = sum(summary(rope$rope.type))
n_groups = length(unique(rope$rope.type))

ss_tot = sum((rope$p.cut - mean(rope$p.cut)^2))
df_tot = n_obs-1

agg_sum_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum(((x) - mean(x))^2))
ss_within = sum(agg_sum_sq_resids$x)
df_within = n_obs- n_groups

ss_among = ss_tot - ss_within
df_among = n_groups-1

ms_within = ss_within / df_within
ms_among  = ss_among / df_among

f_ratio = ms_among/ms_within
f_pval = pf(f_ratio, df_among, df_within, 
            lower.tail = FALSE)