# Code Testing before Template
require(here)
rope = read.csv(here("data", "rope.csv"))

rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)

rope$rope.type

sum(summary(rope$rope.type))

length(unique(rope$rope.type))

# to calculate resids = actual - average

agg_resids_test = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))


agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) (x) - mean(x))

str(agg_resids)

agg_sum_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum(((x) - mean(x))^2))

str(agg_sum_sq_resids)

ss_within = sum(agg_sum_sq_resids$x)

#F

f_ratio = ms_among/ms_within
pf(f_ratio, 5, 115)

#using ANOVA
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"

rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("PI", "VEL", "XTC"))
)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

fit_rope_2 = lm(p.cut ~ rope.type, data=rope2)
rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)

round(rope2_hsd$rope.type, digits = 4)

#     Question 3 Bartlet Test

bartlett.test(rope$p.cut,rope$rope.type)

bartlett.test(p.cut ~ rope.type, data= rope)

agg_resids_test = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

#     Question 5

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

#     Question 8

residuals(fit_rope_1)

shapiro.test(residuals(fit_rope_1))

#     Question 10
shapiro.test(agg_resids$x)

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) (x) - mean(x))


#     Question 12-17
require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")

boxplot(body_mass_g ~ species, data= pen_fem,
        xlab= "Species",
        ylab = "Body Mass",
        main= "Body Mass Conditioned on Species")

bartlett.test(body_mass_g ~ species, data= pen_fem)

fit_pen_1 = lm(body_mass_g ~ species, data= pen_fem)
summary(fit_pen_1)

shapiro.test(residuals(fit_pen_1))

TukeyHSD(aov(fit_pen_1))




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
                        
#Self Check
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)


