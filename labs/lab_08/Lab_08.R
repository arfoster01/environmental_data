require(here)
veg_dat = read.csv(here("data", "vegdata.csv"))
bird_dat = read.csv(here("data", "bird.sub.csv"))
hab_dat = read.csv(here("data", "hab.sub.csv"))

install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)
penguins = data.frame(penguins)

no_gentoo = na.omit(droplevels(
  subset(penguins, species != "Gentoo")))

t.test(flipper_length_mm ~ species, data = no_gentoo, alternative = "less")

install.packages("boot")
library(boot)
require(simpleboot)
require(here)

adelie_dat= na.omit(droplevels(
  subset(no_gentoo, species != "Chinstrap")))
chinstrap_dat= na.omit(droplevels(
  subset(no_gentoo, species != "Adelie")))

x = rnorm(adelie_dat$flipper_length_mm)
y = rnorm(chinstrap_dat$flipper_length_mm)

pen_boot= two.boot(x, 
         y, 
         data= no_gentoo, 
         FUN = mean,
         R = 10000,
         na.rm= TRUE)
str(pen_boot)

hist(pen_boot$t, 
     main= "Histogram of 10000 bootstrap replicates
     of the differences in mean flipper length",
     xlab= "Difference in mean flipper length (mm)")

quantile(pen_boot$t, c(0.025, 0.975))

mean(pen_boot$t)

median(pen_boot$t)

sd(pen_boot$t)

#     Questions 5-7
pen_ecdf <- ecdf(pen_boot$t)
pen_ecdf(pen_boot$t >= -4.5)

pen_ecdf(pen_boot$t <= -8)

#     Question 9

head(veg_dat)
no_removed = na.omit(droplevels(
  subset(veg_dat, treatment != "removed")))
control_clipped = na.omit(droplevels(
  subset( no_removed, treatment != "mixed")))

wilcox.test(control_clipped$pine, na.rm= TRUE)

#     Question 10 and 11
control= subset(control_clipped, treatment== "control")
clipped= subset(control_clipped, treatment== "clipped")

p= rnorm(control$pine)
q= rnorm(clipped$pine)

tree_boot= two.boot(p, q, 
                    FUN = mean,
                    R = 10000,
                    na.rm= TRUE)

quantile(tree_boot$t, c(0.025, 0.975))

boot.ci(tree_boot)
quantile(tree_boot$t, c(0.025, 0.975))

#     Question 12/13
dat_all = merge(
  bird_dat, 
  hab_dat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

dat_all$b.sidi.standardized = 
  (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

#     Question 14 and 15
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

m=10000
result_mc= numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  
  slope_observed = coef(fit_1)[2]
  
  result_mc[i] = coef(fit_resampled_i)[2]
} 

quantile(result_mc, c(.05))

hist(
  result_mc,
  main = "Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v= quantile(result_mc, c(.05)), lty = 2, col = "red", lwd = 2)

#     Question 18-20
set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)

m=10000
result_mc= numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot =  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )
  
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  slope_resampled_i = coef(fit_bs1)[2]
  
  slope_observed = coef(fit_1)[2]
  
  result_boot = coef(fit_bs1)[2]
} 

hist(
  result_boot,
  main = "Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)





 