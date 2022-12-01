#likelihood of 2 Observations
x_observed = c(2,6)
print(x_observed)
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)
dpois(x = 2, lambda = 4.5) * 
  dpois(x = 6, lambda = 4.5)
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
prod(dpois(x = wiwa_counts, lambda = 4.5))
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

#loading data
require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all= merge(dat_habitat, dat_bird)

#question 1 and 2
wiwa_counts = c(2, 6)
sum(dpois(x = wiwa_counts, lambda = 2.3))

#question 3-5
summary(dat_bird$WIWR)
wiwr_counts= c(1,6)
dpois(x = wiwr_counts, lambda = 1)
sum(log(dpois(x = wiwr_counts, lambda = 1)))

par(mfrow = c(1, 2))
dat = dat_all$WIWR
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

#question 6-9
sum(log(dpois(x = wiwr_counts, lambda = 1)))
x_bin = 1:6
dbinom(2, 6, 0.368)
sum(log(rbinom(2, 6, 0.368)))

#question 12-14
set.seed(1)
vec_rnorm = rnorm(n = 10, mean = 0, sd = 1)

sum(log(dnorm(vec_rnorm, mean = 0, sd = 1)))
vec_rnorm = rnorm(n = 10, mean = 0.04, sd = .3)
like = dnorm(vec_rnorm, 0, 2)
log_vec = sum(log(like))
log_vec



