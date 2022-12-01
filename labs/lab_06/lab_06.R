# Prelab info
require(palmerpenguins)
dat_pen

sse_mean = 
  function(x) 
    (sd(x, na.rm = TRUE)/sqrt(length(x)))

sse_mean(penguins$bill_depth_mm) 

#     Question 1
rm(list = ls())
sse_mean = 
  function(x) 
    (sd(x, na.rm = TRUE)/sqrt(length(x)))
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

boxplot(
  flipper_length_mm ~ species, data = penguins,
  ylab = "Flipper length (mm)")

#The code below is not what we wanted 
dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")

#This is what we wanted!
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  boxplot(
    flipper_length_mm ~ species, data = dat_pen,
    ylab = "Flipper length (mm)")
}
#resampling with replacement
# for reproducibility
set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}

#bootstrap resampling and alternative hypothesis
penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}

#repeated MC Resampling
par(mfrow = c(4, 4), mar = c(1, 1, 1, 1))
for (i in 1:16)
{
  
  flipper_shuffled = sample(
    penguins$flipper_length_mm, replace = TRUE)
  
  boxplot(
    flipper_shuffled ~ penguins$species,
    ann = F, axes = F)
  box()
  
}

#T-test a frequentisr approach
#classical ttest:adelie and chinstrap penguins
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

#two-sample resampling
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

#classical test on resampled data
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#difference of means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#using aggregate()
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

#sample size
table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

#simulation function
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

#     Question 2 and 3
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

two_group_resample_diff = 
  function(x, n_1, n_2) 
  {
    diff_simulated = 
     mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE) 
    
    diff_observed = diff(agg_means[, 2])
    
    difference_in_means = 
      diff_observed - diff_simulated
     
    return(difference_in_means)
  }

set.seed(54321)
two_group_resample_diff(x, 68, 152)

#     Question 4
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

#     Question 5

sum(abs(mean_differences) >= diff_observed)

#     Question 7
boxplot(dat_pen$bill_length_mm ~ dat_pen$species,
        xlab= "Species",
        ylab= "Bill Length")

#     Question 8
agg_means_2= aggregate(bill_length_mm ~ species,
          data = dat_pen,
          FUN = "mean",
          na.rm = TRUE)
diff_crit = diff(agg_means_2[, 2])
t.test(dat_pen$bill_length_mm ~ dat_pen$species)

two_group_resample_diff(dat_pen$flipper_length_mm, n_1, n_2)

n_3= 38.79
n_4= 48.83
dat_3= sample(x, n_3, replace = TRUE)
dat_4= sample(x, n_4, replace = TRUE)
diff_simulated = 
  mean(dat_3, na.rm = TRUE) - mean(dat_4, na.rm = TRUE)

two_group_resample_diff = 
  function(x, n_3, n_4) 
  {
    diff_simulated = 
      mean(dat_3, na.rm = TRUE) - mean(dat_4, na.rm = TRUE) 
    
    diff_observed = diff(agg_means_2[, 2])
    
    difference_in_means = 
      diff_observed - diff_simulated
    
    return(difference_in_means)
  }

set.seed(54321)
two_group_resample_diff(x, 38.79, 48.83)

set.seed(54321)
hist(mean_differences,
     ylab= "Mean of Differences",
     breaks= 7)

sum(abs(mean_differences) >= diff_observed)
