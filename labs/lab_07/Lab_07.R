#lab walkthrough
# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)

apply(dat, MARGIN = 1, FUN = max)

apply(dat, MARGIN = 2, FUN = mean)

#data files
require(here)
moths = read.csv(here("data", "moths.csv"))
head(moths)

#calculating Cl
# Choose significance level
alpha = 0.05

# 2: Calculate sample standard error:
n = sum(!is.na(moths$anst))
sse = sd(moths$anst, na.rm = TRUE) / sqrt(n)

# 3: Calculate critical t-values:
t_crit = abs(qt(alpha / 2, df = n - 1))

# 4: Calculate the CI radius:
ci_radius = sse * t_crit

# The CI is the sample mean +/- the radius:
anst_ci = c(
  lower = mean(moths$anst) - ci_radius,
  upper = mean(moths$anst) + ci_radius)

print(round(anst_ci, 4))

#     Question 1
install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)
penguins = data.frame(penguins)
dat_pen = subset(penguins, species != "Adelie")
dat_pen_2 = subset(dat_pen, species != "Chinstrap")




alpha = 0.05

n = sum(!is.na(dat_pen_2$bill_length_mm))
sse = sd(dat_pen_2$bill_length_mm, na.rm = TRUE) / sqrt(n)

t_crit = abs(qt(alpha / 2, df = n - 1))

ci_radius = sse * t_crit
ci_radius

bill_length_ci = c(
  lower = mean(dat_pen_2$bill_length_mm, na.rm=TRUE) - ci_radius,
  upper = mean(dat_pen_2$bill_length_mm, na.rm=TRUE) + ci_radius)

print(round(bill_length_ci, 4))

#     Question 6-8
install.packages("boot")
require(boot)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = dat_pen_2$bill_length_mm,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)

mean(dat_pen_2$bill_length_mm, na.rm = TRUE)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

quantile(
  myboot$t,
  c(0.05, 0.95))


#     Questions 9-13
moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) 
m = 100 
moth_result = matrix(
  nrow = m,
  ncol = n)

for(i in 1:m)
{
  for(j in 1:n)
  {
    rows_j = sample(n, size = j, replace=TRUE)
    
    t1 = moth_dat[rows_j, ]
    
    t2 = apply(t1, 2, sum)
    
    moth_result[i, j] = sum(t2 > 0)
  }
}
head(moth_result)


rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 1000)
head(rarefact)

#second draft
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 1000)
head(rarefact)


#debugging
#question 9 and 10
rm(list = ls())

moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n = sum(!is.na(row(moth_dat)))
  input_dat= moth_dat
  n_iterations= 100
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
      
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out= sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)

head(rarefact)

#question 11
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main="Abby's Rarefaction Curve")

legend(
  'bottomright',
  legend=c('mean','Lower Interval','Upper Interval'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))



