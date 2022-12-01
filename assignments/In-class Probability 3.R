#question 2
x_bin= 0:6
y_bin_2= dbinom(x_bin, size=6, prob= 0.66)

barplot(
  height = y_bin_2,
  names.arg = x_bin,
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 6, p = 0.66")
)

#question 3
x_bin= 0:6
y_bin_2= dbinom(x_bin, size=6, prob= 0)
barplot(
  height = y_bin_2,
  names.arg = x_bin,
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 6, p = 0.66")

pbinom(4, 6, 0.67, lower.tail= FALSE)

x_bin = 0:6
y_bin_2 = dbinom(x_bin, size = 6, prob = 0.67)


#question 7
pnorm(1, 0, 1 , lower.tail=TRUE)

#question 8
pnorm(2, 0, 1, lower.tail=TRUE)- pnorm(1, 0, 1, lower.tail=TRUE)

# question 9
# How many points?
n = 13

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l")
# How many points?
n = 1000

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l", ylab = "Probability Density")
y_2 = dnorm(x, mean = 0, sd = 2)
plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)
z_3 = dnorm(x, mean = -2, sd = 1)
plot(z ~ x, type = "l", ylab = "Probability Density")
points(z_3 ~ x, type = "l", lty  = 3)
