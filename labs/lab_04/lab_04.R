# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

 
pop_sd = 2.4
pop_mean = 10.4
  
norm_17 = rnorm(n= 17, pop_mean, pop_sd)
norm_30 = rnorm(n=30, pop_mean, pop_sd)
norm_300 = rnorm(n=300, pop_mean, pop_sd)
norm_3000 = rnorm(n=3000, pop_mean, pop_sd)

par(mfrow = c(2, 2))
hist(norm_17)
hist(norm_30)
hist(norm_300)
hist(norm_3000)

require(here)
png(
  filename = here("lab_04_hist_01.png"),
    width= 1500, height= 1600,
    res= 180, units= "px")

par(mfrow = c(2, 2))
hist(norm_17)
hist(norm_30)
hist(norm_300)
hist(norm_3000)
dev.off()

dnorm?

#question 7
  
  # Generate a vector of x-values
  
  require(here)
png(
  filename = here("norm_1.png"),
  width= 1500, height= 1600,
  res= 180, units= "px")  
x = seq(3, 18, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4, log = FALSE)

plot(x, y, main = "Normal PDF: mean= 10.4 sd= 2.4", type = "l", xlim = c(3,18))
abline(h = 0)
dev.off()







# Question 9

set.seed(111)
n_pts = 100
x_min = 1
x_max = 20

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 10)
mean_dat_random= mean(dat_random$x)


require(here)
png(
  filename = here("lab_04_Q_10.png"),
  width= 1500, height= 1600,
  res= 180, units= "px")
par(mfrow = c(2, 2))
hist(dat_random$x, 
     main= "Histogram of Random Dataset",
     col= "steelblue")
plot(dat_random$y, 
               main= "Scatter Plot of Random Dataset",
               col= "pink")
boxplot(dat_random$x, 
        main= "Boxplot of Random Dataset 1",
        col= "violet")
boxplot(dat_random$y, 
        main= "Boxplot of Random Dataset 2",
        col= "orange")
dev.off()

#question 10

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

data_center_x= mean(dat_random$x)
data_center_y= mean(dat_random$y)

plot(dat_random$y, 
     main= "Scatter Plot of Random Dataset",
     col= "pink")
points(x= data_center_x, y= data_center_y, col= "black")
curve(
  line_point_slope(x, data_center_x, data_center_y, slope= 0.01), add= TRUE)

#question 11

require(here)
png(
  filename = here("lab_04_Q_12.png"),
  width= 1500, height= 1600,
  res= 180, units= "px")
par(mfrow = c(2, 2))
hist(dat_random$x, 
     main= "Histogram of Random Dataset",
     col= "steelblue")
boxplot(dat_random$x, 
        main= "Boxplot of Random Dataset 1",
        col= "violet")
boxplot(dat_random$y, 
        main= "Boxplot of Random Dataset 2",
        col= "orange")
data_center_x= mean(dat_random$x)
data_center_y= mean(dat_random$y)

plot(dat_random$y, 
     main= "Scatter Plot of Random Dataset",
     col= "pink")
points(x= data_center_x, y= data_center_y, col= "black")
curve(
  line_point_slope(x, data_center_x, data_center_y, slope= 0.01), add= TRUE)

dev.off()

#question 13 y predicted

data_center_x= mean(dat_random$x)
data_center_y= mean(dat_random$y)

plot(dat_random$y, 
     main= "Scatter Plot of Random Dataset",
     col= "pink")
points(x= data_center_x, y= data_center_y, col= "black")
curve(
  line_point_slope(x, data_center_x, data_center_y, slope= 0.01), add= TRUE)

data_center_x= mean(dat_random$x)
data_center_y= mean(dat_random$y)

plot(dat_random$y, 
     main= "Scatter Plot of Random Dataset",
     col= "pink")
points(x= data_center_x, y= data_center_y, col= "black")
curve(
  line_point_slope(x, data_center_x, data_center_y, slope= 0.01), add= TRUE)

guess_x= 10
guess_y= 0.1
guess_slope= 0.1

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
  
dat_random$y_predicted= c(line_point_slope(dat_random$x, guess_x, guess_y, guess_slope))

# question 13 residuals

dat_random$resids= dat_random$y- dat_random$y_predicted

#question 14

require(here)
png(
  filename = here("lab_04_Q_14.png"),
  width= 1500, height= 1600,
  res= 180, units= "px")
par(mfrow = c(2, 1))
hist(dat_random$resids, 
     main= "Histogram of the Residual Values",
     xlab= "Residuals",
     col= "navy")

plot(dat_random$y_predicted, dat_random$resids,
     main= "Scatterplot of Predicted vs Residual Values",
     xlab= "Predicted Values",
     ylab= "Residual Values",
     col="magenta")
dev.off()

