ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

# trying to build a ricker function
curve(
  ricker_fun(-x, 1, 0.5),
  from= 0, to = 5, add= FALSE,
  main= "Ricker Function a = 1, b = 0.5",
  ylab= "f(x)", xlab= "x"
)

#Self test
exp_fun= function(x, a, b)
exp_fun(x, 0.3, 1/15)
  
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

#normal errors 1
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, 
     main = "Normally Distributed Errors\n Constant Variance", 
     xlab = "", 
     ylab = "")

#normal errors 2
error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, 
     main = "Normally Distributed Errors\n Constant Variance", 
     xlab = "", ylab = "")
plot(x_sim, y_observed_2, 
     main = "Normally Distributed Errors\n Increasing Variance", 
     xlab = "", ylab = "")

# downloading the data
require(here)
dat_dispersal = read.csv(here("data", "dispersal.csv"))
head(dat_dispersal)

# question 1

exp_fun_2= function(x, a, b)
{
  return(a * x * exp(-b * x))
}

ef_1= curve(
  exp_fun_2(x, -1, 1/5),
  from= 0, to = 5, add= FALSE,
  col= "red")

ef_2= curve(
    exp_fun_2(x, -1, 1/8),
    from= 0, to = 5, add= FALSE,
    col= "blue")

ef_3= curve(
  exp_fun_2(x, -1, 1/4),
  from= 0, to = 5, add= FALSE,
  col= "pink")

ef_4= curve(
  exp_fun_2(x, -1, 1/4),
  from= 0, to = 5, add= FALSE,
  col= "orange",
  lty= "dashed")

ef_5= curve(
  exp_fun_2(x, -1, 1/3),
  from= 0, to = 5, add= FALSE,
  col= "violet",
  lty= "dotted")

plot(ef_1, main= "Exponential Functions", 
     ylab= "f(x)",
     xlab= "x",
     col= "red")
lines(ef_2, main= "Exponential Functions", 
      ylab= "f(x)",
      xlab= "x",
      col= "blue")
lines(ef_3, main= "Exponential Functions", 
      ylab= "f(x)",
      xlab= "x",
      col= "pink")
lines(ef_4,
      main= "Exponential Functions", 
      ylab= "f(x)",
      xlab= "x",
      col= "orange",
      lty= "dashed")
lines(ef_5,
      main= "Exponential Functions", 
      ylab= "f(x)",
      xlab= "x",
      col= "violet",
      lty= "dotted"
      )

#question 2
exp_f_1= curve(
  exp_fun_2(-x, 1.9, 0.1),
  from= 0, to = 5,
  add = FALSE,
  col= "black",
  lty= "solid",
  main= "Exponential Functions 2",
  ylab="f(x)",
  xlab= "x")
exp_f_2= curve(
  exp_fun_2(-x, 1.9, 0.3),
  from= 0, to = 5,
  add = FALSE,
  col= "black",
  lty= "dotted")
exp_f_3= curve(
  exp_fun_2(-x, 1.2, 0.2),
  from= 0, to = 5,
  add = FALSE,
  col= "red",
  lty= "solid")
exp_f_4= curve(
  exp_fun_2(-x, 1.2, 0.4),
  from= 0, to = 5,
  add = FALSE,
  col= "red",
  lty= "dotted")

plot(exp_f_1, main= "Exponential Functions 2",
     ylab="f(x)",
     xlab= "x",
     col= "black",
     lty= "solid")
lines(exp_f_2, main= "Exponential Functions 2",
      ylab="f(x)",
      xlab= "x",
      col= "black",
      lty= "dotted")
lines(exp_f_3, main= "Exponential Functions 2",
      ylab="f(x)",
      xlab= "x",
      col= "red",
      lty= "solid")
lines(exp_f_4, main= "Exponential Functions 2",
      ylab="f(x)",
      xlab= "x",
      col= "red",
      lty= "dotted")

#testing for question 3 and 4
curve(
  exp_fun_2(x, 1.9, 2),
  from= 0, to = 5,
  add = FALSE,
  col= "black",
  lty= "solid")

#question 5
rick_fun_1= curve(
  ricker_fun(x, 25, 0.2),
  from= 0, to = 30,
  col= "black",
  lty= "solid",
  main= "Ricker Functions",
  ylab="f(x)",
  xlab= "x")
rick_fun_2= curve(
  ricker_fun(x, 20, 0.2),
  from= 0, to = 30, 
  col= "black",
  lty= "dotted")
rick_fun_3= curve(
  ricker_fun(x, 10, 0.2),
  from= 0, to = 30, 
  col= "black",
  lty= "dotted")
rick_fun_4= curve(
  ricker_fun(x, 75, 0.3),
  from= 0, to = 30, 
  col= "red",
  lty= "solid")
rick_fun_5= curve(
  ricker_fun(x, 50, 0.3),
  from= 0, to = 30, 
  col= "red",
  lty= "dotted")
rick_fun_6= curve(
  ricker_fun(x, 40, 0.3),
  from= 0, to = 30, 
  col= "red",
  lty= "dotted")


plot(rick_fun_1, main= "Ricker Functions",
     ylab="f(x)",
     xlab= "x",
     col="black",
     lty= "solid")
lines(rick_fun_2, main= "Ricker Functions",
      ylab="f(x)",
      xlab= "x",
      col= "black",
      lty= "dotted")
lines(rick_fun_3, main= "Ricker Functions",
      ylab="f(x)",
      xlab= "x",
      col= "black",
      lty= "dotted")
lines(rick_fun_4, main= "Ricker Functions",
      ylab="f(x)",
      xlab= "x",
      col= "red",
      lty= "solid")
lines(rick_fun_5, main= "Ricker Functions",
      ylab="f(x)",
      xlab= "x",
      col= "red",
      lty= "dotted")
lines(rick_fun_6, main= "Ricker Functions",
      ylab="f(x)",
      xlab= "x",
      col= "red",
      lty= "dotted")

#question 6, 7 and 8
rick_fun_test= curve(
  ricker_fun(x, 40, 0.9),
  from= 0, to = 30, 
  col= "red",
  lty= "dotted",
  ylab= "f(x)",
  xlab = "x")

#question 9
require(here)
dat_dispersal= read.csv(here("data", "dispersal.csv"))

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

locator(1)

#question 11

exp_fun_sal= curve(
  exp_fun_2(x, 4, 0.0092),
  from= 0, to = 1500,
  add = FALSE,
  col= "black",
  lty= "solid")

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders exponential function model")
lines(exp_fun_sal, col= "black", lty= "solid")



#question 12

rick_fun_sal= curve(
  ricker_fun(x, 0.92, 0.01), 
  from = 0, to = 1500, add = FALSE)

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders ricker function model")
lines( rick_fun_sal, col= "black", lty= "solid")


#question 14
model_1 <- lm(dat_dispersal$disp.rate.ftb~dat_dispersal$dist.class, data = rick_fun_sal)
summary(model_1)
lm(dat_dispersal$disp.rate.ftb~dat_dispersal$dist.class, data = rick_fun_sal)

resids_ricker<-rstandard(model_1)
resids_ricker

model_2 <- lm(dat_dispersal$disp.rate.ftb~dat_dispersal$dist.class, data = exp_fun_sal)
summary(model_2)
lm(dat_dispersal$disp.rate.ftb~dat_dispersal$dist.class, data = exp_fun_sal)

resids_exp<- rstandard(model_2)
resids_exp

model_3<- lm(dat_dispersal$disp.rate.ftb~dat_dispersal$dist.class, data =dat_dispersal)
summary(model_1)
lm(dat_dispersal$disp.rate.ftb~dat_dispersal$dist.class, data =dat_dispersal)

resids_linear<- rstandard(model_3)
resids_linear

#question 15
hist(resids_ricker, xlab= "Residuals")
hist(resids_exp, xlab= "Residuals")
hist(resids_linear, xlab= "Residuals")


