install.packages("psych")

require(psych)
pairs.panels(iris)

names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

require(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
head(dat_bird)

require(here)
dat_habitat = read.csv(here("data", "hab.sta.csv"))
head(dat_habitat)

dat_all = merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)

my_vec = rep(1:3, 5)
my_vec == 3
my_vec>1

sample(dat_all$CEWA, 100)>0

cewa_present_absent = as.numeric(sample(dat_all$CEWA) >0)
plot( x= dat_all$elev, y= cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

names(dat_habitat)
pairs.panels(dat_habitat[, c("slope", "aspect", "elev", "ba.tot")])

names(dat_bird)
home_present_absent = as.numeric(sample(dat_all$HOME)>0)
plot( x= dat_all$ba.tot, y= home_present_absent)
title(main= "HOME Presence and Absence")

coye_present_absent = as.numeric(sample(dat_all$COYE)>0)
plot( x= dat_all$ba.tot, y= coye_present_absent)
title(main= "COYE Presence and Absence")

plot(x = dat_all$ba.tot, y = coye_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
title(main= "CAYE Presence and Absence")

plot(x = dat_all$ba.tot, y = home_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
title(main= "HOME Presence and Absence")

names(dat_bird)
grja_present_absent = as.numeric(sample(dat_bird$GRJA) >0)
sum(grja_present_absent)

names(dat_bird)
grja_present_absent = as.numeric(sample(dat_all$GRJA) >0)
sum(grja_present_absent)

# x <- data.frame(grayjay = as.numeric(sample(dat_all$GRJA) >0))
  
sum("sta", grja_present_absent)
"sta"== sum(grja_present_absent)

total(grja_present_absent)

sum(grja_present_absent = as.numeric(dat_all$GRJA) >0)

sum(sample(dat_all$GRJA) >0)



grja_present_absent = as.numeric(dat_all$GRJA) >0
sum(grja_present_absent)

sum(nrow(grja_present_absent))

sum(sample(dat_all$GRJA))

grja_present_absent = as.numeric(dat_all$GRJA) >0
sum(grja_present_absent)

