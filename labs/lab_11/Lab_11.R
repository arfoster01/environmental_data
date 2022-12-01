require(here)
bird = read.csv(here("data", "bird.sub.csv"))
hab = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(bird, hab)

dim(birdhab)

#Brown creeper BRCR, Late successional forest ls

plot(birdhab$BRCR, birdhab$ls,
     xlab = "Late-Succesional Forest Extent",
     ylab = "Brown Creeper Abundance")

fit_1 = lm(BRCR ~ ls, data = birdhab)
summary(fit_1)

plot(birdhab$BRCR, birdhab$ls,
     xlab = "Late-Succesional Forest Extent",
     ylab = "Brown Creeper Abundance",
     abline(fit_1))

# Deterministic model: Linear Function
x <- c(Inf)
y_int <- function(x, y) mean(y) - slope(x, y) * mean(x)
slope <- function(x, y) cov(x, y) / var(x)

linear = function(x, y_int, slope)

linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

