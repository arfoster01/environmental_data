require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
summary(catrate)

hist(catrate$cat.rate)
shapiro.test(catrate$cat.rate)


t.test(catrate$cat.rate, mu= 2/7)

t.test(catrate$cat.rate, mu= 2/7,
       alernative = "less")
t.test(catrate$cat.rate, mu= 2/7,
       alernative = "greater")

wilcox.test(catrate$cat.rate, mu = 2 / 7)


#     Two sample means
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

t.test(flipper_length_mm ~ species, 
       data = penguin_dat)

wilcox.test(flipper_length_mm ~ species, 
            data = penguin_dat)

levels(penguin_dat$species)

#     Question 1-7
hist(catrate$cat.rate,
     main= "Histogram of Salamander 
     Reproduction Catastrophic Rates",
     xlab= "Reproduction Catastrophic Rates")

shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate, mu= 2/7)

#     Question 11-13

wilcox.test(catrate$cat.rate, mu= 2/7)

#     Question 16-17
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

#     Question 18
par(mfrow=c(1,2))
dev.new(width= 100, height= 50, unit= "in")
hist(dat_adelie$flipper_length_mm,
     main= "Histogram of Penguin Flipper Lengths",
     xlab= "Flipper Length (mm)")
hist(dat_chinstrap$flipper_length_mm,
     main= "Histogram of Penguin Flipper Lengths",
     xlab= "Flipper Length (mm)",
     add= TRUE)

#     Question 19-20

t.test(flipper_length_mm ~ species, 
       data = penguin_dat)















