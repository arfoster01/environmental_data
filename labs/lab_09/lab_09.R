require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

#reproductive success and failure
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

#reproductive catastrophe and late filling
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate)

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

#F distribution example
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

#variance test
veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

veg2$treatment = factor(veg2$treatment)

var.test(
  pine ~ treatment,
  data = veg2)

#F tests assumes normality
shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])

#non parametric variance test
fligner.test(
  pine ~ treatment,
  data = veg2)

#tests for multiple variances
bartlett.test(pine ~ treatment, data = veg)
fligner.test(pine ~ treatment, data = veg)

#ttest
t.test(
  pine ~ treatment,
  data = veg2)

#wilcox test
wilcox.test(
  pine ~ treatment,
  data = veg2)

#test for paired samples
install.packages("datarium")

require(datarium)
data("mice2")
head(mice2)

t.test(mice2$before, mice2$after, paired = TRUE)

t.test(mice2$before, mice2$after, paired = FALSE)

#marbled salamander
disp = read.csv(here("data", "dispersal.csv"))
disp

plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue"
)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time Breeders: ECDF")

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1, 3),
  legend = c("first-time", "experienced"),
  title = "Breeder Class")

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

prop.test(
  x = c(4,16),
  n = c(40,250))

#contingency:chi-square test
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls

#chi-square expected and observed values
round(chisq_owls$expected, 1)
chisq_owls$observed

#chisquare residuals
round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)

#fisher exact test
fisher.test(owls)

#bird habitat data
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

#     Question 1 and 2

rownames(br_creeper_table) = c("E", "I")
colnames(br_creeper_table) = c("TRUE", "FALSE")
chisq_br_creeper = chisq.test(br_creeper_table)
chisq_br_creeper

#     Question 3-5
require(palmerpenguins)
palmerpenguins::penguins

fit_species = lm(
  formula = body_mass_g ~ species, data = penguins
)
fit_species

fit_sex = lm(
  formula = body_mass_g ~ sex, data = penguins
)
fit_sex

fit_both = lm(
  formula = body_mass_g ~ species*sex, data = penguins
)
fit_both

#     Questions 6-9
boxplot(
  formula = body_mass_g ~ species, data = penguins,
  main = "Body Mass and Species Conditional Boxplot",
  ylab = "Body Mass (g)",
  xlab = "Species",
  names = c("Adelie", "Chinstrap", "Gentoo")
  )

boxplot(
  formula = body_mass_g ~ sex, data = penguins,
  main = "Body Mass and Sex Conditional Boxplot",
  ylab = "Body Mass (g)",
  xlab = "Sex",
  names = c("Female", "Male")
)


boxplot(
  formula = body_mass_g ~ species*sex, data = penguins,
  main = "Double Conditional Boxplot",
  ylab = "Body Mass (g)",
  xlab = " ",
  names = c("Female 
            Adelie", "Male
            Adelie", "Female 
            Chinstrap", "Male 
            Chinstrap",
            "Female 
            Gentoo", "Male 
            Gentoo"),
  las = 2
)

#     Question 10-12
bartlett.test(body_mass_g ~ species, data = penguins)
bartlett.test(body_mass_g ~ sex, data = penguins)

#     Question 13-14
dat_groups_species = aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = c)
str(dat_groups_species)

dat_groups_sex = aggregate(
  body_mass_g ~ sex,
  data = penguins,
  FUN = c)
str(dat_groups_sex)

bartlett.test(dat_groups_species$body_mass_g)
bartlett.test(dat_groups_sex$body_mass_g)

#     Question 15
require(here)
dat_fl = read.csv(here("data", "trees_FL.csv"))

fit_failure = lm(
  formula = ProbabilityofFailure ~ Species, data = dat_fl
)
fit_failure

table_PF = table(dat_fl$ProbabilityofFailure)
barplot(table_PF,
        main = "Probability of Failure",
        ylab= "Frequency",
        xlab = "Probability of Failure Class")

table_FS = table(dat_fl$Failure_Standardized)
barplot(table_FS, main = "Failure Standardized",
        ylab = "Frequency",
        xlab = "Failure Class")


hist(dat_fl$DBH_in, main = "Histogram of DBH",
     xlab = "DBH (inches)")

plot(dat_fl$DBH_in, dat_fl$HeighttoTop_ft,
     main= "DBH andd Tree Height",
     xlab= "DBH (inches)",
     ylab = "Tree Height (feet)")

setup = subset(dat_fl, Failure_Standardized != "none")
setup_2 = subset(dat_fl, Failure_Standardized != "branch")

dat_fl_Whole= subset(setup, Failure_Standardized != "branch")
dat_fl_Branch = subset(setup, Failure_Standardized != "whole")
dat_fl_None = subset(setup_2, Failure_Standardized != "whole")

ks.test(dat_fl_Whole$Failure_Standardized, dat_fl$DBH_in)
ks.test(dat_fl_Branch$Failure_Standardized, dat_fl$DBH_in)
ks.test(dat_fl_None$Failure_Standardized, dat_fl$DBH_in)


ks.test(table_FS, dat_fl$DBH_in)

ks.test(dat_fl$DBH_in, dat_fl$HeighttoTop_ft)
cor.test(
  dat_fl$DBH_in,
  dat_fl$HeighttoTop_ft,
  use='complete.obs')


#     Question 21-25
dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2

chisq_fl = chisq.test(fl_table_2)

round(chisq_fl$expected, 1)
chisq_fl$observed

round(chisq_fl$observed - chisq_fl$expected,
      digits = 1)





