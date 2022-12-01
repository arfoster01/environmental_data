require(palmerpenguins)
dat_ade = droplevels(subset(penguins, 
                            species == "Adelie"))

hist(dat_ade$body_mass_g, 
     main = "Adelie Penguins: Body Mass", 
     xlab = "body mass (g)")

boxplot(body_mass_g ~ sex, 
        data = dat_ade,
        main = "Adelie Penguins: Body Mass", 
        xlab = "body mass (g)")


dat_ade_f= subset(dat_ade, sex == "female")

t.test(dat_ade_f$body_mass_g, y = NULL, 
       alternative = c("greater"),
       mu= 0, paired= FALSE, var.equal= FALSE,
       conf.level = 0.95 )

dat_ade_m= subset(dat_ade, sex == "male")
t.test(dat_ade_m$body_mass_g, y = NULL, 
       alternative = c("greater"),
       mu= 4000 )

t.test(dat_ade$body_mass_g ~ dat_ade$sex, 
       alternative = c("two.sided", "less", "greater"))

t.test(dat_ade_m$body_mass_g ~ dat_ade_f$body_mass_g, 
       alternative = c("greater"))



