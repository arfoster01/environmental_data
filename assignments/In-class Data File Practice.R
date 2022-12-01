require(here)
read.csv(here("data", "catrate.csv"))
dat_catrate = data.frame(read.csv(here("data", "catrate.csv")))
read.csv(here("data", "delomys.csv"))
dat_delomys = data.frame(read.csv(here("data", "delomys.csv")))
read.csv(here("data", "rope.csv"))
dat_rope = data.frame(read.csv(here("data", "rope.csv")))


require(here)
dat_catrate = data.frame(read.csv(here("data", "catrate.csv")))
head(dat_catrate)
hist(dat_catrate$success, xlab= "Success",  main= "Success By Abby Foster")