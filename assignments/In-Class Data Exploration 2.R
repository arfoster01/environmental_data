
require(here)
dat_bird = data.frame(read.csv(here("data", "bird.sta.csv")))
head(dat_bird_sta)
dat_habitat = data.frame(read.csv(here("data", "hab.sta.csv")))
head(dat_hab_sta)

pairs(dat_habitat[,c("lat", "long", "aspect")])
hist(dat_bird$CBCH, xlab= "Number of Birds Counted", breaks= 0:7-0.5, main= "CBCH Histogram")