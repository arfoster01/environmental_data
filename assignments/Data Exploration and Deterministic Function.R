require("here")
read.csv(here("data", "hab.sta.csv"))
dat_habitat = data.frame(read.csv(here("data", "hab.sta.csv")))
hist_elevation= hist(dat_habitat$elev, xlab= "Elevation", ylab = "Frequency", main= "Histogram of Sampling Site Elevation")

hist_slope= hist(dat_habitat$slope, xlab= "Slope", ylab = "Frequency", main= "Histogram of Sampling Site Slope")

hist_aspect= hist(dat_habitat$aspect, xlab= "Aspect", ylab = "Frequency", main= "Histogram of Sampling Site Aspect")

require(here)
png(
  filename= here("data_exploration_hist_1"),
  width= 1500, height= 1600,
  res= 180, units= "px")
  par(mfrow= c(3, 1))
  hist_elevation= hist(dat_habitat$elev, xlab= "Elevation", ylab = "Frequency", main= "Histogram of Sampling Site Elevation")
  hist_slope= hist(dat_habitat$slope, xlab= "Slope", ylab = "Frequency", main= "Histogram of Sampling Site Slope")
  hist_aspect= hist(dat_habitat$aspect, xlab= "Aspect", ylab = "Frequency", main= "Histogram of Sampling Site Aspect")
  dev.off()


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



require(here)
png(
  filename= here("data_exploration_scatter_2"),
  width= 1500, height= 1600, res= 180,
  units= "px")
par(mfrow=c(3,1))
data_center_x_elev= mean(dat_habitat$elev)
data_center_y= mean(dat_habitat$ba.tot)

plot(x= dat_habitat$elev, y= dat_habitat$ba.tot, 
        main= "Basal Area and Elevation", 
         xlab= "Elevation", 
         ylab= "Total Basal Area",
        col= "blue")
points(x= data_center_x_elev, y= data_center_y, col= "black")
curve(
   line_point_slope(x, data_center_x, data_center_y, slope= 0.01), add= TRUE)

data_center_x_slope= mean(dat_habitat$slope)
data_center_y= mean(dat_habitat$ba.tot)

plot(dat_habitat$slope, dat_habitat$ba.tot, 
     main= "Basal Area and Slope", 
     xlab= "Slope", 
     ylab= "Total Basal Area",
     col= "Red") 
points(x= data_center_x_slope, y= data_center_y, col= "black")
curve(
  line_point_slope(x, data_center_x, data_center_y, slope= 0.001), add= TRUE)

data_center_x_aspect= mean(dat_habitat$aspect)
data_center_y= mean(dat_habitat$ba.tot)

plot(dat_habitat$aspect, dat_habitat$ba.tot, 
     main= "Basal Area and Aspect", 
     xlab= "Aspect", 
     ylab= "Total Basal Area",
     col= "green")
points(x= data_center_x_aspect, y= data_center_y, col= "black")
curve(
  line_point_slope(x, data_center_x, data_center_y, slope= 0.0008), add= TRUE)
dev.off()

  
  