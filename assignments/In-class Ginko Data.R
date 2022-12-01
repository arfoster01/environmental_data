require(here)
dat_ginkgo = read.csv(here("data", "ginkgo_data_2022.csv"))

seed_site = subset(
    dat_ginkgo, select = 
      c("seeds_present", "site_id") 
  )

trees = length(unique(seed_site$site_id))



boxplot(seed_site$seeds_present)


plot(dat_ginkgo$max_depth, dat_ginkgo$max_width,
     xlab = "Max Depth",
     ylab= "Max Width",
     main= "Max Leaf Depth and Width")
        
