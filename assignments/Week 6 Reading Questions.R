rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
  pol_predation_rate = pol_n_predation/pol_n_total
  
  psd_n_predation = 25
  psd_n_no_predation = 706
  psd_n_total = 731
  psd_predation_rate = psd_n_predation/psd_n_total
  
  print(
    paste0(
      "The seed predation rate for Polyscias fulva is: ",
      round(pol_predation_rate, digits = 3))) 
  
  print(
    paste0(
      "The seed predation rate for Pseudospondias microcarpa is: ",
      round(psd_predation_rate, digits = 3)))
  
data= matrix(c(pol_n_predation,
             pol_n_no_predation,
             pol_n_total,
             pol_predation_rate, 
             psd_n_predation,
             psd_n_no_predation,
             psd_n_total,
             psd_predation_rate), ncol = 4, byrow= TRUE)
colnames(data)= c("Any Taken", "None Taken", "Total", "N Predation Rate")
rownames(data)= c( "Polyscias fulva (pol)", "Pseudospondias microcarpa (psd)")
  
final=as.table(data)  
final 

ratio_seed_predation= pol_predation_rate/psd_predation_rate





  