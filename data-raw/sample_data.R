# sample data pull for vignette
channel <- obdbs::connect_to_database("nova","abeet")

sample_data <- function(){
  data_124 <- obdbs::get_lengths(channel,species="124",year= 1999)
  #save(data_124,file="data/data_124.RData")
  usethis::use_data(data_124)
}


