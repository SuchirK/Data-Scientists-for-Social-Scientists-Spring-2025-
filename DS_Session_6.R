library(tidyverse)

pc11_vd_shrid = read.csv("pc11_vd_clean_shrid.csv")

shrid_location = read.csv("shrid_loc_names.csv")

##add location to pc11 data 

pc11_vd_shrid_with_location = left_join(shrid_location, pc11_vd_shrid, by = 'shrid2')

