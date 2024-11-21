

pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflicts_prefer(dplyr::mutate)
#options(scipen = 100, digits = 3)


##### RDM simulation wrapper #####
##################################


#Notes:

#The "domain" is the level at which we simulate fishery outcomes. For the 2025 cod and haddock RDM, the domain is the combination
#of mode (pr/fh) and season (cod open season in y-1/cod closed season in y-1). So there are 4 domains.

#Projection results are based on 100 simulations of the model. In each iteration we pull in new distributions of catch-per-trip,
#directed fishing effort, and angler preferences. I calibrate the model with 150 simulations, some of which are excluded after Step 2.
#From the pool of remaining simulations, I use the first 100 in the projection algorithm.


#Set up globals for input/output data and code scripts

input_data_cd="C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_data/"
code_cd="C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/" 
output_data_cd="C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/output_data/"



####### STEP 1 #####################
#Run the model for the calibration year to determine the difference between model-based harvest and MRIP-based harvest. 
#I do this for each domain and each domain's 150 draws of MRIP trips/catch/harvest (4*150=600 simulations).
#This code retain for each simulation the percent difference between model-based harvest and MRIP-based harvest by species. 

directed_trips_file_path = paste0(input_data_cd, "directed_trips_calib_150draws_cm.csv")
catch_draws_file_path = input_data_cd
MRIP_comparison = paste0(input_data_cd,"simulated_catch_totals_open_season.csv")
size_data_read = read.csv(paste0(input_data_cd,"rec_selectivity_CaL_open_seasons_cm.csv"))

#Files needed:
  #directed_trips_calib_150draws_cm.csv
  #simulated_catch_totals_open_season.csv
  #rec_selectivity_CaL_open_seasons_cm.csv
  #paste0(catch_draws_file_path, k, "_full.feather")), where k indicates draw number


#Scripts needed:
  #calibrate_rec_catch_hstar_code.R

source(paste0(code_cd,"find_harvest_differences1.R"))

#Output files: 
  #MRIP_simulated_data.rds
  #harvest_differences_check.rds

####### END STEP 1 #####################




####### STEP 2 #####################
# Now, run each of the 600 simulations again, but this time allocate discards to harvest (or harvest to discards) until 
# the difference between model-based harvest and MRIP-based harvest is within abs(5%) or <500 fish. 

#Files needed:
  #Discard_Mortality.csv
  #harvest_differences_check.rds


#Scripts needed:
  #calibration_catch_weights.R

source(paste0(code_cd,"calibration_wrapper.R"))

#Output files: 
  #calibration_comparison.rds
  #calibration_catch_weights_cm.xlsx
  #paste0("pds_new_", i,".rds")), where i is an indicator for a domain-draw combination
  #paste0("costs_", i,".rds"))), where i is an indicator for a domain-draw combination


# Note that in some simulations, the difference in harvest between the model and MRIP from Step 1 is too large relative to the number of 
# fish discarded in the model; this means that even if we allocate all discards as harvest, the percent difference in harvest
# between the model and MRIP will not be within abs(5%) or <500 fish. The code in Step 2 identifies and drops these simulations, 
# as well as the other three simulations in that draw of MRIP trips/catch/harvest. 

# For example, draw 1 of 150 consists of four domains: pr_open, pr_closed, fh_open, fh_closed. If the difference in total harvest numbers 
# between the model and MRIP is much greater than the simulated number of discards for pr_closed, then we drop pr_closed; but because this 
# domain is one of four domains that together make up the total output for draw 1, we must also drop pr_open, fh_open, and fh_closed for draw 1. 
# The last time I ran this, I only drop 15 out of 150 draws. 

# This script saves calibration output, as well as the proportion of choice occasions in which we allocate fish discarded as harvest, 
# or vice versa. Note that I have modified the algorithm to only allocate fish discarded as harvest if those fish discarded are 
# longer than or equal to 2 inches shorter than the minimum size limit.  

####### END STEP 2 #####################




####### STEP 3 #####################
# Transfer the output files into .feather format to reduce to computing time associated with pulling data into R. 

#Files needed:
  #paste0("pds_new_", i,".rds")), where i is an indicator for a domain-draw combination
  #paste0("costs_", i,".rds"))), where i is an indicator for a domain-draw combination


#Scripts needed:
#

#source(paste0(code_cd, "cod_haddock/transfer files to feather.R"))

#Output files: 
  #paste0("pds_new_", mode,"_", season, "_", draw, ".feather"))
  #paste0("costs_",mode,"_", season, "_", draw, ".feather"))

####### END STEP 3 #####################


####### STEP 4 #####################
# Run the projection algorithm.This algorithm pulls in population-adjusted catch-at-length distributions and allocates 
# fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration. So for example, if in the 
# Step 2 we find that for draw 10 in the pr_closed domain, we needed to allocated fish discarded as harvest for 40% of choice occasions, 
# then in draw 10 projection of the pr_closed domain, we will also allocate fish discarded as harvest for 40% of the choice occasions. 

#Files needed:
#
#
#
#
#


#Scripts needed:
#
#
#
#
#

source("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/predict_rec_catch_season3.R")

#Output files: 
#
#
#
#
#

####### END STEP 4 #####################








