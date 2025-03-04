

pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflicts_prefer(dplyr::mutate)


#There are four folders needed: 
 #input data - contains all the MRIP, biological data, angler characteristics data, as well as some data generated in the simulation
 #code - contains all the model code
 #output_data - this folder is empty to begin with. It stores final simulation output
 #iterative_data -this folder is empty to begin with. It compiles data generated in the simulation

#Need to ensure that the globals below are set up in both this file and the stata model_wrapper.do file. 


#Set up R globals for input/output data and code scripts
input_data_cd="C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_data/"
code_cd="C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/"  
output_data_cd="C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/output_data/" 
iterative_input_data_cd= "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_data/iterative_data/" 


###################################################
###############Pre-sim Stata code##################
###################################################

#Stata code extracts and prepares the data needed for the simulation

#Connect Rstudio to Stata
options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

#Set number of original draws. We use 150 for the final run. Choose a lot fewer for test runs
n_simulations<-2

#First, open "$code_cd\model wrapper.do" and set globals:
  #a) data years for different datasets
  #b) number of draws (ndraws), which should be the same as the object n_simulations above
  #c) cd's

#Second, open "$code_cd\set regulations.do" and set regulations for the calibration and projection period.

#Third, run the model wrapper code below:
stata('do "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/model wrapper.do"')

###################################################




###################################################
###############Simulation R code###################
###################################################

#Notes:

#Simulation stratum are the groups in which we allocate and simulate choice occasions.
#For the 2025 cod and haddock RDM, the stratum is the combination of mode (pr/fh) and 
#season (cod open season in y-1/cod closed season in y-1). So there are 4 strata.

#Projection results are based on 100 iterations of the model. In each iteration we pull 
#in new distributions of catch-per-trip, directed fishing effort, projected catch-at-length, 
#and angler preferences. I calibrate the model with 150 iterations, some of which are 
#excluded after Step 2. From the pool of remaining iterations, I use the first 100 in the projection.

#Prior to running the model, transfer the catch_draw files from .csv to .feather to reduce computing time
for(i in 1:n_simulations){
  catch<-read.csv(paste0(iterative_input_data_cd, "catch_draws", i,"_full.csv"))
  write_feather(catch, paste0(iterative_input_data_cd, "catch_draws", i,"_full.feather"))
}

dtrip<-read.csv(paste0(input_data_cd, "directed_trips_calib_150draws_cm.csv"))
write_feather(dtrip, paste0(input_data_cd, "directed_trips_calib_150draws_cm.feather"))

##################### STEP 1 #####################
#Run the calibration algorithm to determine the difference between model-based harvest and MRIP-based harvest. 
#I do this for each stratum and each stratum's 150 draws of MRIP trips/catch/harvest (4*150=600 iterations).
#This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species. 

directed_trips_file_path = paste0(input_data_cd, "directed_trips_calib_150draws_cm.feather")
catch_draws_file_path = iterative_input_data_cd
MRIP_comparison = paste0(input_data_cd,"simulated_catch_totals_open_season.csv")
size_data_read = read.csv(paste0(input_data_cd,"rec_selectivity_CaL_open_seasons_cm.csv"))

#Files needed:
  #directed_trips_calib_150draws_cm.csv
  #simulated_catch_totals_open_season.csv
  #rec_selectivity_CaL_open_seasons_cm.csv
  #paste0(catch_draws_file_path, k, "_full.feather")), where k indicates draw (1-150) number

#Scripts needed:
  #calibrate_rec_catch_hstar_code.R

source(paste0(code_cd,"find_harvest_differences1.R"))

#Output files: 
  #MRIP_simulated_data.rds
  #harvest_differences_check.rds




##################### STEP 2 #####################
#Now, run each stratum's calibration simulation again, but this time allocate discards to harvest, 
#or harvest to discards, until the difference between model-based harvest and MRIP-based harvest 
#is within abs(5%) or <500 fish. 

#If a reallocation of discards to harvest is needed, I select h* percent of choice occasions with 
#positive discards, and allocate all fish that are between [(min. size - 2 inches), min.size] as harvest. 

#If a reallocation of harvest to discards is needed, I select h* percent of choice occasions with 
#positive harvest, and allocate all those fish as discards

#Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 is too large relative to the number of 
#fish discarded in the model; in other words, even if we allocate all discards as harvest, the percent difference in harvest
#between the model and MRIP will not be within abs(5%) or <500 fish. The code in Step 2 identifies and drops these iterations.
#For example, draw 1 of 150 consists of four strata: pr_open, pr_closed, fh_open, fh_closed. If the difference in total harvest numbers 
#between the model and MRIP is too much greater than the simulated number of discards for pr_closed, then we drop pr_closed; but because this 
#stratum is one of four strata that together make up the total fishing year for draw 1, we must also drop pr_open, fh_open, and fh_closed for draw 1. 

#This script saves calibration output, as well as the proportion of choice occasions in which we reallocate discards as harvest, 
#or vice versa. 

#Files needed:
  #Discard_Mortality.csv
  #harvest_differences_check.rds (from step 1)


#Scripts needed:
  #calibration_catch_weights.R - can be commented out to save time if calibration catch weight are not needed.

source(paste0(code_cd,"calibration_wrapper.R"))

#Output files: 
  #calibration_comparison.rds
  #calibration_catch_weights_cm.xlsx
  #paste0("pds_new_", i,".rds")), where i is an indicator for a domain-draw combination
  #paste0("costs_", i,".rds"))), where i is an indicator for a domain-draw combination




##################### STEP 3 #####################
#Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates 
#fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration. So for example, if in the 
#Step 2 we find that for draw 10 in the pr_closed domain, we needed to allocated fish discarded as harvest for 40% of choice occasions, 
#then in draw 10 projection of the pr_closed domain, we will also allocate fish discarded as harvest for 40% of the choice occasions. 

#New files needed:
  #projected_CaL_cod_hadd_cm.csv - stock-adjusted catch-at-length
  #calibration_comparison.rds - gives the proportions of trips to allocate harvest as release or vice versa
  #costs_MODE_SEASON.feather - gives baseline variables and baseline catch levels. These are held constant in the projections. 

source(paste0(code_cd, "predict_rec_catch_season3_new1.R"))

#Save the output
write_xlsx(output2, paste0(output_data_cd, "model_results.xlsx"))

#Output files: 
# RDM_predictions.xlsx - output by mode, season, and draw









