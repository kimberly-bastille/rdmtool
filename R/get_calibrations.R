## get_calibrations
catch_files_CT<- readRDS(here::here(paste0("data-raw/catch/catch_files_CT.rds")))
catch_files_DE<- readRDS(here::here(paste0("data-raw/catch/catch_files_DE.rds")))
catch_files_MA<- readRDS(here::here(paste0("data-raw/catch/catch_files_MA.rds")))
catch_files_MD<- readRDS(here::here(paste0("data-raw/catch/catch_files_MD.rds")))
catch_files_NJ<- readRDS(here::here(paste0("data-raw/catch/catch_files_NJ.rds")))
catch_files_NY<- readRDS(here::here(paste0("data-raw/catch/catch_files_NY.rds")))
catch_files_RI<- readRDS(here::here(paste0("data-raw/catch/catch_files_RI.rds")))
catch_files_VA<- readRDS(here::here(paste0("data-raw/catch/catch_files_VA.rds")))

#### Connecticut
for(k in 1:100){
  calibration<- calibrate_rec_catch("CT", 9, 
                                    catch_files_CT, p_star_sf_CT_variable,
                                    p_star_bsb_CT_variable, p_star_scup_CT_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_CT_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_CT_",k,".rds"))
}

#### Deleware
for(k in 1:100){
  calibration<- calibrate_rec_catch("DE", 10,
                                    catch_files_DE, p_star_sf_DE_variable,
                                    p_star_bsb_DE_variable, p_star_scup_DE_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_DE_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_DE_",k,".rds"))
}

#### Massachusetts
for(k in 1:100){
  calibration<- calibrate_rec_catch("MA", 25, 
                                    catch_files_MA, p_star_sf_MA_variable,
                                    p_star_bsb_MA_variable, p_star_scup_MA_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_MA_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_MA_",k,".rds")) 
}

#### Maryland
for(k in 1:100){
  calibration<- calibrate_rec_catch("MD", 24, 
                                    catch_files_MD, p_star_sf_MD_variable,
                                    p_star_bsb_MD_variable, p_star_scup_MD_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_MD_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_MD_",k,".rds"))
}

#### New Jersey

p_star_sf_NJ_variable_fh<- 0.734
p_star_bsb_NJ_variable_fh<-0.442
p_star_scup_NJ_variable_fh<-0.427
p_star_sf_NJ_variable_pr<-0.819
p_star_bsb_NJ_variable_pr<-0.715
p_star_scup_NJ_variable_pr<-0.503
p_star_sf_NJ_variable_sh<-0.927
p_star_bsb_NJ_variable_sh<-1
p_star_scup_NJ_variable_sh<-1

# m = "fh"
# 
# if(m == "sh"){
#   p_star_bsb <- p_star_bsb_NJ_variable_sh
#   p_star_sf <- p_star_sf_NJ_variable_sh
#   p_star_scup <- p_star_scup_NJ_variable_sh
# } 
# if(m == "fh"){
#   p_star_bsb <- p_star_bsb_NJ_variable_fh
#   p_star_sf <- p_star_sf_NJ_variable_fh
#   p_star_scup <- p_star_scup_NJ_variable_fh
# } 
# if(m == "pr"){
#   p_star_bsb <- p_star_bsb_NJ_variable_pr
#   p_star_sf <- p_star_sf_NJ_variable_pr
#   p_star_scup <- p_star_scup_NJ_variable_pr
# }


for(k in 1:5){
  calibration_fh<- calibrate_rec_catch("NJ", 34, 
                                       p_star_sf_NJ_variable_fh,
                                       p_star_bsb_NJ_variable_fh,
                                       p_star_scup_NJ_variable_fh, 
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])
  
  calibration_pr<- calibrate_rec_catch("NJ", 34, 
                                       p_star_sf_NJ_variable_pr,
                                       p_star_bsb_NJ_variable_pr,
                                       p_star_scup_NJ_variable_pr, 
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])
  
  
  calibration_sh<- calibrate_rec_catch("NJ", 34, 
                                       p_star_sf_NJ_variable_sh,
                                       p_star_bsb_NJ_variable_sh,
                                       p_star_scup_NJ_variable_sh, 
                                       "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])
  
  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh)
  
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NJ_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NJ_",k,"_test.rds"))
}

#### New York
for(k in 1:100){
  calibration<- calibrate_rec_catch("NY", 36, 
                                    catch_files_NY, p_star_sf_NY_variable,
                                    p_star_bsb_NY_variable, p_star_scup_NY_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NY_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NY_",k,".rds"))
}

#### Rhode Island
for(k in 1:100){
  calibration<- calibrate_rec_catch("RI", 44, 
                                    catch_files_RI, p_star_sf_RI_variable,
                                    p_star_bsb_RI_variable, p_star_scup_RI_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_RI_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_RI_",k,".rds"))
}



#### Virginia
for(k in 1:100){
  calibration<- calibrate_rec_catch("VA", 51, 
                                    catch_files_VA, p_star_sf_VA_variable,
                                    p_star_bsb_VA_variable, p_star_scup_VA_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_VA_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_VA_",k,".rds"))
}
