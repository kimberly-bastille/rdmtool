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
  calibration<- calibrate_rec_catch("CT", 9, directed_trips_table_base[[1]],
                                    catch_files_CT, p_star_sf_CT_variable,
                                    p_star_bsb_CT_variable, p_star_scup_CT_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_CT_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_CT_",k,".rds"))
}

#### Deleware
for(k in 1:100){
  calibration<- calibrate_rec_catch("DE", 10, directed_trips_table_base[[2]],
                                    catch_files_DE, p_star_sf_DE_variable,
                                    p_star_bsb_DE_variable, p_star_scup_DE_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_DE_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_DE_",k,".rds"))
}

#### Massachusetts
for(k in 1:100){
  calibration<- calibrate_rec_catch("MA", 25, directed_trips_table_base[[3]],
                                    catch_files_MA, p_star_sf_MA_variable,
                                    p_star_bsb_MA_variable, p_star_scup_MA_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_MA_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_MA_",k,".rds")) 
}

#### Maryland
for(k in 1:100){
  calibration<- calibrate_rec_catch("MD", 24, directed_trips_table_base[[4]],
                                    catch_files_MD, p_star_sf_MD_variable,
                                    p_star_bsb_MD_variable, p_star_scup_MD_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_MD_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_MD_",k,".rds"))
}

#### New Jersey
for(k in 1:1){
  calibration<- calibrate_rec_catch("NJ", 34, directed_trips_table_base[[5]],
                                    catch_files_NJ, p_star_sf_NJ_variable,
                                    p_star_bsb_NJ_variable, p_star_scup_NJ_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NJ_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NJ_",k,".rds"))
}

#### New York
for(k in 1:100){
  calibration<- calibrate_rec_catch("NY", 36, directed_trips_table_base[[6]],
                                    catch_files_NY, p_star_sf_NY_variable,
                                    p_star_bsb_NY_variable, p_star_scup_NY_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NY_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NY_",k,".rds"))
}

#### Rhode Island
for(k in 1:100){
  calibration<- calibrate_rec_catch("RI", 44, directed_trips_table_base[[7]],
                                    catch_files_RI, p_star_sf_RI_variable,
                                    p_star_bsb_RI_variable, p_star_scup_RI_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_RI_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_RI_",k,".rds"))
}



#### Virginia
for(k in 1:100){
  calibration<- calibrate_rec_catch("VA", 51, directed_trips_table_base[[8]],
                                    catch_files_VA, p_star_sf_VA_variable,
                                    p_star_bsb_VA_variable, p_star_scup_VA_variable, k)
  pds_new_all<- data.table::data.table(calibration[1][[1]])
  costs_new_all<-  data.table::data.table(calibration[2][[1]])
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_VA_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_VA_",k,".rds"))
}
