## get_calibrations

### Connecticut
for(k in 1:1){
  calibration_fh<- calibrate_rec_catch("CT", 9,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k = k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("CT", 9,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("CT", 9,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_CT_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_CT_",k,"_test.rds"))
}

#### Deleware
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("DE", 10,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("DE", 10,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("DE", 10,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_DE_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_DE_",k,"_test.rds"))
}

#### Massachusetts
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("MA", 25,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("MA", 25,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("MA", 25,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_MA_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_MA_",k,"_test.rds"))
}
#
# #### Maryland
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("MD", 24,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("MD", 24,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("MD", 24,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_MD_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_MD_",k,"_test.rds"))
}


#### North Carolina
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("NC", 37,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])
  
  calibration_pr<- calibrate_rec_catch("NC", 37,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])
  
  calibration_sh<- calibrate_rec_catch("NC", 37,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])
  
  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)
  
  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NC_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NC_",k,"_test.rds"))
}



#### New Jersey
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("NJ", 34,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("NJ", 34,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("NJ", 34,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NJ_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NJ_",k,"_test.rds"))
}

#### New York
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("NY", 36,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("NY", 36,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("NY", 36,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_NY_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_NY_",k,"_test.rds"))
}

# #### Rhode Island
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("RI", 44,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("RI", 44,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("RI", 44,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_RI_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_RI_",k,"_test.rds"))
}



#### Virginia
for(k in 1:100){
  calibration_fh<- calibrate_rec_catch("VA", 51,
                                       # p_star_sf,
                                       # p_star_bsb,
                                       # p_star_scup,
                                       "fh",  k)
  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch("VA", 51,
                                       # p_star_sf_NJ_variable_pr,
                                       # p_star_bsb_NJ_variable_pr,
                                       # p_star_scup_NJ_variable_pr,
                                       "pr",  k)
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch("VA", 51,
                                       # p_star_sf_NJ_variable_sh,
                                       # p_star_bsb_NJ_variable_sh,
                                       # p_star_scup_NJ_variable_sh,
                                       select_mode = "sh",  k)
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("data-raw/calibration/pds_VA_",k,"_test.rds"))
  saveRDS(costs_new_all, file = paste0("data-raw/calibration/costs_VA_",k,"_test.rds"))
}

