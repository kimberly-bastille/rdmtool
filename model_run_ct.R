##############################
### CT Rec model run  ########
##############################


predictions = list()

eff_seed<-32190
set.seed(eff_seed)

# Start the clock!
#ptm <- proc.time()

for (x in 1:100){
  
  print(x)
  params <- list(state1 = c("CT"),
                 state_no=c(9),
                 directed_trips_table = c(list(directed_trips_table_base[[1]])),
                 p_star_sf = c(p_star_sf_CT_variable),
                 p_star_bsb = c(p_star_bsb_CT_variable),
                 p_star_scup = c(p_star_scup_CT_variable),
                 sf_catch_data_all = c(list(catch_files_all_cal_base[[1]])))
  
  
  calibration_output_by_period<- readRDS(file = paste0("data-raw/calibration/pds_CT_",x,".rds")) 
  
  costs_new_all<- readRDS(file = paste0("data-raw/calibration/costs_CT_",x,".rds")) 

  calibration_data_table_base <- split(calibration_output_by_period, calibration_output_by_period$state)
  cost_files_all_base <- split(costs_new_all, costs_new_all$state)
  
  
  
  
  #2) Run the prediction model
  
  # Function arguments are:
  #State1 = list of states
  #calibration_data_table = list of calibration outcomes including the number of choice occasions
  #directed_trips_table = table containing regulations for state an bimonthly period
  #sf_size_data_read = sf catch-at-length distribution in the baseline year,
  #bsb_size_data_read = bsb catch-at-length distribution in the baseline year,
  #scup_size_data_read = scup catch-at-length distribution in the baseline year,
  #param_draws_MA =  list of utility draws for choice occasions combined for all states
  #costs_new_all = list of costs draws for choice occasions combined for all states
  #sf_catch_data_all = list of catch draws for all species combined for all states
  
  
  ##Run the catch function
  source("R/predict_rec_catch.R")
  
  # parallelly::availableCores()
  # future::plan(future::multisession, workers=6)
  
  
  params <- list(state1 = c( "CT"),
                 calibration_data_table = c(list(calibration_data_table_base[[1]])),
                 directed_trips_table = c(list(directed_trips_table_base[[1]])),
                 sf_size_data_read = c(list(sf_size_data_read_base[[1]])),
                 bsb_size_data_read = c(list(bsb_size_data_read_base[[1]])),
                 scup_size_data_read = c(list(scup_size_data_read_base[[1]])),
                 costs_new_all = c(list(cost_files_all_base[[1]])),
                 sf_catch_data_all = c(list(catch_files_all_base[[1]])))
  #print(head(params))
  
  
  safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
  
  #xx_check <-  furrr::future_pmap(params, safe_predict_rec_catch, .options = furrr::furrr_options(seed = 32190))
  xx_check <-  purrr::pmap(params, safe_predict_rec_catch)
  #print(head(xx_check))
  #prediction_output_by_period1 <- furrr::future_map(xx_check, 1)
  prediction_output_by_period1 <- purrr::map(xx_check, 1)
  print("made it through predict")
  
  
  if (class(prediction_output_by_period1[[1]])[1]!="numeric") {
    print("prediction_output_by_period1 is not numeric")
    prediction_output_by_period1<- rlist::list.stack(prediction_output_by_period1, fill=TRUE)
    
    prediction_output_by_period1 <- prediction_output_by_period1 %>%  tidyr::separate(period2, c("period", "mode"), "_")
    
    
    #Metrics at the choice occasion level
    cv_i<- weighted.mean(prediction_output_by_period1$change_CS, prediction_output_by_period1$expand)
    sf_keep_i<- weighted.mean(prediction_output_by_period1$tot_keep_sf, prediction_output_by_period1$expand)
    bsb_keep_i<- weighted.mean(prediction_output_by_period1$tot_keep_bsb, prediction_output_by_period1$expand)
    
    
    cv_i_boat<- weighted.mean(prediction_output_by_period1$change_CS[prediction_output_by_period1$mode=="bt"],
                              prediction_output_by_period1$expand[prediction_output_by_period1$mode=="bt"])
    
    cv_i_shore<- weighted.mean(prediction_output_by_period1$change_CS[prediction_output_by_period1$mode=="sh"],
                               prediction_output_by_period1$expand[prediction_output_by_period1$mode=="sh"])
    
    
    sf_keep_i_boat<- weighted.mean(prediction_output_by_period1$tot_keep_sf[prediction_output_by_period1$mode=="bt"],
                                   prediction_output_by_period1$expand[prediction_output_by_period1$mode=="bt"])
    
    sf_keep_i_shore<- weighted.mean(prediction_output_by_period1$tot_keep_sf[prediction_output_by_period1$mode=="sh"],
                                    prediction_output_by_period1$expand[prediction_output_by_period1$mode=="sh"])
    
    bsb_keep_i_boat<- weighted.mean(prediction_output_by_period1$tot_keep_bsb[prediction_output_by_period1$mode=="bt"],
                                    prediction_output_by_period1$expand[prediction_output_by_period1$mode=="bt"])
    
    bsb_keep_i_shore<- weighted.mean(prediction_output_by_period1$tot_keep_bsb[prediction_output_by_period1$mode=="sh"],
                                     prediction_output_by_period1$expand[prediction_output_by_period1$mode=="sh"])
    
    
    prediction_output_by_period1 <- prediction_output_by_period1 %>%
      data.table::as.data.table() %>%
      .[, cv_sum := expand*change_CS] %>%
      .[, sf_keep_sum := expand*tot_keep_sf] %>%
      .[, bsb_keep_sum := expand*tot_keep_bsb] %>%
      .[, ntrips_alt := expand*probA] %>%
      .[mode=="bt", cv_sum_boat := expand*change_CS] %>%
      .[mode=="sh", cv_sum_sh := expand*change_CS] %>%
      .[mode=="bt", sf_keep_sum_boat := expand*tot_keep_sf] %>%
      .[mode=="sh", sf_keep_sum_sh := expand*tot_keep_sf] %>%
      .[mode=="bt", bsb_keep_sum_boat := expand*tot_keep_bsb] %>%
      .[mode=="sh", bsb_keep_sum_sh := expand*tot_keep_bsb] %>%
      .[mode=="bt", ntrips_boat := expand*probA] %>%
      .[mode=="sh", ntrips_sh := expand*probA]
    
    prediction_output_by_period1 <- prediction_output_by_period1 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    
    #Metrics at the state level
    for(s in  unique(prediction_output_by_period1$state)){
      
      #Mean values per choice occasion by state
      assign(paste0("cv_i_", s), weighted.mean(prediction_output_by_period1$change_CS[prediction_output_by_period1$state==s],
                                               prediction_output_by_period1$expand[prediction_output_by_period1$state==s]))
      
      assign(paste0("cv_i_boat_", s), weighted.mean(prediction_output_by_period1$change_CS[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"],
                                                    prediction_output_by_period1$expand[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      
      assign(paste0("cv_i_shore_", s), weighted.mean(prediction_output_by_period1$change_CS[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"],
                                                     prediction_output_by_period1$expand[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      
      assign(paste0("sf_keep_i_", s), weighted.mean(prediction_output_by_period1$tot_keep_sf[prediction_output_by_period1$state==s],
                                                    prediction_output_by_period1$expand[prediction_output_by_period1$state==s]))
      
      assign(paste0("sf_keep_i_boat_", s), weighted.mean(prediction_output_by_period1$tot_keep_sf[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"],
                                                         prediction_output_by_period1$expand[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      
      assign(paste0("sf_keep_i_shore_", s), weighted.mean(prediction_output_by_period1$tot_keep_sf[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"],
                                                          prediction_output_by_period1$expand[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      
      assign(paste0("bsb_keep_i_", s), weighted.mean(prediction_output_by_period1$tot_keep_bsb[prediction_output_by_period1$state==s],
                                                     prediction_output_by_period1$expand[prediction_output_by_period1$state==s]))
      
      assign(paste0("bsb_keep_i_boat_", s), weighted.mean(prediction_output_by_period1$tot_keep_bsb[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"],
                                                          prediction_output_by_period1$expand[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      
      assign(paste0("bsb_keep_i_shore_", s), weighted.mean(prediction_output_by_period1$tot_keep_bsb[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"],
                                                           prediction_output_by_period1$expand[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      #Sum values per by state
      assign(paste0("cv_sum_", s), sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state==s]))
      assign(paste0("cv_sum_boat_", s), sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      assign(paste0("cv_sum_shore_", s), sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      assign(paste0("sf_keep_sum_", s), sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state==s]))
      assign(paste0("sf_keep_sum_boat_", s), sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      assign(paste0("sf_keep_sum_shore_", s), sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      assign(paste0("bsb_keep_sum_", s), sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state==s]))
      assign(paste0("bsb_keep_sum_boat_", s), sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      assign(paste0("bsb_keep_sum_shore_", s), sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      assign(paste0("ntrips_sum_", s), sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state==s]))
      assign(paste0("ntrips_sum_boat_", s), sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="bt"]))
      assign(paste0("ntrips_sum_shore_", s), sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state==s & prediction_output_by_period1$mode=="sh"]))
      
      
      
      #Retain calibration catch by state
      assign(paste0("calib_sf_keep_sum_", s), sum(calibration_output_by_period$tot_keep_sf[calibration_output_by_period$state==s]))
      assign(paste0("calib_sf_rel_sum_", s), sum(calibration_output_by_period$tot_rel_sf[calibration_output_by_period$state==s]))
      
      assign(paste0("calib_bsb_keep_sum_", s), sum(calibration_output_by_period$tot_keep_bsb[calibration_output_by_period$state==s]))
      assign(paste0("calib_bsb_rel_sum_", s), sum(calibration_output_by_period$tot_rel_bsb[calibration_output_by_period$state==s]))
      
      #Retain number of choice occasions by state
      assign(paste0("n_choice_occasions_", s), sum(calibration_output_by_period$n_choice_occasions[calibration_output_by_period$state==s]))
      
      
      
      
      #retain the seeds for the random number generator
      assign(paste0("seed_", s), mean(prediction_output_by_period1$seed[prediction_output_by_period1$state==s]))
    }
    
    #Metrics a coast level
    cv_sum<- sum(prediction_output_by_period1$cv_sum)
    cv_sum_boat<- sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$mode=="bt"])
    cv_sum_shore<- sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$mode=="sh"])
    
    sf_keep_sum<- sum(prediction_output_by_period1$sf_keep_sum)
    sf_keep_sum_boat<- sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$mode=="bt"])
    sf_keep_sum_shore<- sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$mode=="sh"])
    
    bsb_keep_sum<- sum(prediction_output_by_period1$bsb_keep_sum)
    bsb_keep_sum_boat<- sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$mode=="bt"])
    bsb_keep_sum_shore<- sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$mode=="sh"])
    
    ntrips_sum<-sum(prediction_output_by_period1$ntrips_alt)
    ntrips_sum_boat<-sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$mode=="bt"])
    ntrips_sum_shore<-sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$mode=="sh"])
    
    n_choice_occasions_sum<-sum(calibration_output_by_period$n_choice_occasions)
    
    
    
    predictions[[x]]<- as.data.frame(cbind(
      
      #Mean CV per choice occasion
      cv_i, 
      cv_i_CT, cv_i_boat_CT, cv_i_shore_CT,
      
      #Sum CV
      cv_sum, cv_sum_shore, cv_sum_boat,
      cv_sum_CT, cv_sum_shore_CT, cv_sum_boat_CT, 
      
      #Mean SF keep per choice occasion
      sf_keep_i, sf_keep_i_boat, sf_keep_i_shore,
      sf_keep_i_CT, sf_keep_i_boat_CT, sf_keep_i_shore_CT,
      
      #Sum SF keep
      sf_keep_sum, sf_keep_sum_boat, sf_keep_i_shore,
      sf_keep_sum_CT, sf_keep_sum_boat_CT, sf_keep_sum_shore_CT,
      
      #Mean BSB keep per choice occasion
      bsb_keep_i, bsb_keep_i_boat, bsb_keep_i_shore,
      bsb_keep_i_CT, bsb_keep_i_boat_CT, bsb_keep_i_shore_CT, 
      
      #Sum BSB keep
      bsb_keep_sum, bsb_keep_sum_boat, bsb_keep_i_shore,
      bsb_keep_sum_CT, bsb_keep_sum_boat_CT, bsb_keep_sum_shore_CT, 
      
      #Sum of number of trips
      ntrips_sum, ntrips_sum_boat, ntrips_sum_shore,
      ntrips_sum_CT, ntrips_sum_boat_CT, ntrips_sum_shore_CT,
      
      #seeds
      seed_CT, 
      
      #calibration year catch
      calib_sf_keep_sum_CT,  calib_sf_rel_sum_CT, 
      calib_bsb_keep_sum_CT, calib_bsb_rel_sum_CT, 
      
      #number of choice occasions
      n_choice_occasions_CT, 
      n_choice_occasions_sum))
    
    predictions[[x]]$draw<-x
    #predictions[[x]]$decade<-d
    
    # predictions[[x]]<-list.append(predictions[[x]],draw=x)
    # predictions[[x]]<-list.append(predictions[[x]],decade=d)
    
    #predictions[[x]]$draw<-x
    #predictions[[x]]$decade<-d
    
  }
  
  
  else{
    print("prediction_output_by_period1 is numeric")
    
  }
  # rm(calibration_data_table_base, calibration_output_by_period, 
  #    params, prediction_output_by_period1, xx_check,
  #    costs_new_all)
}
predictions_all<-list()
predictions_all<-rlist::list.rbind(predictions)


# Stop the clock
#proc.time() - ptm

predictions_all<-as.data.frame(predictions_all)
#write_xlsx(predictions_all,"projections_decade8_test.xlsx") #These results with the substitution parameter == -0.5, decade 8
#write_xlsx(predictions_all,"projections_decade8_sub5.xlsx")
#write_xlsx(predictions_all,"projections_decade8_sub5_add_on.xlsx")
#write_xlsx(predictions_all,"projections_decade8_sub5_4_17.xlsx")
#write_xlsx(predictions_all,"projections_decade8_sub25_test.xlsx")
#write_xlsx(predictions_all,"projections_decade1_sub_orig.xlsx")

readr::write_csv(predictions_all, "projections_CT.csv")


