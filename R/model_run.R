################
#Decade 1, Sub=orig
################

d<-1


#here we can adjust the substitution parameter

substitution_mean_parameter<- -.0596595
substitution_sd_parameter<- .161084

#substitution_mean_parameter<- -.25
#substitution_sd_parameter<- .67501404

#substitution_mean_parameter<- -.5
#substitution_sd_parameter<- 1.3500281



predictions = list()


# Start the clock!
ptm <- proc.time()

for (x in 1:1){


  params <- list(state1 = c("CT", "DE", "MA", "MD", "NJ","NY", "RI", "VA"),
                 state_no=c(9, 10, 25, 24, 34, 36, 44, 51),
                 directed_trips_table = c(list(directed_trips_table_base[[1]]),list(directed_trips_table_base[[2]]),
                                          list(directed_trips_table_base[[3]]),list(directed_trips_table_base[[4]]),
                                          list(directed_trips_table_base[[5]]), list(directed_trips_table_base[[6]]),
                                          list(directed_trips_table_base[[7]]), list(directed_trips_table_base[[8]])),
                 p_star_sf = c(p_star_sf_CT_variable, p_star_sf_DE_variable, p_star_sf_MA_variable, p_star_sf_MD_variable,
                               p_star_sf_NJ_variable, p_star_sf_NY_variable, p_star_sf_RI_variable, p_star_sf_VA_variable),
                 p_star_bsb = c(p_star_bsb_CT_variable, p_star_bsb_DE_variable, p_star_bsb_MA_variable, p_star_bsb_MD_variable,
                                p_star_bsb_NJ_variable, p_star_bsb_NY_variable, p_star_bsb_RI_variable, p_star_bsb_VA_variable),
                 p_star_scup = c(p_star_scup_CT_variable, p_star_scup_DE_variable, p_star_scup_MA_variable, p_star_scup_MD_variable,
                                 p_star_scup_NJ_variable, p_star_scup_NY_variable, p_star_scup_RI_variable, p_star_scup_VA_variable),
                 sf_catch_data_all = c(list(catch_files_all_cal_base[[1]]),list(catch_files_all_cal_base[[2]]),
                                       list(catch_files_all_cal_base[[3]]),list(catch_files_all_cal_base[[4]]),
                                       list(catch_files_all_cal_base[[5]]), list(catch_files_all_cal_base[[6]]),
                                       list(catch_files_all_cal_base[[7]]), list(catch_files_all_cal_base[[8]])))


  source("R/calibration.R")

  parallelly::availableCores()
  future::plan(future::multisession, workers=6)

  safe_calibrate_rec_catch <- purrr::safely(calibrate_rec_catch, otherwise = NA_real_)

  #xx_check_cal <-  pmap(params, safe_calibrate_rec_catch)
  xx_check_cal <-  furrr::future_pmap(params, safe_calibrate_rec_catch, .options = furrr::furrr_options(seed = 32190+x))

  calibration_output_by_period<-list()






  calibration_output_by_period<-rbind( xx_check_cal[[1]][["result"]][[1]],
                                       xx_check_cal[[2]][["result"]][[1]],
                                       xx_check_cal[[3]][["result"]][[1]],
                                       xx_check_cal[[4]][["result"]][[1]],
                                       xx_check_cal[[5]][["result"]][[1]],
                                       xx_check_cal[[6]][["result"]][[1]],
                                       xx_check_cal[[7]][["result"]][[1]],
                                       xx_check_cal[[8]][["result"]][[1]])

  costs_new_all<-               rbind( xx_check_cal[[1]][["result"]][[2]],
                                       xx_check_cal[[2]][["result"]][[2]],
                                       xx_check_cal[[3]][["result"]][[2]],
                                       xx_check_cal[[4]][["result"]][[2]],
                                       xx_check_cal[[5]][["result"]][[2]],
                                       xx_check_cal[[6]][["result"]][[2]],
                                       xx_check_cal[[7]][["result"]][[2]],
                                       xx_check_cal[[8]][["result"]][[2]])



  #saveRDS(calibration_output_by_period, file = "calibration_output_by_period_d8_sub5.rds")

  #calibration_data_table <- readRDS("calibration_output_by_period_d8_sub5.rds")
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
  source("R/projection.R")

  parallelly::availableCores()
  future::plan(future::multisession, workers=6)



  eff_seed<-32190+x

  set.seed(eff_seed)

  params <- list(state1 = c("CT", "DE", "MA", "MD", "NJ","NY", "RI", "VA"),
                 calibration_data_table = c(list(calibration_data_table_base[[1]]),list(calibration_data_table_base[[2]]),
                                            list(calibration_data_table_base[[3]]),list(calibration_data_table_base[[4]]),
                                            list(calibration_data_table_base[[5]]), list(calibration_data_table_base[[6]]),
                                            list(calibration_data_table_base[[7]]), list(calibration_data_table_base[[8]])),
                 directed_trips_table = c(list(directed_trips_table_base[[1]]),list(directed_trips_table_base[[2]]),
                                          list(directed_trips_table_base[[3]]),list(directed_trips_table_base[[4]]),
                                          list(directed_trips_table_base[[5]]), list(directed_trips_table_base[[6]]),
                                          list(directed_trips_table_base[[7]]), list(directed_trips_table_base[[8]])),
                 sf_size_data_read = c(list(sf_size_data_read_base[[1]]),list(sf_size_data_read_base[[2]]),
                                       list(sf_size_data_read_base[[3]]),list(sf_size_data_read_base[[4]]),
                                       list(sf_size_data_read_base[[5]]), list(sf_size_data_read_base[[6]]),
                                       list(sf_size_data_read_base[[7]]), list(sf_size_data_read_base[[8]])),
                 bsb_size_data_read = c(list(bsb_size_data_read_base[[1]]),list(bsb_size_data_read_base[[2]]),
                                        list(bsb_size_data_read_base[[3]]),list(bsb_size_data_read_base[[4]]),
                                        list(bsb_size_data_read_base[[5]]), list(bsb_size_data_read_base[[6]]),
                                        list(bsb_size_data_read_base[[7]]), list(bsb_size_data_read_base[[8]])),
                 scup_size_data_read = c(list(scup_size_data_read_base[[1]]),list(scup_size_data_read_base[[2]]),
                                         list(scup_size_data_read_base[[3]]),list(scup_size_data_read_base[[4]]),
                                         list(scup_size_data_read_base[[5]]), list(scup_size_data_read_base[[6]]),
                                         list(scup_size_data_read_base[[7]]), list(scup_size_data_read_base[[8]])),
                 costs_new_all = c(list(cost_files_all_base[[1]]),list(cost_files_all_base[[2]]),
                                   list(cost_files_all_base[[3]]),list(cost_files_all_base[[4]]),
                                   list(cost_files_all_base[[5]]), list(cost_files_all_base[[6]]),
                                   list(cost_files_all_base[[7]]), list(cost_files_all_base[[8]])),
                 sf_catch_data_all = c(list(catch_files_all_base[[1]]),list(catch_files_all_base[[2]]),
                                       list(catch_files_all_base[[3]]),list(catch_files_all_base[[4]]),
                                       list(catch_files_all_base[[5]]), list(catch_files_all_base[[6]]),
                                       list(catch_files_all_base[[7]]), list(catch_files_all_base[[8]])))


  safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)

  xx_check <-  furrr::future_pmap(params, safe_predict_rec_catch, .options = furrr::furrr_options(seed = 32190+x))

  prediction_output_by_period1 <- furrr::future_map(xx_check, 1)



  if (class(prediction_output_by_period1[[1]])[1]!="numeric" &
      class(prediction_output_by_period1[[2]])[1]!="numeric" &
      class(prediction_output_by_period1[[3]])[1]!="numeric" &
      class(prediction_output_by_period1[[4]])[1]!="numeric" &
      class(prediction_output_by_period1[[5]])[1]!="numeric" &
      class(prediction_output_by_period1[[6]])[1]!="numeric" &
      class(prediction_output_by_period1[[7]])[1]!="numeric" &
      class(prediction_output_by_period1[[8]])[1]!="numeric") {




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
      cv_i, cv_i_shore, cv_i_boat,
      #cv_i_MA, cv_i_boat_MA, cv_i_shore_MA,
      cv_i_RI, cv_i_boat_RI, cv_i_shore_RI,
      cv_i_CT, cv_i_boat_CT, cv_i_shore_CT,
      #cv_i_NY, cv_i_boat_NY, cv_i_shore_NY,
      #cv_i_NJ, cv_i_boat_NJ, cv_i_shore_NJ,
      cv_i_DE, cv_i_boat_DE, cv_i_shore_DE,
      #cv_i_MD, cv_i_boat_MD, cv_i_shore_MD,
      cv_i_VA, cv_i_boat_VA, cv_i_shore_VA,

      #Sum CV
      cv_sum, cv_sum_shore, cv_sum_boat,
      cv_sum_MA, cv_sum_RI, cv_sum_CT, cv_sum_NY, cv_sum_NJ, cv_sum_DE, cv_sum_MD, cv_sum_VA,
      cv_sum_shore_MA, cv_sum_shore_RI, cv_sum_shore_CT, cv_sum_shore_NY, cv_sum_shore_NJ, cv_sum_shore_DE, cv_sum_shore_MD, cv_sum_shore_VA,
      cv_sum_boat_MA, cv_sum_boat_RI, cv_sum_boat_CT, cv_sum_boat_NY, cv_sum_boat_NJ, cv_sum_boat_DE, cv_sum_boat_MD, cv_sum_boat_VA,

      #Mean SF keep per choice occasion
      sf_keep_i, sf_keep_i_boat, sf_keep_i_shore,
      sf_keep_i_MA, sf_keep_i_RI, sf_keep_i_CT, sf_keep_i_NY, sf_keep_i_NJ, sf_keep_i_DE, sf_keep_i_MD, sf_keep_i_VA,
      sf_keep_i_boat_MA, sf_keep_i_boat_RI, sf_keep_i_boat_CT, sf_keep_i_boat_NY, sf_keep_i_boat_NJ, sf_keep_i_boat_DE, sf_keep_i_boat_MD, sf_keep_i_boat_VA,
      sf_keep_i_shore_MA, sf_keep_i_shore_RI, sf_keep_i_shore_CT, sf_keep_i_shore_NY, sf_keep_i_shore_NJ, sf_keep_i_shore_DE, sf_keep_i_shore_MD, sf_keep_i_shore_VA,

      #Sum SF keep
      sf_keep_sum, sf_keep_sum_boat, sf_keep_i_shore,
      sf_keep_sum_MA, sf_keep_sum_RI, sf_keep_sum_CT, sf_keep_sum_NY, sf_keep_sum_NJ, sf_keep_sum_DE, sf_keep_sum_MD, sf_keep_sum_VA,
      sf_keep_sum_boat_MA, sf_keep_sum_boat_RI, sf_keep_sum_boat_CT, sf_keep_sum_boat_NY, sf_keep_sum_boat_NJ, sf_keep_sum_boat_DE, sf_keep_sum_boat_MD, sf_keep_sum_boat_VA,
      sf_keep_sum_shore_MA, sf_keep_sum_shore_RI, sf_keep_sum_shore_CT, sf_keep_sum_shore_NY, sf_keep_sum_shore_NJ, sf_keep_sum_shore_DE, sf_keep_sum_shore_MD, sf_keep_sum_shore_VA,

      #Mean BSB keep per choice occasion
      bsb_keep_i, bsb_keep_i_boat, bsb_keep_i_shore,
      bsb_keep_i_MA, bsb_keep_i_RI, bsb_keep_i_CT, bsb_keep_i_NY, bsb_keep_i_NJ, bsb_keep_i_DE, bsb_keep_i_MD, bsb_keep_i_VA,
      bsb_keep_i_boat_MA, bsb_keep_i_boat_RI, bsb_keep_i_boat_CT, bsb_keep_i_boat_NY, bsb_keep_i_boat_NJ, bsb_keep_i_boat_DE, bsb_keep_i_boat_MD, bsb_keep_i_boat_VA,
      bsb_keep_i_shore_MA, bsb_keep_i_shore_RI, bsb_keep_i_shore_CT, bsb_keep_i_shore_NY, bsb_keep_i_shore_NJ, bsb_keep_i_shore_DE, bsb_keep_i_shore_MD, bsb_keep_i_shore_VA,

      #Sum BSB keep
      bsb_keep_sum, bsb_keep_sum_boat, bsb_keep_i_shore,
      bsb_keep_sum_MA, bsb_keep_sum_RI, bsb_keep_sum_CT, bsb_keep_sum_NY, bsb_keep_sum_NJ, bsb_keep_sum_DE, bsb_keep_sum_MD, bsb_keep_sum_VA,
      bsb_keep_sum_boat_MA, bsb_keep_sum_boat_RI, bsb_keep_sum_boat_CT, bsb_keep_sum_boat_NY, bsb_keep_sum_boat_NJ, bsb_keep_sum_boat_DE, bsb_keep_sum_boat_MD, bsb_keep_sum_boat_VA,
      bsb_keep_sum_shore_MA, bsb_keep_sum_shore_RI, bsb_keep_sum_shore_CT, bsb_keep_sum_shore_NY, bsb_keep_sum_shore_NJ, bsb_keep_sum_shore_DE, bsb_keep_sum_shore_MD, bsb_keep_sum_shore_VA,

      #Sum of number of trips
      ntrips_sum, ntrips_sum_boat, ntrips_sum_shore,
      ntrips_sum_MA, ntrips_sum_RI, ntrips_sum_CT, ntrips_sum_NY, ntrips_sum_NJ, ntrips_sum_DE, ntrips_sum_MD, ntrips_sum_VA,
      ntrips_sum_boat_MA, ntrips_sum_boat_RI, ntrips_sum_boat_CT, ntrips_sum_boat_NY, ntrips_sum_boat_NJ, ntrips_sum_boat_DE, ntrips_sum_boat_MD, ntrips_sum_boat_VA,
      ntrips_sum_shore_MA, ntrips_sum_shore_RI, ntrips_sum_shore_CT, ntrips_sum_shore_NY, ntrips_sum_shore_NJ, ntrips_sum_shore_DE, ntrips_sum_shore_MD, ntrips_sum_shore_VA,

      #seeds
      seed_MA, seed_RI, seed_CT, seed_NY, seed_NJ, seed_DE, seed_MD, seed_VA,


      #calibration year catch
      calib_sf_keep_sum_MA, calib_sf_keep_sum_RI, calib_sf_keep_sum_CT, calib_sf_keep_sum_NY, calib_sf_keep_sum_NJ, calib_sf_keep_sum_DE, calib_sf_keep_sum_MD, calib_sf_keep_sum_VA,
      calib_sf_rel_sum_MA, calib_sf_rel_sum_RI, calib_sf_rel_sum_CT, calib_sf_rel_sum_NY, calib_sf_rel_sum_NJ, calib_sf_rel_sum_DE, calib_sf_rel_sum_MD, calib_sf_rel_sum_VA,
      calib_bsb_keep_sum_MA, calib_bsb_keep_sum_RI, calib_bsb_keep_sum_CT, calib_bsb_keep_sum_NY, calib_bsb_keep_sum_NJ, calib_bsb_keep_sum_DE, calib_bsb_keep_sum_MD, calib_bsb_keep_sum_VA,
      calib_bsb_rel_sum_MA, calib_bsb_rel_sum_RI, calib_bsb_rel_sum_CT, calib_bsb_rel_sum_NY, calib_bsb_rel_sum_NJ, calib_bsb_rel_sum_DE, calib_bsb_rel_sum_MD, calib_bsb_rel_sum_VA,

      #number of choice occasions
      n_choice_occasions_MA, n_choice_occasions_RI, n_choice_occasions_CT, n_choice_occasions_NY, n_choice_occasions_NJ, n_choice_occasions_DE, n_choice_occasions_MD, n_choice_occasions_VA,
      n_choice_occasions_sum))

    predictions[[x]]$draw<-x
    predictions[[x]]$decade<-d

    # predictions[[x]]<-list.append(predictions[[x]],draw=x)
    # predictions[[x]]<-list.append(predictions[[x]],decade=d)

    #predictions[[x]]$draw<-x
    #predictions[[x]]$decade<-d

  }


  else{


  }

}
predictions_all<-list()
predictions_all<-rlist::list.rbind(predictions)


# Stop the clock
proc.time() - ptm

predictions_all<-as.data.frame(predictions_all)
#write_xlsx(predictions_all,"projections_decade8_test.xlsx") #These results with the substitution parameter == -0.5, decade 8
#write_xlsx(predictions_all,"projections_decade8_sub5.xlsx")
#write_xlsx(predictions_all,"projections_decade8_sub5_add_on.xlsx")
#write_xlsx(predictions_all,"projections_decade8_sub5_4_17.xlsx")
#write_xlsx(predictions_all,"projections_decade8_sub25_test.xlsx")
write_xlsx(predictions_all,"projections_decade1_sub_orig.xlsx")




