##############################
### NJ Rec model run  ########
##############################

print("start model_NJ")
state1 = "NJ"
predictions = list()

p_star_sf_NJ_variable<- 0.89
p_star_bsb_NJ_variable<- 0.885
p_star_scup_NJ_variable<- 0.045

catch_files_NJ<- readRDS(here::here(paste0("data-raw/catch/catch_files_NJ.rds"))) %>% 
  dplyr::rename(tot_sf_catch = sf_tot_cat,  
                tot_bsb_catch = bsb_tot_cat, 
                tot_scup_catch = scup_tot_cat) %>% 
  dplyr::mutate(month_day = stringr::str_remove(lubridate::make_date("2023", month, day), "2023-"), 
                period2 = paste0(month_day, "-", mode1))

directed_trips<-readRDS(file.path(here::here(paste0("data-raw/directed_trips/directed_trips_NJ.rds")))) %>% 
  dplyr::mutate(fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas1[1]) & day_i <= lubridate::yday(input$SFnjFH_seas1[2]) ~ as.numeric(input$SFnjFH_1_smbag), TRUE ~ fluke_bag1), 
                fluke_bag1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas1[1]) & day_i <= lubridate::yday(input$SFnjPR_seas1[2]) ~ as.numeric(input$SFnjPR_1_smbag), TRUE ~ fluke_bag1), 
                fluke_bag1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas1[1]) & day_i <= lubridate::yday(input$SFnjSH_seas1[2]) ~ as.numeric(input$SFnjSH_1_smbag), TRUE ~ fluke_bag1),
                fluke_bag2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas1[1]) & day_i <= lubridate::yday(input$SFnjFH_seas1[2]) ~ as.numeric(input$SFnjFH_1_lgbag), TRUE ~ fluke_bag2), 
                fluke_bag2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas1[1]) & day_i <= lubridate::yday(input$SFnjPR_seas1[2]) ~ as.numeric(input$SFnjPR_1_lgbag), TRUE ~ fluke_bag2),
                fluke_bag2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas1[1]) & day_i <= lubridate::yday(input$SFnjSH_seas1[2]) ~ as.numeric(input$SFnjSH_1_lgbag), TRUE ~ fluke_bag2),
                
                fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas2[1]) & day_i <= lubridate::yday(input$SFnjFH_seas2[2]) ~ as.numeric(input$SFnjFH_2_smbag), TRUE ~ fluke_bag1),
                fluke_bag1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas2[1]) & day_i <= lubridate::yday(input$SFnjPR_seas2[2]) ~ as.numeric(input$SFnjPR_2_smbag), TRUE ~ fluke_bag1),
                fluke_bag1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_smbag), TRUE ~ fluke_bag1),
                fluke_bag2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas2[1]) & day_i <= lubridate::yday(input$SFnjFH_seas2[2]) ~ as.numeric(input$SFnjFH_2_lgbag), TRUE ~ fluke_bag2),
                fluke_bag2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas2[1]) & day_i <= lubridate::yday(input$SFnjPR_seas2[2]) ~ as.numeric(input$SFnjPR_2_lgbag), TRUE ~ fluke_bag2),
                fluke_bag2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_lgbag), TRUE ~ fluke_bag2))

### Giant IFELSE to sort out multiple mode options!! ##
if(input$input_type == "Single"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas1[1]) & day_i <= lubridate::yday(input$BSBnj_seas1[2]) ~ as.numeric(input$BSBnj_1_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas2[1]) & day_i <= lubridate::yday(input$BSBnj_seas2[2]) ~ as.numeric(input$BSBnj_2_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas3[1]) & day_i <= lubridate::yday(input$BSBnj_seas3[2]) ~ as.numeric(input$BSBnj_3_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas4[1]) & day_i <= lubridate::yday(input$BSBnj_seas4[2]) ~ as.numeric(input$BSBnj_4_bag), TRUE ~ bsb_bag),
                  bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas5[2]) ~ as.numeric(input$BSBnjFH_5_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas5[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas5[2]) ~ as.numeric(input$BSBnjPR_5_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas5[2]) ~ as.numeric(input$BSBnjSH_5_bag), TRUE ~ bsb_bag))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas1[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas1[2]) ~ as.numeric(input$BSBnjFH_1_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas1[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas1[2]) ~ as.numeric(input$BSBnjPR_1_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas1[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas1[2]) ~ as.numeric(input$BSBnjSH_1_bag), TRUE ~ bsb_bag),
                  bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas2[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas2[2]) ~ as.numeric(input$BSBnjFH_2_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas2[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas2[2]) ~ as.numeric(input$BSBnjPR_2_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas2[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas2[2]) ~ as.numeric(input$BSBnjSH_2_bag), TRUE ~ bsb_bag),
                  bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas3[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas3[2]) ~ as.numeric(input$BSBnjFH_3_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas3[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas3[2]) ~ as.numeric(input$BSBnjPR_3_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas3[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas3[2]) ~ as.numeric(input$BSBnjSH_3_bag), TRUE ~ bsb_bag),
                  bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas4[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas4[2]) ~ as.numeric(input$BSBnjFH_4_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas4[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas4[2]) ~ as.numeric(input$BSBnjPR_4_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas4[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas4[2]) ~ as.numeric(input$BSBnjSH_4_bag), TRUE ~ bsb_bag),
                  bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas5[2]) ~ as.numeric(input$BSBnjFH_5_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas5[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas5[2]) ~ as.numeric(input$BSBnjPR_5_bag), TRUE ~ bsb_bag), 
                  bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas5[2]) ~ as.numeric(input$BSBnjSH_5_bag), TRUE ~ bsb_bag))
}


#print(directed_trips_test)

for (x in 1:3){
  
  print(x)
  
  calibration_output_by_period<- readRDS(here::here(paste0("data-raw/calibration/pds_NJ_",x,".rds"))) %>% 
    tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>% 
    dplyr::mutate(month_day = stringr::str_remove(lubridate::make_date("2023", month, day), "2023-"), 
                  period2 = paste0(month_day, "-", mode)) %>% 
    dplyr::select(-c(month, day, month_day, mode))
  
  costs_new_all<- readRDS(here::here(paste0("data-raw/calibration/costs_NJ_",x,".rds")))%>% 
    tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>% 
    dplyr::mutate(month_day = stringr::str_remove(lubridate::make_date("2023", month, day), "2023-"), 
                  period2 = paste0(month_day, "-", mode)) %>% 
    dplyr::select(-c(month, day, month_day, mode))
  
  calibration_data_table_base <- split(calibration_output_by_period, calibration_output_by_period$state)
  cost_files_all_base <- split(costs_new_all, costs_new_all$state)
  
  
  directed_trips2 <- directed_trips %>% 
    dplyr::filter(draw == x) %>% 
    dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
                  #month = as.numeric(month), 
                  period2 = paste0(month24, "-", day, "-", mode))
  print(directed_trips2)
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
  source(here::here("R/predict_rec_catch.R"))
  
  # parallelly::availableCores()
  # future::plan(future::multisession, workers=6
  # params <- list(state1 =  c("NJ"),
  #                calibration_data_table = c(list(calibration_data_table_base[[1]])),
  #                calibration_data_table2 = clist(calibration_data_table_base[[1]]),
  #                #directed_trips_table = c(list(directed_trips_table_base[[5]])),
  #                sf_size_data_read = c(list(sf_size_data_read_base[[5]])),
  #                bsb_size_data_read = c(list(bsb_size_data_read_base[[5]])),
  #                scup_size_data_read = c(list(scup_size_data_read_base[[5]])),
  #                costs_new_all = c(list(cost_files_all_base[[1]])),
  #                #sf_catch_data_all = c(list(catch_files_NJ[[1]])))
  #                sf_catch_data_all = c(list(catch_files_NJ)))
  #print(head(params))
  test<- predict_rec_catch(state1 = c("NJ"),
                           calibration_data_table = c(list(calibration_data_table_base[[1]])),
                           directed_trips_table = directed_trips2,
                           sf_size_data_read = c(list(sf_size_data_read_base[[5]])),
                           bsb_size_data_read = c(list(bsb_size_data_read_base[[5]])),
                           scup_size_data_read = c(list(scup_size_data_read_base[[5]])),
                           costs_new_all = c(list(cost_files_all_base[[1]])),
                           #sf_catch_data_all = c(list(catch_files_NJ[[1]])))
                           sf_catch_data_all = c(list(catch_files_NJ)))
  
  # safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
  # 
  # #xx_check <-  furrr::future_pmap(params, safe_predict_rec_catch, .options = furrr::furrr_options(seed = 32190))
  # xx_check <-  purrr::pmap(params, safe_predict_rec_catch)
  # print(head(xx_check))
  # #prediction_output_by_period1 <- furrr::future_map(xx_check, 1)
  # prediction_output_by_period1 <- purrr::map(xx_check, 1)
  print("made it through predict")
  
  prediction_output_by_period1 <- data.frame(test)
  
  #### Somewhere here - calculate 
 
  
  if (class(prediction_output_by_period1[[1]])[1]!="numeric") {
    print("prediction_output_by_period1 is not numeric")
    #prediction_output_by_period1<- rlist::list.stack(prediction_output_by_period1, fill=TRUE)
    
    prediction_output_by_period1 <- prediction_output_by_period1 %>%  tidyr::separate(period2, c("month","day", "mode"), "-")
    
    
    #Metrics at the choice occasion level
    prediction_output_by_period1 <- prediction_output_by_period1 %>%
      data.table::as.data.table() %>%
      .[, cv_sum := expand*change_CS] %>%
      .[, sf_keep_sum := expand*tot_keep_sf] %>%
      .[, sf_rel_sum := expand*tot_rel_sf] %>%
      .[, bsb_keep_sum := expand*tot_keep_bsb] %>%
      .[, bsb_rel_sum := expand*tot_rel_bsb] %>%
      .[, scup_keep_sum := expand*tot_keep_scup] %>%
      .[, scup_rel_sum := expand*tot_rel_scup] %>%
      .[, ntrips_alt := expand*probA] %>%
      .[mode=="pr", cv_sum_pr := expand*change_CS] %>%
      .[mode=="fh", cv_sum_fh := expand*change_CS] %>%
      .[mode=="sh", cv_sum_sh := expand*change_CS] %>%
      .[mode=="pr", sf_keep_sum_pr := expand*tot_keep_sf] %>%
      .[mode=="fh", sf_keep_sum_fh := expand*tot_keep_sf] %>%
      .[mode=="sh", sf_keep_sum_sh := expand*tot_keep_sf] %>%
      .[mode=="pr", sf_rel_sum_pr := expand*tot_rel_sf] %>%
      .[mode=="fh", sf_rel_sum_fh := expand*tot_rel_sf] %>%
      .[mode=="sh", sf_rel_sum_sh := expand*tot_rel_sf] %>%
      .[mode=="pr", bsb_keep_sum_pr := expand*tot_keep_bsb] %>%
      .[mode=="fh", bsb_keep_sum_fh := expand*tot_keep_bsb] %>%
      .[mode=="sh", bsb_keep_sum_sh := expand*tot_keep_bsb] %>%
      .[mode=="pr", bsb_rel_sum_pr := expand*tot_rel_bsb] %>%
      .[mode=="fh", bsb_rel_sum_fh := expand*tot_rel_bsb] %>%
      .[mode=="sh", bsb_rel_sum_sh := expand*tot_rel_bsb] %>%
      
      .[mode=="pr", scup_keep_sum_pr := expand*tot_keep_scup] %>%
      .[mode=="fh", scup_keep_sum_fh := expand*tot_keep_scup] %>%
      .[mode=="sh", scup_keep_sum_sh := expand*tot_keep_scup] %>%
      .[mode=="pr", scup_rel_sum_pr := expand*tot_rel_scup] %>%
      .[mode=="fh", scup_rel_sum_fh := expand*tot_rel_scup] %>%
      .[mode=="sh", scup_rel_sum_sh := expand*tot_rel_scup] %>%
      
      .[mode=="pr", ntrips_pr := expand*probA] %>%
      .[mode=="fh", ntrips_fh := expand*probA] %>%
      .[mode=="sh", ntrips_sh := expand*probA]
    
    prediction_output_by_period1 <- prediction_output_by_period1 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    
    #Metrics at the state level
      assign("cv_sum_NJ", base::sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state=="NJ"]))
      assign("cv_sum_pr_NJ", base::sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("cv_sum_fh_NJ", base::sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("cv_sum_shore_NJ", base::sum(prediction_output_by_period1$cv_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("sf_keep_sum_NJ", base::sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state=="NJ"]))
      assign("sf_keep_sum_pr_NJ", base::sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("sf_keep_sum_fh_NJ", base::sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("sf_keep_sum_shore_NJ", base::sum(prediction_output_by_period1$sf_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("sf_rel_sum_NJ", base::sum(prediction_output_by_period1$sf_rel_sum[prediction_output_by_period1$state=="NJ"]))
      assign("sf_rel_sum_pr_NJ", base::sum(prediction_output_by_period1$sf_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("sf_rel_sum_fh_NJ", base::sum(prediction_output_by_period1$sf_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("sf_rel_sum_shore_NJ", base::sum(prediction_output_by_period1$sf_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("bsb_keep_sum_NJ", base::sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state=="NJ"]))
      assign("bsb_keep_sum_pr_NJ", base::sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("bsb_keep_sum_fh_NJ", base::sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("bsb_keep_sum_shore_NJ", base::sum(prediction_output_by_period1$bsb_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("bsb_rel_sum_NJ", base::sum(prediction_output_by_period1$bsb_rel_sum[prediction_output_by_period1$state=="NJ"]))
      assign("bsb_rel_sum_pr_NJ", base::sum(prediction_output_by_period1$bsb_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("bsb_rel_sum_fh_NJ", base::sum(prediction_output_by_period1$bsb_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("bsb_rel_sum_shore_NJ", base::sum(prediction_output_by_period1$bsb_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("scup_keep_sum_NJ", base::sum(prediction_output_by_period1$scup_keep_sum[prediction_output_by_period1$state=="NJ"]))
      assign("scup_keep_sum_pr_NJ", base::sum(prediction_output_by_period1$scup_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("scup_keep_sum_fh_NJ", base::sum(prediction_output_by_period1$scup_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("scup_keep_sum_shore_NJ", base::sum(prediction_output_by_period1$scup_keep_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("scup_rel_sum_NJ", base::sum(prediction_output_by_period1$scup_rel_sum[prediction_output_by_period1$state=="NJ"]))
      assign("scup_rel_sum_pr_NJ", base::sum(prediction_output_by_period1$scup_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("scup_rel_sum_fh_NJ", base::sum(prediction_output_by_period1$scup_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("scup_rel_sum_shore_NJ", base::sum(prediction_output_by_period1$scup_rel_sum[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      
      assign("ntrips_sum_NJ", base::sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state=="NJ"]))
      assign("ntrips_sum_pr_NJ", base::sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="pr"]))
      assign("ntrips_sum_fh_NJ", base::sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="fh"]))
      assign("ntrips_sum_shore_NJ", base::sum(prediction_output_by_period1$ntrips_alt[prediction_output_by_period1$state=="NJ" & prediction_output_by_period1$mode=="sh"]))
      

    predictions[[x]]<- as.data.frame(cbind(
      
      #Sum CV
      cv_sum_NJ, cv_sum_shore_NJ, cv_sum_pr_NJ, cv_sum_fh_NJ, 
      #SF keep
      sf_keep_sum_NJ, sf_keep_sum_pr_NJ, sf_keep_sum_fh_NJ,sf_keep_sum_shore_NJ,
      #SF release
      sf_rel_sum_NJ, sf_rel_sum_pr_NJ, sf_rel_sum_fh_NJ,sf_rel_sum_shore_NJ,
      #BSB keep
      bsb_keep_sum_NJ, bsb_keep_sum_pr_NJ, bsb_keep_sum_fh_NJ, bsb_keep_sum_shore_NJ, 
      #BSB release
      bsb_rel_sum_NJ, bsb_rel_sum_pr_NJ, bsb_rel_sum_fh_NJ, bsb_rel_sum_shore_NJ, 
      #Scup keep
      scup_keep_sum_NJ, scup_keep_sum_pr_NJ, scup_keep_sum_fh_NJ, scup_keep_sum_shore_NJ, 
      #Scup release
      scup_rel_sum_NJ, scup_rel_sum_pr_NJ, scup_rel_sum_fh_NJ, scup_rel_sum_shore_NJ, 
      #Sum of number of trips
      ntrips_sum_NJ, ntrips_sum_pr_NJ, ntrips_sum_fh_NJ, ntrips_sum_shore_NJ
      ))
    
  }else{
    print("prediction_output_by_period1 is numeric")
  }
}
predictions_all<-list()
predictions_all<-rlist::list.rbind(predictions)

predictions_all2<-as.data.frame(predictions_all) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "colname", values_to = "value") %>% 
  janitor::clean_names() %>% 
  dplyr::group_by(colname) %>% 
  dplyr::summarise(across(where(is.numeric), mean))
#write.csv(predictions_all2, file = "output_save_testing3.csv")


#### Length #########
length_out<- prediction_output_by_period1 %>% 
  dplyr::select(-c(month, day, mode, kod, kod_24, n_choice_occasions, tripid, expand, change_CS, state,
                   probA, prob0, tot_keep_sf, tot_rel_sf, tot_keep_bsb, tot_rel_bsb, tot_keep_scup,
                   tot_rel_scup, tot_scup_catch, tot_keep_sf_base, tot_keep_bsb_base, tot_cat_scup_base)) %>%
  dplyr::slice_head(n = 1)

lw_params <- read.csv(here::here("data-raw/lw_params2.csv")) %>% 
  dplyr::mutate(Month = as.numeric(Month))

length_weight_conv<-length_out %>% 
  tidyr::pivot_longer(everything() , names_to = "SppLength", values_to = "NumInd") %>% 
  tidyr::separate(SppLength, into = c("Spp", "keep_rel", "mode", "Month", "Length"), sep = "_") %>% 
  dplyr::filter(!Spp == "NA") %>% 
  dplyr::mutate(Month = as.numeric(Month), 
                Lcm = as.numeric(Length) * 2.54) %>% 
  dplyr::left_join(lw_params, by = c("Spp", "Month"), relationship = "many-to-many") %>% 
  dplyr::filter(State == "NJ") %>% 
  dplyr::mutate(Wkg = dplyr::case_when(Spp == "SF" ~ (a * Lcm ^ b),
                                Spp == "SCUP" & Month %in% c(1, 2,3,4,5,12) ~ (exp(a + b *log(Lcm))), 
                                Spp == "SCUP" & Month %in% c(6:11) ~ (exp(a + b *log(Lcm))), 
                                Spp == "BSB" & Month %in% c(1:6) ~ (a * Lcm ^ b), 
                                Spp == "BSB" & Month %in% c(7:12) ~ (a * Lcm ^ b)), 
                Wlbs = Wkg * 2.20462, 
                Tot_W = NumInd * Wlbs) %>%
  dplyr::group_by(Spp, keep_rel, mode) %>% 
  dplyr::summarise(NumInd = sum(NumInd), 
                   Tot_W = sum(Tot_W))







