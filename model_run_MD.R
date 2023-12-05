##############################
### DE Rec model run  ########
##############################

print("start model_MD")
state1 = "MD"
predictions_all = list()

sf_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/fluke_prob_star_2024_MD.csv")),  show_col_types = FALSE) 

bsb_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/bsb_prob_star_2022_MD.csv")),  show_col_types = FALSE)

scup_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/scup_prob_star_2024_MD.csv")),  show_col_types = FALSE)

l_w_conversion <-readr::read_csv(file.path(here::here("data-raw/size_data/L_W_Conversion.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(State=="MD") %>% 
  dplyr::mutate(ln_a = as.numeric(ln_a))


directed_trips<-readRDS(file.path(here::here(paste0("data-raw/directed_trips/directed_trips_MD.rds")))) 

if(input$SF_MD_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag1=dplyr::case_when(day_i >= lubridate::yday(input$SFmd_seas1[1]) & day_i <= lubridate::yday(input$SFmd_seas1[2]) ~ as.numeric(input$SFmd_1_bag), TRUE ~ 0), 
      fluke_min1=dplyr::case_when(day_i >= lubridate::yday(input$SFmd_seas1[1]) & day_i <= lubridate::yday(input$SFmd_seas1[2]) ~ as.numeric(input$SFmd_1_len[1]), TRUE ~ 100), 
      
      fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFmdFH_seas2[1]) & day_i <= lubridate::yday(input$SFmdFH_seas2[2]) ~ as.numeric(input$SFmdFH_2_bag), TRUE ~ fluke_bag1),
      fluke_bag1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFmdPR_seas2[1]) & day_i <= lubridate::yday(input$SFmdPR_seas2[2]) ~ as.numeric(input$SFmdPR_2_bag), TRUE ~ fluke_bag1),
      fluke_bag1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFmdSH_seas2[1]) & day_i <= lubridate::yday(input$SFmdSH_seas2[2]) ~ as.numeric(input$SFmdSH_2_bag), TRUE ~ fluke_bag1), 
      
      fluke_min1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFmdFH_seas2[1]) & day_i <= lubridate::yday(input$SFmdFH_seas2[2]) ~ as.numeric(input$SFmdFH_2_len[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFmdPR_seas2[1]) & day_i <= lubridate::yday(input$SFmdPR_seas2[2]) ~ as.numeric(input$SFmdPR_2_len[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFmdSH_seas2[1]) & day_i <= lubridate::yday(input$SFmdSH_seas2[2]) ~ as.numeric(input$SFmdSH_2_len[1]), TRUE ~ fluke_min1))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFmdFH_seas1[1]) & day_i <= lubridate::yday(input$SFmdFH_seas1[2]) ~ as.numeric(input$SFmdFH_1_bag), TRUE ~ 0), 
      fluke_bag1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFmdPR_seas1[1]) & day_i <= lubridate::yday(input$SFmdPR_seas1[2]) ~ as.numeric(input$SFmdPR_1_bag), TRUE ~ fluke_bag1), 
      fluke_bag1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFmdSH_seas1[1]) & day_i <= lubridate::yday(input$SFmdSH_seas1[2]) ~ as.numeric(input$SFmdSH_1_bag), TRUE ~ fluke_bag1),
      
      fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFmdFH_seas2[1]) & day_i <= lubridate::yday(input$SFmdFH_seas2[2]) ~ as.numeric(input$SFmdFH_2_bag), TRUE ~ fluke_bag1),
      fluke_bag1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFmdPR_seas2[1]) & day_i <= lubridate::yday(input$SFmdPR_seas2[2]) ~ as.numeric(input$SFmdPR_2_bag), TRUE ~ fluke_bag1),
      fluke_bag1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFmdSH_seas2[1]) & day_i <= lubridate::yday(input$SFmdSH_seas2[2]) ~ as.numeric(input$SFmdSH_2_bag), TRUE ~ fluke_bag1), 
      
      fluke_min1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFmdFH_seas1[1]) & day_i <= lubridate::yday(input$SFmdFH_seas1[2]) ~ as.numeric(input$SFmdFH_1_len[1]), TRUE ~ 100), 
      fluke_min1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFmdPR_seas1[1]) & day_i <= lubridate::yday(input$SFmdPR_seas1[2]) ~ as.numeric(input$SFmdPR_1_len[1]), TRUE ~ fluke_min1), 
      fluke_min1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFmdSH_seas1[1]) & day_i <= lubridate::yday(input$SFmdSH_seas1[2]) ~ as.numeric(input$SFmdSH_1_len[1]), TRUE ~ fluke_min1),
      
      fluke_min1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFmdFH_seas2[1]) & day_i <= lubridate::yday(input$SFmdFH_seas2[2]) ~ as.numeric(input$SFmdFH_2_len[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFmdPR_seas2[1]) & day_i <= lubridate::yday(input$SFmdPR_seas2[2]) ~ as.numeric(input$SFmdPR_2_len[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFmdSH_seas2[1]) & day_i <= lubridate::yday(input$SFmdSH_seas2[2]) ~ as.numeric(input$SFmdSH_2_len[1]), TRUE ~ fluke_min1))
  
}



if(input$BSB_MD_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#black sea bass
      bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBmd_seas1[1]) & day_i <= lubridate::yday(input$BSBmd_seas1[2]) ~ as.numeric(input$BSBmd_1_bag), TRUE ~ 0), 
      bsb_min=dplyr::case_when(day_i >= lubridate::yday(input$BSBmd_seas1[1]) & day_i <= lubridate::yday(input$BSBmd_seas1[2]) ~ as.numeric(input$BSBmd_1_len[1]), TRUE ~ 100), 
      bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBmd_seas2[1]) & day_i <= lubridate::yday(input$BSBmd_seas2[2]) ~ as.numeric(input$BSBmd_2_bag), TRUE ~ bsb_bag), 
      bsb_min=dplyr::case_when(day_i >= lubridate::yday(input$BSBmd_seas2[1]) & day_i <= lubridate::yday(input$BSBmd_seas2[2]) ~ as.numeric(input$BSBmd_2_len[1]), TRUE ~ bsb_min),
      
      bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas3[2]) ~ as.numeric(input$BSBmdFH_3_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas3[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas3[2]) ~ as.numeric(input$BSBmdPR_3_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas3[2]) ~ as.numeric(input$BSBmdSH_3_bag), TRUE ~ bsb_bag), 
      
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas3[2]) ~ as.numeric(input$BSBmdFH_3_len[1]), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas3[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas3[2]) ~ as.numeric(input$BSBmdPR_3_len[1]), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas3[2]) ~ as.numeric(input$BSBmdSH_3_len[1]), TRUE ~ bsb_min))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas1[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas1[2]) ~ as.numeric(input$BSBmdFH_1_bag), TRUE ~ 0), 
      bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas1[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas1[2]) ~ as.numeric(input$BSBmdPR_1_bag), TRUE ~ bsb_bag), 
      bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas1[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas1[2]) ~ as.numeric(input$BSBmdSH_1_bag), TRUE ~ bsb_bag),
      
      bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas2[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas2[2]) ~ as.numeric(input$BSBmdFH_2_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas2[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas2[2]) ~ as.numeric(input$BSBmdPR_2_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas2[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas2[2]) ~ as.numeric(input$BSBmdSH_2_bag), TRUE ~ bsb_bag), 
      
      bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas3[2]) ~ as.numeric(input$BSBmdFH_3_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas3[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas3[2]) ~ as.numeric(input$BSBmdPR_3_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas3[2]) ~ as.numeric(input$BSBmdSH_3_bag), TRUE ~ bsb_bag),
      
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas1[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas1[2]) ~ as.numeric(input$BSBmdFH_1_len[1]), TRUE ~ 100), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas1[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas1[2]) ~ as.numeric(input$BSBmdPR_1_len[1]), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas1[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas1[2]) ~ as.numeric(input$BSBmdSH_1_len[1]), TRUE ~ bsb_min),
      
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas2[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas2[2]) ~ as.numeric(input$BSBmdFH_2_len[1]), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas2[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas2[2]) ~ as.numeric(input$BSBmdPR_2_len[1]), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas2[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas2[2]) ~ as.numeric(input$BSBmdSH_2_len[1]), TRUE ~ bsb_min), 
      
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBmdFH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdFH_seas3[2]) ~ as.numeric(input$BSBmdFH_3_len[1]), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBmdPR_seas3[1]) & day_i <= lubridate::yday(input$BSBmdPR_seas3[2]) ~ as.numeric(input$BSBmdPR_3_len[1]), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBmdSH_seas3[1]) & day_i <= lubridate::yday(input$BSBmdSH_seas3[2]) ~ as.numeric(input$BSBmdSH_3_len[1]), TRUE ~ bsb_min))
  
}




if(input$SCUP_MD_input_type == "All Modes Combined"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Scup
      scup_bag=dplyr::case_when(day_i >= lubridate::yday(input$SCUPmd_seas1[1]) & day_i <= lubridate::yday(input$SCUPmd_seas1[2]) ~ as.numeric(input$SCUPmd_1_bag), TRUE ~ 0), 
      scup_min=dplyr::case_when(day_i >= lubridate::yday(input$SCUPmd_seas1[1]) & day_i <= lubridate::yday(input$SCUPmd_seas1[2]) ~ as.numeric(input$SCUPmd_1_len[1]), TRUE ~ 100), 
      
      scup_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPmdFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdFH_seas2[2]) ~ as.numeric(input$SCUPmdFH_2_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPmdPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdPR_seas2[2]) ~ as.numeric(input$SCUPmdPR_2_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPmdSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdSH_seas2[2]) ~ as.numeric(input$SCUPmdSH_2_bag), TRUE ~ scup_bag), 
      
      scup_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPmdFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdFH_seas2[2]) ~ as.numeric(input$SCUPmdFH_2_len[1]), TRUE ~ scup_min),
      scup_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPmdPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdPR_seas2[2]) ~ as.numeric(input$SCUPmdPR_2_len[1]), TRUE ~ scup_min),
      scup_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPmdSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdSH_seas2[2]) ~ as.numeric(input$SCUPmdSH_2_len[1]), TRUE ~ scup_min))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      scup_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPmdFH_seas1[1]) & day_i <= lubridate::yday(input$SCUPmdFH_seas1[2]) ~ as.numeric(input$SCUPmdFH_1_bag), TRUE ~ 0), 
      scup_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPmdPR_seas1[1]) & day_i <= lubridate::yday(input$SCUPmdPR_seas1[2]) ~ as.numeric(input$SCUPmdPR_1_bag), TRUE ~ scup_bag), 
      scup_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPmdSH_seas1[1]) & day_i <= lubridate::yday(input$SCUPmdSH_seas1[2]) ~ as.numeric(input$SCUPmdSH_1_bag), TRUE ~ scup_bag),
      
      scup_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPmdFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdFH_seas2[2]) ~ as.numeric(input$SCUPmdFH_2_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPmdPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdPR_seas2[2]) ~ as.numeric(input$SCUPmdPR_2_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPmdSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdSH_seas2[2]) ~ as.numeric(input$SCUPmdSH_2_bag), TRUE ~ scup_bag), 
      
      scup_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPmdFH_seas1[1]) & day_i <= lubridate::yday(input$SCUPmdFH_seas1[2]) ~ as.numeric(input$SCUPmdFH_1_len[1]), TRUE ~ 100), 
      scup_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPmdPR_seas1[1]) & day_i <= lubridate::yday(input$SCUPmdPR_seas1[2]) ~ as.numeric(input$SCUPmdPR_1_len[1]), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPmdSH_seas1[1]) & day_i <= lubridate::yday(input$SCUPmdSH_seas1[2]) ~ as.numeric(input$SCUPmdSH_1_len[1]), TRUE ~ scup_min),
      
      scup_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPmdFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdFH_seas2[2]) ~ as.numeric(input$SCUPmdFH_2_len[1]), TRUE ~ scup_min),
      scup_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPmdPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdPR_seas2[2]) ~ as.numeric(input$SCUPmdPR_2_len[1]), TRUE ~ scup_min),
      scup_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPmdSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPmdSH_seas2[2]) ~ as.numeric(input$SCUPmdSH_2_len[1]), TRUE ~ scup_min))
}




future::plan(future::multisession, workers = 36)
#future::plan(future::multisession, workers = 3)
get_predictions_out<- function(x){
  
  
  print(x)
  
  catch_files_MD<- readr::read_csv(file.path(here::here(paste0("data-raw/catch2024/", state1, " catch draws 2024 draw4 ",x ,".csv")))) %>% 
    dplyr::rename(tot_sf_catch = tot_cat_sf,
                  tot_bsb_catch = tot_cat_bsb,
                  tot_scup_catch = tot_cat_scup, 
                  keep_sf = landing_sf, 
                  keep_bsb = landing_bsb, 
                  keep_scup = landing_scup)  %>%
    dplyr::mutate(day = as.numeric(stringr::str_extract(day , "^\\d{2}")),
                  period2 = paste0(month, "_", day, "_", mode1)) %>% 
    dplyr::select(!c("landing_sf_new","landing_scup_new","landing_bsb_new","tot_cat_bsb_new" ))
  
  calibration_output_by_period<- readRDS(here::here(paste0("data-raw/calibration/pds_MD_",x,"_test1.rds")))
  
  costs_new_all<- readRDS(here::here(paste0("data-raw/calibration/costs_MD_",x,"_test1.rds")))
  
  # calibration_data_table_base <- split(calibration_output_by_period, calibration_output_by_period$state)
  # cost_files_all_base <- split(costs_new_all, costs_new_all$state)
  
  
  directed_trips2 <- directed_trips %>% 
    dplyr::filter(draw == x) %>% 
    dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
                  #month = as.numeric(month), 
                  period2 = paste0(month24, "-", day, "-", mode))
  
  ######### Setup ##########################################
  sf_size_data <- sf_size_dat %>% 
    dplyr::filter(draw == x) #Change to X for model for sf and scup
  
  bsb_size_data <- bsb_size_dat %>% 
    dplyr::filter(draw == x)
  
  scup_size_data <- scup_size_dat %>% 
    dplyr::filter(draw == x)
  
  
  ##Run the catch function
  source(here::here("R/predict_rec_catch3.R"))
  
  test<- predict_rec_catch(state1 = c("MD"),
                           calibration_data_table = calibration_output_by_period,
                           directed_trips_table = directed_trips2,
                           sf_size_data_read = sf_size_data,
                           bsb_size_data_read = bsb_size_data,
                           scup_size_data_read = scup_size_data,
                           costs_new_all = costs_new_all,
                           l_w = l_w_conversion,
                           x = x,
                           sf_catch_data_all = c(list(catch_files_MD)))
  
  print("test")
  print(test)  
  #write.csv(directed_trips2, file = paste0("directed_", x, ".csv"))
}
#})
# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 


predictions_out10<- furrr::future_map_dfr(1:100, ~get_predictions_out(.), .id = "draw")
#predictions_out10<- furrr::future_map_dfr(1:3, ~get_predictions_out(.), .id = "draw")


predictions_out10<- predictions_out10 %>%
  dplyr::filter(!mode == "all")



StatusQuo <- openxlsx::read.xlsx(here::here("data-raw/StatusQuo/SQ_projections_11_9_MD.xlsx")) %>% 
  dplyr::rename(value_SQ = Value)


predictions_merge <- predictions_out10 %>% #predictions_out10 %>% 
  dplyr::rename(value_alt= Value) %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::left_join(StatusQuo, by = c("Category","mode", "keep_release","param" ,"number_weight","state", "draw")) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup"),
                mode!="all", 
                keep_release=="keep", 
                number_weight %in% c("Weight_avg", "Weight") ) %>% 
  dplyr::select(-param) %>% 
  dplyr::mutate(value_SQ = as.numeric(value_SQ), 
                value_alt = as.numeric(value_alt))

predictions_weight <- predictions_merge %>%
  dplyr::filter(number_weight == "Weight") %>%
  dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
  dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))

predictions_avg <- predictions_merge %>%
  dplyr::filter(number_weight == "Weight_avg") 

predictions_merge2<- predictions_avg %>% 
  dplyr::left_join(predictions_weight, by = c("Category","mode", "state","draw")) %>%
  dplyr::select(-keep_release.x, -keep_release.y, -number_weight.y) %>% 
  dplyr::mutate(imputed_value_alt= perc_change_weight/100,
                imputed_value_alt = value_SQ * imputed_value_alt, 
                imputed_value_alt=imputed_value_alt+value_SQ) 
#check = ((imputed_value_alt-value_SQ)/value_SQ)*100)

predictions_merge2<- predictions_merge2 %>% 
  #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>% 
  #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       is.na(value_alt_Weight) & 
                                                       value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))
state_harvest_output<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category, state) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_harvest_output <- state_harvest_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() 

state_harvest_output<- state_harvest_output %>% 
  dplyr::mutate(harv_target=dplyr::case_when(Category=="scup"~.9*value_SQ_sum, TRUE~NA), 
                harv_target=dplyr::case_when(Category=="sf"~.72*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=dplyr::case_when(imputed_value_alt_sum<=harv_target  ~1, TRUE~0))

categories_state=list()

for(d in unique(state_harvest_output$domain)){
  
  new<- state_harvest_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  reach_target<- sum(new$reach_target)
  
  
  categories_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
    ))
  
  categories_state[[d]]$domain<-d
  
  
}
state_harvest_results= rlist::list.stack(categories_state, fill=TRUE)
state_harvest_results<- state_harvest_results %>% 
  tidyr::separate(domain, into = c("region", "species"))  %>% 
  dplyr::mutate(stat="harvest pounds", 
                mode="all modes") %>% 
  dplyr::relocate(region, stat, species, mode)


###########################
#state by mode-level output
state_mode_harvest_output<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category, state, mode) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_mode_harvest_output_check<- state_mode_harvest_output %>% 
  dplyr::filter(perc_diff=="NaN")

state_mode_harvest_output <- state_mode_harvest_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

state_mode_harvest_output<- state_mode_harvest_output %>% 
  dplyr::mutate(harv_target=dplyr::case_when(Category=="scup"~.9*value_SQ_sum, TRUE~NA), 
                harv_target=dplyr::case_when(Category=="sf"~.72*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=dplyr::case_when(imputed_value_alt_sum<=harv_target~1, TRUE~0))

categories_state_mode=list()

for(d in unique(state_mode_harvest_output$domain)){
  
  new<- state_mode_harvest_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  reach_target<- sum(new$reach_target)
  
  
  
  categories_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
    ))
  
  categories_state_mode[[d]]$domain<-d
  
  
}
state_mode_harvest_results= rlist::list.stack(categories_state_mode, fill=TRUE)
state_mode_harvest_results<- state_mode_harvest_results %>% 
  tidyr::separate(domain, into = c("region", "species", "mode"))  %>% 
  dplyr::mutate(stat="harvest pounds") 



### Release

predictions_releases_merge <- predictions_out10 %>% 
  dplyr::rename(value_alt= Value) %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt)) %>% 
  dplyr::filter(keep_release %in% c("release", "Discmortality") )

predictions_release_weight <- predictions_releases_merge %>%
  dplyr::filter(number_weight == "Weight") %>%
  dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
  dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))

predictions_release_avg <- predictions_releases_merge %>%
  dplyr::filter(number_weight == "Weight_avg") 



predictions_releases_merge2<- predictions_release_avg %>% 
  dplyr::left_join(predictions_release_weight, by = c("Category","mode", "state","draw", "keep_release")) %>%
  dplyr::select(-number_weight.y) %>% 
  dplyr::mutate(imputed_value_alt= perc_change_weight/100,
                imputed_value_alt = value_SQ * imputed_value_alt, 
                imputed_value_alt=imputed_value_alt+value_SQ)  
#check = ((imputed_value_alt-value_SQ)/value_SQ)*100)

predictions_releases_merge2<- predictions_releases_merge2 %>% 
  #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>% 
  #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       is.na(value_alt_Weight) & 
                                                       value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))  

state_release_output<- predictions_releases_merge2 %>% 
  dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(domain, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_release_output <- state_release_output %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() 

categories_release_state=list()

for(d in unique(state_release_output$domain)){
  
  new<- state_release_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_release_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_release_state[[d]]$domain<-d
  
  
}
state_release_results= rlist::list.stack(categories_release_state, fill=TRUE)

state_release_results<- state_release_results %>% 
  tidyr::separate(domain, into = c("species", "region", "stat1")) %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)

###########################
#state by mode-level release output
state_mode_release_output<- predictions_releases_merge2 %>% 
  dplyr::group_by(draw, Category, state, mode, keep_release) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_release_output <- state_mode_release_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

categories_releases_state_mode=list()

for(d in unique(state_mode_release_output$domain)){
  
  new<- state_mode_release_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_releases_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_releases_state_mode[[d]]$domain<-d
  
  
}
state_mode_release_results= rlist::list.stack(categories_releases_state_mode, fill=TRUE)
state_mode_release_results<- state_mode_release_results %>% 
  tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat)) %>% 
  dplyr::select(-stat1)

release_ouput<- state_release_results %>% rbind(state_mode_release_results) 


### CV
CV_state_mode <- predictions_out10 %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>%
  dplyr::rename(value_alt= Value) %>% 
  dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt)) %>% 
  dplyr::filter(Category %in% c("CV", "ntrips"),
                mode!="all" ) %>% 
  dplyr::select(!c(param.x, param.y, keep_release.x, keep_release.y)) %>% 
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt), 
                perc_diff=((value_alt-value_SQ)/value_SQ)*100)

state_CV<- CV_state_mode %>% 
  dplyr::group_by(draw, Category, state) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=(value_SQ_sum-value_alt_sum)) 


#sort observations and create index by species
state_CV<- state_CV %>% 
  dplyr::group_by(Category, state) %>% 
  dplyr::arrange(Category,state, value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(Category)) %>% 
  dplyr::arrange(Category,state, perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(Category)) %>% 
  dplyr::arrange(Category,state,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(Category)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Category,state,perc_diff) %>% 
  dplyr::mutate(domain=paste0(state, "_", Category))



state_Cvs=list()

for(d in unique(state_CV$domain)){
  
  new<- state_CV %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  state_Cvs[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  state_Cvs[[d]]$domain<-d
  
  
}
state_CV_results= rlist::list.stack(state_Cvs, fill=TRUE)   
state_CV_results<- state_CV_results %>% 
  tidyr::separate(domain, into = c("region", "stat")) %>% 
  dplyr::mutate(mode="all modes", species="all species")



###########################
#state mode-level CV output
state_mode_CV_output<- CV_state_mode %>% 
  dplyr::group_by(draw, Category, state, mode) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=value_SQ_sum-value_alt_sum) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_CV_output <- state_mode_CV_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

categories_CV_state_mode=list()

for(d in unique(state_mode_CV_output$domain)){
  
  new<- state_mode_CV_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_CV_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_CV_state_mode[[d]]$domain<-d
  
  
}
state_mode_CV_results= rlist::list.stack(categories_CV_state_mode, fill=TRUE)
state_mode_CV_results<- state_mode_CV_results %>% 
  tidyr::separate(domain, into = c("region", "stat",  "mode"))  %>% 
  dplyr::mutate(species="all species")



##### Numbers 
alt<- predictions_out10 %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::rename(value_alt = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Number") ) %>% 
  dplyr::select(-param)


StatusQuo <- rbind(StatusQuo) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Number") ) 

predictions_harv_num_merge <- alt %>% 
  dplyr::left_join(StatusQuo, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt))

# predictions_harv_num_merge_MA <- predictions_harv_num_merge %>% 
#   dplyr::filter(state=="MA") %>%
#   dplyr::mutate(value_SQ=value_SQ-diff, value_alt=value_alt-diff) %>% 
#   dplyr::mutate(perc_diff=((value_alt_adj-value_SQ_adj)/value_SQ_adj)*100) %>%
#   dplyr::mutate(perc_diff=dplyr::case_when(is.nan(perc_diff) & value_SQ_adj==0 & value_alt_adj==0~0, TRUE~perc_diff)) %>% 
#   dplyr::mutate(imputed_value_alt=dplyr::case_when(is.nan(imputed_value_alt) & value_SQ_adj==0 & value_alt_adj==0~0, TRUE~imputed_value_alt)) %>% 
#   dplyr::select(Category, mode, keep_release, number_weight, value_SQ, imputed_value_alt, state, draw) %>% 
#   dplyr::rename(value_alt=imputed_value_alt)
# 
# 
# 
# predictions_harv_num_merge<-predictions_harv_num_merge %>% 
#   dplyr::filter(state!="MA")
# 
# predictions_harv_num_merge<-rbind(predictions_harv_num_merge, predictions_harv_num_merge_NJ)




###########################
#state-level output
state_harv_num_output<- predictions_harv_num_merge %>% 
  dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(domain, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_harv_num_output <- state_harv_num_output %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() 

categories_harv_num_state=list()

for(d in unique(state_harv_num_output$domain)){
  
  new<- state_harv_num_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_harv_num_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_harv_num_state[[d]]$domain<-d
  
  
}
state_harv_num_results= rlist::list.stack(categories_harv_num_state, fill=TRUE)

state_harv_num_results<- state_harv_num_results %>% 
  tidyr::separate(domain, into = c("species", "region", "stat1")) %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=dplyr::case_when(stat1=="keep"~ "harvest numbers", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


###########################
#state by mode-level harvest num output
state_mode_harv_num_output<- predictions_harv_num_merge %>% 
  dplyr::group_by(draw, Category, state, mode, keep_release) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 & value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_harv_num_output <- state_mode_harv_num_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  dplyr::mutate(n_weight = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  dplyr::mutate(n_perc = dplyr::row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  dplyr::mutate(n_SQ = dplyr::row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,value_SQ_sum) 

# state_mode_harv_num_output_check<-state_mode_harv_num_output %>% 
#   dplyr::filter(state=="NY" & Category=="scup" & keep_release=="keep" & mode=="fh")

categories_harv_num_state_mode=list()

for(d in unique(state_mode_harv_num_output$domain)){
  
  new<- state_mode_harv_num_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_harv_num_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_harv_num_state_mode[[d]]$domain<-d
  
  
}
state_mode_harv_num_results= rlist::list.stack(categories_harv_num_state_mode, fill=TRUE)
state_mode_harv_num_results<- state_mode_harv_num_results %>% 
  tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>% 
  dplyr::mutate(stat=dplyr::case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=dplyr::case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=dplyr::case_when(stat1=="keep"~ "harvest numbers", TRUE~stat)) %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


predictions<- plyr::rbind.fill(state_mode_harv_num_results, state_harv_num_results,
                               state_harvest_results, state_mode_harvest_results,release_ouput) %>% 
  dplyr::mutate(reach_target = as.character(reach_target),
                reach_target = dplyr::case_when(median_value_SQ == 0 ~ "Not Applicable", TRUE ~ reach_target),
                reach_target = dplyr::case_when(species == "bsb" ~ "No harvest target", TRUE ~ reach_target),
                reach_target = dplyr::case_when(stat == "harvest numbers" ~ "No harvest target", TRUE ~ reach_target),
                reach_target = dplyr::case_when(stat == "release numbers" ~ "No harvest target", TRUE ~ reach_target),
                reach_target = dplyr::case_when(stat == "dead release numbers" ~ "No harvest target", TRUE ~ reach_target),
                median_perc_diff = round(median_perc_diff, 2), 
                median_value_alt = round(median_value_SQ + ((median_perc_diff/100)* median_value_SQ), 0), 
                median_value_SQ = round(median_value_SQ, 0)) %>% 
  plyr::rbind.fill(state_CV_results, state_mode_CV_results) %>% 
  dplyr::mutate(species = dplyr::recode(species, "bsb"= "Black Sea Bass", "sf" = "Summer Flounder", "scup" = "Scup"), 
                mode = dplyr::recode(mode, "fh" = "For Hire", "pr" = "Private", "sh" = "Shore"), 
                median_perc_diff = prettyNum(median_perc_diff, big.mark = ",", scientific = FALSE),
                median_value_alt = prettyNum(median_value_alt, big.mark = ",", scientific = FALSE), 
                median_value_SQ = prettyNum(median_value_SQ, big.mark = ",", scientific = FALSE)) %>% 
  dplyr::select(region, stat, mode, species, median_value_SQ, median_value_alt, median_perc_diff, reach_target) %>% 
  dplyr::rename("State" = region,
                "Statistic" = stat,
                "Mode" = mode, 
                "Species" = species, 
                "Status-quo value (median)" = median_value_SQ, 
                "Alternative option value" = median_value_alt, 
                "% difference from status-quo outcome (median)" = median_perc_diff, 
                "% under harvest target (out of 100 simulations)" = reach_target)

