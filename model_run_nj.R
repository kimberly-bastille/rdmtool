##############################
### NJ Rec model run  ########
##############################

print("start model_NJ")
state1 = "NJ"
predictions_all = list()

#### p_stars old ####
# p_star_sf_NJ_variable<- 0.89
# p_star_bsb_NJ_variable<- 0.885
# p_star_scup_NJ_variable<- 0.045

#### p_stars new ######
# p_star_sf_NJ_variable<- 0.72
# p_star_bsb_NJ_variable<- 0.72
# p_star_scup_NJ_variable<- -0.11
# p_star_sf<- 0.89
# p_star_bsb<- 0.885
# p_star_scup<- 0.045

sf_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/fluke_projected_catch_at_lengths.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(state=="NJ")

bsb_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/bsb_projected_catch_at_lengths.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(state=="NJ")

scup_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/scup_projected_catch_at_lengths.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(state=="NJ")

l_w_conversion <-readr::read_csv(file.path(here::here("data-raw/size_data/L_W_Conversion.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(State=="NJ")

s_star_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/s_star_NJ.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(state=="NJ")

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

for(x in 1:100){
#future::plan(future::multisession, workers = 10)
#get_predictions_out<- function(x){
  
  
  print(x)
  
  catch_files_NJ<- read.csv(file.path(here::here(paste0("data-raw/catch/",state1," catch draws 2022 draw4 ", x, ".csv")))) %>% 
    #dplyr::filter(mode1 == select_mode) %>% 
    dplyr::rename(tot_sf_catch = tot_cat_sf,
                  tot_bsb_catch = tot_cat_bsb,
                  tot_scup_catch = tot_cat_scup, 
                  keep_sf = landing_sf, 
                  keep_bsb = landing_bsb, 
                  keep_scup = landing_scup)  %>%
    dplyr::mutate(day = as.numeric(stringr::str_extract(day , "^\\d{2}")),
                  period2 = paste0(month, "_", day, "_", mode1)) %>% 
    dplyr::select(!c("landing_sf_new","landing_scup_new","landing_bsb_new","tot_cat_bsb_new" ))
  
  calibration_output_by_period<- readRDS(here::here(paste0("data-raw/calibration/pds_NJ_",x,"_test.rds"))) %>% 
    tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>% 
    dplyr::filter(!day == "NA") %>% 
    dplyr::mutate(month_day = stringr::str_remove(lubridate::make_date("2023", month, day), "2023-"), 
                  period2 = paste0(month_day, "-", mode)) %>% 
    dplyr::select(-c(month, day, month_day, mode))
  
  costs_new_all<- readRDS(here::here(paste0("data-raw/calibration/costs_NJ_",x,"_test.rds")))%>% 
    tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>% 
    dplyr::filter(!day == "NA") %>%
    dplyr::mutate(month_day = stringr::str_remove(lubridate::make_date("2023", month, day), "2023-"), 
                  period2 = paste0(month_day, "-", mode)) %>% 
    dplyr::select(-c(month, day, month_day, mode))
  
  # calibration_data_table_base <- split(calibration_output_by_period, calibration_output_by_period$state)
  # cost_files_all_base <- split(costs_new_all, costs_new_all$state)
  
  
  directed_trips2 <- directed_trips %>% 
    dplyr::filter(draw == x) %>% 
    dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
                  #month = as.numeric(month), 
                  period2 = paste0(month24, "-", day, "-", mode))
  
  ######### Setup ##########################################
  sf_size_data <- sf_size_dat %>% 
    dplyr::filter(draw == 0) #Change to X for model for sf and scup
  
  bsb_size_data <- bsb_size_dat %>% 
    dplyr::filter(draw == 0)
  
  scup_size_data <- scup_size_dat %>% 
    dplyr::filter(draw == 0)
  
  print("made it through data read in ")

  s_star_data <- s_star_dat %>% 
    dplyr::filter(draw == x)
  #print(directed_trips2)
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
  #source(here::here("R/predict_rec_catch.R"))
  
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
                           calibration_data_table = calibration_output_by_period,
                           directed_trips_table = directed_trips2,
                           sf_size_data_read = sf_size_data,
                           bsb_size_data_read = bsb_size_data,
                           scup_size_data_read = scup_size_data,
                           costs_new_all = costs_new_all,
                           l_w = l_w_conversion,
                           s_star = s_star_data, 
                           #sf_catch_data_all = c(list(catch_files_NJ[[1]])))
                           sf_catch_data_all = c(list(catch_files_NJ)))
  
    predictions_all <- predictions_all %>% 
      rbind(test)

}
#})
# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
#predictions_out10<- furrr::future_map_dfr(1:10, ~get_predictions_out(.), .id = "run_number")


predictions <- predictions_all %>% 
  dplyr::group_by(Category, 
                  mode, 
                  keep_release, 
                  param, 
                  number_weight,
                  state) %>% 
  dplyr::summarise(Value = mean(Value))
#predictions <- predictions_out

# predictions_all2<-as.data.frame(predictions_all) %>% 
#   tidyr::pivot_longer(cols = everything(), names_to = "colname", values_to = "value") %>% 
#   janitor::clean_names() %>% 
#   dplyr::group_by(colname) %>% 
#   dplyr::summarise(across(where(is.numeric), mean)) #%>% 
#   #dplyr::mutate(value = dplyr::across(value, ~ format(., big.mark = ",")))
# #write.csv(predictions_all2, file = "output_NJ_test1.csv")
# 
# 
# # # #### Length #########
# # length_out<- prediction_output_by_period2 %>%
# #   dplyr::select(-c(period2, kod, kod_24, n_choice_occasions, tripid, expand, change_CS, state,
# #                    probA, prob0, tot_keep_sf, tot_rel_sf, tot_keep_bsb, tot_rel_bsb, tot_keep_scup,
# #                    tot_rel_scup, tot_scup_catch, tot_keep_sf_base, tot_keep_bsb_base, tot_cat_scup_base)) %>%
# #   dplyr::slice_head(n = 1) 
# #   
# # 
# # lw_params <- read.csv(here::here("data-raw/lw_params2.csv")) %>%
# #   dplyr::mutate(Month = as.numeric(Month))
# # 
# # length_weight_conv<-length_out %>%
# #   tidyr::pivot_longer(everything() , names_to = "SppLength", values_to = "NumInd") %>%
# #   tidyr::separate(SppLength, into = c("Spp", "keep_rel", "mode", "Month", "Length"), sep = "_") %>%
# #   dplyr::filter(!Spp == "NA") %>%
# #   dplyr::mutate(Month = as.numeric(Month),
# #                 Lcm = as.numeric(Length) * 2.54) %>%
# #   dplyr::left_join(lw_params, by = c("Spp", "Month"), relationship = "many-to-many") %>%
# #   dplyr::filter(State == "NJ") %>%
# #   dplyr::mutate(Wkg = dplyr::case_when(Spp == "SF" ~ (a * Lcm ^ b),
# #                                 Spp == "SCUP" & Month %in% c(1, 2,3,4,5,12) ~ (exp(a + b *log(Lcm))),
# #                                 Spp == "SCUP" & Month %in% c(6:11) ~ (exp(a + b *log(Lcm))),
# #                                 Spp == "BSB" & Month %in% c(1:6) ~ (a * Lcm ^ b),
# #                                 Spp == "BSB" & Month %in% c(7:12) ~ (a * Lcm ^ b)),
# #                 Wlbs = Wkg * 2.20462,
# #                 Tot_W = NumInd * Wlbs) %>%
# #   dplyr::group_by(Spp, keep_rel, mode) %>%
# #   dplyr::summarise(NumInd = sum(NumInd),
# #                    Tot_W = sum(Tot_W))
# # 
# # 



