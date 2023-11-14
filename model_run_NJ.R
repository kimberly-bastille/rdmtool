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

sf_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/fluke_prob_star_2024_NJ.csv")),  show_col_types = FALSE) 

bsb_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/bsb_prob_star_2022_NJ.csv")),  show_col_types = FALSE)

scup_size_dat <- readr::read_csv(file.path(here::here("data-raw/size_data/scup_prob_star_2024_NJ.csv")),  show_col_types = FALSE)

l_w_conversion <-readr::read_csv(file.path(here::here("data-raw/size_data/L_W_Conversion.csv")),  show_col_types = FALSE) %>%
  dplyr::filter(State=="NJ") %>% 
  dplyr::mutate(ln_a = as.numeric(ln_a))


directed_trips<-readRDS(file.path(here::here(paste0("data-raw/directed_trips/directed_trips_NJ.rds")))) 

if(input$SF_NJ_input_type == "Single"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag1=dplyr::case_when(day_i >= lubridate::yday(input$SFnj_seas1[1]) & day_i <= lubridate::yday(input$SFnj_seas1[2]) ~ as.numeric(input$SFnj_1_smbag), TRUE ~ fluke_bag1), 
      fluke_bag2=dplyr::case_when(day_i >= lubridate::yday(input$SFnj_seas1[1]) & day_i <= lubridate::yday(input$SFnj_seas1[2]) ~ as.numeric(input$SFnj_1_lgbag), TRUE ~ fluke_bag2), 
      fluke_min1=dplyr::case_when(day_i >= lubridate::yday(input$SFnj_seas1[1]) & day_i <= lubridate::yday(input$SFnj_seas1[2]) ~ as.numeric(input$SFnj_1_smlen[1]), TRUE ~ fluke_min1), 
      fluke_min2=dplyr::case_when(day_i >= lubridate::yday(input$SFnj_seas1[1]) & day_i <= lubridate::yday(input$SFnj_seas1[2]) ~ as.numeric(input$SFnj_1_lglen[1]), TRUE ~ fluke_min2), 
      
      fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas2[1]) & day_i <= lubridate::yday(input$SFnjFH_seas2[2]) ~ as.numeric(input$SFnjFH_2_smbag), TRUE ~ fluke_bag1),
      fluke_bag1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas2[1]) & day_i <= lubridate::yday(input$SFnjPR_seas2[2]) ~ as.numeric(input$SFnjPR_2_smbag), TRUE ~ fluke_bag1),
      fluke_bag1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_smbag), TRUE ~ fluke_bag1),
      fluke_bag2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas2[1]) & day_i <= lubridate::yday(input$SFnjFH_seas2[2]) ~ as.numeric(input$SFnjFH_2_lgbag), TRUE ~ fluke_bag2),
      fluke_bag2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas2[1]) & day_i <= lubridate::yday(input$SFnjPR_seas2[2]) ~ as.numeric(input$SFnjPR_2_lgbag), TRUE ~ fluke_bag2),
      fluke_bag2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_lgbag), TRUE ~ fluke_bag2), 
      
      fluke_min1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas2[1]) & day_i <= lubridate::yday(input$SFnjFH_seas2[2]) ~ as.numeric(input$SFnjFH_2_smlen[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas2[1]) & day_i <= lubridate::yday(input$SFnjPR_seas2[2]) ~ as.numeric(input$SFnjPR_2_smlen[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_smlen[1]), TRUE ~ fluke_min1))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas1[1]) & day_i <= lubridate::yday(input$SFnjFH_seas1[2]) ~ as.numeric(input$SFnjFH_1_smbag), TRUE ~ fluke_bag1), 
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
      fluke_bag2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_lgbag), TRUE ~ fluke_bag2), 
      
      fluke_min1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas1[1]) & day_i <= lubridate::yday(input$SFnjFH_seas1[2]) ~ as.numeric(input$SFnjFH_1_smlen[1]), TRUE ~ fluke_min1), 
      fluke_min1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas1[1]) & day_i <= lubridate::yday(input$SFnjPR_seas1[2]) ~ as.numeric(input$SFnjPR_1_smlen[1]), TRUE ~ fluke_min1), 
      fluke_min1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas1[1]) & day_i <= lubridate::yday(input$SFnjSH_seas1[2]) ~ as.numeric(input$SFnjSH_1_smlen[1]), TRUE ~ fluke_min1),
      fluke_min2=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas1[1]) & day_i <= lubridate::yday(input$SFnjFH_seas1[2]) ~ as.numeric(input$SFnjFH_1_lglen[1]), TRUE ~ fluke_min2), 
      fluke_min2=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas1[1]) & day_i <= lubridate::yday(input$SFnjPR_seas1[2]) ~ as.numeric(input$SFnjPR_1_lglen[1]), TRUE ~ fluke_min2),
      fluke_min2=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas1[1]) & day_i <= lubridate::yday(input$SFnjSH_seas1[2]) ~ as.numeric(input$SFnjSH_1_lglen[1]), TRUE ~ fluke_min2),
      
      fluke_min1=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SFnjFH_seas2[1]) & day_i <= lubridate::yday(input$SFnjFH_seas2[2]) ~ as.numeric(input$SFnjFH_2_smlen[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SFnjPR_seas2[1]) & day_i <= lubridate::yday(input$SFnjPR_seas2[2]) ~ as.numeric(input$SFnjPR_2_smlen[1]), TRUE ~ fluke_min1),
      fluke_min1=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SFnjSH_seas2[1]) & day_i <= lubridate::yday(input$SFnjSH_seas2[2]) ~ as.numeric(input$SFnjSH_2_smlen[1]), TRUE ~ fluke_min1))
  
}


if(input$BSB_NJ_input_type == "Single"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(# Black Sea Bass Bag Limit
      bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas1[1]) & day_i <= lubridate::yday(input$BSBnj_seas1[2]) ~ as.numeric(input$BSBnj_1_bag), TRUE ~ bsb_bag), 
      bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas2[1]) & day_i <= lubridate::yday(input$BSBnj_seas2[2]) ~ as.numeric(input$BSBnj_2_bag), TRUE ~ bsb_bag), 
      bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas3[1]) & day_i <= lubridate::yday(input$BSBnj_seas3[2]) ~ as.numeric(input$BSBnj_3_bag), TRUE ~ bsb_bag), 
      bsb_bag=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas4[1]) & day_i <= lubridate::yday(input$BSBnj_seas4[2]) ~ as.numeric(input$BSBnj_4_bag), TRUE ~ bsb_bag),
      bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas5[2]) ~ as.numeric(input$BSBnjFH_5_bag), TRUE ~ bsb_bag), 
      bsb_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas5[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas5[2]) ~ as.numeric(input$BSBnjPR_5_bag), TRUE ~ bsb_bag), 
      bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas5[2]) ~ as.numeric(input$BSBnjSH_5_bag), TRUE ~ bsb_bag), 
      # Black Sea Bass Minimun Size
      bsb_min=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas1[1]) & day_i <= lubridate::yday(input$BSBnj_seas1[2]) ~ as.numeric(input$BSBnj_1_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas2[1]) & day_i <= lubridate::yday(input$BSBnj_seas2[2]) ~ as.numeric(input$BSBnj_2_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas3[1]) & day_i <= lubridate::yday(input$BSBnj_seas3[2]) ~ as.numeric(input$BSBnj_3_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(day_i >= lubridate::yday(input$BSBnj_seas4[1]) & day_i <= lubridate::yday(input$BSBnj_seas4[2]) ~ as.numeric(input$BSBnj_4_len), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas5[2]) ~ as.numeric(input$BSBnjFH_5_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas5[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas5[2]) ~ as.numeric(input$BSBnjPR_5_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas5[2]) ~ as.numeric(input$BSBnjSH_5_len), TRUE ~ bsb_min))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(# Black Sea Bass Bag Limit by Mode
      bsb_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas1[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas1[2]) ~ as.numeric(input$BSBnjFH_1_bag), TRUE ~ bsb_bag), 
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
      bsb_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas5[2]) ~ as.numeric(input$BSBnjSH_5_bag), TRUE ~ bsb_bag), 
      # Black Sea Bass Minimum Length by Mode
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas1[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas1[2]) ~ as.numeric(input$BSBnjFH_1_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas1[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas1[2]) ~ as.numeric(input$BSBnjPR_1_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas1[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas1[2]) ~ as.numeric(input$BSBnjSH_1_len), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas2[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas2[2]) ~ as.numeric(input$BSBnjFH_2_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas2[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas2[2]) ~ as.numeric(input$BSBnjPR_2_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas2[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas2[2]) ~ as.numeric(input$BSBnjSH_2_len), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas3[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas3[2]) ~ as.numeric(input$BSBnjFH_3_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas3[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas3[2]) ~ as.numeric(input$BSBnjPR_3_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas3[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas3[2]) ~ as.numeric(input$BSBnjSH_3_len), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas4[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas4[2]) ~ as.numeric(input$BSBnjFH_4_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas4[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas4[2]) ~ as.numeric(input$BSBnjPR_4_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas4[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas4[2]) ~ as.numeric(input$BSBnjSH_4_len), TRUE ~ bsb_min),
      bsb_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$BSBnjFH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjFH_seas5[2]) ~ as.numeric(input$BSBnjFH_5_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$BSBnjPR_seas5[1]) & day_i <= lubridate::yday(input$BSBnjPR_seas5[2]) ~ as.numeric(input$BSBnjPR_5_len), TRUE ~ bsb_min), 
      bsb_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$BSBnjSH_seas5[1]) & day_i <= lubridate::yday(input$BSBnjSH_seas5[2]) ~ as.numeric(input$BSBnjSH_5_len), TRUE ~ bsb_min))
}

if(input$SCUP_NJ_input_type == "Single"){
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      #SCUP
      scup_bag=dplyr::case_when(day_i >= lubridate::yday(input$SCUPnj_seas1[1]) & day_i <= lubridate::yday(input$SCUPnj_seas1[2]) ~ as.numeric(input$SCUPnj_1_bag), TRUE ~ scup_bag), 
      scup_min=dplyr::case_when(day_i >= lubridate::yday(input$SCUPnj_seas1[1]) & day_i <= lubridate::yday(input$SCUPnj_seas1[2]) ~ as.numeric(input$SCUPnj_1_len), TRUE ~ scup_min), 
      
      scup_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPnjFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjFH_seas2[2]) ~ as.numeric(input$SCUPnjFH_2_bag), TRUE ~ scup_bag), 
      scup_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPnjPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjPR_seas2[2]) ~ as.numeric(input$SCUPnjPR_2_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPnjSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjSH_seas2[2]) ~ as.numeric(input$SCUPnjSH_2_bag), TRUE ~ scup_bag),
      scup_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPnjFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjFH_seas2[2]) ~ as.numeric(input$SCUPnjFH_2_len), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPnjPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjPR_seas2[2]) ~ as.numeric(input$SCUPnjPR_2_len), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPnjSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjSH_seas2[2]) ~ as.numeric(input$SCUPnjSH_2_len), TRUE ~ scup_min))
} else {
  directed_trips<- directed_trips %>%  
    dplyr::mutate(
      scup_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPnjFH_seas1[1]) & day_i <= lubridate::yday(input$SCUPnjFH_seas1[2]) ~ as.numeric(input$SCUPnjFH_1_bag), TRUE ~ scup_bag), 
      scup_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPnjPR_seas1[1]) & day_i <= lubridate::yday(input$SCUPnjPR_seas1[2]) ~ as.numeric(input$SCUPnjPR_1_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPnjSH_seas1[1]) & day_i <= lubridate::yday(input$SCUPnjSH_seas1[2]) ~ as.numeric(input$SCUPnjSH_1_bag), TRUE ~ scup_bag),
      scup_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPnjFH_seas1[1]) & day_i <= lubridate::yday(input$SCUPnjFH_seas1[2]) ~ as.numeric(input$SCUPnjFH_1_len), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPnjPR_seas1[1]) & day_i <= lubridate::yday(input$SCUPnjPR_seas1[2]) ~ as.numeric(input$SCUPnjPR_1_len), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPnjSH_seas1[1]) & day_i <= lubridate::yday(input$SCUPnjSH_seas1[2]) ~ as.numeric(input$SCUPnjSH_1_len), TRUE ~ scup_min), 
      
      scup_bag=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPnjFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjFH_seas2[2]) ~ as.numeric(input$SCUPnjFH_2_bag), TRUE ~ scup_bag), 
      scup_bag=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPnjPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjPR_seas2[2]) ~ as.numeric(input$SCUPnjPR_2_bag), TRUE ~ scup_bag),
      scup_bag=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPnjSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjSH_seas2[2]) ~ as.numeric(input$SCUPnjSH_2_bag), TRUE ~ scup_bag),
      scup_min=dplyr::case_when(mode == "fh" & day_i >= lubridate::yday(input$SCUPnjFH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjFH_seas2[2]) ~ as.numeric(input$SCUPnjFH_2_len), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "pr" & day_i >= lubridate::yday(input$SCUPnjPR_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjPR_seas2[2]) ~ as.numeric(input$SCUPnjPR_2_len), TRUE ~ scup_min), 
      scup_min=dplyr::case_when(mode == "sh" & day_i >= lubridate::yday(input$SCUPnjSH_seas2[1]) & day_i <= lubridate::yday(input$SCUPnjSH_seas2[2]) ~ as.numeric(input$SCUPnjSH_2_len), TRUE ~ scup_min))
}



#for(x in 1:1){
future::plan(future::multisession, workers = 8)
get_predictions_out<- function(x){
  
  
  print(x)
  
  catch_files_NJ<- readr::read_csv(file.path(here::here(paste0("data-raw/catch2024/", state1, " catch draws 2024 draw4 ",x ,".csv")))) %>% 
    dplyr::rename(tot_sf_catch = tot_cat_sf,
                  tot_bsb_catch = tot_cat_bsb,
                  tot_scup_catch = tot_cat_scup, 
                  keep_sf = landing_sf, 
                  keep_bsb = landing_bsb, 
                  keep_scup = landing_scup)  %>%
    dplyr::mutate(day = as.numeric(stringr::str_extract(day , "^\\d{2}")),
                  period2 = paste0(month, "_", day, "_", mode1)) %>% 
    dplyr::select(!c("landing_sf_new","landing_scup_new","landing_bsb_new","tot_cat_bsb_new" ))
  
  calibration_output_by_period<- readRDS(here::here(paste0("data-raw/calibration/pds_NJ_",x,"_test1.rds")))
  
  costs_new_all<- readRDS(here::here(paste0("data-raw/calibration/costs_NJ_",x,"_test1.rds")))
  
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
  source(here::here("R/predict_rec_catch3.R"))
  
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
                           x = x,
                           #sf_catch_data_all = c(list(catch_files_NJ[[1]])))
                           sf_catch_data_all = c(list(catch_files_NJ)))
  
  
}
#})
# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
predictions_out10<- furrr::future_map_dfr(1:3, ~get_predictions_out(.), .id = "draw", options(future.globals.maxSize = 1000 * 1024^2), debug = TRUE)
head(prediction_out10)
# predictions_out10<- predictions_out10 %>%
#   dplyr::rename("StatusQuo" = Value)
# write.csv(predictions_out10, file = here::here("data-raw/StatusQuo/baseline_NJ.csv"))

# predic<- read.csv(here::here("data-raw/StatusQuo/baseline_NJ.csv")) %>% 
#   dplyr::mutate(run_number = as.character(run_number))

StatusQuo <- read.csv(here::here("data-raw/StatusQuo/baseline_NJ.csv"), na.strings = "") 

predictions <- predictions_out10 %>% #predictions_out10 %>% 
  dplyr::mutate(draw = as.numeric(draw)) %>% 
  dplyr::left_join(StatusQuo, by = c("Category","mode", "keep_release","param" ,"number_weight","state", "draw")) %>% 
  dplyr::mutate(StatusQuo = as.numeric(StatusQuo), 
                Value = as.numeric(Value), 
                perc_change = round(((Value/StatusQuo) - 1) * 100, digits = 0)) %>% 
  dplyr::group_by(Category,mode,keep_release,param,number_weight,state  ) %>% 
  dplyr::summarise(Value = mean(Value), 
                   StatusQuo = mean(StatusQuo), 
                   MeetsChange = sum(perc_change > 10),
                   perc_change = mean(perc_change)) %>% 
  dplyr::ungroup()

# test push