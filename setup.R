# Script 1 - Setup

library(magrittr)
## This script creates objects used in multiple following locations
options(future.globals.maxSize= 10000000000)

n_drawz<-1000
n_catch_draws<-30


# Run the calibration files

# Below is a code that finds p* values which lead to projected harvest approximating actual harvest
# Don't need to run this every time

# source("find_pstar_values.R")

#once the p_star values are found, run the calibrations and save the output files
p_star_sf_MA_variable<- 0.85
p_star_sf_RI_variable<- 0.875
p_star_sf_CT_variable<- 0.9
p_star_sf_NY_variable<- 0.91
p_star_sf_NJ_variable<- 0.89
p_star_sf_DE_variable<- 0.725
p_star_sf_MD_variable<- 0.91
p_star_sf_VA_variable<- 0.9
#p_star_sf_NC_variable<- 0

p_star_bsb_MA_variable<- 0.82
p_star_bsb_RI_variable<- 0.81
p_star_bsb_CT_variable<- 0.82
p_star_bsb_NY_variable<- 0.86
p_star_bsb_NJ_variable<- 0.885
p_star_bsb_DE_variable<- 0.885
p_star_bsb_MD_variable<- 0.94
p_star_bsb_VA_variable<- 0.87
#p_star_bsb_NC_variable<- 0.975

p_star_scup_MA_variable<- 0.67
p_star_scup_RI_variable<- 0.595
p_star_scup_CT_variable<- 0.56
p_star_scup_NY_variable<- 0.535
p_star_scup_NJ_variable<- 0.045
#No scup p-stars for DE MD VA NC
p_star_scup_DE_variable<- 0.65
p_star_scup_MD_variable<- 0.65
p_star_scup_VA_variable<- 0.65
#p_star_scup_NC_variable<- 0.65




directed_trips_table<-data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2020.csv")))) %>% 
  dplyr::mutate(fluke_bag1= dplyr::case_when(state1 == "NJ" ~ c(NJ_SFsm_baglimit)))#%>% 
#   dplyr::mutate(#fluke_bag1 = dplyr::case_when(#state1 == "CT" ~ c(CT_SFsm_baglimit),
#     # state1 == "DE" ~ c(DE_SFsm_baglimit),
#     # state1 == "MA" ~ c(MA_SFsm_baglimit),
#     # state1 == "MD" ~ c(MD_SFsm_baglimit),
#     #state == "NJ" ~ c(test)),#,
#     # state1 == "NY" ~ c(NY_SFsm_baglimit),
#     # state1 == "RI" ~ c(RI_SFsm_baglimit), 
#     # state1 == "VA" ~ c(VA_SFlg_baglimit)), 
#     fluke_bag2 = dplyr::case_when(#state1 == "CT" ~ c(CT_SFlg_baglimit),
#       #state1 == "DE" ~ c(DE_SFlg_baglimit),
#       #state1 == "MA" ~ c(MA_SFlg_baglimit),
#       #state1 == "MD" ~ c(MD_SFlg_baglimit),
#       state == "NJ" ~ c(NJ_SFlg_baglimit)),
#     #state1 == "NY" ~ c(NY_SFlg_baglimit),
#     #state1 == "RI" ~ c(RI_SFlg_baglimit),
#     #state1 == "VA" ~ c(VA_SFlg_baglimit)),
#     bsb_bag = dplyr::case_when(  #state1 == "CT" ~ c(CT_BSB_baglimit),
#       #state1 == "DE" ~ c(DE_BSB_baglimit),
#       #state1 == "MA" ~ c(MA_BSB_baglimit),
#       #state1 == "MD" ~ c(MD_BSB_baglimit),
#       state == "NJ" ~ c(NJ_BSB_baglimit)),
#     #state1 == "NY" ~ c(NY_BSB_baglimit),
#     #state1 == "RI" ~ c(RI_BSB_baglimit),
#     #state1 == "VA" ~ c(VA_BSB_baglimit)),
#     scup_bag = dplyr::case_when( #state1 == "CT" ~ c(CT_SCUP_baglimit),
#       #state1 == "DE" ~ c(DE_SCUP_baglimit),
#       #state1 == "MA" ~ c(MA_SCUP_baglimit),
#       #state1 == "MD" ~ c(MD_SCUP_baglimit),
#       state == "NJ" ~ c(NJ_SCUP_baglimit))) #,
# #state1 == "NY" ~ c(NY_SCUP_baglimit),
# #state1 == "RI" ~ c(RI_SCUP_baglimit),
# #state1 == "VA" ~ c(VA_SCUP_baglimit))))
directed_trips_table<-subset(directed_trips_table, state!="NC")
directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)


# For the baseline (no correlation) scenario, use catch-per-trip distributions from the baseline year (observed_catch_2020_SS_MM_md.csv)
# For all scenarios, use the adjusted catch-at-length distribution and no stock adjustments


sf_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/sf_length_distn_2020.csv")),  show_col_types = FALSE),
                       state!="NC",select=c(state, fitted_length, prob_star))
sf_size_data_read_base <- split(sf_size_data, sf_size_data$state)


bsb_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/bsb_length_distn_2020.csv")),  show_col_types = FALSE),
                        state!="NC", select=c(state, fitted_length, prob_star))
bsb_size_data_read_base <- split(bsb_size_data, bsb_size_data$state)


scup_size_data <- subset(readr::read_csv(file.path(here::here("data-raw/scup_length_distn_2020.csv")),  show_col_types = FALSE),
                         state!="NC",select=c(state, fitted_length, prob_star))
scup_size_data_read_base <- split(scup_size_data, scup_size_data$state)

rm(sf_size_data, bsb_size_data, scup_size_data, directed_trips_table)