# Script 1 - Setup

library(magrittr)
## This script creates objects used in multiple following locations
options(future.globals.maxSize= 1000000000)

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




directed_trips_table<-data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2020.csv"))))
directed_trips_table<-subset(directed_trips_table, state!="NC")
directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)

#catch
# Here input  catch-per-trip projections that are based on 2020
# catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_51_*")
# sf_catch_data_va <- readr::read_csv(file.path(here::here("data-raw/", catch_files)))


catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_25_*")
sf_catch_data_ma <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="MA")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_44_*")
sf_catch_data_ri <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="RI")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_9_*")
sf_catch_data_ct <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="CT")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_36_*")
sf_catch_data_ny <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="NY")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_34_*")
sf_catch_data_nj <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="NJ")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_10_*")
sf_catch_data_de <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="DE")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_24_*")
sf_catch_data_md <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="MD")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_ind_catch1_51_*")
sf_catch_data_va <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="VA")



catch_files_all_cal = as.data.frame(rbind(sf_catch_data_ma, sf_catch_data_ri, sf_catch_data_ct,
                                          sf_catch_data_ny, sf_catch_data_nj, sf_catch_data_de,
                                          sf_catch_data_md, sf_catch_data_va)) %>%
  dplyr::filter(decade == 1)

catch_files_all_cal_base <- split(catch_files_all_cal, catch_files_all_cal$state)
rm(catch_files_all_cal)
rm(sf_catch_data_ma, sf_catch_data_ri, sf_catch_data_ct, sf_catch_data_ny, sf_catch_data_nj, sf_catch_data_de, sf_catch_data_md, sf_catch_data_va)



# Here input  catch-per-trip projections that are based on 2020
catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_25_*")
sf_catch_data_ma <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="MA")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_44_*")
sf_catch_data_ri <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="RI")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_9_*")
sf_catch_data_ct <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="CT")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_36_*")
sf_catch_data_ny <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="NY")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_34_*")
sf_catch_data_nj <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="NJ")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_10_*")
sf_catch_data_de <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="DE")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_24_*")
sf_catch_data_md <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="MD")

catch_files <- dir(file.path(here::here("data-raw/")), pattern = "simulated_corr_catch1_51_*")
sf_catch_data_va <- readr::read_csv(file.path(here::here("data-raw/", catch_files))) %>%
  dplyr::select(-state) %>%
  dplyr::mutate(state="VA")



catch_files_all = as.data.frame(rbind(sf_catch_data_ma, sf_catch_data_ri, sf_catch_data_ct,
                                      sf_catch_data_ny, sf_catch_data_nj, sf_catch_data_de,
                                      sf_catch_data_md, sf_catch_data_va)) %>%
  dplyr::filter(decade == 1)

catch_files_all_base <- split(catch_files_all, catch_files_all$state)
rm(catch_files_all)
rm(sf_catch_data_ma, sf_catch_data_ri, sf_catch_data_ct, sf_catch_data_ny, sf_catch_data_nj, sf_catch_data_de, sf_catch_data_md, sf_catch_data_va)

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

