#### Run all of this before going into the predict_rec_catch function

library(magrittr)

state1 = "NJ"

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

directed_trips<-readRDS(file.path(here::here(paste0("data-raw/directed_trips/directed_trips_NJ.rds"))))

x = 1

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
                period2 = paste0(month24, "-", day, "-", mode))

sf_size_data <- sf_size_dat %>% 
  dplyr::filter(draw == 0) #Change to X for model for sf and scup

bsb_size_data <- bsb_size_dat %>% 
  dplyr::filter(draw == 0)

scup_size_data <- scup_size_dat %>% 
  dplyr::filter(draw == 0)

print("made it through data read in ")

s_star_data <- s_star_dat %>% 
  dplyr::filter(draw == x)

state1 = c("NJ")
calibration_data_table = calibration_output_by_period
directed_trips_table = directed_trips2
sf_size_data_read = sf_size_data
bsb_size_data_read = bsb_size_data
scup_size_data_read = scup_size_data
costs_new_all = costs_new_all
l_w = l_w_conversion
s_star = s_star_data
sf_catch_data_all = c(list(catch_files_NJ))
n_drawz = 50
n_catch_draws = 30
eff_seed=190