# Get catch files

#catch
# Here input catch-per-trip calculations that are based on 2020

## Connecticut
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_9_*")
sf_ind_catch_CT <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="CT", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_9_*")
sf_corr_catch_CT <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="CT", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_CT, sf_corr_catch_CT))
catch_files_CT <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_CT,file  = "data-raw/catch/catch_files_CT.rds")

## Deleware
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_10_*")
sf_ind_catch_DE <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="DE", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_10_*")
sf_corr_catch_DE <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="DE", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_DE, sf_corr_catch_DE))
catch_files_DE <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_DE,file  = "data-raw/catch/catch_files_DE.rds")

## Massachusetts
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_25_*")
sf_ind_catch_MA <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MA", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_25_*")
sf_corr_catch_MA <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MA", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_MA, sf_corr_catch_MA))
catch_files_MA <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_MA,file  = "data-raw/catch/catch_files_MA.rds")

## Maryland
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_24_*")
sf_ind_catch_MD <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MD", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_24_*")
sf_corr_catch_MD <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MD", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_MD, sf_corr_catch_MD))
catch_files_MD <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_MD,file  = "data-raw/catch/catch_files_MD.rds")

## New Jersey
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_34_*")
sf_ind_catch_NJ <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NJ", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_34_*")
sf_corr_catch_NJ <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NJ", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_NJ, sf_corr_catch_NJ))
catch_files_NJ <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_NJ,file  = "data-raw/catch/catch_files_NJ.rds")

## New York
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_36_*")
sf_ind_catch_NY <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NY", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_36_*")
sf_corr_catch_NY <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NY", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_NY, sf_corr_catch_NY))
catch_files_NY <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_NY,file  = "data-raw/catch/catch_files_NY.rds")

## Rhode Island
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_44_*")
sf_ind_catch_RI <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="RI", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_44_*")
sf_corr_catch_RI <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="RI", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_RI, sf_corr_catch_RI))
catch_files_RI <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_RI,file  = "data-raw/catch/catch_files_RI.rds")

## Virginia
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_51_*")
sf_ind_catch_VA <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="VA", 
                catch = "ind")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_51_*")
sf_corr_catch_VA <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="VA", 
                catch = "corr")

catch_files_all = as.data.frame(rbind(sf_ind_catch_VA, sf_corr_catch_VA))
catch_files_VA <- split(catch_files_all, catch_files_all$catch)

saveRDS(catch_files_VA,file  = "data-raw/catch/catch_files_VA.rds")

