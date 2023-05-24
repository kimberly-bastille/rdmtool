# Get catch files

#catch
# Here input catch-per-trip calculations that are based on 2020

## Connecticut
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_9_*")
sf_ind_catch_CT <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="CT", 
                catch = "ind")

catch_ind_files_CT <- data.table::data.table(sf_ind_catch_CT)
saveRDS(catch_ind_files_CT,file  = "data-raw/catch/catch_ind_files_CT.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_9_*")
sf_corr_catch_CT <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="CT", 
                catch = "corr")

catch_corr_files_CT <- data.table::data.table(sf_corr_catch_CT)
saveRDS(catch_corr_files_CT,file  = "data-raw/catch/catch_corr_files_CT.rds")

## Deleware
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_10_*")
sf_ind_catch_DE <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="DE", 
                catch = "ind")

catch_ind_files_DE <- data.table::data.table(sf_ind_catch_DE)
saveRDS(catch_ind_files_DE,file  = "data-raw/catch/catch_ind_files_DE.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_10_*")
sf_corr_catch_DE <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="DE", 
                catch = "corr")

catch_corr_files_DE <- data.table::data.table(sf_corr_catch_DE)
saveRDS(catch_corr_files_DE,file  = "data-raw/catch/catch_corr_files_DE.rds")

## Massachusetts
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_25_*")
sf_ind_catch_MA <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MA", 
                catch = "ind")

catch_ind_files_MA <- data.table::data.table(sf_ind_catch_MA)
saveRDS(catch_ind_files_MA,file  = "data-raw/catch/catch_ind_files_MA.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_25_*")
sf_corr_catch_MA <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MA", 
                catch = "corr")

catch_corr_files_MA <- data.table::data.table(sf_corr_catch_MA)
saveRDS(catch_corr_files_MA,file  = "data-raw/catch/catch_corr_files_MA.rds")

## Maryland
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_24_*")
sf_ind_catch_MD <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MD", 
                catch = "ind")

catch_ind_files_MD <- data.table::data.table(sf_ind_catch_MD)
saveRDS(catch_ind_files_MD,file  = "data-raw/catch/catch_ind_files_MD.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_24_*")
sf_corr_catch_MD <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="MD", 
                catch = "corr")

catch_corr_files_MD <- data.table::data.table(sf_corr_catch_MD)
saveRDS(catch_corr_files_MD,file  = "data-raw/catch/catch_corr_files_MD.rds")

## New Jersey
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_34_*")
sf_ind_catch_NJ <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NJ", 
                catch = "ind")

catch_ind_files_NJ <- data.table::data.table(sf_ind_catch_NJ)
saveRDS(catch_ind_files_NJ,file  = "data-raw/catch/catch_ind_files_NJ.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_34_*")
sf_corr_catch_NJ <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NJ", 
                catch = "corr")

catch_corr_files_NJ <- data.table::data.table(sf_corr_catch_NJ)
saveRDS(catch_corr_files_NJ,file  = "data-raw/catch/catch_corr_files_NJ.rds")

## New York
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_36_*")
sf_ind_catch_NY <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NY", 
                catch = "ind")

catch_ind_files_NY <- data.table::data.table(sf_ind_catch_NY)
saveRDS(catch_ind_files_NY,file  = "data-raw/catch/catch_ind_files_NY.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_36_*")
sf_corr_catch_NY <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="NY", 
                catch = "corr")

catch_corr_files_NY <- data.table::data.table(sf_corr_catch_NY)
saveRDS(catch_corr_files_NY,file  = "data-raw/catch/catch_corr_files_NY.rds")

## Rhode Island
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_44_*")
sf_ind_catch_RI <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="RI", 
                catch = "ind")

catch_ind_files_RI <- data.table::data.table(sf_ind_catch_RI)
saveRDS(catch_ind_files_RI,file  = "data-raw/catch/catch_ind_files_RI.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_44_*")
sf_corr_catch_RI <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="RI", 
                catch = "corr")

catch_corr_files_RI <- data.table::data.table(sf_corr_catch_RI)
saveRDS(catch_corr_files_RI,file  = "data-raw/catch/catch_corr_files_RI.rds")

## Virginia
ind_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_ind_catch1_51_*")
sf_ind_catch_VA <- readr::read_csv(file.path(here::here("data-raw/simulated", ind_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="VA", 
                catch = "ind")

catch_ind_files_VA <- data.table::data.table(sf_ind_catch_VA)
saveRDS(catch_ind_files_VA,file  = "data-raw/catch/catch_ind_files_VA.rds")

corr_catch_files <- dir(file.path(here::here("data-raw/simulated")), pattern = "simulated_corr_catch1_51_*")
sf_corr_catch_VA <- readr::read_csv(file.path(here::here("data-raw/simulated", corr_catch_files))) %>%
  dplyr::select(c(-state, -decade)) %>%
  dplyr::mutate(state="VA", 
                catch = "corr")

catch_corr_files_VA <- data.table::data.table(sf_corr_catch_VA)
saveRDS(catch_corr_files_VA,file  = "data-raw/catch/catch_corr_files_VA.rds")
