# get directed trips
directed_trips<- data.frame(read.csv(file.path(here::here("data-raw-calib/directed_trips/directed trips and regulations 2024_100 draws.csv"))))

#############################################################
directed_trips_CT<-directed_trips %>% 
  dplyr::filter(state == "CT") %>% 
  data.table::data.table()
#saveRDS(directed_trips_CT, file = paste0("data-raw/directed_trips/directed_trips_CT.rds"))

directed_trips_DE<-directed_trips %>% 
  dplyr::filter(state == "DE") %>% 
  data.table::data.table()
#saveRDS(directed_trips_DE, file = paste0("data-raw/directed_trips/directed_trips_DE.rds"))

directed_trips_MA<-directed_trips %>% 
  dplyr::filter(state == "MA") %>% 
  data.table::data.table()
#saveRDS(directed_trips_MA, file = paste0("data-raw/directed_trips/directed_trips_MA.rds"))

directed_trips_MD<-directed_trips %>% 
  dplyr::filter(state == "MD") %>% 
  data.table::data.table()
#saveRDS(directed_trips_MD, file = paste0("data-raw/directed_trips/directed_trips_MD.rds"))

directed_trips_NJ<-directed_trips %>% 
  dplyr::filter(state == "NJ") %>% 
  data.table::data.table()
#saveRDS(directed_trips_NJ, file = paste0("data-raw/directed_trips/directed_trips_NJ.rds"))

directed_trips_NY<-directed_trips %>% 
  dplyr::filter(state == "NY") %>% 
  data.table::data.table()
#saveRDS(directed_trips_NY, file = paste0("data-raw/directed_trips/directed_trips_NY.rds"))

directed_trips_RI<-directed_trips %>% 
  dplyr::filter(state == "RI") %>% 
  data.table::data.table()
#saveRDS(directed_trips_RI, file = paste0("data-raw/directed_trips/directed_trips_RI.rds"))

directed_trips_VA<-directed_trips %>% 
  dplyr::filter(state == "VA") %>% 
  data.table::data.table()
#saveRDS(directed_trips_VA, file = paste0("data-raw/directed_trips/directed_trips_VA.rds"))

directed_trips_NC<-directed_trips %>% 
  dplyr::filter(state == "NC") %>% 
  data.table::data.table()
#saveRDS(directed_trips_NC, file = paste0("data-raw/directed_trips/directed_trips_NC.rds"))


directed_trips_st <- list(directed_trips_MA, directed_trips_RI, directed_trips_CT, 
                          directed_trips_NY, directed_trips_NJ, directed_trips_DE, 
                          directed_trips_MD, directed_trips_VA, directed_trips_NC)

saveRDS(directed_trips_st, file = paste0("data-raw-calib/directed_trips/directed_trips_ST.rds"))