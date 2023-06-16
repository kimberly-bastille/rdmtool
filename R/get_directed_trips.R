# get directed trips
directed_trips<- data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2022_100 draws.csv"))))

#############################################################
directed_trips_NJ<-directed_trips %>% 
  dplyr::filter(state == "NJ") %>% 
  data.table::data.table()
saveRDS(directed_trips_NJ, file = paste0("data-raw/directed_trips/directed_trips_NJ.rds"))