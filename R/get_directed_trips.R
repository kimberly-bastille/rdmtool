# get directed trips
directed_trips<- data.frame(readr::read_csv(file.path(here::here("data-raw/directed trips and regulations 2022_100 draws.csv"))))

# Connecticut
for(j in 1:100){
directed_trips_CT<-directed_trips %>% 
  dplyr::filter(state == "CT", 
                draw == j) %>% 
  data.table::data.table()
saveRDS(directed_trips_CT, file = paste0("data-raw/directed_trips/directed_trips_CT_",j,".rds"))
}

# Deleware
for(j in 1:100){
  directed_trips_DE<-directed_trips %>% 
    dplyr::filter(state == "DE", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_DE, file = paste0("data-raw/directed_trips/directed_trips_DE_",j,".rds"))
}

# Massachusetts
for(j in 1:100){
  directed_trips_MA<-directed_trips %>% 
    dplyr::filter(state == "MA", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_MA, file = paste0("data-raw/directed_trips/directed_trips_MA_",j,".rds"))
}

# Maryland
for(j in 1:100){
  directed_trips_MD<-directed_trips %>% 
    dplyr::filter(state == "MD", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_MD, file = paste0("data-raw/directed_trips/directed_trips_MD_",j,".rds"))
}

# New Jersey
for(j in 1:100){
  directed_trips_NJ<-directed_trips %>% 
    dplyr::filter(state == "NJ", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_NJ, file = paste0("data-raw/directed_trips/directed_trips_NJ_",j,".rds"))
}

# New York
for(j in 1:100){
  directed_trips_NY<-directed_trips %>% 
    dplyr::filter(state == "NY", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_NY, file = paste0("data-raw/directed_trips/directed_trips_NY_",j,".rds"))
}

# Rhode Island
for(j in 1:100){
  directed_trips_RI<-directed_trips %>% 
    dplyr::filter(state == "RI", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_RI, file = paste0("data-raw/directed_trips/directed_trips_RI_",j,".rds"))
}

# Virginia
for(j in 1:100){
  directed_trips_VA<-directed_trips %>% 
    dplyr::filter(state == "VA", 
                  draw == j) %>% 
    data.table::data.table()
  saveRDS(directed_trips_VA, file = paste0("data-raw/directed_trips/directed_trips_VA_",j,".rds"))
}