
#This file computes the weight of catch in the calibration year. 
###Length data
#If there is catch of both species:
if(cod_catch_check !=0 & had_catch_check!=0){
  
  
  ##
  # Convert to data.table
  setDT(keep_release_cod)
  setDT(keep_release_hadd)
  setDT(cod_zero_catch)
  setDT(had_zero_catch)
  
  # Full join equivalent in data.table
  length_data <- merge(keep_release_cod, keep_release_hadd, 
                       by = c("period2", "tripid", "catch_draw"), 
                       all = TRUE)
  
  
  
  # Left join and filtering
  zero_catch_check <- merge(cod_zero_catch, had_zero_catch, 
                            by = c("period2", "tripid", "catch_draw"), 
                            all.x = TRUE)[
                              tot_keep_cod_new == 0 & tot_rel_cod_new == 0 & 
                                tot_keep_hadd_new == 0 & tot_rel_hadd_new == 0, 
                              .(period2, tripid, catch_draw)
                            ]
  
  # Bind rows (rbindlist is faster and more memory-efficient)
  length_data <- rbindlist(list(length_data, zero_catch_check), fill = TRUE)
  
  # Replace NA values with 0 again (if necessary)
  length_data[is.na(length_data)] <- 0
  
  rm(cod_zero_catch,had_zero_catch, zero_catch_check )
}

#If there is no catch of either species 
if(cod_catch_check ==0 & had_catch_check==0){
  length_data <- trip_data %>%  
    dplyr::select("period2","tripid", "catch_draw") %>% 
    dplyr::mutate(keep_cod_1=0, release_cod_1=0, keep_had_1=0, release_had_1=0)
  
}

#If there is catch of only haddock 
if(cod_catch_check ==0 & had_catch_check!=0){
  
  keep_release_cod<-trip_data %>% 
    dplyr::select("period2","tripid", "catch_draw") %>% 
    dplyr::mutate(keep_cod_1=0, release_cod_1=0)
  
  length_data <- keep_release_hadd %>%
    dplyr::full_join(keep_release_cod, by = c("period2","tripid", "catch_draw"))
  
  length_data[is.na(length_data)] <- 0
  
  
}

#If there is catch of only cod 
if(cod_catch_check !=0 & had_catch_check==0){
  
  keep_release_hadd<-trip_data %>% 
    dplyr::select("period2","tripid", "catch_draw") %>% 
    dplyr::mutate(keep_had_1=0, release_had_1=0)
  
  length_data <- keep_release_cod %>%
    dplyr::full_join(keep_release_hadd, by = c("period2","tripid", "catch_draw"))
  
  length_data[is.na(length_data)] <- 0
  
}



length_data2<- mean_trip_data_prob_catch_draw %>%
  dplyr::left_join(length_data, by = c("period2", "tripid", "catch_draw")) 

all_vars <- setdiff(names(length_data2), c("period2", "tripid", "probA", "catch_draw"))

length_data3 <- length_data2 %>% 
  data.table::as.data.table()  %>%
  .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = all_vars] %>%
  .[]


all_vars <- setdiff(names(length_data3), c("period2", "tripid", "catch_draw"))

length_data3 <- data.table::as.data.table(length_data3)[
  , lapply(.SD, mean), by = .(period2, tripid), .SDcols = all_vars
]





length_expand <- mean_trip_data %>%
  dplyr::select(period2, tripid, expand) %>%
  dplyr::left_join(length_data3, by = c("period2", "tripid")) %>%
  dplyr::select(-probA)

all_vars<-c()
all_vars <- names(length_expand)[!names(length_expand) %in% c("period2", "tripid", "expand")]

# ## Move to outside function
length_expand <- length_expand %>%
  data.table::as.data.table() %>%
  .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(expand)), .SDcols = all_vars] %>%
  .[]

length_expanded <- length_expand %>%
  tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>%
  dplyr::mutate(day = as.numeric(day),
                month = as.numeric(month),
                period2 = paste0(month, "_", day, "_", mode))



all_vars<-c()
all_vars <- names(length_expanded)[!names(length_expanded) %in% c("period2", "mode", "tripid", "expand", "month", "day")]
#all_vars


length_expanded <- length_expanded %>%
  data.table::as.data.table() %>%
  .[,lapply(.SD, base::sum), by = c("mode", "month"), .SDcols = all_vars]



#This code translates numbers to weights using the l-w equation. The number_weight var is set to "Weight"
#Later on we drop the keep and release numbers computed here.
length_weight<- length_expanded %>%
  tidyr::pivot_longer(cols = !month & !mode,  names_to = "Var", values_to = "Number_at_Length") %>%
  tidyr::separate(Var, into = c("keep_release", "Species", "length"), sep = "_") %>%
  dplyr::rename(Month=month, Mode=mode) %>%
  dplyr::mutate(length_cm = as.numeric(length)) %>% 
  #length_cm = length_in) #*2.54)  %>%  #Convert to cm 
  dplyr::mutate(weight = dplyr::case_when(Species == "cod" ~ cod_lw_a*length_cm^cod_lw_b, TRUE~0),
                weight = dplyr::case_when(Species == "had" ~ had_lw_a*length_cm^had_lw_b, TRUE ~ weight),
                weight = weight*2.20462262185, #convert to lbs
                Total_weight = Number_at_Length * weight) %>%
  dplyr::mutate(spp2 = dplyr::case_when(Species == "had" & length_cm >  50 ~ "had_lg", TRUE~ Species),
                spp2 = dplyr::case_when(Species == "had" & length_cm <=  50 ~ "had_sm", TRUE~spp2)) %>%
  dplyr::left_join(Disc_mort, by = c("Month", "spp2")) %>%
  dplyr::mutate(Discmortality_Total_weight = ifelse(keep_release=="release", Discard_mortality * Total_weight,0),
                Discmortality_Total_Number = ifelse(keep_release=="release", Discard_mortality * Number_at_Length, 0)) %>%
  dplyr::group_by(Species, Mode, keep_release) %>%
  dplyr::summarise(Total_Number = sum(Number_at_Length),
                   Total_Weight = sum(Total_weight),
                   Discmortality_Total_Weight = sum(Discmortality_Total_weight),
                   Discmortality_Total_Number = sum(Discmortality_Total_Number),.groups = 'drop') %>%
  dplyr::rename(mode1 = Mode) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(season=season1, run=draw1)


saveRDS(length_weight, file = paste0(iterative_input_data_cd, "calibrate_catch_wts_", mode1,"_", season1, "_", draw1, ".rds"))

rm(length_data, length_data2, length_data3, length_expanded, length_expand, length_weight)
