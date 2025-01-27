




#=====================================#
#Length data. Combine length data from trips with and without catch (need to retain the zeroes). 
#Pipe around this code if there is no cod catch, no haddock catch, or no catch of both species.

#If there is catch of both species:
if(cod_catch_check !=0 & had_catch_check!=0){
  
  length_data <- keep_release_cod %>%
    dplyr::full_join(keep_release_hadd, by = c("period2","tripid", "catch_draw"))
  
  length_data[is.na(length_data)] <- 0
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw)
  
  zero_catch_check <- cod_zero_catch %>%  dplyr::left_join(had_zero_catch,by = c("period2","tripid", "catch_draw")) %>%
    dplyr::filter(tot_keep_cod_new==0 & tot_rel_cod_new==0 &
                    tot_keep_hadd_new==0 & tot_rel_hadd_new==0) %>%
    dplyr::select("period2","tripid", "catch_draw")
  
  length_data<- plyr::rbind.fill(length_data, zero_catch_check)
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw)
  
  length_data[is.na(length_data)] <- 0
  
  
  ##Now need to merge these length data to the h_star data, and reallocate keeps as releases or vice versa
  ##code for reallocating cod release to keep  
  if (cod_release_2_keep==1){
    
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_cod_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(release_to_keep==1) %>% 
      dplyr::relocate(release_to_keep)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("release_to_keep", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter( disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "release_to_keep", "period2", "catch_draw", "tripid", "length")) %>% 
      dplyr::arrange(period2, tripid, species, catch_draw,length ) #%>% 
    
    check_long<-check_long %>% 
      dplyr::mutate(release = ifelse(species=="cod" & length>=floor_subl_cod_harv, 0, count_rel), 
                    keep = ifelse(species=="cod" & length>=floor_subl_cod_harv, count_keep+count_rel, count_keep)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("release_to_keep", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(release_to_keep==0) 
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-release_to_keep)
    
    
  }
  
  ##code for reallocating cod keep to release  
  if (cod_keep_2_release==1){
    
    if (all_cod_keep_2_release==1){
    
      length_data <- length_data %>% 
        rename_with(~paste0("relnew_cod", sub("keep_cod_*", "_", .)), 
                         starts_with('keep_cod_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("keepnew_cod", sub("release_cod_*", "_", .)), 
                    starts_with('release_cod_'))
      
      
      length_data <- length_data %>% 
        rename_with(~paste0("keep_cod", sub("keepnew_cod_*", "_", .)), 
                    starts_with('keepnew_cod_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("release_cod", sub("relnew_cod_*", "_", .)), 
                    starts_with('relnew_cod_'))
      
    }
    
    if (all_cod_keep_2_release==0){
      
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_cod_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(keep_to_release==1) %>% 
      dplyr::relocate(keep_to_release)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("keep_to_release", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "keep_to_release", "period2", "catch_draw", "tripid", "length"))
    
    check_long<-check_long %>% 
      dplyr::mutate(keep = ifelse(species=="cod", 0, count_keep), 
                    release = ifelse(species=="cod", count_keep+count_rel, count_rel)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("keep_to_release", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(keep_to_release==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-keep_to_release)
    }
    
  }
  
  ##code for reallocating haddock release to keep 
  if (hadd_release_2_keep==1){
    
    
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_hadd_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(release_to_keep==1) %>% 
      dplyr::relocate(release_to_keep)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("release_to_keep", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter( disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "release_to_keep", "period2", "catch_draw", "tripid", "length")) 
    
    check_long<-check_long %>% 
      dplyr::mutate(release = ifelse(species=="had" & length>=floor_subl_hadd_harv, 0, count_rel), 
                    keep = ifelse(species=="had" & length>=floor_subl_hadd_harv, count_keep+count_rel, count_keep)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("release_to_keep", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(release_to_keep==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-release_to_keep)
    
  }
  
  
  ##code for reallocating haddock keep as release   
  if (hadd_keep_2_release==1){
    
    
    if (all_hadd_keep_2_release==1){
      
      length_data <- length_data %>% 
        rename_with(~paste0("relnew_had", sub("keep_had_*", "_", .)), 
                    starts_with('keep_had_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("keepnew_had", sub("release_had_*", "_", .)), 
                    starts_with('release_had_'))
      
      
      length_data <- length_data %>% 
        rename_with(~paste0("keep_had", sub("keepnew_had_*", "_", .)), 
                    starts_with('keepnew_had_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("release_had", sub("relnew_had_*", "_", .)), 
                    starts_with('relnew_had_'))
      
    }
    
    if (all_hadd_keep_2_release==0){
      
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_hadd_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(keep_to_release==1) %>% 
      dplyr::relocate(keep_to_release)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("keep_to_release", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "keep_to_release", "period2", "catch_draw", "tripid", "length"))
    
    check_long<-check_long %>% 
      dplyr::mutate(keep = ifelse(species=="had", 0, count_keep), 
                    release = ifelse(species=="had", count_keep+count_rel, count_rel)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("keep_to_release", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(keep_to_release==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-keep_to_release)
    
    }
  }
  
}


#If there is no catch of either species 
if(cod_catch_check ==0 & had_catch_check==0){
  length_data <- cod_zero_catch %>%  dplyr::left_join(had_zero_catch, by = c("period2","tripid", "catch_draw")) %>%
    dplyr::select("period2","tripid", "catch_draw") %>% 
    dplyr::mutate(keep_cod_1=0, release_cod_1=0, keep_had_1=0, release_had_1=0)
  
}


#If there is catch of only cod 
if(cod_catch_check !=0 & had_catch_check==0){
  keep_release_hadd<-had_zero_catch %>% 
    dplyr::select("period2","tripid", "catch_draw") %>% 
    dplyr::mutate(keep_had_1=0, release_had_1=0)
  
  length_data <- keep_release_cod %>%
    dplyr::full_join(keep_release_hadd, by = c("period2","tripid", "catch_draw"))
  
  length_data[is.na(length_data)] <- 0
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw) %>% 
    plyr::rbind.fill(cod_zero_catch) %>% 
    dplyr::select(-tot_keep_cod_new, -tot_rel_cod_new) %>% 
    dplyr::relocate(period2, tripid, catch_draw)
  length_data[is.na(length_data)] <- 0
  
  
  ##Now need to merge these length data to the h_star data, and reallocate keeps as releases or vice versa
  ##code for reallocating cod release as keep 
  if (cod_release_2_keep==1){
    
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_cod_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(release_to_keep==1) %>% 
      dplyr::relocate(release_to_keep)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("release_to_keep", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter( disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "release_to_keep", "period2", "catch_draw", "tripid", "length"))
    
    check_long<-check_long %>% 
      dplyr::mutate(release = ifelse(species=="cod" & length>=floor_subl_cod_harv, 0, count_rel), 
                    keep = ifelse(species=="cod" & length>=floor_subl_cod_harv, count_keep+count_rel, count_keep)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("release_to_keep", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(release_to_keep==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-release_to_keep)
    
    
  }
  
  if (cod_keep_2_release==1){
    
    if (all_cod_keep_2_release==1){
      
      length_data <- length_data %>% 
        rename_with(~paste0("relnew_cod", sub("keep_cod_*", "_", .)), 
                    starts_with('keep_cod_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("keepnew_cod", sub("release_cod_*", "_", .)), 
                    starts_with('release_cod_'))
      
      
      length_data <- length_data %>% 
        rename_with(~paste0("keep_cod", sub("keepnew_cod_*", "_", .)), 
                    starts_with('keepnew_cod_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("release_cod", sub("relnew_cod_*", "_", .)), 
                    starts_with('relnew_cod_'))
      
    }
    
    if (all_cod_keep_2_release==0){
      
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_cod_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(keep_to_release==1) %>% 
      dplyr::relocate(keep_to_release)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("keep_to_release", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "keep_to_release", "period2", "catch_draw", "tripid", "length"))
    
    check_long<-check_long %>% 
      dplyr::mutate(keep = ifelse(species=="cod", 0, count_keep), 
                    release = ifelse(species=="cod", count_keep+count_rel, count_rel)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("keep_to_release", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(keep_to_release==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-keep_to_release)
    }
    
  }
  
  
  
}


#If there is catch of only haddock 
if(cod_catch_check ==0 & had_catch_check==1){
  keep_release_cod<-cod_zero_catch %>% 
    dplyr::select("period2","tripid", "catch_draw") %>% 
    dplyr::mutate(keep_cod_1=0, release_cod_1=0)
  
  length_data <- keep_release_hadd %>%
    dplyr::full_join(keep_release_cod, by = c("period2","tripid", "catch_draw"))
  
  length_data[is.na(length_data)] <- 0
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw) %>% 
    plyr::rbind.fill(had_zero_catch) %>% 
    dplyr::select(-tot_keep_hadd_new, -tot_rel_hadd_new) %>% 
    dplyr::relocate(period2, tripid, catch_draw)
  length_data[is.na(length_data)] <- 0
  
  
  
  ##Now need to merge these length data to the h_star data, and reallocate keeps as releases or vice versa
  ##code for reallocating haddock release as keep 
  if (hadd_release_2_keep==1){
    
    
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_hadd_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(release_to_keep==1) %>% 
      dplyr::relocate(release_to_keep)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("release_to_keep", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter( disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "release_to_keep", "period2", "catch_draw", "tripid", "length"))
    
    
    check_long<-check_long %>% 
      dplyr::mutate(release = ifelse(species=="had" & length>=floor_subl_hadd_harv, 0, count_rel), 
                    keep = ifelse(species=="had" & length>=floor_subl_hadd_harv, count_keep+count_rel, count_keep)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("release_to_keep", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(release_to_keep==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-release_to_keep)
    
  }
  
  ##code for reallocating haddock keep as release 
  if (hadd_keep_2_release==1){
    
    
    if (all_hadd_keep_2_release==1){
      
      length_data <- length_data %>% 
        rename_with(~paste0("relnew_had", sub("keep_had_*", "_", .)), 
                    starts_with('keep_had_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("keepnew_had", sub("release_had_*", "_", .)), 
                    starts_with('release_had_'))
      
      
      length_data <- length_data %>% 
        rename_with(~paste0("keep_had", sub("keepnew_had_*", "_", .)), 
                    starts_with('keepnew_had_'))
      
      length_data <- length_data %>% 
        rename_with(~paste0("release_had", sub("relnew_had_*", "_", .)), 
                    starts_with('relnew_had_'))
      
    }
    
    if (all_hadd_keep_2_release==0){
      
    length_data<- length_data %>% 
      dplyr::left_join(trip_data_hadd_hstar, by=c("period2","tripid")) 
    length_data[is.na(length_data)] <- 0
    
    check<-length_data %>% 
      dplyr::filter(keep_to_release==1) %>% 
      dplyr::relocate(keep_to_release)
    
    vars<-c()
    vars <- names(check)[!names(check) %in% c("keep_to_release", "catch_draw", "period2","tripid")]
    
    check_long_keep <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="keep") %>% 
      dplyr::rename(count_keep=count) %>% 
      dplyr::select(-disp)
    
    check_long_rel <- check %>%
      tidyr::pivot_longer(cols = c(vars), 
                          names_to = c("disp", "species", "length"), 
                          names_pattern = "(.*)_(.*)_(.*)", 
                          values_to="count") %>% 
      dplyr::filter(disp=="release") %>% 
      dplyr::rename(count_rel=count) %>% 
      dplyr::select(-disp)
    
    check_long<- check_long_keep %>% 
      dplyr::left_join(check_long_rel, by=c("species", "keep_to_release", "period2", "catch_draw", "tripid", "length"))
    
    check_long<-check_long %>% 
      dplyr::mutate(keep = ifelse(species=="had", 0, count_keep), 
                    release = ifelse(species=="had", count_keep+count_rel, count_rel)) %>% 
      dplyr::select(-count_keep, -count_rel)
    
    check_long[is.na(check_long)] <- 0
    
    check_wide_keep <- check_long %>% 
      dplyr::select(-release) %>% 
      tidyr::pivot_wider(names_from = c(species, length), 
                         names_glue = "keep_{species}_{length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    check_wide_rel <- check_long %>%
      dplyr::select(-keep,) %>% 
      tidyr::pivot_wider(names_from = c(species, length),
                         names_glue = "release_{species}_{length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    check_wide<-check_wide_keep %>% 
      dplyr::left_join(check_wide_rel, by=c("keep_to_release", "period2", "catch_draw", "tripid"))
    
    length_data<-length_data %>% 
      dplyr::filter(keep_to_release==0) 
    
    
    length_data2 <- length_data %>% 
      plyr::rbind.fill(check_wide)
    
    length_data<-length_data2 %>% 
      dplyr::select(-keep_to_release)
    }
    
  } 
}




#===============================#
length_data2<- mean_trip_data_prob_catch_draw %>%
  dplyr::left_join(length_data, by = c("period2", "tripid", "catch_draw")) 

all_vars<-c()
all_vars <- names(length_data2)[!names(length_data2) %in% c("period2","tripid", "probA", "catch_draw")]
all_vars


length_data3 <- length_data2 %>% 
  data.table::as.data.table()  %>%
  .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = all_vars] %>%
  .[]



all_vars<-c()
all_vars <- names(length_data3)[!names(length_data3) %in% c("period2","tripid", "catch_draw")]
all_vars

length_data3<- length_data3 %>%
  data.table::data.table() %>%
  .[,lapply(.SD, base::mean), by = c("period2","tripid"), .SDcols = all_vars]




#===============================#


### Keep all sp_length_mode columns and multiple by expand outside function -
##### Should be same number of rows - merge on (period2, tripid)
mean_trip_data01<-dplyr::distinct(mean_trip_data0, .keep_all = FALSE)

length_data3<- length_data3 %>%
  dplyr::left_join(mean_trip_data01, by=c("period2")) %>% 
  dplyr::select(-probA)


list_names <- names(length_data3)[!names(length_data3) %in% c("period2", "tripid", "expand")]
list_names

length_data3 <- length_data3 %>%
  data.table::as.data.table() %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * expand), .SDcols = list_names] %>%
  .[]

length_expanded <- length_data3 %>%
  tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>%
  dplyr::mutate(day = as.numeric(day),
                month = as.numeric(month),
                period2 = paste0(month, "_", day, "_", mode))


all_vars<-c()
all_vars <- names(length_expanded)[!names(length_expanded) %in% c("period2", "mode", "tripid", "expand", "month", "day")]
all_vars


length_expanded <- length_expanded %>%
  data.table::as.data.table() %>%
  .[,lapply(.SD, base::sum), by = c("mode", "month"), .SDcols = all_vars]


#This code translates numbers to weights using the l-w equation. The number_weight var is set to "Weight"
#Later on we drop the keep and release numbers computed here.



length_weight<- length_expanded %>%
  tidyr::pivot_longer(cols = !month & !mode,  names_to = "Var", values_to = "Number_at_Length") %>%
  tidyr::separate(Var, into = c("keep_release", "Species", "length"), sep = "_") %>%
  dplyr::rename(Month=month, Mode=mode) %>%
  dplyr::mutate(length_in = as.numeric(length),
                length_cm = length_in)  %>%  #Convert to cm 
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
  dplyr::ungroup()

length_weight<-length_weight %>% 
  dplyr::mutate(season = select_season, run = k, mrip_index=i)


season1<-unique(length_weight$season)
mode1<-unique(length_weight$mode1)
draw1<-unique(length_weight$run)

saveRDS(length_weight, file = paste0(iterative_input_data_cd, "calibrate_catch_wts_", mode1,"_", season1, "_", draw1, ".rds"))

rm(length_data, length_data2, length_data3, length_expanded, length_weight)
