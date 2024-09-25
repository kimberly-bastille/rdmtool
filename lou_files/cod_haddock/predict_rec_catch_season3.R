pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflicts_prefer(dplyr::mutate)
options(scipen = 100, digits = 3)


#Pull in data that is not draw-specific
catch_draws_file_path = "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws"
directed_trips_table =  read.csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/directed_trips_calib_150draws.csv")            
size_data_read = read.csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/agepro/projected_CaL_cod_hadd.csv")
Disc_mort<- readr::read_csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/Discard_Mortality.csv", show_col_types = FALSE)


output1<-data.frame() 
output2<-data.frame() ##This dataset will store all the results

baseline_comparison1<-readRDS("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/calibration_comparison.rds") %>% 
  dplyr::arrange(draw, mrip_index) %>% 
  dplyr::group_by(draw) %>% 
  dplyr::mutate(draw_id = cur_group_id()) %>% 
  dplyr::filter(draw_id<=100)
n_distinct(baseline_comparison1$draw)


#Save calibration stats
#write_xlsx(baseline_comparison1, "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/calibration_stats.xlsx")




for(i in unique(baseline_comparison1$mrip_index)){
baseline_comparison<-baseline_comparison1 %>% 
  dplyr::filter(mrip_index==i) %>% 
  dplyr::mutate(all_cod_keep_2_release=ifelse(tot_keep_cod_model>0 & tot_cod_keep_mrip==0, 1, 0),
                all_hadd_keep_2_release=ifelse(tot_keep_hadd_model>0 & tot_hadd_keep_mrip==0, 1, 0))

select_mode = unique(baseline_comparison$mode)
select_season = unique(baseline_comparison$open)

k<- baseline_comparison$draw

CaL_draw<- baseline_comparison$draw

#indicate whether we need to allocate keeps to releases, or releases to keeps, for both species 
cod_keep_2_release<-mean(baseline_comparison$cod_keep_2_release)
cod_release_2_keep<-mean(baseline_comparison$cod_release_2_keep)
hadd_keep_2_release<-mean(baseline_comparison$hadd_keep_2_release)
hadd_release_2_keep<-mean(baseline_comparison$hadd_release_2_keep)

#indicate whether we need to allocate ALL keep as release, for both species 
all_cod_keep_2_release<-mean(baseline_comparison$all_cod_keep_2_release)
all_hadd_keep_2_release<-mean(baseline_comparison$all_hadd_keep_2_release)

#Pull in the h_star_values computed from the calibration 
h_star_cod_release_to_keep_variable<-mean(baseline_comparison$h_star_cod_release_to_keep_variable)
h_star_hadd_release_to_keep_variable<-mean(baseline_comparison$h_star_hadd_release_to_keep_variable)
h_star_cod_keep_to_release_variable<-mean(baseline_comparison$h_star_cod_keep_to_release_variable)
h_star_hadd_keep_to_release_variable<-mean(baseline_comparison$h_star_hadd_keep_to_release_variable)

#Pull in data that is draw-specific
calendar_2024_adjust <- readr::read_csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/next year calendar adjustments.csv", show_col_types = FALSE) %>%
  dplyr::filter(draw == k)
calibration_data_table = feather::read_feather(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/pds_new_", select_mode,"_", select_season, "_", k,".feather"))
costs_new_all = feather::read_feather(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/costs_", select_mode,"_", select_season, "_", k,".feather"))

n_drawz = 50
n_catch_draws = 30
set.seed(k)

#l_w_conversion =
cod_lw_a = 0.000005132
cod_lw_b = 3.1625
had_lw_a = 0.000009298
had_lw_b = 3.0205


directed_trips<-directed_trips_table %>%
  tibble::tibble() %>%
  dplyr::filter(draw == k,
                mode == select_mode) %>%
  dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0))



#Create as an object the minimum size at which fish are illegally harvested.
# This object "floor_subl_harvest" will be 2 inches below the minimum size, by mode. 
  #1) If the minimum size changes across the season, floor_subl_harvest=min(min_size) - 2 inches.
  #2a) If the fishery is closed the entire season, floor_subl_harvest=min(min. size of the previous season) -2 inches.
  #2b) If the fishery is closed the entire current and previous season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 

# 1) and 2a) below:
# floor_subl_cod_harv<-min(directed_trips$cod_min_y2)-(2*2.54)
# floor_subl_hadd_harv<-min(directed_trips$hadd_min_y2)-(2*2.54)
# 
# if (floor_subl_cod_harv==248.92){
#   floor_subl_cod_harv<-min(directed_trips$cod_min)-(2*2.54)
# }
# 
# if (floor_subl_hadd_harv==248.92){
#   floor_subl_hadd_harv<-min(directed_trips$hadd_min)-(2*2.54)
# }


floor_subl_cod_harv<-min(directed_trips$cod_min_y2)-2
floor_subl_hadd_harv<-min(directed_trips$hadd_min_y2)-2

if (floor_subl_cod_harv==98){
  floor_subl_cod_harv<-min(directed_trips$cod_min)-2
}

if (floor_subl_hadd_harv==98){
  floor_subl_hadd_harv<-min(directed_trips$hadd_min)-2
}

open<- directed_trips %>%
  dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                period2 = paste0(month, "_", day, "_", mode)) %>%
  dplyr::select(period2, open) %>%
  dplyr::filter(open == select_season)

directed_trips<- directed_trips %>%
  dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                period2 = paste0(month, "_", day, "_", mode)) %>%
  dplyr::filter(open == select_season)


######################################
##   Begin simulating trip outcomes ##
######################################

# Set up an output file for the separately simulated within-season regulatory periods
directed_trips_p <- directed_trips %>%
  dplyr::mutate(month = as.numeric(month1)) %>%
  dplyr::mutate(n_draws = n_drawz)%>%
  dplyr::select(!c(month, mode))

regs <- directed_trips_p %>%
  dplyr::select(period2,
                cod_bag_y2,
                cod_min_y2,
                hadd_bag_y2,
                hadd_min_y2)

param_draws <- directed_trips_p %>%
  dplyr::select(period2, n_draws, open) %>%
  tidyr::uncount(n_draws) 

cod_catch_data <- feather::read_feather(paste0(catch_draws_file_path, k, "_full.feather")) %>% 
  dplyr::mutate(period2=paste0(month, "_", day1, "_", mode)) %>%  
  dplyr::left_join(open, by = "period2") %>%
  dplyr::filter(open == select_season) %>%
  dplyr::select(!open, !day) %>%
  dplyr::rename(tot_cod_catch = cod_catch,
                tot_had_catch = hadd_catch)  %>%
  dplyr::select(mode,month,tot_cod_catch,tot_had_catch,
                tripid,catch_draw,day, draw, period2)


cod_catch_data <- cod_catch_data %>%
  dplyr::mutate(day = as.numeric(stringr::str_extract(day, "\\d+"))) %>%
  dplyr::group_by(period2) %>%
  dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
  dplyr::mutate(catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
                tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
  dplyr::ungroup()%>%
  dplyr::select(!c(month))


if(select_season == 1){
  seas = "open"
}
if(select_season == 0){
  seas = "closed"
}


cod_size_data <- size_data_read %>% 
  dplyr::filter(species == "cod", season == seas,  draw==CaL_draw) %>% 
  dplyr::filter(!is.na(proj_CaL_prob_smooth)) %>% 
  dplyr::select(-proj_CaL_prob_raw)

had_size_data <- size_data_read %>% 
  dplyr::filter(species == "hadd", season == seas,  draw==CaL_draw) %>% 
  dplyr::filter(!is.na(proj_CaL_prob_smooth)) %>% 
  dplyr::select(-proj_CaL_prob_raw)

cod_had_catch_data <- cod_catch_data


# subset trips with zero catch, as no size draws are required
cod_zero_catch <- dplyr::filter(cod_catch_data, tot_cod_catch == 0)


#Check to see if there is no catch for either species and if so, pipe code around keep/release determination
cod_catch_check<-base::sum(cod_catch_data$tot_cod_catch)
had_catch_check<-base::sum(cod_catch_data$tot_had_catch)


if(cod_catch_check ==0 & had_catch_check==0){
  trip_data<-cod_catch_data
  trip_data<- trip_data %>% 
    dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
    dplyr::select(-mode)
  
  trip_data$tot_keep_cod_new<-0
  trip_data$tot_rel_cod_new<-0
  
  
  trip_data$tot_keep_hadd_new<-0
  trip_data$tot_rel_hadd_new<-0
}


if(cod_catch_check !=0){
  
  #keep trips with positive cod catch
  cod_catch_data <- dplyr::filter(cod_catch_data, tot_cod_catch > 0)
  
  row_inds <- seq_len(nrow(cod_catch_data))
  
  cod_catch_data<-cod_catch_data %>%
    dplyr::slice(rep(row_inds,tot_cod_catch))   %>%
    dplyr::mutate(fishid=dplyr::row_number())
  
  # generate lengths for each fish
  catch_size_data <- cod_catch_data %>%
    dplyr::mutate(fitted_length = sample(cod_size_data$length,
                                         nrow(.),
                                         prob = cod_size_data$proj_CaL_prob_smooth,
                                         replace = TRUE)) 
  
  #Create as an object the minimum size at which fish are illegally harvested.
  # This object "floor_subl_harvest" will be 2 inches below the minimum size, by mode. 
  #1) If the minimum size changes across the season, floor_subl_harvest=min(min_size) - 2.
  #2a) If the fishery is closed the entire season, floor_subl_harvest=min(min. size of the previous season) -2 
  #2b) If the fishery is closed the entire current and previous season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
  
  # 2b) below:
  # if (floor_subl_cod_harv==248.92){
  #   floor_subl_cod_harv=mean(catch_size_data$fitted_length)-0.5*sd(catch_size_data$fitted_length)
  #   }
  
  if (floor_subl_cod_harv==98){
    floor_subl_cod_harv=mean(catch_size_data$fitted_length)-0.5*sd(catch_size_data$fitted_length)
  }
  
  
  # Impose regulations, calculate keep and release per trip
  ####### Start Here #################
  
  ############# Length #####################################
  catch_size_data <- catch_size_data %>%
    dplyr::left_join(regs, by = c("period2")) %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=cod_min_y2 ,1,0)) %>%
    dplyr::group_by(tripid, period2, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        cod_bag_y2 > 0 ~ ifelse(csum_keep<=cod_bag_y2 & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0))
  
  catch_size_data<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, period2, catch_draw, mode)  %>%
    dplyr::rename(mode1=mode) %>% 
    dplyr::mutate(floor_subl_cod_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_cod_harv~1,TRUE~0))
  
  new_size_data <- catch_size_data %>%
    dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
    dplyr::summarize(keep = sum(keep),
                     release = sum(release), .groups = "drop") %>%
    dplyr::ungroup()
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(names_from = fitted_length, #_length,
                       names_glue = "keep_cod_{fitted_length}",
                       names_sort = TRUE,
                       values_from = keep,
                       values_fill = 0)
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(names_from = fitted_length, #_length,
                       names_glue = "release_cod_{fitted_length}",
                       names_sort = TRUE,
                       values_from = release,
                       values_fill = 0)
  
  keep_release_cod <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
  
  trip_data <- catch_size_data %>%
    dplyr::group_by(period2, catch_draw, tripid) %>%
    dplyr::summarize(tot_keep_cod_new = sum(keep),
                     tot_rel_cod_new = sum(release),
                     floor_subl_cod_harv_indicator=sum(floor_subl_cod_harv_indicator),
                     .groups = "drop") %>%
    dplyr::ungroup()
  
  cod_zero_catch<-cod_zero_catch %>%
    dplyr::select(tripid, catch_draw, period2) %>%
    dplyr::mutate(tot_keep_cod_new=0,
                  tot_rel_cod_new=0)
  
  trip_data <- dplyr::bind_rows(trip_data, cod_zero_catch) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::select(c("period2", "catch_draw","tripid",
                    "tot_keep_cod_new","tot_rel_cod_new", "floor_subl_cod_harv_indicator"))
  
  
  trip_data<- trip_data %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid))
  trip_data<-data.table::as.data.table(trip_data)
  data.table::setkey(trip_data, "domain2")
}


if (cod_catch_check==0 & had_catch_check!=0){
  trip_data<-cod_catch_data
  trip_data<- trip_data %>% 
    dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
    dplyr::select(-mode) %>% 
    as.data.table()
  
  data.table::setkey(trip_data, "domain2")
  
  trip_data$tot_keep_cod_new<-0
  trip_data$tot_rel_cod_new<-0
}

#########################
###  Haddock  ####
#########################


if (had_catch_check!=0){
  # subset trips with zero catch, as no size draws are required
  had_zero_catch <- dplyr::filter(cod_had_catch_data, tot_had_catch == 0)
  
  #keep trips with positive catch
  had_catch_data <- dplyr::filter(cod_had_catch_data, tot_had_catch > 0)
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(had_catch_data))
  
  had_catch_data<- had_catch_data %>%
    dplyr::slice(rep(row_inds,tot_had_catch))
  
  rownames(had_catch_data) <- NULL
  had_catch_data$fishid <- 1:nrow(had_catch_data)
  
  # generate lengths for each fish
  catch_size_data_had <- had_catch_data %>%
    dplyr::mutate(fitted_length = sample(had_size_data$length,
                                         nrow(.),
                                         prob = had_size_data$proj_CaL_prob_smooth,
                                         #prob = had_size_data$fitted_prob,
                                         replace = TRUE)) 
  
  
  
  #Create as an object the minimum size at which fish are illegally harvested.
  # This object "floor_subl_harvest" will be 2 inches below the minimum size, by mode. 
  #1) If the minimum size changes across the season, floor_subl_harvest=min(min_size) - 2.
  #2a) If the fishery is closed the entire season, floor_subl_harvest=min(min. size of the previous season) -2 
  #2b) If the fishery is closed the entire current and previous season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
  
  # 2b) below:
  
  # if (floor_subl_hadd_harv==248.92){
  #   floor_subl_hadd_harv=mean(catch_size_data_had$fitted_length)-0.5*sd(catch_size_data_had$fitted_length)
  #   }
  
  if (floor_subl_hadd_harv==98){
    floor_subl_hadd_harv=mean(catch_size_data_had$fitted_length)-0.5*sd(catch_size_data_had$fitted_length)
  }
  
  
  # Impose regulations, calculate keep and release per trip
  ####### Start Here #################
  
  ############# Length #####################################
  catch_size_data_had <- catch_size_data_had %>%
    dplyr::left_join(regs, by = c("period2")) %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=hadd_min_y2 ,1,0)) %>%
    dplyr::group_by(tripid, period2, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        hadd_bag_y2 > 0 ~ ifelse(csum_keep<=hadd_bag_y2 & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data_had <- catch_size_data_had %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data_had <- catch_size_data_had %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0))
  
  catch_size_data_had<- catch_size_data_had %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, period2, catch_draw, mode)  %>%
    dplyr::rename(mode1=mode) %>% 
    dplyr::mutate(floor_subl_hadd_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_hadd_harv~1,TRUE~0))
  
  new_size_data <- catch_size_data_had %>%
    dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
    dplyr::summarize(keep = sum(keep),
                     release = sum(release), .groups = "drop") %>%
    dplyr::ungroup()
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(names_from = fitted_length, #_length,
                       names_glue = "keep_had_{fitted_length}",
                       names_sort = TRUE,
                       values_from = keep,
                       values_fill = 0)
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(names_from = fitted_length, #_length,
                       names_glue = "release_had_{fitted_length}",
                       names_sort = TRUE,
                       values_from = release,
                       values_fill = 0)
  
  keep_release_hadd <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
  
  
  trip_data_hadd <- catch_size_data_had %>%
    dplyr::group_by(period2, catch_draw, tripid) %>%
    dplyr::summarize(tot_keep_hadd_new = sum(keep),
                     tot_rel_hadd_new = sum(release),
                     floor_subl_hadd_harv_indicator=sum(floor_subl_hadd_harv_indicator),
                     .groups = "drop") %>%
    dplyr::ungroup()
  
  
  had_zero_catch<-had_zero_catch %>%
    dplyr::select(tripid, catch_draw, period2) %>%
    dplyr::mutate(tot_keep_hadd_new=0,
                  tot_rel_hadd_new=0)
  
  trip_data_hadd <- dplyr::bind_rows(trip_data_hadd, had_zero_catch) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::select(c("period2", "catch_draw","tripid",
                    "tot_keep_hadd_new","tot_rel_hadd_new", "floor_subl_hadd_harv_indicator"))
  
  
  trip_data_hadd<- trip_data_hadd %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
    dplyr::select(-period2, -catch_draw, -tripid)
  trip_data_hadd<-data.table::as.data.table(trip_data_hadd)
  data.table::setkey(trip_data_hadd, "domain2")
  
  # merge the bsb trip data with the rest of the trip data
  trip_data<-trip_data[trip_data_hadd, on = "domain2"]
  
}


if (had_catch_check==0 & cod_catch_check!=0){
  
  trip_data_hadd<-cod_had_catch_data  %>% 
    dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
    dplyr::select(-mode, -period2, -catch_draw, -tripid) %>% 
    as.data.table()
  
  data.table::setkey(trip_data_hadd, "domain2")
  
  trip_data_hadd$tot_keep_hadd_new<-0
  trip_data_hadd$tot_rel_hadd_new<-0
  trip_data<-trip_data[trip_data_hadd, on = "domain2"]
  
}


trip_data<- trip_data %>% as.data.frame() 

if (cod_catch_check!=0){
  
  #If we need to re-allocate cod releases as harvest, cod_release_2_keep will equal 1 
  if (cod_release_2_keep==1){
    
    trip_data_cod_hstar<-trip_data %>% 
      dplyr::select(period2, tripid, catch_draw, tot_keep_cod_new, tot_rel_cod_new, floor_subl_cod_harv_indicator) %>% 
      dplyr::group_by(period2, tripid) %>% 
      dplyr::summarise(sum_floor_subl_cod_harv_indicator=sum(floor_subl_cod_harv_indicator), .groups='drop') %>% 
      dplyr::filter(sum_floor_subl_cod_harv_indicator>0) 
    
    n_row_cod_hstar<-nrow(trip_data_cod_hstar)
    
    trip_data_cod_hstar<-trip_data_cod_hstar %>% 
      dplyr::mutate(uniform=runif(n_row_cod_hstar)) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(tripid2=1:n_row_cod_hstar)
    
    n_occasions_keep_all_cod=round(h_star_cod_release_to_keep_variable*nrow(trip_data_cod_hstar))
    
    trip_data_cod_hstar <-trip_data_cod_hstar %>% 
      dplyr::filter(tripid2<=n_occasions_keep_all_cod) %>% 
      dplyr::mutate(release_to_keep=1) %>% 
      dplyr::select(period2, tripid, release_to_keep)
    
    trip_data<-trip_data %>% 
      dplyr::left_join(trip_data_cod_hstar, by = c("period2","tripid")) %>% 
      dplyr::mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
      dplyr::mutate(tot_keep_cod_new1=ifelse(release_to_keep==1 & floor_subl_cod_harv_indicator>0,
                                             tot_keep_cod_new+floor_subl_cod_harv_indicator, tot_keep_cod_new), 
                    tot_rel_cod_new1= ifelse(release_to_keep==1 & floor_subl_cod_harv_indicator>0, 
                                             tot_rel_cod_new-floor_subl_cod_harv_indicator, tot_rel_cod_new )) %>% 
      dplyr::mutate(tot_keep_cod_new= tot_keep_cod_new1, 
                    tot_rel_cod_new = tot_rel_cod_new1) %>% 
      dplyr::select(-tot_keep_cod_new1, -tot_rel_cod_new1, -release_to_keep, -floor_subl_cod_harv_indicator)
    
    
    
  }
  
  
  #If we need to re-allocate cod harvest as releases, cod_keep_2_release will equal 1 
  if (cod_keep_2_release==1){
    
    #If we need to re-allocate ALL cod harvest as releases, all_cod_keep_2_release will equal 1 
    if (all_cod_keep_2_release==1){
      
      trip_data<-trip_data %>% 
        dplyr::mutate(tot_rel_cod_new1 = tot_keep_cod_new+tot_rel_cod_new, 
                      tot_keep_cod_new1 = 0) %>% 
        dplyr::mutate(tot_keep_cod_new=tot_keep_cod_new1, 
                      tot_rel_cod_new=tot_rel_cod_new1) %>% 
        dplyr::select(-tot_keep_cod_new1, -tot_rel_cod_new1)
      
    }
    
    #If we need to re-allocate some cod harvest as releases, do the following
    
    if (all_cod_keep_2_release==0){
      
      trip_data_cod_hstar<-trip_data %>% 
        dplyr::select(period2, tripid, catch_draw, tot_keep_cod_new, tot_rel_cod_new) %>% 
        dplyr::group_by(period2, tripid) %>% 
        dplyr::summarise(sum_tot_keep_cod_new=sum(tot_keep_cod_new), 
                         sum_tot_rel_cod_new=sum(tot_rel_cod_new), .groups='drop') %>% 
        dplyr::filter(sum_tot_keep_cod_new>0) 
      
      n_row_cod_hstar<-nrow(trip_data_cod_hstar)
      
      trip_data_cod_hstar<-trip_data_cod_hstar %>% 
        dplyr::mutate(uniform=runif(n_row_cod_hstar)) %>%
        dplyr::arrange(uniform) %>% 
        dplyr::mutate(tripid2=1:n_row_cod_hstar)
      
      n_occasions_release_all_cod=round(h_star_cod_keep_to_release_variable*nrow(trip_data_cod_hstar))
      
      trip_data_cod_hstar <-trip_data_cod_hstar %>% 
        dplyr::filter(tripid2<=n_occasions_release_all_cod) %>% 
        dplyr::mutate(keep_to_release=1) %>% 
        dplyr::select(period2, tripid, keep_to_release)
      
      trip_data<-trip_data %>% 
        dplyr::left_join(trip_data_cod_hstar, by = c("period2","tripid")) %>% 
        dplyr::mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
        dplyr::mutate(tot_rel_cod_new1=ifelse(keep_to_release==1,tot_keep_cod_new+tot_rel_cod_new, tot_rel_cod_new), 
                      tot_keep_cod_new1= ifelse(keep_to_release==1, 0, tot_keep_cod_new )) %>% 
        dplyr::mutate(tot_keep_cod_new= tot_keep_cod_new1, 
                      tot_rel_cod_new = tot_rel_cod_new1) %>% 
        dplyr::select(-tot_keep_cod_new1, -tot_rel_cod_new1, -keep_to_release)
      
      
    }
    
  }
  
  
  
  
}

if (had_catch_check!=0){
  
  #If we need to re-allocate hadd releases as harvest, hadd_release_2_keep will equal 1 
  if (hadd_release_2_keep==1){
    
    trip_data_hadd_hstar<-trip_data %>% 
      dplyr::select(period2, tripid, catch_draw, tot_keep_hadd_new, tot_rel_hadd_new, floor_subl_hadd_harv_indicator) %>% 
      dplyr::group_by(period2, tripid) %>% 
      dplyr::summarise(sum_floor_subl_hadd_harv_indicator=sum(floor_subl_hadd_harv_indicator), .groups='drop') %>% 
      dplyr::filter(sum_floor_subl_hadd_harv_indicator>0) 
    
    
    n_row_hadd_hstar<-nrow(trip_data_hadd_hstar)
    
    trip_data_hadd_hstar<-trip_data_hadd_hstar %>% 
      dplyr::mutate(uniform=runif(n_row_hadd_hstar)) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(tripid2=1:n_row_hadd_hstar)
    
    
    n_occasions_keep_all_hadd=round(h_star_hadd_release_to_keep_variable*nrow(trip_data_hadd_hstar))
    
    trip_data_hadd_hstar <-trip_data_hadd_hstar %>% 
      dplyr::filter(tripid2<=n_occasions_keep_all_hadd) %>% 
      dplyr::mutate(release_to_keep=1) %>% 
      dplyr::select(period2, tripid, release_to_keep)
    
    trip_data<-trip_data %>% 
      dplyr::left_join(trip_data_hadd_hstar, by = c("period2","tripid")) %>% 
      dplyr::mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
      dplyr::mutate(tot_keep_hadd_new1=ifelse(release_to_keep==1 & floor_subl_hadd_harv_indicator>0,
                                              tot_keep_hadd_new+floor_subl_hadd_harv_indicator, tot_keep_hadd_new), 
                    tot_rel_hadd_new1= ifelse(release_to_keep==1 & floor_subl_hadd_harv_indicator>0, 
                                              tot_rel_hadd_new-floor_subl_hadd_harv_indicator, tot_rel_hadd_new )) %>% 
      dplyr::mutate(tot_keep_hadd_new= tot_keep_hadd_new1, 
                    tot_rel_hadd_new = tot_rel_hadd_new1) %>% 
      dplyr::select(-tot_keep_hadd_new1, -tot_rel_hadd_new1, -release_to_keep, -floor_subl_hadd_harv_indicator)
    
  }
  
  
  #If we need to re-allocate hadd harvest as releases, hadd_keep_2_release will equal 1 
  if (hadd_keep_2_release==1){
    
    #If we need to re-allocate ALL hadd harvest as releases, all_hadd_keep_2_release will equal 1 
    if (all_hadd_keep_2_release==1){
      
      trip_data<-trip_data %>% 
        dplyr::mutate(tot_rel_hadd_new1 = tot_keep_hadd_new+tot_rel_hadd_new, 
                      tot_keep_hadd_new1 = 0) %>% 
        dplyr::mutate(tot_keep_hadd_new=tot_keep_hadd_new1, 
                      tot_rel_hadd_new=tot_rel_hadd_new1) %>% 
        dplyr::select(-tot_keep_hadd_new1, -tot_rel_hadd_new1)
      
    }
    
    #If we need to re-allocate some hadd harvest as releases, do the following
    
    if (all_hadd_keep_2_release==0){
      
      trip_data_hadd_hstar<-trip_data %>% 
        dplyr::select(period2, tripid, catch_draw, tot_keep_hadd_new, tot_rel_hadd_new) %>% 
        dplyr::group_by(period2, tripid) %>% 
        dplyr::summarise(sum_tot_keep_hadd_new=sum(tot_keep_hadd_new), 
                         sum_tot_rel_hadd_new=sum(tot_rel_hadd_new), .groups='drop') %>% 
        dplyr::filter(sum_tot_keep_hadd_new>0) 
      
      n_row_hadd_hstar<-nrow(trip_data_hadd_hstar)
      
      trip_data_hadd_hstar<-trip_data_hadd_hstar %>% 
        dplyr::mutate(uniform=runif(n_row_hadd_hstar)) %>%
        dplyr::arrange(uniform) %>% 
        dplyr::mutate(tripid2=1:n_row_hadd_hstar)
      
      n_occasions_release_all_hadd=round(h_star_hadd_keep_to_release_variable*nrow(trip_data_hadd_hstar))
      
      trip_data_hadd_hstar <-trip_data_hadd_hstar %>% 
        dplyr::filter(tripid2<=n_occasions_release_all_hadd) %>% 
        dplyr::mutate(keep_to_release=1) %>% 
        dplyr::select(period2, tripid, keep_to_release)
      
      trip_data<-trip_data %>% 
        dplyr::left_join(trip_data_hadd_hstar, by = c("period2","tripid")) %>% 
        dplyr::mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
        dplyr::mutate(tot_rel_hadd_new1=ifelse(keep_to_release==1,tot_keep_hadd_new+tot_rel_hadd_new, tot_rel_hadd_new), 
                      tot_keep_hadd_new1= ifelse(keep_to_release==1, 0, tot_keep_hadd_new )) %>% 
        dplyr::mutate(tot_keep_hadd_new= tot_keep_hadd_new1, 
                      tot_rel_hadd_new = tot_rel_hadd_new1) %>% 
        dplyr::select(-tot_keep_hadd_new1, -tot_rel_hadd_new1, -keep_to_release)
      
      
    }
  }
  
  
}


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
  

#=====================================#
#Now merge the new trip data to the baseline trip data 

trip_data<- trip_data %>% as.data.frame() %>% 
  dplyr::left_join(costs_new_all, by = c("period2","tripid", "catch_draw")) %>% 
  dplyr::arrange(period2, tripid, catch_draw)

trip_data<- trip_data %>% 
  dplyr::rename(tot_keep_hadd_base=tot_keep_had_base, 
                tot_rel_hadd_base=tot_rel_had_base) %>% 
  dplyr::mutate(tot_cat_cod_new=tot_keep_cod_new+tot_rel_cod_new, 
                tot_cat_cod_base=tot_keep_cod_base+tot_rel_cod_base,
                tot_cat_hadd_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                tot_cat_hadd_base=tot_keep_hadd_base+tot_rel_hadd_base) %>% 
  dplyr::select(-domain2, -n_cal_draw)


trip_data <- trip_data %>%
  dplyr::mutate(period = as.numeric(as.factor(period2)))

period_names<-subset(trip_data, select=c("period", "period2"))
period_names <- period_names[!duplicated(period_names), ]

#  utility (prediction year)
trip_data <-trip_data %>%
  dplyr::mutate(
    vA = beta_sqrt_cod_keep*sqrt(tot_keep_cod_new) +
      beta_sqrt_cod_release*sqrt(tot_rel_cod_new) +
      beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_new) +
      beta_sqrt_hadd_release*sqrt(tot_rel_hadd_new) +
      #beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod_new)*sqrt(tot_keep_hadd_new)) +
      beta_cost*cost,
    
    #  utility (base year)
    v0 = beta_sqrt_cod_keep*sqrt(tot_keep_cod_base) +
      beta_sqrt_cod_release*sqrt(tot_rel_cod_base) +
      beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_base) +
      beta_sqrt_hadd_release*sqrt(tot_rel_hadd_base) +
      #beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod_base)*sqrt(tot_keep_hadd_base)) +
      beta_cost*cost)


mean_trip_data <- trip_data %>%
  data.table::data.table()

mean_trip_data <- mean_trip_data %>% dplyr::arrange(period2, tripid, catch_draw)



# Now expand the data to create two alternatives, representing the alternatives available in choice survey
mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
  tidyr::uncount(n_alt) %>%
  dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                opt_out = ifelse(alt == 2, 1, 0))


mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() %>%
  .[, vA_optout := beta_opt_out*opt_out+beta_opt_out_age*age + beta_opt_out_likely*days_fished] %>%
  .[, v0_optout := beta_opt_out*opt_out+beta_opt_out_age*age + beta_opt_out_likely*days_fished] %>%
  .[alt==1, expon_vA := exp(vA)] %>%
  .[alt==2, expon_vA := exp(vA_optout)] %>%
  .[alt==1, expon_v0 := exp(v0)] %>%
  .[alt==2, expon_v0 := exp(v0_optout)]


mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() %>%
  .[, vA_col_sum := base::sum(expon_vA), by=list(period2, catch_draw, tripid)]  %>%
  .[, v0_col_sum := base::sum(expon_v0), by=list(period2, catch_draw, tripid)]


mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() %>%
  .[, change_CS := (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))] %>% 
  .[, CS_base := (1/beta_cost)*log(v0_col_sum)] %>%
  .[, CS_alt := (1/beta_cost)*log(vA_col_sum)] %>%
  .[, probA :=expon_vA/vA_col_sum] %>%
  .[, prob0 :=expon_v0/v0_col_sum]

mean_trip_data<- subset(mean_trip_data, alt==1)

mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table()


all_vars<-c()
all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c( "period","tripid", "period2", "mode")]

mean_trip_data<-mean_trip_data %>% data.table::as.data.table() 

mean_trip_data <- mean_trip_data %>%
  .[,lapply(.SD, base::mean), by = c("tripid", "period2"), .SDcols = all_vars]

# Get rid of things we don't need.
mean_trip_data <- mean_trip_data %>% 
       dplyr::filter(alt==1) %>% 
       dplyr::select(-c(alt, beta_cost,beta_opt_out, beta_opt_out_age, 
                        beta_opt_out_likely, beta_opt_out_prefer, #beta_sqrt_cod_hadd_keep, 
                        beta_sqrt_cod_keep, beta_sqrt_cod_release, beta_sqrt_hadd_keep, 
                        beta_sqrt_hadd_release, days_fished, open, catch_draw, expon_v0,
                        v0_col_sum, expon_vA, opt_out, v0, v0_optout, vA, vA_optout, vA_col_sum, cost, age))

# Multiply the average trip probability by each of the catch variables to get probability-weighted catch
list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new"  )

mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() 


mean_trip_data<-mean_trip_data %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
  .[]

# Multiply the average trip probability in baseline year by each of the catch variables in the basleine year to get probability-weighted catch
list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base"  )

mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
  .[]


mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_choice_occasions = rep(1,nrow(.))) %>%
  dplyr::left_join(period_names, by = c("period2"))

#===============================#
# Take mean of catch_draw for length
all_vars<-c()
all_vars <- names(length_data)[!names(length_data) %in% c("period2","tripid", "catch_draw" )]
all_vars

length_data<- length_data %>%
  data.table::data.table() %>%
  .[,lapply(.SD, base::mean), by = c("period2","tripid"), .SDcols = all_vars]


length_data2<- mean_trip_data %>%
  dplyr::select(period2, tripid, probA) %>%
  dplyr::left_join(length_data, by = c("period2", "tripid")) #%>%

all_vars<-c()
all_vars <- names(length_data2)[!names(length_data2) %in% c("period2","tripid", "probA" )]
all_vars


length_data3 <- length_data2 %>% 
  data.table::as.data.table()  %>%
  .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = all_vars] %>%
  .[]

#===============================#

mean_trip_data <- mean_trip_data%>%
  dplyr::mutate(n_choice_occasions_alt = rep(1,nrow(.))) %>% 
  dplyr::select(-n_choice_occasions)

sims <- calibration_data_table %>%
  dplyr::select(c(n_choice_occasions, period2)) %>%
  dplyr::left_join(mean_trip_data, by = c("period2")) %>%
  dplyr::mutate(ndraws = c(50)) %>%
  tidyr::separate(period2, into = c("month", "day", "mode")) %>%
  dplyr::mutate(month = as.numeric(month)) %>%

  # ## Here we adjust the number of choice occasions to simulate to account for
  ## different numbers of weekend vs. weekday in the projection year versus the calibration
  dplyr::left_join(calendar_2024_adjust, by=c("month", "mode")) %>%

  #multiply the number of choice occasions in the baseline year by the expansion factor
  #For Kim: When we run the projections for 2024, change the "n_choice_occasions*1" below to "n_choice_occasions*expansion_factor" - I already did this
  dplyr::mutate(n_choice_occasions = n_choice_occasions*expansion_factor) %>%
  dplyr::mutate(expand = n_choice_occasions/ndraws) %>%
  dplyr::mutate(period2 = paste0(month, "_", day, "_", mode)) %>%
  dplyr::arrange(period2)


### Keep all sp_length_mode columns and multiple by expand outside function -
##### Should be same number of rows - merge on (period2, tripid)
length_expand <- sims %>%
  dplyr::select(period2, tripid, expand) %>%
  dplyr::left_join(length_data3, by = c("period2", "tripid")) %>%
  dplyr::select(-probA)

all_vars<-c()
all_vars <- names(length_expand)[!names(length_expand) %in% c("period2", "tripid", "expand")]
all_vars

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
                length_cm = length_in*2.54)  %>%  #Convert to cm 
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


l_w_sum <- length_weight %>%
  dplyr::mutate(Var1 = paste0(Species, "_", mode1, "_", keep_release)) %>%
  dplyr::select(Var1, Total_Number, Total_Weight, Discmortality_Total_Weight, Discmortality_Total_Number) %>%
  tidyr::pivot_longer(!Var1, names_to = "Var", values_to = "Value") %>%
  dplyr::mutate(Var = paste0(Var1,"_",Var)) %>%
  dplyr::select(!Var1) %>%
  dplyr::filter(is.na(Value)==FALSE) %>% 
  dplyr::filter(!grepl('keep_Discmortality', Var)) %>% 
  dplyr::mutate(Var=str_replace_all(Var, "release_Discmortality", "Discmortality"))
  

trip_level_output <- sims %>%
  dplyr::select(c(period2,  n_choice_occasions, tripid, expand, change_CS, CS_base, CS_alt,  probA, prob0,
                  tot_keep_cod_new, tot_rel_cod_new, tot_keep_hadd_new, tot_rel_hadd_new,
                  tot_keep_cod_base, tot_rel_cod_base, tot_keep_hadd_base,tot_rel_hadd_base)) %>%
  tidyr::separate(period2, into = c("month", "day", "mode"), sep = "_") %>%
  dplyr::mutate(day = as.numeric(day),
                month = as.numeric(month),
                period2 = paste0(month, "_", day, "_", mode)) %>% 
  as.data.frame()


#Metrics at the choice occasion level
prediction_output_by_period2 <- trip_level_output %>%
  
  data.table::as.data.table() %>%
  .[, cv_sum := expand*change_CS] %>%

  .[, cod_keep_sum := expand*tot_keep_cod_new] %>%
  .[, cod_rel_sum := expand*tot_rel_cod_new] %>%
  
  .[, hadd_keep_sum := expand*tot_keep_hadd_new] %>%
  .[, hadd_rel_sum := expand*tot_rel_hadd_new] %>%
  
  .[, cod_keep_base_sum := expand*tot_keep_cod_base] %>%
  .[, cod_rel_base_sum := expand*tot_rel_cod_base] %>%
  
  .[, hadd_keep_base_sum := expand*tot_keep_hadd_base] %>%
  .[, hadd_rel_base_sum := expand*tot_rel_hadd_new] %>%
  
  .[, ntrips_alt := expand*probA] 


#prediction_output_by_period1 contains CV and ntrips estimates by mode
prediction_output_by_period1 <- prediction_output_by_period2 %>%
  dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
  dplyr::group_by(mode) %>%
  dplyr::summarise(CV = sum(cv_sum),
                   ntrips = sum(ntrips_alt),
                   nchoiceoccasions=sum(expand)) %>%  
                   # codkeepsum=sum(cod_keep_sum),
                   # codrelsum=sum(cod_rel_sum),
                   # haddkeepsum=sum(hadd_keep_sum),
                   # haddrelsum=sum(hadd_rel_sum)) %>%
  dplyr::ungroup()

#prediction_sum contains CV and ntrips estimates
prediction_sum<- prediction_output_by_period1 %>%
  tidyr::pivot_longer(!mode, names_to = "Var", values_to = "Value") %>%
  dplyr::mutate(Var = paste0(Var, "_", mode)) %>%
  dplyr::select(!mode)


#Now we combine all the data into one file
predictions <- rbind(prediction_sum, l_w_sum) %>%
  tidyr::separate(Var, into = c("Category", "mode", "catch_disposition", "param", "number_weight")) %>%
  dplyr::filter(!Value == "NA") %>%
  dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="CV"~"Dollars",TRUE ~ number_weight)) %>%
  dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="ntrips"~"Ntrips",TRUE ~ number_weight)) %>% 
  dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="nchoiceoccasions"~"n_choice_occasions",TRUE ~ number_weight),
                season = select_season, run = k, mrip_index=i)


output1<-predictions

output2<-rbind(output2, output1)
}


#saveRDS(output2, file = "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check.rds")
write_xlsx(output2, "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_inches5.xlsx")

