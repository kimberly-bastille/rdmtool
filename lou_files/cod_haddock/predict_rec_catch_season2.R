#

pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflicts_prefer(dplyr::mutate)
options(scipen = 100, digits = 3)


directed_trips_file_path = "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/directed_trips_calib_150draws.csv"
catch_draws_file_path = "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws_projection"
size_data_read = read.csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/agepro/projected_CaL_cod_hadd.csv")


i=1

#MRIP_data2<- readRDS("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/MRIP_simulated_data.rds") %>% 
#  dplyr::filter(mrip_index==i)

baseline_comparison<-readRDS("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/calibration_comparison.rds") %>% 
  dplyr::filter(mrip_index==i)

select_mode = unique(baseline_comparison$mode)
select_season = unique(baseline_comparison$open)

# MRIP_stats<-MRIP_data2 %>% 
#   dplyr::rename(tot_cod_catch_mrip=tot_cod_catch, 
#                 tot_cod_keep_mrip= tot_cod_keep, 
#                 tot_cod_rel_mrip=tot_cod_rel, 
#                 tot_hadd_catch_mrip=tot_hadd_catch, 
#                 tot_hadd_keep_mrip=tot_hadd_keep, 
#                 tot_hadd_rel_mrip=tot_hadd_rel, 
#                 dtrip_mrip=dtrip) 

k<- baseline_comparison$draw



x = k
#select_season = 1
calibration_data_table =  readr::read_rds(file.path(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/pds_new_", i,".rds")))
directed_trips_table =  read.csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/directed_trips_calib_150draws.csv")
size_data_read = read.csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/agepro/projected_CaL_cod_hadd.csv")
costs_new_all =  readr::read_rds(file.path(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/costs_", i,".rds")))
catch_data_all = read.csv(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws_projection", k, "_full.csv"))

#l_w_conversion =
n_drawz = 50
n_catch_draws = 30
eff_seed=130


# predict_rec_catch_season <- function( x, select_season,  calibration_data_table,
#                                       directed_trips_table,
#                                       size_data_read,
#                                       costs_new_all,
#                                       catch_data_all,
#                                       #l_w_conversion,
#                                       n_drawz = 50,
#                                       n_catch_draws = 30,
#                                       eff_seed=130 ){
  
  
  cod_lw_a = 0.000005132
  cod_lw_b = 3.1625
  had_lw_a = 0.000009298
  had_lw_b = 3.0205
  
  if(select_season == 1){
    seas = "open"
  }
  if(select_season == 0){
    seas = "closed"
  }
  
  calendar_2024_adjust <- readr::read_csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/next year calendar adjustments.csv", show_col_types = FALSE) %>%
    dplyr::filter(draw == x)
  
  Disc_mort<- readr::read_csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/Discard_Mortality.csv", show_col_types = FALSE)
  
  length_expanded1=list()
  prediction_output_by_period_check = list()
  prediction_output_by_period3 = list()
  discard_stats1=list()
  predictions_test=list()
  
  
  
  print("made it through data read in ")
  
  period_conversion<- directed_trips_table %>%tibble::tibble() %>%
    dplyr::filter(draw == x) %>%
    dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0)) %>%
    dplyr::filter(open == select_season) %>%
    dplyr::select(!open) %>%
    dplyr::mutate(#period1 = paste0(as.numeric(month), "-", as.numeric(stringr::str_extract(day, "^.{2}")), "-", mode),
      period2 = paste0(month, "-", stringr::str_extract(day, "^.{2}"), "-", mode),
      day = as.numeric(stringr::str_extract(day, "^.{2}")),
      month = as.numeric(month)) %>%
    dplyr::select(c(period2, month, day, mode))
  
  
  set.seed(eff_seed)
  # Input the calibration output which contains the number of choice occasions needed to simulate
  calibration_data <- calibration_data_table %>% tibble::tibble()
  
  cod_size_data <- size_data_read %>% dplyr::filter(species == "cod", season == seas)
  had_size_data <- size_data_read %>% dplyr::filter(species == "hadd", season == seas)
  
  ######################################
  ##   Begin simulating trip outcomes ##
  ######################################
  print("into directed trips")
  # Set up an output file for the separately simulated within-season regulatory periods
  directed_trips_p <- directed_trips_table %>%
    tibble::tibble() %>%
    dplyr::filter(draw == x) %>%
    dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0))%>%
    dplyr::mutate(period2 = paste0(month, "-", stringr::str_extract(day, "^.{2}"), "-", mode),
                  n_trips = floor(dtrip),
                  n_draws = n_drawz) %>%
    dplyr::filter(open == select_season) %>%
    dplyr::select(!open)
  
  
  open<- directed_trips_table %>%
    tibble::tibble() %>%
    dplyr::filter(draw == x) %>%
    dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0))%>%
    dplyr::mutate(day = stringr::str_extract(day, '\\d{2}'),
                  period2 = paste0(month, "_", day, "_", mode)) %>%
    dplyr::select(period2, open) %>%
    dplyr::filter(open == select_season) %>%
    dplyr::select(!open)
  
  
  print("first kod")
  period_vec <- directed_trips_p %>%
    dplyr::select(period2, n_draws, month, kod) %>%
    tidyr::uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))
  
  regs <- tibble::tibble(directed_trips_p) %>%
    dplyr::select(period2,
                  cod_bag, cod_min,
                  hadd_bag, hadd_min) %>%
    dplyr::left_join(period_conversion, by = c("period2")) %>%
    dplyr::group_by(period2) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(day = as.character(day),
                  period2 = paste0(month, "_", day, "_", mode))
  
  
  #For Kim: Here is where we need to add import 2024 catch-per-trip files. For testing, I used the catch-per trip
  #contained in the costs_new_all files
  
  cod_catch_data <-  catch_data_all%>%
    dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                  period2 = paste0(month, "_", day, "_", mode))
  
  
  cod_catch_data <- cod_catch_data %>%
    dplyr::filter(period2 %in% c(unique(open$period2))) %>%
    dplyr::group_by(period2) %>%
    dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
    dplyr::mutate(#period = rep(period_vec$period2, each = nsamp),
      catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
      tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
    dplyr::ungroup()
  print("postmutate")
  
  
  cod_had_catch_data <- cod_catch_data
  
  
  # subset trips with zero catch, as no size draws are required
  cod_zero_catch <- dplyr::filter(cod_catch_data, cod_catch == 0) %>%
    dplyr::select(!mode)
  
  #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
  cod_catch_check<-base::sum(cod_catch_data$cod_catch)
  had_catch_check<-base::sum(cod_catch_data$hadd_catch)
  
  
  
  #Since this code is not broken out by mode, manually enter a catch checks by mode to pipe around
  
  if (cod_catch_check!=0){
    
    
    #remove trips with zero summer flounder catch
    cod_catch_data <- dplyr::filter(cod_catch_data, cod_catch > 0)
    
    ##Private catch at length
    cod_catch_data_pr<- cod_catch_data %>% dplyr::filter(mode=="pr")
    row_inds <- seq_len(nrow(cod_catch_data_pr))
    
    cod_catch_data_pr<-cod_catch_data_pr %>%
      dplyr::slice(rep(row_inds,cod_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    cod_size_data_pr<-cod_size_data %>% dplyr::filter(mode=="pr")
    
    catch_size_data_pr <- cod_catch_data_pr %>%
      dplyr::mutate(fitted_length = sample(cod_size_data_pr$length,
                                           nrow(.),
                                           prob = cod_size_data_pr$f_l,
                                           replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
    
    
    
    ##shore catch at length
    cod_catch_data_sh<- cod_catch_data %>% dplyr::filter(mode=="sh")
    row_inds <- seq_len(nrow(cod_catch_data_sh))
    
    cod_catch_data_sh<-cod_catch_data_sh %>%
      dplyr::slice(rep(row_inds,cod_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    cod_size_data_sh<-cod_size_data %>% dplyr::filter(mode=="sh")
    
    catch_size_data_sh <- cod_catch_data_sh %>%
      dplyr::mutate(fitted_length = sample(cod_size_data_sh$length,
                                           nrow(.),
                                           prob = cod_size_data_sh$f_l,
                                           replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
    
    ##fh catch at length
    cod_catch_data_fh<- cod_catch_data %>% dplyr::filter(mode=="fh")
    row_inds <- seq_len(nrow(cod_catch_data_fh))
    
    cod_catch_data_fh<-cod_catch_data_fh %>%
      dplyr::slice(rep(row_inds,cod_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    cod_size_data_fh<-cod_size_data %>% dplyr::filter(mode=="fh")
    
    catch_size_data_fh <- cod_catch_data_fh %>%
      dplyr::mutate(fitted_length = sample(cod_size_data_fh$length,
                                           nrow(.),
                                           prob = cod_size_data_fh$fitted_prob,
                                           replace = TRUE))
    
    catch_size_data <- rbind(catch_size_data_fh, catch_size_data_pr, catch_size_data_sh)
    
    
    # Impose regulations, calculate keep and release per trip
    # For summer flounder, retain keep- and release-at-length
    ####### Start Here #################
    
    ############# Length #####################################
    catch_size_data <- catch_size_data %>%
      dplyr::left_join(regs, by = c("period2", "month", "mode")) %>%
      dplyr::select(!day.x) %>%
      dplyr::rename(day = day.y) %>%
      dplyr::mutate(posskeep = ifelse(fitted_length>=cod_min ,1,0)) %>%
      dplyr::group_by(tripid, period2, catch_draw) %>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj = dplyr::case_when(
          cod_bag > 0 ~ ifelse(csum_keep<=cod_bag & posskeep==1,1,0),
          TRUE ~ 0))
    
    
    
    catch_size_data <- catch_size_data %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    catch_size_data <- catch_size_data %>%
      dplyr::mutate(keep_tot = keep_adj,
                    release = ifelse(keep_adj==0,1,0))
    
    catch_size_data<- catch_size_data %>%
      dplyr::select(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw, mode,
                    cod_keep, cod_rel, hadd_keep, hadd_rel)  %>%
      dplyr::rename(keep = keep_tot,
                    mode1=mode)
    
    new_size_data <- catch_size_data %>%
      dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      dplyr::summarize(keep = sum(keep),
                       release = sum(release), .groups = "drop") %>%
      dplyr::ungroup()
    
    
    summed_catch_data <- new_size_data %>%
      dplyr::group_by(period2, catch_draw, tripid) %>%
      dplyr::summarize(tot_keep_cod_new = sum(keep),
                       tot_rel_cod_new = sum(release),
                       .groups = "drop") %>%
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
    
    trip_data <- summed_catch_data
    
    cod_zero_catch<-cod_zero_catch %>%
      dplyr::select(tripid, catch_draw, period2) %>%
      dplyr::mutate(tot_keep_cod_new=0,
                    tot_rel_cod_new=0)
    
    trip_data <- dplyr::bind_rows(trip_data, cod_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::select(c("period2", "catch_draw","tripid",
                      "tot_keep_cod_new","tot_rel_cod_new"))
    
    
    trip_data<- trip_data %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid))
    trip_data<-data.table::as.data.table(trip_data)
    data.table::setkey(trip_data, "domain2")
  }
  
  
  if (cod_catch_check==0){
    trip_data$tot_cod_catch_new<-0
    trip_data$tot_keep_cod_new<-0
    trip_data$tot_rel_cod_new<-0
  }
  
  
  #######Black Sea Bass
  
  if (had_catch_check!=0){
    
    
    #Testing with Scup code
    had_zero_catch <- dplyr::filter(cod_had_catch_data, hadd_catch == 0) %>%
      dplyr::select(-c("cod_catch"))
    
    #remove trips with zero summer flounder catch
    had_catch_data <- dplyr::filter(cod_had_catch_data, hadd_catch > 0)
    
    ##Private catch at length
    had_catch_data_pr<- had_catch_data %>% dplyr::filter(mode=="pr")
    row_inds <- seq_len(nrow(had_catch_data_pr))
    
    had_catch_data_pr<-had_catch_data_pr %>%
      dplyr::slice(rep(row_inds,hadd_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    had_size_data_pr<-had_size_data %>% dplyr::filter(mode=="pr")
    
    catch_size_data_pr <- had_catch_data_pr %>%
      dplyr::mutate(fitted_length = sample(had_size_data_pr$length,
                                           nrow(.),
                                           prob = had_size_data_pr$f_l,
                                           replace = TRUE))
    
    
    ##shore catch at length
    had_catch_data_sh<- had_catch_data %>% dplyr::filter(mode=="sh")
    row_inds <- seq_len(nrow(had_catch_data_sh))
    
    had_catch_data_sh<-had_catch_data_sh %>%
      dplyr::slice(rep(row_inds,hadd_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    had_size_data_sh<-had_size_data %>% dplyr::filter(mode=="sh")
    
    catch_size_data_sh <- had_catch_data_sh %>%
      dplyr::mutate(fitted_length = sample(had_size_data_sh$length,
                                           nrow(.),
                                           prob = had_size_data_sh$f_l,
                                           replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
    
    ##fh catch at length
    had_catch_data_fh<- had_catch_data %>% dplyr::filter(mode=="fh")
    row_inds <- seq_len(nrow(had_catch_data_fh))
    
    had_catch_data_fh<-had_catch_data_fh %>%
      dplyr::slice(rep(row_inds,hadd_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    had_size_data_fh<-had_size_data %>% dplyr::filter(mode=="fh")
    
    catch_size_data_fh <- had_catch_data_fh %>%
      dplyr::mutate(fitted_length = sample(had_size_data_fh$length,
                                           nrow(.),
                                           prob = had_size_data_fh$f_l,
                                           replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
    
    catch_size_data <- rbind(catch_size_data_fh, catch_size_data_pr, catch_size_data_sh)
    
    
    catch_size_data <- catch_size_data %>%
      dplyr::left_join(regs, by = c("period2", "month", "mode")) %>%
      dplyr::select(!day.x) %>%
      dplyr::rename(day = day.y) %>%
      dplyr::mutate(posskeep = ifelse(fitted_length>=as.numeric(hadd_min) ,1,0)) %>%
      dplyr::group_by(tripid, period2, catch_draw) %>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj = dplyr::case_when(
          hadd_bag > 0 ~ ifelse(csum_keep<=hadd_bag & posskeep==1,1,0),
          TRUE ~ 0))
    
    #catch_size_data[is.na(catch_size_data)] <- 0
    catch_size_data <- catch_size_data %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    catch_size_data <- catch_size_data %>%
      dplyr::mutate(keep_tot = keep_adj,
                    release = ifelse(keep_adj==0,1,0))
    
    catch_size_data<- catch_size_data %>%
      dplyr::select(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw, mode,
                    cod_keep, cod_rel, hadd_keep, hadd_rel)  %>%
      dplyr::rename(keep = keep_tot,
                    mode1=mode)
    
    new_size_data <- catch_size_data %>%
      dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      dplyr::summarize(keep = sum(keep),
                       release = sum(release), .groups = "drop") %>%
      dplyr::ungroup()
    
    
    summed_catch_data <- new_size_data %>%
      dplyr::group_by(period2, catch_draw, tripid) %>%
      dplyr::summarize(tot_keep_had_new = sum(keep),
                       tot_rel_had_new = sum(release),
                       .groups = "drop") %>%
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
    
    keep_release_had <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
    
    trip_data_had <- summed_catch_data
    
    had_zero_catch<-had_zero_catch %>%
      dplyr::select(tripid, catch_draw, period2) %>%
      dplyr::mutate(tot_keep_had_new=0,
                    tot_rel_had_new=0)
    
    trip_data_had <- dplyr::bind_rows(trip_data_had, had_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::select(c("period2", "catch_draw","tripid",
                      "tot_keep_had_new","tot_rel_had_new"))
    
    
    trip_data_had<- trip_data_had %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid))
    trip_data_had<-data.table::as.data.table(trip_data_had)
    data.table::setkey(trip_data_had, "domain2")
    
    trip_data<-trip_data[trip_data_had, on = "domain2"]
    trip_data<-trip_data %>% dplyr::select(-i.tripid, -i.catch_draw, -i.period2)
  }
  
  if (had_catch_check==0){
    trip_data$tot_had_catch_new<-0
    trip_data$tot_keep_had_new<-0
    trip_data$tot_rel_had_new<-0
  }
  
  
  
  
  
  
  
  length_data <- keep_release_cod %>%
    dplyr::full_join(keep_release_had, by = c("period2","tripid", "catch_draw"))
  
  
  
  
  # length_data <- keep_release_cod %>%
  #   dplyr::full_join(keep_release_had, by = c("period2","tripid", "catch_draw")) %>%
  #   dplyr::full_join(keep_release_scup, by = c("period2","tripid", "catch_draw"))
  length_data[is.na(length_data)] <- 0
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw)
  
  
  
  zero_catch_check <- cod_zero_catch %>%  dplyr::left_join(had_zero_catch) %>%
    dplyr::filter(tot_keep_cod_new==0 & tot_rel_cod_new==0 &
                    tot_keep_had_new==0 & tot_rel_had_new==0) %>%
    dplyr::select("period2","tripid", "catch_draw")
  
  
  # zero_catch_check <- cod_zero_catch %>%  dplyr::left_join(had_zero_catch) %>% dplyr::left_join(scup_zero_catch) %>%
  #   dplyr::filter(tot_keep_cod_new==0 & tot_rel_cod_new==0 &
  #                 tot_keep_had_new==0 & tot_rel_had_new==0 &
  #                 tot_keep_scup_new==0 & tot_rel_scup_new==0) %>%
  #   dplyr::select("period2","tripid", "catch_draw")
  
  length_data<- plyr::rbind.fill(length_data, zero_catch_check)
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw)
  
  length_data[is.na(length_data)] <- 0
  
  
  
  costs_new_all2 <- data.frame(costs_new_all) %>% #tibble() %>%
    #dplyr::mutate_at(.vars = "period2", .funs = gsub,
    #                 pattern = "-", replace = "_") %>%
    dplyr::rename(beta_sqrt_cod_keep_base=beta_sqrt_cod_keep,
                  beta_sqrt_cod_release_base=beta_sqrt_cod_release,
                  beta_sqrt_hadd_keep_base=beta_sqrt_hadd_keep,
                  beta_sqrt_hadd_release_base=beta_sqrt_hadd_release,
                  beta_sqrt_cod_hadd_keep_base=beta_sqrt_cod_hadd_keep,
                  beta_cost_base = beta_cost,
                  beta_opt_out_base=beta_opt_out,
                  beta_opt_out_age_base=beta_opt_out_age,
                  beta_opt_out_likely_base = beta_opt_out_likely,
                  beta_opt_out_prefer_base = beta_opt_out_prefer)
  
  trip_data[is.na(trip_data)] <- 0
  
  # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
  trip_data <- trip_data %>%
    #tidyr::separate(period2, into =c("month", "day", "mode"), sep = "-") %>%
    #dplyr::mutate(day = as.numeric(day),
    #              period2 = paste0(month, "_", day, "_", mode)) %>%
    #dplyr::select(!c("month", "day", "mode")) %>%
    dplyr::left_join(costs_new_all2, by = c("period2","catch_draw","tripid")) #%>%
  
  
  trip_data[is.na(trip_data)] <- 0
  
  trip_data <- trip_data %>%
    dplyr::mutate(tot_cat_had_base = tot_keep_had_base+tot_rel_had_base,
                  tot_cat_cod_base = tot_keep_cod_base+tot_rel_cod_base)
  
  
  
  #  utility (prediction year)
  trip_data <-trip_data %>%
    dplyr::mutate(
      vA = beta_sqrt_cod_keep_base*sqrt(tot_keep_cod_new) +
        beta_sqrt_cod_release_base*sqrt(tot_rel_cod_new) +
        beta_sqrt_hadd_keep_base*sqrt(tot_keep_had_new) +
        beta_sqrt_hadd_release_base*sqrt(tot_rel_had_new) +
        beta_sqrt_cod_hadd_keep_base*(sqrt(tot_keep_cod_new)*sqrt(tot_keep_had_new)) +
        beta_cost_base*cost,
      
      #  utility (base year)
      v0 = beta_sqrt_cod_keep_base*sqrt(tot_keep_cod_base) +
        beta_sqrt_cod_release_base*sqrt(tot_rel_cod_base) +
        beta_sqrt_hadd_keep_base*sqrt(tot_keep_had_base) +
        beta_sqrt_hadd_release_base*sqrt(tot_rel_had_base) +
        beta_sqrt_cod_hadd_keep_base*(sqrt(tot_keep_cod_base)*sqrt(tot_keep_had_base)) +
        beta_cost_base*cost)
  
  
  trip_data <- trip_data %>%
    dplyr::mutate(period = as.numeric(as.factor(period2)))
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
  #
  # trip_data2 <- trip_data %>%
  #   data.table::as.data.table() %>%
  #   dplyr::left_join(period_conversion, by=c("period2"))
  
  #Checks
  # mean(trip_data$tot_cat_had_base)
  # mean(trip_data$tot_had_catch_new)
  #
  # mean(trip_data$tot_cod_catch_new)
  # mean(trip_data$tot_cat_cod_base)
  #
  # mean(trip_data$tot_scup_catch_new)
  # mean(trip_data$tot_cat_scup_base)
  #
  # mean(trip_data$tot_keep_had_base)
  # mean(trip_data$tot_keep_had_new)
  # #
  # #
  # mean(trip_data[mode1=="fh"]$tot_keep_had_base)
  # mean(trip_data[mode1=="fh"]$tot_keep_had_new)
  #
  # mean(trip_data[mode1=="pr"]$tot_keep_had_base)
  # mean(trip_data[mode1=="pr"]$tot_keep_had_new)
  #
  # mean(trip_data[mode1=="sh"]$tot_keep_had_base)
  # mean(trip_data[mode1=="sh"]$tot_keep_had_new)
  #
  # sum(trip_data[mode1=="fh"]$tot_keep_had_base)
  # sum(trip_data[mode1=="fh"]$tot_keep_had_new)
  #
  # sum(trip_data[mode1=="pr"]$tot_keep_had_base)
  # sum(trip_data[mode1=="pr"]$tot_keep_had_new)
  #
  # sum(trip_data[mode1=="sh"]$tot_keep_had_base)
  # sum(trip_data[mode1=="sh"]$tot_keep_had_new)
  # #
  # mean(trip_data[mode1=="fh"]$tot_keep_cod_base)
  # mean(trip_data[mode1=="fh"]$tot_keep_cod_new)
  #
  # mean(trip_data[mode1=="pr"]$tot_keep_cod_base)
  # mean(trip_data[mode1=="pr"]$tot_keep_cod_new)
  #
  # mean(trip_data[mode1=="sh"]$tot_keep_cod_base)
  # mean(trip_data[mode1=="sh"]$tot_keep_cod_new)
  #
  # sum(trip_data[mode1=="fh"]$tot_keep_cod_base)
  # sum(trip_data[mode1=="fh"]$tot_keep_cod_new)
  #
  # sum(trip_data[mode1=="pr"]$tot_keep_cod_base)
  # sum(trip_data[mode1=="pr"]$tot_keep_cod_new)
  #
  # sum(trip_data[mode1=="sh"]$tot_keep_cod_base)
  # sum(trip_data[mode1=="sh"]$tot_keep_cod_new)
  # #
  # #
  # mean(trip_data[mode1=="fh"]$tot_cat_scup_base)
  # mean(trip_data[mode1=="fh"]$tot_scup_catch_new)
  #
  # mean(trip_data[mode1=="pr"]$tot_cat_scup_base)
  # mean(trip_data[mode1=="pr"]$tot_scup_catch_new)
  #
  # mean(trip_data[mode1=="sh"]$tot_cat_scup_base)
  # mean(trip_data[mode1=="sh"]$tot_scup_catch_new)
  #
  # sum(trip_data[mode1=="fh"]$tot_cat_scup_base)
  # sum(trip_data[mode1=="fh"]$tot_scup_catch_new)
  #
  # sum(trip_data[mode1=="pr"]$tot_cat_scup_base)
  # sum(trip_data[mode1=="pr"]$tot_scup_catch_new)
  #
  # sum(trip_data[mode1=="sh"]$tot_cat_scup_base)
  # sum(trip_data[mode1=="sh"]$tot_scup_catch_new)
  #
  # mean(trip_data$vA)
  # mean(trip_data$v0)
  #
  # # Violin Plots
  #  library(vioplot)
  # cod_keep_base_fh <- trip_data$tot_keep_cod_base[trip_data$mode1=="fh"]
  # cod_keep_new_fh <- trip_data$tot_keep_cod_new[trip_data$mode1=="fh"]
  # vioplot(cod_keep_base_fh, cod_keep_new_fh, names=c("base", "new"))
  #
  # cod_keep_base_fh <- trip_data$tot_keep_cod_base[trip_data$mode1=="sh"]
  # cod_keep_new_fh <- trip_data$tot_keep_cod_new[trip_data$mode1=="sh"]
  # vioplot(cod_keep_base_fh, cod_keep_new_fh, names=c("base", "new"))
  #
  # cod_keep_base_fh <- trip_data$tot_keep_cod_base[trip_data$mode1=="pr"]
  # cod_keep_new_fh <- trip_data$tot_keep_cod_new[trip_data$mode1=="pr"]
  # vioplot(cod_keep_base_fh, cod_keep_new_fh, names=c("base", "new"))
  #
  # cod_keep_base_fh <- trip_data$tot_rel_cod_base[trip_data$mode1=="fh"]
  # cod_keep_new_fh <- trip_data$tot_rel_cod_new[trip_data$mode1=="fh"]
  # vioplot(cod_keep_base_fh, cod_keep_new_fh, names=c("base", "new"))
  #
  # cod_keep_base_fh <- trip_data$tot_rel_cod_base[trip_data$mode1=="pr"]
  # cod_keep_new_fh <- trip_data$tot_rel_cod_new[trip_data$mode1=="pr"]
  # vioplot(cod_keep_base_fh, cod_keep_new_fh, names=c("base", "new"))
  #
  # cod_keep_base_fh <- trip_data$tot_rel_cod_base[trip_data$mode1=="sh"]
  # cod_keep_new_fh <- trip_data$tot_rel_cod_new[trip_data$mode1=="sh"]
  # vioplot(cod_keep_base_fh, cod_keep_new_fh, names=c("base", "new"))
  #
  #
  # had_keep_base_fh <- trip_data$tot_keep_had_base[trip_data$mode1=="pr"]
  # had_keep_new_fh <- trip_data$tot_keep_had_new[trip_data$mode1=="pr"]
  # vioplot(had_keep_base_fh, had_keep_new_fh, names=c("base", "new"))
  #
  # had_keep_base_fh <- trip_data$tot_keep_had_base[trip_data$mode1=="fh"]
  # had_keep_new_fh <- trip_data$tot_keep_had_new[trip_data$mode1=="fh"]
  # vioplot(had_keep_base_fh, had_keep_new_fh, names=c("base", "new"))
  #
  # had_keep_base_fh <- trip_data$tot_keep_had_base[trip_data$mode1=="sh"]
  # had_keep_new_fh <- trip_data$tot_keep_had_new[trip_data$mode1=="sh"]
  # vioplot(had_keep_base_fh, had_keep_new_fh, names=c("base", "new"))
  #
  #
  # vA <- trip_data$vA[trip_data$mode1=="pr"]
  # v0 <- trip_data$v0[trip_data$mode1=="pr"]
  # vioplot(vA, v0, names=c("base", "new"))
  
  
  
  mean_trip_data <- trip_data %>%
    data.table::data.table()
  
  mean_trip_data <- mean_trip_data %>% dplyr::arrange(period2, tripid, catch_draw)
  
  
  #New code to calculate probability of each choice occasion
  
  # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
    tidyr::uncount(n_alt) %>%
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                  opt_out = ifelse(alt == 2, 1, 0))
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, vA_optout := beta_opt_out_base*opt_out+beta_opt_out_age_base*age + beta_opt_out_likely_base*days_fished] %>%
    .[, v0_optout := beta_opt_out_base*opt_out+beta_opt_out_age_base*age + beta_opt_out_likely_base*days_fished] %>%
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
    .[, change_CS := (1/beta_cost_base)*(log(vA_col_sum)-log(v0_col_sum))] %>% ##### SHould this be cost or beta_cost_base??? originally beta_cost
    .[, CS_base := (1/beta_cost_base)*log(v0_col_sum)] %>%
    .[, CS_alt := (1/beta_cost_base)*log(vA_col_sum)] %>%
    .[, probA :=expon_vA/vA_col_sum] %>%
    .[, prob0 :=expon_v0/v0_col_sum]
  
  
  
  # mean(mean_trip_data$change_CS)
  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table()
  #checks
  # mean(mean_trip_data$vA)
  # mean(mean_trip_data$v0)
  # #
  # mean(mean_trip_data$tot_cat_had_base)
  # mean(mean_trip_data$tot_had_catch_new)
  #
  # mean(mean_trip_data$tot_cod_catch_new)
  # mean(mean_trip_data$tot_cat_cod_base)
  #
  # mean(mean_trip_data$tot_scup_catch_new)
  # mean(mean_trip_data$tot_cat_scup_base)
  # #
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_had_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_had_new)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_had_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_had_new)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_had_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_had_new)
  # #
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_cod_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_cod_new)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_cod_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_cod_new)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_cod_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_cod_new)
  #
  #
  #
  # mean(mean_trip_data[mode1=="fh"]$tot_scup_catch_new)
  # mean(mean_trip_data[mode1=="fh"]$tot_cat_scup_base)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_scup_catch_new)
  # mean(mean_trip_data[mode1=="pr"]$tot_cat_scup_base)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_scup_catch_new)
  # mean(mean_trip_data[mode1=="sh"]$tot_cat_scup_base)
  #
  # mean(mean_trip_data$change_CS)
  # mean(mean_trip_data[mode1=="fh"]$change_CS)
  # mean(mean_trip_data[mode1=="pr"]$change_CS)
  # mean(mean_trip_data[mode1=="sh"]$change_CS)
  #
  # mean(mean_trip_data[mode1=="fh"]$cost)
  # mean(mean_trip_data[mode1=="pr"]$cost)
  # mean(mean_trip_data[mode1=="sh"]$cost)
  
  
  # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>%
  #   tibble()
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::select(-c("domain2")) %>% data.table::data.table() #%>% dplyr::arrange(period, tripid, catch_draw)
  
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c( "period","tripid", "period2", "mode1", "month", "day")]
  #all_vars
  mean_trip_data<-mean_trip_data %>% data.table::as.data.table() #%>%
  # dplyr::mutate(month = as.numeric(month),
  #               day = as.numeric(day)) #%>%
  
  mean_trip_data <- mean_trip_data %>%
    .[,lapply(.SD, base::mean), by = c("tripid", "period2"), .SDcols = all_vars]
  
  
  # mean(mean_trip_data$change_CS)
  # mean(mean_trip_data[mode1=="pr"]$vA)
  # mean(mean_trip_data[mode1=="pr"]$v0)
  # mean(mean_trip_data[mode1=="sh"]$change_CS)
  # mean(mean_trip_data[mode1=="fh"]$change_CS2)
  # mean(mean_trip_data[mode1=="pr"]$change_CS2)
  # mean(mean_trip_data[mode1=="sh"]$change_CS2)
  
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_had_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_had_new)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_had_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_had_new)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_had_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_had_new)
  #
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_cod_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_cod_new)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_cod_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_cod_new)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_cod_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_cod_new)
  #
  # max(mean_trip_data[mode1=="sh"]$tot_keep_cod_base)
  # max(mean_trip_data[mode1=="sh"]$tot_keep_cod_new)
  #
  #
  #
  
  # mean(mean_trip_data[mode1=="fh"]$tot_cat_cod_new)
  # mean(mean_trip_data[mode1=="fh"]$tot_cat_cod_base)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_cat_cod_new)
  # mean(mean_trip_data[mode1=="pr"]$tot_cat_cod_base)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_cat_cod_new)
  # mean(mean_trip_data[mode1=="sh"]$tot_cat_cod_base)
  #
  #
  # mean(mean_trip_data[mode1=="fh"]$tot_scup_catch_new)
  # mean(mean_trip_data[mode1=="fh"]$tot_cat_scup_base)
  #
  # mean(mean_trip_data[mode1=="pr"]$tot_scup_catch_new)
  # mean(mean_trip_data[mode1=="pr"]$tot_cat_scup_base)
  #
  # mean(mean_trip_data[mode1=="sh"]$tot_scup_catch_new)
  # mean(mean_trip_data[mode1=="sh"]$tot_cat_scup_base)
  #
  # mean(mean_trip_data$change_CS)
  # mean(mean_trip_data[mode1=="fh"]$change_CS)
  # mean(mean_trip_data[mode1=="pr"]$change_CS)
  # mean(mean_trip_data[mode1=="sh"]$change_CS)
  #
  #
  #
  #
  #
  
  # Get rid of things we don't need.
  mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, cost,
                                                            catch_draw, expon_v0 ,v0_col_sum, expon_vA,
                                                            opt_out, v0, v0_optout, vA, vA_optout, vA_col_sum))
  
  
  mean_trip_data<-mean_trip_data %>%
    dplyr::mutate(tot_had_catch_new=tot_keep_had_new+tot_rel_had_new,
                  tot_cod_catch_new=tot_keep_cod_new+tot_rel_cod_new)
  
  # Multiply the average trip probability by each of the catch variables to get probability-weighted catch
  
  
  list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cod_catch_new",
                  "tot_keep_had_new", "tot_rel_had_new" , "tot_had_catch_new"  )
  
  mean_trip_data <- mean_trip_data %>% ## ADD mean_kr_total for each species by tripid and perdiod2
    data.table::as.data.table() #%>%
  
  mean_trip_data[is.na(mean_trip_data)] <- 0
  
  mean_trip_data<-mean_trip_data %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
    .[]
  
  #########
  
  # Take mean of catch_draw for length
  all_vars<-c()
  all_vars <- names(length_data)[!names(length_data) %in% c("period2","tripid", "catch_draw" )]
  length_data<- length_data %>%
    data.table::data.table() %>%
    .[,lapply(.SD, base::mean), by = c("period2","tripid"), .SDcols = all_vars]
  
  length_data[is.na(length_data)] <- 0
  
  # length_data<- length_data %>%
  #   tidyr::separate(period2, into = c("month", "day", "mode"), sep = "-") %>%
  #   dplyr::mutate(month = as.numeric(month),
  #                 day = as.numeric(day),
  #                 period2 = paste0(month, "_", day, "_", mode)) %>%
  #   dplyr::select(!c(month, day, mode))
  
  length_data2<- mean_trip_data %>%
    dplyr::select(period2, tripid, probA) %>%
    dplyr::full_join(length_data, by = c("period2", "tripid")) #%>%
  length_data2[is.na(length_data2)] <- 0
  all_vars<-c()
  all_vars <- names(length_data2)[!names(length_data2) %in% c("period2","tripid", "mode1", "probA" )]
  
  length_data3 <- length_data2 %>% ## ADD mean_kr_total for each species by tripid and perdiod2
    data.table::as.data.table()  %>%
    .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = all_vars] %>%
    .[]
  
  
  
  
  #Add tot_keep_scup base and tot_rel_scup_base here:
  
  list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_keep_had_base", "tot_rel_had_base")
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    .[]
  
  mean_trip_data<- mean_trip_data %>%
    dplyr::select(unique(colnames(mean_trip_data)))
  
  
  mean_trip_data <- mean_trip_data%>%
    dplyr::mutate( n_choice_occasions_alt = rep(1,nrow(.)))
  
  
  #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in
  #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
  
  sims <- calibration_data %>%
    dplyr::select(c(n_choice_occasions, period2)) %>%
    # dplyr::mutate_at(.vars = "period2", .funs = gsub,
    #                  pattern = "_", replace = "-") %>%
    dplyr::left_join(mean_trip_data, by = c("period2")) %>%
    #dplyr::select()
    dplyr::mutate(ndraws = c(50)) %>%
    #               period = as.character(period2)) %>%
    tidyr::separate(period2, into = c("month", "day", "mode")) %>%
    dplyr::mutate(month = as.numeric(month)) %>%
    # ## Here we adjust the number of choice occasions to simulate to account for
    ## different kind-of-days within a month in 2024 compared to 2022
    dplyr::left_join(calendar_2024_adjust, by=c("month", "mode")) %>%
    dplyr::filter(!tripid == "NA", !expansion_factor == "NA") %>%
    
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
  #
  # ## Move to outside function
  length_expand <- length_expand %>%
    data.table::as.data.table() %>%
    .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(expand)), .SDcols = all_vars] %>%
    .[]
  
  period_conversion <- period_conversion %>%
    dplyr::select(period2) %>%
    tidyr::separate(period2, into = c("month", "day", "mode"), sep = "-") %>%
    dplyr::mutate(day = as.numeric(day),
                  month = as.numeric(month),
                  period2 = paste0(month, "_", day, "_", mode)) %>%
    dplyr::select(period2, month, mode)
  
  length_expanded<- length_expand %>%
    #dplyr::select(period2, mode, month) %>%
    dplyr::left_join(period_conversion, by = c("period2")) %>%
    data.table::data.table()
  
  all_vars<-c()
  all_vars <- names(length_expanded)[!names(length_expanded) %in% c("period2", "mode", "tripid", "expand", "month")]
  
  length_expanded <- length_expanded %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, base::sum), by = c("mode", "month"), .SDcols = all_vars]
  
  
  length_expanded_check<- length_expanded %>%
    tidyr::pivot_longer(cols = !month & !mode,  names_to = "Var", values_to = "Number_at_Length")
  
  
  
  
  #This code translates numbers to weights using the l-w equation. The number_weight var is set to "Weight"
  #Later on we drop the keep and release numbers computed here.
  length_weight<- length_expanded %>%
    tidyr::pivot_longer(cols = !month & !mode,  names_to = "Var", values_to = "Number_at_Length") %>%
    tidyr::separate(Var, into = c("keep_release", "Species", "length"), sep = "_") %>%
    dplyr::rename(Month=month, Mode=mode) %>%
    #dplyr::left_join(l_w_conversion, by = c( "Month", "Species")) %>%
    dplyr::mutate(length_in = as.numeric(length),
                  length_cm = length_in*2.54)  %>%  #Convert to cm
    dplyr::mutate(weight = dplyr::case_when(Species == "cod" ~ cod_lw_a*length_cm^cod_lw_b),
                  weight = dplyr::case_when(Species == "had" ~ had_lw_a*length_cm^had_lw_b, TRUE ~ weight),
                  weight = weight*2.20462262185, #convert to lbs
                  Total_weight = Number_at_Length * weight) %>%
    dplyr::mutate(spp2 = dplyr::case_when(Species == "had" & length_cm >  50 ~ "had_lg",TRUE~ Species),
                  spp2 = dplyr::case_when(Species == "had" & length_cm <=  50 ~ "had_sm", TRUE~spp2)) %>%
    dplyr::left_join(Disc_mort, by = c("Month", "spp2")) %>%
    dplyr::mutate(Discmortality_Total_weight = Discard_mortality * Number_at_Length * weight,
                  Discmortality_Total_Number = Discard_mortality * Number_at_Length) %>%
    dplyr::group_by( Species, Mode, keep_release) %>%
    dplyr::summarise(Total_Number = sum(Number_at_Length),
                     Total_Weight = sum(Total_weight),
                     Discmortality_Total_Weight = sum(Discmortality_Total_weight),
                     Discmortality_Total_Number = sum(Discmortality_Total_Number),.groups = 'drop') %>%
    dplyr::rename(mode1 = Mode) %>%
    dplyr::ungroup()
  
  
  
  l_w_mode <- length_weight %>%
    dplyr::mutate(Var1 = paste0(Species, "_", mode1, "_", keep_release)) %>%
    dplyr::select(Var1, Total_Number, Total_Weight, Discmortality_Total_Weight, Discmortality_Total_Number) %>%
    tidyr::pivot_longer(!Var1, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var1,"_",Var)) %>%
    dplyr::select(!Var1) %>%
    dplyr::filter(is.na(Value)==FALSE)
  
  
  l_w_mode <- data.frame(lapply(l_w_mode, function(x) {
    gsub("release_Discmortality", "Discmortality", x)
  }))
  
  
  l_w_sum <- length_weight %>%
    dplyr::group_by( Species, keep_release) %>%
    dplyr::summarise(Total_Number = sum(Total_Number),
                     Total_Weight = sum(Total_Weight),
                     Discmortality_Total_Weight = sum(Discmortality_Total_Weight),
                     Discmortality_Total_Number = sum(Discmortality_Total_Number), .groups = 'drop') %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mode1="all") %>%
    dplyr::mutate(Var1 = paste0(Species, "_",mode1, "_", keep_release)) %>%
    dplyr::select(Var1, Total_Number, Total_Weight, Discmortality_Total_Weight, Discmortality_Total_Number) %>%
    tidyr::pivot_longer(!Var1, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var1,"_",Var)) %>%
    dplyr::select(!Var1) %>%
    dplyr::filter(is.na(Value)==FALSE)
  
  l_w_sum <- data.frame(lapply(l_w_sum, function(x) {
    gsub("release_Discmortality", "Discmortality", x)
  }))
  
  
  
  trip_level_output <- sims %>%
    dplyr::select(c(period2,  n_choice_occasions, tripid, expand, change_CS, CS_base, CS_alt,  probA, prob0,
                    tot_keep_cod_new, tot_rel_cod_new, tot_keep_had_new, tot_rel_had_new,
                    tot_keep_cod_base, tot_rel_cod_base, tot_keep_had_base,tot_rel_had_base)) %>%
    as.data.frame()
  
  
  prediction_output_by_period1 <- period_conversion %>%
    dplyr::select(period2, month, mode) %>%
    dplyr::right_join(trip_level_output, by = c("period2"))
  
  
  #Metrics at the choice occasion level
  prediction_output_by_period2 <- prediction_output_by_period1 %>%
    
    #prediction_output_by_period2 <- trip_level_output %>%
    data.table::as.data.table() %>%
    .[, cv_sum := expand*change_CS] %>%
    .[, cs_base_sum := expand*CS_base] %>%
    .[, cs_alt_sum := expand*CS_alt] %>%
    .[, cod_keep_sum := expand*tot_keep_cod_new] %>%
    .[, cod_rel_sum := expand*tot_rel_cod_new] %>%
    .[, had_keep_sum := expand*tot_keep_had_new] %>%
    .[, had_rel_sum := expand*tot_rel_had_new] %>%
    
    .[, cod_keep_base_sum := expand*tot_keep_cod_base] %>%
    .[, cod_rel_base_sum := expand*tot_rel_cod_base] %>%
    .[, had_keep_base_sum := expand*tot_keep_had_base] %>%
    .[, had_rel_base_sum := expand*tot_rel_had_new] %>%
    
    .[, ntrips_alt := expand*probA] %>%
    .[mode=="pr", cv_sum_pr := expand*change_CS] %>%
    .[mode=="fh", cv_sum_fh := expand*change_CS] %>%
    .[mode=="sh", cv_sum_sh := expand*change_CS] %>%
    .[mode=="pr", cs_base_pr := expand*CS_base] %>%
    .[mode=="fh", cs_base_fh := expand*CS_base] %>%
    .[mode=="sh", cs_base_sh := expand*CS_base] %>%
    .[mode=="pr", cs_alt_pr := expand*CS_alt] %>%
    .[mode=="fh", cs_alt_fh := expand*CS_alt] %>%
    .[mode=="sh", cs_alt_sh := expand*CS_alt] %>%
    .[mode=="pr", cod_keep_sum_pr := expand*tot_keep_cod_new] %>%
    .[mode=="fh", cod_keep_sum_fh := expand*tot_keep_cod_new] %>%
    .[mode=="sh", cod_keep_sum_sh := expand*tot_keep_cod_new] %>%
    .[mode=="pr", cod_rel_sum_pr := expand*tot_rel_cod_new] %>%
    .[mode=="fh", cod_rel_sum_fh := expand*tot_rel_cod_new] %>%
    .[mode=="sh", cod_rel_sum_sh := expand*tot_rel_cod_new] %>%
    .[mode=="pr", had_keep_sum_pr := expand*tot_keep_had_new] %>%
    .[mode=="fh", had_keep_sum_fh := expand*tot_keep_had_new] %>%
    .[mode=="sh", had_keep_sum_sh := expand*tot_keep_had_new] %>%
    .[mode=="pr", had_rel_sum_pr := expand*tot_rel_had_new] %>%
    .[mode=="fh", had_rel_sum_fh := expand*tot_rel_had_new] %>%
    .[mode=="sh", had_rel_sum_sh := expand*tot_rel_had_new] %>%
    .[mode=="pr", ntrips_pr := expand*probA] %>%
    .[mode=="fh", ntrips_fh := expand*probA] %>%
    .[mode=="sh", ntrips_sh := expand*probA]
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ####Here we compute total harvest weight based on MRIP average weights
  # if (state1 %in% c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
  #
  # prediction_output_by_period_check0 <- prediction_output_by_period2 %>%
  #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
  #   dplyr::group_by(mode1) %>%
  #   dplyr::summarise(cod_keep_sum_num = sum(cod_keep_sum),
  #                     cod_rel_sum_num = sum(cod_rel_sum),
  #                     had_keep_sum_num = sum(had_keep_sum),
  #                     had_rel_sum_num = sum(had_rel_sum),
  #                     scup_keep_sum_num = sum(scup_keep_sum),
  #                     scup_rel_sum_num = sum(scup_rel_sum),
  #                     cod_keep_base_sum_num = sum(cod_keep_base_sum),
  #                     cod_rel_base_sum_num = sum(cod_rel_base_sum),
  #                     had_keep_base_sum_num = sum(had_keep_base_sum),
  #                     had_rel_base_sum_num = sum(had_rel_base_sum)) %>%
  #                     #scup_tot_cat_base_sum = sum(scup_tot_cat_base_sum)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(cod_Discmortality_sum_num = cod_rel_sum_num*.1,
  #                 had_Discmortality_sum_num = had_rel_sum_num*.15,
  #                 scup_Discmortality_sum_num = scup_rel_sum_num*.15,
  #                 cod_Discmortality_sum_weightavg = cod_Discmortality_sum_num*avg_lbs_release_cod,
  #                 had_Discmortality_sum_weightavg = had_Discmortality_sum_num*avg_lbs_release_had,
  #                 scup_Discmortality_sum_weightavg = scup_Discmortality_sum_num*avg_lbs_release_scup) %>%
  #   dplyr::left_join(MRIP_harvest_weights, by=c("mode1")) %>%
  #   dplyr::mutate(cod_keep_sum_weightavg= cod_keep_sum_num*mean_weightcod,
  #                 had_keep_sum_weightavg= had_keep_sum_num*mean_weighthad,
  #                 scup_keep_sum_weightavg= scup_keep_sum_num*mean_weightSCUP,
  #                 cod_release_sum_weightavg= cod_rel_sum_num*avg_lbs_release_cod,
  #                 had_release_sum_weightavg= had_rel_sum_num*avg_lbs_release_had,
  #                 scup_release_sum_weightavg= scup_rel_sum_num*avg_lbs_release_scup) %>%
  #   dplyr::select(mode1, had_keep_sum_num, had_keep_sum_weightavg, had_rel_sum_num,
  #                 scup_keep_sum_num, scup_keep_sum_weightavg, scup_rel_sum_num,
  #                 cod_keep_sum_num, cod_keep_sum_weightavg, cod_rel_sum_num,
  #                 cod_Discmortality_sum_num, had_Discmortality_sum_num, scup_Discmortality_sum_num,
  #                 cod_release_sum_weightavg,had_release_sum_weightavg,scup_release_sum_weightavg,
  #                 scup_Discmortality_sum_weightavg, had_Discmortality_sum_weightavg, cod_Discmortality_sum_weightavg) #%>%
  # }
  #
  #
  #
  #
  #
  # if (state1 %in% c("NJ")){
  #
  #   prediction_output_by_period_check0 <- prediction_output_by_period2 %>%
  #     dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
  #     dplyr::group_by(mode1) %>%
  #     dplyr::summarise(cod_keep_sum_num = sum(cod_keep_sum),
  #                      cod_rel_sum_num = sum(cod_rel_sum),
  #                      had_keep_sum_num = sum(had_keep_sum),
  #                      had_rel_sum_num = sum(had_rel_sum),
  #                      scup_keep_sum_num = sum(scup_keep_sum),
  #                      scup_rel_sum_num = sum(scup_rel_sum),
  #                      cod_keep_base_sum_num = sum(cod_keep_base_sum),
  #                      cod_rel_base_sum_num = sum(cod_rel_base_sum),
  #                      had_keep_base_sum_num = sum(had_keep_base_sum),
  #                      had_rel_base_sum_num = sum(had_rel_base_sum)) %>%
  #     #scup_tot_cat_base_sum = sum(scup_tot_cat_base_sum)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::left_join(NJ_bias_catch1, by="mode1") %>%
  #     dplyr::mutate(cod_keep_sum_num=cod_keep_sum_num+diff_cod_keep,
  #                   had_keep_sum_num=had_keep_sum_num+diff_had_keep,
  #                   scup_keep_sum_num=scup_keep_sum_num+diff_scup_keep,
  #                   cod_rel_sum_num=cod_rel_sum_num+diff_cod_rel,
  #                   had_rel_sum_num=had_rel_sum_num+diff_had_rel,
  #                   scup_rel_sum_num=scup_rel_sum_num+diff_scup_rel) %>%
  #     dplyr::mutate(cod_Discmortality_sum_num = cod_rel_sum_num*.1,
  #                   had_Discmortality_sum_num = had_rel_sum_num*.15,
  #                   scup_Discmortality_sum_num = scup_rel_sum_num*.15,
  #                   cod_Discmortality_sum_weightavg = cod_Discmortality_sum_num*avg_lbs_release_cod,
  #                   had_Discmortality_sum_weightavg = had_Discmortality_sum_num*avg_lbs_release_had,
  #                   scup_Discmortality_sum_weightavg = scup_Discmortality_sum_num*avg_lbs_release_scup) %>%
  #     dplyr::left_join(MRIP_harvest_weights, by=c("mode1")) %>%
  #     dplyr::mutate(cod_keep_sum_weightavg= cod_keep_sum_num*mean_weightcod,
  #                   had_keep_sum_weightavg= had_keep_sum_num*mean_weighthad,
  #                   scup_keep_sum_weightavg= scup_keep_sum_num*mean_weightSCUP,
  #                   cod_release_sum_weightavg= cod_rel_sum_num*avg_lbs_release_cod,
  #                   had_release_sum_weightavg= had_rel_sum_num*avg_lbs_release_had,
  #                   scup_release_sum_weightavg= scup_rel_sum_num*avg_lbs_release_scup) %>%
  #     dplyr::select(mode1, had_keep_sum_num, had_keep_sum_weightavg, had_rel_sum_num,
  #                   scup_keep_sum_num, scup_keep_sum_weightavg, scup_rel_sum_num,
  #                   cod_keep_sum_num, cod_keep_sum_weightavg, cod_rel_sum_num,
  #                   cod_Discmortality_sum_num, had_Discmortality_sum_num, scup_Discmortality_sum_num,
  #                   cod_release_sum_weightavg,had_release_sum_weightavg,scup_release_sum_weightavg,
  #                   scup_Discmortality_sum_weightavg, had_Discmortality_sum_weightavg, cod_Discmortality_sum_weightavg) #%>%
  # }
  #
  
  
  # #prediction_output_by_period_check01 contains keep, release,discard mortality numbers, and keep weights
  # #based on MRIP average weights by mode. The number_weight var is set to "Weight"
  # prediction_output_by_period_check01<- prediction_output_by_period_check0 %>%
  #   tidyr::pivot_longer(!mode1, names_to = "Var", values_to = "Value") %>%
  #   dplyr::rename(mode=mode1) %>%
  #   tidyr::separate(Var, into = c("Category", "keep_release", "param", "number_weight")) %>%
  #   dplyr::mutate(param="Total") %>%
  #   dplyr::mutate(number_weight=dplyr::case_when(number_weight=="num"~"Number", TRUE~number_weight)) %>%
  #   dplyr::mutate(number_weight=dplyr::case_when(number_weight=="weightavg"~"Weight_avg", TRUE~number_weight)) %>%
  #   dplyr::mutate(state = state1) %>%
  #   replace(is.na(.), 0)
  #
  # #prediction_output_by_period_check02 contains keep, release,discard mortality numbers, and keep weights
  # #based on MRIP average weights across all modes. The number_weight var is set to "Weight"
  # prediction_output_by_period_check02<- prediction_output_by_period_check01 %>%
  #   dplyr::group_by(Category, keep_release, param, number_weight) %>%
  #   dplyr::summarise(Value = sum(Value), .groups = "drop") %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(state = state1, mode="all")
  #
  #
  #
  #prediction_output_by_period1 contains CV and ntrips estimates by mode
  #CV and trip estimates - all states besides NJ
  prediction_output_by_period1 <- prediction_output_by_period2 %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::group_by(mode) %>%
    dplyr::summarise(CV = sum(cv_sum),
                     ntrips = sum(ntrips_alt)) %>%
    dplyr::ungroup()
  
  
  
  #prediction_mode contains CV and ntrips estimates by mode
  prediction_mode<- prediction_output_by_period1 %>%
    tidyr::pivot_longer(!mode, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var, "_", mode)) %>%
    dplyr::select(!mode)
  
  #prediction_all_mode contains CV and ntrips estimates across all modes
  prediction_all_mode<- prediction_output_by_period1  %>%
    dplyr::summarise(CV= sum(CV),
                     ntrips = sum(ntrips)) %>%
    dplyr::mutate(mode="all") %>%
    tidyr::pivot_longer(!mode, names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(Var, "_", mode)) %>%
    dplyr::select(!mode)
  
  #Now we combine all the data into one file
  predictions <- rbind(prediction_mode, prediction_all_mode, l_w_mode, l_w_sum) %>%
    tidyr::separate(Var, into = c("Category", "mode", "keep_release", "param", "number_weight")) %>%
    #run_number = x
    dplyr::filter(!Value == "NA") %>%
    dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="CV"~"Dollars",TRUE ~ number_weight)) %>%
    dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="ntrips"~"Ntrips",TRUE ~ number_weight),
                  season = select_season, run = x)
  #dplyr::filter(!param == "Discmortality") %>%
  #dplyr::select(!param)
  #remove from this data keep numbers, release numbers, discard mortality numbers from the length-weight equation data,
  
  
  #}
  
  
  return(predictions)
}

