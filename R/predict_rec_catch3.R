# 

# pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
#                  "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
#                  "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", 
#                  "purrr", "readr", "here","plyr" , "furrr", "profvis", "future")
# install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
# lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
# conflicts_prefer(dplyr::mutate)
# options(scipen = 100, digits = 3)



predict_rec_catch <- function(state1,
                              calibration_data_table,
                              directed_trips_table = directed_trips,
                              sf_size_data_read,
                              bsb_size_data_read,
                              scup_size_data_read,
                              costs_new_all,
                              sf_catch_data_all,
                              l_w_conversion,
                              x,
                              n_drawz = 50,
                              n_catch_draws = 30,
                              eff_seed=190 ){
  
  
  
  calendar_2024_adjust <- readr::read_csv(file.path(here::here("data-raw/calendar 2024 adjustments.csv")),  show_col_types = FALSE) %>%
    dplyr::filter(state==state1, 
                  draw == x) %>%  
    dplyr::rename(mode1=mode)
  
  MRIP_harvest_weights<- readr::read_csv(file.path(here::here("data-raw/MRIP mean harvest weights.csv")),  show_col_types = FALSE) %>%
    dplyr::filter(state==state1, 
                  draw == x)
  
  
  #These next two files are only for NJ
  if(state1 == "NJ"){
  NJ_bias_CV_trips<- readr::read_csv(file.path(here::here(paste0("data-raw/differences_CV_trips_", state1, ".csv"))),  show_col_types = FALSE) 
  
  NJ_bias_catch<- readr::read_csv(file.path(here::here(paste0("data-raw/differences_total_catch_", state1, ".csv"))),  show_col_types = FALSE) 
  
  NJ_bias_CV_trips1<- NJ_bias_CV_trips %>% dplyr::filter(draw==x)
  
  NJ_bias_catch1<- NJ_bias_catch %>% dplyr::filter(draw==x) %>% 
    dplyr::select(diff_bsb_keep, diff_bsb_rel, diff_scup_keep, diff_scup_rel, diff_sf_keep, diff_sf_rel, mode1, draw)
  
  }
  
  #Average weight in pounds per releasd fish 
  avg_lbs_release_sf<- 1.146519246
  avg_lbs_release_bsb<- 0.595385006
  avg_lbs_release_scup<- 0.591392389
  
  
  
  # directed_trips<- readr::read_csv(file.path(here::here("directed trips and regulations 2022_100 draws.csv")),  show_col_types = FALSE) %>% 
  #   dplyr::filter(state==state1)
  # 
  length_expanded1=list()
  prediction_output_by_period_check = list()
  prediction_output_by_period3 = list()
  discard_stats1=list()
  predictions_test=list()
  
 #for (d in 1:10){
      #setup
      #d <- 1
      #profvis({
     

      print("made it through data read in ")
      
      period_conversion<- directed_trips_table %>% 
        tidyr::separate(period2, into = c("month", "day", "mode1")) %>% 
        dplyr::mutate(period1 = paste0(as.numeric(month), "_", as.numeric(day), "_", mode1), 
                      period2 = paste0(month, "-", day, "-", mode1),
                      day = as.numeric(day), 
                      month = as.numeric(month)) %>% 
        dplyr::select(c(period2, period1, month, day, mode1))
      
      
      set.seed(eff_seed)
      # Input the calibration output which contains the number of choice occasions needed to simulate
      calibration_data <- calibration_data_table %>% tibble::tibble()
        

      
      print("first read in")
      print("pre-rename")
      

      sf_size_data <- sf_size_data_read #%>%  dplyr::rename(fitted_prob = prob_star) %>% dplyr::filter(state == state1)
      bsb_size_data <- bsb_size_data_read  #%>%  dplyr::rename(fitted_prob = prob_star) %>% dplyr::filter(state == state1)
      scup_size_data <- scup_size_data_read  #%>%  dplyr::rename(fitted_prob = prob_star) %>% dplyr::filter(state == state1)
      
      print("out of rename")
      ######################################
      ##   Begin simulating trip outcomes ##
      ######################################
      print("into directed trips")
      # Set up an output file for the separately simulated within-season regulatory periods
      directed_trips_p <- directed_trips_table %>% 
        dplyr::mutate(#n_trips = floor(mean(dtrip_2019)),
          n_trips = floor(dtrip),
          n_draws = n_drawz) #%>% 

      
      print("first kod")
      period_vec <- directed_trips_p %>%
        dplyr::select(period2, n_draws, month, kod, kod_24) %>%
        tidyr::uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))
      
      regs <- tibble::tibble(directed_trips_p) %>%
        dplyr::select(period2,
                      fluke_bag1, fluke_min1, fluke_max1,
                      fluke_bag2, fluke_min2, fluke_max2,
                      bsb_bag,
                      bsb_min,
                      scup_bag,
                      scup_min ) %>% 
        dplyr::left_join(period_conversion, by = c("period2")) %>%
        dplyr::select(!period2)  %>%
        dplyr::rename(period2=period1)
      
      
    #For Kim: Here is where we need to add import 2024 catch-per-trip files. For testing, I used the catch-per trip
    #contained in the costs_new_all files 
      
    sf_catch_data <- sf_catch_data_all[[1]] 
    
    sf_catch_data <- sf_catch_data %>%
      dplyr::group_by(period2) %>%
      dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
      dplyr::mutate(#period = rep(period_vec$period2, each = nsamp),
        catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
        tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
      dplyr::ungroup()
    print("postmutate")
      
    # sf_catch_data <- sf_catch_data %>% 
    #     #dplyr::rename(period2=period1) %>%
    #     dplyr::select(c(catch_draw, period2, tripid, state, tot_sf_catch, tot_bsb_catch, tot_scup_catch, mode1, 
    #                     tot_keep_sf_base, tot_rel_sf_base, tot_keep_bsb_base, tot_rel_bsb_base)) 
    #   
      
    sf_bsb_catch_data <- sf_catch_data
  
  
  # subset trips with zero catch, as no size draws are required
  sf_zero_catch <- dplyr::filter(sf_catch_data, tot_sf_catch == 0) %>% 
    dplyr::select(!mode1)
  
  #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
  sf_catch_check<-base::sum(sf_catch_data$tot_sf_catch)
  bsb_catch_check<-base::sum(sf_catch_data$tot_bsb_catch)
  scup_catch_check<-base::sum(sf_catch_data$tot_scup_catch)
  
  
  
  #Since this code is not broken out by mode, manually enter a catch checks by mode to pipe around
  
  if (sf_catch_check!=0){
    
    
  #remove trips with zero summer flounder catch
  sf_catch_data <- dplyr::filter(sf_catch_data, tot_sf_catch > 0)
  
  ##Private catch at length
  sf_catch_data_pr<- sf_catch_data %>% dplyr::filter(mode1=="pr")
  row_inds <- seq_len(nrow(sf_catch_data_pr))
  
  sf_catch_data_pr<-sf_catch_data_pr %>% 
    dplyr::slice(rep(row_inds,tot_sf_catch))   %>%
    dplyr::mutate(fishid=dplyr::row_number()) 
  
  # generate lengths for each fish
  sf_size_data_pr<-sf_size_data %>% dplyr::filter(mode=="pr")
  
  catch_size_data_pr <- sf_catch_data_pr %>% 
    dplyr::mutate(fitted_length = sample(sf_size_data_pr$length,
                                         nrow(.),
                                         prob = sf_size_data_pr$fitted_prob,
                                         replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
  
  
  
  ##shore catch at length
  sf_catch_data_sh<- sf_catch_data %>% dplyr::filter(mode1=="sh")
  row_inds <- seq_len(nrow(sf_catch_data_sh))
  
  sf_catch_data_sh<-sf_catch_data_sh %>% 
    dplyr::slice(rep(row_inds,tot_sf_catch))   %>%
    dplyr::mutate(fishid=dplyr::row_number()) 
  
  # generate lengths for each fish
  sf_size_data_sh<-sf_size_data %>% dplyr::filter(mode=="sh")
  
  catch_size_data_sh <- sf_catch_data_sh %>% 
    dplyr::mutate(fitted_length = sample(sf_size_data_sh$length,
                                         nrow(.),
                                         prob = sf_size_data_sh$fitted_prob,
                                         replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
  
  ##fh catch at length
  sf_catch_data_fh<- sf_catch_data %>% dplyr::filter(mode1=="fh")
  row_inds <- seq_len(nrow(sf_catch_data_fh))
  
  sf_catch_data_fh<-sf_catch_data_fh %>% 
    dplyr::slice(rep(row_inds,tot_sf_catch))   %>%
    dplyr::mutate(fishid=dplyr::row_number()) 
  
  # generate lengths for each fish
  sf_size_data_fh<-sf_size_data %>% dplyr::filter(mode=="fh")
  
  catch_size_data_fh <- sf_catch_data_fh %>% 
    dplyr::mutate(fitted_length = sample(sf_size_data_fh$length,
                                         nrow(.),
                                         prob = sf_size_data_fh$fitted_prob,
                                         replace = TRUE))  
  
  catch_size_data <- rbind(catch_size_data_fh, catch_size_data_pr, catch_size_data_sh) 
  
  
  # Impose regulations, calculate keep and release per trip
  # For summer flounder, retain keep- and release-at-length
  ####### Start Here #################
  
  ############# Length #####################################
  catch_size_data <- catch_size_data %>%
    dplyr::left_join(regs, by = "period2") %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min1 & fitted_length<=fluke_max1,1,0)) %>%
    dplyr::group_by(tripid, period2, catch_draw)   %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>% #,
    # keep = dplyr::case_when(
    #   fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    #   TRUE ~ 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        fluke_bag1 > 0 ~ ifelse(csum_keep<=fluke_bag1 & posskeep==1,1,0),
        TRUE ~ 0))  %>%
    dplyr::mutate(posskeep2 = ifelse(fitted_length>=fluke_min2 & fitted_length<fluke_max2 & keep_adj!=1,1,0)) %>%
    dplyr::group_by(tripid, period2, catch_draw) %>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    dplyr::mutate(csum_keep2 = cumsum(posskeep2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj2 = dplyr::case_when(
        fluke_bag2 > 0 ~ ifelse(csum_keep2<=fluke_bag2 & posskeep2==1,1,0),
        TRUE ~ 0))
  
  


  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep_tot = keep_adj+keep_adj2,
                  release = ifelse(keep_adj==0 & keep_adj2==0,1,0))

  
  ###### ANDREWS CODE #@######
  catch_size_data<- catch_size_data %>% 
    dplyr::select(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw, mode1.x,
                  keep_sf, release_sf, keep_bsb, release_bsb)  %>% 
    dplyr::rename(keep = keep_tot, 
                  mode1=mode1.x)
  
  
  new_size_data <- catch_size_data %>%
    dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
    dplyr::summarize(keep = sum(keep),
                     release = sum(release), .groups = "drop") %>% 
    dplyr::ungroup()
  
  
  
  
  summed_catch_data <- new_size_data %>%
    dplyr::group_by(period2, catch_draw, tripid) %>%
    dplyr::summarize(tot_keep_sf_new = sum(keep),
                     tot_rel_sf_new = sum(release),
                     .groups = "drop") %>% 
    dplyr::ungroup()
  
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(names_from = fitted_length, #_length,
                       names_glue = "keep_sf_{fitted_length}",
                       names_sort = TRUE,
                       values_from = keep,
                       values_fill = 0)
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(names_from = fitted_length, #_length,
                       names_glue = "release_sf_{fitted_length}",
                       names_sort = TRUE,
                       values_from = release,
                       values_fill = 0)
  
  keep_release_sf <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
  
  trip_data <- summed_catch_data 
  
  sf_zero_catch<-sf_zero_catch %>% 
    dplyr::select(tripid, catch_draw, period2) %>% 
    dplyr::mutate(tot_keep_sf_new=0, 
                  tot_rel_sf_new=0)
  
  trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::mutate(state = state1) %>%
    dplyr::select(c("period2", "catch_draw","tripid","state",
                    "tot_keep_sf_new","tot_rel_sf_new"))
  
  
  trip_data<- trip_data %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid, "_",  state))
  trip_data<-data.table::as.data.table(trip_data)
  data.table::setkey(trip_data, "domain2")
  }
  
  
  if (sf_catch_check==0){
    trip_data$tot_sf_catch_new<-0
    trip_data$tot_keep_sf_new<-0
    trip_data$tot_rel_sf_new<-0
  }
  
  
  #######Black Sea Bass
  
  if (bsb_catch_check!=0){
    
    
    #Testing with Scup code
    bsb_zero_catch <- dplyr::filter(sf_bsb_catch_data, tot_bsb_catch == 0) %>%
      dplyr::select(-c("tot_sf_catch", "tot_scup_catch", "mode1"))
    
    #remove trips with zero summer flounder catch
    bsb_catch_data <- dplyr::filter(sf_bsb_catch_data, tot_bsb_catch > 0)
    
    ##Private catch at length
    bsb_catch_data_pr<- bsb_catch_data %>% dplyr::filter(mode1=="pr")
    row_inds <- seq_len(nrow(bsb_catch_data_pr))
    
    bsb_catch_data_pr<-bsb_catch_data_pr %>% 
      dplyr::slice(rep(row_inds,tot_bsb_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number()) 
    
    # generate lengths for each fish
    bsb_size_data_pr<-bsb_size_data %>% dplyr::filter(mode=="pr")
    
    catch_size_data_pr <- bsb_catch_data_pr %>% 
      dplyr::mutate(fitted_length = sample(bsb_size_data_pr$length,
                                           nrow(.),
                                           prob = bsb_size_data_pr$fitted_prob,
                                           replace = TRUE)) 
    
    
    ##shore catch at length
    bsb_catch_data_sh<- bsb_catch_data %>% dplyr::filter(mode1=="sh")
    row_inds <- seq_len(nrow(bsb_catch_data_sh))
    
    bsb_catch_data_sh<-bsb_catch_data_sh %>% 
      dplyr::slice(rep(row_inds,tot_bsb_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number()) 
    
    # generate lengths for each fish
    bsb_size_data_sh<-bsb_size_data %>% dplyr::filter(mode=="sh")
    
    catch_size_data_sh <- bsb_catch_data_sh %>% 
      dplyr::mutate(fitted_length = sample(bsb_size_data_sh$length,
                                           nrow(.),
                                           prob = bsb_size_data_sh$fitted_prob,
                                           replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
    
    ##fh catch at length
    bsb_catch_data_fh<- bsb_catch_data %>% dplyr::filter(mode1=="fh")
    row_inds <- seq_len(nrow(bsb_catch_data_fh))
    
    bsb_catch_data_fh<-bsb_catch_data_fh %>% 
      dplyr::slice(rep(row_inds,tot_bsb_catch))   %>%
      dplyr::mutate(fishid=dplyr::row_number()) 
    
    # generate lengths for each fish
    bsb_size_data_fh<-bsb_size_data %>% dplyr::filter(mode=="fh")
    
    catch_size_data_fh <- bsb_catch_data_fh %>% 
      dplyr::mutate(fitted_length = sample(bsb_size_data_fh$length,
                                           nrow(.),
                                           prob = bsb_size_data_fh$fitted_prob,
                                           replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw) 
    
    catch_size_data <- rbind(catch_size_data_fh, catch_size_data_pr, catch_size_data_sh) 
    
    
    catch_size_data <- catch_size_data %>%
      dplyr::left_join(regs, by = "period2") %>%
      dplyr::mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>%
      dplyr::group_by(tripid, period2, catch_draw) %>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj = dplyr::case_when(
          bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
          TRUE ~ 0))
    
    #catch_size_data[is.na(catch_size_data)] <- 0
    catch_size_data <- catch_size_data %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    catch_size_data <- catch_size_data %>%
      dplyr::mutate(keep_tot = keep_adj, 
                    release = ifelse(keep_adj==0,1,0))
    
    catch_size_data<- catch_size_data %>% 
      dplyr::select(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw, mode1.x,
                    keep_sf, release_sf, keep_bsb, release_bsb)  %>% 
      dplyr::rename(keep = keep_tot, 
                    mode1=mode1.x)
    
    new_size_data <- catch_size_data %>%
      dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      dplyr::summarize(keep = sum(keep),
                       release = sum(release), .groups = "drop") %>% 
      dplyr::ungroup()
    
    
    summed_catch_data <- new_size_data %>%
      dplyr::group_by(period2, catch_draw, tripid) %>%
      dplyr::summarize(tot_keep_bsb_new = sum(keep),
                       tot_rel_bsb_new = sum(release),
                       .groups = "drop") %>% 
      dplyr::ungroup()
    
    keep_size_data <- new_size_data %>%
      dplyr::select(-release) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "keep_bsb_{fitted_length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    release_size_data <- new_size_data %>%
      dplyr::select(-keep) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "release_bsb_{fitted_length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    keep_release_bsb <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
    
    trip_data_bsb <- summed_catch_data 
    
    bsb_zero_catch<-bsb_zero_catch %>% 
      dplyr::select(tripid, catch_draw, period2) %>% 
      dplyr::mutate(tot_keep_bsb_new=0, 
                    tot_rel_bsb_new=0)
    
    trip_data_bsb <- dplyr::bind_rows(trip_data_bsb, bsb_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::mutate(state = state1) %>%
      dplyr::select(c("period2", "catch_draw","tripid","state",
                      "tot_keep_bsb_new","tot_rel_bsb_new"))
    
    
    trip_data_bsb<- trip_data_bsb %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid, "_",  state))
    trip_data_bsb<-data.table::as.data.table(trip_data_bsb)
    data.table::setkey(trip_data_bsb, "domain2")
    
    trip_data<-trip_data[trip_data_bsb, on = "domain2"]
    trip_data<-trip_data %>% dplyr::select(-i.tripid, -i.catch_draw, -i.period2, -i.state)
  }
  
  if (bsb_catch_check==0){
    trip_data$tot_bsb_catch_new<-0
    trip_data$tot_keep_bsb_new<-0
    trip_data$tot_rel_bsb_new<-0
  }
  
  
  
  
  ##############Scup
  
  
  if (state1 %in% c("DE", "MD", "VA", "NC")){
    trip_data$tot_scup_catch_new<-0
    trip_data$tot_keep_scup_new<-0
    trip_data$tot_rel_scup_new<-0
    
  }
  
  if (state1 %in% c("MA", "RI", "CT", "NY", "NJ")){
    
    
    if (scup_catch_check>0){
      # subset trips with zero catch, as no size draws are required
      scup_zero_catch <- dplyr::filter(sf_bsb_catch_data, tot_scup_catch == 0) %>%
        dplyr::select(-c("tot_sf_catch", "tot_bsb_catch", "mode1"))
      
      #remove trips with zero summer flounder catch
      scup_catch_data <- dplyr::filter(sf_bsb_catch_data, tot_scup_catch > 0)
      
      ##Private catch at length
      scup_catch_data_pr<- scup_catch_data %>% dplyr::filter(mode1=="pr")
      row_inds <- seq_len(nrow(scup_catch_data_pr))
      
      scup_catch_data_pr<-scup_catch_data_pr %>% 
        dplyr::slice(rep(row_inds,tot_scup_catch))   %>%
        dplyr::mutate(fishid=dplyr::row_number()) 
      
      # generate lengths for each fish
      scup_size_data_pr<-scup_size_data %>% dplyr::filter(mode=="pr")
      
      catch_size_data_pr <- scup_catch_data_pr %>% 
        dplyr::mutate(fitted_length = sample(scup_size_data_pr$length,
                                             nrow(.),
                                             prob = scup_size_data_pr$fitted_prob,
                                             replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
      
      
      
      ##shore catch at length
      scup_catch_data_sh<- scup_catch_data %>% dplyr::filter(mode1=="sh")
      row_inds <- seq_len(nrow(scup_catch_data_sh))
      
      scup_catch_data_sh<-scup_catch_data_sh %>% 
        dplyr::slice(rep(row_inds,tot_scup_catch))   %>%
        dplyr::mutate(fishid=dplyr::row_number()) 
      
      # generate lengths for each fish
      scup_size_data_sh<-scup_size_data %>% dplyr::filter(mode=="sh")
      
      catch_size_data_sh <- scup_catch_data_sh %>% 
        dplyr::mutate(fitted_length = sample(scup_size_data_sh$length,
                                             nrow(.),
                                             prob = scup_size_data_sh$fitted_prob,
                                             replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
      
      ##fh catch at length
      scup_catch_data_fh<- scup_catch_data %>% dplyr::filter(mode1=="fh")
      row_inds <- seq_len(nrow(scup_catch_data_fh))
      
      scup_catch_data_fh<-scup_catch_data_fh %>% 
        dplyr::slice(rep(row_inds,tot_scup_catch))   %>%
        dplyr::mutate(fishid=dplyr::row_number()) 
      
      # generate lengths for each fish
      scup_size_data_fh<-scup_size_data %>% dplyr::filter(mode=="fh")
      
      catch_size_data_fh <- scup_catch_data_fh %>% 
        dplyr::mutate(fitted_length = sample(scup_size_data_fh$length,
                                             nrow(.),
                                             prob = scup_size_data_fh$fitted_prob,
                                             replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw) 
      
      catch_size_data <- rbind(catch_size_data_fh, catch_size_data_pr, catch_size_data_sh) 
      
      
      
      
      catch_size_data <- catch_size_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=scup_min ,1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw) %>%
        # keep = case_when(
        # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
        # TRUE ~ 0),
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj = dplyr::case_when(
            scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
            TRUE ~ 0))
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(keep_tot = keep_adj, 
                      release = ifelse(keep_adj==0,1,0))
      
      catch_size_data<- catch_size_data %>% 
        dplyr::select(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw, mode1.x,
                      keep_sf, release_sf, keep_bsb, release_bsb)  %>% 
        dplyr::rename(keep = keep_tot, 
                      mode1=mode1.x)
      
      
      new_size_data <- catch_size_data %>%
        dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
        dplyr::summarize(keep = sum(keep),
                         release = sum(release), .groups = "drop") %>% 
        dplyr::ungroup()
      
      
      summed_catch_data <- new_size_data %>%
        dplyr::group_by(period2, catch_draw, tripid) %>%
        dplyr::summarize(tot_keep_scup_new = sum(keep),
                         tot_rel_scup_new = sum(release),
                         .groups = "drop") %>% 
        dplyr::ungroup()
      
      keep_size_data <- new_size_data %>%
        dplyr::select(-release) %>%
        tidyr::pivot_wider(names_from = fitted_length, #_length,
                           names_glue = "keep_scup_{fitted_length}",
                           names_sort = TRUE,
                           values_from = keep,
                           values_fill = 0)
      
      release_size_data <- new_size_data %>%
        dplyr::select(-keep) %>%
        tidyr::pivot_wider(names_from = fitted_length, #_length,
                           names_glue = "release_scup_{fitted_length}",
                           names_sort = TRUE,
                           values_from = release,
                           values_fill = 0)
      
      
      keep_release_scup <- keep_size_data %>%
        dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
      
      
      trip_data_scup <- summed_catch_data 
      
      scup_zero_catch<-scup_zero_catch %>% 
        dplyr::select(tripid, catch_draw, period2) %>% 
        dplyr::mutate(tot_keep_scup_new=0, 
                      tot_rel_scup_new=0)
      
      trip_data_scup <- dplyr::bind_rows(trip_data_scup, scup_zero_catch) %>%
        #arrange(period, catch_draw, tripid) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::mutate(state = state1) %>%
        dplyr::select(c("period2", "catch_draw","tripid","state",
                        "tot_keep_scup_new","tot_rel_scup_new"))
      
      
      trip_data_scup<- trip_data_scup %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid, "_",  state))
      trip_data_scup<-data.table::as.data.table(trip_data_scup)
      data.table::setkey(trip_data_scup, "domain2")
      
      trip_data<-trip_data[trip_data_scup, on = "domain2"]
      trip_data<-trip_data %>% dplyr::select(-i.tripid, -i.catch_draw, -i.period2, -i.state)%>% 
        dplyr::mutate(tot_scup_catch_new = tot_keep_scup_new + tot_rel_scup_new, 
                      tot_bsb_catch_new = tot_keep_bsb_new + tot_rel_bsb_new, 
                      tot_sf_catch_new = tot_keep_sf_new + tot_rel_sf_new)
      
      
      
      
    }
    if (scup_catch_check==0){
      trip_data$tot_scup_catch_new<-0
      trip_data$tot_keep_scup_new<-0
      trip_data$tot_rel_scup_new<-0
      
      trip_data<-trip_data %>% 
        dplyr::mutate(tot_scup_catch_new = tot_keep_scup_new + tot_rel_scup_new, 
                      tot_bsb_catch_new = tot_keep_bsb_new + tot_rel_bsb_new, 
                      tot_sf_catch_new = tot_keep_sf_new + tot_rel_sf_new)
    }
    
  }
  
  
  if (state1 %in% c("DE", "MD", "VA", "NC")){
    length_data <- keep_release_sf %>%
      dplyr::full_join(keep_release_bsb, by = c("period2","tripid", "catch_draw")) 
  }
  
  if (state1 %in% c("MA", "RI", "CT", "NY", "NJ")){
    length_data <- keep_release_sf %>%
      dplyr::full_join(keep_release_bsb, by = c("period2","tripid", "catch_draw")) %>%
      dplyr::full_join(keep_release_scup, by = c("period2","tripid", "catch_draw"))
  }
  
  # length_data <- keep_release_sf %>%
  #   dplyr::full_join(keep_release_bsb, by = c("period2","tripid", "catch_draw")) %>%
  #   dplyr::full_join(keep_release_scup, by = c("period2","tripid", "catch_draw"))
  length_data[is.na(length_data)] <- 0
  
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw)
  
  if (state1 %in% c("MA", "RI", "CT", "NY", "NJ")){
  zero_catch_check <- sf_zero_catch %>%  dplyr::left_join(bsb_zero_catch) %>% dplyr::left_join(scup_zero_catch) %>% 
    dplyr::filter(tot_keep_sf_new==0 & tot_rel_sf_new==0 &
                    tot_keep_bsb_new==0 & tot_rel_bsb_new==0 &
                    tot_keep_scup_new==0 & tot_rel_scup_new==0) %>% 
    dplyr::select("period2","tripid", "catch_draw")
   }

  if (state1 %in% c("DE", "MD", "VA", "NC")){
    zero_catch_check <- sf_zero_catch %>%  dplyr::left_join(bsb_zero_catch) %>% 
      dplyr::filter(tot_keep_sf_new==0 & tot_rel_sf_new==0 &
                      tot_keep_bsb_new==0 & tot_rel_bsb_new==0) %>% 
      dplyr::select("period2","tripid", "catch_draw")
  }
  
  # zero_catch_check <- sf_zero_catch %>%  dplyr::left_join(bsb_zero_catch) %>% dplyr::left_join(scup_zero_catch) %>% 
  #   dplyr::filter(tot_keep_sf_new==0 & tot_rel_sf_new==0 &
  #                 tot_keep_bsb_new==0 & tot_rel_bsb_new==0 &
  #                 tot_keep_scup_new==0 & tot_rel_scup_new==0) %>% 
  #   dplyr::select("period2","tripid", "catch_draw")
  
  length_data<- plyr::rbind.fill(length_data, zero_catch_check)
   
  length_data<-length_data %>% dplyr::arrange(period2,tripid, catch_draw)
  
    length_data[is.na(length_data)] <- 0
  
  
  
  costs_new_all2 <- data.frame(costs_new_all) %>% #tibble() %>%
    dplyr::mutate_at(.vars = "period2", .funs = gsub, 
                     pattern = "-", replace = "_") %>% 
    dplyr::rename(beta_sqrt_sf_keep_base=beta_sqrt_sf_keep,
                  beta_sqrt_sf_release_base=beta_sqrt_sf_release,
                  beta_sqrt_bsb_keep_base=beta_sqrt_bsb_keep,
                  beta_sqrt_bsb_release_base=beta_sqrt_bsb_release,
                  beta_sqrt_sf_bsb_keep_base=beta_sqrt_sf_bsb_keep,
                  beta_sqrt_scup_catch_base=beta_sqrt_scup_catch,
                  beta_opt_out_base=beta_opt_out, 
                  beta_opt_out_avidity_base=beta_opt_out_avidity, 
                  beta_opt_out_age_base=beta_opt_out_age) 
  
  
  
  trip_data[is.na(trip_data)] <- 0
  
  # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
  trip_data <- trip_data %>%
    dplyr::left_join(costs_new_all2, by = c("period2","catch_draw","tripid", "state")) #%>%  
  
  
  trip_data[is.na(trip_data)] <- 0
  
  trip_data <- trip_data %>%
    dplyr::mutate(tot_cat_bsb_base = tot_keep_bsb_base+tot_rel_bsb_base, 
                  tot_cat_sf_base = tot_keep_sf_base+tot_rel_sf_base)
  
  
  
  #  utility (prediction year)
  trip_data <-trip_data %>%
    dplyr::mutate(
      vA = beta_sqrt_sf_keep_base*sqrt(tot_keep_sf_new) +
        beta_sqrt_sf_release_base*sqrt(tot_rel_sf_new) +
        beta_sqrt_bsb_keep_base*sqrt(tot_keep_bsb_new) +
        beta_sqrt_bsb_release_base*sqrt(tot_rel_bsb_new) +
        beta_sqrt_scup_catch_base*sqrt(tot_scup_catch_new) +
        beta_sqrt_sf_bsb_keep_base*(sqrt(tot_keep_sf_new)*sqrt(tot_keep_bsb_new)) +
        beta_cost*cost,
      
   #  utility (base year)
      v0 = beta_sqrt_sf_keep_base*sqrt(tot_keep_sf_base) +
        beta_sqrt_sf_release_base*sqrt(tot_rel_sf_base) +
        beta_sqrt_bsb_keep_base*sqrt(tot_keep_bsb_base) +
        beta_sqrt_bsb_release_base*sqrt(tot_rel_bsb_base) +
        beta_sqrt_scup_catch_base*sqrt(tot_cat_scup_base)  +
        beta_sqrt_sf_bsb_keep_base*(sqrt(tot_keep_sf_base)*sqrt(tot_keep_bsb_base)) +
        beta_cost*cost)
  
  
  trip_data <- trip_data %>%
    dplyr::mutate(period = as.numeric(as.factor(period2)))
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
  
  
  
  period_conversion<-period_conversion %>% dplyr::select(-period2)  %>% 
    dplyr::rename(period2=period1)
  
  trip_data <- trip_data %>%
    data.table::as.data.table() %>% 
    dplyr::left_join(period_conversion, by=c("period2"))
  
  #Checks
  # mean(trip_data$tot_cat_bsb_base)
  # mean(trip_data$tot_bsb_catch_new)
  # 
  # mean(trip_data$tot_sf_catch_new)
  # mean(trip_data$tot_cat_sf_base)
  # 
  # mean(trip_data$tot_scup_catch_new)
  # mean(trip_data$tot_cat_scup_base)
  # 
  # mean(trip_data$tot_keep_bsb_base)
  # mean(trip_data$tot_keep_bsb_new)
  # # 
  # # 
  # mean(trip_data[mode1=="fh"]$tot_keep_bsb_base)
  # mean(trip_data[mode1=="fh"]$tot_keep_bsb_new)
  # 
  # mean(trip_data[mode1=="pr"]$tot_keep_bsb_base)
  # mean(trip_data[mode1=="pr"]$tot_keep_bsb_new)
  # 
  # mean(trip_data[mode1=="sh"]$tot_keep_bsb_base)
  # mean(trip_data[mode1=="sh"]$tot_keep_bsb_new)
  # 
  # sum(trip_data[mode1=="fh"]$tot_keep_bsb_base)
  # sum(trip_data[mode1=="fh"]$tot_keep_bsb_new)
  # 
  # sum(trip_data[mode1=="pr"]$tot_keep_bsb_base)
  # sum(trip_data[mode1=="pr"]$tot_keep_bsb_new)
  # 
  # sum(trip_data[mode1=="sh"]$tot_keep_bsb_base)
  # sum(trip_data[mode1=="sh"]$tot_keep_bsb_new)
  # # 
  # mean(trip_data[mode1=="fh"]$tot_keep_sf_base)
  # mean(trip_data[mode1=="fh"]$tot_keep_sf_new)
  # 
  # mean(trip_data[mode1=="pr"]$tot_keep_sf_base)
  # mean(trip_data[mode1=="pr"]$tot_keep_sf_new)
  # 
  # mean(trip_data[mode1=="sh"]$tot_keep_sf_base)
  # mean(trip_data[mode1=="sh"]$tot_keep_sf_new)
  # 
  # sum(trip_data[mode1=="fh"]$tot_keep_sf_base)
  # sum(trip_data[mode1=="fh"]$tot_keep_sf_new)
  # 
  # sum(trip_data[mode1=="pr"]$tot_keep_sf_base)
  # sum(trip_data[mode1=="pr"]$tot_keep_sf_new)
  # 
  # sum(trip_data[mode1=="sh"]$tot_keep_sf_base)
  # sum(trip_data[mode1=="sh"]$tot_keep_sf_new)
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
  # sf_keep_base_fh <- trip_data$tot_keep_sf_base[trip_data$mode1=="fh"]
  # sf_keep_new_fh <- trip_data$tot_keep_sf_new[trip_data$mode1=="fh"]
  # vioplot(sf_keep_base_fh, sf_keep_new_fh, names=c("base", "new"))
  # 
  # sf_keep_base_fh <- trip_data$tot_keep_sf_base[trip_data$mode1=="sh"]
  # sf_keep_new_fh <- trip_data$tot_keep_sf_new[trip_data$mode1=="sh"]
  # vioplot(sf_keep_base_fh, sf_keep_new_fh, names=c("base", "new"))
  # 
  # sf_keep_base_fh <- trip_data$tot_keep_sf_base[trip_data$mode1=="pr"]
  # sf_keep_new_fh <- trip_data$tot_keep_sf_new[trip_data$mode1=="pr"]
  # vioplot(sf_keep_base_fh, sf_keep_new_fh, names=c("base", "new"))
  # 
  # sf_keep_base_fh <- trip_data$tot_rel_sf_base[trip_data$mode1=="fh"]
  # sf_keep_new_fh <- trip_data$tot_rel_sf_new[trip_data$mode1=="fh"]
  # vioplot(sf_keep_base_fh, sf_keep_new_fh, names=c("base", "new"))
  # 
  # sf_keep_base_fh <- trip_data$tot_rel_sf_base[trip_data$mode1=="pr"]
  # sf_keep_new_fh <- trip_data$tot_rel_sf_new[trip_data$mode1=="pr"]
  # vioplot(sf_keep_base_fh, sf_keep_new_fh, names=c("base", "new"))
  # 
  # sf_keep_base_fh <- trip_data$tot_rel_sf_base[trip_data$mode1=="sh"]
  # sf_keep_new_fh <- trip_data$tot_rel_sf_new[trip_data$mode1=="sh"]
  # vioplot(sf_keep_base_fh, sf_keep_new_fh, names=c("base", "new"))
  # 
  # 
  # bsb_keep_base_fh <- trip_data$tot_keep_bsb_base[trip_data$mode1=="pr"]
  # bsb_keep_new_fh <- trip_data$tot_keep_bsb_new[trip_data$mode1=="pr"]
  # vioplot(bsb_keep_base_fh, bsb_keep_new_fh, names=c("base", "new"))
  # 
  # bsb_keep_base_fh <- trip_data$tot_keep_bsb_base[trip_data$mode1=="fh"]
  # bsb_keep_new_fh <- trip_data$tot_keep_bsb_new[trip_data$mode1=="fh"]
  # vioplot(bsb_keep_base_fh, bsb_keep_new_fh, names=c("base", "new"))
  # 
  # bsb_keep_base_fh <- trip_data$tot_keep_bsb_base[trip_data$mode1=="sh"]
  # bsb_keep_new_fh <- trip_data$tot_keep_bsb_new[trip_data$mode1=="sh"]
  # vioplot(bsb_keep_base_fh, bsb_keep_new_fh, names=c("base", "new"))
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
    .[, vA_optout := beta_opt_out_base*opt_out+beta_opt_out_age_base*age + beta_opt_out_avidity_base*days_fished] %>%
    .[, v0_optout := beta_opt_out_base*opt_out+beta_opt_out_age_base*age + beta_opt_out_avidity_base*days_fished] %>%
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
  

  
  # mean(mean_trip_data$change_CS)
  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table()
  #checks
  # mean(mean_trip_data$vA)
  # mean(mean_trip_data$v0)
  # #
  # mean(mean_trip_data$tot_cat_bsb_base)
  # mean(mean_trip_data$tot_bsb_catch_new)
  # 
  # mean(mean_trip_data$tot_sf_catch_new)
  # mean(mean_trip_data$tot_cat_sf_base)
  # 
  # mean(mean_trip_data$tot_scup_catch_new)
  # mean(mean_trip_data$tot_cat_scup_base)
  # #
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_bsb_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_bsb_new)
  # 
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_bsb_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_bsb_new)
  # 
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_bsb_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_bsb_new)
  # #
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_sf_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_sf_new)
  # 
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_sf_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_sf_new)
  # 
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_sf_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_sf_new)
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
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c( "period","tripid", "period2", "mode1", "month", "day", "state")]
  #all_vars
  mean_trip_data<-mean_trip_data %>% data.table::as.data.table() %>%
    dplyr::mutate(month = as.numeric(month), 
                  day = as.numeric(day)) #%>% 

  mean_trip_data <- mean_trip_data %>% 
    .[,lapply(.SD, base::mean), by = c("period","tripid", "period2", "mode1", "month", "day", "state"), .SDcols = all_vars]
  

  # mean(mean_trip_data$change_CS)
  # mean(mean_trip_data[mode1=="pr"]$vA)
  # mean(mean_trip_data[mode1=="pr"]$v0)
  # mean(mean_trip_data[mode1=="sh"]$change_CS)
  # mean(mean_trip_data[mode1=="fh"]$change_CS2)
  # mean(mean_trip_data[mode1=="pr"]$change_CS2)
  # mean(mean_trip_data[mode1=="sh"]$change_CS2)
  
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_bsb_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_bsb_new)
  # 
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_bsb_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_bsb_new)
  # 
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_bsb_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_bsb_new)
  # 
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_sf_base)
  # mean(mean_trip_data[mode1=="fh"]$tot_keep_sf_new)
  # 
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_sf_base)
  # mean(mean_trip_data[mode1=="pr"]$tot_keep_sf_new)
  # 
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_sf_base)
  # mean(mean_trip_data[mode1=="sh"]$tot_keep_sf_new)
  # 
  # max(mean_trip_data[mode1=="sh"]$tot_keep_sf_base)
  # max(mean_trip_data[mode1=="sh"]$tot_keep_sf_new)
  # 
  # 
  # 
  
  # mean(mean_trip_data[mode1=="fh"]$tot_cat_sf_new)
  # mean(mean_trip_data[mode1=="fh"]$tot_cat_sf_base)
  # 
  # mean(mean_trip_data[mode1=="pr"]$tot_cat_sf_new)
  # mean(mean_trip_data[mode1=="pr"]$tot_cat_sf_base)
  # 
  # mean(mean_trip_data[mode1=="sh"]$tot_cat_sf_new)
  # mean(mean_trip_data[mode1=="sh"]$tot_cat_sf_base)
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
  mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost,
                                                            catch_draw, expon_v0 ,v0_col_sum, expon_vA,
                                                            opt_out, v0, v0_optout, vA, vA_optout, vA_col_sum))
  
  
  mean_trip_data<-mean_trip_data %>% 
    dplyr::mutate(tot_bsb_catch_new=tot_keep_bsb_new+tot_rel_bsb_new,
                  tot_scup_catch_new=tot_keep_scup_new+tot_rel_scup_new,
                  tot_sf_catch_new=tot_keep_sf_new+tot_rel_sf_new)
  
  # Multiply the average trip probability by each of the catch variables to get probability-weighted catch
  
  
  list_names <- c("tot_keep_sf_new","tot_rel_sf_new", "tot_sf_catch_new", "tot_keep_bsb_new", "tot_rel_bsb_new" , 
                  "tot_bsb_catch_new" , "tot_keep_scup_new" , "tot_rel_scup_new","tot_scup_catch_new" )
  
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
  
  list_names <- c("tot_keep_sf_base","tot_rel_sf_base", "tot_keep_bsb_base", "tot_rel_bsb_base" , 
                  "tot_cat_scup_base")
  
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
    dplyr::mutate_at(.vars = "period2", .funs = gsub, 
                     pattern = "-", replace = "_") %>% 
    dplyr::left_join(mean_trip_data, by = c("period2")) %>% 
    #dplyr::select()
    dplyr::mutate(ndraws = c(50),
                  period = as.character(period2)) %>% 
    
  ## Here we adjust the number of choice occasions to simulate to account for 
  ## different kind-of-days within a month in 2024 compared to 2022
    dplyr::left_join(calendar_2024_adjust, by=c("state", "month", "mode1")) %>% 
  
    
  #multiply the number of choice occasions in the baseline year by the expansion factor
  #For Kim: When we run the projections for 2024, change the "n_choice_occasions*1" below to "n_choice_occasions*expansion_factor" - I already did this 
    dplyr::mutate(n_choice_occasions = n_choice_occasions*expansion_factor) %>%
    dplyr::mutate(expand = n_choice_occasions/ndraws) %>%
    dplyr::arrange(period)
  
  
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

  length_expanded<- period_conversion %>%   
    dplyr::select(period2, mode1, month) %>% 
    dplyr::right_join(length_expand, by = c("period2")) %>% 
    data.table::data.table() 
  
  all_vars<-c()
  all_vars <- names(length_expanded)[!names(length_expanded) %in% c("period2", "mode1", "tripid", "expand", "month")]
  
  length_expanded <- length_expanded %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, base::sum), by = c("mode1", "month"), .SDcols = all_vars]
  
  
  length_expanded_check<- length_expanded %>%
    tidyr::pivot_longer(cols = !month & !mode1,  names_to = "Var", values_to = "Number_at_Length")

  
  
  ##For Kim: add other states to L_W_Conversion - I have done this 

  #This code translates numbers to weights using the l-w equation. The number_weight var is set to "Weight"
  #Later on we drop the keep and release numbers computed here. 
   length_weight<- length_expanded %>%
     tidyr::pivot_longer(cols = !month & !mode1,  names_to = "Var", values_to = "Number_at_Length") %>%
     tidyr::separate(Var, into = c("keep_release", "Species", "length"), sep = "_") %>%
     dplyr::rename(Month=month, Mode=mode1) %>% 
     dplyr::mutate(State = state1) %>%
     dplyr::left_join(l_w_conversion, by = c("State", "Month", "Species")) %>%
     dplyr::mutate(length_in = as.numeric(length),
                   ln_a = as.numeric(ln_a),
                   ln_b = as.numeric(ln_b),
                   a = as.numeric(a),
                   b = as.numeric(b),
                  length_cm = length_in*2.54)  %>%  #Convert to cm
     dplyr::mutate(ln_a = ifelse(is.na(ln_a), 0, ln_a)) %>% 
     dplyr::mutate(a = ifelse(is.na(a), 0, a)) %>%
     dplyr::mutate(b = ifelse(is.na(b), 0, b)) %>%
     dplyr::mutate(ln_b = ifelse(is.na(ln_b), 0, ln_b))  %>%   
     dplyr::mutate(weight = dplyr::case_when(Species == "scup" ~ exp(ln_a + b*log(length_cm)))) %>% 
     dplyr::mutate(weight = dplyr::case_when(Species == "sf" ~ a*length_cm^b, TRUE ~ weight))  %>% 
     dplyr::mutate(weight = dplyr::case_when(Species == "bsb" ~ a*length_cm^b, TRUE ~ weight),
                  weight = weight*2.20462262185, #convert to lbs
                  Total_weight = Number_at_Length * weight,
                  Discmortality_Total_weight = dplyr::case_when(keep_release == "release" & Species == "sf" ~ (.1 * Number_at_Length * weight)),
                  Discmortality_Total_weight = dplyr::case_when(keep_release == "release" & Species == "scup" ~ (.15 * Number_at_Length * weight), TRUE ~ Discmortality_Total_weight),
                  Discmortality_Total_weight = dplyr::case_when(keep_release == "release" & Species == "bsb" ~ (.15 * Number_at_Length * weight), TRUE ~ Discmortality_Total_weight),
                  Discmortality_Total_Number = dplyr::case_when(keep_release == "release" & Species == "sf" ~ (.1 * Number_at_Length)),
                  Discmortality_Total_Number = dplyr::case_when(keep_release == "release" & Species == "scup" ~ (.15 * Number_at_Length), TRUE ~ Discmortality_Total_Number),
                  Discmortality_Total_Number = dplyr::case_when(keep_release == "release" & Species == "bsb" ~ (.15 * Number_at_Length), TRUE ~ Discmortality_Total_Number))  %>%
    dplyr::group_by(State, Species, Mode, keep_release) %>%
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
    dplyr::group_by(State, Species, keep_release) %>%
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
    dplyr::mutate(state=state1)   %>%
    dplyr::select(c(period2,  n_choice_occasions, tripid, expand, change_CS, CS_base, CS_alt, state, probA, prob0, 
                    tot_keep_sf_new, tot_rel_sf_new, 
                    tot_keep_bsb_new, tot_rel_bsb_new,
                    tot_keep_scup_new, tot_rel_scup_new,
                    tot_scup_catch_new, 
                    tot_keep_sf_base, tot_rel_sf_base, tot_keep_bsb_base,tot_rel_bsb_base,  tot_cat_scup_base)) 
  
  
  prediction_output_by_period1 <- period_conversion %>%  
    dplyr::select(period2, month, day, mode1) %>% 
    dplyr::right_join(trip_level_output, by = c("period2")) 
  
  
  #Metrics at the choice occasion level
  prediction_output_by_period2 <- prediction_output_by_period1 %>%
    data.table::as.data.table() %>%
    .[, cv_sum := expand*change_CS] %>%
    .[, cs_base_sum := expand*CS_base] %>%
    .[, cs_alt_sum := expand*CS_alt] %>%
    .[, sf_keep_sum := expand*tot_keep_sf_new] %>%
    .[, sf_rel_sum := expand*tot_rel_sf_new] %>%
    .[, bsb_keep_sum := expand*tot_keep_bsb_new] %>%
    .[, bsb_rel_sum := expand*tot_rel_bsb_new] %>%
    .[, scup_keep_sum := expand*tot_keep_scup_new] %>%
    .[, scup_rel_sum := expand*tot_rel_scup_new] %>%
    
    .[, sf_keep_base_sum := expand*tot_keep_sf_base] %>%
    .[, sf_rel_base_sum := expand*tot_rel_sf_base] %>%
    .[, bsb_keep_base_sum := expand*tot_keep_bsb_base] %>%
    .[, bsb_rel_base_sum := expand*tot_rel_bsb_new] %>%
    .[, scup_tot_cat_base_sum := expand*tot_cat_scup_base] %>%

    .[, ntrips_alt := expand*probA] %>%
    .[mode1=="pr", cv_sum_pr := expand*change_CS] %>%
    .[mode1=="fh", cv_sum_fh := expand*change_CS] %>%
    .[mode1=="sh", cv_sum_sh := expand*change_CS] %>%
    .[mode1=="pr", cs_base_pr := expand*CS_base] %>%
    .[mode1=="fh", cs_base_fh := expand*CS_base] %>%
    .[mode1=="sh", cs_base_sh := expand*CS_base] %>%
    .[mode1=="pr", cs_alt_pr := expand*CS_alt] %>%
    .[mode1=="fh", cs_alt_fh := expand*CS_alt] %>%
    .[mode1=="sh", cs_alt_sh := expand*CS_alt] %>%
    .[mode1=="pr", sf_keep_sum_pr := expand*tot_keep_sf_new] %>%
    .[mode1=="fh", sf_keep_sum_fh := expand*tot_keep_sf_new] %>%
    .[mode1=="sh", sf_keep_sum_sh := expand*tot_keep_sf_new] %>%
    .[mode1=="pr", sf_rel_sum_pr := expand*tot_rel_sf_new] %>%
    .[mode1=="fh", sf_rel_sum_fh := expand*tot_rel_sf_new] %>%
    .[mode1=="sh", sf_rel_sum_sh := expand*tot_rel_sf_new] %>%
    .[mode1=="pr", bsb_keep_sum_pr := expand*tot_keep_bsb_new] %>%
    .[mode1=="fh", bsb_keep_sum_fh := expand*tot_keep_bsb_new] %>%
    .[mode1=="sh", bsb_keep_sum_sh := expand*tot_keep_bsb_new] %>%
    .[mode1=="pr", bsb_rel_sum_pr := expand*tot_rel_bsb_new] %>%
    .[mode1=="fh", bsb_rel_sum_fh := expand*tot_rel_bsb_new] %>%
    .[mode1=="sh", bsb_rel_sum_sh := expand*tot_rel_bsb_new] %>%
    .[mode1=="pr", scup_keep_sum_pr := expand*tot_keep_scup_new] %>%
    .[mode1=="fh", scup_keep_sum_fh := expand*tot_keep_scup_new] %>%
    .[mode1=="sh", scup_keep_sum_sh := expand*tot_keep_scup_new] %>%
    .[mode1=="pr", scup_rel_sum_pr := expand*tot_rel_scup_new] %>%
    .[mode1=="fh", scup_rel_sum_fh := expand*tot_rel_scup_new] %>%
    .[mode1=="sh", scup_rel_sum_sh := expand*tot_rel_scup_new] %>%
    .[mode1=="pr", ntrips_pr := expand*probA] %>%
    .[mode1=="fh", ntrips_fh := expand*probA] %>%
    .[mode1=="sh", ntrips_sh := expand*probA]
  
  
  
  
  ####Here we compute total harvest weight based on MRIP average weights 
  if (state1 %in% c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
    
  prediction_output_by_period_check0 <- prediction_output_by_period2 %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
    dplyr::group_by(mode1) %>% 
    dplyr::summarise(sf_keep_sum_num = sum(sf_keep_sum),
                      sf_rel_sum_num = sum(sf_rel_sum),
                      bsb_keep_sum_num = sum(bsb_keep_sum),
                      bsb_rel_sum_num = sum(bsb_rel_sum),
                      scup_keep_sum_num = sum(scup_keep_sum),
                      scup_rel_sum_num = sum(scup_rel_sum),
                      sf_keep_base_sum_num = sum(sf_keep_base_sum),
                      sf_rel_base_sum_num = sum(sf_rel_base_sum),
                      bsb_keep_base_sum_num = sum(bsb_keep_base_sum),
                      bsb_rel_base_sum_num = sum(bsb_rel_base_sum)) %>% 
                      #scup_tot_cat_base_sum = sum(scup_tot_cat_base_sum)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(sf_Discmortality_sum_num = sf_rel_sum_num*.1, 
                  bsb_Discmortality_sum_num = bsb_rel_sum_num*.15, 
                  scup_Discmortality_sum_num = scup_rel_sum_num*.15, 
                  sf_Discmortality_sum_weightavg = sf_Discmortality_sum_num*avg_lbs_release_sf, 
                  bsb_Discmortality_sum_weightavg = bsb_Discmortality_sum_num*avg_lbs_release_bsb, 
                  scup_Discmortality_sum_weightavg = scup_Discmortality_sum_num*avg_lbs_release_scup) %>% 
    dplyr::left_join(MRIP_harvest_weights, by=c("mode1")) %>% 
    dplyr::mutate(sf_keep_sum_weightavg= sf_keep_sum_num*mean_weightSF, 
                  bsb_keep_sum_weightavg= bsb_keep_sum_num*mean_weightBSB, 
                  scup_keep_sum_weightavg= scup_keep_sum_num*mean_weightSCUP,
                  sf_release_sum_weightavg= sf_rel_sum_num*avg_lbs_release_sf, 
                  bsb_release_sum_weightavg= bsb_rel_sum_num*avg_lbs_release_bsb, 
                  scup_release_sum_weightavg= scup_rel_sum_num*avg_lbs_release_scup) %>% 
    dplyr::select(mode1, bsb_keep_sum_num, bsb_keep_sum_weightavg, bsb_rel_sum_num, 
                  scup_keep_sum_num, scup_keep_sum_weightavg, scup_rel_sum_num, 
                  sf_keep_sum_num, sf_keep_sum_weightavg, sf_rel_sum_num, 
                  sf_Discmortality_sum_num, bsb_Discmortality_sum_num, scup_Discmortality_sum_num, 
                  sf_release_sum_weightavg,bsb_release_sum_weightavg,scup_release_sum_weightavg, 
                  scup_Discmortality_sum_weightavg, bsb_Discmortality_sum_weightavg, sf_Discmortality_sum_weightavg) #%>% 
  }
  
  
  
  
  
  if (state1 %in% c("NJ")){
    
    prediction_output_by_period_check0 <- prediction_output_by_period2 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
      dplyr::group_by(mode1) %>% 
      dplyr::summarise(sf_keep_sum_num = sum(sf_keep_sum),
                       sf_rel_sum_num = sum(sf_rel_sum),
                       bsb_keep_sum_num = sum(bsb_keep_sum),
                       bsb_rel_sum_num = sum(bsb_rel_sum),
                       scup_keep_sum_num = sum(scup_keep_sum),
                       scup_rel_sum_num = sum(scup_rel_sum),
                       sf_keep_base_sum_num = sum(sf_keep_base_sum),
                       sf_rel_base_sum_num = sum(sf_rel_base_sum),
                       bsb_keep_base_sum_num = sum(bsb_keep_base_sum),
                       bsb_rel_base_sum_num = sum(bsb_rel_base_sum)) %>% 
      #scup_tot_cat_base_sum = sum(scup_tot_cat_base_sum)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(NJ_bias_catch1, by="mode1") %>% 
      dplyr::mutate(sf_keep_sum_num=sf_keep_sum_num+diff_sf_keep, 
                    bsb_keep_sum_num=bsb_keep_sum_num+diff_bsb_keep, 
                    scup_keep_sum_num=scup_keep_sum_num+diff_scup_keep, 
                    sf_rel_sum_num=sf_rel_sum_num+diff_sf_rel, 
                    bsb_rel_sum_num=bsb_rel_sum_num+diff_bsb_rel, 
                    scup_rel_sum_num=scup_rel_sum_num+diff_scup_rel) %>% 
      dplyr::mutate(sf_Discmortality_sum_num = sf_rel_sum_num*.1, 
                    bsb_Discmortality_sum_num = bsb_rel_sum_num*.15, 
                    scup_Discmortality_sum_num = scup_rel_sum_num*.15, 
                    sf_Discmortality_sum_weightavg = sf_Discmortality_sum_num*avg_lbs_release_sf, 
                    bsb_Discmortality_sum_weightavg = bsb_Discmortality_sum_num*avg_lbs_release_bsb, 
                    scup_Discmortality_sum_weightavg = scup_Discmortality_sum_num*avg_lbs_release_scup) %>% 
      dplyr::left_join(MRIP_harvest_weights, by=c("mode1")) %>% 
      dplyr::mutate(sf_keep_sum_weightavg= sf_keep_sum_num*mean_weightSF, 
                    bsb_keep_sum_weightavg= bsb_keep_sum_num*mean_weightBSB, 
                    scup_keep_sum_weightavg= scup_keep_sum_num*mean_weightSCUP, 
                    sf_release_sum_weightavg= sf_rel_sum_num*avg_lbs_release_sf, 
                    bsb_release_sum_weightavg= bsb_rel_sum_num*avg_lbs_release_bsb, 
                    scup_release_sum_weightavg= scup_rel_sum_num*avg_lbs_release_scup) %>% 
      dplyr::select(mode1, bsb_keep_sum_num, bsb_keep_sum_weightavg, bsb_rel_sum_num, 
                    scup_keep_sum_num, scup_keep_sum_weightavg, scup_rel_sum_num, 
                    sf_keep_sum_num, sf_keep_sum_weightavg, sf_rel_sum_num, 
                    sf_Discmortality_sum_num, bsb_Discmortality_sum_num, scup_Discmortality_sum_num, 
                    sf_release_sum_weightavg,bsb_release_sum_weightavg,scup_release_sum_weightavg, 
                    scup_Discmortality_sum_weightavg, bsb_Discmortality_sum_weightavg, sf_Discmortality_sum_weightavg) #%>% 
  }
  
  

    #prediction_output_by_period_check01 contains keep, release,discard mortality numbers, and keep weights
    #based on MRIP average weights by mode. The number_weight var is set to "Weight" 
    prediction_output_by_period_check01<- prediction_output_by_period_check0 %>% 
      tidyr::pivot_longer(!mode1, names_to = "Var", values_to = "Value") %>% 
      dplyr::rename(mode=mode1) %>% 
      tidyr::separate(Var, into = c("Category", "keep_release", "param", "number_weight")) %>% 
      dplyr::mutate(param="Total") %>% 
      dplyr::mutate(number_weight=dplyr::case_when(number_weight=="num"~"Number", TRUE~number_weight)) %>%
      dplyr::mutate(number_weight=dplyr::case_when(number_weight=="weightavg"~"Weight_avg", TRUE~number_weight)) %>% 
      dplyr::mutate(state = state1) %>% 
      replace(is.na(.), 0)
      
    #prediction_output_by_period_check02 contains keep, release,discard mortality numbers, and keep weights
    #based on MRIP average weights across all modes. The number_weight var is set to "Weight"   
    prediction_output_by_period_check02<- prediction_output_by_period_check01 %>% 
      dplyr::group_by(Category, keep_release, param, number_weight) %>% 
      dplyr::summarise(Value = sum(Value), .groups = "drop") %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(state = state1, mode="all")
      
      
  
    #prediction_output_by_period1 contains CV and ntrips estimates by mode 
    #CV and trip estimates - all states besides NJ
if (state1 %in% c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
  prediction_output_by_period1 <- prediction_output_by_period2 %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>% 
    dplyr::group_by(mode1) %>% 
    dplyr::summarise(CV = sum(cv_sum), 
                     ntrips = sum(ntrips_alt)) %>% 
    dplyr::ungroup()
  }
  
  
  
  
  if (state1 %in% c("NJ")){
    prediction_output_by_period1 <- prediction_output_by_period2 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)  %>%
      dplyr::mutate(draw=x) %>% 
      dplyr::group_by(mode1) %>% 
      dplyr::summarise(CV = sum(cv_sum), 
                       ntrips = sum(ntrips_alt)) %>% 
      dplyr::ungroup()  %>% 
      dplyr::left_join(NJ_bias_CV_trips1, by="mode1") %>% 
      dplyr::mutate(CV=CV+difference_CV, ntrips=ntrips+difference_ntrips) %>% 
      dplyr::select(-difference_CV, -difference_ntrips, -draw)
    }  
  
    
    #prediction_mode contains CV and ntrips estimates by mode 
    prediction_mode<- prediction_output_by_period1 %>% 
    tidyr::pivot_longer(!mode1, names_to = "Var", values_to = "Value") %>% 
    dplyr::mutate(Var = paste0(Var, "_", mode1)) %>% 
    dplyr::select(!mode1)
  
    #prediction_all_mode contains CV and ntrips estimates across all modes 
    prediction_all_mode<- prediction_output_by_period1  %>% 
    dplyr::summarise(CV= sum(CV), 
                     ntrips = sum(ntrips)) %>% 
      dplyr::mutate(mode1="all") %>% 
      tidyr::pivot_longer(!mode1, names_to = "Var", values_to = "Value") %>% 
      dplyr::mutate(Var = paste0(Var, "_", mode1)) %>% 
      dplyr::select(!mode1)
  
  #Now we combine all the data into one file
    predictions <- rbind(prediction_mode, prediction_all_mode, l_w_mode, l_w_sum) %>% 
      tidyr::separate(Var, into = c("Category", "mode", "keep_release", "param", "number_weight")) %>% 
      dplyr::mutate(state = state1) %>% 
      #run_number = x
      dplyr::filter(!Value == "NA") %>% 
      dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="CV"~"Dollars",TRUE ~ number_weight)) %>% 
      dplyr::mutate(number_weight=dplyr::case_when(is.na(number_weight) & Category=="ntrips"~"Ntrips",TRUE ~ number_weight))  %>% 
      #remove from this data keep numbers, release numbers, discard mortality numbers from the length-weight equation data, 
      #as we use values from prediction_output_by_period_check0
      dplyr::filter(number_weight!="Number") %>% 
      #Now bind the keep and release numbers, discard mortality numbers, and weights based on MRIP averages
      rbind(prediction_output_by_period_check01, prediction_output_by_period_check02) %>% 
      dplyr::mutate(keep_release=dplyr::case_when(keep_release=="rel"~"release",TRUE ~ keep_release)) 
    
 #}
 
  return(predictions)
}
  
  