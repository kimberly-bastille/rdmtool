pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflicts_prefer(dplyr::mutate)
options(scipen = 100, digits = 3)

## calibrate_pstars

select_mode = "pr"
select_season = 1
k = 1
directed_trips_file_path = "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/directed_trips_calib_150draws.csv"
catch_draws_file_path = "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws"
MRIP_comparison = "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/simulated_catch_totals_open_season.csv"
#pstar_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars"
size_data_read = read.csv("C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/input_code/agepro/projected_CaL_cod_hadd_open_season_test.csv")

#pds_test <-   readRDS(file.path(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/pds_new_all_1.rds")))
#costs_test <-   readRDS(file.path(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/cost_new_all_1.rds")))

calibrate_rec_catch <- function(p_star_cod, p_star_had, select_mode, k, select_season,
                             directed_trips_file_path, catch_draws_file_path, MRIP_comparison, pstar_file_path){

  # MRIP_data <-   read.csv(file.path(paste0(MRIP_comparison))) %>%
  #   dplyr::filter(mode == select_mode,
  #                 draw == k, 
  #                 dtrip>0)

  
  
  MRIP_data <-   read.csv(file.path(paste0(MRIP_comparison))) %>%
    dplyr::filter(#draw == k,
                  dtrip>0) %>% 
    dplyr::mutate(mrip_index=1:nrow(MRIP_data))

  MRIP_data <-   read.csv(file.path(paste0(MRIP_comparison))) %>%
    dplyr::filter(dtrip>0)
  
  MRIP_data<-MRIP_data %>% 
    dplyr::mutate(mrip_index=1:nrow(MRIP_data))
  
  
  # if (MRIP_data$dtrip == 0) {
  #   pds_new_all<- data.frame(period2 = NA, tot_keep_cod = 0, tot_keep_had = 0,
  #                            tot_rel_cod = 0, tot_rel_had = 0, tot_cod_catch = 0,
  #                            tot_had_catch = 0, estimated_trips = 0, n_choice_occasions = 0,
  #                            n_cal_draw = NA, mode = NA)
  # 
  # } else {
  
  for(i in 1:nrow(MRIP_data)){
    
    MRIP_data<-MRIP_data %>% 
      dplyr::filter(mrip_index==1)
    
    select_mode = unique(MRIP_data$mode)
    select_season = unique(MRIP_data$open)
    
    MRIP_stats<-MRIP_data %>% 
      dplyr::rename(tot_cod_catch_mrip=tot_cod_catch, 
                    tot_cod_keep_mrip= tot_cod_keep, 
                    tot_cod_rel_mrip=tot_cod_rel, 
                    tot_hadd_catch_mrip=tot_hadd_catch, 
                    tot_hadd_keep_mrip=tot_hadd_keep, 
                    tot_hadd_rel_mrip=tot_hadd_rel, 
                    dtrip_mrip=dtrip) 
                    
    
    
  
  
   print(k)

    n_drawz = 50
    n_catch_draws = 30
    set.seed(k)
    directed_trips<-read.csv(directed_trips_file_path) %>%
      tibble::tibble() %>%
      dplyr::filter(draw == k,
                    mode == select_mode) %>%
      dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0))

    # New_DTRIP <- read.csv(directed_trips_file_path)  %>%
    #   dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0)) %>%
    #   dplyr::reframe(dtrip= sum(dtrip), .by = c(mode, open, draw))


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
      dplyr::mutate(month = as.numeric(month)) %>%
      #dplyr::mutate(period2 = as.character(paste0(month, "_", day, "_", mode))) %>% #make day of year and mode combo
      #group_by(period) %>%
      dplyr::mutate(#n_trips = floor(mean(dtrip_2019)),
        #n_trips = floor(dtrip),
        n_draws = n_drawz)%>%
      dplyr::select(!c(month, mode))

    regs <- directed_trips_p %>%
      dplyr::select(period2,
                    cod_bag,
                    cod_min,
                    hadd_bag,
                    hadd_min)

    param_draws <- directed_trips_p %>%
      dplyr::select(period2, n_draws, open) %>%
      tidyr::uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))

    cod_catch_data <- read.csv(file.path(paste0(catch_draws_file_path, k, "_full.csv"))) %>%
      dplyr::mutate(day1 = as.numeric(stringr::str_sub(day, 1, 2)), 
                    month1=stringr::str_sub(day, 3, 5),
                    month=ifelse(month1=="jan", 1,month1), 
                    month=ifelse(month1=="feb", 2,month),
                    month=ifelse(month1=="mar", 3,month),
                    month=ifelse(month1=="apr", 4,month),
                    month=ifelse(month1=="may", 5,month),
                    month=ifelse(month1=="jun", 6,month),
                    month=ifelse(month1=="jul", 7,month),
                    month=ifelse(month1=="aug", 8,month),
                    month=ifelse(month1=="sep", 9,month),
                    month=ifelse(month1=="oct", 10,month),
                    month=ifelse(month1=="nov", 11,month),
                    month=ifelse(month1=="dec", 12,month),  
                    period2 = paste0(month, "_", day1, "_", mode)) %>%
      dplyr::left_join(open, by = "period2") %>%
      dplyr::filter(open == select_season) %>%
      dplyr::select(!open, !day) %>%
      dplyr::rename(tot_cod_catch = cod_catch,
                    tot_had_catch = hadd_catch)  %>%
      dplyr::select(mode,month,tot_cod_catch,tot_had_catch,
                    tripid,catch_draw,day, draw, age, days_fished, cost, period2)


    trip_costs<-cod_catch_data  %>%
      dplyr::filter(mode == select_mode) %>%
      dplyr::select(cost)

    age<-cod_catch_data  %>%
      dplyr::filter(mode == select_mode) %>%
      dplyr::select(age)

    avidity<-cod_catch_data  %>%
      dplyr::filter(mode == select_mode) %>%
      dplyr::select(days_fished)

    cod_catch_data <- cod_catch_data %>%
      # dplyr::rename(sf_tot_cat = tot_cat_sf,
      #               bsb_tot_cat = tot_cat_bsb,
      #               scup_tot_cat = tot_cat_scup)  %>%
      # dplyr::rename(mode = mode1) %>%
      dplyr::mutate(day = as.numeric(stringr::str_extract(day, "\\d+"))) %>%
      dplyr::group_by(period2) %>%
      dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
      dplyr::mutate(#period = rep(period_vec$period2, each = nsamp),
        catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
        tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
      dplyr::ungroup()%>%
      dplyr::select(!c(age, days_fished, cost))%>%
      dplyr::select(!c(month))
    print("postmutate")


    if(select_season == 1){
    seas = "open"
    }
    if(select_season == 0){
      seas = "closed"
    }
    
    ##Lou uncommented 
    # pstar<- read.csv(file.path(here::here(paste0(pstar_file_path, "/pstar_", select_mode, "_",seas, ".csv")))) %>%
    #   dplyr::filter(run_number == k)
    # p_star_cod <- pstar %>%
    #   dplyr::filter(species == "COD")
    # p_star_cod <- p_star_cod$p_star_value
    # 
    # p_star_had <- pstar %>%
    #   dplyr::filter(species == "HAD")
    # p_star_had <- p_star_had$p_star_value
    ##End Lou uncommented 
    
    cod_size_data <- size_data_read %>% dplyr::filter(species == "cod", season == seas, run_number==k, mode==select_mode)
    had_size_data <- size_data_read %>% dplyr::filter(species == "hadd", season == seas, run_number==k, mode==select_mode)

    cod_had_catch_data <- cod_catch_data


    # subset trips with zero catch, as no size draws are required
    cod_zero_catch <- dplyr::filter(cod_catch_data, tot_cod_catch == 0)


    #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
    cod_catch_check<-base::sum(cod_catch_data$tot_cod_catch)
    had_catch_check<-base::sum(cod_catch_data$tot_had_catch)

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
                                             prob = cod_size_data$fitted_prob,
                                             replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
      
      
      
      
      
      # Impose regulations, calculate keep and release per trip
      # For summer flounder, retain keep- and release-at-length
      ####### Start Here #################
      
      ############# Length #####################################
      catch_size_data <- catch_size_data %>%
        dplyr::left_join(regs, by = c("period2")) %>%
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
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0))
      
      catch_size_data<- catch_size_data %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, period2, catch_draw, mode)  %>%
        dplyr::rename(mode1=mode)
      
      # new_size_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      #   dplyr::summarize(keep = sum(keep),
      #                    release = sum(release), .groups = "drop") %>%
      #   dplyr::ungroup()
      
      
      trip_data <- catch_size_data %>%
        dplyr::group_by(period2, catch_draw, tripid) %>%
        dplyr::summarize(tot_keep_cod_new = sum(keep),
                         tot_rel_cod_new = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      
      # keep_size_data <- new_size_data %>%
      #   dplyr::select(-release) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "keep_cod_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = keep,
      #                      values_fill = 0)
      # 
      # release_size_data <- new_size_data %>%
      #   dplyr::select(-keep) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "release_cod_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = release,
      #                      values_fill = 0)
      # 
      # keep_release_cod <- keep_size_data %>%
      #   dplyr::left_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
      
      # trip_data <- summed_catch_data
      
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

      # # generate lengths for each fish
      catch_size_data_had <- had_catch_data %>%
        dplyr::mutate(fitted_length = sample(had_size_data$length,
                                             nrow(.),
                                             prob = had_size_data$fitted_prob,
                                             replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
      
      
      
      
      
      # Impose regulations, calculate keep and release per trip
      # For summer flounder, retain keep- and release-at-length
      ####### Start Here #################
      
      ############# Length #####################################
      catch_size_data_had <- catch_size_data_had %>%
        dplyr::left_join(regs, by = c("period2")) %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=hadd_min ,1,0)) %>%
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
      
      
      catch_size_data_had <- catch_size_data_had %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data_had <- catch_size_data_had %>%
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0))
      
      catch_size_data_had<- catch_size_data_had %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, period2, catch_draw, mode)  %>%
        dplyr::rename(mode1=mode)
      
      # new_size_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      #   dplyr::summarize(keep = sum(keep),
      #                    release = sum(release), .groups = "drop") %>%
      #   dplyr::ungroup()
      
      
      trip_data_hadd <- catch_size_data_had %>%
        dplyr::group_by(period2, catch_draw, tripid) %>%
        dplyr::summarize(tot_keep_hadd_new = sum(keep),
                         tot_rel_hadd_new = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      

      had_zero_catch<-had_zero_catch %>%
        dplyr::select(tripid, catch_draw, period2) %>%
        dplyr::mutate(tot_keep_hadd_new=0,
                      tot_rel_hadd_new=0)
      
      trip_data_hadd <- dplyr::bind_rows(trip_data_hadd, had_zero_catch) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::select(c("period2", "catch_draw","tripid",
                        "tot_keep_hadd_new","tot_rel_hadd_new"))
      
      
      trip_data_hadd<- trip_data_hadd %>% dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
        dplyr::select(-period2, -catch_draw, -tripid)
      trip_data_hadd<-data.table::as.data.table(trip_data_hadd)
      data.table::setkey(trip_data_hadd, "domain2")
      
      # merge the bsb trip data with the rest of the trip data
      #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))
      trip_data<-trip_data[trip_data_hadd, on = "domain2"]
      
    }
    
    
    if (had_catch_check==0){
      trip_data$tot_had_catch<-0
      trip_data$tot_keep_had<-0
      trip_data$tot_rel_had<-0
    }
    
    
    period_vec1 <- param_draws %>%
      dplyr::mutate(beta_sqrt_cod_keep = rnorm(nrow(param_draws), mean = 1.594, sd = .615),
                    beta_sqrt_cod_release = rnorm(nrow(param_draws), mean = 0.162 , sd = 0.445),
                    beta_sqrt_hadd_keep = rnorm(nrow(param_draws), mean = 1.156, sd = 0.603 ),
                    beta_sqrt_hadd_release = rnorm(nrow(param_draws), mean = 0.094 , sd = 0 ),
                    beta_sqrt_cod_hadd_keep = rnorm(nrow(param_draws), mean =-0.314  , sd = 0.778 ),
                    beta_cost = rnorm(nrow(param_draws), mean =-0.015 , sd =0 ),
                    beta_opt_out = rnorm(nrow(param_draws), mean =-1.871 , sd = 3.208 ),
                    beta_opt_out_age = rnorm(nrow(param_draws), mean =0.047 , sd = 0 ),
                    beta_opt_out_likely = rnorm(nrow(param_draws), mean =-1.272 , sd = 0 ),
                    beta_opt_out_prefer = rnorm(nrow(param_draws), mean =-1.079 , sd = 0 ))%>%
      dplyr::group_by(period2) %>% dplyr::mutate(tripid = dplyr::row_number(period2))


    trip_data<- trip_data %>%
      dplyr::left_join(period_vec1, by = c("period2","tripid")) %>% 
      dplyr::arrange(period2, tripid, catch_draw) %>% 
      cbind(trip_costs) %>% 
      cbind(age) %>% 
      cbind(avidity)
    
    
    
    
    #Re-allocate releases as discards (cod)
    h_star_cod<-.02
    
    trip_data_cod_hstar<-trip_data %>% 
      dplyr::select(period2, tripid, catch_draw, tot_keep_cod_new, tot_rel_cod_new) %>% 
      dplyr::group_by(period2, tripid) %>% 
      dplyr::summarise(sum_tot_keep_cod_new=sum(tot_keep_cod_new), 
                       sum_tot_rel_cod_new=sum(tot_rel_cod_new), .groups='drop') %>% 
      dplyr::filter(sum_tot_rel_cod_new>0) 
     
    trip_data_cod_hstar<-trip_data_cod_hstar %>% 
      dplyr::mutate(uniform=runif(nrow(trip_data_cod_hstar))) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(tripid2=1:nrow(trip_data_cod_hstar))
    
    n_occasions_keep_all_cod=h_star_cod*nrow(trip_data_cod_hstar)
    
    trip_data_cod_hstar <-trip_data_cod_hstar %>% 
    dplyr::filter(tripid2<=n_occasions_keep_all_cod) %>% 
      dplyr::mutate(release_to_keep=1) %>% 
      dplyr::select(period2, tripid, release_to_keep)
    
    trip_data<-trip_data %>% 
      dplyr::left_join(trip_data_cod_hstar, by = c("period2","tripid")) %>% 
      data.table::as.data.table() %>%
      .[release_to_keep==1, tot_keep_cod_new1 := tot_rel_cod_new] %>% ##if marked as a full harvest CE, 
                                                                        #cod keep = what was originally released
      .[release_to_keep==1, tot_rel_cod_new1 := 0]%>%                   #cod released = 0 
      .[release_to_keep==1, tot_keep_cod_new := tot_keep_cod_new1] %>%
      .[release_to_keep==1, tot_rel_cod_new := tot_rel_cod_new1] %>% 
      dplyr::select(-tot_keep_cod_new1, -tot_rel_cod_new1, -release_to_keep)
    
    
    
    #Re-allocate releases as discards (haddock)
    h_star_hadd<-.2
    
    trip_data_hadd_hstar<-trip_data %>% 
      dplyr::select(period2, tripid, catch_draw, tot_keep_hadd_new, tot_rel_hadd_new) %>% 
      dplyr::group_by(period2, tripid) %>% 
      dplyr::summarise(sum_tot_keep_hadd_new=sum(tot_keep_hadd_new), 
                       sum_tot_rel_hadd_new=sum(tot_rel_hadd_new), .groups='drop') %>% 
      dplyr::filter(sum_tot_rel_hadd_new>0) 
    
    trip_data_hadd_hstar<-trip_data_hadd_hstar %>% 
      dplyr::mutate(uniform=runif(nrow(trip_data_hadd_hstar))) %>%
      dplyr::arrange(uniform) %>% 
      dplyr::mutate(tripid2=1:nrow(trip_data_hadd_hstar))
    
    n_occasions_keep_all_hadd=h_star_hadd*nrow(trip_data_hadd_hstar)
    
    trip_data_hadd_hstar <-trip_data_hadd_hstar %>% 
      dplyr::filter(tripid2<=n_occasions_keep_all_hadd) %>% 
      dplyr::mutate(release_to_keep=1) %>% 
      dplyr::select(period2, tripid, release_to_keep)
    
    trip_data<-trip_data %>% 
      dplyr::left_join(trip_data_hadd_hstar, by = c("period2","tripid")) %>% 
      data.table::as.data.table() %>%
      .[release_to_keep==1, tot_keep_hadd_new1 := tot_rel_hadd_new] %>% ##if marked as a full harvest CE, 
                                                                         #hadd keep = what was originally released
      .[release_to_keep==1, tot_rel_hadd_new1 := 0]%>%                   #hadd released = 0 
      .[release_to_keep==1, tot_keep_hadd_new := tot_keep_hadd_new1] %>%
      .[release_to_keep==1, tot_rel_hadd_new := tot_rel_hadd_new1] %>% 
      dplyr::select(-tot_keep_hadd_new1, -tot_rel_hadd_new1, -release_to_keep)
    
    
    # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario.
    # We will merge these data to the prediction year outcomes to calculate changes in CS.
    costs_new_all <- trip_data %>%
      dplyr::select(c(tripid, cost, catch_draw, tot_keep_cod_new, tot_rel_cod_new,
                      age, days_fished, beta_opt_out_age,  beta_opt_out_likely,  beta_opt_out_prefer,
                      tot_keep_hadd_new,tot_rel_hadd_new,
                      beta_cost, beta_opt_out, beta_sqrt_hadd_keep,
                      beta_sqrt_hadd_release, beta_sqrt_cod_hadd_keep,
                      beta_sqrt_cod_keep, beta_sqrt_cod_release, period2)) %>%
      dplyr::rename(tot_keep_cod_base = tot_keep_cod_new,
                    tot_rel_cod_base = tot_rel_cod_new,
                    tot_keep_had_base = tot_keep_hadd_new,
                    tot_rel_had_base = tot_rel_hadd_new)%>%
      dplyr::mutate(n_cal_draw = k,
                    mode = select_mode,
                    open = select_season)


    #  utility (prediction year)
    trip_data <-trip_data %>%
      dplyr::mutate(
        vA = beta_sqrt_cod_keep*sqrt(tot_keep_cod_new) +
          beta_sqrt_cod_release*sqrt(tot_rel_cod_new) +
          beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_new) +
          beta_sqrt_hadd_release*sqrt(tot_rel_hadd_new) +
          beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod_new)*sqrt(tot_keep_hadd_new)) +
          beta_cost*cost)

    trip_data <- trip_data %>%
      dplyr::mutate(period = as.numeric(as.factor(period2)))

    period_names<-subset(trip_data, select=c("period", "period2"))
    period_names <- period_names[!duplicated(period_names), ]


    mean_trip_data <- trip_data %>% data.table::data.table() #%>% dplyr::arrange(period, tripid, catch_draw)

    # Now expand the data to create two alternatives, representing the alternatives available in choice survey
    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
      tidyr::uncount(n_alt) %>%
      dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                    opt_out = ifelse(alt == 2, 1, 0))

    #Calculate the expected utility of alts 2 parameters of the utility function,
    #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[, vA_optout := beta_opt_out*opt_out+
          beta_opt_out_age*age + beta_opt_out_likely*days_fished] %>%
      .[alt==1, expon_vA := exp(vA)] %>%
      .[alt==2, expon_vA := exp(vA_optout)]


    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[, vA_col_sum := sum(expon_vA), by=list(period, catch_draw, tripid)]

    #Calculate probability of each choice occasion
    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[, probA :=expon_vA/vA_col_sum]

    mean_trip_data<- subset(mean_trip_data, alt==1) %>% 
      dplyr::select(-domain2) %>% 
      dplyr::mutate(tot_cat_cod_new=tot_keep_cod_new+tot_rel_cod_new, 
                    tot_cat_hadd_new=tot_keep_hadd_new+tot_rel_hadd_new)

    # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>%
    #   tibble()

    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid")]


    mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2","tripid"), .SDcols = all_vars]


    # Get rid of things we don't need.
    mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost,beta_opt_out, beta_opt_out_age, 
                                                              beta_opt_out_likely, beta_opt_out_prefer, beta_sqrt_cod_hadd_keep, 
                                                              beta_sqrt_cod_keep, beta_sqrt_cod_release, beta_sqrt_hadd_keep, 
                                                              beta_sqrt_hadd_release, days_fished, open, period, catch_draw, expon_vA,
                                                              opt_out, vA, vA_optout, vA_col_sum, cost, age))

    # Multiply the average trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
    list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid"
                                           & colnames(mean_trip_data) !="period2"
                                           & colnames(mean_trip_data) !="probA"]


    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]

    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(n_choice_occasions = rep(1,nrow(.))) %>%
      dplyr::left_join(period_names, by = c("period2"))

    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    #calibration_data <- calibration_data  %>%   rename(period2 = period)

    sims <- directed_trips_p %>%
      dplyr::select(c(dtrip, period2)) 
    
    
    mean_trip_data<-mean_trip_data %>% 
      dplyr::select(-period, -tripid)
    

    mean_trip_data <- mean_trip_data %>%
      dplyr::left_join(sims, by="period2")


    mean_probs<-mean_trip_data  %>% 
      dplyr::select(period2, probA) %>% 
      dplyr::rename(mean_prob=probA) %>% 
      data.table::as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2"), .SDcols = "mean_prob"]

    mean_trip_data <- mean_trip_data %>%
      dplyr::left_join(mean_probs, by="period2")
    

  
    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(sims=dtrip/mean_prob,
                    expand=sims/n_drawz) %>% 
      dplyr::rename(tot_keep_cod_model=tot_keep_cod_new, 
                    tot_keep_hadd_model=tot_keep_hadd_new, 
                    tot_rel_cod_model=tot_rel_cod_new, 
                    tot_rel_hadd_model=tot_rel_hadd_new, 
                    tot_cat_hadd_model=tot_cat_hadd_new, 
                    tot_cat_cod_model=tot_cat_cod_new)

    
    # all_vars<-c()
    # all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2")]
    # 
    # 
    # mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
    #   .[,lapply(.SD, sum), by = c("period2"), .SDcols = all_vars]
    # 
    

    list_names = c("tot_keep_cod_model","tot_keep_hadd_model",
                   "tot_rel_cod_model","tot_rel_hadd_model",
                   "tot_cat_hadd_model","tot_cat_cod_model",
                   "probA", "n_choice_occasions")


    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * expand), .SDcols = list_names] %>%
      .[]

    aggregate_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2"), .SDcols = list_names]

    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "estimated_trips"
    pds_new_all<-aggregate_trip_data %>%
      #dplyr::left_join(aggragate_length_data, by = "period2") %>%
      dplyr::mutate(draw = k,
                    mode = select_mode,
                    open = select_season)
    
    aggregate_trip_data2=pds_new_all %>% 
      dplyr::group_by(draw, mode, open) %>% 
      dplyr::summarize(tot_keep_cod_model=sum(tot_keep_cod_model),
                       tot_keep_hadd_model=sum(tot_keep_hadd_model), 
                       tot_rel_cod_model=sum(tot_rel_cod_model), 
                       tot_rel_hadd_model=sum(tot_rel_hadd_model), 
                       tot_cat_hadd_model=sum(tot_cat_hadd_model), 
                       tot_cat_cod_model=sum(tot_cat_cod_model), 
                       estimated_trips=sum(estimated_trips), .groups='drop') %>% 
      dplyr::mutate(mrip_index=1)

    comparison<-aggregate_trip_data2 %>% 
      dplyr::left_join(MRIP_stats, by=c("mode", "open", "draw", "mrip_index"))
    
    
    comparison<-comparison %>% 
      dplyr::mutate(diff_cod_harv=tot_keep_cod_model-tot_cod_keep_mrip, 
                    diff_hadd_harv=tot_keep_hadd_model-tot_hadd_keep_mrip)
    
    diff_cod_harv<-comparison$diff_cod_harv
    perc_diff_cod_harv<-((comparison$tot_keep_cod_model-comparison$tot_cod_keep_mrip)/comparison$tot_cod_keep_mrip)*100
    
    diff_hadd_harv<-comparison$diff_hadd_harv
    perc_diff_hadd_harv<-((comparison$tot_keep_hadd_model-comparison$tot_hadd_keep_mrip)/comparison$tot_hadd_keep_mrip)*100
    
    
    
    comparison$tot_keep_cod_model
    comparison$tot_cod_keep_mrip
    diff_cod_harv
    perc_diff_cod_harv
    
    
    comparison$tot_keep_hadd_model
    comparison$tot_hadd_keep_mrip
    diff_hadd_harv
    perc_diff_hadd_harv
    
    
  }
    
    
   # output<-list(pds_new_all, costs_new_all)
    #write.csv(pds_new_all, file = here::here(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/out/pds_new_all_", select_mode, "_", k, ".csv")))
    #write.csv(costs_new_all, file = here::here(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/out/costs_new_all_", select_mode, "_", k, ".csv")))
    #return(output)
  #return(output)
#}
#}





