# state1 <- "CT"
# state_no<-9
# directed_trips_table <- directed_trips_table_base[[1]]
# sf_catch_data_all <- catch_files_CT[[2]]
# p_star_sf <- p_star_sf_CT_variable
# p_star_bsb<-p_star_bsb_CT_variable
# p_star_scup<-p_star_scup_CT_variable
# k <- 1
# state1 <- "VA"
# state_no<-51
# directed_trips_table <- directed_trips_table_base[[8]]
# sf_catch_data_all <- catch_files_all_base[[8]]
# p_star_sf <- p_star_sf_VA_variable
# p_star_bsb<-p_star_bsb_VA_variable
# p_star_scup<-p_star_scup_VA_variable
#  state1 <- "NJ"
#  state_no<-34
#  k = 1
#  select_mode = "fh"
# # directed_trips_table <- directed_trips_table_base[[5]]
# # sf_catch_data_all <- readRDS(here::here("data-raw/catch/catch_files_NJ.rds"))
# p_star_sf <- p_star_sf_NJ_variable_fh
# p_star_bsb<-p_star_bsb_NJ_variable_fh
# p_star_scup<-p_star_scup_NJ_variable_fh

calibrate_rec_catch <- function(state1,
                                state_no,
                                #sf_catch_data_all,
                                p_star_sf,
                                p_star_bsb,
                                p_star_scup,
                                select_mode, 
                                k){
  
  print(k)
  
  n_drawz = 50
  n_catch_draws = 30
  
  directed_trips<-readRDS(file.path(here::here(paste0("data-raw/directed_trips/directed_trips_NJ.rds")))) %>% 
    tibble::tibble() %>% 
    dplyr::filter(draw == k, 
                  mode == select_mode)
  

  ######################################
  ##   Begin simulating trip outcomes ##
  ######################################
  
  # Set up an output file for the separately simulated within-season regulatory periods
  directed_trips_p <- directed_trips %>%
    dplyr::mutate(day = as.numeric(stringr::str_extract(day, "^\\d{2}")), 
                  month = as.numeric(month)) %>% 
    dplyr::mutate(period2 = as.character(paste0(month, "_", day, "_", mode))) %>% #make day of year and mode combo
    #group_by(period) %>%
    dplyr::mutate(#n_trips = floor(mean(dtrip_2019)),
      n_trips = floor(dtrip),
      n_draws = n_drawz)
  
  regs <- directed_trips_p %>%
    dplyr::select(period2,
                  fluke_bag1, fluke_min1, fluke_max1,
                  fluke_bag2, fluke_min2, fluke_max2,
                  bsb_bag,
                  bsb_min,
                  scup_bag,
                  scup_min )
  
  period_vec <- directed_trips_p %>%
    dplyr::select(period2, n_draws, month) %>%
    tidyr::uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))
  
  sf_catch_data <- read.csv(file.path(here::here(paste0("data-raw/catch/",state1," catch draws 2022 draw ", k, ".csv")))) %>% 
    dplyr::filter(mode1 == select_mode) %>% 
     dplyr::rename(tot_sf_catch = tot_cat_sf,
                   tot_bsb_catch = tot_cat_bsb,
                   tot_scup_catch = tot_cat_scup) # %>% 
    # dplyr::rename(mode = mode1) 
    
    #dplyr::rename(tot_sf_catch = sf_catch,  tot_bsb_catch = bsb_catch, tot_scup_catch = scup_catch)  %>%
    #dplyr::select(-c(month))
  
  sf_catch_data <- sf_catch_data %>%
    # dplyr::rename(sf_tot_cat = tot_cat_sf,
    #               bsb_tot_cat = tot_cat_bsb,
    #               scup_tot_cat = tot_cat_scup)  %>%
    # dplyr::rename(mode = mode1) %>%
    dplyr::mutate(day = as.numeric(stringr::str_extract(day, "\\d+")),
                  period2 = paste0(month, "_", day, "_", mode1)) %>% 
    dplyr::group_by(period2) %>%
    dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
    dplyr::mutate(#period = rep(period_vec$period2, each = nsamp),
      catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
      tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
    dplyr::ungroup()
  print("postmutate")
  
  #Need 1,000xn_catch_draws(per trip) random draws of catch for each period, with catch-per-trip rates that vary by month.
  
  # check <- directed_trips_p %>%
  #     dplyr::select(period2,month, mode1) %>%
  #     distinct(period2,month, mode1) %>%
  #     mutate(period2=as.factor(period2)) %>%
  #     mutate(month=as.numeric(month))%>%
  #     #mutate(month=sprintf("%02d", month)) %>%
  #     rename(month1=month)
  #
  #
  # sf_catch_data_pd<-list()
  # sf_catch_data_check_draw<-list()
  #
  #
  # levels(check$period2)
  #   for(p in levels(check$period2)){
  #     #p<-"10_bt"
  #       check1<-subset(check, period2==p)
  #       month_val<-unique(check1$month1)
  #       md_val<-unique(check1$mode1)
  #
  #       for(i in 1:n_catch_draws){
  #        # i=1
  #         sf_catch_data_check<- sf_catch_data %>% dplyr::filter(month == month_val & mode == md_val) # %>%
  #         #sf_catch_data_check <- sf_catch_data_check %>%  slice_sample(n = 1000)
  #         #sf_catch_data_check<-sf_catch_data_check[sample(nrow(sf_catch_data_check), size=n_drawz), ]
  #         sf_catch_data_check<- sample_n(sf_catch_data_check,n_drawz)
  #
  #         #[sample(nrow(sf_catch_data_check), size=n_draws), ]
  #
  #
  #         sf_catch_data_check <- sf_catch_data_check  %>%
  #         mutate(catch_draw=i) %>%
  #         mutate(tripid=1:n_drawz)
  #
  #         sf_catch_data_check_draw[[i]]<-sf_catch_data_check
  #       }
  #
  #       sf_catch_data_check_draw_all<- list.stack(sf_catch_data_check_draw)
  #       sf_catch_data_check_draw_all<- sf_catch_data_check_draw_all %>% mutate(period2=p)
  #       sf_catch_data_pd[[p]]=sf_catch_data_check_draw_all
  #
  #       }
  #
  #       sf_catch_data<- list.stack(sf_catch_data_pd)
  #       rm(sf_catch_data_pd)
  #       rm(sf_catch_data_check_draw_all, sf_catch_data_check_draw)
  
  #sf_catch_data<- sf_catch_data %>%   #dplyr::arrange(period2, tripid, catch_draw) %>% plyr::select(-wp_int)
  
  
  # sf_catch_data$tab<-1
  # agg_tbl <- sf_catch_data %>% group_by(period2) %>%
  #   summarise(sum_tab=sum(tab), .groups = 'drop')
  
  sf_bsb_catch_data <- sf_catch_data
  
  
  # subset trips with zero catch, as no size draws are required
  sf_zero_catch <- dplyr::filter(sf_catch_data, tot_sf_catch == 0) %>% 
    dplyr::select(!mode1)
  
  #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
  sf_catch_check<-base::sum(sf_catch_data$tot_sf_catch)
  bsb_catch_check<-base::sum(sf_catch_data$tot_bsb_catch)
  scup_catch_check<-base::sum(sf_catch_data$tot_scup_catch)
  
  
  #remove trips with zero summer flounder catch
  sf_catch_data <- dplyr::filter(sf_catch_data, tot_sf_catch > 0)
  
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(sf_catch_data))
  #sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
  sf_catch_data<- sf_catch_data %>%
    dplyr::slice(rep(row_inds,tot_sf_catch))   %>%
    dplyr::mutate(fishid=dplyr::row_number())
  
  
  
  # sf_size_data<- sf_size_data %>% 
  #   dplyr::filter(state == state1)
  # # generate lengths for each fish
  # catch_size_data <- sf_catch_data %>%
  #   dplyr::mutate(fitted_length = sample(sf_size_data$length,
  #                                        nrow(.),
  #                                        prob = sf_size_data$fitted_prob,
  #                                        replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
  # 
  ##I()
  
  # Impose regulations, calculate keep and release per trip
  # For summer flounder, retain keep- and release-at-length
  ####### Start Here #################
  
  # ################### P_Star #############################
  catch_data2 <- sf_catch_data %>%
    dplyr::left_join(regs, by = "period2") %>%
    dplyr::mutate(uniform=runif(nrow(sf_catch_data))) %>%
    dplyr::mutate(posskeep = ifelse(uniform>=p_star_sf, 1,0)) %>%
    dplyr::group_by(tripid, period2, catch_draw)   %>%
    dplyr::mutate(sf_bag_total = fluke_bag1+fluke_bag2) %>% 
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj =dplyr:: case_when(
        sf_bag_total > 0 ~ ifelse(csum_keep<=sf_bag_total & posskeep==1,1,0),
        TRUE ~ 0)) #%>%
    #dplyr::mutate(posskeep2 = ifelse(uniform>=p_star_sf, 1,0)) %>%
    #dplyr::group_by(tripid, period2, catch_draw) #%>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    
  
  #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
  #catch_size_data[is.na(catch_size_data)] <- 0
  catch_data <- catch_data2 %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_data <- catch_data %>%
    dplyr::mutate(keep_tot = keep_adj,
                  release = ifelse(keep_adj==0, 1,0))

   catch_data<- catch_data %>%
     dplyr::select(c(fishid, tripid, keep_tot, release, period2, catch_draw, mode1, month)) %>%
     dplyr::rename(keep = keep_tot)
  
   summed_catch_data <- catch_data %>%
     dplyr::group_by(period2, catch_draw, tripid) %>%
     dplyr::summarize(tot_keep_sf = sum(keep),
                      tot_rel_sf = sum(release),
                      .groups = "drop") %>%
     dplyr::ungroup()
  ############# Length #####################################
  # catch_size_data <- catch_size_data %>%
  #   dplyr::left_join(regs, by = "period2") %>%
  #   dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min1 & fitted_length<fluke_max1,1,0)) %>%
  #   dplyr::group_by(tripid, period2, catch_draw)   %>%
  #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>% #,
  #   # keep = dplyr::case_when(
  #   #   fitted_length>=minsize & fitted_length<=maxsize ~ 1,
  #   #   TRUE ~ 0)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(
  #     keep_adj = dplyr::case_when(
  #       fluke_bag1 > 0 ~ ifelse(csum_keep<=fluke_bag1 & posskeep==1,1,0),
  #       TRUE ~ 0))  %>%
  #   
  #   dplyr::mutate(posskeep2 = ifelse(fitted_length>=fluke_min2 & fitted_length<fluke_max2,1,0)) %>%
  #   dplyr::group_by(tripid, period2, catch_draw) %>%
  #   # keep = case_when(
  #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
  #   # TRUE ~ 0),
  #   dplyr::mutate(csum_keep2 = cumsum(posskeep2)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(
  #     keep_adj2 = dplyr::case_when(
  #       fluke_bag2 > 0 ~ ifelse(csum_keep2<=fluke_bag2 & posskeep2==1,1,0),
  #       TRUE ~ 0))
  # 
  # #catch_size_data[is.na(catch_size_data)] <- 0
  # catch_size_data <- catch_size_data %>%
  #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  # 
  # catch_size_data <- catch_size_data %>%
  #   dplyr::mutate(keep_tot = keep_adj+keep_adj2,
  #                 release = ifelse(keep_adj==0 & keep_adj2==0,1,0))
  # 
  # ###### ANDREWS CODE #@######
  # catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw)) %>% 
  #   dplyr::rename(keep = keep_tot)
  # 
  # new_size_data <- catch_size_data %>%
  #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
  #   dplyr::summarize(keep = sum(keep),
  #                    release = sum(release), .groups = "drop") %>% 
  #   dplyr::ungroup()
  # 
  # summed_catch_data <- catch_data %>%
  #   dplyr::group_by(period2, catch_draw, tripid) %>%
  #   dplyr::summarize(tot_keep_sf = sum(keep),
  #                    tot_rel_sf = sum(release),
  #                    .groups = "drop") %>%
  #   dplyr::ungroup()
  # 
  # keep_size_data <- new_size_data %>%
  #   dplyr::select(-release) %>%
  #   tidyr::pivot_wider(names_from = fitted_length, #_length,
  #                      names_glue = "keep_sf_{fitted_length}",
  #                      names_sort = TRUE,
  #                      values_from = keep, 
  #                      values_fill = 0) 
  # 
  # release_size_data <- new_size_data %>%
  #   dplyr::select(-keep) %>%
  #   tidyr::pivot_wider(names_from = fitted_length, #_length,
  #                      names_glue = "release_sf_{fitted_length}",
  #                      names_sort = TRUE,
  #                      values_from = release, 
  #                      values_fill = 0) 
  # 
  # keep_release_sf <- keep_size_data %>% 
  #   dplyr::full_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
  # 
  # sf_zero_catch<- sf_zero_catch %>% ## ADD back zero catches
  #   dplyr::select(period2, tripid, catch_draw)
  # 
  # keep_release_sf <- keep_release_sf %>% 
  #   dplyr::full_join(sf_zero_catch, by = c("period2", "catch_draw", "tripid")) %>% 
  #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  # 
  
  trip_data <- summed_catch_data 
  
  # trip_test<- trip_data %>% 
  #   dplyr::filter(period2 == "6_6_fh", 
  #                 tripid == 1)
  # 
  # sf_zero_test <- sf_zero_catch %>% 
  #   dplyr::filter(period2 == "6_6_fh", 
  #                 tripid == 1)
  trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
    #arrange(period, catch_draw, tripid) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::mutate(state = state1,
                  tot_sf_catch = tot_keep_sf + tot_rel_sf) %>%
    dplyr::select(-c("tot_bsb_catch", "tot_scup_catch", "tot_cat_bsb_new", "tot_cat_scup_new"))
  
  
  if (sf_catch_check==0){
    trip_data<-sf_catch_data %>%
      dplyr::mutate(tot_keep_sf=0,
                    tot_rel_sf=0,
                    tot_sf_catch = tot_keep_sf+tot_rel_sf)
    subset(dplyr::select(-c(tot_bsb_catch, tot_scup_catch)))
    
  }
  
  
  #########################
  ###  Black sea bass  ####
  #########################
  
  
  #######Black Sea Bass
  
  if (bsb_catch_check!=0){
    # subset trips with zero catch, as no size draws are required
    bsb_zero_catch <- dplyr::filter(sf_bsb_catch_data, tot_bsb_catch == 0) %>% 
      dplyr::select(!mode1)
    
    #remove trips with zero summer flounder catch
    #sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    bsb_catch_data <- dplyr::filter(sf_bsb_catch_data, tot_bsb_catch > 0)
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow(bsb_catch_data))
    #bsb_catch_data <- bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
    
    bsb_catch_data<- bsb_catch_data %>%
      dplyr::slice(rep(row_inds,tot_bsb_catch))
    
    rownames(bsb_catch_data) <- NULL
    bsb_catch_data$fishid <- 1:nrow(bsb_catch_data)
    
    
    # # ###### P_star ####################
    # catch_size_data2 <- bsb_catch_data %>%
    #   dplyr::left_join(regs, by = "period2") %>%
    #   dplyr::mutate(uniform=runif(nrow(bsb_catch_data))) %>%
    #   dplyr::mutate(posskeep = ifelse(uniform>=p_star_bsb, 1,0)) %>%
    #   dplyr::group_by(tripid, period2, catch_draw)   %>%
    #   # keep = case_when(
    #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    #   # TRUE ~ 0),
    #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(
    #     keep_adj =dplyr:: case_when(
    #       bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
    #       TRUE ~ 0))
    # #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
    # #catch_size_data[is.na(catch_size_data)] <- 0
    # catch_size_data2 <- catch_size_data2 %>%
    #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    # 
    # catch_size_data <- catch_size_data2 %>%
    #   dplyr::mutate(keep_tot = keep_adj,
    #                 release = ifelse(keep_adj==0,1,0))
    # 
    # catch_size_data<- catch_size_data %>%
    #   dplyr::select(c(fishid, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
    #   dplyr::rename(keep = keep_adj)
    
    # 
    # # generate lengths for each fish
    bsb_data<- bsb_catch_data %>% 
      dplyr::filter(state == state1)
    
    # ################### P_Star #############################
    catch_data2 <- bsb_data %>%
      dplyr::left_join(regs, by = "period2") %>%
      dplyr::mutate(uniform=runif(nrow(bsb_catch_data))) %>%
      dplyr::mutate(posskeep = ifelse(uniform>=p_star_bsb, 1,0)) %>%
      dplyr::group_by(tripid, period2, catch_draw)   %>%
      #dplyr::mutate(sf_bag_total = bsb_bag1+fluke_bag2) %>% 
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj =dplyr:: case_when(
          bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
          TRUE ~ 0)) #%>%
    #dplyr::mutate(posskeep2 = ifelse(uniform>=p_star_sf, 1,0)) %>%
    #dplyr::group_by(tripid, period2, catch_draw) #%>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    
    
    #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
    #catch_size_data[is.na(catch_size_data)] <- 0
    catch_data <- catch_data2 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    catch_data <- catch_data %>%
      dplyr::mutate(keep_tot = keep_adj,
                    release = ifelse(keep_adj==0, 1,0))
    
    catch_data<- catch_data %>%
      dplyr::select(c(fishid, tripid, keep_tot, release, period2, catch_draw, mode1, month)) %>%
      dplyr::rename(keep = keep_tot)
    
    summed_catch_data <- catch_data %>%
      dplyr::group_by(period2, catch_draw, tripid) %>%
      dplyr::summarize(tot_keep_bsb = sum(keep),
                       tot_rel_bsb = sum(release),
                       .groups = "drop") %>%
      dplyr::ungroup()
    
    
    # catch_size_data <- bsb_catch_data %>%
    #   dplyr::mutate(fitted_length = sample(bsb_size_data$length,
    #                                        nrow(.),
    #                                        prob = bsb_size_data$fitted_prob,
    #                                        replace = TRUE)) #%>%
    # 
    # 
    # catch_size_data <- catch_size_data %>%
    #   dplyr::left_join(regs, by = "period2") %>%
    #   dplyr::mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>%
    #   dplyr::group_by(tripid, period2, catch_draw) %>%
    #   # keep = case_when(
    #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    #   # TRUE ~ 0),
    #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(
    #     keep_adj = dplyr::case_when(
    #       bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
    #       TRUE ~ 0))
    # catch_size_data <- catch_size_data %>%
    #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    # 
    # catch_size_data <- catch_size_data %>%
    #   dplyr::mutate(keep_tot = keep_adj,
    #                 release = ifelse(keep_adj==0,1,0))
    # 
    # catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw)) %>% 
    #   dplyr::rename(keep = keep_tot)
    # 
    # new_size_data <- catch_size_data %>%
    #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
    #   dplyr::summarize(keep = sum(keep),
    #                    release = sum(release), .groups = "drop") %>% 
    #   dplyr::ungroup()
    # 
    # summed_catch_data <- catch_size_data %>%
    #   dplyr::group_by(period2, catch_draw, tripid) %>%
    #   dplyr::summarize(tot_keep_bsb = sum(keep),
    #                    tot_rel_bsb = sum(release),
    #                    .groups = "drop") %>% 
    #   dplyr::ungroup()
    # 
    # keep_size_data <- new_size_data %>%
    #   dplyr::select(-release) %>%
    #   tidyr::pivot_wider(names_from = fitted_length, #_length,
    #                      names_glue = "keep_bsb_{fitted_length}",
    #                      names_sort = TRUE,
    #                      values_from = keep, 
    #                      values_fill = 0) 
    # 
    # release_size_data <- new_size_data %>%
    #   dplyr::select(-keep) %>%
    #   tidyr::pivot_wider(names_from = fitted_length, #_length,
    #                      names_glue = "release_bsb_{fitted_length}",
    #                      names_sort = TRUE,
    #                      values_from = release, 
    #                      values_fill = 0) 
    # 
    # keep_release_bsb <- keep_size_data %>% 
    #   dplyr::full_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
    # 
    
    # bsb_zero_catch2<- bsb_zero_catch %>% ## ADD back zero catches
    #   dplyr::select(period2, tripid, catch_draw)
    # 
    # keep_release_bsb <- keep_release_bsb %>% 
    #   dplyr::full_join(bsb_zero_catch2, by = c("period2", "catch_draw", "tripid")) %>% 
    #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    # 
    
    trip_data_bsb <- summed_catch_data 
    
    
    trip_data_bsb <- dplyr::bind_rows(trip_data_bsb, bsb_zero_catch) %>%
      #arrange(period, catch_draw, tripid) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::mutate(state = state1,
                    tot_bsb_catch = tot_keep_bsb + tot_rel_bsb) %>%
      dplyr::select(-c("tot_sf_catch", "tot_scup_catch", "tot_cat_scup_new", "tot_cat_bsb_new","day", "day_i", "draw", "month"))
    
    
    # print(setdiff(trip_data_bsb$period2, keep_release_bsb$period2))
    # print(setdiff(keep_release_bsb$period2, trip_data_bsb$period2))
    # 
    
    # merge the bsb trip data with the rest of the trip data
    #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))
    
    trip_data <- trip_data %>%
      dplyr::left_join(trip_data_bsb, by = c("period2", "catch_draw", "tripid", "state")) #%>%  select(-decade.x, -decade.y)
    
    # %>%
    
  }
  
  if (bsb_catch_check==0){
    trip_data$tot_bsb_catch<-0
    trip_data$tot_keep_bsb<-0
    trip_data$tot_rel_bsb<-0
  }
  
  
  
  
  ##############Scup
  
  
  if (state1 %in% c("DE", "MD", "VA")){
    trip_data$tot_scup_catch<-0
    trip_data$tot_keep_scup<-0
    trip_data$tot_rel_scup<-0
    
  }
  
  if (state1 %in% c("MA", "RI", "CT", "NY", "NJ")){
    
    
    if (scup_catch_check>0){
      # subset trips with zero catch, as no size draws are required
      scup_zero_catch <- dplyr::filter(sf_bsb_catch_data, tot_scup_catch == 0) %>%
        dplyr::select(-c("tot_sf_catch", "tot_bsb_catch", "mode1"))
      
      #remove trips with zero summer flounder catch
      scup_catch_data <- dplyr::filter(sf_bsb_catch_data, tot_scup_catch > 0)
      
      #expand the sf_catch_data so that each row represents a fish
      row_inds <- seq_len(nrow(scup_catch_data))
      #scup_catch_data <- scup_catch_data[c(rep(row_inds, scup_catch_data$tot_scup_catch)), ]
      
      
      scup_catch_data<- scup_catch_data %>%
        dplyr::slice(rep(row_inds,tot_scup_catch))
      
      
      rownames(scup_catch_data) <- NULL
      scup_catch_data$fishid <- 1:nrow(scup_catch_data)
      
      
      # generate lengths for each fish
      scup_data<- scup_catch_data %>% 
        dplyr::filter(state == state1)
      
      # ################### P_Star #############################
      catch_data2 <- scup_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(uniform=runif(nrow(scup_catch_data))) %>%
        dplyr::mutate(posskeep = ifelse(uniform>=p_star_scup, 1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw)   %>%
        #dplyr::mutate(sf_bag_total = bsb_bag1+fluke_bag2) %>% 
        # keep = case_when(
        # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
        # TRUE ~ 0),
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj =dplyr:: case_when(
            scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
            TRUE ~ 0)) #%>%
      #dplyr::mutate(posskeep2 = ifelse(uniform>=p_star_sf, 1,0)) %>%
      #dplyr::group_by(tripid, period2, catch_draw) #%>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      
      
      #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
      #catch_size_data[is.na(catch_size_data)] <- 0
      catch_data <- catch_data2 %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_data <- catch_data %>%
        dplyr::mutate(keep_tot = keep_adj,
                      release = ifelse(keep_adj==0, 1,0))
      
      catch_data<- catch_data %>%
        dplyr::select(c(fishid, tripid, keep_tot, release, period2, catch_draw, mode1, month)) %>%
        dplyr::rename(keep = keep_tot)
      
      summed_catch_data <- catch_data %>%
        dplyr::group_by(period2, catch_draw, tripid) %>%
        dplyr::summarize(tot_keep_scup = sum(keep),
                         tot_rel_scup = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      
      # catch_size_data <- scup_catch_data %>%
      #   dplyr::mutate(fitted_length = sample(scup_size_data$length,
      #                                        nrow(.),
      #                                        prob = scup_size_data$fitted_prob,
      #                                        replace = TRUE)) #%>%
      # 
      # catch_size_data <- catch_size_data %>%
      #   dplyr::left_join(regs, by = "period2") %>%
      #   dplyr::mutate(uniform=runif(nrow(catch_size_data))) %>%
      #   dplyr::mutate(posskeep = ifelse(uniform>=p_star_scup, 1,0)) %>%
      #   dplyr::group_by(tripid, period2, catch_draw) %>%
      #   # keep = case_when(
      #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      #   # TRUE ~ 0),
      #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::mutate(
      #     keep_adj = dplyr::case_when(
      #       scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
      #       TRUE ~ 0))
      # #,
      # # keep_adj = case_when(
      # #   csum_keep<=bag & keep==1 ~ 1,
      # #   TRUE ~ 0),
      # #release = case_when(
      # # scup_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>scup_bag ), 1,0)))
      # 
      # #catch_size_data[is.na(catch_size_data)] <- 0
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      # 
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate(release = ifelse(keep_adj==0,1,0))
      # 
      # #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
      # 
      # catch_size_data<- catch_size_data %>%
      #   dplyr::select(c(fishid, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
      #   dplyr::rename(keep = keep_adj)
      
      
      
      # catch_size_data <- catch_size_data %>%
      #   dplyr::left_join(regs, by = "period2") %>%
      #   dplyr::mutate(posskeep = ifelse(fitted_length>=scup_min ,1,0)) %>%
      #   dplyr::group_by(tripid, period2, catch_draw) %>%
      #   # keep = case_when(
      #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      #   # TRUE ~ 0),
      #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::mutate(
      #     keep_adj = dplyr::case_when(
      #       scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
      #       TRUE ~ 0))
      # #,
      # # keep_adj = case_when(
      # #   csum_keep<=bag & keep==1 ~ 1,
      # #   TRUE ~ 0),
      # #release = case_when(
      # # scup_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>scup_bag ), 1,0)))
      # 
      # #catch_size_data[is.na(catch_size_data)] <- 0
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      # 
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate(keep_tot = keep_adj, 
      #                 release = ifelse(keep_adj==0,1,0))
      # 
      # #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
      # 
      # catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw)) %>%
      #   dplyr::rename(keep = keep_tot)
      # 
      # 
      # 
      # new_size_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      #   dplyr::summarize(keep = sum(keep),
      #                    release = sum(release), .groups = "drop") %>% 
      #   dplyr::ungroup()
      # 
      # summed_catch_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid) %>%
      #   dplyr::summarize(tot_keep_scup = sum(keep),
      #                    tot_rel_scup = sum(release),
      #                    .groups = "drop") %>% 
      #   dplyr::ungroup()
      # 
      # keep_size_data <- new_size_data %>%
      #   dplyr::select(-release) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "keep_scup_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = keep, 
      #                      values_fill = 0) 
      # 
      # release_size_data <- new_size_data %>%
      #   dplyr::select(-keep) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "release_scup_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = release, 
      #                      values_fill = 0) 
      # 
      # keep_release_scup <- keep_size_data %>% 
      #   dplyr::full_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
      # 
      # 
      # scup_zero_catch2<- scup_zero_catch %>% ## ADD back zero catches
      #   dplyr::select(period2, tripid, catch_draw)
      # 
      # keep_release_scup <- keep_release_scup %>% 
      #   dplyr::full_join(scup_zero_catch2, by = c("period2", "catch_draw", "tripid")) %>% 
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      # 
      
      trip_data_scup <- summed_catch_data 
      
      
      trip_data_scup <- dplyr::bind_rows(trip_data_scup, scup_zero_catch) %>%
        #arrange(period, catch_draw, tripid) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::mutate(state = state1,
                      tot_scup_catch = tot_keep_scup + tot_rel_scup) #%>%
        #dplyr::select(-c("tot_sf_catch", "tot_bsb_catch", "day", "day_i", "draw", "month"))
        #dplyr::select(-c("day", "day_i", "draw", "month"))
      
      
     
      
      
      # merge the scup trip data with the rest of the trip data
      #trip_data <-  merge(trip_data,trip_data_scup,by=c("period2", "catch_draw", "tripid", "state", "mode", "month"))
      
      trip_data <- trip_data %>%
        dplyr::left_join(trip_data_scup, by = c("period2","tripid",  "catch_draw", "state")) %>% 
        dplyr::select(!c(month.x, day.x, day_i.x, draw.x, 
                         month.y, day.y, day_i.y, draw.y, 
                         tot_cat_scup_new, tot_cat_bsb_new, 
                         tot_sf_catch, tot_bsb_catch, tot_scup_catch)) %>% 
        dplyr::mutate(tot_scup_catch = tot_keep_scup + tot_rel_scup, 
                      tot_bsb_catch = tot_keep_bsb + tot_rel_bsb, 
                      tot_sf_catch = tot_keep_sf + tot_rel_sf)
      
      # trip_data  <- setcolorder(trip_data,c("state", "period2", "mode1", "month1", "tripid", "catch_draw",
      #                                       "tot_sf_catch", "tot_keep_sf", "tot_rel_sf","tot_bsb_catch", "tot_keep_bsb",
      #                                       "tot_rel_bsb","tot_scup_catch","tot_keep_scup", "tot_rel_scup"))
      
      
      
      
      
    }
    if (scup_catch_check==0){
      trip_data$tot_scup_catch<-0
      trip_data$tot_keep_scup<-0
      trip_data$tot_rel_scup<-0
    }
    
  }
  
  
  # length_data <- keep_release_sf %>% 
  #   dplyr::full_join(keep_release_bsb, by = c("period2","tripid", "catch_draw")) %>% 
  #   dplyr::full_join(keep_release_scup, by = c("period2","tripid", "catch_draw"))
  # 
  
  period_vec1 <- period_vec %>%
    dplyr::mutate(beta_sqrt_sf_keep= rnorm(nrow(period_vec), mean = 0.827, sd = 1.267), 
           beta_sqrt_sf_release = rnorm(nrow(period_vec), mean = 0.065 , sd = 0.325) , 
           beta_sqrt_bsb_keep = rnorm(nrow(period_vec), mean = 0.353, sd = 0.129), 
           beta_sqrt_bsb_release = rnorm(nrow(period_vec), mean = 0.074 , sd = 0), 
           beta_sqrt_sf_bsb_keep = rnorm(nrow(period_vec), mean=-0.056  , sd = 0.196 ), 
           beta_sqrt_scup_catch = rnorm(nrow(period_vec), mean = 0.018 , sd = 0), 
           beta_opt_out = rnorm(nrow(period_vec), mean =-2.056 , sd = 1.977), 
           beta_opt_out_avidity = rnorm(nrow(period_vec), mean =-0.010 , sd = 0), 
           beta_opt_out_age = rnorm(nrow(period_vec), mean =0.010 , sd = 0), 
           beta_cost = -0.012) %>%
    dplyr::group_by(period2) %>% dplyr::mutate(tripid = dplyr::row_number(period2))
  
  trip_data<- trip_data %>%
    dplyr::left_join(period_vec1, by = c("period2","tripid"))
  
  trip_costs<-data.frame(readr::read_csv(file.path(here::here("data-raw/trip_costs_state_summary.csv")), show_col_types = FALSE)) %>% 
    dplyr::filter(state==state1, 
                  mode == select_mode) %>% 
    dplyr::rename(mode1 = mode)
  
  trip_data <- trip_data %>%
    dplyr::left_join(trip_costs)
  
  trip_data <- trip_data %>%
    dplyr::mutate(cost=rnorm(nrow(trip_data), mean=trip_data$mean, sd=trip_data$st_error))
  
  # trip_data <- trip_data %>%
  #   dplyr::select(-state.x, -state.y)
  
  trip_data <- trip_data %>%
    dplyr::mutate(state=state1)
  
  #import age and avidity distribution and assign each trip an age and avidity
  
  period_vec2<- period_vec %>% dplyr::select(-month) %>%
    dplyr::mutate(period2=as.factor(period2)) 
  
  demographics0<-list()
  
  levels(period_vec2$period2)
  for(p in levels(period_vec2$period2)){
    #p<-"10_bt"
    
    #Ages 
    age_distn <- data.frame(read.csv(file.path(here::here("data-raw/age_distribution_by_state.csv")))) %>%
      dplyr::filter(state == "NJ")  
    
    #next two commands ensure there are enough observations  per period
    expand_rows=round((n_drawz/nrow(age_distn)))+1
    
    age_distn <- age_distn %>%
      dplyr::slice(rep(1:dplyr::n(), each = expand_rows))   
    
    #now need to assign tripid in order to merge to trip data
    age_distn <- age_distn %>%  dplyr::slice_sample(n = n_drawz) %>% 
      dplyr::mutate(period2=p, 
                    tripid = 1:n_drawz) 
    
    #Avidities
    avid_distn <- data.frame(read.csv(file.path(here::here("data-raw/avidity_distribution_by_state.csv")))) %>%
      dplyr::filter(state == "NJ")  
    
    #next two commands ensure there are enough observations per period
    expand_rows=round(n_drawz/nrow(avid_distn))+1
    
    avid_distn <- avid_distn %>%
      dplyr::slice(rep(1:dplyr::n(), each = expand_rows))   
    
    #now need to assign tripid in order to merge to trip data
    avid_distn <- avid_distn %>%  dplyr::slice_sample(n = n_drawz) %>% 
      dplyr::mutate(period2=p, 
                    tripid = 1:n_drawz) 
    
    avid_distn<- avid_distn %>%
      dplyr::left_join(age_distn, by = c("tripid", "state", "period2"))
    
    
    demographics0[[p]]=avid_distn
    
  }
  
  demographics<- as.data.frame(do.call(rbind, demographics0))
  
  #now merge the ages and avidities to the trip data
  
  trip_data<- trip_data %>%
    dplyr::left_join(demographics, by = c("tripid", "state", "period2"))
  
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario.
  # We will merge these data to the prediction year outcomes to calculate changes in CS.
  costs_new_all <- trip_data %>%
    dplyr::select(c(tripid, cost, catch_draw, tot_keep_sf, tot_rel_sf,
                    age, days_fished, beta_opt_out_age, beta_opt_out_avidity,
                    tot_keep_bsb,tot_rel_bsb,tot_scup_catch, beta_cost, beta_opt_out, beta_sqrt_bsb_keep,
                    beta_sqrt_bsb_release, beta_sqrt_scup_catch, beta_sqrt_sf_bsb_keep,
                    beta_sqrt_sf_keep, beta_sqrt_sf_release, state, period2)) %>%
    dplyr::rename(tot_keep_sf_base = tot_keep_sf,
                  tot_rel_sf_base = tot_rel_sf,
                  tot_keep_bsb_base = tot_keep_bsb,
                  tot_rel_bsb_base = tot_rel_bsb,
                  tot_cat_scup_base = tot_scup_catch)%>% 
    dplyr::mutate(n_cal_draw = k)
  
  
  
  # trip_data <- trip_data %>%
  #   dplyr::select(-month.x, -month.y)
  #trip_data <- trip_data  %>%  dplyr::arrange(period2, tripid, catch_draw)
  
  
  #  utility (prediction year)
  trip_data <-trip_data %>%
    dplyr::mutate(
      vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf) +
        beta_sqrt_sf_release*sqrt(tot_rel_sf) +
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +
        beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf)*sqrt(tot_keep_bsb)) +
        beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
        beta_cost*cost)
  
  trip_data <- trip_data %>%
    dplyr::mutate(period = as.numeric(as.factor(period2)))
  
  trip_test<- trip_data %>% 
    dplyr::filter(period2 == "6_6_fh", 
                  tripid == 1)
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
  
  mean_trip_data <- trip_data %>%
    dplyr::select(-c("state", "mode1")) %>% data.table::data.table() #%>% dplyr::arrange(period, tripid, catch_draw)
  
  #mean_trip_data<-mean_trip_data %>% dplyr::arrange(period, tripid, catch_draw)
  
  #New code to calculate probability of each choice occasion
  
  # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
    tidyr::uncount(n_alt) %>%
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                  opt_out = ifelse(alt == 2, 1, 0))
  
  
  #Caluculate the expected utility of alts 2 parameters of the utility function,
  #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  # mean_trip_data<-mean_trip_data %>%
  #   mutate(vA_optout= beta_opt_out*opt_out,
  #          v0_optout= beta_opt_out*opt_out,
  #          expon_vA= case_when(alt==1 ~ exp(vA),
  #                                alt==2 ~ exp(vA_optout)),
  #          expon_v0= case_when(alt==1 ~ exp(v0),
  #                                alt==2 ~ exp(v0_optout)))
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, vA_optout := beta_opt_out*opt_out+
        beta_opt_out_age*age + beta_opt_out_avidity*days_fished] %>%
    .[alt==1, expon_vA := exp(vA)] %>%
    .[alt==2, expon_vA := exp(vA_optout)]
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, vA_col_sum := sum(expon_vA), by=list(period, catch_draw, tripid)]
  
  
  #
  # mean_trip_data1 <- mean_trip_data %>%
  #   mutate(change_CS =(1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum)),
  #          probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum)
  #
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, probA :=expon_vA/vA_col_sum]
  
  
  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>%
  #   tibble()
  
  
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid")]
  
  
  
  mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period2","tripid"), .SDcols = all_vars]
  
  
  
  # Get rid of things we don't need.
  mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost, mean, st_error,
                                                            catch_draw,  expon_vA,
                                                            opt_out, vA, vA_optout, vA_col_sum))
  
  # Multiply the average trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
  list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid"
                                         & colnames(mean_trip_data) !="period2"
                                         & colnames(mean_trip_data) !="probA"]
  
  # for (l in list_names){
  #   mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
  # }
  
  
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
    .[]
  
  # all_vars<-c()
  # all_vars <- names(length_data)[!names(length_data) %in% c("period2","tripid", "catch_draw" )]
  # length_data<- length_data %>% 
  #   data.table::data.table() %>% 
  #   .[,lapply(.SD, base::mean), by = c("period2","tripid"), .SDcols = all_vars]
  # 
  # length_data[is.na(length_data)] <- 0
  # 
  # length_data2<- mean_trip_data %>%
  #   dplyr::select(period2, tripid, probA) %>%
  #   dplyr::full_join(length_data, by = c("period2", "tripid")) #%>% 
  # #dplyr::select(!c(month.x.x, month.y.x, month.x.y, month.y.y)) 
  # length_data2[is.na(length_data2)] <- 0
  # all_vars<-c()
  # all_vars <- names(length_data2)[!names(length_data2) %in% c("period2","tripid", "mode1" )]
  # 
  # length_data3 <- length_data2 %>% ## ADD mean_kr_total for each species by tripid and perdiod2
  #   data.table::as.data.table()  %>% 
  #   .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = all_vars] %>%
  #   .[]
  # 
  # 
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate( n_choice_occasions = rep(1,nrow(.))) %>%
    dplyr::left_join(period_names, by = c("period2"))
  
  # mean_trip_data <- mean_trip_data %>%
  #   dplyr::select(-c("period"))
  # 
  
  #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in
  #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
  #calibration_data <- calibration_data  %>%   rename(period2 = period)
  
  sims <- directed_trips_p %>%
    dplyr::select(c(dtrip, period2))
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::left_join(sims, by="period2")
  
  
  mean_probs<-mean_trip_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period2"), .SDcols = "probA"]
  
  mean_probs<-mean_probs  %>%
    dplyr::rename(mean_prob=probA)
  
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::left_join(mean_probs, by="period2")
  
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(sims=dtrip/mean_prob,
                  expand=sims/n_drawz)
  
  
  
  list_names = c("tot_bsb_catch",  "tot_keep_bsb", "tot_rel_bsb",
                 "tot_scup_catch", "tot_keep_scup", "tot_rel_scup",
                 "tot_sf_catch", "tot_keep_sf", "tot_rel_sf",
                 "cost", "probA","n_choice_occasions" )
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * expand), .SDcols = list_names] %>%
    .[]
  
  # length_expand <- mean_trip_data %>%
  #   dplyr::select(period2, tripid, expand) %>%
  #   dplyr::full_join(length_data3, by = c("period2", "tripid")) %>% 
  #   dplyr::select(-probA)
  # 
  # 
  # all_vars<-c()
  # all_vars <- names(length_expand)[!names(length_expand) %in% c("period2","tripid", "mode1", "expand")]
  # 
  # ## Move to outside function 
  # length_expand <- length_expand %>% 
  #   data.table::as.data.table() %>%
  #   .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(expand)), .SDcols = all_vars] %>%
  #   .[]
  
  
  # list_names = names(length_expand)[!names(length_expand) %in% c("period2","tripid", "mode1", "expand")]
  # 
  # aggragate_length_data<- length_expand %>% ### Sum across tirpid
  #   data.table::as.data.table() %>%
  #   .[,lapply(.SD, sum), by =c("period2"), .SDcols = list_names]
  # 
  list_names = c("n_choice_occasions", "probA", "tot_bsb_catch","tot_keep_bsb", "tot_keep_scup" , "tot_keep_sf",
                 "tot_rel_bsb" ,   "tot_rel_scup","tot_rel_sf","tot_scup_catch","tot_sf_catch")
  
  aggregate_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period2"), .SDcols = list_names]
  
  names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "estimated_trips"
  pds_new_all<-aggregate_trip_data %>%
    #dplyr::left_join(aggragate_length_data, by = "period2") %>% 
    dplyr::mutate(state=state1, 
                  n_cal_draw = k)
  
  output<-list(pds_new_all, costs_new_all)

  
  #return(output)
  
  ## Calucate_Pstars
  MRIP_data <-  read.csv(here::here("data-raw/total AB1B2 by state mode_pstar.csv")) %>% 
    dplyr::filter(state=="NJ", 
                  mode == select_mode) 
  
  
  
  
  ##SF
  sum(pds_new_all$tot_keep_sf)
  sum(MRIP_data$sf_harvest)
  sf_harvest_harv_diff<-((sum(MRIP_data$sf_harvest)-sum(pds_new_all$tot_keep_sf))/sum(MRIP_data$sf_harvest))*100
  sf_harvest_harv_diff
  
  sum(pds_new_all$tot_rel_sf)
  sum(MRIP_data$sf_releases)
  sf_rel_diff<- ((sum(MRIP_data$sf_releases)-sum(pds_new_all$tot_rel_sf))/sum(MRIP_data$sf_releases))*100
  sf_rel_diff
  
  sum(pds_new_all$tot_sf_catch)
  sum(MRIP_data$sf_tot_cat)
  sf_tot_cat_diff<-((sum(MRIP_data$sf_tot_cat)-sum(pds_new_all$tot_sf_catch))/sum(MRIP_data$sf_tot_cat))*100
  sf_tot_cat_diff
  
  
  
  ##BSB
  sum(pds_new_all$tot_keep_bsb)
  sum(MRIP_data$bsb_harvest)
  bsb_harvest_harv_diff<-((sum(MRIP_data$bsb_harvest)-sum(pds_new_all$tot_keep_bsb))/sum(MRIP_data$bsb_harvest))*100
  bsb_harvest_harv_diff
  
  sum(pds_new_all$tot_rel_bsb)
  sum(MRIP_data$bsb_releases)
  bsb_rel_diff<- ((sum(MRIP_data$bsb_releases)-sum(pds_new_all$tot_rel_bsb))/sum(MRIP_data$bsb_releases))*100
  bsb_rel_diff
  
  sum(pds_new_all$tot_bsb_catch)
  sum(MRIP_data$bsb_tot_cat)
  bsb_tot_cat_diff<-((sum(MRIP_data$bsb_tot_cat)-sum(pds_new_all$tot_bsb_catch))/sum(MRIP_data$bsb_tot_cat))*100
  bsb_tot_cat_diff
  
  
  
  
  
  ##scup
  sum(pds_new_all$tot_keep_scup)
  sum(MRIP_data$scup_harvest)
  scup_harvest_harv_diff<-((sum(MRIP_data$scup_harvest)-sum(pds_new_all$tot_keep_scup))/sum(MRIP_data$scup_harvest))*100
  scup_harvest_harv_diff
  
  sum(pds_new_all$tot_rel_scup)
  sum(MRIP_data$scup_releases)
  scup_rel_diff<- ((sum(MRIP_data$scup_releases)-sum(pds_new_all$tot_rel_scup))/sum(MRIP_data$scup_releases))*100
  
  sum(pds_new_all$tot_scup_catch)
  sum(MRIP_data$scup_tot_cat)
  scup_tot_cat_diff<-((sum(MRIP_data$scup_tot_cat)-sum(pds_new_all$tot_scup_catch))/sum(MRIP_data$scup_tot_cat))*100
  
  
  p_stars <- data.frame(species = c("SF", "BSB", "SCUP"), 
                        state = state1,
                        p_star_value = c(p_star_sf,p_star_bsb,p_star_scup), 
                        mode = c(select_mode, select_mode, select_mode), 
                        tot_keep_model = c(sum(pds_new_all$tot_keep_sf), sum(pds_new_all$tot_keep_bsb), sum(pds_new_all$tot_keep_scup)),
                        tot_rel_model = c(sum(pds_new_all$tot_rel_sf), sum(pds_new_all$tot_rel_bsb), sum(pds_new_all$tot_rel_scup)),
                        tot_catch_model = c(sum(pds_new_all$tot_sf_catch), sum(pds_new_all$tot_bsb_catch), sum(pds_new_all$tot_scup_catch)),
                        hervest_MRIP = c(sum(MRIP_data$sf_harvest), sum(MRIP_data$bsb_harvest), sum(MRIP_data$scup_harvest)),
                        harvest_diff = c(sf_harvest_harv_diff, bsb_harvest_harv_diff, scup_harvest_harv_diff), 
                        rel_diff = c(sf_rel_diff, bsb_rel_diff, scup_rel_diff), 
                        tot_cat_diff = c(sf_tot_cat_diff,bsb_tot_cat_diff,scup_tot_cat_diff), 
                        run_number = k)
  return(p_stars)
}
  # return(pds_new_all)
  # return(costs_new_all)
  



