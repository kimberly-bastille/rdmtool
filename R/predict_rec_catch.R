
# state1 = c("NJ")
# calibration_data_table = c(list(calibration_data_table_base[[1]]))
# directed_trips_table = directed_trips2
# sf_size_data_read = c(list(sf_size_data_read_base[[5]]))
# bsb_size_data_read = c(list(bsb_size_data_read_base[[5]]))
# scup_size_data_read = c(list(scup_size_data_read_base[[5]]))
# costs_new_all = c(list(cost_files_all_base[[1]]))
# sf_catch_data_all = c(list(catch_files_NJ))
# n_drawz = 50
# n_catch_draws = 30
# eff_seed=190



predict_rec_catch <- function(state1,
                              calibration_data_table,
                              directed_trips_table = directed_trips,
                              sf_size_data_read,
                              bsb_size_data_read,
                              scup_size_data_read,
                              costs_new_all,
                              sf_catch_data_all, 
                              n_drawz = 50, 
                              n_catch_draws = 30, 
                              eff_seed=190){
  

  set.seed(eff_seed)
  # Input the calibration output which contains the number of choice occasions needed to simulate
  calibration_data <- calibration_data_table[[1]] %>% tibble::tibble() 
  
  print("pre-rename")
  # Input regul
  #directed_trips <- directed_trips_table[[1]] %>% tibble::tibble() %>% dplyr::filter(state == state1) 
  sf_size_data <- sf_size_data_read[[1]] #%>%  dplyr::rename(fitted_prob = prob_star) %>% dplyr::filter(state == state1)
  bsb_size_data <- bsb_size_data_read[[1]]  #%>%  dplyr::rename(fitted_prob = prob_star) %>% dplyr::filter(state == state1)
  scup_size_data <- scup_size_data_read[[1]]  #%>%  dplyr::rename(fitted_prob = prob_star) %>% dplyr::filter(state == state1)
  
  print("out of rename")
  ######################################
  ##   Begin simulating trip outcomes ##
  ######################################
  print("into directed trips")
  # Set up an output file for the separately simulated within-season regulatory periods
  directed_trips_p <- directed_trips_table %>% #subset(directed_trips, period == p) %>% 
    #dplyr::mutate(period2 = as.character(paste0(month_day,"-", mode)))%>%
    #group_by(period) %>%
    dplyr::mutate(#n_trips = floor(mean(dtrip_2019)),
      n_trips = floor(dtrip),
      n_draws = n_drawz)
  
  print("first kod")
  period_vec <- directed_trips_p %>%
    dplyr::select(period2, n_draws, month, kod, kod_24) %>%
    tidyr::uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))
  
  regs <- tibble::tibble( directed_trips_p ) %>%
    dplyr::select(period2,
                  fluke_bag1, fluke_min1, fluke_max1,
                  fluke_bag2, fluke_min2, fluke_max2,
                  bsb_bag,
                  bsb_min,
                  scup_bag,
                  scup_min )
  

  sf_catch_data <- sf_catch_data_all[[1]]  

  print("premutate")
  print(class(sf_catch_data))
  sf_catch_data <- sf_catch_data %>%
    #dplyr::mutate(period2 = paste0(month, "-", day, "-", mode1)) %>% 
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
  sf_zero_catch <- dplyr::filter(sf_catch_data, tot_sf_catch == 0)
  
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
  
  
  # generate lengths for each fish
  catch_size_data <- sf_catch_data %>%
    dplyr::mutate(fitted_length = sample(sf_size_data$fitted_length,
                                         nrow(.),
                                         prob = sf_size_data$fitted_prob,
                                         replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
  
  ##I()
  
  # Impose regulations, calculate keep and release per trip
  # For summer flounder, retain keep- and release-at-length
  ####### Start Here #################
  
  ################### P_Star #############################
  catch_size_data2 <- catch_size_data %>%
    dplyr::left_join(regs, by = "period2") %>%
    dplyr::mutate(uniform=runif(nrow(sf_catch_data))) %>%
    dplyr::mutate(posskeep = ifelse(uniform>=p_star_sf, 1,0)) %>%
    dplyr::group_by(tripid, period2, catch_draw)   %>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj =dplyr:: case_when(
        fluke_bag1 > 0 ~ ifelse(csum_keep<=fluke_bag1 & posskeep==1,1,0),
        TRUE ~ 0))
  #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
  #catch_size_data[is.na(catch_size_data)] <- 0
  catch_size_data2 <- catch_size_data2 %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

  catch_size_data <- catch_size_data2 %>%
    dplyr::mutate(keep_tot = keep_adj,
                  release = ifelse(keep_adj==0,1,0))

   catch_size_data<- catch_size_data %>% 
     dplyr::select(c(fishid, tripid, keep_tot, release, period2, catch_draw, mode1, month)) %>%
     dplyr::rename(keep = keep_tot)
   
  # ############# Length #####################################
  #   catch_size_data2 <- catch_size_data %>%
  #   dplyr::left_join(regs, by = "period2") %>%
  #   dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min1 & fitted_length<fluke_max1,1,0)) %>%
  #   dplyr::group_by(tripid, period2, catch_draw)   %>%
  #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>% #,
  #                 # keep = dplyr::case_when(
  #                 #   fitted_length>=minsize & fitted_length<=maxsize ~ 1,
  #                 #   TRUE ~ 0)) %>%
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
  # catch_size_data <- catch_size_data2 %>%
  #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  # 
  # catch_size_data <- catch_size_data %>%
  #   dplyr::mutate(keep_tot = keep_adj+keep_adj2,
  #                 release = ifelse(keep_adj==0 & keep_adj2==0,1,0))
  # 
  # 
  # 
  # catch_size_data<- catch_size_data %>% 
  #   dplyr::select(c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw, mode1, month)) %>%
  #   dplyr::rename(keep = keep_tot)
  
  summed_catch_data <- catch_size_data %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, base::sum), by =c("period2", "catch_draw", "tripid",  "mode1", "month"), .SDcols = c("keep", "release")]
  #print(head(summed_catch_data))
  summed_catch_data <- summed_catch_data %>%
    dplyr::rename(tot_keep_sf = keep,
                  tot_rel_sf = release)
  
  #Uncomment this if you want sizes of fish - Gives Number of fish at each lenght in each trip_id and catch_draw
  # new_size_data <- catch_size_data %>%
  #   dplyr::group_by(period2, catch_draw, tripid, fitted_length, mode1, month) %>%
  #   dplyr::summarize(keep = sum(keep),
  #                    release = sum(release), .groups = "drop") #%>%    dplyr::arrange(tripid, period2,  catch_draw)
  # 
  # 
  # 
  # 
  # 
  # #The following code will retain the length of fish kept and released.
  # #This takes up a lot of memory, so for now will comment out these lines.
  # keep_size_data_sf <- new_size_data %>%
  #   dplyr::select(-release) %>%
  #   dplyr::group_by(period2, tripid, fitted_length, catch_draw, mode1) %>%
  #   dplyr::summarise(Number = sum(keep)) %>%
  #   dplyr::group_by(period2, tripid, fitted_length) %>%
  #   dplyr::mutate( AvgNum = Number/n_catch_draws,
  #                    Species = "SF",
  #                    Keep_Release = "Keep") %>%
  #   dplyr::select(-catch_draw)
  # 
  #   keep_size_data_sf <- keep_size_data_sf[!duplicated(keep_size_data_sf),]
  # 
  # 
  #   release_size_data_sf <- new_size_data %>%
  #     dplyr::select(-keep) %>%
  #     dplyr::group_by(period2, tripid, fitted_length, catch_draw, mode1) %>%
  #     dplyr::summarise(Number = sum(release)) %>%
  #     dplyr::group_by(period2, tripid, fitted_length) %>%
  #     dplyr::mutate( AvgNum = Number/n_catch_draws,
  #                    Species = "SF",
  #                    Keep_Release = "Release") %>%
  #     dplyr::select(-catch_draw)
  # 
  #   release_size_data_sf <- release_size_data_sf[!duplicated(release_size_data_sf),]
  # 
  # 
  # length_data_sf <- rbind(keep_size_data_sf, release_size_data_sf)
  
  
  #end retaining sizes of fish kept and released
  
  trip_data<-summed_catch_data
  #add the zero catch trips
  
  trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
    #arrange(period, catch_draw, tripid) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::mutate(state = state1,
                  tot_sf_catch = tot_keep_sf + tot_rel_sf) %>%
    dplyr::select(-c("tot_bsb_catch", "tot_scup_catch", "month_day"))
  
  
  
  #######Black Sea Bass
  
  if (bsb_catch_check!=0){
    # subset trips with zero catch, as no size draws are required
    bsb_zero_catch <- dplyr::filter(sf_bsb_catch_data, tot_bsb_catch == 0)
    
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
    
    
    # ###### P_star ####################
    catch_size_data2 <- bsb_catch_data %>%
      dplyr::left_join(regs, by = "period2") %>%
      dplyr::mutate(uniform=runif(nrow(bsb_catch_data))) %>%
      dplyr::mutate(posskeep = ifelse(uniform>=p_star_bsb, 1,0)) %>%
      dplyr::group_by(tripid, period2, catch_draw)   %>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj =dplyr:: case_when(
          bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
          TRUE ~ 0))
    #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
    #catch_size_data[is.na(catch_size_data)] <- 0
    catch_size_data2 <- catch_size_data2 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

    catch_size_data <- catch_size_data2 %>%
      dplyr::mutate(keep_tot = keep_adj,
                    release = ifelse(keep_adj==0,1,0))
    
    catch_size_data<- catch_size_data %>%
      dplyr::select(c(fishid, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
      dplyr::rename(keep = keep_adj)
    
    # 
    # # generate lengths for each fish
    # catch_size_data <- bsb_catch_data %>%
    #   dplyr::mutate(fitted_length = sample(bsb_size_data$fitted_length,
    #                                        nrow(.),
    #                                        prob = bsb_size_data$fitted_prob,
    #                                        replace = TRUE)) #%>%
    # 
    # 
    # catch_size_data2 <- catch_size_data %>%
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
    # catch_size_data2 <- catch_size_data2 %>%
    #     dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    # 
    # catch_size_data <- catch_size_data2 %>%
    #     dplyr::mutate(keep_tot = keep_adj,
    #                   release = ifelse(keep_adj==0,1,0))
    # 
    # catch_size_data<- catch_size_data %>%
    #   dplyr::select(c(fishid, fitted_length, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
    #   dplyr::rename(keep = keep_adj)

    # new_size_data <- catch_size_data %>%
    #   dplyr::group_by(period2, catch_draw, tripid, fitted_length, mode1, month) %>%
    #   dplyr::summarize(keep = sum(keep),
    #                    release = sum(release), .groups = "drop")
    # #Uncomment this if you want sizes of fish
    # keep_size_data_bsb <- new_size_data %>%
    #   dplyr::select(-release) %>%
    #   dplyr::group_by(period2, tripid, fitted_length, catch_draw, mode1) %>%
    #   dplyr::summarise(Number = sum(keep)) %>%
    #   dplyr::group_by(period2, tripid, fitted_length) %>%
    #   dplyr::mutate( AvgNum = Number/n_catch_draws,
    #                  Species = "BSB",
    #                  Keep_Release = "Keep") %>%
    #   dplyr::select(-catch_draw)
    # 
    # keep_size_data_bsb <- keep_size_data_bsb[!duplicated(keep_size_data_bsb),]
    # 
    # 
    # release_size_data_bsb <- new_size_data %>%
    #   dplyr::select(-keep) %>%
    #   dplyr::group_by(period2, tripid, fitted_length, catch_draw, mode1) %>%
    #   dplyr::summarise(Number = sum(release)) %>%
    #   dplyr::group_by(period2, tripid, fitted_length) %>%
    #   dplyr::mutate( AvgNum = Number/n_catch_draws,
    #                  Species = "BSB",
    #                  Keep_Release = "Release") %>%
    #   dplyr::select(-catch_draw)
    # 
    # release_size_data_bsb <- release_size_data_bsb[!duplicated(release_size_data_bsb),]
    # 
    # 
    # length_data_bsb <- rbind(keep_size_data_bsb, release_size_data_bsb)
    
    summed_catch_data <- catch_size_data %>%
      data.table::as.data.table() %>%
      .[,lapply(.SD, base::sum), by =c("period2", "catch_draw", "tripid",  "mode1", "month"), .SDcols = c("keep", "release")]
    
    summed_catch_data <- summed_catch_data %>%
      dplyr::rename(tot_keep_bsb = keep,
                    tot_rel_bsb = release)
    
    trip_data_bsb<-summed_catch_data
    #add the zero catch trips
    trip_data_bsb <- dplyr::bind_rows(trip_data_bsb, bsb_zero_catch) %>%
      #arrange(period, catch_draw, tripid) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::mutate(state = state1,
                    tot_bsb_catch = tot_keep_bsb + tot_rel_bsb)  %>%
      dplyr::select(-c("tot_sf_catch", "tot_scup_catch", "month_day"))
    
    
    # merge the bsb trip data with the rest of the trip data
    #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))
    
    trip_data <- trip_data %>%
      dplyr::left_join(trip_data_bsb, by = c("period2", "catch_draw", "tripid", "state", "mode1", "month" )) #%>%  select(-decade.x, -decade.y)
    
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
        dplyr::select(-c("tot_sf_catch", "tot_bsb_catch"))
      
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
      catch_size_data <- scup_catch_data %>%
        dplyr::mutate(fitted_length = sample(scup_size_data$fitted_length,
                                             nrow(.),
                                             prob = scup_size_data$fitted_prob,
                                             replace = TRUE)) #%>%

      catch_size_data <- catch_size_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(uniform=runif(nrow(catch_size_data))) %>%
        dplyr::mutate(posskeep = ifelse(uniform>=p_star_scup, 1,0)) %>%
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
      #,
      # keep_adj = case_when(
      #   csum_keep<=bag & keep==1 ~ 1,
      #   TRUE ~ 0),
      #release = case_when(
      # scup_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>scup_bag ), 1,0)))

      #catch_size_data[is.na(catch_size_data)] <- 0
      catch_size_data <- catch_size_data %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

      catch_size_data <- catch_size_data %>%
        dplyr::mutate(release = ifelse(keep_adj==0,1,0))

      #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)

      catch_size_data<- catch_size_data %>%
        dplyr::select(c(fishid, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
        dplyr::rename(keep = keep_adj)
      
      
      
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
      #   dplyr::mutate(release = ifelse(keep_adj==0,1,0))
      # 
      # #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
      # 
      # catch_size_data<- catch_size_data %>%
      #   dplyr::select(c(fishid, fitted_length, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
      #   dplyr::rename(keep = keep_adj)
      # 
      # new_size_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid, fitted_length, mode1, month) %>%
      #   dplyr::summarize(keep = sum(keep),
      #                    release = sum(release), .groups = "drop")
      # 
      # #Uncomment this if you want sizes of fish
      # keep_size_data_scup <- new_size_data %>%
      #   dplyr::select(-release) %>%
      #   dplyr::group_by(period2, tripid, fitted_length, catch_draw, mode1) %>%
      #   dplyr::summarise(Number = sum(keep)) %>%
      #   dplyr::group_by(period2, tripid, fitted_length) %>%
      #   dplyr::mutate( AvgNum = Number/n_catch_draws,
      #                  Species = "SCUP",
      #                  Keep_Release = "Keep") %>%
      #   dplyr::select(-catch_draw)
      # 
      # keep_size_data_scup <- keep_size_data_scup[!duplicated(keep_size_data_scup),]
      # 
      # 
      # release_size_data_scup <- new_size_data %>%
      #   dplyr::select(-keep) %>%
      #   dplyr::group_by(period2, tripid, fitted_length, catch_draw, mode1) %>%
      #   dplyr::summarise(Number = sum(release)) %>%
      #   dplyr::group_by(period2, tripid, fitted_length) %>%
      #   dplyr::mutate( AvgNum = Number/n_catch_draws,
      #                  Species = "SCUP",
      #                  Keep_Release = "Release") %>%
      #   dplyr::select(-catch_draw)
      # 
      # release_size_data_scup <- release_size_data_scup[!duplicated(release_size_data_scup),]
      # 
      # 
      # length_data_scup <- rbind(keep_size_data_scup, release_size_data_scup)
      #
      summed_catch_data <- catch_size_data %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid",  "mode1", "month"), .SDcols = c("keep", "release")]
      
      summed_catch_data <- summed_catch_data %>%
        dplyr::rename(tot_keep_scup = keep,
                      tot_rel_scup = release)
      
      trip_data_scup<-summed_catch_data
      #add the zero catch trips
      trip_data_scup <- dplyr::bind_rows(trip_data_scup, scup_zero_catch) %>%
        #arrange(period, catch_draw, tripid) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::mutate(state = state1,
                      tot_scup_catch = tot_keep_scup + tot_rel_scup) %>% 
        dplyr::select(-("month_day"))
      
      
      
      # merge the scup trip data with the rest of the trip data
      #trip_data <-  merge(trip_data,trip_data_scup,by=c("period2", "catch_draw", "tripid", "state", "mode", "month"))
      
      trip_data <- trip_data %>%
        dplyr::left_join(trip_data_scup, by = c("period2", "catch_draw", "tripid", "state", "mode1", "month"))
      
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
  
  
  #length_data <- rbind(length_data_bsb, length_data_scup, length_data_sf)
  
  #names<- c(grep("*beta*", names(costs_new_all), value=TRUE, invert=TRUE))
  costs_new_all2 <- data.frame(  costs_new_all[[1]]) %>% #tibble() %>%
    #dplyr::filter(catch_draw<=n_catch_draws) %>% #%>%select(all_of(names)) %>%
    dplyr::filter(catch_draw<=n_drawz) %>% 
    dplyr::select(-beta_cost) %>%
    dplyr::rename(beta_sqrt_sf_keep_base=beta_sqrt_sf_keep,
                  beta_sqrt_sf_release_base=beta_sqrt_sf_release,
                  beta_sqrt_bsb_keep_base=beta_sqrt_bsb_keep,
                  beta_sqrt_bsb_release_base=beta_sqrt_bsb_release,
                  beta_sqrt_sf_bsb_keep_base=beta_sqrt_sf_bsb_keep,
                  beta_sqrt_scup_catch_base=beta_sqrt_scup_catch,
                  beta_opt_out_base=beta_opt_out)
  
  # names<- c(grep("*length*", names(trip_data), value=TRUE, invert=TRUE))
  # trip_data<-trip_data %>%
  #   select(all_of(names))
  
  
  # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
  trip_data <- trip_data %>%
    dplyr::left_join(costs_new_all2, by = c("period2","catch_draw","tripid", "state")) #%>%  select(-decade.x, -decade.y)
  
  # %>%
  
  
  
  #Draw from the distirbtuion of utility parameters for each iteration of the projection
  
  period_vec1 <- period_vec %>%
    dplyr::mutate(#beta_sqrt_sf_keep= rnorm(nrow(period_vec), mean = .8093247, sd = 1.288768),
      #        beta_sqrt_sf_release = rnorm(nrow(period_vec), mean = .0656076 , sd = .2454453) ,
      #        beta_sqrt_bsb_keep = rnorm(nrow(period_vec), mean = .3607657, sd = .2085837),
      #        beta_sqrt_bsb_release = rnorm(nrow(period_vec), mean = .0665897 , sd = .0711506),
      #        beta_sqrt_sf_bsb_keep = rnorm(nrow(period_vec), mean =-.0596595  , sd = .161084),
      #        beta_sqrt_scup_catch = rnorm(nrow(period_vec), mean = .019203 , sd = 0),
      #        beta_opt_out = rnorm(nrow(period_vec), mean =-1.637635 , sd = 2.059597),
      beta_cost = -.0114955) %>%
    dplyr::group_by(period2) %>%
    dplyr::mutate(tripid = dplyr::row_number(period2))
  
  trip_data2 <- trip_data %>%
    dplyr::left_join(period_vec1, by = c("period2","tripid"))
  
  trip_data <- trip_data2 %>%
    dplyr::select(-month.x, -month.y, -wv2.y, -day.y, -day.x, -day.y)
  #trip_data <- trip_data  %>%  dplyr::arrange(period2, tripid, catch_draw)
  
  
  #  utility (prediction year)
  trip_data <-trip_data %>%
    dplyr::mutate(
      # vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf) +
      #   beta_sqrt_sf_release*sqrt(tot_rel_sf) +
      #   beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
      #   beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +
      #   beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf)*sqrt(tot_keep_bsb)) +
      #   beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
      #   beta_cost*cost,
      
      vA = beta_sqrt_sf_keep_base*sqrt(tot_keep_sf) +
        beta_sqrt_sf_release_base*sqrt(tot_rel_sf) +
        beta_sqrt_bsb_keep_base*sqrt(tot_keep_bsb) +
        beta_sqrt_bsb_release_base*sqrt(tot_rel_bsb) +
        beta_sqrt_scup_catch_base*sqrt(tot_scup_catch) +
        #beta_sqrt_scup_keep_base*sqrt(tot_keep_scup) +
        #beta_sqrt_scup_release_base*sqrt(tot_rel_scup) +
        beta_sqrt_sf_bsb_keep_base*(sqrt(tot_keep_sf)*sqrt(tot_keep_bsb)) +
        beta_cost*cost,
      
      #  utility (base year)
      v0 = beta_sqrt_sf_keep_base*sqrt(tot_keep_sf_base) +
        beta_sqrt_sf_release_base*sqrt(tot_rel_sf_base) +
        beta_sqrt_bsb_keep_base*sqrt(tot_keep_bsb_base) +
        beta_sqrt_bsb_release_base*sqrt(tot_rel_bsb_base) +
        beta_sqrt_scup_catch_base*sqrt(tot_cat_scup_base)  +
        #beta_sqrt_scup_keep_base*sqrt(tot_keep_scup_base) +
        #beta_sqrt_scup_release_base*sqrt(tot_rel_scup_base) +
        beta_sqrt_sf_bsb_keep_base*(sqrt(tot_keep_sf_base)*sqrt(tot_keep_bsb_base)) +
        beta_cost*cost)
  
  
  #These stats should be roughly the same under no chnage in fishery conditions
  
   
  # mean(trip_data$v0)
  #
  #
  # mean(trip_data$tot_keep_sf)
  # mean(trip_data$tot_keep_sf_base)
  # 
  # mean(trip_data$tot_rel_sf)
  # mean(trip_data$tot_rel_sf_base)
  # 
  # mean(trip_data$tot_keep_bsb)
  # mean(trip_data$tot_keep_bsb_base)
  # 
  # mean(trip_data$tot_rel_bsb)
  # mean(trip_data$tot_rel_bsb_base)
  # 
  # mean(trip_data$tot_scup_catch)
  # mean(trip_data$tot_cat_scup_base)
  
  
  trip_data <- trip_data %>%
    dplyr::mutate(period = as.numeric(as.factor(period2)))
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
  
  mean_trip_data <- trip_data %>%
    data.table::data.table()
    #dplyr::select(-c("state", "period2", "mode1")) %>% data.table::data.table() #%>% dplyr::arrange(period, tripid, catch_draw)
  
  #mean_trip_data<-mean_trip_data %>% dplyr::arrange(period, tripid, catch_draw)
  
  #New code to calculate probability of each choice occasion
  
  # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  mean_trip_data2 <- mean_trip_data %>%
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
  
  mean_trip_data3 <- mean_trip_data2 %>%
    data.table::as.data.table() %>%
    .[, vA_optout := beta_opt_out_base*opt_out] %>%
    .[, v0_optout := beta_opt_out_base*opt_out] %>%
    .[alt==1, expon_vA := exp(vA)] %>%
    .[alt==2, expon_vA := exp(vA_optout)] %>%
    .[alt==1, expon_v0 := exp(v0)] %>%
    .[alt==2, expon_v0 := exp(v0_optout)]
  
  
  
  # mean_trip_data <- mean_trip_data %>%
  #   group_by(period, tripid, catch_draw) %>%
  #   mutate(vA_col_sum = sum(expon_vA),
  #          v0_col_sum = sum(expon_v0)) %>%
  #   ungroup()
  
  
  mean_trip_data4 <- mean_trip_data3 %>%
    data.table::as.data.table() %>%
    .[, vA_col_sum := base::sum(expon_vA), by=list(period, catch_draw, tripid)]  %>%
    .[, v0_col_sum := base::sum(expon_v0), by=list(period, catch_draw, tripid)]
  
  
  #
  # mean_trip_data1 <- mean_trip_data %>%
  #   mutate(change_CS =(1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum)),
  #          probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum)
  #
  
  mean_trip_data5 <- mean_trip_data4 %>%
    data.table::as.data.table() %>%
    .[, change_CS := (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))] %>%
    .[, probA :=expon_vA/vA_col_sum] %>%
    .[, prob0 :=expon_v0/v0_col_sum]
  
  
  mean_trip_data6<- subset(mean_trip_data5, alt==1)
  
  # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>%
  #   tibble()
  
  
  all_vars<-c()
  all_vars <- names(mean_trip_data6)[!names(mean_trip_data6) %in% c("period","tripid", "period2", "kod", "kod_24", "state", "mode1", "month.day.x")]
  
  mean_trip_data7<-mean_trip_data6  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, base::mean), by = c("period2","tripid", "kod", "kod_24", "state"), .SDcols = all_vars]
  
  
  #original code
  # Collapse data from the X catch draws so that each row contains mean values
  #mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
  # mean_trip_data <- trip_data %>%
  #   dplyr::select(-c("state", "period2", "mode1", "month1")) %>% data.table() #%>% dplyr::arrange(period, tripid, catch_draw)
  #
  # mean_trip_data <- mean_trip_data[, lapply(.SD, mean), by=list(period,tripid)] %>%
  #   tibble() #%>%
  #
  # mean(mean_trip_data$vA)
  # mean(mean_trip_data$v0)
  # #
  mean(mean_trip_data$tot_keep_sf)
  mean(mean_trip_data$tot_keep_sf_base)

  mean(mean_trip_data$tot_rel_sf)
  mean(mean_trip_data$tot_rel_sf_base)

  mean(mean_trip_data$tot_keep_bsb)
  mean(mean_trip_data$tot_keep_bsb_base)

  mean(mean_trip_data$tot_rel_bsb)
  mean(mean_trip_data$tot_rel_bsb_base)

  mean(mean_trip_data$tot_scup_catch)
  mean(mean_trip_data$tot_cat_scup_base)

  mean(mean_trip_data$prob0)
  mean(mean_trip_data$probA)
  
  #
  #
  #
  # # nkeep <- trip_data %>%
  # #   group_by(period, tripid) %>%
  # #   summarise(keep_one = length(which(tot_keep>0))/length(tot_keep), #n(),
  # #             .groups = "drop")
  # # mean_trip_data <- left_join(mean_trip_data, nkeep, by = c("period", "tripid"))
  #
  #
  # # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  # #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  # mean_trip_data <- mean_trip_data %>%
  #   mutate(n_alt = rep(2,nrow(.))) %>%
  #   uncount(n_alt) %>%
  #   mutate(alt = rep(1:2,nrow(.)/2),
  #          opt_out = ifelse(alt == 2, 1, 0))
  #
  #
  #
  # #Caluculate the expected utility of alts 2 parameters of the utility function
  # mean_trip_data$vA_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out
  # mean_trip_data$v0_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out
  #
  #
  # #Now put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  # mean_trip_data$expon_vA <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA),
  #                                      mean_trip_data$alt==2 ~ exp(mean_trip_data$vA_optout))
  #
  # mean_trip_data$expon_v0 <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$v0),
  #                                      mean_trip_data$alt==2 ~ exp(mean_trip_data$v0_optout))
  #
  # mean_trip_data <- mean_trip_data %>%
  #   group_by(period, tripid) %>%
  #   mutate(vA_col_sum = sum(expon_vA),
  #          v0_col_sum = sum(expon_v0)) %>%
  #   ungroup()
  #
  # mean_trip_data <- mean_trip_data %>%
  #   mutate(CS_base = (1/beta_cost)*log(v0_col_sum),
  #          CS_alt = (1/beta_cost)*log(vA_col_sum),
  #          change_CS = CS_alt-CS_base,
  #          probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum)
  
  #end original code
  
  #   vars<- grep("*length*", names(mean_trip_data), invert=TRUE, value=TRUE)
  #   mean_trip_data_check[[x]]  <- subset(mean_trip_data,select = c(vars))
  #   mean_trip_data_check[[x]]$run<-x
  # }
  #   mean_trip_data_check_all<- list.stack(mean_trip_data_check)
  #   mean_trip_data_check_all <- subset(mean_trip_data_check_all, alt==1)
  #
  #   aggregate_mean_trip_data_check_all <- mean_trip_data_check_all %>%
  #     group_by(run) %>%
  #     summarize_all(sum, na.rm = TRUE) %>%
  #     ungroup()
  #
  #
  
  
  # mutate(change_CS = -(1/beta_cost)*(log(expon_v0) - log(expon_vA))) %>%
  #   mutate(probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum)
  #mean(mean_trip_data$prob0)
  # Get rid of things we don't need.
  mean_trip_data8 <- subset(mean_trip_data7, alt==1,select=-c(alt, beta_cost,
                                                            catch_draw, expon_v0 ,v0_col_sum, expon_vA,
                                                            opt_out, v0, v0_optout, vA, vA_optout, vA_col_sum))
  
  # Multiply the average trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
  
  # list_names <- colnames(mean_trip_data8)[colnames(mean_trip_data8) !="tripid"
  #                                        & colnames(mean_trip_data8) !="period"
  #                                        & colnames(mean_trip_data8) !="probA"
  #                                        & colnames(mean_trip_data8) !="prob0"
  #                                        & colnames(mean_trip_data8) !="change_CS"
  #                                        & colnames(mean_trip_data8) !="CS_base"
  #                                        & colnames(mean_trip_data8) !="CS_alt"
  #                                        & colnames(mean_trip_data8) !="tot_keep_bsb_base"
  #                                        & colnames(mean_trip_data8) !="tot_cat_scup_base"
  #                                        & colnames(mean_trip_data8) !="tot_keep_sf_base"
  #                                        & colnames(mean_trip_data8) !="tot_rel_bsb_base"
  #                                        & colnames(mean_trip_data8) !="tot_rel_sf_base"] #Add period2, kod, kod_24, and mode1?
  
  # for (l in list_names){
  #   mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
  # }
  list_names <- c("tot_keep_sf","tot_rel_sf", "tot_sf_catch", "tot_keep_bsb", "tot_rel_bsb" , 
                  "tot_bsb_catch" , "tot_keep_scup" , "tot_rel_scup","tot_scup_catch" )
  
  mean_trip_data9 <- mean_trip_data8 %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
    .[]
  
  # length_data2<- mean_trip_data9 %>%
  #   dplyr::select(period2, tripid, probA) %>%
  #   dplyr::left_join(length_data, relationship = "many-to-many") %>%
  #   dplyr::mutate(ProbabilityNumber = AvgNum * probA) %>%  # Average Number * probA
  #   dplyr::group_by(Species, Keep_Release, period2, fitted_length) %>%
  #   dplyr::summarise(ProbabilityNumber = sum(ProbabilityNumber))#Sum probNum over Spp, KeepRelease, period2, and fitted_length

  
  # list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid"
  #                                        & colnames(mean_trip_data) !="period"
  #                                        & colnames(mean_trip_data) !="probA"
  #                                        & colnames(mean_trip_data) !="prob0"
  #                                        & colnames(mean_trip_data) !="change_CS"
  #                                        & colnames(mean_trip_data) !="CS_base"
  #                                        & colnames(mean_trip_data) !="CS_alt"
  #                                        & colnames(mean_trip_data) !="tot_keep_bsb"
  #                                        & colnames(mean_trip_data) !="tot_scup_catch"
  #                                        & colnames(mean_trip_data) !="tot_keep_sf"
  #                                        & colnames(mean_trip_data) !="tot_rel_bsb"
  #                                        & colnames(mean_trip_data) !="tot_rel_sf"]
  list_names <- c("tot_keep_sf_base","tot_rel_sf_base", "tot_keep_bsb_base", "tot_rel_bsb_base" , 
                  "tot_cat_scup_base" )
  
  mean_trip_data9 <- mean_trip_data8 %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    .[]
 
  mean_trip_data9<- mean_trip_data9 %>% 
    dplyr::select(unique(colnames(mean_trip_data9)))
  
  mean_trip_data <- mean_trip_data9 %>%
    dplyr::mutate( n_choice_occasions_alt = rep(1,nrow(.))) %>%
    dplyr::left_join(period_names, by = c("period2"))
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::select(-c("period"))
  
  
  #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in
  #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
  #calibration_data <- data.frame(calibration_data[[1]])  #%>%   rename(period2 = period)
  #calibration_data<- calibration_output_by_period
  
  sims <- calibration_data %>%
    dplyr::select(c(n_choice_occasions, period2)) %>%
    dplyr::left_join(mean_trip_data, by = "period2") %>% 
    dplyr::mutate(ndraws = c(50),
      period = as.character(period2)) %>%
    dplyr::mutate(expand = n_choice_occasions/ndraws)
 
  # length_expand <- sims %>%
  #   dplyr::select(period2, expand) %>%
  #   dplyr::left_join(length_data2, by = "period2", relationship = "many-to-many") %>%
  #   dplyr::mutate(Expanded_prob_number = expand * ProbabilityNumber,
  #                 SppLength = paste0(Species, "_" ,Keep_Release, "_",  stringr::str_extract(period2, "[a-z]+"),  "_", stringr::str_extract(period2, "\\d\\d"), "_", fitted_length)) %>%
  #   dplyr::group_by(SppLength) %>%
  #   dplyr::summarise(NumLength = mean(Expanded_prob_number)) %>%
  #   tidyr:: pivot_wider(., names_from = "SppLength", values_from = "NumLength") ### Uncomment when this should work

  # # help<- length_expand %>% 
  # #   dplyr::filter(stringr::str_detect(SppLength, 'NA'))
  # # 
  # 
  # length_test <- length_expand %>% 
  #   tidyr::separate("SppLength", into = c("Spp", "keep_rel", "mode", "month", "length"), sep = "_") %>% 
  #   dplyr::group_by(Spp, keep_rel, mode) %>% 
  #   dplyr::summarise(sum(NumLength))
  # 
  #length_expand<- length_expand[rep(seq_len(nrow(length_expand)), each = nrow(sims)), ]
    #Multiply Expand by probNum then
    #Sum by fish length across all periods and modes = single value (total Number) of SF at 17in
  
  ############################# ADD CORRECTED n_choice_occasions HERE!!!! ######################
  
  #mean_trip_data$sim=1
  # mean_trip_data <- mean_trip_data %>%
  #   dplyr::mutate(sim = rep(1,nrow(.)))
  
  #Here add other trip quality statistics
  #keep_one = ifelse(tot_keep>0,1,0))
  
  #datset to compute mean cv over all trip
      
  trip_level_output <- sims%>%
    dplyr::mutate(state=state1)   %>%
    dplyr::select(c(period2, kod, kod_24, n_choice_occasions, tripid, expand, change_CS, state, probA, prob0, 
                    tot_keep_sf, tot_rel_sf, 
                    tot_keep_bsb, tot_rel_bsb,
                    tot_keep_scup, tot_rel_scup,
                    tot_scup_catch, 
                    tot_keep_sf_base, tot_keep_bsb_base, tot_cat_scup_base)) #%>% 
    #cbind(length_expand)
  mean(trip_level_output$change_CS)
  
  ## Add Length_expand to trip_level_output
  #left_join(LengthProbs) LengthProbablities(average Length for each tripID catch draws and days multiplied by probA (example with catch - line 900))
  
  if (state1 %in% c("DE", "MD", "VA")){
    rm(trip_data, trip_data_bsb, sf_zero_catch, bsb_zero_catch, sf_catch_data, bsb_catch_data)
  }
  
  if (state1 %in% c("MA", "RI", "CT", "NY", "NJ")){
    rm(trip_data, trip_data_bsb, trip_data_scup, sf_zero_catch, scup_zero_catch, bsb_zero_catch, sf_catch_data, bsb_catch_data, scup_catch_data)
  }

  return(trip_level_output)
  print(head(trip_level_output))
  #end function
}

#sum probability weighted catch over all choice occasions
#aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
# aggregate_trip_data <- mean_trip_data %>%
#   group_by(period2, sim) %>%
#   summarize_all(sum, na.rm = TRUE) %>%
#   ungroup() %>%
#   left_join(sims , by = c("period2"))
# # right_join(sims %>% mutate(period = as.numeric(period)) %>% dplyr::select(-n_choice_occasions),
# #            by = c("period","sim"))
#
#
# ls(aggregate_trip_data)
# list_names = colnames(aggregate_trip_data)[ colnames(aggregate_trip_data) !="tripid"
#                                             & colnames(aggregate_trip_data) !="catch_draw" & colnames(aggregate_trip_data) !="period2"
#                                             & colnames(aggregate_trip_data) !="vA" & colnames(aggregate_trip_data) !="v0"
#                                             & colnames(aggregate_trip_data) != "state" & colnames(aggregate_trip_data) !="period"
#                                             & colnames(aggregate_trip_data) != "ndraws"
#                                             & colnames(aggregate_trip_data) != "expand" & colnames(aggregate_trip_data) != "n_choice_occasions"
#                                             & colnames(aggregate_trip_data) != "parameter_draw" ]
#
#
# aggregate_trip_data <- aggregate_trip_data %>%
#   mutate(across(.cols = all_of(list_names),.fns=function(x) expand*x)) %>%
#   select(-c(tripid, sim, expand, n_choice_occasions)) %>%
#   rename(projected_trips = probA,
#           baseline_trips = prob0 )
#
#
# projection_output <- aggregate_trip_data %>%  #list.stack(pds_new, fill=TRUE) %>%
#   #mutate_at(vars(contains("length")), replace_na, replace = 0) %>%
#   #pds_new_all_MA[is.na(pds_new_all_MA)] = 0
#   mutate(state = state1) %>%
#   select(baseline_trips, projected_trips, change_CS, CS_alt, CS_base, state) %>%
#   group_by(state)  %>%
#   summarise(across(everything(), sum),
#             .groups = 'drop')  %>%
#   as.data.frame()   #%>%   mutate(avg_cv=avg_cv)
#
# #sum(projection_output$change_CS)
# #})
# # write_xlsx(pds_new_all_MA,"MA_prediction_output_check.xlsx")
# return(projection_output)

#end function
#}

