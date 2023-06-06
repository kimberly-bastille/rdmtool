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
state1 <- "NJ"
state_no<-34
directed_trips_table <- directed_trips_table_base[[5]]
sf_catch_data_all <- readRDS(here::here("data-raw/catch/catch_files_NJ.rds"))
p_star_sf <- p_star_sf_NJ_variable
p_star_bsb<-p_star_bsb_NJ_variable
p_star_scup<-p_star_scup_NJ_variable
p_star_scup<-p_star_scup_NJ_variable

calibrate_rec_catch <- function(state1,
                                state_no,
                                directed_trips_table,
                                sf_catch_data_all,
                                p_star_sf,
                                p_star_bsb,
                                p_star_scup, k){
  
  
  # Input regul
  directed_trips <- directed_trips_table %>% tibble::tibble() %>% dplyr::filter(state == state1)
  
  
  
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
  
  sf_catch_data <- sf_catch_data_all #%>%
    #dplyr::rename(tot_sf_catch = sf_catch,  tot_bsb_catch = bsb_catch, tot_scup_catch = scup_catch)  %>%
    #dplyr::select(-c(month))
  
  sf_catch_data <- sf_catch_data %>%
    dplyr::mutate(period2 = paste0(month, "_", day, "_", mode1)) %>% 
    dplyr::group_by(period2) %>%
    dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
    dplyr::mutate(#period = rep(period_vec$period2, each = nsamp),
      catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
      tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
    dplyr::ungroup()
  
  
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
  sf_zero_catch <- dplyr::filter(sf_catch_data, sf_tot_cat == 0)
  
  #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
  sf_catch_check<-sum(sf_catch_data$sf_tot_cat)
  bsb_catch_check<-sum(sf_catch_data$bsb_tot_cat)
  scup_catch_check<-sum(sf_catch_data$scup_tot_cat)
  
  
  #remove trips with zero summer flounder catch
  sf_catch_data <- dplyr::filter(sf_catch_data, sf_tot_cat > 0)
  
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(sf_catch_data))
  
  #sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
  sf_catch_data<- sf_catch_data %>%
    dplyr::slice(rep(row_inds,sf_tot_cat))   %>%
    dplyr::mutate(fishid=dplyr::row_number())
  
  
  sf_catch_data2 <- sf_catch_data %>%
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
  sf_catch_data <- sf_catch_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  sf_catch_data <- sf_catch_data %>%
    dplyr::mutate(keep_tot = keep_adj,
                  release = ifelse(keep_adj==0,1,0))
  
  
  
  sf_catch_data2<- sf_catch_data %>% 
    dplyr::select=c(fishid, tripid, keep_tot, release, period2, catch_draw, mode, month) %>%
    dplyr::rename(keep = keep_tot)
  
  #Uncomment this if you want sizes of fish
  # new_size_data <- catch_size_data %>%
  #   group_by(period2, catch_draw, tripid, fitted_length, mode, month) %>%
  #   summarize(keep = sum(keep),
  #             release = sum(release), .groups = "drop") #%>%    dplyr::arrange(tripid, period2,  catch_draw)
  
  # summed_catch_data <- catch_size_data %>%
  #   group_by(period2, catch_draw, tripid,  mode, month) %>%
  #   summarize(tot_keep_sf = sum(keep),
  #             tot_rel_sf = sum(release),
  #             .groups = "drop") #%>%
  
  summed_catch_data <- sf_catch_data %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid",  "mode", "month"), .SDcols = c("keep", "release")]
  
  summed_catch_data <- summed_catch_data %>%
    dplyr::rename(tot_keep_sf = keep,
                  tot_rel_sf = release)
  
  #The following code will retain the length of fish kept and released.
  #This takes up a lot of memory, so for now will comment out these lines.
  
  
  # keep_size_data <- new_size_data %>%
  #   #ungroup() %>%
  #   dplyr::select(-release) %>%
  #   pivot_wider(names_from = fitted_length, #_length,
  #               names_glue = "keep_length_sf_{fitted_length}",
  #               names_sort = TRUE,
  #               values_from = keep,
  #               values_fill = 0) # %>%
  # #I()
  # #keep_size_data
  #
  # release_size_data <- new_size_data %>%
  #   #ungroup() %>%
  #   dplyr::select(-keep) %>%
  #   pivot_wider(names_from = fitted_length, #_length,
  #               names_glue = "release_length_sf_{fitted_length}",
  #               names_sort = TRUE,
  #               values_from = release,
  #               values_fill = 0) #%>%
  #
  #
  # trip_data <- summed_catch_data %>%
  #   left_join(keep_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) %>%
  #   left_join(release_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) #%>%
  
  #end retaining sizes of fish kept and released
  
  trip_data<-summed_catch_data
  #add the zero catch trips
  
  trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
    #arrange(period, catch_draw, tripid) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
    dplyr::mutate(state = state1,
                  tot_sf_catch = tot_keep_sf + tot_rel_sf) %>%
    dplyr::select(-c("tot_bsb_catch", "tot_scup_catch"))
  
  
  if (sf_catch_check==0){
    trip_data<-sf_catch_data %>%
      dplyr::mutate(tot_keep_sf=0,
                    tot_rel_sf=0,
                    tot_sf_catch = tot_keep_sf+tot_rel_sf)
    subset(select=-c(tot_bsb_catch, tot_scup_catch))
    
  }
  
  
  #########################
  ###  Black sea bass  ####
  #########################
  
  
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
    
    
    
    bsb_catch_data <- bsb_catch_data %>%
      dplyr::left_join(regs, by = "period2") %>%
      dplyr::mutate(uniform=runif(nrow(bsb_catch_data))) %>%
      dplyr::mutate(posskeep = ifelse(uniform>=p_star_bsb, 1,0)) %>%
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
    #,
    # keep_adj = case_when(
    #   csum_keep<=bag & keep==1 ~ 1,
    #   TRUE ~ 0),
    #release = case_when(
    # bsb_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>bsb_bag ), 1,0)))
    
    #catch_size_data[is.na(catch_size_data)] <- 0
    bsb_catch_data <- bsb_catch_data %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    bsb_catch_data <- bsb_catch_data %>%
      dplyr:: mutate(release = ifelse(keep_adj==0,1,0))
    
    #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
    
    bsb_catch_data<- subset(bsb_catch_data, select=c(fishid, tripid, keep_adj, release, period2, catch_draw, mode, month)) %>%
      dplyr::rename(keep = keep_adj)
    
    #Uncomment this if you want sizes of fish
    # new_size_data <- catch_size_data %>%
    # group_by(period2, catch_draw, tripid, fitted_length,  mode, month) %>%
    # summarize(keep = sum(keep),
    #           release = sum(release), .groups = "drop") #%>%
    
    # summed_catch_data <- catch_size_data %>%
    #   group_by(period2, catch_draw, tripid,  mode, month) %>%
    #   summarize(tot_keep_bsb = sum(keep),
    #             tot_rel_bsb = sum(release),
    #             .groups = "drop") #%>%
    
    summed_catch_data <- bsb_catch_data %>%
      data.table::as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid",  "mode", "month"), .SDcols = c("keep", "release")]
    
    summed_catch_data <- summed_catch_data %>%
      dplyr::rename(tot_keep_bsb = keep,
                    tot_rel_bsb = release)
    
    #The following code will retain the length of fish kept and released.
    #This takes up a lot of memory, so for now will comment out these lines.
    
    # keep_size_data <- new_size_data %>%
    #   #ungroup() %>%
    #   dplyr::select(-release) %>%
    #   pivot_wider(names_from = fitted_length, #_length,
    #               names_glue = "keep_length_bsb_{fitted_length}",
    #               names_sort = TRUE,
    #               values_from = keep,
    #               values_fill = 0) # %>%
    # #I()
    # #keep_size_data
    #
    # release_size_data <- new_size_data %>%
    #   #ungroup() %>%
    #   dplyr::select(-keep) %>%
    #   pivot_wider(names_from = fitted_length, #_length,
    #               names_glue = "release_length_bsb_{fitted_length}",
    #               names_sort = TRUE,
    #               values_from = release,
    #               values_fill = 0) #%>%
    #
    #
    # trip_data_bsb <- summed_catch_data %>%
    #   left_join(keep_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) %>%
    #   left_join(release_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) #%>%
    #end retaining sizes of fish kept and released
    
    trip_data_bsb<-summed_catch_data
    #add the zero catch trips
    trip_data_bsb <- dplyr::bind_rows(trip_data_bsb, bsb_zero_catch) %>%
      #arrange(period, catch_draw, tripid) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::mutate(state = state1,
                    tot_bsb_catch = tot_keep_bsb + tot_rel_bsb)  %>%
      dplyr::select(-c("tot_sf_catch", "tot_scup_catch"))
    
    
    # merge the bsb trip data with the rest of the trip data
    #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))
    
    trip_data <- trip_data %>%
      dplyr::left_join(trip_data_bsb, by = c("period2", "catch_draw", "tripid", "state", "mode", "month" )) #%>%  select(-decade.x, -decade.y)
    
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
      
      
      
      scup_catch_data <- scup_catch_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(uniform=runif(nrow(scup_catch_data))) %>%
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
      scup_catch_data <- scup_catch_data %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      scup_catch_data <- scup_catch_data %>%
        dplyr::mutate(release = ifelse(keep_adj==0,1,0))
      
      #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
      
      scup_catch_data<- subset(scup_catch_data, select=c(fishid, tripid, keep_adj, release, period2, catch_draw, mode, month)) %>%
        dplyr::rename(keep = keep_adj)
      
      #Uncomment this if you want sizes of fish
      # new_size_data <- catch_size_data %>%
      #   group_by(period2, catch_draw, tripid, fitted_length, mode, month) %>%
      #   summarize(keep = sum(keep),
      #             release = sum(release), .groups = "drop") #%>%
      
      # summed_catch_data <- catch_size_data %>%
      #   group_by(period2, catch_draw, tripid,mode, month) %>%
      #   summarize(tot_keep_scup = sum(keep),
      #             tot_rel_scup = sum(release),
      #             .groups = "drop") #%>%
      
      summed_catch_data <- scup_catch_data %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid",  "mode", "month"), .SDcols = c("keep", "release")]
      
      summed_catch_data <- summed_catch_data %>%
        dplyr::rename(tot_keep_scup = keep,
                      tot_rel_scup = release)
      
      
      
      #The following code will retain the length of fish kept and released.
      #This takes up a lot of memory, so for now will comment out these lines.
      
      # keep_size_data <- new_size_data %>%
      #   #ungroup() %>%
      #   dplyr::select(-release) %>%
      #   pivot_wider(names_from = fitted_length, #_length,
      #               names_glue = "keep_length_scup_{fitted_length}",
      #               names_sort = TRUE,
      #               values_from = keep,
      #               values_fill = 0) # %>%
      # #I()
      # #keep_size_data
      #
      # release_size_data <- new_size_data %>%
      #   #ungroup() %>%
      #   dplyr::select(-keep) %>%
      #   pivot_wider(names_from = fitted_length, #_length,
      #               names_glue = "release_length_scup_{fitted_length}",
      #               names_sort = TRUE,
      #               values_from = release,
      #               values_fill = 0) #%>%
      #
      #
      # trip_data_scup <- summed_catch_data %>%
      #   left_join(keep_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) %>%
      #   left_join(release_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) #%>%
      #end retaining sizes of fish kept and released
      
      
      trip_data_scup<-summed_catch_data
      #add the zero catch trips
      trip_data_scup <- dplyr::bind_rows(trip_data_scup, scup_zero_catch) %>%
        #arrange(period, catch_draw, tripid) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::mutate(state = state1,
                      tot_scup_catch = tot_keep_scup + tot_rel_scup)
      
      
      
      # merge the scup trip data with the rest of the trip data
      #trip_data <-  merge(trip_data,trip_data_scup,by=c("period2", "catch_draw", "tripid", "state", "mode", "month"))
      
      trip_data <- trip_data %>%
        dplyr::left_join(trip_data_scup, by = c("period2", "catch_draw", "tripid", "state", "mode", "month")) #%>%  select(-decade.x, -decade.y)
      
      #trip_data  <- trip_data[ , order(names(trip_data))]
      
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
  
  
  
  
  trip_data<- trip_data %>%
    dplyr::mutate(tot_bsb_catch=tot_keep_bsb+tot_rel_bsb,
                  tot_sf_catch=tot_keep_sf+tot_rel_sf,
                  tot_scup_catch=tot_keep_scup+tot_rel_scup)
  
  
  period_vec1 <- period_vec %>%
    dplyr::mutate(beta_sqrt_sf_keep= rnorm(nrow(period_vec), mean = .8093247, sd = 1.288768),
                  beta_sqrt_sf_release = rnorm(nrow(period_vec), mean = .0656076 , sd = .2454453) ,
                  beta_sqrt_bsb_keep = rnorm(nrow(period_vec), mean = .3607657, sd = .2085837),
                  beta_sqrt_bsb_release = rnorm(nrow(period_vec), mean = .0665897 , sd = .0711506),
                  beta_sqrt_sf_bsb_keep = rnorm(nrow(period_vec), mean=-.0596595  , sd = .161084 ),
                  beta_sqrt_scup_catch = rnorm(nrow(period_vec), mean = .019203 , sd = 0),
                  beta_opt_out = rnorm(nrow(period_vec), mean =-1.637635 , sd = 2.059597),
                  beta_cost = -.0114955) %>%
    dplyr::group_by(period2) %>% dplyr::mutate(tripid = dplyr::row_number(period2))
  
  trip_data<- trip_data %>%
    dplyr::left_join(period_vec1, by = c("period2","tripid"))
  
  trip_costs<-data.frame(readr::read_csv(file.path(here::here("data-raw/trip_costs_state_summary.csv")), show_col_types = FALSE))
  trip_costs<-trip_costs %>%
    subset(state==state1)
  
  trip_data <- trip_data %>%
    dplyr::left_join(trip_costs, by = c("mode"))
  
  trip_data <- trip_data %>%
    dplyr::mutate(cost=rnorm(nrow(trip_data), mean=trip_data$mean, sd=trip_data$st_error))
  
  trip_data <- trip_data %>%
    dplyr::select(-state.x, -state.y)
  
  trip_data <- trip_data %>%
    dplyr::mutate(state=state1)
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario.
  # We will merge these data to the prediction year outcomes to calculate changes in CS.
  costs_new_all <- trip_data %>%
    subset(select=c(tripid, cost, catch_draw, tot_keep_sf, tot_rel_sf,
                    tot_keep_bsb,tot_rel_bsb,tot_scup_catch, beta_cost, beta_opt_out, beta_sqrt_bsb_keep,
                    beta_sqrt_bsb_release, beta_sqrt_scup_catch, beta_sqrt_sf_bsb_keep,
                    beta_sqrt_sf_keep, beta_sqrt_sf_release, state, period2)) %>%
    dplyr::rename(tot_keep_sf_base = tot_keep_sf,
                  tot_rel_sf_base = tot_rel_sf,
                  tot_keep_bsb_base = tot_keep_bsb,
                  tot_rel_bsb_base = tot_rel_bsb,
                  tot_cat_scup_base = tot_scup_catch) %>% 
    dplyr::mutate(n_cal_draw = k)
  
  
  
  trip_data <- trip_data %>%
    dplyr::select(-month.x, -month.y)
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
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
  
  mean_trip_data <- trip_data %>%
    dplyr::select(-c("state", "period2", "mode")) %>% data.table::data.table() #%>% dplyr::arrange(period, tripid, catch_draw)
  
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
    .[, vA_optout := beta_opt_out*opt_out] %>%
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
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period","tripid")]
  
  
  
  mean_trip_data2<-mean_trip_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period","tripid"), .SDcols = all_vars]
  
  
  
  # Get rid of things we don't need.
  mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost, mean, st_error,
                                                            catch_draw,  expon_vA,
                                                            opt_out, vA, vA_optout, vA_col_sum))
  
  # Multiply the average trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
  list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid"
                                         & colnames(mean_trip_data) !="period"
                                         & colnames(mean_trip_data) !="probA"]
  
  # for (l in list_names){
  #   mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
  # }
  
  
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
    .[]
  
  
  
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate( n_choice_occasions = rep(1,nrow(.))) %>%
    dplyr::left_join(period_names, by = c("period"))
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::select(-c("period"))
  
  
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
  
  
  
  list_names = c("n_choice_occasions", "probA", "tot_bsb_catch","tot_keep_bsb", "tot_keep_scup" , "tot_keep_sf",
                 "tot_rel_bsb" ,   "tot_rel_scup","tot_rel_sf","tot_scup_catch","tot_sf_catch")
  
  aggregate_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period2"), .SDcols = list_names]
  
  names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "estimated_trips"
  pds_new_all<-aggregate_trip_data %>%
    dplyr::mutate(state=state1, 
                  n_cal_draw = k)
  
  output<-list(pds_new_all, costs_new_all)
  #
  return(output)

  # return(pds_new_all)
  # return(costs_new_all)
  
}



