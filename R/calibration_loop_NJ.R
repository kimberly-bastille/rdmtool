

state1="NJ"
state_no=34

p_star_sf_bt <- p_star_sf_NJ_variable_bt
p_star_bsb_bt<-p_star_bsb_NJ_variable_bt
p_star_scup_bt<-p_star_scup_NJ_variable_bt

p_star_sf_sh <- p_star_sf_NJ_variable_sh
p_star_bsb_sh<-p_star_bsb_NJ_variable_sh

n_drawz = 50
n_catch_draws = 30
######################################
##   Begin simulating trip outcomes ##
######################################

#Import directed trips file - gives directed trips by regulatory period in 2020
directed_trips <- data.frame( read.csv(here::here("data-raw/directed trips and regulations 2022_pstar.csv"))) %>% 
  dplyr::filter(draw == 1) %>%
  dplyr::mutate(day = as.numeric(stringr::str_extract(day, "^\\d{2}")), 
              month = as.numeric(month)) %>% 
  dplyr::mutate(period2 = as.character(paste0(month, "_", day, "_", mode))) 

#directed_trips$dtrip=round(directed_trips$dtrip)
directed_trips <-  directed_trips %>% dplyr::filter(state == state1)

# min_period=min(directed_trips$period)
# max_period=max(directed_trips$period)



# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()

#periodz=as.factor(directed_trips$period2)
#levels(periodz)
period_list<- as.list(unique(directed_trips$period2))

dfs <- data.frame()

#for(p in levels(periodz)){
for(p in period_list){
  #p<- "14_bt"
  directed_trips_p <- directed_trips %>% 
    dplyr::filter( period2 == p)
  n_trips = mean(directed_trips_p$dtrip)
  #n_draws = min(1000,n_trips*2.5 )
  n_draws = n_drawz
  
  
  fluke_bag = mean(directed_trips_p$fluke_bag1)
  fluke_min = mean(directed_trips_p$fluke_min1)
  bsb_bag = mean(directed_trips_p$bsb_bag)
  bsb_min = mean(directed_trips_p$bsb_min)
  scup_bag = mean(directed_trips_p$scup_bag)
  scup_min = mean(directed_trips_p$scup_min)
  
  
  #obtain the mode and month 
  month <-as.numeric(directed_trips_p[1,]$month)
  month1 <- sprintf("%02d", month)
  mode1 <- directed_trips_p[1,]$mode
  
  # Set up an output file for catch draw files 
  # dfs <- data.frame()
  
  #Run the catch loop X times to give X draws of catch for each species
  for(i in 1:n_catch_draws){
    # Input catch-per-trip numbers 
    # Two ways to do this, I've included code for both.
    # 1) import 2020 MRIP trip-level catch of the three species - this retains any correlation in catch between the species  
    # 2) estimate the NB parameters using the raw data then simulate the distribution - this assumes independence in catch between the species
    
    
    
    
    
    
    #method 1)
    
    # nbs_params<- subset(read_csv("nb_params_baseline.csv",  show_col_types = FALSE))
    # nbs_params$month <-as.numeric(nbs_params$month)
    # nbs_params$month <- sprintf("%02d", nbs_params$month)
    # nbs_params
    # 
    # nbs_params<-subset(nbs_params, state==state1)
    # nbs_params<-subset(nbs_params, mode==mode1)
    # nbs_params<-subset(nbs_params, month==month1)
    # 
    # 
    # sf_mu_param <- nbs_params$sf_mu
    # sf_size_param <- nbs_params$sf_size
    # bsb_mu_param <- nbs_params$bsb_mu
    # bsb_size_param <- nbs_params$bsb_size
    # scup_mu_param <- nbs_params$scup_mu
    # scup_size_param <- nbs_params$scup_size
    # ###
    # 
    # if (sf_mu_param!=0){
    #   tot_sf_catch <- rnbinom(1:10000, mu = sf_mu_param, size = sf_size_param)
    # }
    # 
    # if (sf_mu_param==0){
    #   tot_sf_catch <- data.frame(1:10000)
    #   tot_sf_catch$tot_sf_catch<-0
    #   tot_sf_catch<-subset(tot_sf_catch, select=c(tot_sf_catch))
    # }
    # 
    # 
    # if (bsb_mu_param!=0){
    #   tot_bsb_catch <- rnbinom(1:10000, mu = bsb_mu_param, size = bsb_size_param)
    # }
    # 
    # if (bsb_mu_param==0){
    #   tot_bsb_catch <- data.frame(1:10000)
    #   tot_bsb_catch$tot_bsb_catch<-0
    #   tot_bsb_catch<-subset(tot_bsb_catch, select=c(tot_bsb_catch))
    # }
    # 
    # if (scup_mu_param!=0){
    #   tot_scup_catch <- rnbinom(1:10000, mu = scup_mu_param, size = scup_size_param)
    # }
    # 
    # if (scup_mu_param==0){
    #   tot_scup_catch <- data.frame(1:10000)
    #   tot_scup_catch$tot_scup_catch<-0
    #   tot_scup_catch<-subset(tot_scup_catch, select=c(tot_scup_catch))
    # }
    
    # sf_catch_data <- data.frame(tot_sf_catch,tot_bsb_catch,tot_scup_catch)
    #end method 1)
    
    #######
    #method 2)
    sf_catch_data = readRDS(file.path(here::here('data-raw/catch/catch_files_NJ.rds')))
    #sf_catch_data = subset(sf_catch_data, decade==d)
    
    #sf_catch_data = data.frame(read.csv(paste0('observed_catch_2020_',state_no,'_', month1, '_', mode1, '.csv')))
    
    tot_sf_catch = sf_catch_data$sf_tot_cat
    tot_bsb_catch = sf_catch_data$bsb_tot_cat
    tot_scup_catch = sf_catch_data$scup_tot_cat
    sf_catch_data = data.frame(tot_sf_catch,tot_bsb_catch,tot_scup_catch)
    
    #end method 2)
    #######
    
    
    
    
    
    # random draw of fluke and bsb catch
    sf_catch_data = as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_drawz), ])
    
    #see if there is positive catch for all species; if not, then skip the keep/release allocation 
    
    catch_check_sf<-sum(sf_catch_data$tot_sf_catch)
    catch_check_bsb<-sum(sf_catch_data$tot_bsb_catch)    
    catch_check_scup<-sum(sf_catch_data$tot_scup_catch)    
    
    sf_catch_data$tripid = 1:nrow(sf_catch_data)
    sf_bsb_catch_data = sf_catch_data
    
    
    # subset trips with zero catch, as no size draws are required
    sf_zero_catch = subset(sf_catch_data, tot_sf_catch == 0)
    
    
    
    if (catch_check_sf!=0){
      
      #remove trips with zero summer flounder catch, will add them on later
      sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
      
      
      #expand the sf_catch_data so that each row represents a fish
      row_inds = seq_len(nrow(sf_catch_data))
      sf_catch_data = sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
      rownames(sf_catch_data) = NULL
      sf_catch_data$fishid = 1:nrow(sf_catch_data)
      
      
      
      #Execute the following code if the seasonal period has a positive bag limit 
      if(fluke_bag>0){
        
        sf_catch_data1= as.data.frame(sf_catch_data)  
        sf_catch_data1$uniform=runif(nrow(sf_catch_data1))
        sf_catch_data1$keep = ifelse(sf_catch_data1$uniform>=p_star_sfn  , 1,0)
        
        sf_catch_data1$csum_keep <- ave(sf_catch_data1$keep, sf_catch_data1$tripid, FUN=cumsum)
        sf_catch_data1$keep_adj = ifelse(sf_catch_data1$csum_keep>fluke_bag, 0,sf_catch_data1$keep)
        
        # #Add the following lines to end the trip once the bag limit is reached (rather than continuing to discard)
        # ###
        # sf_catch_data1$post_bag_fish=ifelse(sf_catch_data1$csum_keep>fluke_bag, 1,0)
        # sf_catch_data1= subset(sf_catch_data1,post_bag_fish==0 )
        # sf_catch_data1 <- subset(sf_catch_data1, select=-c(post_bag_fish ))
        # ###
        
        sf_catch_data1 <- subset(sf_catch_data1, select=-c(keep, csum_keep))
        names(sf_catch_data1)[names(sf_catch_data1) == "keep_adj"] = "keep"
        
        sf_catch_data1$release = ifelse(sf_catch_data1$keep==0, 1,0) 
        sf_catch_data1=subset(sf_catch_data1, select=c(tripid, keep, release))
        
        sf_catch_data1 <- sf_catch_data1 %>% 
          dplyr::group_by(tripid) %>% 
          dplyr::summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(sf_catch_data1)[names(sf_catch_data1) == "keep"] = "tot_keep_sf"
        names(sf_catch_data1)[names(sf_catch_data1) == "release"] = "tot_rel_sf"
        
      }
      
      if(fluke_bag==0){
        
        sf_catch_data1= as.data.frame(sf_catch_data)  
        sf_catch_data1$keep = 0
        sf_catch_data1$release = 1
        
        sf_catch_data1=subset(sf_catch_data1, select=c(tripid, keep, release))
        sf_catch_data1 <- sf_catch_data1 %>% 
          dplyr::group_by(tripid) %>% 
          dplyr::summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(sf_catch_data1)[names(sf_catch_data1) == "keep"] = "tot_keep_sf"
        names(sf_catch_data1)[names(sf_catch_data1) == "release"] = "tot_rel_sf"
        
      }
      
      
      
      trip_data =  as.data.frame(sf_catch_data1)
      
      #add the zero catch trips 
      trip_data = dplyr::bind_rows(trip_data, sf_zero_catch)
      
      #quick sort and cleanup 
      trip_data = trip_data[order(trip_data$tripid),]
      rownames(trip_data) <- NULL
      
      trip_data<-subset(trip_data, select=-c(tot_sf_catch, tot_bsb_catch, tot_scup_catch))
      trip_data[is.na(trip_data)] = 0
      trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
      trip_data[is.na(trip_data)] = 0
      
    }
    
    
    if (catch_check_sf==0){
      trip_data<-sf_catch_data
      trip_data$tot_keep_sf<-0
      trip_data$tot_rel_sf<-0
      trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
      trip_data <- subset(trip_data, select=-c(tot_bsb_catch, tot_scup_catch))
      
    }
    
    #########################
    ###  Black sea bass  ####
    #########################
    
    
    #draw sizes for black sea bass catch
    bsb_catch_data =subset(sf_bsb_catch_data, select=c(tripid, tot_bsb_catch))
    bsb_catch_data = bsb_catch_data[!duplicated(bsb_catch_data), ]
    
    #subset trips with zero bsb catch 
    bsb_zero_catch = subset(bsb_catch_data, tot_bsb_catch == 0, select=c(tripid, tot_bsb_catch))
    
    if (catch_check_bsb!=0){
      
      
      #remove trips with zero bsb catch, will add them on later
      bsb_catch_data=bsb_catch_data[bsb_catch_data$tot_bsb_catch!=0, ]
      rownames(bsb_catch_data) = NULL
      
      
      #expand the bsb_catch_data so that each row represents a fish
      row_inds = seq_len(nrow(bsb_catch_data))
      bsb_catch_data[is.na(bsb_catch_data)] = 0
      bsb_catch_data = bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
      rownames(bsb_catch_data) = NULL
      bsb_catch_data$fishid = 1:nrow(bsb_catch_data)
      
      
      
      #Execute the following code if the seasonal period has a positive bag limit 
      if(bsb_bag>0){
        
        bsb_catch_data1= as.data.frame(bsb_catch_data)  
        bsb_catch_data1$uniform=runif(nrow(bsb_catch_data1))
        bsb_catch_data1$keep = ifelse(bsb_catch_data1$uniform>=p_star_bsb, 1,0) 
        
        bsb_catch_data1$csum_keep <- ave(bsb_catch_data1$keep, bsb_catch_data1$tripid, FUN=cumsum)
        bsb_catch_data1$keep_adj = ifelse(bsb_catch_data1$csum_keep>bsb_bag, 0,bsb_catch_data1$keep)
        bsb_catch_data1 <- subset(bsb_catch_data1, select=-c(keep, csum_keep))
        names(bsb_catch_data1)[names(bsb_catch_data1) == "keep_adj"] = "keep"
        
        
        bsb_catch_data1$release = ifelse(bsb_catch_data1$keep==0, 1,0) 
        
        bsb_catch_data1=subset(bsb_catch_data1, select=c(tripid, keep, release))
        
        bsb_catch_data1 <- subset(bsb_catch_data1, select=c(tripid, keep, release))
        bsb_catch_data1 <- bsb_catch_data1 %>% 
          dplyr::group_by(tripid) %>% 
          dplyr::summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(bsb_catch_data1)[names(bsb_catch_data1) == "keep"] = "tot_keep_bsb"
        names(bsb_catch_data1)[names(bsb_catch_data1) == "release"] = "tot_rel_bsb"
        
      }
      
      if(bsb_bag==0){
        
        bsb_catch_data1= as.data.frame(bsb_catch_data)  
        bsb_catch_data1$keep = 0
        bsb_catch_data1$release = 1
        
        bsb_catch_data1 <- subset(bsb_catch_data1, select=c(tripid, keep, release))
        bsb_catch_data1 <- bsb_catch_data1 %>% 
          dplyr::group_by(tripid) %>% 
          dplyr::summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(bsb_catch_data1)[names(bsb_catch_data1) == "keep"] = "tot_keep_bsb"
        names(bsb_catch_data1)[names(bsb_catch_data1) == "release"] = "tot_rel_bsb"
        
      }
      
      
      #add the zero catch trips 
      bsb_catch_data1 = dplyr::bind_rows(bsb_catch_data1, bsb_zero_catch)
      bsb_catch_data1 = subset(bsb_catch_data1, select=-c(tot_bsb_catch))
      
      #quick sort and cleanup 
      bsb_catch_data1 = bsb_catch_data1[order(bsb_catch_data1$tripid),]
      rownames(bsb_catch_data1) <- NULL
      
      
      
      bsb_catch_data1[is.na(bsb_catch_data1)] = 0
      
      
      # merge the trip data (summer flounder catch, lengths, and cost) with the bsb data (numbers kept and released))
      trip_data =  merge(trip_data,bsb_catch_data1,by="tripid")
      trip_data[is.na(trip_data)] = 0
      
    }
    
    if (catch_check_bsb==0){ 
      trip_data_bsb<-bsb_catch_data
      trip_data_bsb$tot_keep_bsb<-0
      trip_data_bsb$tot_rel_bsb<-0
      trip_data <-  merge(trip_data,trip_data_bsb,by="tripid")
      
    }
    
    
    
    #########################
    ###       Scup       ####
    #########################
    
    
    #draw sizes for scup catch
    scup_catch_data =subset(sf_bsb_catch_data, select=c(tripid, tot_scup_catch))
    scup_catch_data = scup_catch_data[!duplicated(scup_catch_data), ]
    
    #subset trips with zero scup catch 
    scup_zero_catch = subset(scup_catch_data, tot_scup_catch == 0, select=c(tripid, tot_scup_catch))
    
    if (catch_check_scup!=0){
      
      #remove trips with zero scup catch, will add them on later
      scup_catch_data=scup_catch_data[scup_catch_data$tot_scup_catch!=0, ]
      rownames(scup_catch_data) = NULL
      
      
      #expand the scup_catch_data so that each row represents a fish
      row_inds = seq_len(nrow(scup_catch_data))
      scup_catch_data[is.na(scup_catch_data)] = 0
      scup_catch_data = scup_catch_data[c(rep(row_inds, scup_catch_data$tot_scup_catch)), ]
      rownames(scup_catch_data) = NULL
      scup_catch_data$fishid = 1:nrow(scup_catch_data)
      
      
      
      #Execute the following code if the seasonal period has a positive bag limit 
      if(scup_bag>0){
        
        scup_catch_data1= as.data.frame(scup_catch_data)  
        scup_catch_data1$uniform=runif(nrow(scup_catch_data1))
        scup_catch_data1$keep = ifelse(scup_catch_data1$uniform>=p_star_scup, 1,0) 
        
        scup_catch_data1$csum_keep <- ave(scup_catch_data1$keep, scup_catch_data1$tripid, FUN=cumsum)
        scup_catch_data1$keep_adj = ifelse(scup_catch_data1$csum_keep>scup_bag, 0,scup_catch_data1$keep)
        
        scup_catch_data1 <- subset(scup_catch_data1, select=-c(keep, csum_keep))
        names(scup_catch_data1)[names(scup_catch_data1) == "keep_adj"] = "keep"
        
        
        scup_catch_data1$release = ifelse(scup_catch_data1$keep==0, 1,0) 
        
        scup_catch_data1 <- subset(scup_catch_data1, select=c(tripid, keep, release))
        scup_catch_data1 <- scup_catch_data1 %>% 
          dplyr::group_by(tripid) %>% 
          dplyr::summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(scup_catch_data1)[names(scup_catch_data1) == "keep"] = "tot_keep_scup"
        names(scup_catch_data1)[names(scup_catch_data1) == "release"] = "tot_rel_scup"
        
      }
      
      if(scup_bag==0){
        
        scup_catch_data1= as.data.frame(scup_catch_data)  
        scup_catch_data1$keep = 0
        scup_catch_data1$release = 1
        
        
        scup_catch_data1 <- subset(scup_catch_data1, select=c(tripid, keep, release))
        scup_catch_data1 <- scup_catch_data1 %>% 
          dplyr::group_by(tripid) %>% 
          dplyr::summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        
        names(scup_catch_data1)[names(scup_catch_data1) == "keep"] = "tot_keep_scup"
        names(scup_catch_data1)[names(scup_catch_data1) == "release"] = "tot_rel_scup"
        
      }
      
      
      #add the zero catch trips 
      scup_catch_data1 = dplyr::bind_rows(scup_catch_data1, scup_zero_catch)
      scup_catch_data1 = subset(scup_catch_data1, select=-c(tot_scup_catch))
      scup_catch_data1 <- scup_catch_data1 %>% dplyr::mutate(tot_scup_catch = c("NA"))
      
      #quick sort and cleanup 
      scup_catch_data1 = scup_catch_data1[order(scup_catch_data1$tripid),]
      rownames(scup_catch_data1) <- NULL
      
      scup_catch_data1[is.na(scup_catch_data1)] = 0
      
      
      # merge the trip data (summer flounder catch, lengths, and cost) with the bsb/fluke data (numbers kept and released))
      trip_data =  merge(trip_data,scup_catch_data1,by="tripid")
    }
    
    if (catch_check_scup==0){ 
      trip_data_scup<-scup_catch_data
      trip_data_scup$tot_keep_scup<-0
      trip_data_scup$tot_rel_scup<-0
      trip_data <-  merge(trip_data,trip_data_scup,by="tripid")
      
    }
    
    
    trip_data[is.na(trip_data)] = 0
    
    trip_data <-trip_data %>% 
      dplyr::mutate(catch_draw = i, 
                    period2 = p)
    
    #dfs<-dfs %>% rbind(trip_data) 
    #trip_data$catch_draw=i
    #dfs[[i]]=trip_data
    
  }
  
  #combine all the catch draw files 
  #dfs_all<- list.stack(dfs, fill=TRUE)
  #dfs_all<- list(dfs, fill=TRUE)
  dfs<-dfs %>% rbind(trip_data) 
  pds<- dfs
  
  # dfs_all[is.na(dfs_all)] = 0
  # #dfs_all <- dfs_all[order(dfs_all$tripid),]
  # rownames(dfs_all) = NULL
  # 
  # dfs_all$period=p
  # pds[[p]] = dfs_all
}

######################################
##   End simulating trip outcomes   ##
######################################

# pds_all= list(pds, fill=TRUE)
# pds_all[is.na(pds_all)] = 0
pds_all <- pds %>% 
  dplyr::mutate(tot_bsb_catch = tot_keep_bsb+tot_rel_bsb, 
                tot_sf_catch = tot_keep_sf+tot_rel_sf, 
                tot_scup_catch = tot_keep_scup+tot_rel_scup)
# pds_all$tot_bsb_catch=pds_all$tot_keep_bsb+pds_all$tot_rel_bsb
# pds_all$tot_sf_catch=pds_all$tot_keep_sf+pds_all$tot_rel_sf
# pds_all$tot_scup_catch=pds_all$tot_keep_scup+pds_all$tot_rel_scup

#rm(pds)

#pds_all<-subset(pds_all, select=-c(tot_bsb_catch.x,tot_bsb_catch.y, tot_scup_catch.x, tot_scup_catch.y))

#Create random draws of preference parameters based on the estimated means and SD from the choice model
param_draws_NJ = as.data.frame(1:n_drawz)
names(param_draws_NJ)[1] <- 'tripid'
param_draws_NJ$beta_sqrt_sf_keep = rnorm(n_drawz, mean = .8093247, sd = 1.288768)
param_draws_NJ$beta_sqrt_sf_release = rnorm(n_drawz, mean = .0656076 , sd = .2454453)
param_draws_NJ$beta_sqrt_bsb_keep = rnorm(n_drawz, mean = .3607657, sd = .2085837)
param_draws_NJ$beta_sqrt_bsb_release = rnorm(n_drawz, mean = .0665897 , sd = .0711506)
param_draws_NJ$beta_sqrt_sf_bsb_keep = rnorm(n_drawz, mean =-0.06  , sd = .2)
param_draws_NJ$beta_sqrt_scup_catch = rnorm(n_drawz, mean = .019203 , sd = 0)
param_draws_NJ$beta_opt_out = rnorm(n_drawz, mean =-1.637635 , sd = 2.059597)
param_draws_NJ$beta_cost = rnorm(n_drawz, mean =-.0114955 , sd =0)
#param_draws_NJ$parameter_draw=d


# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion
#costs_new_NJ = list()
pds_new = list()
#levels(periodz)
for(p in period_list){
  #p<-'14_bt'
  directed_trips_p <- subset(directed_trips, period2 == p)
  n_trips <- mean(directed_trips_p$dtrip)  
  mode_val <- directed_trips_p[1,]$mode
  
  # Add trip costs. These are mean and sd estimates from over all modes from the expenditure survey
  #pds<-subset(pds_all, period2==p)
  #pds<-pds_all[p]
  pds<-pds_all %>% dplyr::filter(period2 == p)
  
  trip_costs<-data.frame(read.csv(here::here("data-raw/trip_costs_state_summary.csv")))
  trip_costs<- subset(trip_costs, mode==mode_val & state==state1)
  mean_cost<-mean(trip_costs$mean)
  sd_cost<-mean(trip_costs$st_error)
  
  trip_data<-pds
  trip_data$cost<-rnorm(nrow(trip_data[1]), mean=mean_cost,sd= sd_cost)
  trip_data[is.na(trip_data)] <- 0
  trip_data$cost<-ifelse(trip_data$cost<0, 0,trip_data$cost )
  
  trip_data =  merge(param_draws_NJ,trip_data,by="tripid")
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario. 
  # We will merge these data to the prediction year outcomes to calculate changes in CS. 
  # costs_new_NJ[[p]] = subset(trip_data, select=c(tripid, cost, catch_draw, tot_keep_sf, tot_rel_sf,
  #                                                tot_keep_bsb,tot_rel_bsb,tot_scup_catch, beta_cost, beta_opt_out, beta_sqrt_bsb_keep, 
  #                                                beta_sqrt_bsb_release, beta_sqrt_scup_catch, beta_sqrt_sf_bsb_keep, 
  #                                                beta_sqrt_sf_keep, beta_sqrt_sf_release))
  costs_new_NJ <- trip_data %>% 
    dplyr::filter(period2 == p) %>% 
    dplyr::select(c(tripid, cost, catch_draw, tot_keep_sf, tot_rel_sf,
                    tot_keep_bsb,tot_rel_bsb,tot_scup_catch, beta_cost, beta_opt_out, beta_sqrt_bsb_keep, 
                    beta_sqrt_bsb_release, beta_sqrt_scup_catch, beta_sqrt_sf_bsb_keep,
                    beta_sqrt_sf_keep, beta_sqrt_sf_release)) %>% 
    dplyr::rename("tot_keep_sf_base" = tot_keep_sf, 
                  "tot_rel_sf_base" = tot_rel_sf, 
                  "tot_keep_bsb_base" = tot_keep_bsb,
                  "tot_rel_bsb_base" = tot_rel_bsb, 
                  "tot_scup_catch_base" = tot_scup_catch)
  
  #Expected utility
  trip_data$vA = 
    trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf) +
    trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf) +  
    trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
    trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) + 
    trip_data$beta_sqrt_sf_bsb_keep*(sqrt(trip_data$tot_keep_sf)*sqrt(trip_data$tot_keep_bsb)) +
    trip_data$beta_sqrt_scup_catch*sqrt(trip_data$tot_scup_catch) +
    trip_data$beta_cost*trip_data$cost 
  
  mean_trip_data <- trip_data %>% 
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>% 
    tidyr::uncount(n_alt) %>% 
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
           opt_out = ifelse(alt == 2, 1, 0))
  
  mean_trip_data$v0_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  
  
  #Now put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  
  mean_trip_data$expon_v0 <- dplyr::case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA), 
                                       mean_trip_data$alt==2 ~ exp(mean_trip_data$v0_optout)) 
  
  mean_trip_data <- mean_trip_data %>% 
    dplyr::group_by(period2, tripid, catch_draw) %>% 
    dplyr::mutate( v0_col_sum = sum(expon_v0)) %>% 
    dplyr::ungroup()
  
  mean_trip_data <- mean_trip_data %>% 
    dplyr::mutate(prob = expon_v0/v0_col_sum) 
  
  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  mean_trip_data<-mean_trip_data %>% 
    dplyr::group_by(period2,tripid) %>% 
    dplyr::summarise(across(everything(), mean), .groups = 'drop') %>% 
    tibble::tibble()
  
  
  
  # Get rid of things we don't need. 
  mean_trip_data = subset(mean_trip_data,  select=-c(alt, opt_out, v0_optout,  catch_draw, vA, v0_optout,beta_cost, beta_opt_out, beta_sqrt_scup_catch,beta_sqrt_bsb_release, 
                                                     beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep, beta_sqrt_sf_bsb_keep,
                                                     period2 ))
  
  
  # Multiply the trip probability by each of the catch and cost variables  to get probability-weighted catch
  list_names = c("tot_bsb_catch","tot_keep_bsb","tot_keep_scup", "tot_keep_sf","tot_rel_bsb", "tot_rel_scup",
                 "tot_rel_sf","tot_scup_catch" , "tot_sf_catch", "cost")
  
  for (l in list_names){
    mean_trip_data[,l] = mean_trip_data[,l]*mean_trip_data$prob
  }
  
  
  # Multiply each choice occasion's trip outcomes (catch, cost, trip probabilities) in mean_trip_pool 
  # by the expansion factor (expand), so that each choice occasion represents a certain number of choice occasions
  
  mean_prob=mean(mean_trip_data$prob)
  observed_trips=n_trips
  sims = round(observed_trips/mean_prob)
  ndraws = nrow(mean_trip_data)
  expand=sims/ndraws
  mean_trip_data$n_choice_occasions=1
  
  list_names = c("tot_bsb_catch",  "tot_keep_bsb", "tot_rel_bsb",  
                 "tot_scup_catch", "tot_keep_scup", "tot_rel_scup",
                 "tot_sf_catch", "tot_keep_sf", "tot_rel_sf",
                 "cost", "prob","n_choice_occasions" )
  
  for (l in list_names){
    mean_trip_data[,l] = mean_trip_data[,l]*expand
  }
  
  
  
  mean_trip_data$sim=1
  
  #sum probability weighted catch over all choice occasions
  aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
  
  
  aggregate_trip_data = subset(aggregate_trip_data, select=-c(Group.1, tripid, expon_v0, v0_col_sum,sim))
  names(aggregate_trip_data)[names(aggregate_trip_data) == "prob"] = "estimated_trips"
  
  aggregate_trip_data$period<-p
  
  
  aggregate_trip_data$sim =1
  
  
  aggregate_trip_data$period=p
  pds_new[[p]]=aggregate_trip_data
  
}

pds_new_all_NJ <- pds
#pds_new_all_NJ=list.stack(pds_new, fill=TRUE)

pds_new_all_NJ[is.na(pds_new_all_NJ)] = 0
#pds_new_all_NJ$state = state1
#rm(pds_new)

# costs_new_all contain trip outcomes for the baseline period. Will use to calculate welfare changes, 
# and assign catch-per-trip in the prediction years. 
costs_new_all_NJ <- costs_new_NJ
#costs_new_all_NJ=list.stack(costs_new_NJ, fill=TRUE)
costs_new_all_NJ[is.na(costs_new_all_NJ)] = 0
#rm(costs_new_NJ)


###Compare calibration model output with MRIP 

MRIP_data <-  read.csv(here::here("data-raw/total AB1B2 by state mode_pstar.csv")) %>% 
  dplyr::filter(state=="NJ")                                                                          



##SF
sum(pds_new_all_NJ$tot_keep_sf)
sum(MRIP_data$sf_harvest)
sf_harvest_harv_diff<-((sum(MRIP_data$sf_harvest)-sum(pds_new_all_NJ$tot_keep_sf))/sum(MRIP_data$sf_harvest))*100
sf_harvest_harv_diff

sum(pds_new_all_NJ$tot_rel_sf)
sum(MRIP_data$sf_releases)
((sum(MRIP_data$sf_releases)-sum(pds_new_all_NJ$tot_rel_sf))/sum(MRIP_data$sf_releases))*100

sum(pds_new_all_NJ$tot_sf_catch)
sum(MRIP_data$sf_tot_cat)
((sum(MRIP_data$sf_tot_cat)-sum(pds_new_all_NJ$tot_sf_catch))/sum(MRIP_data$sf_tot_cat))*100





##BSB
sum(pds_new_all_NJ$tot_keep_bsb)
sum(MRIP_data$bsb_harvest)
bsb_harvest_harv_diff<-((sum(MRIP_data$bsb_harvest)-sum(pds_new_all_NJ$tot_keep_bsb))/sum(MRIP_data$bsb_harvest))*100
bsb_harvest_harv_diff

sum(pds_new_all_NJ$tot_rel_bsb)
sum(MRIP_data$bsb_releases)
((sum(MRIP_data$bsb_releases)-sum(pds_new_all_NJ$tot_rel_bsb))/sum(MRIP_data$bsb_releases))*100

sum(pds_new_all_NJ$tot_bsb_catch)
sum(MRIP_data$bsb_tot_cat)
((sum(MRIP_data$bsb_tot_cat)-sum(pds_new_all_NJ$tot_bsb_catch))/sum(MRIP_data$bsb_tot_cat))*100





##scup
sum(pds_new_all_NJ$tot_keep_scup)
sum(MRIP_data$scup_harvest)
scup_harvest_harv_diff<-((sum(MRIP_data$scup_harvest)-sum(pds_new_all_NJ$tot_keep_scup))/sum(MRIP_data$scup_harvest))*100
scup_harvest_harv_diff

sum(pds_new_all_NJ$tot_rel_scup)
sum(MRIP_data$scup_releases)
((sum(MRIP_data$scup_releases)-sum(pds_new_all_NJ$tot_rel_scup))/sum(MRIP_data$scup_releases))*100

sum(pds_new_all_NJ$tot_scup_catch)
sum(MRIP_data$scup_tot_cat)
((sum(MRIP_data$scup_tot_cat)-sum(pds_new_all_NJ$tot_scup_catch))/sum(MRIP_data$scup_tot_cat))*100
