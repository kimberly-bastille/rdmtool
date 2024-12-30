# pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
#                  "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
#                  "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
#                  "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather")
# install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))
# lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
# conflicts_prefer(dplyr::mutate)


options(scipen = 100, digits = 6)


  #Pull in effort, size, and disc mortality data
  size_data_read = read.csv(paste0(input_data_cd, "projected_CaL_cod_hadd_cm.csv"))
  Disc_mort<- readr::read_csv(paste0(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)
  
  #l_w_conversion parameters =
  cod_lw_a = 0.000005132
  cod_lw_b = 3.1625
  had_lw_a = 0.000009298
  had_lw_b = 3.0205
  
  output1<-data.frame() 
  output2<-data.frame() ##This dataset will store all the results
  
  baseline_comparison1<-readRDS(paste0(input_data_cd, "calibration_comparison.rds")) %>% 
    dplyr::relocate(draw, mrip_index, open, mode, tot_keep_cod_model, tot_cod_keep_mrip, diff_cod_harv, perc_diff_cod_harv, h_star_cod_keep_to_release_variable, h_star_cod_release_to_keep_variable, 
                    tot_rel_cod_model, tot_cod_rel_mrip, tot_keep_hadd_model, tot_hadd_keep_mrip, diff_hadd_harv, perc_diff_hadd_harv,  h_star_hadd_keep_to_release_variable, h_star_hadd_release_to_keep_variable,
                    tot_rel_hadd_model, tot_hadd_rel_mrip ) %>% 
    dplyr::arrange(draw, mrip_index) %>% 
    dplyr::group_by(draw) %>% 
    dplyr::mutate(draw_id = cur_group_id()) %>% 
    dplyr::filter(draw_id<=100)
  
  #n_distinct(baseline_comparison1$draw)
  execution_time <- system.time({
  for(i in unique(baseline_comparison1$mrip_index)){
    
    # Import regulations
    
    #Lou's code: 
    #regulation variables with no subscript, i.e., cod_min, are the regulations in the calibration year 
    #regulation variables _y2 subscript, i.e., cod_min_y2, are currently the SQ regulations for the projection year
    #regulation variables _SQ subscript, i.e., cod_min_SQ, are the SQ regulations for the projection year
    
    #for kim: We need to retain the _SQ variables and allow the _y2 variables to reflect alternative regulations. This needs to
    #to be done after we copy the variables _y2=_SQ below
    
    directed_trips_table =  read_feather(paste0(input_data_cd, "directed_trips_calib_150draws_cm.feather")) %>% 
      dplyr::mutate(cod_min_SQ=cod_min_y2, cod_bag_SQ=cod_bag_y2, hadd_min_SQ=hadd_min_y2, hadd_bag_SQ=hadd_bag_y2)
    
    # End import regulations
    
    
    baseline_comparison<-baseline_comparison1 %>% 
      dplyr::filter(mrip_index==i) %>% 
      dplyr::mutate(all_cod_keep_2_release=ifelse(tot_keep_cod_model>0 & tot_cod_keep_mrip==0, 1, 0),
                    all_hadd_keep_2_release=ifelse(tot_keep_hadd_model>0 & tot_hadd_keep_mrip==0, 1, 0))
    
    baseline_output0<-readRDS(paste0(input_data_cd, "harvest_differences_check.rds")) %>% 
      dplyr::filter(mrip_index==i)
    
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
    
    #pull in the h_star_values computed from the calibration 
    h_star_cod_release_to_keep_variable<-mean(baseline_comparison$h_star_cod_release_to_keep_variable)
    h_star_hadd_release_to_keep_variable<-mean(baseline_comparison$h_star_hadd_release_to_keep_variable)
    h_star_cod_keep_to_release_variable<-mean(baseline_comparison$h_star_cod_keep_to_release_variable)
    h_star_hadd_keep_to_release_variable<-mean(baseline_comparison$h_star_hadd_keep_to_release_variable)
    
    #number of legal cod/haddock released, or illegal cod/haddock harvested in the baseline year
    n_legal_cod_rel_base<-mean(baseline_comparison$n_legal_cod_rel)
    n_legal_hadd_rel_base<-mean(baseline_comparison$n_legal_hadd_rel)
    n_sub_cod_kept_base<-mean(baseline_comparison$n_sub_cod_kept)
    n_sub_had_kept_base<-mean(baseline_comparison$n_sub_had_kept)
    
    #Pull in data that is draw-specific
    calendar_2024_adjust <- readr::read_csv(paste0(input_data_cd, "next year calendar adjustments.csv"), show_col_types = FALSE) %>%
      dplyr::filter(draw == k)
    calibration_data_table = feather::read_feather(paste0(iterative_input_data_cd, "pds_new_", select_mode,"_", select_season, "_", k,".feather"))
    costs_new_all = feather::read_feather(paste0(iterative_input_data_cd, "costs_", select_mode,"_", select_season, "_", k,".feather"))
    
    n_drawz = 50
    n_catch_draws = 30
    set.seed(5)

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
    floor_subl_cod_harv<-min(directed_trips$cod_min_y2)-(2*2.54)
    floor_subl_hadd_harv<-min(directed_trips$hadd_min_y2)-(2*2.54)
    
    if (floor_subl_cod_harv>=248.9){
      floor_subl_cod_harv<-min(directed_trips$cod_min)-(2*2.54)
    }
    
    if (floor_subl_hadd_harv>=248.9){
      floor_subl_hadd_harv<-min(directed_trips$hadd_min)-(2*2.54)
    }
    
    open<- directed_trips %>%
      dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                    period2 = paste0(month, "_", day, "_", mode)) %>%
      dplyr::select(period2, open, month, day) %>%
      dplyr::filter(open == select_season)
    
    directed_trips<- directed_trips %>%
      dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                    period2 = paste0(month, "_", day, "_", mode)) %>%
      dplyr::filter(open == select_season)
    
    
    ######################################
    ##   Begin simulating trip outcomes ##
    ######################################
    
    directed_trips_p <- directed_trips %>%
      dplyr::mutate(n_draws = n_drawz)%>%
      dplyr::select(!c(month, mode))
    
    regs <- directed_trips_p %>%
      dplyr::select(period2,cod_bag_y2, cod_min_y2, hadd_bag_y2,hadd_min_y2, cod_bag, cod_min, hadd_bag, hadd_min, 
                    cod_bag_SQ, cod_min_SQ, hadd_bag_SQ, hadd_min_SQ) 
    
    cod_catch_data <- costs_new_all %>%
      dplyr::mutate(tot_cod_catch = tot_keep_cod_base + tot_rel_cod_base,
                    tot_had_catch = tot_keep_had_base + tot_rel_had_base) %>% 
      dplyr::left_join(open, by = "period2") %>%
      dplyr::select(mode,month,tot_cod_catch,tot_had_catch, 
                    tripid,catch_draw,day, period2)
    
    
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
    

    #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
    cod_catch_check<-base::sum(cod_catch_data$tot_cod_catch)
    had_catch_check<-base::sum(cod_catch_data$tot_had_catch)
    
    #if there is no catch of both species 
    if(cod_catch_check ==0 & had_catch_check==0){
      trip_data<-cod_catch_data
      trip_data<- trip_data %>% 
        dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
        dplyr::select(-mode) %>% 
        dplyr::mutate(tot_keep_cod_new=0, tot_rel_cod_new=0, tot_keep_hadd_new=0, tot_rel_hadd_new=0)
    }
    
    
    #########################
    ###  Cod  ####
    #########################
    
    # subset trips with zero catch, as no size draws are required
    cod_zero_catch <- dplyr::filter(cod_catch_data, tot_cod_catch == 0)
    
    
    #if there is catch of cod
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
                                             #prob = cod_size_data$fitted_prob,
                                             replace = TRUE)) 
      
      # Create as an object the minimum size at which fish are illegally harvested.
      # This object "floor_subl_harvest" will be 2 inches below the minimum size, by mode. 
      #1) If the minimum size changes across the season, floor_subl_harvest=min(min_size) - 2.
      #2a) If the fishery is closed the entire season, floor_subl_harvest=min(min. size of the previous season) -2 
      #2b) If the fishery is closed the entire current and previous season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
      
      #2b) below:
      if (floor_subl_cod_harv>=248.9){
        floor_subl_cod_harv=mean(catch_size_data$fitted_length)-0.5*sd(catch_size_data$fitted_length)
      }
      
      
      # Impose regulations, calculate keep and release per trip
      ####### Start Here #################
      
      ############# Length #####################################
      
      #Compute keep and release under new regulations
      catch_size_data_new <- catch_size_data %>%
        dplyr::left_join(regs, by = c("period2")) %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=cod_min_y2 ,1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw) %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj = dplyr::case_when(
            cod_bag_y2 > 0 ~ ifelse(csum_keep<=cod_bag_y2 & posskeep==1,1,0),
            TRUE ~ 0))
      
      #Compute keep and release under SQ regulations
      catch_size_data_base <- catch_size_data %>%
        dplyr::left_join(regs, by = c("period2")) %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=cod_min_SQ ,1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw) %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj = dplyr::case_when(
            cod_bag_SQ > 0 ~ ifelse(csum_keep<=cod_bag_SQ & posskeep==1,1,0),
            TRUE ~ 0))
      
      
      catch_size_data_new <- catch_size_data_new %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data_new <- catch_size_data_new %>%
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0))
      
      catch_size_data_base <- catch_size_data_base %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data_base <- catch_size_data_base %>%
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0))
      
      
      #sum(catch_size_data_base$keep)
      #sum(catch_size_data_new$keep)
      
      
      #  Here, I allow sue-legal harvest behavior to change with increases in the new cod min. size limit of more than 2 inches. 
      #  If the new size limit is 2 or more inches higher then the old size limit, 
      #  the  pool are fish that may be illegal harvest are within 4 inches below the minimum size limit.
      
      catch_size_data<- catch_size_data_new %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, period2, catch_draw, mode, cod_min, cod_min_y2)  %>%
        dplyr::rename(mode1=mode) %>% 
        dplyr::mutate(floor_subl_cod_harv1=floor_subl_cod_harv, diff_min=cod_min_y2-cod_min) %>%     
        dplyr::mutate(floor_subl_cod_harv1=case_when(diff_min>5~floor_subl_cod_harv-5.08, TRUE~floor_subl_cod_harv1)) %>% 
        dplyr::mutate(floor_subl_cod_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_cod_harv1~1,TRUE~0)) %>% 
        dplyr::select(-floor_subl_cod_harv1, -diff_min)
      
      sum_cod_rel<-sum(catch_size_data$release)
      sum_cod_keep<-sum(catch_size_data$keep)

      ##Now reallocate a portion of all releases as kept if needed 
      if (cod_release_2_keep==1 & sum_cod_rel>0){
        
        catch_size_data_cod_re_allocate<- catch_size_data %>%
          dplyr::filter(floor_subl_cod_harv_indicator==1) %>%
          dplyr::select(-mode1)
        
        catch_size_data_cod_re_allocate_base<- catch_size_data %>%
          dplyr::filter(floor_subl_cod_harv_indicator==0) %>%
          dplyr::select(-mode1)
        
        catch_size_data_cod_re_allocate <- catch_size_data_cod_re_allocate %>%
          dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>%
          dplyr::arrange(uniform) %>%
          dplyr::ungroup()
        
        n_row_cod_re_allocate<-nrow(catch_size_data_cod_re_allocate)
        
        # If there are more fish kept (fewer released) under the new regulations/length distribution, 
        # the number of illegal harvest is no greater than the baseline year 
        
        if(sum(catch_size_data_new$keep)>=sum(catch_size_data_base$keep)){
          n_sub_cod_kept= n_sub_cod_kept_base
        }
        
        # If there are fewer fish kept (more released) under the new regulations/length distributions, 
        # the number of illegal harvest is proportionate to (number of illegal fish kept:possible released fish) in the baseline year. 
        
        if(sum(catch_size_data_new$keep)<sum(catch_size_data_base$keep)){
          n_sub_cod_kept= round(h_star_cod_release_to_keep_variable*n_row_cod_re_allocate)
          
        }
        
        catch_size_data_cod_re_allocate <- catch_size_data_cod_re_allocate %>%
          dplyr::mutate(fishid2=1:n_row_cod_re_allocate) %>%
          dplyr::mutate(keep_new=case_when(fishid2<=n_sub_cod_kept~1, TRUE~ 0))
        
        catch_size_data_cod_re_allocate <- catch_size_data_cod_re_allocate %>%
          dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>%
          dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>%
          dplyr::rename(keep=keep_new, release=rel_new) %>% 
          dplyr::mutate(diff_min_size=cod_min_y2-cod_min) %>% 
          dplyr::mutate(cutoff = case_when(diff_min_size>=5~2, TRUE~1))
        
        setDT(catch_size_data_cod_re_allocate)
        catch_size_data_cod_re_allocate <- catch_size_data_cod_re_allocate[order(tripid, period2, catch_draw, fishid)][
          , `:=`(
            csum_keep = cumsum(keep),               # Calculate cumulative sum of keep
            bag_cutoff =  cutoff                    # Define bag cutoff
          ), by = .(tripid, period2, catch_draw)][
            , `:=`(
              release_new = as.integer(csum_keep > bag_cutoff | release == 1), # Update release flag
              keep_new = as.integer(!(csum_keep > bag_cutoff | release == 1))  # Update keep flag
            )][, .(tripid, period2, fitted_length, floor_subl_cod_harv_indicator, catch_draw, fishid, keep = keep_new, release = release_new)] # Select and rename]
        
        catch_size_data<- rbind.fill(catch_size_data_cod_re_allocate,catch_size_data_cod_re_allocate_base) %>%
          dplyr::select(-floor_subl_cod_harv_indicator)
        
      }
      
      
      ##Now reallocate a portion of all keeps as releases if needed
      if (cod_keep_2_release==1 & sum_cod_keep>0){
        
        #If all cod kept must be release, all_cod_keep_2_release==1
        if (all_cod_keep_2_release==1){
          
          catch_size_data<-catch_size_data %>%
            dplyr::mutate(rel_new = keep+release,
                          keep_new = 0) %>%
            dplyr::select(-keep, -release) %>%
            dplyr::rename(release=rel_new,  keep=keep_new)
          
        }
        
        #If not all cod kept must be release, all_cod_keep_2_release==0
        if (all_cod_keep_2_release==0){
          
          catch_size_data_cod_re_allocate<- catch_size_data %>%
            dplyr::filter(keep==1)
          
          catch_size_data_cod_re_allocate_base<- catch_size_data %>%
            dplyr::filter(keep==0)
          
          sum_keep_cod_re_allocate=sum(catch_size_data_cod_re_allocate$keep)
          n_row_cod_re_allocate<-nrow(catch_size_data_cod_re_allocate)
          
          catch_size_data_cod_re_allocate<-catch_size_data_cod_re_allocate %>%
            dplyr::mutate(uniform=runif(n_row_cod_re_allocate)) %>%
            dplyr::arrange(uniform) %>%
            dplyr::mutate(fishid2=1:n_row_cod_re_allocate)
          
          
          # If there are fewer fish released (more kept) under the new regulations/length distribution compared to the baseline, 
          # the number of voluntary release is equal to base year
          
          if(sum(catch_size_data_new$keep)>=sum(catch_size_data_base$keep)){
            n_legal_cod_rel= n_legal_cod_rel_base
          }
          
          # If there are more fish released (fewer kept) under the new regulations/length distributions, 
          # the number of voluntary release is proportionate (number of legal fish released:possible kept fish) in base year 
          
          if(sum(catch_size_data_new$keep)<sum(catch_size_data_base$keep)){
            n_legal_cod_rel<-round(h_star_cod_keep_to_release_variable*n_row_cod_re_allocate)
          }
          
          catch_size_data_cod_re_allocate<-catch_size_data_cod_re_allocate %>%
            dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_legal_cod_rel~1, TRUE~ 0))
          
          catch_size_data_cod_re_allocate<-catch_size_data_cod_re_allocate %>%
            dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>%
            dplyr::select(-keep, -release, -fishid2, -uniform) %>%
            dplyr::rename(keep=keep_new, release=rel_new)
          
          catch_size_data<-rbind.fill(catch_size_data_cod_re_allocate,catch_size_data_cod_re_allocate_base )
          
          rm(catch_size_data_cod_re_allocate, catch_size_data_cod_re_allocate_base, catch_size_data_new, catch_size_data_base)
        }
      }
      
      
      #length data 
      catch_size_data <- as.data.table(catch_size_data)
      new_size_data <- catch_size_data[, .(
        keep = sum(keep),
        release = sum(release)
      ), by = .(period2, catch_draw, tripid, fitted_length)]
      
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
      

      setDT(catch_size_data)
      
      trip_data <- catch_size_data[, .(
        tot_keep_cod_new = sum(keep),
        tot_rel_cod_new = sum(release)
      ), by = .(period2, catch_draw, tripid)]
      
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
    
    #if there is catch of only haddock 
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
    
    #if there is catch of haddock
    if (had_catch_check!=0){
      # subset trips with zero catch, as no size draws are required
      had_zero_catch <- dplyr::filter(cod_had_catch_data, tot_had_catch == 0)
      
      #keep trips with positive catch
      had_catch_data <- dplyr::filter(cod_had_catch_data, tot_had_catch > 0)
      
      #expand the sf_catch_data so that each row represents a fish
      row_inds <- seq_len(nrow(had_catch_data))
      
      had_catch_data<-had_catch_data %>%
        dplyr::slice(rep(row_inds,tot_had_catch))   %>%
        dplyr::mutate(fishid=dplyr::row_number())
      
      
      # generate lengths for each fish
      catch_size_data_had <- had_catch_data %>%
        dplyr::mutate(fitted_length = base::sample(had_size_data$length,
                                                   nrow(.),
                                                   prob = had_size_data$proj_CaL_prob_smooth,
                                                   #prob = had_size_data$fitted_prob,
                                                   replace = TRUE)) 
      
      # Create as an object the minimum size at which fish are illegally harvested.
      # This object "floor_subl_harvest" will be 2 inches below the minimum size, by mode. 
      #1) If the minimum size changes across the season, floor_subl_harvest=min(min_size) - 2.
      #2a) If the fishery is closed the entire season, floor_subl_harvest=min(min. size of the previous season) -2 
      #2b) If the fishery is closed the entire current and previous season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
      
      # 2b) below:
      
      if (floor_subl_hadd_harv>=248.9){
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
      
      # Create a variable indicating possible illegal harvests - these are fish that are (a) released due to 
      # being outside the size limit or over the bag limit and (b) longer than 2" below the minimum size limit  
      catch_size_data_had<- catch_size_data_had %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, period2, catch_draw, mode, hadd_bag_y2, hadd_min_y2, hadd_bag, hadd_min)  %>%
        dplyr::rename(mode1=mode) %>% 
        dplyr::mutate(floor_subl_hadd_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_hadd_harv~1,TRUE~0))
      
      sum_hadd_rel<-sum(catch_size_data_had$release)
      sum_hadd_kept<-sum(catch_size_data_had$keep)
      
      # Now reallocate a portion of all releases as kept if needed 
      if (hadd_release_2_keep==1 & sum_hadd_rel>0){
        
        catch_size_data_had_re_allocate<- catch_size_data_had %>%
          dplyr::filter(floor_subl_hadd_harv_indicator==1) %>%  # Subset potential illegal harvests
          dplyr::select(-mode1)
        
        catch_size_data_had_re_allocate_base<- catch_size_data_had %>%
          dplyr::filter(floor_subl_hadd_harv_indicator==0) %>%
          dplyr::select(-mode1)
        
        catch_size_data_had_re_allocate <- catch_size_data_had_re_allocate %>%
          dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>%
          dplyr::arrange(uniform)
        
        n_row_had_re_allocate<-nrow(catch_size_data_had_re_allocate) #Number of potential illegal harvests
        
        # Number of illegal harvests is proportionate to (number of illegal fish kept:possible released fish) in base year (but note 
        # there is a cap on illegal harvest per trip, see below):
        
        n_sub_had_kept=round(h_star_hadd_release_to_keep_variable*n_row_had_re_allocate)
        
        catch_size_data_had_re_allocate <- catch_size_data_had_re_allocate %>%
          dplyr::mutate(fishid2=1:n_row_had_re_allocate) %>%
          dplyr::mutate(keep_new=case_when(fishid2<=n_sub_had_kept~1, TRUE~ 0))
        
        
        #  Here, I implement a ceiling on the number of illegal harvests per trip and catch draw.
        #  In the calibration year, the ceiling is 5 for haddock and 1 for cod.
        #  Because the ceiling value has an important influence on total predicted haddock mortality,  
        #  I make it a function of the projection year haddock regulations relative to the calibration year regulations. 
        #  Based on discussions with the group, if BLs are constrained relative to the calibration year, we 
        #  assume lower illegal harvest behav
        
        #  If the new BL = the old BL, then ceiling = 5 (same)
        #  If the new BL n.e. to the old BL, I take the difference hadd_bag-hadd_bag_y2
        #  If the new BL is >= old BL, hadd_bag-hadd_bag_y2<=0, and I keep the ceiling value at 5
        #  If the new BL is < old BL hadd_bag-hadd_bag_y2>0, I set the ceiling value at max(1, 5-diff_bag). So the 
        #  fewest number of illegal harvested per trip is 1 and the max is 5. 
        
        catch_size_data_had_re_allocate <- catch_size_data_had_re_allocate %>%
          dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>%
          dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>%
          dplyr::rename(keep=keep_new, release=rel_new) %>% 
          dplyr::mutate(diff_min_size=hadd_min-hadd_min_y2, diff_bag=hadd_bag-hadd_bag_y2) %>% 
          dplyr::mutate(cutoff = case_when(diff_bag<=0~min(9, 5-diff_bag), TRUE~max(1, 5-diff_bag)))
        
        setDT(catch_size_data_had_re_allocate)
        catch_size_data_had_re_allocate <- catch_size_data_had_re_allocate[order(tripid, period2, catch_draw, fishid)][
          , `:=`(
            csum_keep = cumsum(keep),                # Calculate cumulative sum of keep
            bag_cutoff=cutoff
          ), by = .(tripid, period2, catch_draw)][
            , `:=`(
              release_new = as.integer(csum_keep > bag_cutoff | release == 1), # Update release flag
              keep_new = as.integer(!(csum_keep > bag_cutoff | release == 1))  # Update keep flag
            )][, .(tripid, period2, fitted_length, floor_subl_hadd_harv_indicator, catch_draw, fishid, keep = keep_new, release = release_new)] # Select and rename]
        
        catch_size_data_had<- rbind.fill(catch_size_data_had_re_allocate,catch_size_data_had_re_allocate_base) %>%
          dplyr::select(-floor_subl_hadd_harv_indicator)
        
        
      }
      
      ##Now reallocate a portion of all keeps as releases if needed 
      if (hadd_keep_2_release==1 & sum_hadd_kept>0){
        
        #If all hadd kept must be release, all_hadd_keep_2_release==1
        if (all_hadd_keep_2_release==1){
          
          catch_size_data_had<-catch_size_data_had %>% 
            dplyr::mutate(rel_new = keep+release, 
                          keep_new = 0) %>% 
            dplyr::select(-keep, -release) %>% 
            dplyr::rename(release=rel_new,  keep=keep_new) 
          
        }
        
        
        #If not all hadd kept must be release, all_hadd_keep_2_release==0
        if (all_hadd_keep_2_release==0){
          
          catch_size_data_had_re_allocate<- catch_size_data_had %>%
            dplyr::filter(keep==1)
          
          catch_size_data_had_re_allocate_base<- catch_size_data_had %>%
            dplyr::filter(keep==0) 
          
          sum_keep_hadd_re_allocate=sum(catch_size_data_had_re_allocate$keep)
          n_row_had_re_allocate<-nrow(catch_size_data_had_re_allocate)
          
          catch_size_data_had_re_allocate<-catch_size_data_had_re_allocate %>% 
            dplyr::mutate(uniform=runif(n_row_had_re_allocate)) %>%
            dplyr::arrange(uniform) %>% 
            dplyr::mutate(fishid2=1:n_row_had_re_allocate)
          
          # the number of voluntary release is proportionate (number of legal fish released:possible kept fish) in base year 
          n_legal_hadd_rel=round(h_star_hadd_keep_to_release_variable*n_row_had_re_allocate)
          
          catch_size_data_had_re_allocate<-catch_size_data_had_re_allocate %>% 
            dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_legal_hadd_rel~1, TRUE~ 0))
          
          catch_size_data_had_re_allocate<-catch_size_data_had_re_allocate %>% 
            dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>% 
            dplyr::select(-keep, -release, -fishid2, -uniform) %>% 
            dplyr::rename(keep=keep_new, release=rel_new)
          
          sum(catch_size_data_had$release)
          sum(catch_size_data_had$keep)
          
          catch_size_data_had<-rbind.fill(catch_size_data_had_re_allocate,catch_size_data_had_re_allocate_base )
          
          sum(catch_size_data_had$release)
          sum(catch_size_data_had$keep)
          
          rm(catch_size_data_had_re_allocate, catch_size_data_had_re_allocate_base)
        }
      }
      
      
      #length data 
      catch_size_data_had <- as.data.table(catch_size_data_had)
      new_size_data <- catch_size_data_had[, .(
        keep = sum(keep),
        release = sum(release)
      ), by = .(period2, catch_draw, tripid, fitted_length)]
      
      
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
      
      

      setDT(catch_size_data_had)
      
      trip_data_hadd <- catch_size_data_had[, .(
        tot_keep_hadd_new = sum(keep),
        tot_rel_hadd_new = sum(release)
      ), by = .(period2, catch_draw, tripid)]
      
      
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
      trip_data<-trip_data[trip_data_hadd, on = "domain2"]
      
    }
    
    #if there is catch of only cod 
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
    
    #=====================================#
    #Now merge the new trip data to the baseline trip data 
    
    trip_data<- trip_data %>% as.data.frame() %>% 
      dplyr::left_join(costs_new_all, by = c("period2","tripid", "catch_draw")) 
    
    
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
          beta_cost*cost,
        
        #  utility (base year)
        v0 = beta_sqrt_cod_keep*sqrt(tot_keep_cod_base) +
          beta_sqrt_cod_release*sqrt(tot_rel_cod_base) +
          beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_base) +
          beta_sqrt_hadd_release*sqrt(tot_rel_hadd_base) +
          beta_cost*cost)
    
    
    mean_trip_data <- trip_data %>%
      data.table::data.table() %>% 
      .[, group_index := .GRP, by = .(period2, catch_draw, tripid)]
    
    
    # Now expand the data to create two alternatives, representing the alternatives available in choice survey
    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
      tidyr::uncount(n_alt) %>%
      dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                    opt_out = ifelse(alt == 2, 1, 0))
    
    
    setDT(mean_trip_data)
    
    # Filter only alt == 2 once, and calculate vA and v0
    mean_trip_data[alt == 2, c("vA", "v0") := .(
      beta_opt_out * opt_out +
        beta_opt_out_age * (age * opt_out) +
        beta_opt_out_likely * (likely_to_fish * opt_out) +
        beta_opt_out_prefer * (fish_pref_more * opt_out)
    )]
    
    # Pre-compute exponential terms
    mean_trip_data[, `:=`(exp_vA = exp(vA), exp_v0 = exp(v0))]
    
    # Group by group_index and calculate probabilities and log-sums
    mean_trip_data[, `:=`(
      probA = exp_vA / sum(exp_vA),
      prob0 = exp_v0 / sum(exp_v0),
      log_sum_base = log(sum(exp_vA)),
      log_sum_alt = log(sum(exp_v0))
    ), by = group_index]
    
    # Calculate consumer surplus 
    mean_trip_data[, `:=`(
      CS_base = log_sum_base / -beta_cost,
      CS_alt = log_sum_alt / -beta_cost
    )]
    
    # Calculate change consumer surplus 
    mean_trip_data[, `:=`(
      change_CS = CS_alt - CS_base
    )]
    
    
    mean(mean_trip_data$change_CS)
    
    # Get rid of things we don't need.
    mean_trip_data <- mean_trip_data %>% 
      dplyr::filter(alt==1) %>% 
      dplyr::select(-c(alt, beta_cost,beta_opt_out, beta_opt_out_age, 
                       beta_opt_out_likely, beta_opt_out_prefer, #beta_sqrt_cod_hadd_keep, 
                       beta_sqrt_cod_keep, beta_sqrt_cod_release, beta_sqrt_hadd_keep, 
                       beta_sqrt_hadd_release, likely_to_fish, fish_pref_more, open, v0, vA, cost, age, 
                       exp_vA, exp_v0, log_sum_base, log_sum_alt, group_index))
    
    
    # Multiply the trip probability by each of the catch variables to get probability-weighted catch
    # Update 9/97/24 - multiply CS by probA to get probability-weighted change CS
    list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                    "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new" , "change_CS" )
    
    
    mean_trip_data<-mean_trip_data %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
      .[]
    
    
    # Multiply the trip probability in baseline year by each of the catch variables in the basleine year to get probability-weighted catch
    list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                    "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base"  )
    
    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
      .[]
    
    
    mean_trip_data_prob_catch_draw<-mean_trip_data %>% 
      dplyr::select("period2","tripid", "catch_draw", "probA")
    
    
    #Average the outcomes over catch draws 
    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c( "period","tripid", "period2", "mode")]

    mean_trip_data <- mean_trip_data %>%
      .[,lapply(.SD, base::mean), by = c("tripid", "period2"), .SDcols = all_vars]
    
    
    
    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(n_choice_occasions = rep(1,nrow(.))) %>%
      dplyr::left_join(period_names, by = c("period2"))
    
    #===============================#
    
    
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
    
    
    #===============================#
    
    mean_trip_data <- mean_trip_data %>%
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
      dplyr::ungroup()
    
    
    l_w_sum <- length_weight %>%
      dplyr::mutate(Var1 = paste0(Species, "_", mode1, "_", keep_release )) %>%
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
      
      .[, ntrips_alt := expand*probA] %>%
      .[, ntrips_base := expand*prob0] 
    
    
    #prediction_output_by_period1 contains CV and ntrips estimates by mode
    prediction_output_by_period1 <- prediction_output_by_period2 %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::group_by(mode) %>%
      dplyr::summarise(CV = sum(cv_sum),
                       ntrips = sum(ntrips_alt),
                       nchoiceoccasions=sum(expand), 
                       .groups="drop") %>%  
      dplyr::ungroup()
    
    #prediction_sum contains CV and ntrips estimates
    prediction_sum<- prediction_output_by_period1 %>%
      tidyr::pivot_longer(!c(mode), names_to = "Var", values_to = "Value") %>%
      dplyr::mutate(Var = paste0(Var, "_", mode)) %>%
      dplyr::select(!c(mode))
    
    
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
    
  })
  
  print(execution_time)
  #write_xlsx(output2, paste0(output_data_cd, "model_predictions.xlsx"))
  

