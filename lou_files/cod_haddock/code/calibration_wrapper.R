
#This file pulls in the data from step 1, i.e., the differences between model simulated harvest 
#and MRIP estimates of harvest, and re-runs the calibration model but this time adjusts per-trip
#outcomes until simulated harvest in numbers of fish is within 5% or 500 fish of the MRIP estimate. 

baseline_output0<-readRDS(paste0(input_data_cd, "harvest_differences_check.rds")) 

n_distinct(baseline_output0$draw)


#l_w_conversion =
cod_lw_a = 0.000005132
cod_lw_b = 3.1625
had_lw_a = 0.000009298
had_lw_b = 3.0205

Disc_mort<- readr::read_csv(paste0(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)


for(i in unique(baseline_output0$mrip_index)){
  
  p_cod_kp_2_rl<-0
  p_cod_rl_2_kp<-0
  p_hadd_kp_2_rl<-0
  p_hadd_rl_2_kp<-0
  
  baseline_output<-readRDS(paste0(input_data_cd,"harvest_differences_check.rds")) %>% 
    dplyr::filter(mrip_index==i)
  
  #Do we need to reallocate some cod/haddock keep as releases, and vice versa? Makes indicator objects
  cod_keep_2_release<-mean(baseline_output$cod_keep_2_release)
  cod_release_2_keep<-mean(baseline_output$cod_release_2_keep)
  hadd_keep_2_release<-mean(baseline_output$hadd_keep_2_release)
  hadd_release_2_keep<-mean(baseline_output$hadd_release_2_keep)
  
  #Do we need to reallocate ALL cod/haddock keep as releases? Makes indicator objects
  baseline_output<-baseline_output %>% 
    dplyr::mutate(all_cod_keep_2_release=ifelse(tot_keep_cod_model>0 & tot_keep_cod_mrip==0, 1, 0),
                  all_hadd_keep_2_release=ifelse(tot_keep_hadd_model>0 & tot_keep_hadd_mrip==0, 1, 0))
  
  all_cod_keep_2_release<-mean(baseline_output$all_cod_keep_2_release)
  all_hadd_keep_2_release<-mean(baseline_output$all_hadd_keep_2_release)
  
  
  h_star_cod_keep_to_release_variable<-0
  h_star_hadd_keep_to_release_variable<-0
  h_star_cod_release_to_keep_variable<-0
  h_star_hadd_release_to_keep_variable<-0
  
  base_cod_harv_diff<-abs(baseline_output$diff_cod_harv)
  base_hadd_harv_diff<-abs(baseline_output$diff_hadd_harv)
  base_cod_harv_diff
  base_hadd_harv_diff
  
  base_cod_harv_perc_diff<-abs((baseline_output$tot_keep_cod_model-baseline_output$tot_keep_cod_mrip)/baseline_output$tot_keep_cod_mrip)*100
  base_hadd_harv_perc_diff<-abs((baseline_output$tot_keep_hadd_model-baseline_output$tot_keep_hadd_mrip)/baseline_output$tot_keep_hadd_mrip)*100
  base_cod_harv_perc_diff
  base_hadd_harv_perc_diff
  
  base_cod_achieved<-case_when((base_cod_harv_diff<500 | base_cod_harv_perc_diff<5)~1, TRUE~0)
  base_hadd_achieved<-case_when((base_hadd_harv_diff<500 | base_hadd_harv_perc_diff<5)~1, TRUE~0)
  base_cod_achieved
  base_hadd_achieved
  
  #If all_cod/hadd_keep_2_release, then set h_stars to one (for all CE's with harvest, re-allocate harvest as release)
  if(all_cod_keep_2_release==1 & base_cod_achieved!=1){
    h_star_cod_keep_to_release_variable<-1
  }
  
  if(all_hadd_keep_2_release==1 & base_hadd_achieved!=1){
    h_star_hadd_keep_to_release_variable<-1
  }
  
  #If some portion of keep needs to be reallocated as released, then set h_stars to baseline proportion 
  if(all_cod_keep_2_release!=1 & cod_keep_2_release==1  & base_cod_achieved!=1){
    h_star_cod_keep_to_release_variable<-max(0.06, mean(baseline_output$h_star_cod_keep_to_release_variable))
  }
  
  if(all_hadd_keep_2_release!=1 & hadd_keep_2_release==1 & base_hadd_achieved!=1){
    h_star_hadd_keep_to_release_variable<-max(0.06,mean(baseline_output$h_star_hadd_keep_to_release_variable))
  }
  
  #If some release needs to be re-allocated kept, then set h_stars to baseline proportion 
  if(cod_release_2_keep==1  & base_cod_achieved!=1){
  h_star_cod_release_to_keep_variable<-max(0.06, mean(baseline_output$h_star_cod_release_to_keep_variable))
  }
  
  if(hadd_release_2_keep==1 & base_hadd_achieved!=1){
  h_star_hadd_release_to_keep_variable<-max(0.06, mean(baseline_output$h_star_hadd_release_to_keep_variable))
  }
  

  source(paste0(code_cd, "calibrate_rec_catch_hstar_code2_new.R"))
  
  
  print("new run")
  
  print("h_star_cod_keep_to_release")
  print(comparison$h_star_cod_keep_to_release_variable)
  
  print("h_star_cod_release_to_keep")
  print(comparison$h_star_cod_release_to_keep_variable)
  
  print("tot_keep_cod_model")
  print(comparison$tot_keep_cod_model)
  
  print("tot_cod_keep_mrip")
  print(comparison$tot_cod_keep_mrip)
  
  print("diff_cod_harv")
  print(comparison$diff_cod_harv)
  
  print("perc_diff_cod_harv")
  print(comparison$perc_diff_cod_harv)
  
  print("h_star_hadd_keep_to_release_variable")
  print(comparison$h_star_hadd_keep_to_release_variable)
  
  print("h_star_hadd_release_to_keep_variable")
  print(comparison$h_star_hadd_release_to_keep_variable)
  
  print("tot_keep_hadd_model")
  print(comparison$tot_keep_hadd_model)
  
  print("tot_hadd_keep_mrip")
  print(comparison$tot_hadd_keep_mrip)
  
  print("diff_hadd_harv")
  print(comparison$diff_hadd_harv)
  
  print("perc_diff_hadd_harv")
  print(comparison$perc_diff_hadd_harv)
  
 
  repeat{
    
    #For draws where release_to_keep==1:
    
    #If in the baseline run, cod harvest is less than MRIP, but in  a new run cod harvest is greater than MRIP, 
    #reduce the baseline h_star_release_to_keep value 
    
    if(cod_release_2_keep==1 & comparison$cod_achieved!=1) {
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv> 10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp -.05
      }
      
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp -.025
      }
      
      
      #If in the baseline run, cod harvest is less than MRIP, and in the new run cod harvest is still less than MRIP, 
      #increase the baseline h_star_release_to_keep value 
      if(comparison$diff_cod_harv<0 & comparison$perc_diff_cod_harv< -10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp +.06
      }
      
      if(comparison$diff_cod_harv<0 & comparison$perc_diff_cod_harv>= -10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp +.03
      }
    }
    
    
    
    #same for haddock
    if(hadd_release_2_keep==1 & comparison$hadd_achieved!=1) {
  
      
      if(comparison$diff_hadd_harv>0 & comparison$perc_diff_hadd_harv> 25 ){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp -.05
      }
      
      if(comparison$diff_hadd_harv>0 & comparison$perc_diff_hadd_harv<= 25 ){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp -.03
      }
      
      
      if(comparison$diff_hadd_harv<0 & comparison$perc_diff_hadd_harv< -25){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp +.06
      }
      
      if(comparison$diff_hadd_harv<0 & comparison$perc_diff_hadd_harv>= -25){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp +.04
      }
      
    }
    
    
    #For draws where release_to_keep==0, keep_to_release==1
    #If in the baseline run, cod harvest is less than MRIP, but in  a new run cod harvest is greater than MRIP, 
    #reduce the baseline h_star_release_to_keep value 
    
    if(cod_keep_2_release==1 & comparison$cod_achieved!=1) {
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv> 10){
        p_cod_kp_2_rl<-p_cod_kp_2_rl +.02
      }
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
        p_cod_kp_2_rl<-p_cod_kp_2_rl +.005
      }
      
      #If in the baseline run, cod harvest is less than MRIP, and in the new run cod harvest is still less than MRIP, 
      #increase the baseline h_star_release_to_keep value 
      if(comparison$diff_cod_harv<0){
        p_cod_kp_2_rl<-p_cod_kp_2_rl -.004
      }
    }
    
    
    #same for haddock
    if(hadd_keep_2_release==1 & comparison$hadd_achieved!=1) {
      
      if(comparison$diff_hadd_harv>0){
        p_hadd_kp_2_rl<-p_hadd_kp_2_rl +.005
      }
      
      if(comparison$diff_hadd_harv<0){
        p_hadd_kp_2_rl<-p_hadd_kp_2_rl -.004
      }
    }
    
    
    
    if (comparison$hadd_achieved==1 & comparison$cod_achieved==1) break
    if (comparison$hadd_achieved==0 & mean(baseline_output$hadd_release_2_keep==1) & comparison$h_star_hadd_release_to_keep_variable>1) break
    if (comparison$cod_achieved==0 & mean(baseline_output$cod_release_2_keep==1) & comparison$h_star_cod_release_to_keep_variable>1) break

    if(all_cod_keep_2_release==1){
      h_star_cod_keep_to_release_variable<-1
    }
    
    
    if(all_hadd_keep_2_release==1){
      h_star_hadd_keep_to_release_variable<-1
    }
    
    
    if(all_cod_keep_2_release!=1 & cod_keep_2_release==1 & comparison$cod_achieved!=1){
      h_star_cod_keep_to_release_variable<-mean(baseline_output$h_star_cod_keep_to_release_variable)+p_cod_kp_2_rl
    }
    
    
    if(all_hadd_keep_2_release!=1 & hadd_keep_2_release==1 & comparison$hadd_achieved!=1){
      h_star_hadd_keep_to_release_variable<-mean(baseline_output$h_star_hadd_keep_to_release_variable)+p_hadd_kp_2_rl
    }
    
    
    if(cod_release_2_keep==1 & comparison$cod_achieved!=1){
    h_star_cod_release_to_keep_variable<-mean(baseline_output$h_star_cod_release_to_keep_variable)+p_cod_rl_2_kp
    }
    
    
    if(hadd_release_2_keep==1 & comparison$hadd_achieved!=1){
    h_star_hadd_release_to_keep_variable<-mean(baseline_output$h_star_hadd_release_to_keep_variable)+p_hadd_rl_2_kp
    }
    
    
    h_star_cod_release_to_keep_variable
    h_star_hadd_release_to_keep_variable
    h_star_hadd_keep_to_release_variable
    h_star_cod_keep_to_release_variable
    
    source(paste0(code_cd, "calibrate_rec_catch_hstar_code2_new.R"))
  
    print("new run")
    
    print("h_star_cod_keep_to_release")
    print(comparison$h_star_cod_keep_to_release_variable)
    
    print("h_star_cod_release_to_keep")
    print(comparison$h_star_cod_release_to_keep_variable)
    
    print("tot_keep_cod_model")
    print(comparison$tot_keep_cod_model)
    
    print("tot_cod_keep_mrip")
    print(comparison$tot_cod_keep_mrip)
    
    print("diff_cod_harv")
    print(comparison$diff_cod_harv)
    
    print("perc_diff_cod_harv")
    print(comparison$perc_diff_cod_harv)
    
    print("h_star_hadd_keep_to_release_variable")
    print(comparison$h_star_hadd_keep_to_release_variable)
    
    print("h_star_hadd_release_to_keep_variable")
    print(comparison$h_star_hadd_release_to_keep_variable)
    
    print("tot_keep_hadd_model")
    print(comparison$tot_keep_hadd_model)
    
    print("tot_hadd_keep_mrip")
    print(comparison$tot_hadd_keep_mrip)
    
    print("diff_hadd_harv")
    print(comparison$diff_hadd_harv)
    
    print("perc_diff_hadd_harv")
    print(comparison$perc_diff_hadd_harv)
    
    
  }
  
  ##Uncomment this if you want calibration catch weights
  #source(paste0(code_cd, "calibration_catch_weights2.R"))

}


baseline_output0<-readRDS(paste0(input_data_cd, "harvest_differences_check.rds")) 
  
n_distinct(baseline_output0$draw)

check1<-data.frame() 
check2<-data.frame() 

for(i in unique(baseline_output0$mrip_index)){

  check0<-baseline_output0 %>% dplyr::filter(mrip_index==i)
  
  season1<-unique(check0$open)
  mode1<-unique(check0$mode)
  draw1<-unique(check0$draw)
  
  check1 <- feather::read_feather(paste0(iterative_input_data_cd, "comparison_", mode1,"_", season1, "_", draw1, ".feather"))  
  check2<-rbind(check1, check2)
  
}


baseline<- readRDS(paste0(input_data_cd, "harvest_differences_check.rds")) %>% 
  dplyr::select(cod_keep_2_release, cod_release_2_keep, hadd_keep_2_release, hadd_release_2_keep, draw, 
                mode, mrip_index, diff_cod_harv, diff_hadd_harv, tot_cod_catch_model, tot_hadd_catch_model, 
                tot_keep_cod_model, tot_keep_hadd_model, tot_rel_cod_model, tot_rel_hadd_model) 
colnames(baseline) <- gsub("_model", "_model_base", colnames(baseline))
colnames(baseline) <- gsub("_harv", "_harv_base", colnames(baseline))

check2<-check2 %>% 
  dplyr::left_join(baseline, by=c("draw", "mode", "mrip_index")) %>% 
  dplyr::mutate(tab=case_when((cod_achieved==0 | hadd_achieved==0)~1, TRUE~0)) %>% 
  dplyr::group_by(draw) %>% 
  dplyr::mutate(sumtab=sum(tab)) %>% 
  dplyr::filter(sumtab==0)

n_distinct(check2$draw)

check3<-check2 %>% 
  dplyr::filter((abs_perc_diff_cod_harv>5 & abs(diff_cod_harv)>500) | (abs_perc_diff_hadd_harv>5 & abs(diff_hadd_harv)>500))

saveRDS(check2, file = paste0(input_data_cd, "calibration_comparison.rds"))
n_distinct(check2$draw)


##Now we have the data for the projections stored in input_data_cd:
#pds_new_x = number of choice occasion
#comparison_x = percent of choice occasions the keep all harvest/release all harvest 
#costs_x = baseline catch levels, trip costs, and demographics. 




##Uncomment this if you want calibration catch weights
#Compile the calibration catch weights
# check1a<-data.frame()
# check2a<-data.frame()
# 
# for(i in unique(check2$mrip_index)){
#   check0a<- check2 %>% dplyr::filter(mrip_index==i)
# 
#   season1<-unique(check0a$open)
#   mode1<-unique(check0a$mode)
#   draw1<-unique(check0a$draw)
# 
#   check1a<- readRDS(paste0(iterative_input_data_cd, "calibrate_catch_wts_", mode1,"_", season1, "_", draw1, ".rds"))
#   check2a<-rbind(check1a, check2a)
# 
# }
# n_distinct(check2a$run)
# write_xlsx(check2a, paste0(input_data_cd, "calibration_catch_weights_cm.xlsx"))


