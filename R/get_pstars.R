

get_pstars<- function(select_mode, select_state){
  if(select_state == "MA"){
    state_code = 25
  }
  if(select_state == "RI"){
    state_code = 44
  }
  if(select_state == "CT"){
    state_code = 9
  }
  if(select_state == "NY"){
    state_code = 36
  }
  if(select_state == "NJ"){
    state_code = 34
  }
  if(select_state == "DE"){
    state_code = 10
  }
  if(select_state == "MD"){
    state_code = 24
  }
  if(select_state == "VA"){
    state_code = 51
  }
  if(select_state == "NC"){
    state_code = 37
  }
  
  start_est <-  read.csv(here::here("data-raw/total AB1B2 by state mode_pstar.csv")) %>% 
    dplyr::group_by(state, mode) %>% 
    dplyr::mutate(sf_start = 1 - sf_harvest/sf_tot_cat, 
                  bsb_start = 1 - bsb_harvest/bsb_tot_cat,
                  scup_start = 1- scup_harvest/scup_tot_cat) %>% 
    dplyr::select(state, mode, sf_start, bsb_start, scup_start)
  start_est<- start_est %>% dplyr::filter(state == select_state, 
                                          mode == select_mode) 
  
  p_star_sf <- start_est[[3]]
  p_star_bsb <- start_est[[4]]
  p_star_scup <- start_est[[5]]

  pstar_out <- data.frame()

  for(k in 1:100){
    repeat{

      pstar <- calibrate_rec_catch(state1 = select_state,
                                   state_no = state_code,
                                   #sf_catch_data_all,
                                   p_star_sf,
                                   p_star_bsb,
                                   p_star_scup,
                                   select_mode = select_mode,
                                   k = k)

      sf <- pstar %>% dplyr::filter(species == "SF")
      sf_harvest_harv_diff <- sf[[11]]
      sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)

      bsb <- pstar %>%dplyr::filter(species == "BSB")
      p_star_bsb <- bsb[[3]]
      bsb_harvest_harv_diff <- bsb[[11]]
      bsb_model_mrip_diff <- abs(bsb$tot_keep_model - bsb$harvest_MRIP)

      scup <- pstar %>% dplyr::filter(species == "SCUP")
      p_star_scup <- scup[[3]]
      scup_harvest_harv_diff <- scup[[11]]
      scup_model_mrip_diff <- abs(scup$tot_keep_model - scup$harvest_MRIP)
      
      
      if(sf_harvest_harv_diff == "NaN" & scup_harvest_harv_diff == "NaN"){
        if(bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb +.001
        }
        
        if(bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb -.001
        }
        
        if(p_star_bsb < 0) break
        if((abs(bsb_harvest_harv_diff)<2)|(bsb_model_mrip_diff < 500)) break
      }else{
      
      if(bsb_harvest_harv_diff == "NaN"  & scup_harvest_harv_diff == "NaN" ){
        if(sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf +.001
        }
        
        if(sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf -.001
        }
        if(p_star_sf < 0) break
        if((abs(sf_harvest_harv_diff)<2)|(sf_model_mrip_diff < 500)) break
      }else{
      
      
      if(sf_harvest_harv_diff == "NaN" & bsb_harvest_harv_diff == "NaN"){

        if(scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup +.001
        }
        
        if(scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup -.001
        }
        
        if(p_star_scup < 0) break
        if((abs(scup_harvest_harv_diff)<2)|(scup_model_mrip_diff < 500)) break
        
      }else{
        
      if(sf_harvest_harv_diff == "NaN"){
        if(bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb +.001
        }
        
        if(bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb -.001
        }
        
        if(scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup +.001
        }
        
        if(scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup -.001
        }
        
        if(p_star_bsb < 0) break
        if(p_star_scup < 0) break
        if((abs(bsb_harvest_harv_diff)<2)|(bsb_model_mrip_diff < 500) & 
           (abs(scup_harvest_harv_diff)<2)|(scup_model_mrip_diff < 500)) break
        
      }else{
      
      if(bsb_harvest_harv_diff == "NaN"){
        if(sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf +.001
        }
        
        if(sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf -.001
        }
        
        if(scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup +.001
        }
        
        if(scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup -.001
        }
        
        if(p_star_sf < 0) break
        if(p_star_scup < 0) break
        if((abs(sf_harvest_harv_diff)<2)|(sf_model_mrip_diff < 500) & 
           (abs(scup_harvest_harv_diff)<2)|(scup_model_mrip_diff < 500)) break
        
      }else{
      
      if(scup_harvest_harv_diff == "NaN"){
        if(sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup +.001
        }
        
        if(sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf -.001
        }
        
        if(bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb +.001
        }
        
        if(bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb -.001
        }
        
        if(p_star_sf < 0) break
        if(p_star_bsb < 0) break
        if((abs(sf_harvest_harv_diff)<2)|(sf_model_mrip_diff < 500) & 
           (abs(bsb_harvest_harv_diff)<2)|(bsb_model_mrip_diff < 500)) break
        
      }else{ 
        
        if(sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf +.001
        }
        
        if(sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
          p_star_sf<-p_star_sf -.001
        }
        
        if(bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb +.001
        }
        
        if(bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
          p_star_bsb<-p_star_bsb -.001
        }
        
        if(scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup +.001
        }
        
        if(scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
          p_star_scup<-p_star_scup -.001
        }
        
        if(p_star_sf < 0) break
        if(p_star_bsb < 0) break
        if(p_star_scup < 0) break
        if((abs(sf_harvest_harv_diff)<2)|(sf_model_mrip_diff < 500) & 
           (abs(bsb_harvest_harv_diff)<2)|(bsb_model_mrip_diff < 500) & 
           (abs(scup_harvest_harv_diff)<2)|(scup_model_mrip_diff < 500)) break
        
      }}}}}}}
     

      print(pstar)

        pstar_out <- pstar_out %>%
          rbind(pstar)
      }

    return(pstar_out)
}
      
      
      
      
      
      