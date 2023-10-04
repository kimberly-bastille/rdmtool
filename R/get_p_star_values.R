

## Get estimate starting values for P_star calc
start_est <-  read.csv(here::here("data-raw/total AB1B2 by state mode_pstar.csv")) %>% 
  dplyr::group_by(state, mode) %>% 
  dplyr::mutate(sf_start = 1 - sf_harvest/sf_tot_cat, 
                bsb_start = 1 - bsb_harvest/bsb_tot_cat,
                scup_start = 1- scup_harvest/scup_tot_cat) %>% 
  dplyr::select(state, mode, sf_start, bsb_start, scup_start)

###MA
#starting points 
p_star_sf_MA_variable<-0.85
p_star_bsb_MA_variable<-0.82
p_star_scup_MA_variable<-0.67

repeat {
  source("calibration MA loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_MA_variable<-p_star_sf_MA_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_MA_variable<-p_star_sf_MA_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_MA_variable<-p_star_bsb_MA_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_MA_variable<-p_star_bsb_MA_variable -.005
  }
  
  if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_MA_variable<-p_star_scup_MA_variable +.005
  }
  
  if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_MA_variable<-p_star_scup_MA_variable -.005
  } 
  
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_MA<-sf_harvest_harv_diff
bsb_harvest_harv_diff_MA<-bsb_harvest_harv_diff
scup_harvest_harv_diff_MA<-scup_harvest_harv_diff

p_star_sf_MA_variable<- p_star_sf
p_star_bsb_MA_variable<- p_star_bsb
p_star_scup_MA_variable<- p_star_scup






###RI
#starting points 
p_star_sf_RI_variable<-0.875
p_star_bsb_RI_variable<-0.805
p_star_scup_RI_variable<-0.595

repeat {
  source("calibration RI loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_RI_variable<-p_star_sf_RI_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_RI_variable<-p_star_sf_RI_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_RI_variable<-p_star_bsb_RI_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_RI_variable<-p_star_bsb_RI_variable -.005
  }
  
  if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_RI_variable<-p_star_scup_RI_variable +.005
  }
  
  if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_RI_variable<-p_star_scup_RI_variable -.005
  } 
  
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_RI<-sf_harvest_harv_diff
bsb_harvest_harv_diff_RI<-bsb_harvest_harv_diff
scup_harvest_harv_diff_RI<-scup_harvest_harv_diff

p_star_sf_RI_variable<- p_star_sf
p_star_bsb_RI_variable<- p_star_bsb
p_star_scup_RI_variable<- p_star_scup




###CT
#starting points 
p_star_sf_CT_variable<-0.9
p_star_bsb_CT_variable<-0.82
p_star_scup_CT_variable<-0.56

repeat {
  source("calibration CT loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_CT_variable<-p_star_sf_CT_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_CT_variable<-p_star_sf_CT_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_CT_variable<-p_star_bsb_CT_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_CT_variable<-p_star_bsb_CT_variable -.005
  }
  
  if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_CT_variable<-p_star_scup_CT_variable +.005
  }
  
  if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_CT_variable<-p_star_scup_CT_variable -.005
  } 
  
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_CT<-sf_harvest_harv_diff
bsb_harvest_harv_diff_CT<-bsb_harvest_harv_diff
scup_harvest_harv_diff_CT<-scup_harvest_harv_diff

p_star_sf_CT_variable<- p_star_sf
p_star_bsb_CT_variable<- p_star_bsb
p_star_scup_CT_variable<- p_star_scup



###NY
#starting points 
p_star_sf_NY_variable<-0.91
p_star_bsb_NY_variable<-0.86
p_star_scup_NY_variable<-0.52

repeat {
  source("calibration NY loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_NY_variable<-p_star_sf_NY_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_NY_variable<-p_star_sf_NY_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_NY_variable<-p_star_bsb_NY_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_NY_variable<-p_star_bsb_NY_variable -.005
  }
  
  if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_NY_variable<-p_star_scup_NY_variable +.005
  }
  
  if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    p_star_scup_NY_variable<-p_star_scup_NY_variable -.005
  } 
  
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_NY<-sf_harvest_harv_diff
bsb_harvest_harv_diff_NY<-bsb_harvest_harv_diff
scup_harvest_harv_diff_NY<-scup_harvest_harv_diff

p_star_sf_NY_variable<- p_star_sf
p_star_bsb_NY_variable<- p_star_bsb
p_star_scup_NY_variable<- p_star_scup



###NJ
# old starting points 
# p_star_sf_NJ_variable<-0.89
# p_star_bsb_NJ_variable<-0.89
# p_star_scup_NJ_variable<-0.06

### Values from harvest/total_catch in start_est above
# p_star_sf_NJ_variable_fh<- 0.81434551
# p_star_bsb_NJ_variable_fh<-0.8419572
# p_star_scup_NJ_variable_fh<-0.48232862
# p_star_sf_NJ_variable_pr<-0.86414832
# p_star_bsb_NJ_variable_pr<-0.8598893
# p_star_scup_NJ_variable_pr<-0.49895278
# p_star_sf_NJ_variable_sh<-0.93668828
# p_star_bsb_NJ_variable_sh<-1
# p_star_scup_NJ_variable_sh<-1

### Values from draw8 estimate
p_star_sf_NJ_variable_fh<- 0.729
p_star_bsb_NJ_variable_fh<-0.452
p_star_scup_NJ_variable_fh<-0.422
p_star_sf_NJ_variable_pr<-0.814
p_star_bsb_NJ_variable_pr<-0.720
p_star_scup_NJ_variable_pr<-0.503
p_star_sf_NJ_variable_sh<-0.932
p_star_bsb_NJ_variable_sh<-1
p_star_scup_NJ_variable_sh<-1
#p_star_scup_NJ_variable_sh<-NA # No shore based catch of Scup

select_mode = "sh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_NJ_variable_sh
  p_star_sf <- p_star_sf_NJ_variable_sh
  p_star_scup <- p_star_scup_NJ_variable_sh
} 
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_NJ_variable_fh
  p_star_sf <- p_star_sf_NJ_variable_fh
  p_star_scup <- p_star_scup_NJ_variable_fh
} 
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_NJ_variable_pr
  p_star_sf <- p_star_sf_NJ_variable_pr
  p_star_scup <- p_star_scup_NJ_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
repeat{
  
  #pstar <- calculate_pstar_NJ(m = "sh") 
  pstar <- calibrate_rec_catch(state1 = "NJ",
                               state_no = 34,
                               #sf_catch_data_all,
                               p_star_sf,
                               p_star_bsb,
                               p_star_scup,
                               select_mode = "sh", 
                               k = k) 
  
  sf <- pstar %>% 
    dplyr::filter(species == "SF") 
  p_star_sf <- sf[[3]]
  sf_harvest_harv_diff <- sf[[9]]
  
  bsb <- pstar %>% 
    dplyr::filter(species == "BSB") 
  p_star_bsb <- bsb[[3]]
  bsb_harvest_harv_diff <- bsb[[9]]
  
  scup <- pstar %>% 
    dplyr::filter(species == "SCUP") 
  p_star_scup <- scup[[3]]
  scup_harvest_harv_diff <- scup[[9]]
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf<-p_star_sf +.005
  }

  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf<-p_star_sf -.005
  }

  # if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
  #   p_star_bsb<-p_star_bsb +.005
  # }
  # 
  # if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
  #   p_star_bsb<-p_star_bsb -.005
  # }

  # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup<-p_star_scup +.005
  # }
  # 
  # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup<-p_star_scup -.005
  # }

  print(pstar)
  #print(bsb_harvest_harv_diff)
  print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
  #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
  if ((abs(sf_harvest_harv_diff)<2)) break
  
}
  pstar_out <- pstar_out %>% 
    rbind(pstar)
}  


pstar_out_sh<- pstar_out


### FH #########
select_mode = "fh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_NJ_variable_sh
  p_star_sf <- p_star_sf_NJ_variable_sh
  p_star_scup <- p_star_scup_NJ_variable_sh
} 
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_NJ_variable_fh
  p_star_sf <- p_star_sf_NJ_variable_fh
  p_star_scup <- p_star_scup_NJ_variable_fh
} 
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_NJ_variable_pr
  p_star_sf <- p_star_sf_NJ_variable_pr
  p_star_scup <- p_star_scup_NJ_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    
    #pstar <- calculate_pstar_NJ(m = "sh") 
    pstar <- calibrate_rec_catch(state1 = "NJ",
                                 state_no = 34,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "fh", 
                                 k = k) 
    
    sf <- pstar %>% 
      dplyr::filter(species == "SF") 
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[9]]
    
    bsb <- pstar %>% 
      dplyr::filter(species == "BSB") 
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[9]]
    
    scup <- pstar %>% 
      dplyr::filter(species == "SCUP") 
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[9]]
    
    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.005
    }
    
    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.005
    }
    
     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
       p_star_bsb<-p_star_bsb +.005
     }
    
     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
       p_star_bsb<-p_star_bsb -.005
     }

     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
       p_star_scup<-p_star_scup +.005
     }
    
     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
       p_star_scup<-p_star_scup -.005
     }
    
    print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if(p_star_sf < 0) break
    if(p_star_bsb < 0) break
    if(p_star_scup < 0) break
    if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break
    
  }
  pstar_out <- pstar_out %>% 
    rbind(pstar)
}  

pstar_out_fh <- pstar_out



### PR #########
select_mode = "pr"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_NJ_variable_sh
  p_star_sf <- p_star_sf_NJ_variable_sh
  p_star_scup <- p_star_scup_NJ_variable_sh
} 
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_NJ_variable_fh
  p_star_sf <- p_star_sf_NJ_variable_fh
  p_star_scup <- p_star_scup_NJ_variable_fh
} 
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_NJ_variable_pr
  p_star_sf <- p_star_sf_NJ_variable_pr
  p_star_scup <- p_star_scup_NJ_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    
    #pstar <- calculate_pstar_NJ(m = "sh") 
    pstar <- calibrate_rec_catch(state1 = "NJ",
                                 state_no = 34,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "pr", 
                                 k = k) 
    
    sf <- pstar %>% 
      dplyr::filter(species == "SF") 
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[9]]
    
    bsb <- pstar %>% 
      dplyr::filter(species == "BSB") 
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[9]]
    
    scup <- pstar %>% 
      dplyr::filter(species == "SCUP") 
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[9]]
    
    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.005
    }
    
    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.005
    }
    
    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.005
    }
    
    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.005
    }
    
    if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup +.005
    }
    
    if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup -.005
    }
    
    #print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break
    
  }
  pstar_out <- pstar_out %>% 
    rbind(pstar)
}  



pstar_out_pr <- pstar_out





pstar_NJ <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr) #%>% 
  #dplyr::select(species, state, p_star_value, mode, run_number)

# pstar_sh2<- pstar_out_sh %>% 
  # dplyr::select(species, state, p_star_value, mode, run_number)

write.csv(pstar_NJ, file = "pstar_NJ_test1.csv")


#fh_pstar <- pstar
pr_pstar <- pstar
#sh_pstar <- pstar

p_stars_NJ <- rbind(fh_pstar, pr_pstar, sh_pstar) 

write.csv(p_stars_NJ, file = "p_star_NJ_draw3_8_23_2.csv")

###DE
#starting points 
p_star_sf_DE_variable<-0.725
p_star_bsb_DE_variable<-0.885
p_star_scup_DE_variable<-0.055

repeat {
  source("calibration DE loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_DE_variable<-p_star_sf_DE_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_DE_variable<-p_star_sf_DE_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_DE_variable<-p_star_bsb_DE_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_DE_variable<-p_star_bsb_DE_variable -.005
  }
  
  # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup_DE_variable<-p_star_scup_DE_variable +.005
  # }
  # 
  # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup_DE_variable<-p_star_scup_DE_variable -.005
  # } 
  
  #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
  if ((abs(sf_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_DE<-sf_harvest_harv_diff
bsb_harvest_harv_diff_DE<-bsb_harvest_harv_diff
scup_harvest_harv_diff_DE<-scup_harvest_harv_diff

p_star_sf_DE_variable<- p_star_sf
p_star_bsb_DE_variable<- p_star_bsb
p_star_scup_DE_variable<- p_star_scup






###MD
#starting points 
p_star_sf_MD_variable<-0.915
p_star_bsb_MD_variable<-0.94
p_star_scup_MD_variable<-0.65

repeat {
  source("calibration MD loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_MD_variable<-p_star_sf_MD_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_MD_variable<-p_star_sf_MD_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_MD_variable<-p_star_bsb_MD_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_MD_variable<-p_star_bsb_MD_variable -.005
  }
  
  # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup_MD_variable<-p_star_scup_MD_variable +.005
  # }
  # 
  # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup_MD_variable<-p_star_scup_MD_variable -.005
  # } 
  
  #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_MD<-sf_harvest_harv_diff
bsb_harvest_harv_diff_MD<-bsb_harvest_harv_diff
scup_harvest_harv_diff_MD<-scup_harvest_harv_diff

p_star_sf_MD_variable<- p_star_sf
p_star_bsb_MD_variable<- p_star_bsb
p_star_scup_MD_variable<- p_star_scup


###VA
#starting points 
p_star_sf_VA_variable<-0.885
p_star_bsb_VA_variable<-0.875
p_star_scup_VA_variable<-0.65

repeat {
  source("calibration VA loop.R")
  
  
  if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_VA_variable<-p_star_sf_VA_variable +.005
  }
  
  if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    p_star_sf_VA_variable<-p_star_sf_VA_variable -.005
  }
  
  if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_VA_variable<-p_star_bsb_VA_variable +.005
  }
  
  if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    p_star_bsb_VA_variable<-p_star_bsb_VA_variable -.005
  }
  
  # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup_VA_variable<-p_star_scup_VA_variable +.005
  # }
  # 
  # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
  #   p_star_scup_VA_variable<-p_star_scup_VA_variable -.005
  # } 
  
  #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
  
}

sf_harvest_harv_diff_VA<-sf_harvest_harv_diff
bsb_harvest_harv_diff_VA<-bsb_harvest_harv_diff
scup_harvest_harv_diff_VA<-scup_harvest_harv_diff

p_star_sf_VA_variable<- p_star_sf
p_star_bsb_VA_variable<- p_star_bsb
p_star_scup_VA_variable<- p_star_scup






p_star_sf_MA
p_star_sf_RI
p_star_sf_CT
p_star_sf_NY
p_star_sf_NJ
p_star_sf_DE
p_star_sf_MD
p_star_sf_VA
p_star_sf_NC


p_star_bsb_MA
p_star_bsb_RI
p_star_bsb_CT
p_star_bsb_NY
p_star_bsb_NJ
p_star_bsb_DE
p_star_bsb_MD
p_star_bsb_VA
p_star_bsb_NC


p_star_scup_MA
p_star_scup_RI
p_star_scup_CT
p_star_scup_NY
p_star_scup_NJ
p_star_scup_DE
p_star_scup_MD
p_star_scup_VA
p_star_scup_NC