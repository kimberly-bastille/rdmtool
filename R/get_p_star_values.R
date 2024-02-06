

## Get estimate starting values for P_star calc
start_est <-  read.csv(here::here("data-raw/total AB1B2 by state mode_pstar.csv")) %>% 
  dplyr::group_by(state, mode) %>% 
  dplyr::mutate(sf_start = 1 - sf_harvest/sf_tot_cat, 
                bsb_start = 1 - bsb_harvest/bsb_tot_cat,
                scup_start = 1- scup_harvest/scup_tot_cat) %>% 
  dplyr::select(state, mode, sf_start, bsb_start, scup_start)

### Massachusetts

# # ### Values from draw8 estimate
# p_star_sf_MA_variable_fh<- 0.71466680
# p_star_bsb_MA_variable_fh<-0.8812922
# p_star_scup_MA_variable_fh<-0.26348051
# p_star_sf_MA_variable_pr<-0.78951940
# p_star_bsb_MA_variable_pr<-0.8725747
# p_star_scup_MA_variable_pr<-0.64309893
# p_star_sf_MA_variable_sh<-1
# p_star_bsb_MA_variable_sh<-1
# p_star_scup_MA_variable_sh<-0.55955308
# 
# 
# select_mode = "sh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_MA_variable_sh
#   p_star_sf <- p_star_sf_MA_variable_sh
#   p_star_scup <- p_star_scup_MA_variable_sh
# }
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_MA_variable_fh
#   p_star_sf <- p_star_sf_MA_variable_fh
#   p_star_scup <- p_star_scup_MA_variable_fh
# }
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_MA_variable_pr
#   p_star_sf <- p_star_sf_MA_variable_pr
#   p_star_scup <- p_star_scup_MA_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
# 
#     pstar <- calibrate_rec_catch(state1 = "MA",
#                                  state_no = 25,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "sh",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     # if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#     #   p_star_sf<-p_star_sf +.005
#     # }
#     #
#     # if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#     #   p_star_sf<-p_star_sf -.005
#     # }
# 
#     # if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#     #   p_star_bsb<-p_star_bsb +.005
#     # }
#     #
#     # if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#     #   p_star_bsb<-p_star_bsb -.005
#     # }
# 
#     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup +.005
#     }
# 
#     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup -.005
#     }
# 
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     if ((abs(scup_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# 
# pstar_out_sh<- pstar_out
# 
# 
# ### FH #########
# select_mode = "fh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_MA_variable_sh
#   p_star_sf <- p_star_sf_MA_variable_sh
#   p_star_scup <- p_star_scup_MA_variable_sh
# }
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_MA_variable_fh
#   p_star_sf <- p_star_sf_MA_variable_fh
#   p_star_scup <- p_star_scup_MA_variable_fh
# }
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_MA_variable_pr
#   p_star_sf <- p_star_sf_MA_variable_pr
#   p_star_scup <- p_star_scup_MA_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
# 
#     #pstar <- calculate_pstar_NJ(m = "sh")
#     pstar <- calibrate_rec_catch(state1 = "MA",
#                                  state_no = 25,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "fh",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.005
#     }
# 
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.005
#     }
# 
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.005
#     }
# 
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.005
#     }
# 
#     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup +.005
#     }
# 
#     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup -.005
#     }
# 
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     if(p_star_sf < 0) break
#     if(p_star_bsb < 0) break
#     if(p_star_scup < 0) break
#     if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# pstar_out_fh <- pstar_out
# 
# 
# 
# ### PR #########
# select_mode = "pr"
# 
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_MA_variable_pr
#   p_star_sf <- p_star_sf_MA_variable_pr
#   p_star_scup <- p_star_scup_MA_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
#     pstar <- calibrate_rec_catch(state1 = "MA",
#                                  state_no = 25,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "pr",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.005
#     }
# 
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.005
#     }
# 
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.005
#     }
# 
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.005
#     }
# 
#     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup +.005
#     }
# 
#     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup -.005
#     }
# 
#     #print(pstar)
#     #print(bsb_harvest_harv_diff)
#     #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# 
# 
# pstar_out_pr <- pstar_out
# 
# 
# pstar_MA <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)
# 
# write.csv(pstar_MA, file = "pstar_MA_test1.csv")









##RI
#starting points
## Values from draw8 estimate
p_star_sf_RI_variable_fh<- 0.55619810
p_star_bsb_RI_variable_fh<-0.8119615
p_star_scup_RI_variable_fh<-0.46444545
p_star_sf_RI_variable_pr<-0.76866190
p_star_bsb_RI_variable_pr<-0.9016561
p_star_scup_RI_variable_pr<-0.57585542
p_star_sf_RI_variable_sh<-0.98714088
#p_star_sf_RI_variable_sh<-0.98 # starting value too close to 1
p_star_bsb_RI_variable_sh<-0.9891113
p_star_scup_RI_variable_sh<-0.35087106


select_mode = "sh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_RI_variable_sh
  p_star_sf <- p_star_sf_RI_variable_sh
  p_star_scup <- p_star_scup_RI_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_RI_variable_fh
  p_star_sf <- p_star_sf_RI_variable_fh
  p_star_scup <- p_star_scup_RI_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_RI_variable_pr
  p_star_sf <- p_star_sf_RI_variable_pr
  p_star_scup <- p_star_scup_RI_variable_pr
}

pstar_out <- data.frame()

for(k in 43:100){
  repeat{

    pstar <- calibrate_rec_catch(state1 = "RI",
                                 state_no = 44,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "sh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]
    sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)


    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]
    bsb_model_mrip_diff <- abs(bsb$tot_keep_model - bsb$harvest_MRIP)

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
       p_star_sf<-p_star_sf +.0002
     }

     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
       p_star_sf<-p_star_sf -.0002
     }

     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
       p_star_bsb<-p_star_bsb +.0002
     }

     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
       p_star_bsb<-p_star_bsb -.0002
    }

    if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup +.005
    }

    if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup -.005
    }

    print(pstar)
    #print(bsb_harvest_harv_diff)
    print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if (((abs(sf_harvest_harv_diff)<2)|sf_model_mrip_diff < 50) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(scup_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}


pstar_out_sh<- pstar_out


### FH #########
select_mode = "fh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_RI_variable_sh
  p_star_sf <- p_star_sf_RI_variable_sh
  p_star_scup <- p_star_scup_RI_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_RI_variable_fh
  p_star_sf <- p_star_sf_RI_variable_fh
  p_star_scup <- p_star_scup_RI_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_RI_variable_pr
  p_star_sf <- p_star_sf_RI_variable_pr
  p_star_scup <- p_star_scup_RI_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    #pstar <- calculate_pstar_NJ(m = "sh")
    pstar <- calibrate_rec_catch(state1 = "RI",
                                 state_no = 44,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "fh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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

if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_RI_variable_pr
  p_star_sf <- p_star_sf_RI_variable_pr
  p_star_scup <- p_star_scup_RI_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    pstar <- calibrate_rec_catch(state1 = "RI",
                                 state_no = 44,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "pr",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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


pstar_RI <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)

write.csv(pstar_RI, file = "pstar_RI_test1.csv")


# ###CT
#starting points
### Values from draw8 estimate
p_star_sf_CT_variable_fh<- 0.81736043
p_star_bsb_CT_variable_fh<-0.7085026
p_star_scup_CT_variable_fh<-0.30864155
p_star_sf_CT_variable_pr<-0.90095397
p_star_bsb_CT_variable_pr<-0.8963786
p_star_scup_CT_variable_pr<-0.64885454
p_star_sf_CT_variable_sh<-0.53516571
p_star_bsb_CT_variable_sh<-1
p_star_scup_CT_variable_sh<-0.68448140
#
#
select_mode = "sh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_CT_variable_sh
  p_star_sf <- p_star_sf_CT_variable_sh
  p_star_scup <- p_star_scup_CT_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_CT_variable_fh
  p_star_sf <- p_star_sf_CT_variable_fh
  p_star_scup <- p_star_scup_CT_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_CT_variable_pr
  p_star_sf <- p_star_sf_CT_variable_pr
  p_star_scup <- p_star_scup_CT_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    pstar <- calibrate_rec_catch(state1 = "CT",
                                 state_no = 9,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "sh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.002
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.002
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
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(scup_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}


pstar_out_sh<- pstar_out
#
#
# ### FH #########
select_mode = "fh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_CT_variable_sh
  p_star_sf <- p_star_sf_CT_variable_sh
  p_star_scup <- p_star_scup_CT_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_CT_variable_fh
  p_star_sf <- p_star_sf_CT_variable_fh
  p_star_scup <- p_star_scup_CT_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_CT_variable_pr
  p_star_sf <- p_star_sf_CT_variable_pr
  p_star_scup <- p_star_scup_CT_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    pstar <- calibrate_rec_catch(state1 = "CT",
                                 state_no = 9,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "fh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]
    sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.001
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.001
    }

    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.0001
    }

    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.0001
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
    #if(p_star_sf < 0) break
    #if(p_star_bsb < 0) break
    #if(p_star_scup < 0) break
    if ((abs(sf_harvest_harv_diff)<2)|(sf_model_mrip_diff < 50) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(bsb_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}

pstar_out_fh <- pstar_out
#
#
#
### PR #########
select_mode = "pr"

if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_CT_variable_pr
  p_star_sf <- p_star_sf_CT_variable_pr
  p_star_scup <- p_star_scup_CT_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    pstar <- calibrate_rec_catch(state1 = "CT",
                                 state_no = 9,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "pr",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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


pstar_CT <- rbind(pstar_out_sh,  pstar_out_fh )#, pstar_out_pr)

write.csv(pstar_CT, file = "pstar_CT_test1.csv")
# pstar_CT<- read.csv(here::here("pstars/pstar_CT_test1.csv")) %>%
#   dplyr::filter(!mode == "fh") %>%
#   dplyr::select(!x) %>%
#   rbind(pstar_out_fh)
# write.csv(pstar_CT, file = "pstar_CT_test4.csv")

### NY
# starting points 
# ### Values from draw8 estimate
# p_star_sf_NY_variable_fh<- 0.82768161
# p_star_bsb_NY_variable_fh<-0.7712775
# p_star_scup_NY_variable_fh<-0.27222176
# p_star_sf_NY_variable_pr<-0.91062808
# p_star_bsb_NY_variable_pr<-0.8841262
# p_star_scup_NY_variable_pr<-0.48319410
# p_star_sf_NY_variable_sh<-0.95724892
# #p_star_bsb_NY_variable_sh<-0.9955043
# p_star_bsb_NY_variable_sh<-0.998
# p_star_scup_NY_variable_sh<-0.52660397
# # 
# # 
# select_mode = "sh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_NY_variable_sh
#   p_star_sf <- p_star_sf_NY_variable_sh
#   p_star_scup <- p_star_scup_NY_variable_sh
# }
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_NY_variable_fh
#   p_star_sf <- p_star_sf_NY_variable_fh
#   p_star_scup <- p_star_scup_NY_variable_fh
# }
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_NY_variable_pr
#   p_star_sf <- p_star_sf_NY_variable_pr
#   p_star_scup <- p_star_scup_NY_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
# 
#     pstar <- calibrate_rec_catch(state1 = "NY",
#                                  state_no = 36,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "sh",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.001
#     }
# 
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.001
#     }
# 
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.00005
#     }
# 
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.00005
#     }
# 
#     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup +.005
#     }
# 
#     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup -.005
#     }
# 
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(scup_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# 
# pstar_out_sh<- pstar_out
# #write.csv(pstar_out, file = "pstar_NY_1_53.csv")
# 
# ### FH #########
# select_mode = "fh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_NY_variable_sh
#   p_star_sf <- p_star_sf_NY_variable_sh
#   p_star_scup <- p_star_scup_NY_variable_sh
# }
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_NY_variable_fh
#   p_star_sf <- p_star_sf_NY_variable_fh
#   p_star_scup <- p_star_scup_NY_variable_fh
# }
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_NY_variable_pr
#   p_star_sf <- p_star_sf_NY_variable_pr
#   p_star_scup <- p_star_scup_NY_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
# 
#     #pstar <- calculate_pstar_NJ(m = "sh")
#     pstar <- calibrate_rec_catch(state1 = "NY",
#                                  state_no = 36,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "fh",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.005
#     }
# 
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.005
#     }
# 
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.005
#     }
# 
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.005
#     }
# 
#     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup +.005
#     }
# 
#     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup -.005
#     }
# 
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     if(p_star_sf < 0) break
#     if(p_star_bsb < 0) break
#     if(p_star_scup < 0) break
#     if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# pstar_out_fh <- pstar_out
# 
# 
# 
# ### PR #########
# select_mode = "pr"
# 
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_NY_variable_pr
#   p_star_sf <- p_star_sf_NY_variable_pr
#   p_star_scup <- p_star_scup_NY_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 48:100){
#   repeat{
#     pstar <- calibrate_rec_catch(state1 = "NY",
#                                  state_no = 36,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "pr",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.001
#     }
# 
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.001
#     }
# 
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.005
#     }
# 
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.005
#     }
# 
#     if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup +.005
#     }
# 
#     if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#       p_star_scup<-p_star_scup -.005
#     }
# 
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# 
# 
# pstar_out_pr <- pstar_out
# pstar<- read.csv(here::here("pstar_NY_test3.csv")) %>% 
#   dplyr::select(!X)
# pstar_NY <- rbind(pstar, pstar_out_pr)
# 
# #pstar_NY <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)
# 
# write.csv(pstar_NY, file = "pstar_NY_test3.csv")


# 
###NJ

### Values from draw8 estimate
p_star_sf_NJ_variable_fh<- 0.81434551
p_star_bsb_NJ_variable_fh<-0.8419572
p_star_scup_NJ_variable_fh<-0.48232862
p_star_sf_NJ_variable_pr<-0.86414832
p_star_bsb_NJ_variable_pr<-	0.8598893
p_star_scup_NJ_variable_pr<-0.49895278
p_star_sf_NJ_variable_sh<-0.93668828
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
  sf_harvest_harv_diff <- sf[[11]]

  bsb <- pstar %>%
    dplyr::filter(species == "BSB")
  p_star_bsb <- bsb[[3]]
  bsb_harvest_harv_diff <- bsb[[11]]

  scup <- pstar %>%
    dplyr::filter(species == "SCUP")
  p_star_scup <- scup[[3]]
  scup_harvest_harv_diff <- scup[[11]]

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
  if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
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
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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


pstar_NJ <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)

write.csv(pstar_NJ, file = "pstar_NJ_test1.csv")


#fh_pstar <- pstar
# pr_pstar <- pstar
# #sh_pstar <- pstar
# 
# p_stars_NJ <- rbind(fh_pstar, pr_pstar, sh_pstar) 
# 
# write.csv(p_stars_NJ, file = "p_star_NJ_test1.csv")




##DE
#starting points
## Values from draw8 estimate
p_star_sf_DE_variable_fh<- 0.65310390
p_star_bsb_DE_variable_fh<-0.6814807
p_star_scup_DE_variable_fh<-0.28664550
p_star_sf_DE_variable_pr<-0.79363383
p_star_bsb_DE_variable_pr<-0.8134801
p_star_scup_DE_variable_pr<-0.78883711
p_star_sf_DE_variable_sh<-0.97001462
p_star_bsb_DE_variable_sh<-1
p_star_scup_DE_variable_sh<-1


select_mode = "sh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_DE_variable_sh
  p_star_sf <- p_star_sf_DE_variable_sh
  p_star_scup <- p_star_scup_DE_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_DE_variable_fh
  p_star_sf <- p_star_sf_DE_variable_fh
  p_star_scup <- p_star_scup_DE_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_DE_variable_pr
  p_star_sf <- p_star_sf_DE_variable_pr
  p_star_scup <- p_star_scup_DE_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    pstar <- calibrate_rec_catch(state1 = "DE",
                                 state_no = 10,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "sh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.001
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.001
    }

    # if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
    #   p_star_bsb<-p_star_bsb +.005
    # }
    #
    # if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
    #   p_star_bsb<-p_star_bsb -.005
    # }
    #
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
    if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}


pstar_out_sh<- pstar_out


### FH #########
select_mode = "fh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_DE_variable_sh
  p_star_sf <- p_star_sf_DE_variable_sh
  p_star_scup <- p_star_scup_DE_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_DE_variable_fh
  p_star_sf <- p_star_sf_DE_variable_fh
  p_star_scup <- p_star_scup_DE_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_DE_variable_pr
  p_star_sf <- p_star_sf_DE_variable_pr
  p_star_scup <- p_star_scup_DE_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    #pstar <- calculate_pstar_NJ(m = "sh")
    pstar <- calibrate_rec_catch(state1 = "DE",
                                 state_no = 10,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "fh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]
    sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]
    scup_model_mrip_diff <- abs(scup$tot_keep_model - scup$harvest_MRIP)

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.001
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.001
    }

    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.005
    }

    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.005
    }

    if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup +.001
    }

    if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup -.001
    }

    print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if(p_star_sf < 0) break
    if(p_star_bsb < 0) break
    if(p_star_scup < 0) break
    if ((abs((sf_harvest_harv_diff)<2)|(sf_model_mrip_diff < 50)) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)|(sf_model_mrip_diff < 50)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}

pstar_out_fh <- pstar_out



### PR #########
select_mode = "pr"

if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_DE_variable_pr
  p_star_sf <- p_star_sf_DE_variable_pr
  p_star_scup <- p_star_scup_DE_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    pstar <- calibrate_rec_catch(state1 = "DE",
                                 state_no = 10,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "pr",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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
      p_star_scup<-p_star_scup +.001
    }

    if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup -.001
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


pstar_DE <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)

write.csv(pstar_DE, file = "pstar_DE_test1.csv")






###MD
#starting points
### Values from draw8 estimate
p_star_sf_MD_variable_fh<- 0.71466680
p_star_bsb_MD_variable_fh<-0.8812922
p_star_scup_MD_variable_fh<-0.26348051
p_star_scup_MD_variable_fh<-0.99
p_star_sf_MD_variable_pr<-0.78951940
p_star_bsb_MD_variable_pr<-0.8725747
p_star_scup_MD_variable_pr<-0.64309893
p_star_sf_MD_variable_sh<-0.98
p_star_bsb_MD_variable_sh<-1
p_star_scup_MD_variable_sh<-0.55955308


# select_mode = "sh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_MD_variable_sh
#   p_star_sf <- p_star_sf_MD_variable_sh
#   p_star_scup <- p_star_scup_MD_variable_sh
# }
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_MD_variable_fh
#   p_star_sf <- p_star_sf_MD_variable_fh
#   p_star_scup <- p_star_scup_MD_variable_fh
# }
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_MD_variable_pr
#   p_star_sf <- p_star_sf_MD_variable_pr
#   p_star_scup <- p_star_scup_MD_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
# 
#     pstar <- calibrate_rec_catch(state1 = "MD",
#                                  state_no = 24,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "sh",
#                                  k = k)
# 
#     sf <- pstar %>%
#       dplyr::filter(species == "SF")
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
# 
#     bsb <- pstar %>%
#       dplyr::filter(species == "BSB")
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
# 
#     scup <- pstar %>%
#       dplyr::filter(species == "SCUP")
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
# 
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.001
#     }
# 
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.001
#     }
# 
#     # if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#     #   p_star_bsb<-p_star_bsb +.005
#     # }
#     #
#     # if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#     #   p_star_bsb<-p_star_bsb -.005
#     # }
#     #
#     # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup +.005
#     # }
#     # 
#     # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup -.005
#     # }
# 
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     if ((abs(sf_harvest_harv_diff)<2)) break
# 
#   }
#   pstar_out <- pstar_out %>%
#     rbind(pstar)
# }
# 
# 
# pstar_out_sh<- pstar_out


### FH #########
select_mode = "fh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_MD_variable_sh
  p_star_sf <- p_star_sf_MD_variable_sh
  p_star_scup <- p_star_scup_MD_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_MD_variable_fh
  p_star_sf <- p_star_sf_MD_variable_fh
  p_star_scup <- p_star_scup_MD_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_MD_variable_pr
  p_star_sf <- p_star_sf_MD_variable_pr
  p_star_scup <- p_star_scup_MD_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    #pstar <- calculate_pstar_NJ(m = "sh")
    pstar <- calibrate_rec_catch(state1 = "MD",
                                 state_no = 24,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "fh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]
    scup_model_mrip_diff <- abs(scup$tot_keep_model - scup$harvest_MRIP)

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.001
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.001
    }

    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.005
    }

    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.005
    }

    # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    #   p_star_scup<-p_star_scup +.001
    # }
    # 
    # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    #   p_star_scup<-p_star_scup -.001
    # }

    print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if(p_star_sf < 0) break
    if(p_star_bsb < 0) break
    if(p_star_scup < 0) break
    if((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}

pstar_out_fh <- pstar_out



### PR #########
select_mode = "pr"

if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_MD_variable_pr
  p_star_sf <- p_star_sf_MD_variable_pr
  p_star_scup <- p_star_scup_MD_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    pstar <- calibrate_rec_catch(state1 = "MD",
                                 state_no = 24,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "pr",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

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

    # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    #   p_star_scup<-p_star_scup +.005
    # }
    # 
    # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    #   p_star_scup<-p_star_scup -.005
    # }

    print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}



pstar_out_pr <- pstar_out


pstar_MD <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)

write.csv(pstar_MD, file = "pstar_MD_test1.csv")

###VA
# #starting points 
# ### Values from draw8 estimate
# p_star_sf_VA_variable_fh<- 0.08985999
# p_star_bsb_VA_variable_fh<-0.6313503
# p_star_scup_VA_variable_fh<-1
# p_star_sf_VA_variable_pr<-0.73702044
# p_star_bsb_VA_variable_pr<-0.8367242
# p_star_scup_VA_variable_pr<-1
# p_star_sf_VA_variable_sh<-0.87346700
# p_star_bsb_VA_variable_sh<-1
# p_star_scup_VA_variable_sh<-1
# 
# 
# select_mode = "sh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_VA_variable_sh
#   p_star_sf <- p_star_sf_VA_variable_sh
#   p_star_scup <- p_star_scup_VA_variable_sh
# } 
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_VA_variable_fh
#   p_star_sf <- p_star_sf_VA_variable_fh
#   p_star_scup <- p_star_scup_VA_variable_fh
# } 
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_VA_variable_pr
#   p_star_sf <- p_star_sf_VA_variable_pr
#   p_star_scup <- p_star_scup_VA_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
#     
#     pstar <- calibrate_rec_catch(state1 = "VA",
#                                  state_no = 51,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "sh", 
#                                  k = k) 
#     
#     sf <- pstar %>% 
#       dplyr::filter(species == "SF") 
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
#     
#     bsb <- pstar %>% 
#       dplyr::filter(species == "BSB") 
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
#     
#     scup <- pstar %>% 
#       dplyr::filter(species == "SCUP") 
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
#     
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.005
#     }
#     
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.005
#     }
#     
#     # if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#     #   p_star_bsb<-p_star_bsb +.005
#     # }
#     # 
#     # if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#     #   p_star_bsb<-p_star_bsb -.005
#     # }
#     # 
#     # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup +.005
#     # }
#     # 
#     # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup -.005
#     # }
#     
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     if ((abs(sf_harvest_harv_diff)<2)) break
#     
#   }
#   pstar_out <- pstar_out %>% 
#     rbind(pstar)
# }  
# 
# 
# pstar_out_sh<- pstar_out
# 
# 
# ### FH #########
# select_mode = "fh"
# 
# if(select_mode == "sh"){
#   p_star_bsb <- p_star_bsb_VA_variable_sh
#   p_star_sf <- p_star_sf_VA_variable_sh
#   p_star_scup <- p_star_scup_VA_variable_sh
# } 
# if(select_mode == "fh"){
#   p_star_bsb <- p_star_bsb_VA_variable_fh
#   p_star_sf <- p_star_sf_VA_variable_fh
#   p_star_scup <- p_star_scup_VA_variable_fh
# } 
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_VA_variable_pr
#   p_star_sf <- p_star_sf_VA_variable_pr
#   p_star_scup <- p_star_scup_VA_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
#     
#     #pstar <- calculate_pstar_NJ(m = "sh") 
#     pstar <- calibrate_rec_catch(state1 = "VA",
#                                  state_no = 51,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "fh", 
#                                  k = k) 
#     
#     sf <- pstar %>% 
#       dplyr::filter(species == "SF") 
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
#     sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)
#     
#     bsb <- pstar %>% 
#       dplyr::filter(species == "BSB") 
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
#     
#     scup <- pstar %>% 
#       dplyr::filter(species == "SCUP") 
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
#     
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.002
#     }
#     
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.002
#     }
#     
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.005
#     }
#     
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.005
#     }
#     
#     # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup +.005
#     # }
#     # 
#     # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup -.005
#     # }
#     
#     print(pstar)
#     #print(bsb_harvest_harv_diff)
#     #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     if(p_star_sf < 0) break
#     if(p_star_bsb < 0) break
#     if(p_star_scup < 0) break
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     if ((abs(sf_harvest_harv_diff)<2|sf_model_mrip_diff < 50) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2)) break
#     
#   }
#   pstar_out <- pstar_out %>% 
#     rbind(pstar)
# }  
# 
# pstar_out_fh <- pstar_out
# 
# 
# 
# ### PR #########
# select_mode = "pr"
# 
# if(select_mode == "pr"){
#   p_star_bsb <- p_star_bsb_VA_variable_pr
#   p_star_sf <- p_star_sf_VA_variable_pr
#   p_star_scup <- p_star_scup_VA_variable_pr
# }
# 
# pstar_out <- data.frame()
# 
# for(k in 1:100){
#   repeat{
#     pstar <- calibrate_rec_catch(state1 = "VA",
#                                  state_no = 51,
#                                  #sf_catch_data_all,
#                                  p_star_sf,
#                                  p_star_bsb,
#                                  p_star_scup,
#                                  select_mode = "pr", 
#                                  k = k) 
#     
#     sf <- pstar %>% 
#       dplyr::filter(species == "SF") 
#     p_star_sf <- sf[[3]]
#     sf_harvest_harv_diff <- sf[[11]]
#     
#     bsb <- pstar %>% 
#       dplyr::filter(species == "BSB") 
#     p_star_bsb <- bsb[[3]]
#     bsb_harvest_harv_diff <- bsb[[11]]
#     
#     scup <- pstar %>% 
#       dplyr::filter(species == "SCUP") 
#     p_star_scup <- scup[[3]]
#     scup_harvest_harv_diff <- scup[[11]]
#     
#     if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf +.005
#     }
#     
#     if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
#       p_star_sf<-p_star_sf -.005
#     }
#     
#     if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb +.005
#     }
#     
#     if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
#       p_star_bsb<-p_star_bsb -.005
#     }
#     
#     # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup +.005
#     # }
#     # 
#     # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
#     #   p_star_scup<-p_star_scup -.005
#     # }
#     
#     #print(pstar)
#     #print(bsb_harvest_harv_diff)
#     #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
#     #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
#     if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
#     #if ((abs(sf_harvest_harv_diff)<2)) break
#     
#   }
#   pstar_out <- pstar_out %>% 
#     rbind(pstar)
# }  
# 
# 
# 
# pstar_out_pr <- pstar_out
# 
# # pstar_out_sh <- read.csv(here::here("pstar_VA_sh.csv")) %>% 
# #   dplyr::select(!X)
# pstar_VA <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr) 
# 
# write.csv(pstar_VA, file = "pstar_VA_test1.csv")








## North Carolina
#starting points 
# ### Values from draw8 estimate
p_star_sf_NC_variable_fh<- 0
p_star_bsb_NC_variable_fh<-0.7063293
p_star_scup_NC_variable_fh<-0.04348199
p_star_sf_NC_variable_pr<-0.01
p_star_bsb_NC_variable_pr<-0.9605864
#p_star_scup_NC_variable_pr<-0.61371757
p_star_scup_NC_variable_pr<-0.01371757
p_star_sf_NC_variable_sh<-1
p_star_bsb_NC_variable_sh<-0.9870439
p_star_scup_NC_variable_sh<-1


select_mode = "sh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_NC_variable_sh
  p_star_sf <- p_star_sf_NC_variable_sh
  p_star_scup <- p_star_scup_NC_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_NC_variable_fh
  p_star_sf <- p_star_sf_NC_variable_fh
  p_star_scup <- p_star_scup_NC_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_NC_variable_pr
  p_star_sf <- p_star_sf_NC_variable_pr
  p_star_scup <- p_star_scup_NC_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    pstar <- calibrate_rec_catch(state1 = "NC",
                                 state_no = 37,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "sh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

    # if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
    #   p_star_sf<-p_star_sf +.005
    # }
    #
    # if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
    #   p_star_sf<-p_star_sf -.005
    # }

    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.001
    }

    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.001
    }

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
    if ((abs(bsb_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}


pstar_out_sh<- pstar_out
# 
# 
### FH #########
select_mode = "fh"

if(select_mode == "sh"){
  p_star_bsb <- p_star_bsb_NC_variable_sh
  p_star_sf <- p_star_sf_NC_variable_sh
  p_star_scup <- p_star_scup_NC_variable_sh
}
if(select_mode == "fh"){
  p_star_bsb <- p_star_bsb_NC_variable_fh
  p_star_sf <- p_star_sf_NC_variable_fh
  p_star_scup <- p_star_scup_NC_variable_fh
}
if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_NC_variable_pr
  p_star_sf <- p_star_sf_NC_variable_pr
  p_star_scup <- p_star_scup_NC_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{

    #pstar <- calculate_pstar_NJ(m = "sh")
    pstar <- calibrate_rec_catch(state1 = "NC",
                                 state_no = 37,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "fh",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]
    sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.001
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.001
    }

    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.005
    }

    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.005
    }

    if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup +.001
    }

    if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
      p_star_scup<-p_star_scup -.001
    }

    print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if(p_star_sf < 0) break
    if(p_star_bsb < 0) break
    if(p_star_scup < 0) break
    if ((abs(sf_harvest_harv_diff)<2)|sf_model_mrip_diff < 50 & (abs(bsb_harvest_harv_diff)<2) & (abs(scup_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}

pstar_out_fh <- pstar_out
# 
# 
# 
### PR #########
select_mode = "pr"

if(select_mode == "pr"){
  p_star_bsb <- p_star_bsb_NC_variable_pr
  p_star_sf <- p_star_sf_NC_variable_pr
  p_star_scup <- p_star_scup_NC_variable_pr
}

pstar_out <- data.frame()

for(k in 1:100){
  repeat{
    pstar <- calibrate_rec_catch(state1 = "NC",
                                 state_no = 37,
                                 #sf_catch_data_all,
                                 p_star_sf,
                                 p_star_bsb,
                                 p_star_scup,
                                 select_mode = "pr",
                                 k = k)

    sf <- pstar %>%
      dplyr::filter(species == "SF")
    p_star_sf <- sf[[3]]
    sf_harvest_harv_diff <- sf[[11]]
    sf_model_mrip_diff <- abs(sf$tot_keep_model - sf$harvest_MRIP)

    bsb <- pstar %>%
      dplyr::filter(species == "BSB")
    p_star_bsb <- bsb[[3]]
    bsb_harvest_harv_diff <- bsb[[11]]

    scup <- pstar %>%
      dplyr::filter(species == "SCUP")
    p_star_scup <- scup[[3]]
    scup_harvest_harv_diff <- scup[[11]]
    scup_model_mrip_diff <- abs(scup$tot_keep_model - scup$harvest_MRIP)

    if (sf_harvest_harv_diff<0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf +.001
    }

    if (sf_harvest_harv_diff>0 & abs(sf_harvest_harv_diff)>1){
      p_star_sf<-p_star_sf -.001
    }

    if (bsb_harvest_harv_diff<0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb +.001
    }

    if (bsb_harvest_harv_diff>0 & abs(bsb_harvest_harv_diff)>1){
      p_star_bsb<-p_star_bsb -.001
    }

    # if (scup_harvest_harv_diff<0 & abs(scup_harvest_harv_diff)>1){
    #   p_star_scup<-p_star_scup +.005
    # }
    #
    # if (scup_harvest_harv_diff>0 & abs(scup_harvest_harv_diff)>1){
    #   p_star_scup<-p_star_scup -.005
    # }

    print(pstar)
    #print(bsb_harvest_harv_diff)
    #print((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2))
    if ((abs(sf_harvest_harv_diff)<2|sf_model_mrip_diff < 50|p_star_sf < 0) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2) & (abs(bsb_harvest_harv_diff)<2)) break
    #if ((abs(sf_harvest_harv_diff)<2)) break

  }
  pstar_out <- pstar_out %>%
    rbind(pstar)
}



pstar_out_pr <- pstar_out


pstar_NC <- rbind(pstar_out_sh,  pstar_out_fh, pstar_out_pr)
# 
write.csv(pstar_NC, file = "pstar_NC_testsh1.csv")