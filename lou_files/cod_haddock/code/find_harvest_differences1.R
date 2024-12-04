


MRIP_data <-   read.csv(file.path(paste0(MRIP_comparison))) %>%
  dplyr::filter(dtrip>0)

MRIP_data<-MRIP_data %>% 
  dplyr::mutate(mrip_index=1:nrow(MRIP_data))

saveRDS(MRIP_data, file = paste0(input_data_cd, "MRIP_simulated_data.rds"))


output4<-data.frame() 
output5<-data.frame() 
for(i in 1:nrow(MRIP_data)){
  
  source(paste0(code_cd, "calibrate_rec_catch_hstar_code.R"))
  
  output4<-cbind(i, diff_cod_harv, diff_hadd_harv,tot_keep_cod_model,tot_rel_cod_model,tot_keep_cod_mrip,tot_rel_cod_mrip,  
                 tot_keep_hadd_model, tot_rel_hadd_model, tot_keep_hadd_mrip, tot_rel_hadd_mrip)
  
  output5<-rbind(output5, output4)
}



saveRDS(output5, file = paste0(input_data_cd, "harvest_differences.rds"))
output5 <- readRDS(paste0(input_data_cd,"harvest_differences.rds"))


MRIP_data3<-MRIP_data %>% 
  dplyr::select(draw, mrip_index, mode, open) 

##make a variable for both species indicating if in that mode-season-draw, we need to allocate keep as release or vice versa
output6<-output5 %>% 
  dplyr::rename(mrip_index=i) %>% 
  dplyr::left_join(MRIP_data3, by=c("mrip_index")) %>% 
  dplyr::mutate(cod_release_2_keep=ifelse(diff_cod_harv<0, 1, 0), 
                cod_keep_2_release=ifelse(diff_cod_harv>0, 1, 0),
                hadd_release_2_keep=ifelse(diff_hadd_harv<0, 1, 0), 
                hadd_keep_2_release=ifelse(diff_hadd_harv>0, 1, 0))
output6<-output6 %>% 
  dplyr::relocate(mrip_index, draw, mode, open )

saveRDS(output6, file = paste0(input_data_cd,"harvest_differences.rds"))


output7<-readRDS(paste0(input_data_cd,"harvest_differences.rds")) %>% 
  dplyr::mutate(diff_cod_rel=tot_rel_cod_model-tot_rel_cod_mrip, 
                diff_hadd_rel=tot_rel_hadd_model-tot_rel_hadd_mrip, 
                h_star_cod_keep_to_release_variable=case_when(cod_keep_2_release==1 ~ abs(diff_cod_harv/tot_keep_cod_model), TRUE ~ 0), 
                h_star_hadd_keep_to_release_variable=case_when(hadd_keep_2_release==1 ~ abs(diff_hadd_harv/tot_keep_hadd_model), TRUE~0), 
                h_star_cod_release_to_keep_variable=case_when(cod_release_2_keep==1 ~  abs(diff_cod_harv/tot_rel_cod_model), TRUE~0),
                h_star_hadd_release_to_keep_variable=case_when(hadd_release_2_keep==1 ~ abs(diff_hadd_harv/tot_rel_hadd_model), TRUE~0)) %>% 
  dplyr::mutate(h_star_cod_keep_to_release_variable=ifelse(tot_keep_cod_model>0 & tot_keep_cod_mrip==0, 1, h_star_cod_keep_to_release_variable),
                h_star_hadd_keep_to_release_variable=ifelse(tot_keep_hadd_model>0 & tot_keep_hadd_mrip==0, 1, h_star_hadd_keep_to_release_variable))
output7[is.na(output7)] <- 0

output7_check<-output7 %>% 
  dplyr::filter(h_star_hadd_release_to_keep_variable<0 | h_star_cod_release_to_keep_variable<0)

#Drop draws where h_star_cod_release_to_keep_variable>1 or  h_star_hadd_release_to_keep_variable>1, 
#as we cannot allocate any more than all the releases as discards

drops<-output7 %>% 
  dplyr::mutate(drop=case_when((h_star_hadd_release_to_keep_variable>=1 | h_star_cod_release_to_keep_variable>=1) ~1, TRUE~0)) %>% 
  dplyr::group_by(draw) %>% 
  dplyr::summarise(sum_drop=sum(drop), .groups="drop")

output7<-output7 %>% 
  dplyr::left_join(drops, by="draw") %>% 
  dplyr::filter(sum_drop==0) %>% 
  dplyr::select(-sum_drop)  #%>% 

n_distinct(output7$draw)

saveRDS(output7, file = paste0(input_data_cd,"harvest_differences_check.rds"))

