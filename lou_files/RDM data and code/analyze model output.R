# 

pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", 
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "reactable", "fishmethods")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflicts_prefer(dplyr::mutate)
options(scipen = 100, digits = 5)


##Coastwide harvest target = 10% reduction in scup, 28% reduction in black sea bass
##                         = .90 * (median value scup SQ), .72 * (median value SF SQ)
##                         = 13,761,298 for scup, 6,392,837 for summer flounder


#fluke_target<- 6392837
#scup_target<- 13761298


#Here we read in the bias estimates for NJ. To get accurate harvest/release/discard numbers, 
#we will subract the bias from from the output of the model, then compute the percentage chnages based on these values. 
#We will then apply the percent changes to the orginal, unadjusted value of the status quo to get the "imputed" value of the alternitve
#Then we will compute the percent differences based on these original status quo and the imputed value. 

NJ_bias_est<- readr::read_csv(file.path(here::here("differences_total_catch_NJ.csv")),  show_col_types = FALSE) %>% 
  dplyr::select(diff_bsb_keep,diff_bsb_rel,diff_scup_keep ,    
                diff_scup_rel,diff_sf_keep,diff_sf_rel,draw,mode1)

all_vars=c("diff_bsb_keep","diff_bsb_rel","diff_scup_keep" ,    
           "diff_scup_rel","diff_sf_keep","diff_sf_rel")

append=list()
for(v in all_vars){
  new<- NJ_bias_est %>%  dplyr::select(v, draw,mode1)
  
  append[[v]]<-new
  append[[v]]<-append[[v]] %>% dplyr::rename(diff=v) %>% 
    dplyr::mutate(stat=v)
  
  
}
NJ_bias_est_long= list.stack(append, fill=TRUE)

NJ_bias_est_long<- NJ_bias_est_long %>% 
  tidyr::separate(stat, into = c("x", "Category", "keep_release")) %>% 
  dplyr::mutate(keep_release=case_when(keep_release=="rel"~"release", TRUE~keep_release)) %>% 
  dplyr::select(-x)

NJ_bias_est_long_disc_mort<- NJ_bias_est_long %>% 
  dplyr::filter(keep_release=="release") %>% 
  dplyr::mutate(diff=.1*diff, 
                keep_release="Discmortality")

NJ_bias_est_long<-rbind.fill(NJ_bias_est_long, NJ_bias_est_long_disc_mort) 

NJ_bias_est_long<-NJ_bias_est_long %>% 
  dplyr::rename(mode=mode1)

#

#Read in the output files for status quo options
StatusQuo_MA <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_MA.xlsx"))
StatusQuo_RI <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_RI.xlsx"))
StatusQuo_CT <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_CT.xlsx"))

#StatusQuo_CT_corrections<- openxlsx::read.xlsx(here::here("CT_SQ_corrections1.xlsx"))
# StatusQuo_CT<-StatusQuo_CT %>% 
#   dplyr::left_join(StatusQuo_CT_corrections, by=c("state", "mode", "Category", "keep_release", "number_weight")) %>% 
#   dplyr::mutate(correction=dplyr::case_when(is.na(correction)~1, TRUE~correction)) %>% 
#   dplyr::mutate(Value=as.numeric(Value), correction=as.numeric(correction),
#                 Value=Value*correction)



StatusQuo_NY <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_NY.xlsx"))
StatusQuo_NJ <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_NJ.xlsx"))
StatusQuo_DE <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_DE.xlsx"))
StatusQuo_MD <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_MD.xlsx"))
StatusQuo_VA <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_VA.xlsx"))
StatusQuo_NC <- openxlsx::read.xlsx(here::here("SQ_projections_11_9_NC.xlsx"))

# check<-StatusQuo_NJ %>% 
#   dplyr::filter(Category=="sf" & keep_release=="keep" & number_weight=="Number" & mode=="all") %>% 
#   dplyr::mutate(Value=as.numeric(Value))
# check %>% dplyr::group_by(keep_release) %>%  dplyr::summarise(mean(Value))
#Read in the output files for alternative options
alt_MA <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_MA.xlsx"))
alt_RI <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_RI.xlsx"))
alt_CT <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_CT.xlsx"))
alt_NY <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_NY.xlsx"))
alt_NJ <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_NJ.xlsx"))
alt_DE <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_DE.xlsx"))
alt_MD <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_MD.xlsx"))
alt_VA <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_VA.xlsx"))
alt_NC <- openxlsx::read.xlsx(here::here("coastwide2_projections_11_15_NC.xlsx"))

#Read in the output files for alternative options
alt_MA <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_MA.xlsx"))
alt_RI <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_RI.xlsx"))
alt_CT <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_CT.xlsx"))
alt_NY <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_NY.xlsx"))
alt_NJ <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_NJ.xlsx"))
alt_DE <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_DE.xlsx"))
alt_MD <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_MD.xlsx"))
alt_VA <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_VA.xlsx"))
alt_NC <- openxlsx::read.xlsx(here::here("coastwide4_projections_11_27_NC.xlsx"))

#Read in the output files for alternative options
alt_MA <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_MA.xlsx"))
alt_RI <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_RI.xlsx"))
alt_CT <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_CT.xlsx"))
alt_NY <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_NY.xlsx"))
alt_NJ <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_NJ.xlsx"))
alt_DE <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_DE.xlsx"))
alt_MD <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_MD.xlsx"))
alt_VA <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_VA.xlsx"))
alt_NC <- openxlsx::read.xlsx(here::here("coastwide5_projections_11_30_NC.xlsx"))


#Read in the output files for alternative options
alt_MA <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_MA.xlsx"))
alt_RI <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_RI.xlsx"))
alt_CT <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_CT.xlsx"))
alt_NY <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_NY.xlsx"))
alt_NJ <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_NJ.xlsx"))
alt_DE <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_DE.xlsx"))
alt_MD <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_MD.xlsx"))
alt_VA <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_VA.xlsx"))
alt_NC <- openxlsx::read.xlsx(here::here("coastwide3_projections_11_20_NC.xlsx"))


#Read in the output files for alternative options
alt_MA <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_MA.xlsx"))
alt_RI <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_RI.xlsx"))
alt_CT <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_CT.xlsx"))
alt_NY <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_NY.xlsx"))
alt_NJ <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_NJ.xlsx"))
alt_DE <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_DE.xlsx"))
alt_MD <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_MD.xlsx"))
alt_VA <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_VA.xlsx"))
alt_NC <- openxlsx::read.xlsx(here::here("coastwide1_projections_11_9_NC.xlsx"))


##NJ check1
# alt_NJ <- openxlsx::read.xlsx(here::here("projections_check1_11_21_NJ.xlsx"))

##Begin with harvest weights
alt <- rbind(alt_MA, alt_RI, alt_CT,
             alt_NY, alt_DE, alt_VA,
             alt_NJ, alt_MD, alt_NC) %>%
  # alt <- rbind(alt_NJ)  %>%
  dplyr::rename(value_alt = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release=="keep" ) %>%
  dplyr::filter(number_weight %in% c("Weight_avg", "Weight") ) %>% 
  dplyr::select(-param)


StatusQuo <- rbind(StatusQuo_MA, StatusQuo_RI, StatusQuo_CT,
                   StatusQuo_NY, StatusQuo_DE, StatusQuo_VA,
                   StatusQuo_NJ, StatusQuo_MD, StatusQuo_NC) %>%
 # StatusQuo <- rbind(StatusQuo_NJ) %>%
  dplyr::rename(value_SQ = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release=="keep" ) %>%
  dplyr::filter(number_weight %in% c("Weight_avg", "Weight") ) %>% 
  dplyr::select(-param)
  
write_xlsx(StatusQuo,"StatusQuo_proj_4_19.xlsx")

predictions_merge <- StatusQuo %>% 
  dplyr::left_join(alt, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt))

predictions_weight <- predictions_merge %>%
  dplyr::filter(number_weight == "Weight") %>%
  dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
  dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))

predictions_avg <- predictions_merge %>%
  dplyr::filter(number_weight == "Weight_avg") %>% 
  dplyr::mutate(value_SQ=value_SQ*0.989699809)

predictions_merge2<- predictions_avg %>% 
  dplyr::left_join(predictions_weight, by = c("Category","mode", "state","draw")) %>%
  dplyr::select(-keep_release.x, -keep_release.y, -number_weight.y) %>% 
  dplyr::mutate(imputed_value_alt= perc_change_weight/100,
                imputed_value_alt = value_SQ * imputed_value_alt, 
                imputed_value_alt=imputed_value_alt+value_SQ) 
                #check = ((imputed_value_alt-value_SQ)/value_SQ)*100)

predictions_merge2<- predictions_merge2 %>% 
  #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>% 
  #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       is.na(value_alt_Weight) & 
                                                       value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))


###########################
#Coastwide harvest results
# Collapse imputed values by for coastwide and compute CIs
aggregate_data<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100)

#sort observations and create index by species
aggregate_data <- aggregate_data %>% 
  dplyr::group_by(Category) %>% 
  dplyr::arrange(Category,imputed_value_alt_sum) %>% 
  mutate(n_weight = row_number(Category)) %>% 
  dplyr::arrange(Category,perc_diff) %>% 
  mutate(n_perc = row_number(Category)) %>% 
  dplyr::arrange(Category,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(Category)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Category,perc_diff)  
  



ggplot(aggregate_data, aes(x=as.factor(Category), y=value_SQ_sum)) + 
  geom_violin(fill="slateblue", alpha=0.2) + 
  geom_jitter( height = 0, width = 0.05)+ 
  scale_fill_manual(values = c("red", "blue", "green")) +
  ggplot2::ylab("Total harvest weight in pounds") +
  ggplot2::xlab("") + geom_hline(aes(yintercept=6350000)) +
  geom_text(aes(3,6350000,label = "sf RHL", vjust = -.8)) +

  ggplot2::xlab("") + geom_hline(aes(yintercept=12510000)) +
  geom_text(aes(2,12510000,label = "scup RHL", vjust = -.8))+  
  
  ggplot2::xlab("") + geom_hline(aes(yintercept=6270000) ) +
  geom_text(aes(1,6270000,label = "bsb RHL", vjust = 1.6)) + 
  scale_y_continuous(labels = comma, breaks=round(seq(6000000, 18000000, by = 1000000),1) )


# 
# 6.35 million pound average RHL for summer flounder (same RHL in both 2024 and 2025)
# 12.51 million pound average RHL for scup (13.18 in 2024 and 11.84 in 2025)
# 6.27 million pound RHL for BSB (the 2024 RHL; we don’t have a 2025 RHL yet)


aggregate_data<- aggregate_data %>% 
  dplyr::mutate(harv_target=case_when(Category=="scup"~.9*value_SQ_sum, TRUE~NA), 
                harv_target=case_when(Category=="sf"~.72*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=case_when(imputed_value_alt_sum<=harv_target & value_SQ_sum!=0~1, TRUE~0))

  
categories=list()

for(d in unique(aggregate_data$Category)){
  
new<- aggregate_data %>% 
  dplyr::filter(Category==d) #%>% 
  #dplyr::arrange(n_perc)

lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
lb_perc_diff<- new$perc_diff[new$n_perc==11] 
lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 

ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
ub_perc_diff<- new$perc_diff[new$n_perc==90] 
ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 

median_value_alt<- median(new$imputed_value_alt_sum)
median_perc_diff<- median(new$perc_diff)
median_value_SQ<- median(new$value_SQ_sum)


reach_target<- sum(new$reach_target)



categories[[d]] <- as.data.frame(
  cbind(
    median_perc_diff,lb_perc_diff, ub_perc_diff,
    median_value_alt,lb_value_alt, ub_value_alt,
    median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
      ))

categories[[d]]$species<-d
  

}
coastwide_harvest_results= list.stack(categories, fill=TRUE)
coastwide_harvest_results<- coastwide_harvest_results %>% 
  dplyr::mutate(region="coastwide", 
                stat="harvest pounds", 
                mode="all modes") %>% 
  dplyr::relocate(region, stat, species, mode)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)

###########################
#state-level output
state_harvest_output<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category, state) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_harvest_output <- state_harvest_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() 

state_harvest_output<- state_harvest_output %>% 
  dplyr::mutate(harv_target=case_when(Category=="scup"~.9*value_SQ_sum, TRUE~NA), 
                harv_target=case_when(Category=="sf"~.72*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=case_when(imputed_value_alt_sum<=harv_target & value_SQ_sum!=0 ~1, TRUE~0))

categories_state=list()

for(d in unique(state_harvest_output$domain)){
  
  new<- state_harvest_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  reach_target<- sum(new$reach_target)
  
  
  categories_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
    ))
  
  categories_state[[d]]$domain<-d
  
  
}
state_harvest_results= list.stack(categories_state, fill=TRUE)
state_harvest_results<- state_harvest_results %>% 
  tidyr::separate(domain, into = c("region", "species"))  %>% 
  dplyr::mutate(stat="harvest pounds", 
                mode="all modes") %>% 
  dplyr::relocate(region, stat, species, mode)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


# state_harvest_results_check<- state_harvest_results 
#write_xlsx(state_harvest_results_check,"state_harvest_results_check1.xlsx")

###########################
#state by mode-level output
state_mode_harvest_output<- predictions_merge2 %>% 
  dplyr::group_by(draw, Category, state, mode) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

# state_mode_harvest_output_check<- state_mode_harvest_output %>% 
#   dplyr::filter(is.na(perc_diff)| perc_diff==0)


state_mode_harvest_output <- state_mode_harvest_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 
  
state_mode_harvest_output<- state_mode_harvest_output %>% 
  dplyr::mutate(harv_target=case_when(Category=="scup"~.9*value_SQ_sum, TRUE~NA), 
                harv_target=case_when(Category=="sf"~.72*value_SQ_sum, TRUE~harv_target)) %>% 
  dplyr::mutate(reach_target=case_when(imputed_value_alt_sum<=harv_target & value_SQ_sum!=0 ~1, TRUE~0))



categories_state_mode=list()

for(d in unique(state_mode_harvest_output$domain)){
  
  new<- state_mode_harvest_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  reach_target<- sum(new$reach_target)
  
  
  
  categories_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ, reach_target
    ))
  
  categories_state_mode[[d]]$domain<-d
  
  
}
state_mode_harvest_results= list.stack(categories_state_mode, fill=TRUE)
state_mode_harvest_results<- state_mode_harvest_results %>% 
  tidyr::separate(domain, into = c("region", "species", "mode"))  %>% 
  dplyr::mutate(stat="harvest pounds") %>% 
  dplyr::relocate(region, stat, species, mode, reach_target, median_value_SQ, median_value_alt, median_perc_diff ) 

rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)

 state_mode_harvest_results_check<- state_mode_harvest_results %>% 
   dplyr::filter(region %in% c("MD", "VA", "DE"))
# write_xlsx(state_mode_harvest_results_check,"state_mode_harvest_results_check1.xlsx")

##in state_mode_harvest_results, if the median perc_diff is "Inf", then that means that there was zero 
##harvest in the SQ and positive harvest in the alternative. If the median perc_diff is "NaN", then that
##means that there was zero harvest in the SQ and zero harvest in the alternative. 



#######
#Now compute chnages in releases and discard mortality. 
alt <- rbind(alt_MA, alt_RI, alt_CT,
             alt_NY, alt_DE, alt_VA,
             alt_NJ, alt_MD, alt_NC) %>%
  dplyr::rename(value_alt = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Weight_avg", "Weight") ) %>% 
  dplyr::select(-param)


StatusQuo <- rbind(StatusQuo_MA, StatusQuo_RI, StatusQuo_CT,
                   StatusQuo_NY, StatusQuo_DE, StatusQuo_VA,
                   StatusQuo_NJ, StatusQuo_MD, StatusQuo_NC) %>%
  dplyr::rename(value_SQ = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Weight_avg", "Weight") ) %>% 
  dplyr::select(-param)


predictions_releases_merge <- StatusQuo %>% 
  dplyr::left_join(alt, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt))

predictions_release_weight <- predictions_releases_merge %>%
  dplyr::filter(number_weight == "Weight") %>%
  dplyr::rename(value_alt_Weight=value_alt, value_SQ_Weight=value_SQ) %>%
  dplyr::mutate(perc_change_weight = (((value_alt_Weight-value_SQ_Weight)/value_SQ_Weight) * 100))

predictions_release_avg <- predictions_releases_merge %>%
  dplyr::filter(number_weight == "Weight_avg") 



predictions_releases_merge2<- predictions_release_avg %>% 
  dplyr::left_join(predictions_release_weight, by = c("Category","mode", "state","draw", "keep_release")) %>%
  dplyr::select(-number_weight.y) %>% 
  dplyr::mutate(imputed_value_alt= perc_change_weight/100,
                imputed_value_alt = value_SQ * imputed_value_alt, 
                imputed_value_alt=imputed_value_alt+value_SQ)  
#check = ((imputed_value_alt-value_SQ)/value_SQ)*100)

predictions_releases_merge2<- predictions_releases_merge2 %>% 
  #If imputed_value_alt is missing and value_alt_Weight is not missing, replace imputed_value_alt with value_alt_Weight
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       !is.na(value_alt_Weight) ~ value_alt_Weight, TRUE ~ imputed_value_alt)) %>% 
  #If imputed_value_alt is missing and value_alt_Weight IS missing, and value_SQ =0,  replace imputed_value_alt with 0
  dplyr::mutate(imputed_value_alt = dplyr::case_when(is.na(imputed_value_alt) & 
                                                       is.na(value_alt_Weight) & 
                                                       value_SQ==0 ~ 0, TRUE ~ imputed_value_alt))  




###########################
#Coastwide release results
# Now Collapse these imputed values by for coastwide and compute CIs
aggregate_release_data<- predictions_releases_merge2 %>% 
  dplyr::mutate(domain=paste0(Category, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) 
  

#sort observations and create index by species
aggregate_release_data<- aggregate_release_data %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain, imputed_value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() 

aggregate_releases=list()

for(d in unique(aggregate_release_data$domain)){
  
  new<- aggregate_release_data %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  aggregate_releases[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  aggregate_releases[[d]]$domain<-d
  
  
}
coastwide_release_results= list.stack(aggregate_releases, fill=TRUE)
coastwide_release_results<- coastwide_release_results %>% 
  tidyr::separate(domain, into = c("species", "stat1"))  %>% 
  dplyr::mutate(region="coastwide", 
                stat=case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)

rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


###########################
#state-level output
state_release_output<- predictions_releases_merge2 %>% 
  dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(domain, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_release_output <- state_release_output %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() 

categories_release_state=list()

for(d in unique(state_release_output$domain)){
  
  new<- state_release_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_release_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_release_state[[d]]$domain<-d
  
  
}
state_release_results= list.stack(categories_release_state, fill=TRUE)

state_release_results<- state_release_results %>% 
  tidyr::separate(domain, into = c("species", "region", "stat1")) %>% 
  dplyr::mutate(stat=case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)

###########################
#state by mode-level release output
state_mode_release_output<- predictions_releases_merge2 %>% 
  dplyr::group_by(draw, Category, state, mode, keep_release) %>% 
  dplyr::summarise(imputed_value_alt_sum = sum(imputed_value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((imputed_value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               imputed_value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_release_output <- state_mode_release_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,imputed_value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

categories_releases_state_mode=list()

for(d in unique(state_mode_release_output$domain)){
  
  new<- state_mode_release_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$imputed_value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$imputed_value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$imputed_value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_releases_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_releases_state_mode[[d]]$domain<-d
  
  
}
state_mode_release_results= list.stack(categories_releases_state_mode, fill=TRUE)
state_mode_release_results<- state_mode_release_results %>% 
  tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>% 
  dplyr::mutate(stat=case_when(stat1=="release" ~ "release pounds", TRUE~stat1), 
                stat=case_when(stat1=="Discmortality"~ "dead release pounds", TRUE~stat)) %>% 
  dplyr::select(-stat1)

rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


#######
#Now compute chnages in harvest numbers  
alt <- rbind(alt_MA, alt_RI, alt_CT,
             alt_NY, alt_DE, alt_VA,
             alt_NJ, alt_MD, alt_NC) %>%
  dplyr::rename(value_alt = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Number") ) %>% 
  dplyr::select(-param)


StatusQuo <- rbind(StatusQuo_MA, StatusQuo_RI, StatusQuo_CT,
                   StatusQuo_NY, StatusQuo_DE, StatusQuo_VA,
                   StatusQuo_NJ, StatusQuo_MD, StatusQuo_NC) %>%
  dplyr::rename(value_SQ = Value) %>% 
  dplyr::filter(Category %in% c("sf", "bsb", "scup")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::filter(keep_release %in% c("keep", "release", "Discmortality") ) %>%
  dplyr::filter(number_weight %in% c("Number") ) %>% 
  dplyr::select(-param)


predictions_harv_num_merge <- StatusQuo %>% 
  dplyr::left_join(alt, by=c("Category","mode", "number_weight","state", "draw", "keep_release")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt))

predictions_harv_num_merge_check<-predictions_harv_num_merge %>% 
  dplyr::filter(is.na(value_SQ))
##Make the adjustments for NJ 

predictions_harv_num_merge_NJ <- predictions_harv_num_merge %>% 
  dplyr::filter(state=="NJ") %>%
  dplyr::left_join(NJ_bias_est_long, by=c("draw", "mode", "Category", "keep_release")) %>% 
  dplyr::mutate(value_SQ_adj=value_SQ-diff, value_alt_adj=value_alt-diff) %>% 
  dplyr::mutate(perc_diff=((value_alt_adj-value_SQ_adj)/value_SQ_adj)*100, 
              imputed_value_alt= perc_diff/100,
              imputed_value_alt = value_SQ * imputed_value_alt, 
              imputed_value_alt=imputed_value_alt+value_SQ) %>%
  dplyr::mutate(perc_diff=case_when(is.nan(perc_diff) & value_SQ_adj==0 & value_alt_adj==0~0, TRUE~perc_diff)) %>% 
  dplyr::mutate(imputed_value_alt=case_when(is.nan(imputed_value_alt) & value_SQ_adj==0 & value_alt_adj==0~0, TRUE~imputed_value_alt)) %>% 
  dplyr::select(Category, mode, keep_release, number_weight, value_SQ, imputed_value_alt, state, draw) %>% 
  dplyr::rename(value_alt=imputed_value_alt)
 


predictions_harv_num_merge<-predictions_harv_num_merge %>% 
  dplyr::filter(state!="NJ")

predictions_harv_num_merge<-rbind(predictions_harv_num_merge, predictions_harv_num_merge_NJ)

###########################
#Coastwide harvest num  results
# Now Collapse these imputed values by for coastwide and compute CIs
aggregate_harv_num_data<- predictions_harv_num_merge %>% 
  dplyr::mutate(domain=paste0(Category, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) 


#sort observations and create index by species
aggregate_harv_num_data<- aggregate_harv_num_data %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain, value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() 

aggregate_harv_nums=list()

for(d in unique(aggregate_harv_num_data$domain)){
  
  new<- aggregate_harv_num_data %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  aggregate_harv_nums[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  aggregate_harv_nums[[d]]$domain<-d
  
  
}
coastwide_harv_num_results= list.stack(aggregate_harv_nums, fill=TRUE)
coastwide_harv_num_results<- coastwide_harv_num_results %>% 
  tidyr::separate(domain, into = c("species", "stat1"))  %>% 
  dplyr::mutate(region="coastwide", 
                stat=case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=case_when(stat1=="keep"~ "harvest numbers", TRUE~stat),
                mode="all modes") %>% 
  dplyr::select(-stat1)

rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


###########################
#state-level output
state_harv_num_output<- predictions_harv_num_merge %>% 
  dplyr::mutate(domain=paste0(Category, "_", state, "_", keep_release)) %>% 
  dplyr::group_by(draw, domain) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(domain, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &
                                               value_alt_sum==0 ~ 0, TRUE ~ perc_diff))

state_harv_num_output <- state_harv_num_output %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() 

categories_harv_num_state=list()

for(d in unique(state_harv_num_output$domain)){
  
  new<- state_harv_num_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_harv_num_state[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_harv_num_state[[d]]$domain<-d
  
  
}
state_harv_num_results= list.stack(categories_harv_num_state, fill=TRUE)

state_harv_num_results<- state_harv_num_results %>% 
  tidyr::separate(domain, into = c("species", "region", "stat1")) %>% 
  dplyr::mutate(stat=case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=case_when(stat1=="keep"~ "harvest numbers", TRUE~stat), 
                mode="all modes") %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


###########################
#state by mode-level harvest num output
state_mode_harv_num_output<- predictions_harv_num_merge %>% 
  dplyr::group_by(draw, Category, state, mode, keep_release) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  dplyr::arrange(state, mode, Category, draw) %>% 
  dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 & value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_harv_num_output <- state_mode_harv_num_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode, "_", keep_release)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,value_SQ_sum) 

# state_mode_harv_num_output_check<-state_mode_harv_num_output %>% 
#   dplyr::filter(state=="NY" & Category=="scup" & keep_release=="keep" & mode=="fh")

categories_harv_num_state_mode=list()

for(d in unique(state_mode_harv_num_output$domain)){
  
  new<- state_mode_harv_num_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_harv_num_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_harv_num_state_mode[[d]]$domain<-d
  
  
}
state_mode_harv_num_results= list.stack(categories_harv_num_state_mode, fill=TRUE)
state_mode_harv_num_results<- state_mode_harv_num_results %>% 
  tidyr::separate(domain, into = c("region", "species",  "mode", "stat1"))  %>% 
  dplyr::mutate(stat=case_when(stat1=="release" ~ "release numbers", TRUE~stat1),
                stat=case_when(stat1=="Discmortality"~ "dead release numbers", TRUE~stat),
                stat=case_when(stat1=="keep"~ "harvest numbers", TRUE~stat)) %>% 
  dplyr::select(-stat1)


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)

###Now compute chnages in trips and welfare 

alt <- rbind(alt_MA, alt_RI, alt_CT,
             alt_NY, alt_DE, alt_VA,
             alt_NJ, alt_MD, alt_NC) %>%
  dplyr::rename(value_alt = Value) %>% 
  dplyr::filter(Category %in% c("CV", "ntrips")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::select(-param, -keep_release)


StatusQuo <- rbind(StatusQuo_MA, StatusQuo_RI, StatusQuo_CT,
                   StatusQuo_NY, StatusQuo_DE, StatusQuo_VA,
                   StatusQuo_NJ, StatusQuo_MD, StatusQuo_NC) %>%
  dplyr::rename(value_SQ = Value) %>% 
  dplyr::filter(Category %in% c("CV", "ntrips")) %>%
  dplyr::filter(mode!="all" ) %>%
  dplyr::select(-param, -keep_release)



CV_state_mode <- StatusQuo %>% 
  dplyr::left_join(alt, by=c("Category","mode", "number_weight","state", "draw")) %>%
  dplyr::mutate(value_SQ = as.numeric(value_SQ),
                value_alt = as.numeric(value_alt), 
                perc_diff=((value_alt-value_SQ)/value_SQ)*100)
 

##coatswide CV/trips results
aggregate_CV<- CV_state_mode %>% 
  dplyr::group_by(draw, Category) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  #Here I add the absolute difference. I keep the variable name "perc_diff" to save time
  #Subtracting Alt from SQ gives the correct signs
  
  #Change the signs on CV so that positive means better 
  dplyr::mutate(value_alt_sum=case_when(Category=="CV" ~ value_alt_sum*-1, TRUE~value_alt_sum),
                value_SQ_sum=case_when(Category=="CV" ~ value_SQ_sum*-1, TRUE~value_SQ_sum), 
                perc_diff=((value_alt_sum-value_SQ_sum)/abs(value_SQ_sum))*100)
                #perc_diff = value_SQ_sum-value_alt_sum) 


#sort observations and create index by species
aggregate_CV<- aggregate_CV %>% 
  dplyr::group_by(Category) %>% 
  dplyr::arrange(Category,value_alt_sum) %>% 
  mutate(n_weight = row_number(Category)) %>% 
  dplyr::arrange(Category,perc_diff) %>% 
  mutate(n_perc = row_number(Category)) %>% 
  dplyr::arrange(Category,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(Category)) %>% 
  dplyr::ungroup() 

aggregate_CVs=list()

for(d in unique(aggregate_CV$Category)){
  
  new<- aggregate_CV %>% 
    dplyr::filter(Category==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  aggregate_CVs[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  aggregate_CVs[[d]]$Category<-d
  
  
}
coastwide_CV_results= list.stack(aggregate_CVs, fill=TRUE)   
coastwide_CV_results<- coastwide_CV_results %>% 
    dplyr::rename(stat=Category) %>% 
    dplyr::mutate(region="coastwide", mode="all modes", species="all species")

rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


#state cv results 
state_CV<- CV_state_mode %>% 
  dplyr::group_by(draw, Category, state) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  #Here I add the absolute difference. I keep the variable name "perc_diff" to save time
  #Subtracting Alt from SQ gives the correct signs
  dplyr::mutate(#perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100, 
    perc_diff = value_SQ_sum-value_alt_sum) 
  #dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) 


#sort observations and create index by species
state_CV<- state_CV %>% 
  dplyr::group_by(Category, state) %>% 
  dplyr::arrange(Category,state, value_alt_sum) %>% 
  mutate(n_weight = row_number(Category)) %>% 
  dplyr::arrange(Category,state, perc_diff) %>% 
  mutate(n_perc = row_number(Category)) %>% 
  dplyr::arrange(Category,state,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(Category)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Category,state,perc_diff) %>% 
  dplyr::mutate(domain=paste0(state, "_", Category))
  
  

state_Cvs=list()

for(d in unique(state_CV$domain)){
  
  new<- state_CV %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  state_Cvs[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  state_Cvs[[d]]$domain<-d
  
  
}
state_CV_results= list.stack(state_Cvs, fill=TRUE)   
state_CV_results<- state_CV_results %>% 
  tidyr::separate(domain, into = c("region", "stat")) %>% 
  dplyr::mutate(mode="all modes", species="all species")


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)

###########################
#state mode-level CV output
state_mode_CV_output<- CV_state_mode %>% 
  dplyr::group_by(draw, Category, state, mode) %>% 
  dplyr::summarise(value_alt_sum = sum(value_alt),
                   value_SQ_sum = sum(value_SQ)) %>% 
  dplyr::ungroup() %>% 
  #Here I add the absolute difference. I keep the variable name "perc_diff" to save time
  #Subtracting Alt from SQ gives the correct signs
  dplyr::mutate(#perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100, 
    perc_diff = value_SQ_sum-value_alt_sum) 
  #dplyr::mutate(perc_diff=((value_alt_sum-value_SQ_sum)/value_SQ_sum)*100) %>% 
  #dplyr::arrange(state, mode, Category, draw) %>% 
  #dplyr::mutate(perc_diff = dplyr::case_when(value_SQ_sum==0 &value_alt_sum==0 ~ 0, TRUE ~ perc_diff))


state_mode_CV_output <- state_mode_CV_output %>% 
  dplyr::mutate(domain=paste0(state, "_", Category, "_", mode)) %>% 
  dplyr::group_by(domain) %>% 
  dplyr::arrange(domain,value_alt_sum) %>% 
  mutate(n_weight = row_number(domain)) %>% 
  dplyr::arrange(domain,perc_diff) %>% 
  mutate(n_perc = row_number(domain)) %>% 
  dplyr::arrange(domain,value_SQ_sum) %>% 
  mutate(n_SQ = row_number(domain)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(domain,perc_diff) 

categories_CV_state_mode=list()

for(d in unique(state_mode_CV_output$domain)){
  
  new<- state_mode_CV_output %>% 
    dplyr::filter(domain==d) #%>% 
  #dplyr::arrange(n_perc)
  
  lb_value_alt<- new$value_alt_sum[new$n_weight==11] 
  lb_perc_diff<- new$perc_diff[new$n_perc==11] 
  lb_value_SQ<- new$value_SQ_sum[new$n_SQ==11] 
  
  ub_value_alt<- new$value_alt_sum[new$n_weight==90] 
  ub_perc_diff<- new$perc_diff[new$n_perc==90] 
  ub_value_SQ<- new$value_SQ_sum[new$n_SQ==90] 
  
  median_value_alt<- median(new$value_alt_sum)
  median_perc_diff<- median(new$perc_diff)
  median_value_SQ<- median(new$value_SQ_sum)
  
  
  categories_CV_state_mode[[d]] <- as.data.frame(
    cbind(
      median_perc_diff,lb_perc_diff, ub_perc_diff,
      median_value_alt,lb_value_alt, ub_value_alt,
      median_value_SQ,lb_value_SQ, ub_value_SQ
    ))
  
  categories_CV_state_mode[[d]]$domain<-d
  
  
}
state_mode_CV_results= list.stack(categories_CV_state_mode, fill=TRUE)
state_mode_CV_results<- state_mode_CV_results %>% 
  tidyr::separate(domain, into = c("region", "stat",  "mode"))  %>% 
  dplyr::mutate(species="all species")


rm(lb_value_alt,lb_perc_diff,lb_value_SQ, ub_value_alt, ub_perc_diff,
   ub_value_SQ,median_value_alt,median_perc_diff,median_value_SQ)


###Now append all the data 

full_results <- rbind.fill(state_mode_CV_results, state_CV_results, coastwide_CV_results, 
                      state_mode_release_results, state_release_results, coastwide_release_results, 
                      state_mode_harvest_results, state_harvest_results, coastwide_harvest_results, 
                      state_mode_harv_num_results, state_harv_num_results, coastwide_harv_num_results)

full_results<-full_results %>% 
  dplyr::mutate(reach_target=case_when(median_value_SQ==0 & stat=="harvest pounds"~NA, TRUE~reach_target)) %>% 
  dplyr::mutate(reach_target=case_when(species=="bsb" ~NA, TRUE~reach_target)) %>% 
  dplyr::relocate(region, stat, species, mode, reach_target, median_value_SQ, median_value_alt, median_perc_diff) 
  


full_results_check<-full_results %>%
  dplyr::filter(region=="coastwide" & stat=="harvest pounds")





# 
 # full_results_check<-full_results %>%
 #   dplyr::filter(median_value_alt<median_value_SQ & median_perc_diff>0 )
 # full_results_check<-full_results %>%
 #       dplyr::filter(is.na(median_value_alt) )
#write_xlsx(full_results,"analyze_output_example.xlsx")
#write_xlsx(full_results_check,"full_results_check1.xlsx")

# full_results_check<-full_results %>% 
#   dplyr::filter(stat=="harvest pounds") %>% 
#   dplyr::arrange(species, region)%>% 
#   dplyr::relocate(region, stat, species, mode, reach_target)
  