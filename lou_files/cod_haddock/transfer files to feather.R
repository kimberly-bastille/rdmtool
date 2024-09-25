

##Translate datasets into file formats that R reads fastest
library(feather)

baseline_comparison<-readRDS("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/calibration_comparison.rds")
n_distinct(baseline_comparison$draw)


for(i in unique(baseline_comparison$mrip_index)){
#pds_new
pds_new<-readr::read_rds(file.path(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/pds_new_", i,".rds")))

season<-unique(pds_new$open)
mode<-unique(pds_new$mode)
draw<-unique(pds_new$draw)

write_feather(pds_new, paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/pds_new_", mode,"_", season, "_", draw, ".feather"))

#costs
costs_new_all =  readr::read_rds(file.path(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/costs_", i,".rds")))
season<-  unique(costs_new_all$open)
mode<-  unique(costs_new_all$mode)
draw<-unique(pds_new$draw)

write_feather(costs_new_all, paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/costs_",mode,"_", season, "_", draw, ".feather"))
}




# for(i in unique(baseline_comparison$draw)){
# #catch_draws
# catch<-read.csv(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws_projection", i, "_full.csv"))
# write_feather(catch, paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws_projection",i,"_full.feather"))
# }
# 
# 
# ##calibration catches
# for(i in 1:150){
#   #catch_draws
#   catch<-read.csv(paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws", i, "_full.csv"))
#   write_feather(catch, paste0("C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/catch_draws",i,"_full.feather"))
# }
# 

