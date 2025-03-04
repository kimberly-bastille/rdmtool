

##Translate datasets into file formats that R reads fastest
library(feather)

baseline_comparison<-readRDS(paste0(input_data_cd, "calibration_comparison.rds"))
n_distinct(baseline_comparison$draw)


for(i in unique(baseline_comparison$mrip_index)){
pds_new<-readr::read_rds(file.path(paste0(input_data_cd, "pds_new_", i,".rds")))

season<-unique(pds_new$open)
mode<-unique(pds_new$mode)
draw<-unique(pds_new$draw)

write_feather(pds_new, paste0(input_data_cd, "pds_new_", mode,"_", season, "_", draw, ".feather"))

#costs
costs_new_all =  readr::read_rds(file.path(paste0(input_data_cd, "costs_", i,".rds")))
season<-  unique(costs_new_all$open)
mode<-  unique(costs_new_all$mode)
draw<-unique(pds_new$draw)

write_feather(costs_new_all, paste0(input_data_cd, "costs_",mode,"_", season, "_", draw, ".feather"))
}

