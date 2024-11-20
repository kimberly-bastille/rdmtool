
import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_cm.xlsx", clear first 
renvarlab, lower
rename value value_lou
tempfile lou
save `lou', replace 


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\pred_test_KB_works.csv", clear 

drop v1 option 
rename draw_out run 
rename value value_kim
replace catch_d ="" if catch_d=="NA"
replace param="" if param=="NA"
merge 1:1 category mode catch_disposition param number_weight season run mrip_index using `lou'

sort mrip_index run
gen diff=value_kim-value_l
gen perc_diff= ((value_kim-value_l)/value_l)*100
sort perc

drop _merge