
*This file pulls in our simulated estimates of catch, harvest, discards, and directed trips created in step 5 of the model wrapper, 
*$input_code_cd\simulated_catch_keep_totals_open_seasons.do and computes the means over the 150 iterations. These means
*can be generated at any level (season and mode, mode only, both modes and both seasons conbined, etc.), but we are primary interested 
*in the means by mode and season, as this is the level at which the simulation model is run. The file also pulls in MRIP point estimates 
*of total catch, harvest, discards, and directed trips for each simulation strata. It then compares differences between MRIP point estimates 
*and our simulated means. These comparisons are useful for identifying any large discrepencies that may result from the way we format the input data. 


**differences by season and mode 
import delimited using "$input_data_cd\MRIP_catch_totals_open_season.csv", clear 
merge 1:1 mode season using "$input_data_cd\MRIP_dtrip_totals_open_season.dta"
order mode season _ dtrip
mvencode dtrip, mv(0) override
drop _merge
rename dtrip dtrip_mrip
tempfile mrip 
save `mrip', replace 

import delimited using "$input_data_cd\simulated_catch_totals_open_season.csv", clear 

gen season="op" if open==1
replace season="cl" if open==0
merge m:1 mode season using `mrip'
drop if _merge==1
drop _merge 

collapse (mean) tot_cod_keep tot_cod_rel tot_cod_catch tot_hadd_keep tot_hadd_rel tot_hadd_catch cod_catch_mrip ///
						cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_keep_mrip hadd_rel_mrip dtrip dtrip_mrip, by(season mode)

rename tot_cod_keep cod_keep_model
rename tot_cod_rel cod_rel_model
rename tot_cod_catch cod_catch_model
rename tot_hadd_keep hadd_keep_model
rename tot_hadd_rel hadd_rel_model
rename tot_hadd_catch hadd_catch_model
rename dtrip dtrip_model

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch dtrip
foreach v of local vars{
	gen perc_diff_`v'= ((`v'_model-`v'_mrip)/`v'_mrip)*100
	gen diff_`v'= `v'_model-`v'_mrip
	
}

order mode season *perc* cod_keep* *cod_rel* *cod_catch* *hadd_keep* *hadd_rel* *hadd_catch* *dtrip*
browse mode season cod_keep_model cod_keep_mrip diff_cod_keep perc_diff_cod_keep
browse mode season cod_catch_model cod_catch_mrip diff_cod_catch perc_diff_cod_catch
browse mode season hadd_keep_model hadd_keep_mrip diff_hadd_keep perc_diff_hadd_keep
browse mode season hadd_catch_model hadd_catch_mrip diff_hadd_catch perc_diff_hadd_catch
browse mode season *dtrip*

**differences over all modes

import delimited using "$input_data_cd\MRIP_catch_totals_open_season.csv", clear 
merge 1:1 mode season using "$input_data_cd\MRIP_dtrip_totals_open_season.dta"
order mode season _ dtrip
mvencode dtrip, mv(0) override
drop _merge
rename dtrip dtrip_mrip

collapse (sum) cod_catch_mrip cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_keep_mrip hadd_rel_mrip dtrip_mrip, by(mode)
gen tab=1

tempfile mrip 
save `mrip', replace 

import delimited using "$input_data_cd\simulated_catch_totals_open_season.csv", clear 
gen season="op" if open==1
replace season="cl" if open==0

collapse (sum) tot_cod_keep tot_cod_rel tot_cod_catch tot_hadd_keep tot_hadd_rel tot_hadd_catch dtrip, by(draw mode)
gen tab=1
merge m:1 tab mode using `mrip'
drop _merge 
					
collapse (mean) tot_cod_keep tot_cod_rel tot_cod_catch tot_hadd_keep tot_hadd_rel tot_hadd_catch cod_catch_mrip ///
						cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_keep_mrip hadd_rel_mrip dtrip dtrip_mrip, by( mode)

rename tot_cod_keep cod_keep_model
rename tot_cod_rel cod_rel_model
rename tot_cod_catch cod_catch_model
rename tot_hadd_keep hadd_keep_model
rename tot_hadd_rel hadd_rel_model
rename tot_hadd_catch hadd_catch_model
rename dtrip dtrip_model

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch dtrip
foreach v of local vars{
	gen perc_diff_`v'= ((`v'_model-`v'_mrip)/`v'_mrip)*100
	gen diff_`v'= `v'_model-`v'_mrip
	
}

order   *perc* cod_keep* *cod_rel* *cod_catch* *hadd_keep* *hadd_rel* *hadd_catch* *dtrip*

browse mode  cod_keep_model cod_keep_mrip diff_cod_keep perc_diff_cod_keep
browse mode  cod_catch_model cod_catch_mrip diff_cod_catch perc_diff_cod_catch
browse mode  hadd_keep_model hadd_keep_mrip diff_hadd_keep perc_diff_hadd_keep
browse mode  hadd_catch_model hadd_catch_mrip diff_hadd_catch perc_diff_hadd_catch
browse mode  *dtrip*