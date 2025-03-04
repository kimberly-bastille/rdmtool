

**differences by mode 
import delimited using "$draw_file_cd\MRIP_catch_totals_month.csv", clear 
merge 1:1 mode month using "$draw_file_cd\MRIP_dtrip_totals_month.dta"
order mode month _ dtrip
mvencode dtrip, mv(0) override
drop _merge
rename dtrip dtrip_mrip
tempfile mrip 
save `mrip', replace 

import delimited using "$draw_file_cd\simulated_catch_totals_month.csv", clear 
merge m:1 mode month using `mrip'

drop _merge 

collapse (mean) tot_cod_keep tot_cod_rel tot_cod_catch tot_hadd_keep tot_hadd_rel tot_hadd_catch cod_catch_mrip ///
						cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_keep_mrip hadd_rel_mrip dtrip dtrip_mrip, by(month mode)

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

order mode month *perc* cod_keep* *cod_rel* *cod_catch* *hadd_keep* *hadd_rel* *hadd_catch* *dtrip*

**differences over all modes


import delimited using "$draw_file_cd\MRIP_catch_totals.csv", clear
gen dtrip_mrip=57211.25 if mode=="fh"
replace dtrip_mrip=161487 if mode=="pr"
replace dtrip_mrip=15760.72 if mode=="sh"

collapse (sum) cod_catch_mrip cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_keep_mrip hadd_rel_mrip dtrip_mrip
gen tab=1
 
tempfile mrip 
save `mrip', replace 

import delimited using "$draw_file_cd\simulated_catch_totals.csv", clear 
collapse (sum) tot_cod_keep tot_cod_rel tot_cod_catch tot_hadd_keep tot_hadd_rel tot_hadd_catch dtrip, by(draw)
gen tab=1
merge m:1 tab using `mrip'
					
					
collapse (mean) tot_cod_keep tot_cod_rel tot_cod_catch tot_hadd_keep tot_hadd_rel tot_hadd_catch cod_catch_mrip ///
						cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_keep_mrip hadd_rel_mrip dtrip dtrip_mrip

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

order *perc* *cod_keep* *cod_rel* *cod_catch* *hadd_keep* *hadd_rel* *hadd_catch* *dtrip*
