
import delimited using "$draw_file_cd\catch_draws55.csv", clear 
import delimited using  "$input_code_cd\directed_trips_calib_150draws.csv", clear 
keep if draw==1
su dtrip
return list
*/


global domainz
forv i=1/150{
local i=1
import delimited using  "$input_code_cd\directed_trips_calib_150draws.csv", clear 
keep if draw==1
keep day  mode dtrip
tempfile dtrip
save `dtrip', replace 
local i=1

import delimited using "$draw_file_cd\catch_draws`i'_full.csv", clear 

*keep if catch_draw==1
collapse (mean)  cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch, by(day  mode)
merge 1:1 day mode using `dtrip', keep (3) nogen 
sort mode day_i 

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

collapse (sum) tot* dtrip, by(mode)
gen draw=`i'

	
tempfile domainz`i'
save `domainz`i'', replace
global domainz "$domainz "`domainz`i''" " 

}


clear
dsconcat $domainz

**Expand so there are three rows, one for each mode
gen tab=1
egen sumtab=sum(tab), by(draw)
expand 2 if sumtab==2 & mode=="pr", gen(dup)

replace mode="sh" if dup==1

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch
foreach v of local vars{
	replace tot_`v'= 0 if dup==1 & mode=="sh"
	
}

sort draw mode
drop tab sumtab dup

export delimited using "$draw_file_cd\simulated_catch_totals.csv", replace 





