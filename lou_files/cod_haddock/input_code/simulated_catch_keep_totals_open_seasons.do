/*
import delimited using "$draw_file_cd\catch_draws1.csv", clear 
keep month mode day day_i dtrip draw tripid catch_draw
*keep if mode=="sh" 
gen data="old"
tempfile old
save `old', replace 


import delimited using "$draw_file_cd\catch_draws1_test.csv", clear 
*keep if mode=="sh"
*duplicates drop 
*duplicates tag month mode day day_i dtrip draw tripid, gen(dup)
keep month mode day day_i dtrip draw tripid catch_draw
gen data="new"
merge 1:1 month mode day day_i dtrip draw tripid catch_draw using `old'



import delimited using "$draw_file_cd\catch_draws1_full.csv", clear 
gen year2=substr(day, 6, 4)
destring year2, replace

gen date=mdy( month, day1, year2)
format date %td

gen open=1 if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 ))
keep if draw==1

mvencode open, mv(0) override
tabstat dtrip, stat(sum) by(month)

tab month if open
tab month if !open
tabstat dtrip, stat(sum) by(open)

import delimited using  "$input_code_cd\directed_trips_calib_150draws.csv", clear 
*keep if draw==1
gen open=1 if ((day_i>=336 & day_i<=349) | (day_i>=124 & day_i<=160))
collapse (sum) dtrip, by(mode open draw)
mvencode open, mv(0) override

mvencode open, mv(0) override
sort draw mode open 

gen tab=1 if dtrip!=0
egen sumtab=sum(tab), by(draw)

keep if mode=="sh"
keep if dtrip!=0
tabstat dtrip, stat(sum) by(open)
tab month if open
tab month if !open

*/


global domainz
qui forv i=1/150{
*local i=1

*import delimited using "$draw_file_cd\catch_draws`i'.csv", clear 
*import delimited using "$draw_file_cd\catch_draws1_full.csv", clear 
import delimited using "$draw_file_cd\catch_draws`i'_full.csv", clear 
*import delimited using "$draw_file_cd\catch_draws1.csv", clear 

*local i=1
gen year2=substr(day, 6, 4)
destring year2, replace

*gen day1=substr(day, 1,2)
*destring day1, replace

gen date=mdy( month, day1, year2)
format date %td

gen open=1 if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 ))
replace open=0 if open==.

preserve
keep day day_i mode open
duplicates drop 
tempfile seasons
save `seasons', replace 
restore 

keep if catch_draw==1
collapse (mean)  cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch, by(day day_i mode)
*local i=1
preserve 
import delimited using  "$input_code_cd\directed_trips_calib_150draws.csv", clear 
keep if draw==`i'
keep mode day day_i dtrip
tempfile dtrip
save `dtrip', replace
restore

merge 1:1 mode day day_i using `dtrip'
mvencode cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch dtrip, mv(0) override
drop _merge

sort mode day_i 

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

merge m:1 day day_i mode using `seasons',keep(3) nogen
collapse (sum) tot* dtrip, by(mode open)
gen draw=`i'

	
tempfile domainz`i'
save `domainz`i'', replace
global domainz "$domainz "`domainz`i''" " 

}


clear
dsconcat $domainz

sort draw mode open 
order draw mode open 

export delimited using "$draw_file_cd\simulated_catch_totals_open_season.csv", replace 


