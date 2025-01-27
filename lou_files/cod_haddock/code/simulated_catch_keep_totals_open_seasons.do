

global domainz
qui forv i=1/$ndraws{
import delimited using "$iterative_input_data_cd\catch_draws`i'_full.csv", clear 

gen year2=substr(day, 6, 4)
destring year2, replace

gen date=mdy( month, day1, year2)
format date %td

merge m:1 date using "$input_data_cd\cod_open_season_dates.dta"
drop if _merge==2
drop _merge
gen open=1 if  cod_season_open==1
replace open=0 if open==.
drop cod_season_open


preserve
keep day day_i mode open
duplicates drop 
tempfile seasons
save `seasons', replace 
restore 

keep if catch_draw==1
collapse (mean)  cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch, by(day day_i mode)

preserve 
import delimited using  "$input_data_cd\directed_trips_calib_150draws_cm.csv", clear 
keep if draw==`i'
keep mode day  dtrip
tempfile dtrip
save `dtrip', replace
restore

merge 1:1 mode day  using `dtrip'
mvencode cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch dtrip, mv(0) override
drop _merge

*sort mode day_i 

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

export delimited using "$input_data_cd\simulated_catch_totals_open_season.csv", replace 


