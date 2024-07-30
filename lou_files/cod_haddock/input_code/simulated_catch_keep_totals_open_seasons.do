/*
import delimited using "$draw_file_cd\catch_draws55.csv", clear 
import delimited using  "$input_code_cd\directed_trips_calib_150draws.csv", clear 
keep if draw==1
su dtrip
return list
*/


global domainz
forv i=1/150{
import delimited using "$draw_file_cd\catch_draws`i'.csv", clear 


gen day2=substr(day, 1, 2)
destring day2, replace

gen year2=substr(day, 6, 4)
destring year2, replace


gen date=mdy( month, day2, year2)
format date %td

gen open=1 if ((date>=$cod_start_date1_fh & date<=$cod_end_date1_fh ) | (date>=$cod_start_date2_fh & date<=$cod_end_date2_fh ))
replace open=0 if open==.

preserve
keep day day_i mode open
duplicates drop 
tempfile seasons
save `seasons', replace 
restore 

*keep if catch_draw==1
collapse (mean) dtrip cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch, by(day day_i mode)

sort mode day_i 

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

merge m:1 day day_i mode using `seasons', keep(3) nogen
collapse (sum) tot* dtrip, by(mode open)
gen draw=`i'

	
tempfile domainz`i'
save `domainz`i'', replace
global domainz "$domainz "`domainz`i''" " 

}


clear
dsconcat $domainz

**Expand so there are three rows, one for each mode
gen tab=1
egen sumtab=sum(tab), by(draw open)
expand 2 if sumtab==2 & mode=="pr", gen(dup)

replace mode="sh" if dup==1

local vars cod_keep cod_rel cod_catch hadd_keep hadd_rel hadd_catch 
foreach v of local vars{
	replace tot_`v'= 0 if dup==1 & mode=="sh"
	
}

replace dtrip=0  if dup==1 & mode=="sh" 

sort draw mode open
drop tab sumtab dup

export delimited using "$draw_file_cd\simulated_catch_totals_open_season.csv", replace 





