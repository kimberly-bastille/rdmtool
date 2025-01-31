

cd $input_data_cd

clear

mata: mata clear

tempfile tl1 sl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
*replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
drop if strmatch(id_code, "*xx*")==1
drop if strat_id==""
duplicates drop 
save `tl1'
clear
 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen


 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
keep if $calibration_year
 
gen st2 = string(st,"%02.0f")

*OLD MRIP site allocations
/*
*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_data_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge


/*classify into (O)ther, Gulf of (M)aine, or Georges (B)ank */
gen str3 area_s="O"

replace area_s="M" if st2=="23" | st2=="33"
replace area_s="M" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="B" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
*/

*NEW MRIP site allocations
preserve 
import excel using "$input_data_cd/ma_site_list_updated_SS.xlsx", clear first
keep SITE_EXTERNAL_ID NMFS_STAT_AREA
renvarlab, lower
rename site_external_id intsite
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite using `mrip_sites',  keep(1 3)

/*classify into GOM or GBS */
gen str3 area_s="O"

replace area_s="M" if st2=="23" | st2=="33"
replace area_s="M" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="B" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="M" if st2=="25" & intsite==224


gen mode1="pr" if inlist(mode_fx, "1", "2", "3", "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")



 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" use the id_code*/
gen common_dom="z"
 /*
if strmatch("$my_common","atlanticcod")==1{
  replace common_dom="$my_common" if strmatch(sp_code,"8791030402")
 }
 
 if strmatch("$my_common","haddock")==1{
  replace common_dom="$my_common" if strmatch(sp_code,"8791031301")
 }
*/
replace common_dom="c" if strmatch(sp_code,"8791030402")
replace common_dom="h" if strmatch(sp_code,"8791031301")

*tab common if common_dom=="atlanticcod"
 
tostring wave, gen(w2)
tostring year, gen(year2)

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo


/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace $length_bin = 0 if !inlist(common_dom, "c", "h")

sort year w2 strat_id psu_id id_code

keep if area_s=="M"
drop if common_dom=="z"
/*
replace common_dom="had" if common_dom=="haddock"
replace common_dom="cod" if common_dom=="atlanticcod"
*/


*create a variable indicating if the observation came from a month where the season was open or closed
destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td





drop _merge 
*global calibration_year "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"

gen min="18" if date>=td(14aug2023) & mode1=="fh"
replace min="17" if min==""

gen my_dom_id_string=common_dom+"_"+area_s+"_"+min
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

gen species="cod" if common_dom=="c"
replace species="hadd" if common_dom=="h"
keep if species=="hadd"

*drop if lngth_imp==1
*preserve

svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)
tab $length_bin my_dom_id_string

svy: tab $length_bin my_dom_id_string, count
/*save some stuff  -matrix of proportions, row names, column names, estimate of total population size*/
mat eP=e(Prop)
mat eR=e(Row)'
mat eC=e(Col)
local PopN=e(N_pop)

local mycolnames: colnames(eC)
mat colnames eP=`mycolnames'
	
clear
/*read the eP into a dataset and convert proportion of population into numbers*/
svmat eP, names(col)
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}
/*read in the "row" */
svmat eR
order eR
rename eR $length_bin


ds $length_bin, not
renvarlab `r(varlist)', prefix(tab_)
reshape long tab_, i($length_bin) j(new) string	
split new, parse(_)
rename new1 species
rename new3 min_size

drop new2 new
replace species="cod" if species=="c"
replace species="hadd" if species=="h"
keep if species=="hadd"
replace l=l/2.54
rename l l_inches
destring min, replace
rename tab nfish_ab1	

gen below=1 if l_inches<min
mvencode below, mv(0)
collapse (sum) nfish_ab1, by(below)





global calibration_year "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"

rename tab nfish_ab1	
sort  season species $length_bin
