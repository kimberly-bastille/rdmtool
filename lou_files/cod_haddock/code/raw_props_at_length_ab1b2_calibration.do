

*MRIP release data 
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
 

dsconcat $b2list
sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

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
replace common_dom="c" if strmatch(sp_code,"8791030402")
replace common_dom="h" if strmatch(sp_code,"8791031301")

tostring wave, gen(w2)
tostring year, gen(year2)

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace $length_bin =0 if !inlist(common_dom, "c", "h")


sort year w2 strat_id psu_id id_code

keep if area_s=="M"
drop if common_dom=="z"


*create a variable indicating if the observation came from a month where the season was open or closed
destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td


gen season="JanJun" if inlist(month1, 1, 2, 3, 4, 5, 6)
replace season="JulDec" if inlist(month1, 7, 8, 9, 10, 11, 12)

gen my_dom_id_string=common_dom+"_"+season+"_"+area_s

replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)


svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)

***for cod, use unweighted b2 data, weighted a+b1
***for haddock, use weighted b2 data, weighted a+b1

preserve
keep my_dom_id my_dom_id_string season common_dom $length_bin
keep if common_dom=="c"
gen species="cod" if common_dom=="c"
replace species="hadd" if common_dom=="h"
gen nfish_b2=1
collapse (sum) nfish_b2, by(season species $length_bin)
tempfile codb2
save `codb2', replace
restore

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
rename new2 season
replace species="cod" if species=="c"
replace species="hadd" if species=="h"

drop new
rename tab nfish_b2	
drop if species=="cod"
append using `codb2'
sort  season species $length_bin


tempfile b2
save `b2', replace 


*************Now pull keep lengths from MRIP
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
replace common_dom="c" if strmatch(sp_code,"8791030402")
replace common_dom="h" if strmatch(sp_code,"8791031301")

*tab common if common_dom=="atlanticcod"
 
tostring wave, gen(w2)
tostring year, gen(year2)

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace $length_bin = 0 if !inlist(common_dom, "c", "h")

sort year w2 strat_id psu_id id_code

keep if area_s=="M"
drop if common_dom=="z"


*create a variable indicating if the observation came from a month where the season was open or closed
destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td

gen season="JanJun" if inlist(month1, 1, 2, 3, 4, 5, 6)
replace season="JulDec" if inlist(month1, 7, 8, 9, 10, 11, 12)

gen my_dom_id_string=common_dom+"_"+season+"_"+area_s
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)


svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)

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
rename new2 season
replace species="cod" if species=="c"
replace species="hadd" if species=="h"

drop new
rename tab nfish_ab1	
sort  season species $length_bin


merge 1:1 $length_bin species season using `b2'

sort species  season l


gen panel_var=species+"_"+season
encode panel_var, gen(panel_var2)
xtset panel_var2 $length_bin
tsfill, full
mvencode nfish*, mv(0) over
decode panel_var2, gen(panel_var3)
split panel_var3, parse(_)
replace species=panel_var31
replace season=panel_var32

keep $length_bin nfish* species season
order species season  $length_bin nfish* 

egen sum_ab1=sum(nfish_ab1), by(species season ) 
egen sum_b2=sum(nfish_b2), by(species season ) 

gen prop_ab1=nfish_ab1/sum_ab1
gen prop_b2=nfish_b2/sum_b2

keep species season $length_bin prop_ab1 prop_b2
save "$input_data_cd/raw_props_at_length_ab1b2_season.dta", replace  //This file has raw proportions at length in the calibration period



*********Compute sizes for the whole year 

*MRIP release data 
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
 

dsconcat $b2list
sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

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
replace common_dom="c" if strmatch(sp_code,"8791030402")
replace common_dom="h" if strmatch(sp_code,"8791031301")

tostring wave, gen(w2)
tostring year, gen(year2)

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo


/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace $length_bin =0 if !inlist(common_dom, "c", "h")


sort year w2 strat_id psu_id id_code

keep if area_s=="M"
drop if common_dom=="z"


*create a variable indicating if the observation came from a month where the season was open or closed
destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td


gen season="JanJun" if inlist(month1, 1, 2, 3, 4, 5, 6)
replace season="JulDec" if inlist(month1, 7, 8, 9, 10, 11, 12)

gen my_dom_id_string=common_dom+"_"+area_s

replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)


svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)

***for cod, use unweighted b2 data, weighted a+b1
***for haddock, use weighted b2 data, weighted a+b1

preserve
keep my_dom_id my_dom_id_string  common_dom $length_bin
keep if common_dom=="c"
gen species="cod" if common_dom=="c"
replace species="hadd" if common_dom=="h"
gen nfish_b2=1
collapse (sum) nfish_b2, by( species $length_bin)
tempfile codb2
save `codb2', replace
restore

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
drop new2
replace species="cod" if species=="c"
replace species="hadd" if species=="h"

drop new
rename tab nfish_b2	
drop if species=="cod"
append using `codb2'
sort   species $length_bin


tempfile b2
save `b2', replace 


*************Now pull keep lengths from MRIP
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
replace common_dom="c" if strmatch(sp_code,"8791030402")
replace common_dom="h" if strmatch(sp_code,"8791031301")
 
tostring wave, gen(w2)
tostring year, gen(year2)

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace $length_bin = 0 if !inlist(common_dom, "c", "h")
*replace l_in_bin=l_in_bin+.5 if inlist(common_dom, "c", "h")

sort year w2 strat_id psu_id id_code

keep if area_s=="M"
drop if common_dom=="z"

*create a variable indicating if the observation came from a month where the season was open or closed
destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td

gen season="JanJun" if inlist(month1, 1, 2, 3, 4, 5, 6)
replace season="JulDec" if inlist(month1, 7, 8, 9, 10, 11, 12)

gen my_dom_id_string=common_dom+"_"+area_s
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)

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
replace species="cod" if species=="c"
replace species="hadd" if species=="h"


drop new new2
rename tab nfish_ab1	
sort  species $length_bin


merge 1:1 $length_bin species  using `b2'

sort species   l


encode species, gen(panel_var2)
xtset panel_var2 $length_bin
tsfill, full
mvencode nfish*, mv(0) over
decode panel_var2, gen(panel_var3)
replace species=panel_var3

keep $length_bin nfish* species 
order species   $length_bin nfish* 

egen sum_ab1=sum(nfish_ab1), by(species  ) 
egen sum_b2=sum(nfish_b2), by(species  ) 

gen prop_ab1=nfish_ab1/sum_ab1
gen prop_b2=nfish_b2/sum_b2

keep species  $length_bin prop_ab1 prop_b2
save "$input_data_cd/raw_props_at_length_ab1b2_annual.dta", replace  //This file has raw proportions at length in the calibration period




