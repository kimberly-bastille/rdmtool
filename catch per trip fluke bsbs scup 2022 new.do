/* This is a file that produces a dataset that contains #of fish encountered per trip.
This is a port of Scott's "domain_catch_frequencies_gom_cod_wave_2013.sas"



This is a template program for estimating catch frequecies
using the MRIP public-use datasets.

The program is setup to use information in the trip_yyyyw
dataset to define custom domains.  The catch frequencies are
estimated within the domains by merging the trip information
onto the catch_yyyyw datasets.

Required input datasets:
 trip_yyyyw
 catch_yyyyw

yyyy = year
w    = wave


*/
qui{
clear
clear matrix
clear mata
set maxvar 100000  
version 12.1

/* General strategy 
COMPUTE totals and std deviations  catch

 */
clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

clear

mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
*replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
drop if strmatch(id_code, "*xx*")==1
drop if strat_id==""
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate

 /* THIS IS THE END OF THE DATA MERGING CODE */
*replace mode_fx=MODE_FX if mode_fx=="" & MODE_FX!=""
*replace area_x=AREA_X if area_x=="" & AREA_X!=""


 /* ensure only relevant states */
keep if inlist(st,25, 44, 9, 36, 34, 51, 10, 24, 37)

/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

/*
gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="bt" if inlist(mode_fx, "4", "5", "6", "7")
*keep if mode1=="bt"
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF" if inlist(prim1_common, "scup") 

tostring wave, gen(wv2)
tostring year, gen(yr2)
*gen my_dom_id_string=state+"_"+common_dom+"_"+yr2
*gen my_dom_id_string=state+"_"+common_dom
*gen my_dom_id_string=state+"_"+month1+"_"+mode1+"_"+common_dom
*gen my_dom_id_string=state+"_"+wv2+"_"+mode1+"_"+common_dom
gen my_dom_id_string=state+"_"+wv2+"_"+mode1+"_"+common_dom





/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (Cod or Haddock) then it should be marked in the domain "_ATLCO"
	1. Caught my_common.  We retain tot_cat
	2. Did not catch my_common.  We set tot_cat=0
   B.  Trip did not (Target or Caught) (Cod or Haddock) then it is marked in the the domain "ZZZZZ"
	1. Caught my_common.  This is impossible.
	2. Did not catch my_common.  We set tot_cat=0
	
To do this:
1.  We set tot_cat, landing, claim, harvest, and release to zero for all instances of common~="my_common"
2.  We set a variable "no_dup"=0 if the record is "my_common" catch and no_dup=1 otherwise.
3.  We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string".
  For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the 
  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified as an (A2 from above).
4. After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/



local vars claim harvest release landing tot_cat
foreach v of local vars{
	gen sf_`v'=`v' if common=="summerflounder"
	egen sum_sf_`v'=sum(sf_`v'), by(strat_id psu_id id_code )
	drop sf_`v'
	rename sum_sf_`v' sf_`v'
	
	gen bsb_`v'=`v' if common=="blackseabass"
	egen sum_bsb_`v'=sum(bsb_`v'), by(strat_id psu_id id_code )
	drop bsb_`v'
	rename sum_bsb_`v' bsb_`v'
	
	gen scup_`v'=`v' if common=="scup"
	egen sum_scup_`v'=sum(scup_`v'), by(strat_id psu_id id_code )
	drop scup_`v'
	rename sum_scup_`v' scup_`v'
}


/*
1  Set tot_cat, landing, claim, harvest, and release to zero for all instances of common~="my_common"
2.  We set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/


 gen no_dup=0
 replace no_dup=1 if !inlist(common, "summerflounder", "blackseabass", "scup")
 /*
 replace no_dup=1 if  strmatch(common, "summerflounder")==0
 replace no_dup=1 if strmatch(common, "blackseabass")==0
 replace no_dup=1 if strmatch(common, "scup")==0
*/
 bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="SF"


keep var_id strat_id psu_id id_code common sp_code claim harvest release landing ///
	tot_cat wp_catch  leader cntrbtrs wp_int state mode1 state wv2 /// 
	sf_claim bsb_claim scup_claim sf_harvest bsb_harvest scup_harvest sf_release bsb_release scup_release sf_landing bsb_landing scup_landing ///
	bsb_tot_cat sf_tot_cat scup_tot_cat claim_unadj harvest_unadj release_unadj year month common_dom my_dom_id_string
/*
*original unadjusted catch datasets
local vars sf bsb scup
foreach v of local vars{
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
svy:total `v'_claim
mat list r(table), format(%12.0gc)
	
svy:total `v'_harvest
mat list r(table), format(%12.0gc)

svy:total `v'_landing
mat list r(table), format(%12.0gc)

svy:total `v'_release
mat list r(table), format(%12.0gc)

svy:total `v'_tot_cat
mat list r(table), format(%12.0gc)
}
*/
 



/***********************************************************************
Following code defines new catch count fields to handle grouped catches
************************************************************************/

*1)
*proc sort data=mycatch;
*	by strat_id psu_id leader id_code;
*run;

sort strat_id psu_id leader id_code

*2) data mycatch;
*	set mycatch;
*	by strat_id psu_id leader id_code;
*	group_order + 1;
*	if first.leader then group_order=1;
*run;

bysort strat_id psu_id leader id_code: gen group_order= _n 

*3) data mycatch;
*	set mycatch;
*	if claim=. then claim=0;
*	if harvest=. then harvest=0;
*	if landing=. then landing=0;
*	new_claim1 = claim/cntrbtrs;
	*new_release = release;
*	new_harvest = harvest;
*run;

local vars claim harvest release landing
foreach v of local vars{
	
	replace sf_`v'=0 if sf_`v'==.
	replace bsb_`v'=0 if bsb_`v'==.
	replace scup_`v'=0 if scup_`v'==.

}

*As per John Foster's recommendation, we need to divide claim by total contributers 2) harvest and release by counts of id_code within leader 
*First, sum the catch within leader:
local vars claim harvest release landing
foreach v of local vars{
	
	egen sum_sf_`v'=sum(sf_`v'), by(strat_id psu_id leader)
	egen sum_bsb_`v'=sum(bsb_`v'), by(strat_id psu_id leader)
	egen sum_scup_`v'=sum(scup_`v'), by(strat_id psu_id leader)

}

*Now get counts of id_codes within leader: 
gen tab=1
egen count_id_codes=sum(tab), by(strat_id psu_id leader)
drop tab 

egen max_cntrbtrs=max(cntrbtrs), by(strat_id psu_id leader)

	
*Now divide claim by cntrbtrs and the other catch variables by count_id_codes:
local vars sf bsb scup
foreach v of local vars{
	
*gen new_claim1_`v'=sum_`v'_claim/cntrbtrs
gen new_claim1_`v'=sum_`v'_claim/count_id_codes

gen new_harvest1_`v'=sum_`v'_harvest/count_id_codes
gen new_release1_`v'=sum_`v'_release/count_id_codes
mvencode new_claim1_`v'  new_harvest1_`v' new_release1_`v', mv(0) override
}

*Now multiply wp_int by count of id_code to get new wp_catch:
gen new_wp_int=wp_int*count_id_codes
	
*we ultimately keep only the first observation within leader, so mark these rows:
bysort strat_id psu_id leader: gen first=1 if _n==1

*generate total catch for the species of interest:
local vars sf bsb scup
foreach v of local vars{
	gen tot_cat_`v'=new_claim1_`v'+new_harvest1_`v'+new_release1_`v'
	gen landing_`v'=new_claim1_`v'+new_harvest1_`v'
	gen release_`v'=new_release1_`v'

}


*keep if first==1
}



*compute # days in each month; I will merge this file to the catch draw file to ensure each day 
*contains 100 catch draws across 100 iterations
preserve
clear 
set obs 2
gen day=td(1jan2022) if _n==1
replace day=td(31dec2022) if _n==2
format day %td
tsset day
tsfill, full
gen day_i=_n
gen month=month(day)
gen tab=1
egen ndays=sum(tab), by(month)
gen wv2="1" if inlist(month, 1, 2)
replace wv2="2" if inlist(month, 3,4)
replace wv2="3" if inlist(month, 5,6)
replace wv2="4" if inlist(month, 7,8)
replace wv2="5" if inlist(month, 9,10)
replace wv2="6" if inlist(month, 11,12)
keep month ndays wv2
duplicates drop 
*tempfile ndays
*save `ndays', replace 
save "ndays.dta", replace 
restore


/*
There are discrepencies in total MRIP catch versus simulated MRIP catch due to how we account for MRIP grouped-harvest.
In some MRIP intercepts, total contributers to harvest is higher than number of id_codes (interviewees) on a given trip. This means
the interview did not collect release information for all anglers on the trip. Because we interested in estimated catch-per-angler not catch-per-trip, 
as the utility model is at the individual level, we divide harvest (claim) by the number of contributers and release by total interviewees. This leads
to the "new" total survey-weighted harvest (and thus total catch) for the trip being lower than the "old" survey-weighted estimate of total catch.   

To account for this discrepency, I estimate total catch per domain using both methods. Where the % difference is more than 5%, I will reallocate 
the difference across all trips that already caught that species of fish. Because the difference leads to decimal values, which we then round, I first round
and again recalclate the difference. If the difference is again greater than 5%, I randomly a number of trips that caught fish and reduce their catch by
X fish until the difference is below 5%.
  
*/
encode my_dom_id_string, gen(my_dom_id)

gen scup_flag=.
gen scup_diff=.

gen bsb_flag=.
gen bsb_diff=.

gen sf_flag=.
gen sf_diff=.

gen scup_harv_flag=.
gen scup_harv_diff=.

gen bsb_harv_flag=.
gen bsb_harv_diff=.

gen sf_harv_flag=.
gen sf_harv_diff=.



replace tot_cat_sf=round(tot_cat_sf)
replace tot_cat_bsb=round(tot_cat_bsb)
replace tot_cat_scup=round(tot_cat_scup)

replace landing_sf=round(landing_sf)
replace landing_bsb=round(landing_bsb)
replace landing_scup=round(landing_scup)

replace release_sf=round(release_sf)
replace release_bsb=round(release_bsb)
replace release_scup=round(release_scup)

/*
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_landing if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_sf if first==1 & state=="NJ" & mode1=="fh"

svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total bsb_landing if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_bsb if first==1 & state=="NJ" & mode1=="fh"

svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total bsb_tot_cat if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_bsb if first==1 & state=="NJ" & mode1=="fh"


svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_tot_cat if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_sf if first==1 & state=="NJ" & mode1=="fh"
*/


replace new_wp_int=round(new_wp_int)
levelsof my_dom_id_string if common_dom=="SF", local(doms)
foreach d of local doms {

*scup	
*total catch 
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total scup_tot_cat if my_dom_id_string=="`d'"
local base = r(table)[1,1]
di `base' 

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_scup if first==1 & my_dom_id_string=="`d'"
local new = r(table)[1,1]
di `new'

di `base'/`new'
local scalar= abs(1-`base'/`new')
di `scalar'

local diff_n_fish= `base'-`new'
di `diff_n_fish'

replace scup_flag=1 if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.
replace scup_diff = `diff_n_fish' if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.


*harvest  
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total scup_landing if my_dom_id_string=="`d'"
local base = r(table)[1,1]
di `base' 

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_scup if first==1 & my_dom_id_string=="`d'"
local new = r(table)[1,1]
di `new'

di `base'/`new'
local scalar= abs(1-`base'/`new')
di `scalar'

local diff_n_fish= `base'-`new'
di `diff_n_fish'

replace scup_harv_flag=1 if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.
replace scup_harv_diff = `diff_n_fish' if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.

*bsb
*total catch
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total bsb_tot_cat if my_dom_id_string=="`d'"
local base = r(table)[1,1]
di `base' 

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_bsb if first==1 & my_dom_id_string=="`d'"
local new = r(table)[1,1]
di `new'

di `base'/`new'
local scalar= abs(1-`base'/`new')
di `scalar'

local diff_n_fish= `base'-`new'
di `diff_n_fish'

replace bsb_flag=1 if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.
replace bsb_diff = `diff_n_fish' if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.

*harvest
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total bsb_landing if my_dom_id_string=="`d'"
local base = r(table)[1,1]
di `base' 

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_bsb if first==1 & my_dom_id_string=="`d'"
local new = r(table)[1,1]
di `new'

di `base'/`new'
local scalar= abs(1-`base'/`new')
di `scalar'

local diff_n_fish= `base'-`new'
di `diff_n_fish'

replace bsb_harv_flag=1 if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.
replace bsb_harv_diff = `diff_n_fish' if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.

*sf
*total catch
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_tot_cat if my_dom_id_string=="`d'"
local base = r(table)[1,1]
di `base' 

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_sf if first==1 & my_dom_id_string=="`d'"
local new = r(table)[1,1]
di `new'

di `base'/`new'
local scalar= abs(1-`base'/`new')
di `scalar'

local diff_n_fish= `base'-`new'
di `diff_n_fish'

replace sf_flag=1 if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.
replace sf_diff = `diff_n_fish' if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.


*harvest
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_landing if my_dom_id_string=="`d'"
local base = r(table)[1,1]
di `base' 

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_sf if first==1 & my_dom_id_string=="`d'"
local new = r(table)[1,1]
di `new'

di `base'/`new'
local scalar= abs(1-`base'/`new')
di `scalar'

local diff_n_fish= `base'-`new'
di `diff_n_fish'

replace sf_harv_flag=1 if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.
replace sf_harv_diff = `diff_n_fish' if my_dom_id_string=="`d'" &  `scalar'>.025 & `scalar' !=.

}
/*
svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_landing if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_sf if first==1 & state=="NJ" & mode1=="fh"

svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_tot_cat if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_sf if first==1 & state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_bsb if first==1 & state=="NJ" & mode1=="fh"

svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total bsb_landing if state=="NJ" & mode1=="fh" & wv2=="3"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total landing_bsb if first==1 & state=="NJ" & mode1=="fh" & wv2=="3"

svyset psu_id [pweight= wp_int] , strata(strat_id) singleunit(certainty)
svy: total sf_tot_cat if state=="NJ" & mode1=="fh"

svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy: total tot_cat_sf if first==1 & state=="NJ" & mode1=="fh"
*/

destring month, replace

keep if first==1

*Build a data set of trip catches. 
*The simulation model will draw 50 trips * 30 catch draws per state-mode-day. 
*Catch per trip is estimated at the state-wave-mode level. 
*I will create 10,000 draws of catch per trip for each state-mode-month combination. 
*Then, in the calibration model, we will randomly select a set of 50 * 30 catch draws for each state-mode-day



*The following code tries to account for rounding error. 

tempfile base0
save `base0', replace

global catchez

levelsof my_dom_id_string if common_dom=="SF" /*& state="NJ"*/, local(doms)

foreach d of local doms{

u `base0', clear
 
keep if my_dom_id_string=="`d'"
keep my_dom_id_string state mode1 month wv2 tot_cat_sf tot_cat_bsb tot_cat_scup new_wp_int  common_dom *_diff landing_scup landing_bsb landing_sf release_scup release_bsb release_sf




drop month
gen month=.
expand 2, gen(dup)
replace month=1 if wv2=="1" & dup==0
replace month=2 if wv2=="1" & dup==1

replace month=3 if wv2=="2" & dup==0
replace month=4 if wv2=="2" & dup==1

replace month=5 if wv2=="3" & dup==0
replace month=6 if wv2=="3" & dup==1

replace month=7 if wv2=="4" & dup==0
replace month=8 if wv2=="4" & dup==1

replace month=9 if wv2=="5" & dup==0
replace month=10 if wv2=="5" & dup==1

replace month=11 if wv2=="6" & dup==0
replace month=12 if wv2=="6" & dup==1
drop dup 


replace scup_diff=round(scup_diff/2)
replace bsb_diff=round(bsb_diff/2)
replace sf_diff=round(sf_diff/2)

replace scup_harv_diff=round(scup_harv_diff/2)
replace bsb_harv_diff=round(bsb_harv_diff/2)
replace sf_harv_diff=round(sf_harv_diff/2)

*merge m:1 month wv2 using "ndays.dta", keep(3) nogen 

tempfile base
save `base', replace

levelsof month , local(mnths)
	foreach m of local mnths{
								
	u `base', clear
	keep if month==`m'
	expand new_wp_int
	
	
	gen id=_n
	
	*scup tot_cat adjustments 
	su scup_diff
	local flags=`r(N)'
	
	if `flags'!=0  {
	su scup_diff
	return list
	local nsamp=`r(mean)'

	if `nsamp'>=0  {
	preserve 
	sample `nsamp', count
	gen tot_cat_scup_new=tot_cat_scup+1
	keep id tot_cat_scup_new
	tempfile adj_scup
	save `adj_scup', replace
	restore
	
	merge 1:1 id using `adj_scup' 
	replace tot_cat_scup = tot_cat_scup_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	if `nsamp'<0  {
	preserve 
	keep if tot_cat_scup>0
	local nsamp_neg=abs(`nsamp')
	sample `nsamp_neg', count
	gen tot_cat_scup_new=tot_cat_scup-1
	keep id tot_cat_scup_new
	tempfile adj_scup
	save `adj_scup', replace
	restore
	
	merge 1:1 id using `adj_scup' 
	replace tot_cat_scup = tot_cat_scup_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	
	
	}
	
	else{
		
	}
	
	*scup harv adjustments 
	su scup_harv_diff
	local flags=`r(N)'
	
	if `flags'!=0  {
	su scup_harv_diff
	return list
	local nsamp=`r(mean)'

	if `nsamp'>=0  {
	preserve 
	sample `nsamp', count
	gen landing_scup_new=landing_scup+1
	keep id landing_scup_new
	tempfile adj_scup
	save `adj_scup', replace
	restore
	
	merge 1:1 id using `adj_scup' 
	replace landing_scup = landing_scup_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	if `nsamp'<0  {
	preserve 
	keep if landing_scup>0
	local nsamp_neg=abs(`nsamp')
	sample `nsamp_neg', count
	gen landing_scup_new=landing_scup-1
	keep id landing_scup_new
	tempfile adj_scup
	save `adj_scup', replace
	restore
	
	merge 1:1 id using `adj_scup' 
	replace landing_scup = landing_scup_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	
	}
	
	else{
		
	}
	
	*bsb tot_cat adjustments 
	su bsb_diff
	return list
	local flags=`r(N)'
	
	if `flags'!=0  {
	su bsb_diff
	return list
	local nsamp=`r(mean)'

	if `nsamp'>=0  {
	preserve 
	sample `nsamp', count
	gen tot_cat_bsb_new=tot_cat_bsb+1
	keep id tot_cat_bsb_new
	tempfile adj_bsb
	save `adj_bsb', replace
	restore
	
	merge 1:1 id using `adj_bsb' 
	replace tot_cat_bsb = tot_cat_bsb_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	if `nsamp'<0  {
	preserve 
	keep if tot_cat_bsb>0
	local nsamp_neg=abs(`nsamp')
	sample `nsamp_neg', count
	gen tot_cat_bsb_new=tot_cat_bsb-1
	keep id tot_cat_bsb_new
	tempfile adj_bsb
	save `adj_bsb', replace
	restore
	
	merge 1:1 id using `adj_bsb' 
	replace tot_cat_bsb = tot_cat_bsb_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	
	}
	
	else{
		
	}
	
	*bsb harv adjustments 
	su bsb_harv_diff
	return list
	local flags=`r(N)'
	
	if `flags'!=0  {
	su bsb_harv_diff
	return list
	local nsamp=`r(mean)'

	if `nsamp'>=0  {
	preserve 
	sample `nsamp', count
	gen landing_bsb_new=landing_bsb+1
	keep id landing_bsb_new
	tempfile adj_bsb
	save `adj_bsb', replace
	restore
	
	merge 1:1 id using `adj_bsb' 
	replace landing_bsb = landing_bsb_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	if `nsamp'<0  {
	preserve 
	keep if landing_bsb>0
	local nsamp_neg=abs(`nsamp')
	sample `nsamp_neg', count
	gen landing_bsb_new=landing_bsb-1
	keep id landing_bsb_new
	tempfile adj_bsb
	save `adj_bsb', replace
	restore
	
	merge 1:1 id using `adj_bsb' 
	replace landing_bsb = landing_bsb_new if _merge==3
	drop _merge 
	}
	
	else{
	}
	
	}
	
	else{
		
	}
	
	*sf tot_cat adjustments 
	su sf_diff
	return list
	local flags=`r(N)'
	
	if `flags'!=0  {
	su sf_diff
	return list
	local nsamp=`r(mean)'

	if `nsamp'>=0  {

	preserve 
	sample `nsamp', count
	gen tot_cat_sf_new=tot_cat_sf+1
	keep id tot_cat_sf_new
	tempfile adj_sf
	save `adj_sf', replace
	restore
	
	merge 1:1 id using `adj_sf' 
	replace tot_cat_sf = tot_cat_sf_new if _merge==3
	drop _merge 
	}
	
	else{
		
	}
	
	if `nsamp'<0  {

	preserve 
	keep if tot_cat_sf>0
	local nsamp_neg=abs(`nsamp')
	sample `nsamp_neg', count
	gen tot_cat_sf_new=tot_cat_sf-1
	keep id tot_cat_sf_new
	tempfile adj_sf
	save `adj_sf', replace
	restore
	
	merge 1:1 id using `adj_sf' 
	replace tot_cat_sf = tot_cat_sf_new if _merge==3
	drop _merge 
	}
	
	else{
		
	}
	
	
	}
	
	else{
		
	}
	
	*sf harv adjustments 
	su sf_harv_diff
	return list
	local flags=`r(N)'
	di `flags'
	
	if `flags'!=0  {
	su sf_harv_diff
	return list
	local nsamp=`r(mean)'
	di `nsamp'

	if `nsamp'>=0  {

	preserve 
	sample `nsamp', count
	gen landing_sf_new= landing_sf+1
	keep id landing_sf_new
	tempfile adj_sf
	save `adj_sf', replace
	restore
	
	merge 1:1 id using `adj_sf' 
	replace landing_sf = landing_sf_new if _merge==3
	drop _merge 
	}
	
	else{
		
	}
	
	if `nsamp'<0  {

	preserve 
	keep if landing_sf>0
	local nsamp_neg=abs(`nsamp')
	sample `nsamp_neg', count
	gen landing_sf_new= landing_sf-1
	keep id landing_sf_new
	tempfile adj_sf
	save `adj_sf', replace
	restore
	
	merge 1:1 id using `adj_sf' 
	replace landing_sf = landing_sf_new if _merge==3
	drop _merge 
	}
	
	else{
		
	}
	
	}
	
	else{
		
	}
	
	
	count 
	local num=`r(N)'
	if `num'<=20000{

	local expand = round(20000/`num')+2
	expand `expand' 
		
	sample 20000, count 

	tempfile catchez`d'`m'
	save `catchez`d'`m'', replace
	global catchez "$catchez "`catchez`d'`m''" " 
	}
	
	else{
	sample 20000, count
							
	
	tempfile catchez`d'`m'
	save `catchez`d'`m'', replace
	global catchez "$catchez "`catchez`d'`m''" " 
	}
}
}

dsconcat $catchez
drop my_dom_id_string common_dom

**Estimate mean catch and harvest per trip
*gen scup_rel_check = tot_cat_scup-landing_scup
*browse  tot_cat_scup  landing_scup scup_rel_check release_scup if scup_rel_check!=0


*drop new_wp
order state mode wv month


replace tot_cat_bsb=landing_bsb if landing_bsb>tot_cat_bsb & landing_bsb!=0
replace tot_cat_scup=landing_scup if landing_scup>tot_cat_scup & landing_scup!=0
replace tot_cat_sf=landing_sf if landing_sf>tot_cat_sf & landing_sf!=0

replace release_scup=tot_cat_scup-landing_scup
replace release_sf=tot_cat_sf-landing_sf
replace release_bsb=tot_cat_bsb-landing_bsb

su tot_cat_scup
su tot_cat_sf
su tot_cat_bsb

su landing_scup
su landing_sf
su landing_bsb

su release_scup
su release_sf
su release_bsb

/*
preserve
tempfile new1
save `new1', replace 
levelsof domain, local(doms)
foreach d of local doms{
	u `new1', 
	
}
*/

save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\fluke catch draws 2022 new4.dta", replace 
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\fluke catch draws 2022 new4.csv", replace 


*import delimited using   "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\fluke catch draws 2022 new.csv", clear 

*create pr/fh modes
/*
preserve 
keep if mode=="bt"
replace mode="fh"
tempfile new
save `new', replace
restore

replace mode="pr" if mode=="bt"
append using `new'
*/
*export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\fluke catch draws 2022 new.csv", replace 




import delimited using  "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\fluke catch draws 2022 new4.csv", clear
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

tempfile base1
save `base1', replace
levelsof domain /*if state=="NJ"*/, local(doms) clean
foreach d of local doms{
u `base1', clear 
keep if domain=="`d'"
export delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `d'.csv", replace 

}



**NJ
/*
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="NJ"
drop draw
tempfile base
save `base', replace

forv i=11/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\NJ catch draws 2022 draw4 `i'.csv", replace 


}
*/



**MA
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="MA"
drop draw
tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\MA catch draws 2022 draw4 `i'.csv", replace 


}




**RI
/*
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="RI"
drop draw
tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\RI catch draws 2022 draw4 `i'.csv", replace 


}
*/




**CT
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="CT"
drop draw
tempfile base
save `base', replace

forv i=39/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\CT catch draws 2022 draw4 `i'.csv", replace 


}





**NY
/*
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="NY"
drop draw
tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\NY catch draws 2022 draw4 `i'.csv", replace 


}

*/

**DE
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="DE"
drop draw
tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\DE catch draws 2022 draw4 `i'.csv", replace 


}




**MD
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="MD"
drop draw
tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\MD catch draws 2022 draw4 `i'.csv", replace 


}



**VA
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="VA"
drop draw
tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\VA catch draws 2022 draw4 `i'.csv", replace 


}





**NC
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0

keep if state=="NC"
drop draw
tempfile base
save `base', replace

forv i=79/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="`d'"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	import delimited using  "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\fluke catch draws 2022 new4 `dom'.csv", clear
	*keep if state=="`st'" & mode1=="`md'" & month==`mnth' 
	sample 1500, count
	
	egen tripid = seq(), f(1) t(50)
	sort tripid
	bysort tripid: gen catch_draw=_n
	
	gen day="`day1'"
	gen day_i=`day_i1'

	keep state mode1 month day_i  day tripid catch_draw tot* landing* release* 
	
	egen mean_sf_tot_cat = mean(tot_cat_sf)
	egen mean_bsb_tot_cat = mean(tot_cat_bsb)
	egen mean_scup_tot_cat = mean(tot_cat_scup)

	egen mean_sf_harvest = mean(landing_sf)
	egen mean_bsb_harvest = mean(landing_bsb)
	egen mean_scup_harvest = mean(landing_scup)

	egen mean_sf_release = mean(release_sf)
	egen mean_bsb_release = mean(release_bsb)
	egen mean_scup_release = mean(release_scup)

	
	*drop *new
	
	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

	
}

dsconcat $domainz

gen draw=`i'

export delimited using "C:\Users\andrew.carr-harris\Desktop\HCR catch data 2022\NC catch draws 2022 draw4 `i'.csv", replace 


}