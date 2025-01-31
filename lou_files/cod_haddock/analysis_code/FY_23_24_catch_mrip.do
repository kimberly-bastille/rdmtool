
cd $input_data_cd

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
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
*keep if $calibration_year
keep if inlist(year, 2023, 2024)

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
replace state="ME" if st==23
replace state="NH" if st==33


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

*classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (Cod or Haddock) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (Cod or Haddock) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


*OLD MRIP site allocations

/*
*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_data_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
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
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


tostring wave, gen(wv2)
tostring year, gen(yr2)

destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td
drop _merge 

merge m:1 date using "$input_data_cd\cod_open_season_dates.dta"
drop if _merge==2
drop _merge
gen season="op" if cod_season_open==1
replace season="cl" if season==""
drop cod_season_open


gen my_dom_id_string=common_dom+"_"+wv2+"_"+area_s+"_"+mode1+"_"+yr2
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

*gen my_dom_id_string=area_s+"_"+mode1+"_"+common_dom+"_"+season

gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
gen cod_claim=claim if common=="atlanticcod"
egen sum_cod_claim=sum(cod_claim), by(strat_id psu_id id_code)

gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)

gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

gen hadd_claim=claim if common=="haddock"
egen sum_hadd_claim=sum(hadd_claim), by(strat_id psu_id id_code)


drop cod_tot_cat cod_harvest cod_releases hadd_tot_cat hadd_harvest hadd_releases  hadd_claim  cod_claim
rename sum_cod_tot_cat cod_catch
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_cod_claim cod_claim

rename sum_hadd_tot_cat hadd_catch
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel
rename sum_hadd_claim hadd_claim


**Round these estimates to align with catch-per-trip computations
/*
local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
	replace `v'=round(`v')
	
}
*/
/* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="ATLCO"
keep if area_s=="GOM"

*replace my_dom_id_string=mode1+"_"+season

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

tempfile base
save `base', replace

global catchez

local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
u `base', clear 
svy: total `v' , over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor

split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

*rename b `v'
gen var="`v'"
*rename rname my_dom_id_string

keep my_dom_id_string var b se ll ul

tempfile catchez`v'
save `catchez`v'', replace
global catchez "$catchez "`catchez`v''" " 

}

dsconcat $catchez

split my_dom, parse(_)
rename my_dom_id_string2 wave
rename my_dom_id_string4 mode
rename my_dom_id_string5 year

drop my_dom_id_string my_dom_id_string1 my_dom_id_string3 
keep mode wave year var b ll ul se

reshape wide b se ll ul, i(mode wave year) j(var) string
renvarlab b*, predrop(1)
ds mode wave year, not
renvarlab `r(varlist)', postfix(_mrip)
sort mode year wave 

ds mode wave year se* ll* ul*, not
mvencode `r(varlist)', mv(0) over
sort mode year wave
encode year, gen(yr2)
encode wave, gen(wv2)
/*
local vars cod_catch_mrip cod_claim_mrip cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_claim_mrip hadd_keep_mrip hadd_rel_mrip
foreach v of local vars{
	replace se`v'=`v' if se`v'==0
	replace ll`v' = `v'-1.96*se`v' if se`v'==`v'
	replace ul`v' = `v'+1.96*se`v' if se`v'==`v'

	}
*/

drop if wave=="6"
destring wave, replace

gen wave_2023 = wave - 0.1 if year == "2023"
gen wave_2024 = wave + 0.1 if year == "2024"

twoway (rcap llcod_catch_mrip ulcod_catch_mrip wave_2023 if year=="2023" & mode=="pr",  vertical lcolor(blue)) ///
       (scatter cod_catch_mrip wave_2023 if year=="2023" & mode=="pr",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_catch_mrip ulcod_catch_mrip wave_2024 if year=="2024" & mode=="pr", vertical   lcolor(red)) ///
       (scatter cod_catch_mrip wave_2024 if year=="2024" & mode=="pr", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Cod catch, # fish", size(medium)) ///
       title("Cod catch estimates by wave, private sector", size(medium))
graph export "$figure_cd/cod_catch_pr_23_24.png", as(png) replace


twoway (rcap llcod_keep_mrip ulcod_keep_mrip wave_2023 if year=="2023" & mode=="pr",  vertical lcolor(blue)) ///
       (scatter cod_keep_mrip wave_2023 if year=="2023" & mode=="pr",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_keep_mrip ulcod_keep_mrip wave_2024 if year=="2024" & mode=="pr", vertical   lcolor(red)) ///
       (scatter cod_keep_mrip wave_2024 if year=="2024" & mode=="pr", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Cod harvest (a+b1), # fish", size(medium)) ///
       title("Cod harvest estimates by wave, private sector", size(medium))
graph export "$figure_cd/cod_harvest_pr_23_24.png", as(png) replace
	   
	   
twoway (rcap llcod_catch_mrip ulcod_catch_mrip wave_2023 if year=="2023" & mode=="fh",  vertical lcolor(blue)) ///
       (scatter cod_catch_mrip wave_2023 if year=="2023" & mode=="fh",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_catch_mrip ulcod_catch_mrip wave_2024 if year=="2024" & mode=="fh", vertical   lcolor(red)) ///
       (scatter cod_catch_mrip wave_2024 if year=="2024" & mode=="fh", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Cod catch, # fish", size(medium)) ///
       title("Cod catch estimates by wave, for-hire sector", size(medium))
graph export "$figure_cd/cod_catch_fh_23_24.png", as(png) replace


twoway (rcap llcod_keep_mrip ulcod_keep_mrip wave_2023 if year=="2023" & mode=="fh",  vertical lcolor(blue)) ///
       (scatter cod_keep_mrip wave_2023 if year=="2023" & mode=="fh",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_keep_mrip ulcod_keep_mrip wave_2024 if year=="2024" & mode=="fh", vertical   lcolor(red)) ///
       (scatter cod_keep_mrip wave_2024 if year=="2024" & mode=="fh", ylabel(#15,labsize(vsmall) angle(horizontal) )  msize(vsmall) msymbol(circle) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Cod harvest (a+b1), # fish", size(medium)) ///
       title("Cod harvest estimates by wave, for-hire sector", size(medium))
graph export "$figure_cd/cod_harvest_fh_23_24.png", as(png) replace
	   
	   
	   
twoway (rcap llhadd_catch_mrip ulhadd_catch_mrip wave_2023 if year=="2023" & mode=="pr",  vertical lcolor(blue)) ///
       (scatter hadd_catch_mrip wave_2023 if year=="2023" & mode=="pr",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_catch_mrip ulhadd_catch_mrip wave_2024 if year=="2024" & mode=="pr", vertical   lcolor(red)) ///
       (scatter hadd_catch_mrip wave_2024 if year=="2024" & mode=="pr", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Haddock catch, # fish", size(medium)) ///
       title("Haddock catch estimates by wave, private sector", size(medium))
graph export "$figure_cd/hadd_catch_pr_23_24.png", as(png) replace
	   

twoway (rcap llhadd_keep_mrip ulhadd_keep_mrip wave_2023 if year=="2023" & mode=="pr",  vertical lcolor(blue)) ///
       (scatter hadd_keep_mrip wave_2023 if year=="2023" & mode=="pr",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_keep_mrip ulhadd_keep_mrip wave_2024 if year=="2024" & mode=="pr", vertical   lcolor(red)) ///
       (scatter hadd_keep_mrip wave_2024 if year=="2024" & mode=="pr", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Haddock harvest (a+b1), # fish", size(medium)) ///
       title("Haddock harvest estimates by wave, private sector", size(medium))
graph export "$figure_cd/hadd_keep_pr_23_24.png", as(png) replace
	   
	   
twoway (rcap llhadd_catch_mrip ulhadd_catch_mrip wave_2023 if year=="2023" & mode=="fh",  vertical lcolor(blue)) ///
       (scatter hadd_catch_mrip wave_2023 if year=="2023" & mode=="fh",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_catch_mrip ulhadd_catch_mrip wave_2024 if year=="2024" & mode=="fh", vertical   lcolor(red)) ///
       (scatter hadd_catch_mrip wave_2024 if year=="2024" & mode=="fh", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Haddock catch, # fish", size(medium)) ///
       title("Haddock catch estimates by wave, for-hire sector", size(medium))
graph export "$figure_cd/hadd_catch_fh_23_24.png", as(png) replace


twoway (rcap llhadd_keep_mrip ulhadd_keep_mrip wave_2023 if year=="2023" & mode=="fh",  vertical lcolor(blue)) ///
       (scatter hadd_keep_mrip wave_2023 if year=="2023" & mode=="fh",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_keep_mrip ulhadd_keep_mrip wave_2024 if year=="2024" & mode=="fh", vertical   lcolor(red)) ///
       (scatter hadd_keep_mrip wave_2024 if year=="2024" & mode=="fh", ylabel(#15,labsize(vsmall) angle(horizontal) )  msize(vsmall) msymbol(circle) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Haddock harvest (a+b1), # fish", size(medium)) ///
       title("Haddock harvest estimates by wave, for-hire sector", size(medium))   
graph export "$figure_cd/hadd_keep_fh_23_24.png", as(png) replace
	   
	
***************Both sectors combined

cd $input_data_cd

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
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
*keep if $calibration_year
keep if inlist(year, 2023, 2024)

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
replace state="ME" if st==23
replace state="NH" if st==33


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

*classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (Cod or Haddock) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (Cod or Haddock) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


*OLD MRIP site allocations

/*
*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_data_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
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
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224

gen FY=2023 if year==2023 & inlist(wave, 3, 4, 5, 6)
replace FY=2023 if year==2024 & inlist(wave, 1, 2)
replace FY=2024 if year==2024 & inlist(wave, 3, 4, 5, 6)

tostring wave, gen(wv2)
tostring year, gen(yr2)
tostring FY, gen(fyr2)

destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td
drop _merge 

merge m:1 date using "$input_data_cd\cod_open_season_dates.dta"
drop if _merge==2
drop _merge
gen season="op" if cod_season_open==1
replace season="cl" if season==""
drop cod_season_open


gen my_dom_id_string=common_dom+"_"+wv2+"_"+area_s+"_"+fyr2
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

*gen my_dom_id_string=area_s+"_"+mode1+"_"+common_dom+"_"+season

gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
gen cod_claim=claim if common=="atlanticcod"
egen sum_cod_claim=sum(cod_claim), by(strat_id psu_id id_code)

gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)

gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

gen hadd_claim=claim if common=="haddock"
egen sum_hadd_claim=sum(hadd_claim), by(strat_id psu_id id_code)


drop cod_tot_cat cod_harvest cod_releases hadd_tot_cat hadd_harvest hadd_releases  hadd_claim  cod_claim
rename sum_cod_tot_cat cod_catch
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_cod_claim cod_claim

rename sum_hadd_tot_cat hadd_catch
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel
rename sum_hadd_claim hadd_claim


**Round these estimates to align with catch-per-trip computations
/*
local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
	replace `v'=round(`v')
	
}
*/

/* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="ATLCO"
keep if area_s=="GOM"

*replace my_dom_id_string=mode1+"_"+season


encode strat_id, gen(strat_id2)
encode psu_id, gen(psu_id2)

*replace wp_int=round(wp_int)
svyset psu_id2 [pweight= wp_int], strata(strat_id2) singleunit(certainty)

*encode my_dom_id_string, gen(my_dom_id)

*svy: total hadd_catch, over(my_dom_id2)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

tempfile base
save `base', replace

global catchez

local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
u `base', clear 
svy: total `v' , over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor

split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

*rename b `v'
gen var="`v'"
*rename rname my_dom_id_string

keep my_dom_id_string var b se ll ul

tempfile catchez`v'
save `catchez`v'', replace
global catchez "$catchez "`catchez`v''" " 

}

dsconcat $catchez

split my_dom, parse(_)
rename my_dom_id_string2 wave
rename my_dom_id_string4 FY
drop if FY=="."
drop my_dom_id_string my_dom_id_string1 my_dom_id_string3 
keep  wave year var b ll ul se

reshape wide b se ll ul, i( wave year) j(var) string
renvarlab b*, predrop(1)
ds  wave year, not
renvarlab `r(varlist)', postfix(_mrip)
sort  year wave 

ds  wave year se* ll* ul*, not
mvencode `r(varlist)', mv(0) over
sort  year wave
encode year, gen(yr2)
encode wave, gen(wv2)
/*
local vars cod_catch_mrip cod_claim_mrip cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_claim_mrip hadd_keep_mrip hadd_rel_mrip
foreach v of local vars{
	replace se`v'=`v' if se`v'==0
	replace ll`v' = `v'-1.96*se`v' if se`v'==`v'
	replace ul`v' = `v'+1.96*se`v' if se`v'==`v'

	}
*/

*drop if wave=="6"
destring wave, replace

gen wave_2023 = wave - 0.1 if FY == "2023"
gen wave_2024 = wave + 0.1 if FY == "2024"

twoway (rcap llcod_catch_mrip ulcod_catch_mrip wave_2023 if FY=="2023" ,  vertical lcolor(blue)) ///
       (scatter cod_catch_mrip wave_2023 if FY=="2023" ,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_catch_mrip ulcod_catch_mrip wave_2024 if FY=="2024" , vertical   lcolor(red)) ///
       (scatter cod_catch_mrip wave_2024 if FY=="2024" , ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Cod catch, # fish", size(medium)) ///
       title("Cod catch estimates by wave, all fishing modes", size(medium))
graph export "$figure_cd/cod_catch_all_23_24.png", as(png) replace
	   

twoway (rcap llcod_keep_mrip ulcod_keep_mrip wave_2023 if year=="2023" ,  vertical lcolor(blue)) ///
       (scatter cod_keep_mrip wave_2023 if year=="2023" ,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_keep_mrip ulcod_keep_mrip wave_2024 if year=="2024" , vertical   lcolor(red)) ///
       (scatter cod_keep_mrip wave_2024 if year=="2024" , ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Cod harvest (a+b1), # fish", size(medium)) ///
       title("Cod harvest estimates by wave,all fishing modes", size(medium))
graph export "$figure_cd/cod_harvest_all_23_24.png", as(png) replace
	   

	   
twoway (rcap llhadd_catch_mrip ulhadd_catch_mrip wave_2023 if year=="2023" ,  vertical lcolor(blue)) ///
       (scatter hadd_catch_mrip wave_2023 if year=="2023" ,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_catch_mrip ulhadd_catch_mrip wave_2024 if year=="2024" , vertical   lcolor(red)) ///
       (scatter hadd_catch_mrip wave_2024 if year=="2024" , ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Haddock catch, # fish", size(medium)) ///
       title("Haddock catch estimates by wave, all fishing modes", size(medium))
graph export "$figure_cd/hadd_catch_all_23_24.png", as(png) replace
	   

twoway (rcap llhadd_keep_mrip ulhadd_keep_mrip wave_2023 if year=="2023" ,  vertical lcolor(blue)) ///
       (scatter hadd_keep_mrip wave_2023 if year=="2023" ,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_keep_mrip ulhadd_keep_mrip wave_2024 if year=="2024" , vertical   lcolor(red)) ///
       (scatter hadd_keep_mrip wave_2024 if year=="2024" , ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("Haddock harvest (a+b1), # fish", size(medium)) ///
       title("Haddock harvest estimates by wave,all fishing modes", size(medium))   
graph export "$figure_cd/hadd_keep_all_23_24.png", as(png) replace
	   
	   
***************Both sectors combined, whole year exluding wave 6 2023

cd $input_data_cd

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
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
*keep if $calibration_year
keep if inlist(year, 2023, 2024)

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
replace state="ME" if st==23
replace state="NH" if st==33


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

*classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (Cod or Haddock) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (Cod or Haddock) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


*OLD MRIP site allocations

/*
*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_data_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
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
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


tostring wave, gen(wv2)
tostring year, gen(yr2)

destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td
drop _merge 

merge m:1 date using "$input_data_cd\cod_open_season_dates.dta"
drop if _merge==2
drop _merge
gen season="op" if cod_season_open==1
replace season="cl" if season==""
drop cod_season_open


gen my_dom_id_string=common_dom+"_"+area_s+"_"+yr2
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

*gen my_dom_id_string=area_s+"_"+mode1+"_"+common_dom+"_"+season

gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
gen cod_claim=claim if common=="atlanticcod"
egen sum_cod_claim=sum(cod_claim), by(strat_id psu_id id_code)

gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)

gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

gen hadd_claim=claim if common=="haddock"
egen sum_hadd_claim=sum(hadd_claim), by(strat_id psu_id id_code)


drop cod_tot_cat cod_harvest cod_releases hadd_tot_cat hadd_harvest hadd_releases  hadd_claim  cod_claim
rename sum_cod_tot_cat cod_catch
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_cod_claim cod_claim

rename sum_hadd_tot_cat hadd_catch
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel
rename sum_hadd_claim hadd_claim


**Round these estimates to align with catch-per-trip computations
/*
local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
	replace `v'=round(`v')
	
}
*/

/* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="ATLCO"
keep if area_s=="GOM"

*replace my_dom_id_string=mode1+"_"+season


encode strat_id, gen(strat_id2)
encode psu_id, gen(psu_id2)
drop if wave=="6"

*replace wp_int=round(wp_int)
svyset psu_id2 [pweight= wp_int], strata(strat_id2) singleunit(certainty)

*encode my_dom_id_string, gen(my_dom_id)

*svy: total hadd_catch, over(my_dom_id2)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

tempfile base
save `base', replace

global catchez

local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
u `base', clear 
svy: total `v' , over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor

split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

*rename b `v'
gen var="`v'"
*rename rname my_dom_id_string

keep my_dom_id_string var b se ll ul

tempfile catchez`v'
save `catchez`v'', replace
global catchez "$catchez "`catchez`v''" " 

}

dsconcat $catchez

split my_dom, parse(_)
rename my_dom_id_string3 year

drop my_dom_id_string my_dom_id_string1 my_dom_id_string2 
keep  year var b ll ul se

reshape wide b se ll ul, i( year) j(var) string
renvarlab b*, predrop(1)
ds   year, not
renvarlab `r(varlist)', postfix(_mrip)
sort  year  

ds   year se* ll* ul*, not
mvencode `r(varlist)', mv(0) over
sort  year 
encode year, gen(yr2)

/*
local vars cod_catch_mrip cod_claim_mrip cod_keep_mrip cod_rel_mrip hadd_catch_mrip hadd_claim_mrip hadd_keep_mrip hadd_rel_mrip
foreach v of local vars{
	replace se`v'=`v' if se`v'==0
	replace ll`v' = `v'-1.96*se`v' if se`v'==`v'
	replace ul`v' = `v'+1.96*se`v' if se`v'==`v'

	}
*/

destring year, replace

preserve
clear
set obs 4
gen year=2021 if _n==1
replace year=2022 if _n==2
replace year=2025 if _n==3
replace year=2026 if _n==4
tempfile yrs
save `yrs', replace 
restore

append using `yrs'
sort year

gen year_2023 = year - 0.1 if year == 2023
gen year_2024 = year + 0.1 if year == 2024

twoway (rcap llcod_catch_mrip ulcod_catch_mrip year  if year<=2023,  vertical lcolor(blue)) ///
       (scatter cod_catch_mrip year  if year<=2023,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_catch_mrip ulcod_catch_mrip year  if year>2023, vertical   lcolor(red)) ///
       (scatter cod_catch_mrip year  if year>2023, ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("", size(medium)) xlab(2023 2024) ///
       ytitle("Cod catch, # fish", size(medium)) ///
       title("Cod catch estimates by year (exluding wave 6), all fishing modes", size(medium))
graph export "$figure_cd/cod_catch_all_annual_23_24.png", as(png) replace
	   

twoway (rcap llcod_keep_mrip ulcod_keep_mrip year  if year<=2023,  vertical lcolor(blue)) ///
       (scatter cod_keep_mrip year  if year<=2023,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llcod_keep_mrip ulcod_keep_mrip year   if year>2023, vertical   lcolor(red)) ///
       (scatter cod_keep_mrip year if year>2023, ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("", size(medium)) xlab(2023 2024) ///
       ytitle("Cod harvest, # fish", size(medium)) ///
       title("Cod harvest estimates by year (exluding wave 6), all fishing modes", size(medium))
graph export "$figure_cd/cod_harvest_all_annual_23_24.png", as(png) replace
	   

	   
twoway (rcap llhadd_catch_mrip ulhadd_catch_mrip year  if year<=2023,  vertical lcolor(blue)) ///
       (scatter hadd_catch_mrip year  if year<=2023,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_catch_mrip ulhadd_catch_mrip year  if year>2023, vertical   lcolor(red)) ///
       (scatter hadd_catch_mrip year  if year>2023, ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("", size(medium)) xlab(2023 2024) ///
       ytitle("Haddock catch, # fish", size(medium)) ///
       title("Haddock catch estimates by year (exluding wave 6), all fishing modes", size(medium))
graph export "$figure_cd/hadd_catch_all_annual_23_24.png", as(png) replace
	   

twoway (rcap llhadd_keep_mrip ulhadd_keep_mrip year  if year<=2023,  vertical lcolor(blue)) ///
       (scatter hadd_keep_mrip year  if year<=2023,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  llhadd_keep_mrip ulhadd_keep_mrip year   if year>2023, vertical   lcolor(red)) ///
       (scatter hadd_keep_mrip year if year>2023, ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("", size(medium)) xlab(2023 2024) ///
       ytitle("Haddock harvest, # fish", size(medium)) ///
       title("Haddock harvest estimates by year (exluding wave 6), all fishing modes", size(medium))
graph export "$figure_cd/hadd_harvest_all_annual_23_24.png", as(png) replace
	   
	   
	   
****************Fishing effort 


cd $input_data_cd

clear
global fluke_effort

tempfile tl1 cl1
dsconcat $triplist

/* *dtrip will be used to estimate total directed trips, do not change it*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
keep if inlist(year, 2023, 2024)


/* THIS IS THE END OF THE DATA MERGING CODE */

 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
*tempfile tc1
*save `tc1'

 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 

tostring wave, gen(w2)
tostring year, gen(year2)
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

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")


*New MRIP site allocations
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
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh)*/
gen my_dom_id_string=area_s+"_"+year2+"_"+w2+"_"+mode1+"_"+ dom_id
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)
*svy: total dtrip if area_s=="GOM" & dom_id=="1"  
*svy: total dtrip if area_s=="GOM" & dom_id=="1", over(mode2)

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se  ll ul
gen pse=(se/b)*100

split my, parse(_)
rename my_dom_id_string1 area_s
rename my_dom_id_string2 year
rename my_dom_id_string3 wave
rename my_dom_id_string4 mode
rename my_dom_id_string5 dom_id
drop my_dom_id_string
rename b dtrip

keep if dom_id=="1"
keep if area_s=="GOM"


*drop if wave=="6"
destring wave, replace

gen wave_2023 = wave - 0.1 if year == "2023"
gen wave_2024 = wave + 0.1 if year == "2024"

twoway (rcap ll ul wave_2023 if year=="2023" & mode=="pr",  vertical lcolor(blue)) ///
       (scatter dtrip wave_2023 if year=="2023"  & mode=="pr",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  ll ul  wave_2024 if year=="2024"  & mode=="pr", vertical   lcolor(red)) ///
       (scatter dtrip wave_2024 if year=="2024"  & mode=="pr", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("# directed trips", size(medium)) ///
       title("Directed trips for cod and haddock, private sector", size(medium))
	   
twoway (rcap ll ul wave_2023 if year=="2023" & mode=="fh",  vertical lcolor(blue)) ///
       (scatter dtrip wave_2023 if year=="2023"  & mode=="fh",   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  ll ul  wave_2024 if year=="2024"  & mode=="fh", vertical   lcolor(red)) ///
       (scatter dtrip wave_2024 if year=="2024"  & mode=="fh", ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("# directed trips", size(medium)) ///
       title("Directed trips for cod and haddock, for-hire sector", size(medium))
	   	  
		  
***all sectors combined

/*This code uses the MRIP data to 
	1) estimate dircetd trips and their standard error at the year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh) level during the calibraiton period, 
	2) use those paramters to create 150 random draws of directed trips for each stratum,
	3) divide each random draw by the number of days in that stratum to obtain an estimate of trips-per-day calibration period, 
	4) compute for each stratum an calender year adjustment = (# of calender days in that stratum for the projection period)/(# of calender days in that stratum for the calibration period), 
		that we will use to correct for differences in the number of calinder days in each stratum between the calibration and projection period, 
	5) set the baseline year and projection year regulations ("$input_code_cd/set regulations.do")
*/
		

cd $input_data_cd

clear
global fluke_effort

tempfile tl1 cl1
dsconcat $triplist

/* *dtrip will be used to estimate total directed trips, do not change it*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
keep if inlist(year, 2023, 2024)


/* THIS IS THE END OF THE DATA MERGING CODE */

 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
*tempfile tc1
*save `tc1'

 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 

tostring wave, gen(w2)
tostring year, gen(year2)
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

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")


*New MRIP site allocations
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
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh)*/
gen my_dom_id_string=area_s+"_"+year2+"_"+w2+"_"+ dom_id
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)
*svy: total dtrip if area_s=="GOM" & dom_id=="1"  
*svy: total dtrip if area_s=="GOM" & dom_id=="1", over(mode2)

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se  ll ul
gen pse=(se/b)*100

split my, parse(_)
rename my_dom_id_string1 area_s
rename my_dom_id_string2 year
rename my_dom_id_string3 wave
rename my_dom_id_string4 dom_id
drop my_dom_id_string
rename b dtrip

keep if dom_id=="1"
keep if area_s=="GOM"


*drop if wave=="6"
destring wave, replace

gen wave_2023 = wave - 0.1 if year == "2023"
gen wave_2024 = wave + 0.1 if year == "2024"

twoway (rcap ll ul wave_2023 if year=="2023" ,  vertical lcolor(blue)) ///
       (scatter dtrip wave_2023 if year=="2023"  ,   msymbol(circle) msize(vsmall) mcolor(blue)) ///
       (rcap  ll ul  wave_2024 if year=="2024" , vertical   lcolor(red)) ///
       (scatter dtrip wave_2024 if year=="2024" , ylabel(#15,labsize(vsmall) angle(horizontal) ) msymbol(circle) msize(vsmall) mcolor(red)), ///
       legend(order(2 "2023 Estimates" ///
                    4 "2024 Estimates") region(lcolor(none)) position(11) ring(0) rows(2) size(small)) ///
       xtitle("wave", size(medium)) ///
       ytitle("# directed trips", size(medium)) ///
       title("Directed trips for cod and haddock, all fishing modes", size(medium))
	   

	   

**********
*catch-per-trip

cd $input_data_cd

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
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
*keep if $calibration_year
keep if inlist(year, 2023, 2024)

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
replace state="ME" if st==23
replace state="NH" if st==33


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

*classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (Cod or Haddock) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (Cod or Haddock) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


*OLD MRIP site allocations

/*
*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_data_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
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
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


tostring wave, gen(wv2)
tostring year, gen(yr2)

destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td
drop _merge 

merge m:1 date using "$input_data_cd\cod_open_season_dates.dta"
drop if _merge==2
drop _merge
gen season="op" if cod_season_open==1
replace season="cl" if season==""
drop cod_season_open


gen my_dom_id_string=common_dom+"_"+wv2+"_"+area_s+"_"+mode1+"_"+yr2
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

*gen my_dom_id_string=area_s+"_"+mode1+"_"+common_dom+"_"+season

gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
gen cod_claim=claim if common=="atlanticcod"
egen sum_cod_claim=sum(cod_claim), by(strat_id psu_id id_code)

gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)

gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

gen hadd_claim=claim if common=="haddock"
egen sum_hadd_claim=sum(hadd_claim), by(strat_id psu_id id_code)


drop cod_tot_cat cod_harvest cod_releases hadd_tot_cat hadd_harvest hadd_releases  hadd_claim  cod_claim
rename sum_cod_tot_cat cod_catch
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_cod_claim cod_claim

rename sum_hadd_tot_cat hadd_catch
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel
rename sum_hadd_claim hadd_claim


**Round these estimates to align with catch-per-trip computations
/*
local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
	replace `v'=round(`v')
	
}
*/
/* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="ATLCO"
keep if area_s=="GOM"

*replace my_dom_id_string=mode1+"_"+season

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

tempfile base
save `base', replace

global catchez

local vars cod_catch cod_keep cod_rel hadd_catch hadd_keep hadd_rel cod_claim hadd_claim
foreach v of local vars{
u `base', clear 
*svy: total `v' , over(my_dom_id)
svy: mean `v' , over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor

split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

*rename b `v'
gen var="`v'"
*rename rname my_dom_id_string

keep my_dom_id_string var b se ll ul

tempfile catchez`v'
save `catchez`v'', replace
global catchez "$catchez "`catchez`v''" " 

}

dsconcat $catchez

split my_dom, parse(_)
rename my_dom_id_string2 wave
rename my_dom_id_string4 mode
rename my_dom_id_string5 year

drop my_dom_id_string my_dom_id_string1 my_dom_id_string3 
keep mode wave year var b ll ul se

reshape wide b se ll ul, i(mode wave year) j(var) string
renvarlab b*, predrop(1)
ds mode wave year, not
renvarlab `r(varlist)', postfix(_mrip)
sort mode year wave 

ds mode wave year se* ll* ul*, not
mvencode `r(varlist)', mv(0) over
sort mode year wave
encode year, gen(yr2)
encode wave, gen(wv2) 		  
		  