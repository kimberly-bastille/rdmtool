

*This file generates MRIP point estimates of directed trips in the calibration year across the simulation strata (for FY2025, these were species and cod open season)


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
*drop strat_interval
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
*keep if strmatch(common, "summerflounder") | strmatch(common,"summerflounder")
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
drop if strmatch(id_code, "*xx*")==1

replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
keep if $calibration_year


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




/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")


*OLD MRIP site allocations
/*
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_data_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

/*classify into GOM or GBS */
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
drop _merge 

/*classify into GOM or GBS */
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,511, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224

gen date2=substr(id_code, 6,8)
gen month1=substr(date2, 5, 2)
gen day1=substr(date2, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace
destring month1, replace

gen date=mdy( month1, day1, year)
format date %td

merge m:1 date using "$input_data_cd\cod_open_season_dates.dta"
drop if _merge==2
drop _merge
gen season="op" if cod_season_open==1
replace season="cl" if season==""
drop cod_season_open



gen my_dom_id_string=dom_id+"_"+season+"_"+area_s+"_"+mode1
*gen my_dom_id_string=dom_id+"_"+w2+"_"+area_s+"_"+mode_fx

replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
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
svy: total dtrip if area_s=="GOM" & dom_id=="1"  
/*
--------------------------------------------------------------
             |             Linearized
             |      Total   std. err.     [95% conf. interval]
-------------+------------------------------------------------
       dtrip |     234459    8745.14      217174.6    251743.4
--------------------------------------------------------------
*/

svy: total dtrip if area_s=="GOM" & dom_id=="1", over(mode2)
/*
---------------------------------------------------------------
              |             Linearized
              |      Total   std. err.     [95% conf. interval]
--------------+------------------------------------------------
c.dtrip@mode2 |
          fh  |   57211.25   4593.586      48132.21    66290.28
          pr  |     161487   7441.536      146779.1    176194.9
          sh  |   15760.72          .             .           .
---------------------------------------------------------------

*/


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

rename my_dom_id_string1 dom_id
rename my_dom_id_string2 season
rename my_dom_id_string3 area_s
rename my_dom_id_string4 mode
keep if  dom_id=="1"
drop my_dom_id_string
rename b dtrip
/*
rename my_dom_id_string1 dom_id
rename my_dom_id_string2 wave
rename my_dom_id_string3 area_s
rename my_dom_id_string4 mode
keep if  dom_id=="1"
drop my_dom_id_string
rename b dtrip
*/

keep if area_s=="GOM"

rename ll ll_dtrip 
rename ul ul_dtrip
keep dtrip season mode  ul_dtrip ll_dtrip
order season mode
*browse if mode!="3"
sort season mode
save  "$input_data_cd\MRIP_dtrip_totals_open_season.dta", replace 


