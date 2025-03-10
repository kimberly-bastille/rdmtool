


global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)


******************************************************
******************************************************
*Begin fluke catch-at-length distribution 
******************************************************
******************************************************


set matsize 10000
/* Use these to control the years for which the MRIP data is polled/queried*/


global yearlist   2019 2020 2021 2022 
global wavelist 1 2 3 4 5 6

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global catchlist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}



/*SIZE_LIST */
global sizelist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}


***ALS 2021
**no mode-specific numbers
cd "Z:\size data 2023 RDM"
import excel using "ALS_FLK_2021.xlsx", clear first 
renvarlab, lower

split date, pars("/")
destring date3, replace
tab date3, missing
replace date3=2022 if date3==6202
keep if inlist(date3, 2021)
rename date3 year

tempfile als2021
save `als2021', replace

**Updated ALS release data from Tony pre-2021
import excel using "ALS_fluke_ALL.xlsx", clear first 
renvarlab, lower
split date, pars("/")
destring date3, replace
keep if inlist(date3, 2020, 2019)
rename date3 year

append using `als2021', force

tempfile als_pre22
save `als_pre22', replace


***ALS 2022 - 2023
import excel using "ALS_FLK_22_23.xlsx", clear first 
renvarlab, lower
split date, pars("/")
destring date3, replace

rename date3 year

append using `als_pre22'
drop if year==2023

keep length release year placetagged tag zonedescription


split placetagged, pars(",")
replace placetagged2=ltrim(rtrim(placetagged2))
replace placetagged3=ltrim(rtrim(placetagged3))


gen state="MA" if placetagged2=="MA"
replace state="RI" if placetagged2=="RI"
replace state="CT" if placetagged2=="CT"
replace state="NY" if placetagged2=="NY"
replace state="NJ" if placetagged2=="NJ"
replace state="DE" if placetagged2=="DE"
replace state="MD" if placetagged2=="MD"
replace state="VA" if placetagged2=="VA"
replace state="NC" if placetagged2=="NC"

replace state="MA" if placetagged3=="MA" & state==""
replace state="RI" if placetagged3=="RI" & state==""
replace state="CT" if placetagged3=="CT" & state==""
replace state="NY" if placetagged3=="NY" & state==""
replace state="NJ" if placetagged3=="NJ" & state==""
replace state="DE" if placetagged3=="DE" & state==""
replace state="MD" if placetagged3=="MD" & state==""
replace state="VA" if placetagged3=="VA" & state==""
replace state="NC" if placetagged3=="NC" & state==""

*browse if state==""

drop if strmatch(placetagged, "* SC")==1

replace state= "NJ" if strmatch(placetagged, "* NJ")==1
replace state= "NY" if strmatch(placetagged, "* NY")==1
replace state= "NY" if strmatch(placetagged, "* NYC")==1
replace state= "DE" if strmatch(zonedescription, "* DE")==1
replace state= "MD" if strmatch(zonedescription, "* MD")==1
replace state= "NJ" if strmatch(placetagged, "* Nj")==1
replace state= "NY" if strmatch(placetagged, "Long Island Sound")==1
replace state= "NY" if strmatch(placetagged, "Cold Spring Harbor")==1
replace state= "NJ" if strmatch(placetagged, "Axel Carson Reef")==1
replace state= "CT" if strmatch(placetagged, "*CT")==1
replace state= "NJ" if strmatch(zonedescription, "NJ*")==1
replace state= "NY" if strmatch(placetagged, "*NY*")==1
replace state= "RI" if strmatch(zonedescription, "*Rhode Island*")==1
replace state= "NY" if strmatch(zonedescription, "*NY*")==1
replace state= "NJ" if strmatch(placetagged, "*South Amboy*")==1
replace state= "NJ" if strmatch(placetagged, "*S. Amboy*")==1
replace state= "NY" if strmatch(placetagged, "*Brooklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Bklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Rockaway*")==1
replace state= "NJ" if strmatch(placetagged, "*Atlantic Beach Reef*")==1
replace state= "NY" if strmatch(placetagged, "*Coney Island*")==1
replace state= "NY" if strmatch(zonedescription, "*Fire Island Inlet*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keansburg*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keyport*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Mattituck, LIS*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Middle Ground Light, LIS*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Navesink*")==1 & state==""
replace state= "CT" if strmatch(placetagged, "*Sandy Hook*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Perth Amboy*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Verizano Bridge*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Raritan Bay, Buoy 41*")==1 & state==""
replace state= "NY" if strmatch(zonedescription, "*Raritan Bay*")==1 & state==""

drop if state==""


*browse
expand release


keep state year length
gen source="ALS"

tempfile als
save `als', replace 


*CT VAS 
*we do have mode-specific values, but not a lot 
import excel using "CT VAS Report_2013_2021_DRMolnar 01112022.xlsx", clear first 
renvarlab, lower
*keep if inlist(year, 2017, 2016)
*keep if inlist(speciescaught, "Flounder, Summer", "Scup","Bass, Black Sea") 
keep if inlist(speciescaught, "Flounder, Summer") 

keep if disposition=="RELEASED"
keep quantity catchlength year tripmode
expand quantity
drop quantity 
*gen nfish=1
rename catchlength length
drop if length=="-"
destring length, replace
gen state="CT"
keep if inlist(year, 2021, 2020, 2019)
gen source="CT_VAS"

tempfile ct_vas_pre22
save `ct_vas_pre22', replace 


import excel using "CT_VAS_2022.xlsx", clear first 
renvarlab, lower
*keep if inlist(year, 2017, 2016)
*keep if inlist(speciescaught, "Flounder, Summer", "Scup","Bass, Black Sea") 
keep if inlist(catch_common_name, "FLOUNDER, SUMMER") 
keep if disp=="RLSD"
expand quantity
drop quantity 
rename length_tlinches length
destring length, replace
keep if inlist(year, 2022)
gen source="CT_VAS"
keep year length state source
append using `ct_vas_pre22'

tempfile ct_vas
save `ct_vas', replace


*NJ VAS
*some shore values, but not many 
import excel using "NJ_VAS_SF_thru2021.xlsx", clear first 
renvarlab, lower
rename fishyear year
keep if inlist(year, 2020, 2019)
keep if disp=="Rel"
keep year length 


tempfile nj_vas_pre22
save `nj_vas_pre22', replace

import excel using "NJ_VAS_FLK_2022.xlsx", clear first 
renvarlab, lower
rename fishyear year
keep if inlist(year, 2022,2021)
keep if disp=="Rel"
keep year length 

append using `nj_vas_pre22'

gen state="NJ"
gen source="NJ_VAS"

tempfile nj_vas
save `nj_vas', replace


*RI VAS
import excel using "RI_lengths_all_2022.xlsx", clear first 
renvarlab, lower
*keep if inlist(species, "Flounder, summer", "Scup","Bass, Black Sea") 
keep if inlist(common, "FLOUNDER, SUMMER") 
gen year=year(tripstartdate)


keep if inlist(year, 2022, 2021, 2020, 2019)
keep if inlist(disposition,"DEAD DISCARD", "RELEASED", "RELEASED ALIVE", "TOO SMALL")
*keep lengthin year reported

drop if reported=="-"
destring reported, replace
expand reported
drop reported
rename lengthin length
tab length, missing
drop if length==0
keep length state year 


gen source="RI_VAS"


tempfile ri_vas
save `ri_vas', replace



*MA VAS
*not a lot of shore 
import excel using "MA_VAS_SF-scup-BSB.xlsx", clear first 
gen year= year(sample_date) 
keep if inlist(year, 2022, 2021, 2020, 2019)

keep if common=="fluke"
keep if status=="R"

rename total_l length
keep length year
gen state="MA"

gen source="MA_VAS"

tempfile MA_vas
save `MA_vas', replace


*MRIP release data 
clear 
mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeb2

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1', replace
clear

 

dsconcat $b2list
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2019, 2020, 2021, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1', replace

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

*Here, group together NC and VA
*replace state="VA" if st==37


 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
 gen common_dom="zzzzzz"
 replace common_dom="SF" if strmatch(common, "summerflounder") 
 
 keep if common_dom=="SF"
 
 keep state l_in_bin year
 rename l_in_bin length 
 gen source="MRIP"



 
 

**********End editing individual state file
append using  `MA_vas'
append using `ri_vas'
append using `nj_vas'
append using `ct_vas'
append using `als'

tostring year, gen(yr2)
gen state_year=state+"_"+yr2
drop if length==.
gen fish=1


preserve
collapse (sum) fish, by(state year source)
keep if year==2022
reshape wide fish, i( source year) j(state) string
restore



*twoway kdensity length if year==2019, lcol(blue) || kdensity length if year==2020, lcol(red) || kdensity length if year==2021, lcol(orange) ||  kdensity length if year==2022, lcol(green)


***Now combine the data 
**For now, start with combining the data by region: MA-NY, NJ, DE-MD
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

tabstat fish, stat(sum) by(region)

gen nfish=1
drop if length==0

replace length=round(length)

collapse (sum)  nfish, by(length year region)

egen sumfish=sum(nfish), by(region year)
gen prop_b2=nfish/sumfish
sort reg len

drop nfish sumfish

rename len l_in_bin

tempfile proportions_b2
save `proportions_b2', replace 


***Now pull keep lengths from MRIP 
clear

mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2019, 2020, 2021, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
gen common_dom="zzzzzz"
replace common_dom="SF" if strmatch(common, "summerflounder") 
 


tostring wave, gen(w2)
tostring year, gen(year2)

gen my_dom_id_string=region+"_"+year2+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "summerflounder")==0

drop if strmatch(common, "summerflounder")==0

gen measured=1 if !inlist(l_in_bin, ., 0) & lngth_imp==0
tabstat measured if year==2022, stat(sum) by(state)


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)

gen nobs=1
gen state_year=state+"_"+year2
tabstat nobs if year2=="2022", stat(sum) by(state)

	svy: tab l_in_bin my_dom_id_string, count
	/*save some stuff  
	matrix of proportions, row names, column names, estimate of total population size*/
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
rename eR l_in_bin
	
ds l, not
renvarlab `r(varlist)', prefix(i)
	
reshape long i, i(l_in) j(new) string
split new, parse(_)
rename new1 region
rename new2 year
destring year, replace 
drop new
rename i harvest


egen sumfish=sum(harvest), by(region year)
gen prop_ab1=harvest/sumfish


drop harvest sumfish
merge 1:1 l_in region year using  `proportions_b2'

drop _merge
mvencode prop*, mv(0) override

sort year region l_in



/*
preserve
keep if year==2022
encode region, gen(reg2)
xtset reg2 l_in
drop if l_in==.
tsfill, full
levelsof region, local(regs)
foreach r of local regs{
mvencode prop*, mv(0) override
twoway(scatter prop_ab1 l_in if region=="`r'" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="`r'" & year==2022, connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "released fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("`r'") xlab(4(2)34 ) ylab(, angle(horizontal)) ytitle("Proportion of total harvest or release") name("gr`r'", replace))
			
}
grc1leg grNO grSO grNJ, cols(3) ycommon
restore
		 
twoway(scatter prop_ab1 l_in if region=="NJ" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="NJ" & year==2022, connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "released fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("") xlab(4(2)34 ) ylab(, angle(horizontal)) ytitle("Proportion of total harvest or release"))			 

twoway(scatter prop_ab1 l_in if region=="SO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="SO" & year==2022, connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "released fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("") xlab(4(2)34 ) ylab(, angle(horizontal)) ytitle("Proportion of total harvest or release"))					 
			 
restore
*/			 

tempfile proportions
save `proportions', replace 


*********estimate total harvest and release by region and year from MRIP 
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

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

*keep if mode1=="bt"


 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 


tostring wave, gen(wv2)
tostring year, gen(yr2)


gen my_dom_id_string=region+"_"+yr2+"_"+common_dom

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
encode my_dom_id_string, gen(my_dom_id)

local vars release landing tot_cat
foreach v of local vars{
	
	gen sf_`v'=`v' if common=="summerflounder"


}


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

local vars sf_release   sf_landing   sf_tot_cat  
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
drop if strmatch(my_dom_id, "*ZZ")==1
drop ll ul
reshape wide b se, i(my_dom_id) j( var) string
ds my se*, not
*renvarlab `r(varlist)', predrop(1)

split my, parse("_")
rename my_dom_id_string1 region
rename my_dom_id_string2 year
destring year, replace 
drop my_dom_id_string my_dom_id_string3

*rename sf_landing sf_harvest 
*rename sf_release sf_releases 

*order region   sf_landing sf_release sf_tot_cat

*keep region sf_release sf_harvest year
merge 1:m region  year using `proportions'


gen tot_keep= prop_ab1*bsf_landing
gen tot_rel= prop_b2*bsf_release
gen tot_cat=tot_keep+tot_rel

/*
preserve 
keep year region l_in_bin tot_rel
keep if year==2022
sort reg l
save "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\calibration stats and tests\sf_release_at_length_2022.dta", replace 
restore
*/
collapse (sum) tot_cat, by(l_ region year)




**now compute probabilities 
egen sum_tot=sum(tot_cat),  by(region year)
format tot_cat sum_tot %20.0gc
gen prob_catch=tot_cat/sum_tot

tostring year, gen(yr2)
gen reg_year=region+"_"+yr2
rename l_ length 

keep if year==2022 

sort region length 
preserve 
rename prob observed_prob
rename length fitted_length
keep fitted_length observed_prob region year reg_year tot_cat
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


/*
twoway(scatter prob_catch l_in if region=="NO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prob_catch l_in if region=="SO" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i))
scatter prop l_in if region=="DE_NC", connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) xlab(15)
scatter prop l_in if region=="ME_NY", connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) xlab(15)
*/

****estimate gamma parameters for each distirbution


tempfile new
save `new', replace
global fitted_sizes

levelsof reg_year , local(regs)
foreach r of local regs{
u `new', clear

keep if reg_year=="`r'"
keep length tot
replace tot=round(tot)
expand tot
drop if tot==0
gammafit length
local alpha=e(alpha)
local beta=e(beta)

gen gammafit=rgamma(`alpha', `beta')
replace gammafit=round(gammafit, .5)
gen nfish=1
collapse (sum) nfish, by(gammafit)

drop if gammafit<6.5 | gammafit>31.5

egen sumnfish=sum(nfish)
gen fitted_prob=nfish/sumnfish
gen reg_year="`r'"

tempfile fitted_sizes`r'
save `fitted_sizes`r'', replace
global fitted_sizes "$fitted_sizes "`fitted_sizes`r''" " 
}
clear
dsconcat $fitted_sizes
rename gammafit fitted_length		   

merge 1:1 fitted_length reg_year using `observed_prob'
sort reg_year fitted_length 
mvencode fitted_prob observed_prob, mv(0) override
split reg_year, parse(_)
drop region year 
rename reg_year1 region
rename reg_year2 year
destring year, replace

replace observed = . if observed ==0

egen sum_tot_catch=sum(tot_cat), by(reg_year)

*cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
*save "fluke_fitted_sizes.dta", replace 
save "fluke_fitted_sizes_3regs.dta", replace 



cd "Z:\size data 2023 RDM"


*u  "fluke_fitted_sizes.dta", clear 
u  "fluke_fitted_sizes_3regs.dta", clear 
keep if year==2022


preserve
keep reg_year sum_tot
duplicates drop 
tempfile tot_cat
save `tot_cat', replace 
restore 

collapse (sum) fitted_prob observed_prob, by(region year reg_year fitted_length)
merge m:1 reg_year using `tot_cat'

gen catch_at_length=fitted_prob*sum_tot_catch
drop _merge
rename fitted_length length
replace observed_prob=. if observed_prob==0


/*
u fluke_fitted_sizes_adjusted_3regs.dta, clear 

gen observed2 = observed/2

twoway(scatter observed2 length if region=="NO" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(gs12)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob length if region=="NO"  & year==2022 , connect(direct) lcol(black)   lwidth(medthick)  lpat(dash) msymbol(i)   ///
		   xtitle("Length (inches)", yoffset(-2)) ytitle("Proportion of total catch")    ylab(, angle(horizontal) labsize(vsmall)) ///
		   legend(lab(1 "raw data") lab(2 "fitted (gamma) data") cols() yoffset(-2) region(color(none)))  xlab(4(2)36,  labsize(vsmall))  $graphoptions )
*/		   
/*
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

levelsof region, local(sts)
foreach s of local sts{
twoway(scatter observed_prob length if region=="`s'" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob length if region=="`s'"  & year==2022 , connect(direct) lcol(black)   lwidth(medthick)  lpat(dash) msymbol(i)   ///
		   xtitle("Length (inches)", yoffset(-2)) ytitle("Proportion of total catch")    ylab(, angle(horizontal) labsize(vsmall)) ///
		   legend(lab(1 "Raw data") lab(2 "Fitted data") cols() yoffset(-2) region(color(none)))  xlab(4(2)36,  labsize(vsmall))  $graphoptions name("gr`s'", replace))
			
}
grc1leg grNO grSO grNJ, cols(3) ycommon

twoway(scatter fitted_prob length if region=="NO" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
			(scatter fitted_prob length if region=="NJ" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
			(scatter fitted_prob length if region=="SO" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions  ///
			 legend(lab(1 "NO") lab(2 "NJ") lab(3 "SO") cols(3)))
			 
*/			 


**now adjust for p_star
u  "fluke_fitted_sizes_adjusted_3regs.dta", clear 

expand 4 if region=="NO", gen(dup)
sort region length dup

bysort region  length: gen tab=_n
sort region  length
replace region="MA" if region=="NO" & tab==1
replace region="RI" if region=="NO" & tab==2
replace region="CT" if region=="NO" & tab==3
replace region="NY" if region=="NO" & tab==4
drop dup tab

expand 4 if region=="SO", gen(dup)
sort region length dup

bysort region  length: gen tab=_n
replace region="DE" if region=="SO" & tab==1
replace region="MD" if region=="SO" & tab==2
replace region="VA" if region=="SO" & tab==3
replace region="NC" if region=="SO" & tab==4
drop dup tab

rename region state
sort state length


cd "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data"
save "fluke_catch_at_length_2022.dta", replace 







cd "C:\Users\andrew.carr-harris\Desktop\RDM data and code"








			 
**Now compute recreational selectivity 
**import the numbers at age, use age-length key to convert to inches, then merge back to catch-at-length 
*input age-length key used to age the recreational catch
cd "Z:\size data 2023 RDM"
import delimited using "NMFS_bottom_trawl_survey.txt", clear
tab stratum

tostring cruise, gen(cruise2)
gen year=substr(cruise2, 1, 4)
destring year, replace
gen count=1
collapse (sum) count, by(year svspp age length cruise2)

gen cruise3 = substr(cruise2, 5, 2)
destring cruise3, replace
tab cruise3

*keep only NEFSC trawl
drop if cruise3>10

*last five years of data 
keep if inlist(year, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013) 

/*
svspp codes:
summer flounder =103
bsb =141
scup =143
*/

keep if svspp==103

replace age=7 if age>=7

replace length=length/2.54
replace length=round(length, .5)
collapse (sum) count, by(svspp age length)



preserve
clear 
set obs 100
gen length=6.5 if _n==1
replace length=length[_n-1]+.5 if _n!=1
keep if length<=31.5
gen length_tab=_n
tempfile length_tab
save `length_tab', replace 
restore

merge m:1 length using `length_tab', nogen

collapse (sum) count, by(svspp age length_tab)

sort age length_tab
xtset age length_tab

tsfill, full
mvencode count, mv(0) override
egen sum_count=sum(count), by(age)
gen prop_age_a=count/sum_count
sort age length_tab


preserve 
keep length_tab prop_age_a age
tempfile rawdata
save `rawdata', replace 
restore

collapse (sum) count, by( age length_tab)
sort  age length_tab 

sort age length
tempfile new
save `new'

global sizes
levelsof age, local(ages)
foreach a of local ages{
u `new', clear
keep if age==`a'

*lowess smoother
sort length

lowess count length, bwidth(.3) gen(prop_age_a_smoothed) nograph adjust 
replace prop_age_a_smoothed=0 if prop_age_a_smoothed<0
replace age=`a' if age==.

*collapse (sum) new_prop, by(age length)

su prop_age_a_smoothed
replace prop_age_a_smoothed=prop_age_a_smoothed/`r(sum)'

tempfile sizes`a'
save `sizes`a'', replace
global sizes "$sizes "`sizes`a''" " 
}
clear
dsconcat $sizes

merge 1:1 age length using `rawdata', nogen
merge m:1 length using `length_tab', nogen


mvencode prop_age_a, mv(0) override
sort age length   

*Plots showing the raw versus smoothed data 
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

levelsof age, local(ages)
foreach a of local ages{
	twoway(scatter prop_age_a length  if age==`a', connect(direct) lcol(black)  lwidth(medium)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter prop_age_a_smoothed length if age==`a', connect(direct) lcol(red)  lwidth(medium)  lpat(solid) msymbol(i) $graphoptions name("age`a'", replace ) ///
				ytitle("", size(small)) title("Age `a'", size(small)) xtitle("", size(small)) xlab(6(2)32, grid labsize(vsmall)) ///
				ytick(, labsize(vsmall) angle(horizontal)) ylab(, angle(horizontal) labsize(vsmall) )  graphregion(margin(small)) ///
				legend(lab(1 "raw data (NEFSC trawl, 2013 - 2022 combined)") lab(2 "lowess-smoothed data (bandwidth=0.3)") size(vsmall)  cols(2) region(color(none))) )
}
grc1leg  age0 age1 age2 age3 age4 age5 age6 age7, cols(2) ycommon xcommon graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) 



save "fluke_age_length_conversion.dta", replace







**Here, pull in the Numbers-at-age data for the baseline year to compute baseline recreational selectivity. 
**Translate numbers-at-age to numbers-at-length using the (smoothed) conversion key above 
**We have one distribution of catch-at-length and have 1,000 draws of numbers-at-length.
**Compute rec. selectivity (q) for every draw of numbers-at-length and take the median value. 


cd "Z:\size data 2023 RDM"
import excel using "Fluke_N_at_age_2022.xlsx", clear first 
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace
drop if iter==.


/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )
*/

/*
graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 fluke numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 
replace nfish=nfish*1000
tabstat nfish, stat(sum) by(i) format(%12.0gc)
/*randomly select 100 draws
bysort iter: gen rand=runiform() if _n==1
egen mean_rand=mean(rand), by(iter)
sort mean_rand age
egen group =group(mean_rand)
*/
 
tempfile new
save `new', replace

global sizes
forv i=1/1000{
u `new', clear 
keep if iter==`i'
merge 1:m age using "fluke_age_length_conversion.dta"

sort age length
gen n_fish_population=nfish*prop_age_a_smoothed

tempfile sizes`i'
save `sizes`i'', replace
global sizes "$sizes "`sizes`i''" " 
}
clear
dsconcat $sizes

replace n_fish_population=n_fish_population/1000
/*
graph box n_fish_population , over(length) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
 
replace n_fish_population=n_fish_population*1000
 su n_fish_population if iteration==1
 return list
***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length


tempfile new
save `new', replace

global selec
forv i=1/1000{
u `new', clear 
keep if iter==`i'

collapse (sum) n_fish, by(length iter)

tempfile new2
save `new2', replace 

u "fluke_fitted_sizes_adjusted_3regs.dta", clear 
levelsof reg_year, local(regs)

	foreach r of local regs{
	u "fluke_fitted_sizes_adjusted_3regs.dta", clear 
	keep if reg_year=="`r'"
	
	merge 1:1 length using `new2'
	gen rec_selec=catch_at_length/n_fish
	
	tempfile selec`i'`r'
	save `selec`i'`r'', replace
	global selec "$selec "`selec`i'`r''" " 
	
	}
}
clear
dsconcat $selec
	
/*	
tempfile new
save `new', replace

global selec
forv i=1/1000{
u `new', clear 
keep if iter==`i'

tempfile new2
save `new2', replace 
drop if reg_year==""

levelsof reg_year, local(regs)

	foreach r of local regs{
	use `new2', clear
	
	su rec_selec 
	gen normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) 

	su n_fish_population 
	gen normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) 
 
	su catch_at_length 
	
	gen normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) 	
	tempfile selec`i'`r'
	save `selec`i'`r'', replace
	global selec "$selec "`selec`i'`r''" " 
	
	}
}
clear
dsconcat $selec	
collapse (mean) normal_rec_selec normal_n_fish_population normal_catch_at_length, by(length region)
*/	
	
/*
graph box rec_selec if region=="NO" , over(length, label(labsize(vsmall))) $graphoptions title("Distribution of 2022 recreational selectivity, MA-NY", size(medium)) ytitle("q{sub:l}", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 
graph box rec_selec if region=="SO" , over(length, label(angle(45) labsize(tiny))) $graphoptions title("Distribution of 2022 rec. selectivity, NJ-NC" "(catch-at-length/pop. numbers-at-length)", size(medium)) ytitle("rec. selectivity", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides
 */

*now take the median value of rec. selectivity by length  
collapse (median) rec_selec n_fish_population  catch_at_length, by(length region)
sort region length

*scale all the values so max is one and put them on the same graph 
preserve
keep if iter==321
su rec_selec if region=="NO"
gen normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="NO"

su n_fish_population if region=="NO"
gen normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="NO"
 
su catch_at_length if region=="NO"
gen normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="NO"
 
su rec_selec if region=="SO"
replace normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="SO"

su n_fish_population if region=="SO"
replace normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="SO"
 
su catch_at_length if region=="SO"
replace normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="SO" 


su rec_selec if region=="NJ"
replace normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="NJ"

su n_fish_population if region=="NJ"
replace normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="NJ"
 
su catch_at_length if region=="NJ"
replace normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="NJ" 

gen diff=abs(catch_at_length- n_fish_population)
 
twoway 	(scatter normal_rec_selec length if region=="NO", connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="NO", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="NO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)
				
restore				
				
twoway 	(scatter normal_rec_selec length if region=="SO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="SO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="SO", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)
twoway 	(scatter normal_rec_selec length if region=="NJ", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="NJ", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="NJ", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)  
 

*save "fluke_rec_selectivity_2022.dta", replace  
save "fluke_rec_selectivity_2022_3reg.dta", replace  

 twoway 	(scatter catch_at_length length if region=="NO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter catch_at_length length  if region=="SO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
				  legend(lab(1 "North (MA-NY)") lab(2 "South (NJ-NC)")) xlab(, labsize(vsmall) angle(horizontal)) ///
				  ylab(, labsize(vsmall) angle(horizontal)) ytitle("Recreational catch", size(small) xoffset(-1)) xtitle("Length (inches)")  )


u "fluke_rec_selectivity_2022_3reg.dta", clear 

*create prob(catch at length) distributions for 2024
cd "Z:\size data 2023 RDM"
import excel using "Fluke_N_at_age_2024.xlsx", clear first 
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace
drop if iter==.

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )
*/

/*
graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 fluke numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 

*randomly select 100 draws
bysort iter: gen rand=runiform() if _n==1
egen mean_rand=mean(rand), by(iter)
sort mean_rand age
egen draw =group(mean_rand)

keep if draw<=100
drop rand mean_rand iter
order draw age nfish

*collapse (mean) mean_fish = nfish (sd) sd_nfish=nfish, by(age)

tempfile new
save `new', replace

global sizes
qui forv i=1/100{
u `new', clear 
keep if draw==`i'
merge 1:m age using "fluke_age_length_conversion.dta"

sort age length
gen n_fish_population=nfish*prop_age_a_smoothed

tempfile sizes`i'
save `sizes`i'', replace
global sizes "$sizes "`sizes`i''" " 
}
clear
dsconcat $sizes
collapse (sum) n_fish_population, by(length draw)
sort draw length

collapse (mean) mean_fish = n_fish (sd) sd_nfish=n_fish, by(length)

*replace n_fish_population=n_fish_population/1000
/*
graph box n_fish_population , over(length) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
 
*replace n_fish_population=n_fish_population*1000
 
***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length


tempfile new1
save `new1', replace

global selec
forv i=1/100{
u `new1', clear 
keep if draw==`i'
merge 1:m length using "fluke_rec_selectivity_2022_3reg.dta"
gen catch_at_length24=rec_selec*n_fish_population
sort reg len

	
tempfile selec`i'`r'
save `selec`i'`r'', replace
global selec "$selec "`selec`i'`r''" " 
	
}
clear
dsconcat $selec

drop if region==""
drop if catch_at_length24==.

/*
keep if region=="NO"
sort iter len  
replace n_fish_population=n_fish_population/1000

line n_fish_population length if length != 0,  lc(gs8)  c(L) lwidth(vthin) $graphoptions ylab(, angle(horizontal)) xlab(6(2)32, grid) ytitle("Numbers-at-length (000's of fish)") xtitle("Length (inches)", yoffset(-2))
*/


egen sum_catch=sum(catch_at_length24), by(reg draw)
gen fitted_prob=catch_at_length24/sum_catch

*line fitted_prob length if length != 0  & reg=="NJ",  lc(gs8)  c(L) lwidth(vthin) $graphoptions ylab(, angle(horizontal)) xlab(6(2)32, grid) ytitle("Prob(C{sub:l}, length=l)") xtitle("Length (inches)", yoffset(-2))


*Now keep the fitted_prob from 2024 (draws 1-100) and append the 2022 distirbution (draw = 0)
keep length fitted_prob region draw

preserve
u  "fluke_fitted_sizes_adjusted_3regs.dta", clear  
keep length fitted_prob region
drop if fitted_prob==0
gen draw=0
tempfile CaL22
save `CaL22', replace 
restore

append using `CaL22'
sort draw region length
order region draw length fit

expand 4 if region=="NO", gen(dup)
sort region draw length dup

bysort region draw length: gen tab=_n
replace region="MA" if region=="NO" & tab==1
replace region="RI" if region=="NO" & tab==2
replace region="CT" if region=="NO" & tab==3
replace region="NY" if region=="NO" & tab==4
drop dup tab

expand 4 if region=="SO", gen(dup)
sort region draw length dup

bysort region draw length: gen tab=_n
replace region="DE" if region=="SO" & tab==1
replace region="MD" if region=="SO" & tab==2
replace region="VA" if region=="SO" & tab==3
replace region="NC" if region=="SO" & tab==4
drop dup tab

rename region state
sort draw state length

cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
save "fluke_projected_catch_at_lengths.dta", replace 

export delimited using "fluke_projected_catch_at_lengths.csv", replace 

import delimited using "fluke_projected_catch_at_lengths.csv", clear  

line fitted_prob length if length != 0  & state=="NJ" & draw>0 & draw<6,  lc(gs8)  c(L) lwidth(vthin) $graphoptions ylab(, angle(horizontal)) xlab(6(2)32, grid) ytitle("Prob(C{sub:l}, length=l)") xtitle("Length (inches)", yoffset(-2)) note("Probability at or above 17 inches for the five draws: 15.4%, 16.7%, 15.7%, 16.8%, 17.4%", size(small) yoffset(-3))

forv i =1/6{
	su fitted_prob if length>=17 &  state=="NJ" & draw==`i'
	di "sum above= " `r(sum)'
	
}

******************************************************
******************************************************
*End fluke catch-at-length distribution 
******************************************************
******************************************************






global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)


******************************************************
******************************************************
*Begin black sea bass catch-at-length distribution 
******************************************************
******************************************************


set matsize 10000
/* Use these to control the years for which the MRIP data is polled/queried*/


global yearlist   2019 2020 2021 2022 
global wavelist 1 2 3 4 5 6

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global catchlist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}



/*SIZE_LIST */
global sizelist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}


***ALS 2021
**no mode-specific numbers
cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
import excel using "ALS_BSB_2021.xlsx", clear first 
renvarlab, lower

split date, pars("/")
destring date3, replace
tab date3, missing
replace date3=2022 if date3==6202
keep if inlist(date3, 2021)
rename date3 year

tempfile als2021
save `als2021', replace

**Updated ALS release data from Tony pre-2021
import excel using "ALS_bsb_ALL.xlsx", clear first 
renvarlab, lower

keep if inlist(year, 2020, 2019)

append using `als2021', force
drop date suffix
tempfile als_pre22
save `als_pre22', replace


***ALS 2022 - 2023
import excel using "ALS_BSB_22_23.xlsx", clear first 
renvarlab, lower
split date, pars("/")
destring date3, replace

rename date3 year

append using `als_pre22'
drop if year==2023

keep length release year placetagged tag zonedescription


split placetagged, pars(",")
replace placetagged2=ltrim(rtrim(placetagged2))
replace placetagged3=ltrim(rtrim(placetagged3))


gen state="MA" if placetagged2=="MA"
replace state="RI" if placetagged2=="RI"
replace state="CT" if placetagged2=="CT"
replace state="NY" if placetagged2=="NY"
replace state="NJ" if placetagged2=="NJ"
replace state="DE" if placetagged2=="DE"
replace state="MD" if placetagged2=="MD"
replace state="VA" if placetagged2=="VA"
replace state="NC" if placetagged2=="NC"

replace state="MA" if placetagged3=="MA" & state==""
replace state="RI" if placetagged3=="RI" & state==""
replace state="CT" if placetagged3=="CT" & state==""
replace state="NY" if placetagged3=="NY" & state==""
replace state="NJ" if placetagged3=="NJ" & state==""
replace state="DE" if placetagged3=="DE" & state==""
replace state="MD" if placetagged3=="MD" & state==""
replace state="VA" if placetagged3=="VA" & state==""
replace state="NC" if placetagged3=="NC" & state==""

*browse if state==""

drop if strmatch(placetagged, "* SC")==1

replace state= "NJ" if strmatch(placetagged, "* NJ")==1
replace state= "NY" if strmatch(placetagged, "* NY")==1
replace state= "NY" if strmatch(placetagged, "* NYC")==1
replace state= "DE" if strmatch(zonedescription, "* DE")==1
replace state= "MD" if strmatch(zonedescription, "* MD")==1
replace state= "NJ" if strmatch(placetagged, "* Nj")==1
replace state= "NY" if strmatch(placetagged, "Long Island Sound")==1
replace state= "NY" if strmatch(placetagged, "Cold Spring Harbor")==1
replace state= "NJ" if strmatch(placetagged, "Axel Carson Reef")==1
replace state= "CT" if strmatch(placetagged, "*CT")==1
replace state= "NJ" if strmatch(zonedescription, "NJ*")==1
replace state= "NY" if strmatch(placetagged, "*NY*")==1
replace state= "RI" if strmatch(zonedescription, "*Rhode Island*")==1
replace state= "NY" if strmatch(zonedescription, "*NY*")==1
replace state= "NJ" if strmatch(placetagged, "*South Amboy*")==1
replace state= "NJ" if strmatch(placetagged, "*S. Amboy*")==1
replace state= "NY" if strmatch(placetagged, "*Brooklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Bklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Rockaway*")==1
replace state= "NJ" if strmatch(placetagged, "*Atlantic Beach Reef*")==1
replace state= "NY" if strmatch(placetagged, "*Coney Island*")==1
replace state= "NY" if strmatch(zonedescription, "*Fire Island Inlet*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keansburg*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keyport*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Mattituck, LIS*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Middle Ground Light, LIS*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Navesink*")==1 & state==""
replace state= "CT" if strmatch(placetagged, "*Sandy Hook*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Perth Amboy*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Verizano Bridge*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Raritan Bay, Buoy 41*")==1 & state==""
replace state= "NY" if strmatch(zonedescription, "*Raritan Bay*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Mid L.I. Sound*")==1 & state==""


drop if state==""


browse
expand release


keep state year length
gen source="ALS"

tempfile als
save `als', replace 


*CT VAS 
*we do have mode-specific values, but not a lot 
import excel using "CT VAS Report_2013_2021_DRMolnar 01112022.xlsx", clear first 
renvarlab, lower
*keep if inlist(year, 2017, 2016)
*keep if inlist(speciescaught, "Flounder, Summer", "Scup","Bass, Black Sea") 
keep if inlist(speciescaught, "Bass, Black Sea") 

keep if disposition=="RELEASED"
keep quantity catchlength year 
expand quantity
drop quantity 
*gen nfish=1
rename catchlength length
drop if length=="-"
destring length, replace
gen state="CT"
keep if inlist(year, 2021, 2020, 2019)
gen source="CT_VAS"

tempfile ct_vas_pre22
save `ct_vas_pre22', replace 


import excel using "CT_VAS_2022.xlsx", clear first 
renvarlab, lower
*keep if inlist(year, 2017, 2016)
*keep if inlist(speciescaught, "Flounder, Summer", "Scup","Bass, Black Sea") 
keep if inlist(catch_common_name, "BASS, BLACK SEA") 
keep if disp=="RLSD"
expand quantity
drop quantity 
rename length_tlinches length
destring length, replace
keep if inlist(year, 2022)
gen source="CT_VAS"
keep year length state source
append using `ct_vas_pre22'

tempfile ct_vas
save `ct_vas', replace


*NJ VAS
*some shore values, but not many 
import excel using "NJ_VAS_BSB_thru2021.xlsx", clear first 
renvarlab, lower
rename fishyear year
keep if inlist(year, 2020, 2019)
keep if disp=="Rel"
keep year length 


tempfile nj_vas_pre22
save `nj_vas_pre22', replace

import excel using "NJ_VAS_BSB_2022.xlsx", clear first 
renvarlab, lower
rename fishyear year
keep if inlist(year, 2022,2021)
keep if disp=="Rel"
keep year length 

append using `nj_vas_pre22'

gen state="NJ"
gen source="NJ_VAS"

tempfile nj_vas
save `nj_vas', replace


*RI VAS
import excel using "RI_lengths_all_2022.xlsx", clear first 
renvarlab, lower
*keep if inlist(species, "Flounder, summer", "Scup","Bass, Black Sea") 
keep if inlist(common, "BASS, BLACK SEA") 
gen year=year(tripstartdate)


keep if inlist(year, 2022, 2021, 2020, 2019)
keep if inlist(disposition,"DEAD DISCARD", "RELEASED", "RELEASED ALIVE", "TOO SMALL")
*keep lengthin year reported

drop if reported=="-"
destring reported, replace
expand reported
drop reported
rename lengthin length
tab length, missing
drop if length==0
keep length state year 


gen source="RI_VAS"


tempfile ri_vas
save `ri_vas', replace



*MA VAS
*not a lot of shore 
import excel using "MA_VAS_SF-scup-BSB.xlsx", clear first 
gen year= year(sample_date) 
keep if inlist(year, 2022, 2021, 2020, 2019)

keep if common=="black sea bass"
keep if status=="R"

rename total_l length
keep length year
gen state="MA"

gen source="MA_VAS"

tempfile MA_vas
save `MA_vas', replace


*MRIP release data 
clear 
mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeb2

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1', replace
clear

 

dsconcat $b2list
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2019, 2020, 2021, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1', replace

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

*Here, group together NC and VA
*replace state="VA" if st==37


 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
 gen common_dom="zzzzzz"
 replace common_dom="SF" if strmatch(common, "blackseabass") 
 
 keep if common_dom=="SF"
 
 keep state l_in_bin year
 rename l_in_bin length 
 gen source="MRIP"



 
 

**********End editing individual state file
append using  `MA_vas'
append using `ri_vas'
append using `nj_vas'
append using `ct_vas'
append using `als'

tostring year, gen(yr2)
gen state_year=state+"_"+yr2

gen fish=1
preserve
collapse (sum) fish, by(state year source)
keep if year==2022
reshape wide fish, i( source year) j(state) string
restore



*twoway kdensity length if year==2019, lcol(blue) || kdensity length if year==2020, lcol(red) || kdensity length if year==2021, lcol(orange) ||  kdensity length if year==2022, lcol(green)







***Now combine the data 
**For now, start with combining the data by region: MA-NY, NJ, DE-MD
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

tabstat fish, stat(sum) by(region)

gen nfish=1
drop if length==0

replace length=round(length)

collapse (sum)  nfish, by(length year region)

rename len l_in_bin
drop if l_in==.

egen sumfish=sum(nfish), by(region year)
gen prop_b2=nfish/sumfish
sort reg l_in_bin

drop nfish sumfish


tempfile proportions_b2
save `proportions_b2', replace 


***Now pull keep lengths from MRIP 
clear

mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2019, 2020, 2021, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
gen common_dom="zzzzzz"
replace common_dom="SF" if strmatch(common, "blackseabass") 
 


tostring wave, gen(w2)
tostring year, gen(year2)

gen my_dom_id_string=region+"_"+year2+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "blackseabass")==0

drop if strmatch(common, "blackseabass")==0

gen measured=1 if !inlist(l_in_bin, ., 0) & lngth_imp==0
tabstat measured if year==2022, stat(sum) by(state)


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)

gen nobs=1
gen state_year=state+"_"+year2

	svy: tab l_in_bin my_dom_id_string, count
	/*save some stuff  
	matrix of proportions, row names, column names, estimate of total population size*/
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
rename eR l_in_bin
	
ds l, not
renvarlab `r(varlist)', prefix(i)
	
reshape long i, i(l_in) j(new) string
split new, parse(_)
rename new1 region
rename new2 year
destring year, replace 
drop new
rename i harvest


egen sumfish=sum(harvest), by(region year)
gen prop_ab1=harvest/sumfish


drop harvest sumfish
merge 1:1 l_in region year using  `proportions_b2'

drop _merge
mvencode prop*, mv(0) override

sort year region l_in

/*
preserve
keep if year==2022
encode region, gen(reg2)
xtset reg2 l_in
drop if l_in==.
tsfill, full
mvencode prop*, mv(0) override
twoway(scatter prop_ab1 l_in if region=="NO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="NO" & year==2022, connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "released fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("", size(medium)) xlab(2(2)34 ) ylab(, angle(horizontal)) ytitle("Proportion of total harvest or released"))
			 
twoway(scatter prop_ab1 l_in if region=="NJ" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="NJ" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "discarded fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("Raw proportions of fluke harvest and release at length, NJ 2022", size(medium)) xlab(4(2)38 ) ylab(, angle(horizontal)) ytitle("Proportion"))			 

twoway(scatter prop_ab1 l_in if region=="SO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="SO" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "discarded fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("Raw proportions of fluke harvest and release at length, DE-NC 2022", size(medium)) xlab(4(2)38 ) ylab(, angle(horizontal)) ytitle("Proportion"))					 
			 
restore
*/			 

tempfile proportions
save `proportions', replace 


*********estimate total harvest and release by region and year from MRIP 
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

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

*keep if mode1=="bt"


 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 


tostring wave, gen(wv2)
tostring year, gen(yr2)


gen my_dom_id_string=region+"_"+yr2+"_"+common_dom

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
encode my_dom_id_string, gen(my_dom_id)

local vars release landing tot_cat
foreach v of local vars{
	
	gen bsb_`v'=`v' if common=="blackseabass"


}


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

local vars bsb_release   bsb_landing   bsb_tot_cat  
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
drop if strmatch(my_dom_id, "*ZZ")==1
drop ll ul
reshape wide b se, i(my_dom_id) j( var) string
ds my se*, not
*renvarlab `r(varlist)', predrop(1)

split my, parse("_")
rename my_dom_id_string1 region
rename my_dom_id_string2 year
destring year, replace 
drop my_dom_id_string my_dom_id_string3

*rename sf_landing sf_harvest 
*rename sf_release sf_releases 

*order region   sf_landing sf_release sf_tot_cat

*keep region sf_release sf_harvest year
merge 1:m region  year using `proportions'


gen tot_keep= prop_ab1*bbsb_landing
gen tot_rel= prop_b2*bbsb_release
gen tot_cat=tot_keep+tot_rel
collapse (sum) tot_cat, by(l_ region year)




**now compute probabilities 
egen sum_tot=sum(tot_cat),  by(region year)
format tot_cat sum_tot %20.0gc
gen prob_catch=tot_cat/sum_tot

tostring year, gen(yr2)
gen reg_year=region+"_"+yr2
rename l_ length 

keep if year==2022 

preserve 
rename prob observed_prob
rename length fitted_length
keep fitted_length observed_prob region year reg_year tot_cat
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


/*
twoway(scatter prob_catch l_in if region=="NO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prob_catch l_in if region=="SO" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i))
scatter prop l_in if region=="DE_NC", connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) xlab(15)
scatter prop l_in if region=="ME_NY", connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) xlab(15)
*/

****estimate gamma parameters for each distirbution


tempfile new
save `new', replace
global fitted_sizes

levelsof reg_year , local(regs)
foreach r of local regs{
u `new', clear

keep if reg_year=="`r'"
keep length tot
replace tot=round(tot)
expand tot
drop if tot==0
gammafit length
local alpha=e(alpha)
local beta=e(beta)

gen gammafit=rgamma(`alpha', `beta')
replace gammafit=round(gammafit, .5)
drop if gammafit<1 | gammafit>24.5

gen nfish=1
collapse (sum) nfish, by(gammafit)
egen sumnfish=sum(nfish)
gen fitted_prob=nfish/sumnfish
gen reg_year="`r'"

tempfile fitted_sizes`r'
save `fitted_sizes`r'', replace
global fitted_sizes "$fitted_sizes "`fitted_sizes`r''" " 
}
clear
dsconcat $fitted_sizes
rename gammafit fitted_length		   

merge 1:1 fitted_length reg_year using `observed_prob'
sort reg_year fitted_length 
mvencode fitted_prob observed_prob, mv(0) override
split reg_year, parse(_)
drop region year 
rename reg_year1 region
rename reg_year2 year
destring year, replace

replace observed = . if observed ==0

egen sum_tot_catch=sum(tot_cat), by(reg_year)

cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
save "bsb_fitted_sizes_3regs.dta", replace 




u  "bsb_fitted_sizes_3regs.dta", clear 

keep if year==2022

preserve
keep reg_year sum_tot
duplicates drop 
tempfile tot_cat
save `tot_cat', replace 
restore 

collapse (sum) fitted_prob observed_prob, by(region year reg_year fitted_length)
merge m:1 reg_year using `tot_cat'

gen catch_at_length=fitted_prob*sum_tot_catch
drop _merge
rename fitted_length length
replace observed_prob=. if observed_prob==0

gen observed2 = observed/2

twoway(scatter observed2 length if region=="NO" & year==2022 & length<24,  xlab(2(2)24, grid labsize(vsmall)) connect(direct) lcol(gs12)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob length if region=="NO"  & year==2022 & length<24 , connect(direct) lcol(black)   lwidth(medthick)  lpat(dash) msymbol(i)   ///
		   xtitle("Length (inches)", yoffset(-2)) ytitle("Proportion of total catch")    ylab(, angle(horizontal) labsize(vsmall)) ///
		   legend(lab(1 "raw data") lab(2 "fitted (gamma) data") cols() yoffset(-2) region(color(none)))   title("") $graphoptions )


save "bsb_fitted_sizes_adjusted_3regs.dta", replace 


keep length fitted_prob region
drop if fitted_prob==0
gen draw=0

/*
preserve
u  "fluke_fitted_sizes_adjusted_3regs.dta", clear  
keep length fitted_prob region
drop if fitted_prob==0
gen draw=0
tempfile CaL22
save `CaL22', replace 
restore

append using `CaL22'
sort draw region length
order region draw length fit
*/
expand 4 if region=="NO", gen(dup)
sort region draw length dup

bysort region draw length: gen tab=_n
replace region="MA" if region=="NO" & tab==1
replace region="RI" if region=="NO" & tab==2
replace region="CT" if region=="NO" & tab==3
replace region="NY" if region=="NO" & tab==4
drop dup tab

expand 4 if region=="SO", gen(dup)
sort region draw length dup

bysort region draw length: gen tab=_n
replace region="DE" if region=="SO" & tab==1
replace region="MD" if region=="SO" & tab==2
replace region="VA" if region=="SO" & tab==3
replace region="NC" if region=="SO" & tab==4
drop dup tab

rename region state
sort draw state length

cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
save "bsb_projected_catch_at_lengths.dta", replace 
export delimited using "bsb_projected_catch_at_lengths.csv", replace 




/*
u  "bsb_fitted_sizes_adjusted_3regs.dta", clear 
ksmirnov fitted_prob if inlist(region, "NO", "NJ"), by(region) exact
ksmirnov fitted_prob if inlist(region, "NO", "SO"), by(region) exact
ksmirnov fitted_prob if inlist(region, "NJ", "SO"), by(region) exact
*/



/*
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

levelsof region, local(sts)
foreach s of local sts{
twoway(scatter observed_prob length if region=="`s'" & year==2022,  xlab(2(2)24, grid labsize(vsmall)) connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob length if region=="`s'"  & year==2022 , connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)   ///
		   xtitle("Length (inches)", yoffset(-2)) ytitle("Probability")    ylab(, angle(horizontal) labsize(vsmall)) ///
		   legend(lab(1 "Raw probability") lab(2 "Fitted probability") cols() yoffset(-2) region(color(none)))  xlab(2(2)24,  labsize(vsmall))  title("`s'") $graphoptions name("gr`s'", replace))
			
}
grc1leg grNO grSO grNJ, cols(3) ycommon

twoway(scatter fitted_prob length if region=="NO" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
			(scatter fitted_prob length if region=="NJ" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
			(scatter fitted_prob length if region=="SO" & year==2022,  xlab(4(2)36, grid labsize(vsmall)) connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions  ///
			 legend(lab(1 "NO") lab(2 "NJ") lab(3 "SO") cols(3)))
			 
*/			 
			 
**Now compute recreational selectivity 
**import the numbers at age, use age-length key to convert to inches, then merge back to catch-at-length 
*input age-length key used to age the recreational catch

import delimited using "NMFS_bottom_trawl_survey.txt", clear
tab stratum

tostring cruise, gen(cruise2)
gen year=substr(cruise2, 1, 4)
destring year, replace
gen count=1
collapse (sum) count, by(year svspp age length cruise2)

gen cruise3 = substr(cruise2, 5, 2)
destring cruise3, replace
tab cruise3

*keep only NEFSC trawl
drop if cruise3>10

*last five years of data 
keep if inlist(year, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013) 

/*
svspp codes:
summer flounder =103
bsb =141
scup =143
*/

keep if svspp==141

replace age=7 if age>=7

replace length=length/2.54
replace length=round(length, .5)
collapse (sum) count, by(svspp age length)



preserve
clear 
set obs 100
gen length=1 if _n==1
replace length=length[_n-1]+.5 if _n!=1
keep if length<=22.5
gen length_tab=_n
tempfile length_tab
save `length_tab', replace 
restore

merge m:1 length using `length_tab', nogen

collapse (sum) count, by(svspp age length_tab)

sort age length_tab
xtset age length_tab

tsfill, full
mvencode count, mv(0) override
egen sum_count=sum(count), by(age)
gen prop_age_a=count/sum_count
sort age length_tab


preserve 
keep length_tab prop_age_a age
tempfile rawdata
save `rawdata', replace 
restore

collapse (sum) count, by( age length_tab)
sort  age length_tab 

sort age length
tempfile new
save `new'

global sizes
levelsof age, local(ages)
foreach a of local ages{
u `new', clear
keep if age==`a'

*lowess smoother
sort length

lowess count length, bwidth(.3) gen(prop_age_a_smoothed) nograph adjust 
replace prop_age_a_smoothed=0 if prop_age_a_smoothed<0
replace age=`a' if age==.

*collapse (sum) new_prop, by(age length)

su prop_age_a_smoothed
replace prop_age_a_smoothed=prop_age_a_smoothed/`r(sum)'

tempfile sizes`a'
save `sizes`a'', replace
global sizes "$sizes "`sizes`a''" " 
}
clear
dsconcat $sizes

merge 1:1 age length using `rawdata', nogen
merge m:1 length using `length_tab', nogen


mvencode prop_age_a, mv(0) override
sort age length 

*Plots showing the raw versus smoothed data 
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
/*
levelsof age, local(ages)
foreach a of local ages{
	twoway(scatter prop_age_a length  if age==`a', connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter prop_age_a_smoothed length if age==`a', connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions name("age`a'", replace ) ///
				ytitle("Prob. that age-a" "fish is length-y", size(small)) title("Age `a'", size(small)) xtitle("Length (inches)", size(small)) xlab(1(2)23,  labsize(vsmall)) ///
				ytick(, labsize(vsmall) angle(horizontal)) ylab(, angle(horizontal) labsize(vsmall) ) ///
				legend(lab(1 "raw data (NEFSC trawl, 2018 - 2022 combined)") lab(2 "lowess-smoothed data (bandwidth=0.3)") size(small)  cols(1)) )
}
grc1leg  age0 age1 age2 age3 age4 age5 age6 age7, ycommon xcommon graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) 
*/


save "bsb_age_length_conversion.dta", replace







**Here, pull in the Numbers-at-age data for the baseline year to compute baseline recreational selectivity. 
**Translate numbers-at-age to numbers-at-length using the (smoothed) conversion key above 
**We have one distribution of catch-at-length and have 1,000 draws of numbers-at-length.
**Compute rec. selectivity (q) for every draw of numbers-at-length and take the median value. 


cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
import excel using "Fluke_N_at_age_2022.xlsx", clear first 
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace
drop if iter==.

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )
*/

/*
graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 
replace nfish=nfish*1000

/*randomly select 100 draws
bysort iter: gen rand=runiform() if _n==1
egen mean_rand=mean(rand), by(iter)
sort mean_rand age
egen group =group(mean_rand)
*/
 
tempfile new
save `new', replace

global sizes
forv i=1/1000{
u `new', clear 
keep if iter==`i'
merge 1:m age using "fluke_age_length_conversion.dta"

sort age length
gen n_fish_population=nfish*prop_age_a_smoothed

tempfile sizes`i'
save `sizes`i'', replace
global sizes "$sizes "`sizes`i''" " 
}
clear
dsconcat $sizes

replace n_fish_population=n_fish_population/1000

/*
graph box n_fish_population , over(length, label(angle(45) labsize(vsmall))) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
 
replace n_fish_population=n_fish_population*1000
 
***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length


tempfile new
save `new', replace

global selec
forv i=1/1000{
u `new', clear 
keep if iter==`i'

collapse (sum) n_fish, by(length iter)

tempfile new2
save `new2'

u "fluke_fitted_sizes_adjusted_3regs.dta", clear 
levelsof reg_year, local(regs)

	foreach r of local regs{
	u "fluke_fitted_sizes_adjusted_3regs.dta", clear 
	keep if reg_year=="`r'"
	
	merge 1:1 length using `new2'
	gen rec_selec=catch_at_length/n_fish
	
	tempfile selec`i'`r'
	save `selec`i'`r'', replace
	global selec "$selec "`selec`i'`r''" " 
	
	}
}
clear
dsconcat $selec
	
	

graph box rec_selec if region=="NO" , over(length, label(angle(45) labsize(vsmall))) $graphoptions title("Distribution of 2022 rec. selectivity, MA-NY" "(catch-at-length/pop. numbers-at-length)", size(medium)) ytitle("rec. selectivity", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 
graph box rec_selec if region=="SO" , over(length, label(angle(45) labsize(tiny))) $graphoptions title("Distribution of 2022 rec. selectivity, NJ-NC" "(catch-at-length/pop. numbers-at-length)", size(medium)) ytitle("rec. selectivity", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides
 

*now take the median value of rec. selectivity by length  
collapse (median) rec_selec n_fish_population  catch_at_length, by(length region)
sort region length

*scale all the values so max is one and put them on the same graph 
su rec_selec if region=="NO"
gen normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="NO"

su n_fish_population if region=="NO"
gen normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="NO"
 
su catch_at_length if region=="NO"
gen normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="NO"
 
su rec_selec if region=="SO"
replace normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="SO"

su n_fish_population if region=="SO"
replace normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="SO"
 
su catch_at_length if region=="SO"
replace normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="SO" 


su rec_selec if region=="NJ"
replace normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="NJ"

su n_fish_population if region=="NJ"
replace normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="NJ"
 
su catch_at_length if region=="NJ"
replace normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="NJ" 


 
twoway 	(scatter normal_rec_selec length if region=="NO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="NO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="NO", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)
twoway 	(scatter normal_rec_selec length if region=="SO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="SO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="SO", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)
twoway 	(scatter normal_rec_selec length if region=="NJ", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="NJ", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="NJ", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)  
 

*save "fluke_rec_selectivity_2022.dta", replace  
save "fluke_rec_selectivity_2022_3reg.dta", replace  

 twoway 	(scatter catch_at_length length if region=="NO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter catch_at_length length  if region=="SO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
				  legend(lab(1 "North (MA-NY)") lab(2 "South (NJ-NC)")) xlab(, labsize(vsmall) angle(horizontal)) ///
				  ylab(, labsize(vsmall) angle(horizontal)) ytitle("Recreational catch", size(small) xoffset(-1)) xtitle("Length (inches)")  )



u "fluke_rec_selectivity_2022_3reg.dta", clear 


******************************************************
******************************************************
*End black sea bass catch-at-length distribution 
******************************************************
******************************************************




******************************************************
******************************************************
*Begin scup catch-at-length distribution 
******************************************************
******************************************************


set matsize 10000
/* Use these to control the years for which the MRIP data is polled/queried*/


global yearlist   2019 2020 2021 2022 
global wavelist 1 2 3 4 5 6

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global catchlist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}



/*SIZE_LIST */
global sizelist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}


***ALS 2021
**no mode-specific numbers
cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
import excel using "ALS_SCUP_22_23.xlsx", clear first 
renvarlab, lower

split date, pars("/")
destring date3, replace
tab date3, missing
keep if inlist(date3, 2022)
rename date3 year

keep length release year placetagged tag zonedescription


split placetagged, pars(",")
replace placetagged2=ltrim(rtrim(placetagged2))


gen state="MA" if placetagged2=="MA"
replace state="RI" if placetagged2=="RI"
replace state="CT" if placetagged2=="CT"
replace state="NY" if placetagged2=="NY"
replace state="NJ" if placetagged2=="NJ"
replace state="DE" if placetagged2=="DE"
replace state="MD" if placetagged2=="MD"
replace state="VA" if placetagged2=="VA"
replace state="NC" if placetagged2=="NC"


*browse if state==""

drop if strmatch(placetagged, "* SC")==1

drop if state==""


*browse
expand release


keep state year length
gen source="ALS"

tempfile als
save `als', replace 


*CT VAS 
*we do have mode-specific values, but not a lot 
import excel using "CT VAS Report_2013_2021_DRMolnar 01112022.xlsx", clear first 
renvarlab, lower
*keep if inlist(year, 2017, 2016)
*keep if inlist(speciescaught, "Flounder, Summer", "Scup","Bass, Black Sea") 
keep if inlist(speciescaught, "Scup") 

keep if disposition=="RELEASED"
keep quantity catchlength year 
expand quantity
drop quantity 
*gen nfish=1
rename catchlength length
drop if length=="-"
destring length, replace
gen state="CT"
keep if inlist(year, 2021, 2020, 2019)
gen source="CT_VAS"

tempfile ct_vas_pre22
save `ct_vas_pre22', replace 


import excel using "CT_VAS_2022.xlsx", clear first 
renvarlab, lower
*keep if inlist(year, 2017, 2016)
*keep if inlist(speciescaught, "Flounder, Summer", "Scup","Bass, Black Sea") 
keep if inlist(catch_common_name, "SCUP") 
keep if disp=="RLSD"
expand quantity
drop quantity 
rename length_tlinches length
destring length, replace
keep if inlist(year, 2022)
gen source="CT_VAS"
keep year length state source
append using `ct_vas_pre22'

tempfile ct_vas
save `ct_vas', replace


*NJ VAS
*some shore values, but not many 
import excel using "NJ_VAS_SCUP_thru2021.xlsx", clear first 
renvarlab, lower
rename fishyear year
keep if inlist(year, 2020, 2019)
keep if disp=="Rel"
keep year length 

/*
tempfile nj_vas_pre22
save `nj_vas_pre22', replace

import excel using "NJ_VAS_BSB_2022.xlsx", clear first 
renvarlab, lower
rename fishyear year
keep if inlist(year, 2022,2021)
keep if disp=="Rel"
keep year length 

append using `nj_vas_pre22'
*/

**NO NJ VAS survey for scup 2022
gen state="NJ"
gen source="NJ_VAS"

tempfile nj_vas
save `nj_vas', replace


*RI VAS
import excel using "RI_lengths_all_2022.xlsx", clear first 
renvarlab, lower
*keep if inlist(species, "Flounder, summer", "Scup","Bass, Black Sea") 
keep if inlist(common, "SCUP") 
gen year=year(tripstartdate)


keep if inlist(year, 2022, 2021, 2020, 2019)
keep if inlist(disposition,"DEAD DISCARD", "RELEASED", "RELEASED ALIVE", "TOO SMALL")
*keep lengthin year reported

drop if reported=="-"
destring reported, replace
expand reported
drop reported
rename lengthin length
tab length, missing
drop if length==0
keep length state year 


gen source="RI_VAS"


tempfile ri_vas
save `ri_vas', replace



*MA VAS
*not a lot of shore 
import excel using "MA_VAS_SF-scup-BSB.xlsx", clear first 
gen year= year(sample_date) 
keep if inlist(year, 2022, 2021, 2020, 2019)

keep if common=="scup"
keep if status=="R"

rename total_l length
keep length year
gen state="MA"

gen source="MA_VAS"

tempfile MA_vas
save `MA_vas', replace


*MRIP release data 
clear 
mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeb2

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1', replace
clear

 

dsconcat $b2list
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2019, 2020, 2021, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1', replace

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

*Here, group together NC and VA
*replace state="VA" if st==37


 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
 gen common_dom="zzzzzz"
 replace common_dom="SF" if strmatch(common, "scup") 
 
 keep if common_dom=="SF"
 
 keep state l_in_bin year
 rename l_in_bin length 
 gen source="MRIP"



 
 

**********End editing individual state file
append using  `MA_vas'
append using `ri_vas'
append using `nj_vas'
append using `ct_vas'
append using `als'

tostring year, gen(yr2)
gen state_year=state+"_"+yr2

gen fish=1
preserve
collapse (sum) fish, by(state year source)
keep if year==2022
reshape wide fish, i( source year) j(state) string
restore



*twoway kdensity length if year==2019, lcol(blue) || kdensity length if year==2020, lcol(red) || kdensity length if year==2021, lcol(orange) ||  kdensity length if year==2022, lcol(green)







***Now combine the data 
*Because there are so few scup releases from NJ-NC, will combine them for one southern region
**For now, start with combining the data by region: MA-NY, NJ-MD
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

tabstat fish, stat(sum) by(region)

gen nfish=1
drop if length==0

replace length=round(length)

collapse (sum)  nfish, by(length year region)
rename len l_in_bin
drop if l_in==.

egen sumfish=sum(nfish), by(region year)
gen prop_b2=nfish/sumfish
sort reg l_in_bin


drop nfish sumfish

tempfile proportions_b2
save `proportions_b2', replace 


***Now pull keep lengths from MRIP 
clear

mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2019, 2020, 2021, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="NJ" if inlist(state, )
replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
gen common_dom="zzzzzz"
replace common_dom="SF" if strmatch(common, "scup") 
 


tostring wave, gen(w2)
tostring year, gen(year2)

gen my_dom_id_string=region+"_"+year2+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "scup")==0

drop if strmatch(common, "scup")==0

gen measured=1 if !inlist(l_in_bin, ., 0) & lngth_imp==0
tabstat measured if year==2022, stat(sum) by(state)

sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)

gen nobs=1
gen state_year=state+"_"+year2

	svy: tab l_in_bin my_dom_id_string, count
	/*save some stuff  
	matrix of proportions, row names, column names, estimate of total population size*/
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
rename eR l_in_bin
	
ds l, not
renvarlab `r(varlist)', prefix(i)
	
reshape long i, i(l_in) j(new) string
split new, parse(_)
rename new1 region
rename new2 year
destring year, replace 
drop new
rename i harvest


egen sumfish=sum(harvest), by(region year)
gen prop_ab1=harvest/sumfish


drop harvest sumfish
merge 1:1 l_in region year using  `proportions_b2'

drop _merge
mvencode prop*, mv(0) override

sort year region l_in


preserve
keep if year==2022
encode region, gen(reg2)
xtset reg2 l_in
drop if l_in==.
tsfill, full
mvencode prop*, mv(0) override
twoway(scatter prop_ab1 l_in if region=="NO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="NO" & year==2022, connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "released fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("", size(medium)) xlab(4(2)20 ) ylab(, angle(horizontal)) ytitle("Proportion of total harvest or release"))
			 
twoway(scatter prop_ab1 l_in if region=="NJ" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="NJ" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "discarded fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("Raw proportions of fluke harvest and release at length, NJ 2022", size(medium)) xlab(4(2)38 ) ylab(, angle(horizontal)) ytitle("Proportion"))			 

twoway(scatter prop_ab1 l_in if region=="SO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_b2 l_in if region=="SO" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
			 legend(lab(1 "harvested fish") lab(2 "discarded fish") cols(2) yoffset(-2) region(color(none))) xtitle("Length (inches)", yoffset(-2)) title("Raw proportions of fluke harvest and release at length, DE-NC 2022", size(medium)) xlab(4(2)38 ) ylab(, angle(horizontal)) ytitle("Proportion"))					 
			 
restore
*/			 

tempfile proportions
save `proportions', replace 


*********estimate total harvest and release by region and year from MRIP 
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

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

*keep if mode1=="bt"


 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "scup") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 


tostring wave, gen(wv2)
tostring year, gen(yr2)


gen my_dom_id_string=region+"_"+yr2+"_"+common_dom

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
encode my_dom_id_string, gen(my_dom_id)

local vars release landing tot_cat
foreach v of local vars{
	
	gen scup_`v'=`v' if common=="scup"


}


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

local vars scup_release   scup_landing   scup_tot_cat  
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
drop if strmatch(my_dom_id, "*ZZ")==1
drop ll ul
reshape wide b se, i(my_dom_id) j( var) string
ds my se*, not
*renvarlab `r(varlist)', predrop(1)

split my, parse("_")
rename my_dom_id_string1 region
rename my_dom_id_string2 year
destring year, replace 
drop my_dom_id_string my_dom_id_string3

*rename sf_landing sf_harvest 
*rename sf_release sf_releases 

*order region   sf_landing sf_release sf_tot_cat

*keep region sf_release sf_harvest year
merge 1:m region  year using `proportions'


gen tot_keep= prop_ab1*bscup_landing
gen tot_rel= prop_b2*bscup_release
gen tot_cat=tot_keep+tot_rel
collapse (sum) tot_cat, by(l_ region year)




**now compute probabilities 
egen sum_tot=sum(tot_cat),  by(region year)
format tot_cat sum_tot %20.0gc
gen prob_catch=tot_cat/sum_tot

tostring year, gen(yr2)
gen reg_year=region+"_"+yr2
rename l_ length 

keep if year==2022 

preserve 
rename prob observed_prob
rename length fitted_length
keep fitted_length observed_prob region year reg_year tot_cat
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


/*
twoway(scatter prob_catch l_in if region=="NO" & year==2022, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prob_catch l_in if region=="SO" & year==2022, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i))
scatter prop l_in if region=="DE_NC", connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) xlab(15)
scatter prop l_in if region=="ME_NY", connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) xlab(15)
*/

****estimate gamma parameters for each distirbution

*note: it is helpful within the next loop to restrict the range of fitted values to within a desired range. 
*For example, might want to restrict to the min/max of observed catch
*For 2022 , the range of observed catch is 4 to 19

tempfile new
save `new', replace
global fitted_sizes

levelsof reg_year , local(regs)
foreach r of local regs{
u `new', clear

keep if reg_year=="`r'"
keep length tot
replace tot=round(tot)
expand tot
drop if tot==0
gammafit length
local alpha=e(alpha)
local beta=e(beta)

gen gammafit=rgamma(`alpha', `beta')
replace gammafit=round(gammafit, .5)
gen nfish=1

*restrict catch to within range of observed values
keep if gammafit>=4 & gammafit<=19

collapse (sum) nfish, by(gammafit)
egen sumnfish=sum(nfish)
gen fitted_prob=nfish/sumnfish
gen reg_year="`r'"

tempfile fitted_sizes`r'
save `fitted_sizes`r'', replace
global fitted_sizes "$fitted_sizes "`fitted_sizes`r''" " 
}
clear
dsconcat $fitted_sizes
rename gammafit fitted_length		   

merge 1:1 fitted_length reg_year using `observed_prob'
sort reg_year fitted_length 
mvencode fitted_prob observed_prob, mv(0) override
split reg_year, parse(_)
drop region year 
rename reg_year1 region
rename reg_year2 year
destring year, replace

replace observed = . if observed ==0

egen sum_tot_catch=sum(tot_cat), by(reg_year)




cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
*save "fluke_fitted_sizes.dta", replace 
save "scup_fitted_sizes_2regs.dta", replace 






*u  "fluke_fitted_sizes.dta", clear 
u  "scup_fitted_sizes_2regs.dta", clear 

keep if year==2022
preserve
keep reg_year sum_tot
duplicates drop 
tempfile tot_cat
save `tot_cat', replace 
restore 

drop if fitted_prob==0
collapse (sum) fitted_prob observed_prob, by(region year reg_year fitted_length)
merge m:1 reg_year using `tot_cat'

gen catch_at_length=fitted_prob*sum_tot_catch
drop _merge
rename fitted_length length
replace observed_prob=. if observed_prob==0


gen observed2 = observed/2

twoway(scatter observed2 length if region=="NO" & year==2022 & length<20,  xlab(2(2)20, grid labsize(vsmall)) connect(direct) lcol(gs12)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob length if region=="NO"  & year==2022 & length<20 , connect(direct) lcol(black)   lwidth(medthick)  lpat(dash) msymbol(i)   ///
		   xtitle("Length (inches)", yoffset(-2)) ytitle("Proportion of total catch")    ylab(, angle(horizontal) labsize(vsmall)) ///
		   legend(lab(1 "raw data") lab(2 "fitted (gamma) data") cols() yoffset(-2) region(color(none)))   title("") $graphoptions )


save "scup_fitted_sizes_adjusted_2regs.dta", replace 

*test to see if distributions for north and south are significantly different from each other 
ksmirnov fitted_prob, by(region) exact

u  "scup_fitted_sizes_adjusted_2regs.dta", clear 


global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

levelsof region, local(sts)
foreach s of local sts{
twoway(scatter observed_prob length if region=="`s'" & year==2022,  xlab(2(2)24, grid labsize(vsmall)) connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob length if region=="`s'"  & year==2022 , connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)   ///
		   xtitle("Length (inches)", yoffset(-2)) ytitle("Probability")    ylab(, angle(horizontal) labsize(vsmall)) ///
		   legend(lab(1 "Raw probability") lab(2 "Fitted probability") cols() yoffset(-2) region(color(none)))  xlab(2(2)24,  labsize(vsmall))  title("`s'") $graphoptions name("gr`s'", replace))
			
}
grc1leg grNO grSO, cols(3) ycommon

twoway(scatter fitted_prob length if region=="NO" & year==2022,  xlab(2(2)24, grid labsize(vsmall)) connect(direct) lcol(black)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
			(scatter fitted_prob length if region=="SO" & year==2022,  xlab(2(2)24, grid labsize(vsmall)) connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions  ///
			 legend(lab(1 "NO") lab(2 "SO") cols(2)))
			 
*/			 
			 
**Now compute recreational selectivity 
**import the numbers at age, use age-length key to convert to inches, then merge back to catch-at-length 
*input age-length key used to age the recreational catch
cd "Z:\size data 2023 RDM"
import delimited using "NMFS_bottom_trawl_survey.txt", clear
tab stratum

tostring cruise, gen(cruise2)
gen year=substr(cruise2, 1, 4)
destring year, replace
gen count=1
collapse (sum) count, by(year svspp age length cruise2)

gen cruise3 = substr(cruise2, 5, 2)
destring cruise3, replace
tab cruise3

*keep only NEFSC trawl
drop if cruise3>10

*last five years of data 
keep if inlist(year, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013) 

/*
svspp codes:
summer flounder =103
bsb =141
scup =143
*/

keep if svspp==143

replace age=7 if age>=7

replace length=length/2.54
replace length=round(length, .5)
collapse (sum) count, by(svspp age length)



*Need to:
	*1) fill in the gaps of missing lengths over the distribution . Here, missing only length 16.5. 
	*Since there was only one measured fish of 16 and 17, and both were age 7, we will create a new length bin for 16.5 and 
	*assume one fish caught of age 7
	*2) extend the age-length key to include catch-lengths outside the range of measured lengths
	*Here, the longest fish measured for age was 17.5 and the longest observed catch-at-length was 19. Since the last two 
	*length categories of measured fish were 17 and 17.5, and there was only one fish in these bins that were that length, 
	*we will create new length categories for 18, 18.5, and 19 and assume they are age 7+
preserve
clear 
set obs 4
gen length=16.5 if _n==1
replace length=18 if _n==2
replace length=18.5 if _n==3
replace length=19 if _n==4

gen age=7
gen count=1
tempfile missing_ages
save `missing_ages'
restore

append using `missing_ages'

drop svspp

preserve
clear 
set obs 100
gen length=1 if _n==1
replace length=length[_n-1]+.5 if _n!=1
keep if length<=19
gen length_tab=_n
tempfile length_tab
save `length_tab', replace 
restore

merge m:1 length using `length_tab', nogen

collapse (sum) count, by (age length_tab)

sort age length_tab
xtset age length_tab

tsfill, full
mvencode count, mv(0) override
egen sum_count=sum(count), by(age)
gen prop_age_a=count/sum_count
sort age length_tab


preserve 
keep length_tab prop_age_a age
tempfile rawdata
save `rawdata', replace 
restore

collapse (sum) count, by( age length_tab)
sort  age length_tab 

sort age length
tempfile new
save `new'

global sizes
levelsof age, local(ages)
foreach a of local ages{
u `new', clear
keep if age==`a'

*lowess smoother
sort length

lowess count length, bwidth(.3) gen(prop_age_a_smoothed) nograph adjust 
replace prop_age_a_smoothed=0 if prop_age_a_smoothed<0
replace age=`a' if age==.

*collapse (sum) new_prop, by(age length)

su prop_age_a_smoothed
replace prop_age_a_smoothed=prop_age_a_smoothed/`r(sum)'

tempfile sizes`a'
save `sizes`a'', replace
global sizes "$sizes "`sizes`a''" " 
}
clear
dsconcat $sizes

merge 1:1 age length using `rawdata', nogen
merge m:1 length using `length_tab', nogen


mvencode prop_age_a, mv(0) override
sort age length 

*Plots showing the raw versus smoothed data 
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

levelsof age, local(ages)
foreach a of local ages{
	twoway(scatter prop_age_a length  if age==`a', connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter prop_age_a_smoothed length if age==`a', connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions name("age`a'", replace ) ///
				ytitle("Prob. that age-a" "fish is length-y", size(small)) title("Age `a'", size(small)) xtitle("Length (inches)", size(small)) xlab(1(2)23,  labsize(vsmall)) ///
				ytick(, labsize(vsmall) angle(horizontal)) ylab(, angle(horizontal) labsize(vsmall) ) ///
				legend(lab(1 "raw data (NEFSC trawl, 2018 - 2022 combined)") lab(2 "lowess-smoothed data (bandwidth=0.3)") size(small)  cols(1)) )
}
grc1leg  age0 age1 age2 age3 age4 age5 age6 age7, ycommon xcommon graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) 
*/


*Plots showing the raw versus smoothed data 
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

levelsof age, local(ages)
foreach a of local ages{
	twoway(scatter prop_age_a length  if age==`a', connect(direct) lcol(black)  lwidth(medium)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter prop_age_a_smoothed length if age==`a', connect(direct) lcol(red)  lwidth(medium)  lpat(solid) msymbol(i) $graphoptions name("age`a'", replace ) ///
				ytitle("", size(small)) title("Age `a'", size(small)) xtitle("", size(small)) xlab(1(2)23, grid labsize(vsmall)) ///
				ytick(, labsize(vsmall) angle(horizontal)) ylab(, angle(horizontal) labsize(vsmall) )  graphregion(margin(small)) ///
				legend(lab(1 "raw data (NEFSC trawl, 2013 - 2022 combined)") lab(2 "lowess-smoothed data (bandwidth=0.3)") size(vsmall)  cols(1) region(color(none))) )
}
grc1leg  age0 age1 age2 age3 age4 age5 age6 age7, cols(2) ycommon xcommon graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) 



save "scup_age_length_conversion.dta", replace







**Here, pull in the Numbers-at-age data for the baseline year to compute baseline recreational selectivity. 
**Translate numbers-at-age to numbers-at-length using the (smoothed) conversion key above 
**We have one distribution of catch-at-length and have 1,000 draws of numbers-at-length.
**Compute rec. selectivity (q) for every draw of numbers-at-length and take the median value. 


cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
import excel using "Scup_N_at_age_2022.xlsx", clear first 
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace
drop if iter==.

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )
*/


graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides  name("scup n_age", replace ) 
 */
 
replace nfish=nfish*1000

/*randomly select 100 draws
bysort iter: gen rand=runiform() if _n==1
egen mean_rand=mean(rand), by(iter)
sort mean_rand age
egen group =group(mean_rand)
*/
 
tempfile new
save `new', replace

global sizes
forv i=1/1000{
u `new', clear 
keep if iter==`i'
merge 1:m age using "scup_age_length_conversion.dta"

sort age length
gen n_fish_population=nfish*prop_age_a_smoothed

tempfile sizes`i'
save `sizes`i'', replace
global sizes "$sizes "`sizes`i''" " 
}
clear
dsconcat $sizes

replace n_fish_population=n_fish_population/1000

/*
graph box n_fish_population , over(length, label(angle(45) labsize(vsmall))) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
 
replace n_fish_population=n_fish_population*1000
 
***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length


tempfile new
save `new', replace

global selec
forv i=1/1000{
u `new', clear 
keep if iter==`i'

collapse (sum) n_fish, by(length iter)

tempfile new2
save `new2'

u "scup_fitted_sizes_adjusted_2regs.dta", clear 
levelsof reg_year, local(regs)

	foreach r of local regs{
	u "scup_fitted_sizes_adjusted_2regs.dta", clear 
	keep if reg_year=="`r'"
	
	merge 1:1 length using `new2'
	gen rec_selec=catch_at_length/n_fish
	
	tempfile selec`i'`r'
	save `selec`i'`r'', replace
	global selec "$selec "`selec`i'`r''" " 
	
	}
}
clear
dsconcat $selec
	
drop if rec_selec==.

graph box rec_selec if region=="NO" , over(length, label(angle(45) labsize(vsmall))) $graphoptions title("Distribution of 2022 rec. selectivity, MA-NY" "(catch-at-length/pop. numbers-at-length)", size(medium)) ytitle("rec. selectivity", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 
graph box rec_selec if region=="SO" , over(length, label(angle(45) labsize(tiny))) $graphoptions title("Distribution of 2022 rec. selectivity, NJ-NC" "(catch-at-length/pop. numbers-at-length)", size(medium)) ytitle("rec. selectivity", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides
 

*now take the median value of rec. selectivity by length  
collapse (median) rec_selec n_fish_population  catch_at_length, by(length region)
sort region length

*scale all the values so max is one and put them on the same graph 
su rec_selec if region=="NO"
gen normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="NO"

su n_fish_population if region=="NO"
gen normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="NO"
 
su catch_at_length if region=="NO"
gen normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="NO"
 
su rec_selec if region=="SO"
replace normal_rec_selec = (rec_selec - r(min)) / (r(max) - r(min)) if region=="SO"

su n_fish_population if region=="SO"
replace normal_n_fish_population = (n_fish_population - r(min)) / (r(max) - r(min)) if region=="SO"
 
su catch_at_length if region=="SO"
replace normal_catch_at_length = (catch_at_length - r(min)) / (r(max) - r(min)) if region=="SO" 



 
twoway 	(scatter normal_rec_selec length if region=="NO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="NO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="NO", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)
twoway 	(scatter normal_rec_selec length if region=="SO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="SO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="SO", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)
twoway 	(scatter normal_rec_selec length if region=="NJ", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_n_fish_population length  if region=="NJ", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter normal_catch_at_length length if region=="NJ", connect(direct) lcol(green)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions)  
 

save "scup_rec_selectivity_2022_2reg.dta", replace  

 twoway 	(scatter catch_at_length length if region=="NO", connect(direct) lcol(red)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter catch_at_length length  if region=="SO", connect(direct) lcol(blue)  lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ///
				  legend(lab(1 "North (MA-NY)") lab(2 "South (NJ-NC)")) xlab(, labsize(vsmall) angle(horizontal)) ///
				  ylab(, labsize(vsmall) angle(horizontal)) ytitle("Recreational catch", size(small) xoffset(-1)) xtitle("Length (inches)")  )



				  
*create prob(catch at length) distributions for 2024
cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
import excel using "Scup_N_at_age_2024.xlsx", clear first 
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace
drop if iter==.

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )
*/

/*
graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 fluke numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 

*randomly select 100 draws
bysort iter: gen rand=runiform() if _n==1
egen mean_rand=mean(rand), by(iter)
sort mean_rand age
egen draw =group(mean_rand)

keep if draw<=100
drop rand mean_rand iter

tempfile new
save `new', replace

global sizes
forv i=1/100{
u `new', clear 
keep if draw==`i'
merge 1:m age using "scup_age_length_conversion.dta"

sort age length
gen n_fish_population=nfish*prop_age_a_smoothed

tempfile sizes`i'
save `sizes`i'', replace
global sizes "$sizes "`sizes`i''" " 
}
clear
dsconcat $sizes
collapse (sum) n_fish_population, by(length draw)


*replace n_fish_population=n_fish_population/1000
/*
graph box n_fish_population , over(length) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
 
*replace n_fish_population=n_fish_population*1000
 
***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length


tempfile new1
save `new1', replace

global selec
forv i=1/100{
u `new1', clear 
keep if draw==`i'
merge 1:m length using "scup_rec_selectivity_2022_2reg.dta"
gen catch_at_length24=rec_selec*n_fish_population
sort reg len

	
tempfile selec`i'`r'
save `selec`i'`r'', replace
global selec "$selec "`selec`i'`r''" " 
	
}
clear
dsconcat $selec

drop if region==""
drop if catch_at_length24==.

/*
keep if region=="NO"
sort iter len  
replace n_fish_population=n_fish_population/1000

line n_fish_population length if length != 0,  lc(gs8)  c(L) lwidth(vthin) $graphoptions ylab(, angle(horizontal)) xlab(6(2)32, grid) ytitle("Numbers-at-length (000's of fish)") xtitle("Length (inches)", yoffset(-2))
*/


egen sum_catch=sum(catch_at_length24), by(reg draw)
gen fitted_prob=catch_at_length24/sum_catch

line fitted_prob length if length != 0  & reg=="SO",  lc(gs8)  c(L) lwidth(vthin) $graphoptions ylab(, angle(horizontal)) xlab(6(2)32, grid) ytitle("Prob(C{sub:l}, length=l)") xtitle("Length (inches)", yoffset(-2))


*Now keep the fitted_prob from 2024 (draws 1-100) and append the 2022 distirbution (draw = 0)
keep length fitted_prob region draw

preserve
u  "scup_fitted_sizes_adjusted_2regs.dta", clear  
keep length fitted_prob region
drop if fitted_prob==0
gen draw=0
tempfile CaL22
save `CaL22', replace 
restore

append using `CaL22'
sort draw region length
order region draw length fit

expand 4 if region=="NO", gen(dup)
sort region draw length dup

bysort region draw length: gen tab=_n
replace region="MA" if region=="NO" & tab==1
replace region="RI" if region=="NO" & tab==2
replace region="CT" if region=="NO" & tab==3
replace region="NY" if region=="NO" & tab==4
drop dup tab

expand 5 if region=="SO", gen(dup)
sort region draw length dup

bysort region draw length: gen tab=_n
replace region="DE" if region=="SO" & tab==1
replace region="MD" if region=="SO" & tab==2
replace region="VA" if region=="SO" & tab==3
replace region="NC" if region=="SO" & tab==4
replace region="NJ" if region=="SO" & tab==5
drop dup tab

rename region state
sort draw state length

cd "\\net.nefsc.noaa.gov\aharris\size data 2023 RDM"
save "scup_projected_catch_at_lengths.dta", replace 
export delimited using "scup_projected_catch_at_lengths.csv", replace 

				  
				  
				  
				  
				  
				  
				  
				  
				  
