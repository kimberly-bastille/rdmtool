

*Set the global length to pull either ionches or centimeters from MRIP
global length_bin l_cm_bin
*global length_bin l_in_bin

*MRIP release data 
cd $mrip_data_cd

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


*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_code_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge


/*classify into (O)ther, Gulf of (M)aine, or Georges (B)ank */
gen str3 area_s="O"

replace area_s="M" if st2=="23" | st2=="33"
replace area_s="M" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="B" if st2=="25" & strmatch(stock_region_calc,"SOUTH")


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


gen season="op" if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 )) 
replace season="cl" if season==""

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
replace season="closed" if season=="cl"
replace season="open" if season=="op"
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
drop new3
replace species="cod" if species=="c"
replace species="hadd" if species=="h"
replace season="closed" if season=="cl"
replace season="open" if season=="op"

drop new
rename tab nfish_b2	
drop if species=="cod"
append using `codb2'
sort  season species $length_bin


tempfile b2
save `b2', replace 


*************Now pull keep lengths from MRIP
cd $mrip_data_cd

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


*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_code_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge


/*classify into (O)ther, Gulf of (M)aine, or Georges (B)ank */
gen str3 area_s="O"

replace area_s="M" if st2=="23" | st2=="33"
replace area_s="M" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="B" if st2=="25" & strmatch(stock_region_calc,"SOUTH")


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


gen season="op" if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 )) 
replace season="cl" if season==""

gen my_dom_id_string=common_dom+"_"+season+"_"+area_s
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)


*preserve

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
drop new3
replace species="cod" if species=="c"
replace species="hadd" if species=="h"
replace season="closed" if season=="cl"
replace season="open" if season=="op"


rename tab nfish_ab1	
sort  season species $length_bin


merge 1:1 $length_bin species season using `b2'

sort species  season l


gen panel_var=species+"_"+season
encode panel_var, gen(panel_var2)
xtset panel_var2 $length_bin
tsfill, full
mvencode nfish*, mv(0) over
keep $length_bin nfish* species season
order species season  $length_bin nfish* 

egen sum_ab1=sum(nfish_ab1), by(species season ) 
egen sum_b2=sum(nfish_b2), by(species season ) 

gen prop_ab1=nfish_ab1/sum_ab1
gen prop_b2=nfish_b2/sum_b2


tempfile props
save `props', replace 



***Now need to estimate total keep/release by species month and multiply these proportions
cd $mrip_data_cd

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
 
keep if $calibration_year

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo

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


*classify into GOM or GBS
rename intsite SITE_ID
merge m:1 SITE_ID using "$input_code_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")


tostring wave, gen(wv2)
tostring year, gen(yr2)


*create a variable indicating if the observation came from a month where the season was open or closed
destring month, gen(month1)
gen day=substr(id_code, 12, 2)
destring day, gen(day1)
gen date=mdy( month1, day1, year)
format date %td


gen season="op" if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 )) 
replace season="cl" if season==""


gen my_dom_id_string=common_dom+"_"+area_s
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)


gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)

gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

drop cod_tot_cat cod_harvest cod_releases hadd_tot_cat hadd_harvest hadd_releases 
rename sum_cod_tot_cat cod_catch
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_hadd_tot_cat hadd_catch
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel


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

*replace my_dom_id_string=season+"_"+common_dom
encode my_dom_id_string, gen(my_dom_id)

encode strat_id, gen(strat_id2)
encode psu_id, gen(psu_id2)

*replace wp_int=round(wp_int)
*svy: total hadd_catch, over(my_dom_id2)
svyset psu_id2 [pweight= wp_int], strata(strat_id2) singleunit(certainty)

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
local vars hadd_catch hadd_keep hadd_rel cod_catch cod_keep cod_rel
foreach v of local vars{
u `base', clear 

gen open=1 if ((date>=$cod_start_date1 & date<=$cod_end_date1 ) | (date>=$cod_start_date2 & date<=$cod_end_date2 ))

preserve
svy: total `v' if  open==1, over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor

split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

gen var="`v'"
gen season="open"

keep my_dom_id_string var b se ll ul season

tempfile cod_open
save `cod_open'
restore 

svy: total `v' if open!=1, over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor

split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

gen var="`v'"
gen season="closed"

keep my_dom_id_string var b se ll ul season
append using `cod_open'

tempfile catchez`v'
save `catchez`v'', replace
global catchez "$catchez "`catchez`v''" " 

}

dsconcat $catchez


keep season  b var
split var, parse(_)
rename var1 species 
rename var2 disp
drop var
reshape wide b, i(season species) j(disp) string
drop bcatch
drop if bkeep ==0 & brel==0

merge 1:m season species using `props'
drop _merge 

replace nfish_ab1=prop_ab1*bkeep
replace nfish_b2=prop_b2*brel
mvencode nfish_ab1  nfish_b2, mv(0) override
gen nfish_catch=nfish_ab1+nfish_b2
order  species season $length_bin nfish_catch
collapse (sum) nfish_catch nfish_ab1 nfish_b2, by( species season $length_bin)
collapse (sum) nfish_catch  , by( species season $length_bin)

drop if species==""
gen panel_var=species+"_"+season
encode panel_var, gen(panel_var2)
xtset panel_var2 $length_bin
tsfill, full
decode panel_var2, gen(panel_var3)
split panel_var3, pars(_)
replace species=panel_var31
replace season=panel_var32
mvencode nfish_catch, mv(0) over

drop panel_var* 
 
rename l_ length 


*fit these counts to a gamma distribution 
egen sumfish=sum(nfish), by(season species)
gen observed_prob=nfish/sum
drop sumfish
gen domain = species+"_"+season

drop if nfish_catch==0

preserve 
rename length fitted_length
keep fitted_length observed_prob nfish_catch species season domain
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


/*
twoway(scatter observed_prob length if species=="cod" & season=="open", connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter observed_prob length if species=="cod" & season=="closed", connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter observed_prob length if species=="hadd" & season=="open", connect(direct) lcol(green)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter observed_prob length if species=="hadd" & season=="closed", connect(direct) lcol(orange)   lwidth(medthick)  lpat(solid) msymbol(i) ) 
*/

****estimate gamma parameters for each distirbution

*note: it is helpful within the next loop to restrict the range of fitted values to within a desired range. 
*For example, might want to restrict to the min/max of observed catch
*For 2022 , the range of observed catch is 4 to 19

tempfile new
save `new', replace
global fitted_sizes

levelsof domain , local(regs)
foreach r of local regs{
u `new', clear

keep if domain=="`r'"
keep length nfish_catch
su length if nfish_catch!=0
local minL=`r(min)'
local maxL=`r(max)'

su nfish_catch
if `r(sum)'<100000{
	egen sumfish=sum(nfish_catch)
	gen expand=100000/sumfish
	replace nfish_catch=nfish_catch*expand
	drop sumfish expand
}

else{
}

replace nfish_catch=round(nfish_catch)
expand nfish_catch
drop if nfish_catch==0
gammafit length
local alpha=e(alpha)
local beta=e(beta)

gen gammafit=rgamma(`alpha', `beta')
*replace gammafit=round(gammafit, .5)
replace gammafit=round(gammafit)

gen nfish=1

*restrict catch to within range of observed values
keep if gammafit>=`minL' & gammafit<=`maxL'

collapse (sum) nfish, by(gammafit)
egen sumnfish=sum(nfish)
gen fitted_prob=nfish/sumnfish
gen domain="`r'"

tempfile fitted_sizes`r'
save `fitted_sizes`r'', replace
global fitted_sizes "$fitted_sizes "`fitted_sizes`r''" " 
}
clear
dsconcat $fitted_sizes
rename gammafit fitted_length		   

merge 1:1 fitted_length domain using `observed_prob'
sort domain fitted_length 
mvencode fitted_prob observed_prob, mv(0) override 

*replace fitted_length=fitted_length+.5
split domain, parse(_)
replace species=domain1
replace season=domain2

drop if _merge==2
drop _merge 

egen sum_nfish_catch=sum(nfish_catch), by(species season)
replace observed_prob = nfish_catch/sum_nfish_catch

gen tab=1
egen sumtab=sum(tab), by(species season)
gen tab1=1 if observed_prob==.
egen sumtab1=sum(tab1), by(species season)
gen observed_prob2 = observed_prob/(sumtab/sumtab1)

encode domain, gen(domain3)

drop nfish sumnfish nfish_catch  domain1 domain2  tab sumtab tab1 sumtab1 domain3
rename fitted_l length

gen nfish_catch_from_fitted=fitted_prob*sum_nfish_catch
gen nfish_catch_from_raw=observed_prob*sum_nfish_catch

levelsof domain, local(domz)
foreach d of local domz{
twoway (scatter observed_prob length if domain=="`d'" ,   connect(direct) lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob length if  domain=="`d'"   , connect(direct) lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "raw data") lab(2 "fitted (gamma) data") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames'

drop observed_prob2

save "$age_pro_cd/rec_selectivity_CaL_open_seasons_cm.dta", replace  //This file has the fitted catch-at-length probabilities in the baseline year


*****Now obtain draws of population numbers at length from AGEPRO and translate these to numbers at length 
*1) pull raw trawl survey data and create age-length key. M-Y has not been smoothing these data 
		* use the last three years of data available. 
		* by the time we update the data (Nov. 15), there will only be spring trawl survey data from the most recent year 
		* for now I will use the datas M-Y has pulled, but will have to pull new data for final model estimation

		
*****cod 
* for cod, there is are few obs for age 7+
* combine these into 6+ category
**M-Y 2023 model:
	*Bottomtrawl survey data from 2021-2023 to form the age-length keys.

import excel using "$age_pro_cd/fall_spring_cruises_lou.xlsx", clear first
renvarlab, lower
tempfile cruises
save `cruises', replace 

import excel using "$age_pro_cd/cod_svspp_raw_lou.xlsx", clear first
renvarlab, lower
merge m:1 cruise6 using `cruises'
keep if _merge==3
drop if age==0
replace age=6 if age>=6
collapse (sum) count, by(year svspp age length)
destring year, replace
sort svspp year age length count

keep if year>=$trawl_svy_start_yr & year<=$trawl_svy_end_yr

*use "$age_pro_cd/cod_svspp_raw.dta", clear 
*keep if year>=2020

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
di `min_svy_yr'

replace age=6 if age>=6
collapse (sum) count, by (age length)
/*
tsset age length
tsfill, full
*/
/*
preserve
su length 
clear 
set obs 2
gen length=`r(min)' if _n==1
replace length=round(`r(max)') if _n==2
tsset length
tsfill, full
expand 2, gen(dup)
replace length=length+.5 if dup==1
sort length 
drop dup
expand 6
bysort length: gen age=_n
tempfile full_lengths
save `full_lengths', replace 
restore 

merge 1:1 length age using `full_lengths'
drop _merge 
*/

sort age length 
mvencode count, mv(0) override 


levelsof age, local(ages)
foreach a of local ages{
	su length if age==`a' & count!=0
	
	lowess count length if age==`a' & length>=`r(min)' & length<=`r(max)', adjust bwidth(.3) gen(s`a') nograph
	replace s`a'=0 if s`a'<=0
}
egen smoothed=rowtotal(s1-s6)
drop s1-s6




egen sum=sum(smoothed), by(age)	
gen prop_smoothed=smoothed/sum	

egen sum_raw=sum(count), by(age)	
gen prop_raw=count/sum_raw	

/*
levelsof age, local(ages)
foreach a of local ages{
twoway(scatter prop_raw length if age==`a', connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_smoothed length if age==`a', connect(direct) lcol(blue) title("cod age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr'", size(small)) ///
			ytitle(# fish, size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length inches, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames'
*/


drop sum sum_raw
tempfile al_cod
save `al_cod', replace 


*historical data to compute rec selectivity
use "$age_pro_cd/historical_and_mean_projected_Cod_NAA.dta", clear 
egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
keep if year==2023
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_cod', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length)

sort length 

gen species="cod"

preserve
use "$age_pro_cd/rec_selectivity_CaL_open_seasons_cm.dta", clear
keep if species=="cod"
tempfile cod
save `cod', replace 
restore 

merge 1:m length species using `cod'
drop if _merge==1
sort season species  length 

egen sum_fitted=sum(fitted), by(species season)
egen sum_obs=sum(observed), by(species season)

drop _merge
gen ql_raw=nfish_catch_from_raw/NaL_from_raw_trawl
gen ql_smooth=nfish_catch_from_fitted/NaL_from_smooth_trawl

keep length species observed fitted_prob  ql* season
order species season length   fitted_prob  ql*

tempfile cod_ql
save `cod_ql', replace


*****haddock 
import excel using "$age_pro_cd/fall_spring_cruises_lou.xlsx", clear first
renvarlab, lower
tempfile cruises
save `cruises', replace 

import excel using "$age_pro_cd/haddock_svspp_raw_lou.xlsx", clear first
renvarlab, lower
merge m:1 cruise6 using `cruises'
keep if _merge==3
drop if age==0
replace age=9 if age>=9
collapse (sum) count, by(year svspp age length)
destring year, replace
sort svspp year age length count

keep if year>=$trawl_svy_start_yr & year<=$trawl_svy_end_yr

*use "$age_pro_cd/haddock_svspp_raw.dta", clear 
*keep if year>=2020
su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
replace age=9 if age>9
collapse (sum) count, by (age length)

/*
tsset age length
tsfill, full
*/
/*
preserve
su length 
clear 
set obs 2
gen length=`r(min)' if _n==1
replace length=`r(max)' if _n==2
tsset length
tsfill, full
expand 2, gen(dup)
replace length=length+.5 if dup==1
sort length 
drop dup
expand 9
bysort length: gen age=_n
tempfile full_lengths
save `full_lengths', replace 
restore 

merge 1:1 length age using `full_lengths'
drop _merge 
*/

sort age length 
mvencode count, mv(0) override 


levelsof age, local(ages)
foreach a of local ages{
	su length if age==`a' & count!=0
	
	lowess count length if age==`a' & length>=`r(min)' & length<=`r(max)', adjust bwidth(.3) gen(s`a') nograph
	replace s`a'=0 if s`a'<=0
}
egen smoothed=rowtotal(s1-s9)
drop s1-s9

egen sum=sum(smoothed), by(age)	
gen prop_smoothed=smoothed/sum	

egen sum_raw=sum(count), by(age)	
gen prop_raw=count/sum_raw	

drop sum sum_raw

/*
levelsof age, local(ages)
foreach a of local ages{
twoway(scatter count length if age==`a', connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter smoothed length if age==`a', connect(direct) lcol(blue) title(haddock age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr', size(small)) ///
			ytitle(# fish, size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length inches, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames'
*/

tempfile al_hadd
save `al_hadd', replace 

use "$age_pro_cd/historical_and_mean_projected_Haddock_NAA.dta", clear 
keep if year==2023
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 

sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length)

sort length 

gen species="hadd"

preserve
use "$age_pro_cd/rec_selectivity_CaL_open_seasons_cm.dta", clear
keep if species=="hadd"
tempfile hadd
save `hadd', replace 
restore 

merge 1:m length species using `hadd'
drop if _merge==1
sort species season length 

egen sum_fitted=sum(fitted), by(species season)
egen sum_obs=sum(observed), by(species season)

drop _merge
gen ql_raw=nfish_catch_from_raw/NaL_from_raw_trawl
gen ql_smooth=nfish_catch_from_fitted/NaL_from_smooth_trawl



keep length species season observed fitted_prob ql*  
order species season length 

append using `cod_ql'
sort species season length



tempfile cod_hadd_ql
save `cod_hadd_ql', replace


****Having computed slectivities by month, now draw projected NaA, translate to lengths, and
****merge these data to the ql data and create catch-at-length in the projection year *  and compute projected catch-at-lengths
use "$age_pro_cd/cod_beginning_sorted2023.dta", clear 
egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
keep if year==2024

sample 150, count
gen id2=_n

tempfile new
save `new', replace 

global nal

forv i=1/150{
	u `new', clear
	keep if id2 ==`i'
	*keep if id2 ==1


reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_cod', keep(3) nogen 
sort  age length

gen NaL_2024_raw_trawl = prop_raw*nfish
gen NaL_2024_smooth_trawl = prop_smoothed*nfish

collapse (sum) NaL_2024*, by(length)
sort length 
gen id2=`i'


tempfile nal`i'
save `nal`i'', replace
global nal "$nal "`nal`i''" " 

}	

clear
dsconcat $nal
gen species="cod"
tempfile proj_cod
save `proj_cod', replace

*haddock
use "$age_pro_cd/haddock_beginning_sorted2023.dta", clear 
keep if year==2024

sample 150, count
gen id2=_n

tempfile new
save `new', replace 

global nal

forv i=1/150{
	u `new', clear
	keep if id2 ==`i'


reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_2024_raw_trawl = prop_raw*nfish
gen NaL_2024_smooth_trawl = prop_smoothed*nfish

collapse (sum) NaL_2024*, by(length)
sort length 
gen id2=`i'


tempfile nal`i'
save `nal`i'', replace
global nal "$nal "`nal`i''" " 

}	

clear
dsconcat $nal
gen species="hadd"
append using `proj_cod'

sort id2 species length 

gen season="open"
expand 2, gen(dup)
replace season="closed" if dup==1
drop dup 
merge m:1 length species season using `cod_hadd_ql'

sort id2 species season length
drop if _merge==1
drop if _merge==2

drop _merge

gen catch24_raw=ql_raw*NaL_2024_raw
gen catch24_smooth=ql_smooth*NaL_2024_smooth

egen sumcatch24_raw=sum(catch24_raw), by(season species id2) 
egen sumcatch24_smooth=sum(catch24_smooth), by(season species id2) 

gen proj_CaL_prob_raw= catch24_raw/sumcatch24_raw
gen proj_CaL_prob_smooth= catch24_smooth/sumcatch24_smooth

egen sumprob_raw=sum(proj_CaL_prob_raw), by(season species id2)
egen sumprob_sm=sum(proj_CaL_prob_smooth), by(season species id2)

drop sumprob*

/*
sort id2 length 
twoway(scatter NaL_2024_smooth_trawl length if id2<=5, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter proj_CaL_prob_smooth length if id2<=5, connect(direct) lcol(blue) title(haddock age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr', size(small)) ///
			ytitle(# fish, size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length inches, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i)  name(dom`a', replace))

replace NaL_2024_smooth_trawl=NaL_2024_smooth_trawl/1000000
twoway(scatter NaL_2024_smooth_trawl length if species=="hadd" & id2==5 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter NaL_2024_smooth_trawl length if species=="hadd" & id2==4 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter NaL_2024_smooth_trawl length if species=="hadd" & id2==3 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter NaL_2024_smooth_trawl length if species=="hadd" & id2==2 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter NaL_2024_smooth_trawl length if species=="hadd" & id2==1 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) 
			
twoway(scatter proj_CaL_prob_smooth length if species=="hadd" & id2==5 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter proj_CaL_prob_smooth length if species=="hadd" & id2==4 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter proj_CaL_prob_smooth length if species=="hadd" & id2==3 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter proj_CaL_prob_smooth length if species=="hadd" & id2==2 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) ///
			(scatter proj_CaL_prob_smooth length if species=="hadd" & id2==1 & season=="open", connect(direct) lcol(black)   lwidth(thin)  lpat(solid) msymbol(i) ) 

tabstat	proj_CaL_prob_smooth  if species=="hadd" & id2<=5 & season=="open" & length>=43.18, stat(sum) by(id2)
*/		
			
keep length species id2 season proj_CaL_prob*
order id2 species season   proj_CaL_prob*

rename id2 draw

save "$age_pro_cd/projected_CaL_cod_hadd_cm.dta", replace 
export delimited using "$age_pro_cd/projected_CaL_cod_hadd_cm.csv", replace



