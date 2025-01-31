
**Add this to data wrapper 
global base_year 2022


clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

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
 
keep if year==$base_year 
 
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
gen my_dom_id_string=state+"_"+wv2+"_"+mode1+"_"+common_dom

gen no_dup=0
replace no_dup=1 if !inlist(common, "summerflounder", "blackseabass", "scup")
bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="SF"

gen sf_tot_cat=tot_cat if common=="summerflounder"
egen sum_sf_tot_cat=sum(sf_tot_cat), by(strat_id psu_id id_code)

gen sf_harvest=landing if common=="summerflounder"
egen sum_sf_harvest=sum(sf_harvest), by(strat_id psu_id id_code)
 
gen sf_releases=release if common=="summerflounder"
egen sum_sf_releases=sum(sf_releases), by(strat_id psu_id id_code)
 
gen bsb_tot_cat=tot_cat if common=="blackseabass"
egen sum_bsb_tot_cat=sum(bsb_tot_cat), by(strat_id psu_id id_code)

gen bsb_harvest=landing if common=="blackseabass"
egen sum_bsb_harvest=sum(bsb_harvest), by(strat_id psu_id id_code)

gen bsb_releases=release if common=="blackseabass"
egen sum_bsb_releases=sum(bsb_releases), by(strat_id psu_id id_code)

gen scup_tot_cat=tot_cat if common=="scup"
egen sum_scup_tot_cat=sum(scup_tot_cat), by(strat_id psu_id id_code)

gen scup_harvest=landing if common=="scup"
egen sum_scup_harvest=sum(scup_harvest), by(strat_id psu_id id_code)

gen scup_releases =release if common=="scup"
egen sum_scup_releases=sum(scup_releases), by(strat_id psu_id id_code)

encode strat_id, gen(strat_id2)
encode psu_id, gen(psu_id2)

svyset psu_id2 [pweight= wp_int], strata(strat_id2) singleunit(certainty)


drop sf_tot_cat sf_harvest sf_releases bsb_tot_cat bsb_harvest bsb_releases scup_tot_cat scup_harvest scup_releases
rename sum_sf_tot_cat sf_catch
rename sum_sf_harvest sf_keep
rename sum_sf_releases sf_rel
rename sum_bsb_tot_cat bsb_catch
rename sum_bsb_harvest bsb_keep
rename sum_bsb_releases bsb_rel
rename sum_scup_tot_cat scup_catch
rename sum_scup_harvest scup_keep
rename sum_scup_releases scup_rel

local vars sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel scup_catch  scup_keep scup_rel
foreach v of local vars{
	replace `v'=round(`v')
	
}

//gen sf_tot_cat_round=round(sum_sf_tot_cat)
//gen bsb_tot_cat_round=round(sum_bsb_tot_cat)
//gen scup_tot_cat_round=round(sum_scup_tot_cat)

//svy: total dtrip if my_dom_id_string=="VA_6_sh_SF" & sum_bsb_tot_cat==0

tempfile new 
save `new', replace

*Loop over domains to get estimated numbers of trips that caught, harvested, and released X fish and se's
global catch 
levelsof my_dom_id_string, local(sts)
foreach s of local sts{
	
u `new', clear 

**SF catch
preserve
estpost svy: tab sf_catch my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="catch"		
gen species="sf"
gen domain="`s'"
tempfile sf`s'catch
save `sf`s'catch', replace 
restore 


**BSB catch
preserve
estpost svy: tab bsb_catch my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="catch"			
gen species="bsb"
gen domain="`s'"
tempfile bsb`s'catch
save `bsb`s'catch', replace 
restore 


**Scup catch
preserve
estpost svy: tab scup_catch my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="catch"		
gen species="scup"
gen domain="`s'"

tempfile scup`s'catch
save `scup`s'catch', replace 
restore 

**SF keep
preserve
estpost svy: tab sf_keep my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="keep"		
gen species="sf"
gen domain="`s'"
tempfile sf`s'keep
save `sf`s'keep', replace 
restore 


**BSB keep
preserve
estpost svy: tab bsb_keep my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="keep"	
gen species="bsb"
gen domain="`s'"
tempfile bsb`s'keep
save `bsb`s'keep', replace 
restore 


**Scup keep
preserve
estpost svy: tab scup_keep my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="keep"	
gen species="scup"
gen domain="`s'"

tempfile scup`s'keep
save `scup`s'keep', replace 
restore 


**SF release
preserve
estpost svy: tab sf_rel my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="rel"	
gen species="sf"
gen domain="`s'"
tempfile sf`s'rel
save `sf`s'rel', replace 
restore 


**BSB release
preserve
estpost svy: tab bsb_rel my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips
gen disp="rel"	
gen species="bsb"
gen domain="`s'"
tempfile bsb`s'rel
save `bsb`s'rel', replace 
restore 


**Scup release
preserve
estpost svy: tab scup_rel my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames nfish
rename b1 tot_ntrips
rename se se_ntrips

gen disp="rel"	
gen species="scup"
gen domain="`s'"

tempfile scup`s'rel
save `scup`s'rel', replace 
restore 



u `sf`s'catch', clear 
append using  `bsb`s'catch'
append using  `scup`s'catch'
append using  `sf`s'keep'
append using  `bsb`s'keep'
append using  `scup`s'keep'
append using  `sf`s'rel'
append using  `bsb`s'rel'
append using  `scup`s'rel'


tempfile catch`s'
save `catch`s'', replace
global catch "$catch "`catch`s''" " 

}	

clear
dsconcat $catch

sort domain species nfish

replace tot_n=round(tot_n)
replace se=round(se)

replace domain=domain+"_"+species+"_"+disp
drop species disp


*With the point estimated and standard errors, create 150 probability distributions of harvest, relased, total catch 

tempfile new
save `new', replace 

global domainz
global catch_draw

levelsof domain, local(doms)
foreach d of local doms{
	
u `new', clear 
keep if domain=="`d'"
//keep if domain=="CT_2_sh_SF_bsb"

tempfile new1
save `new1', replace 

levelsof nfish, local(levs)
foreach c of local levs{
	
u `new1', clear 

keep if nfish==`c'
//keep if tot_cat==0

su tot_ntrip
local mean=`r(mean)'

su se
local se=`r(mean)'

clear
set obs 150
gen nfish=`c'
gen tot_ntrip=max(0, rnormal(`mean', `se'))
gen draw=_n
reshape wide tot_ntrip, i(nfish) j(draw)

gen domain="`d'"
tempfile catch_draw`c'
save `catch_draw`c'', replace
global catch_draw "$catch_draw "`catch_draw`c''" " 


}	

clear
dsconcat $catch_draw

tempfile domainz`d'
save `domainz`d'', replace
global domainz "$domainz "`domainz`d''" " 

}
clear 
dsconcat $domainz
order domain
sort domain nfish

duplicates drop 

split domain, parse("_")
order domain*
rename domain1 state
rename domain2 wave
rename domain3 mode
rename domain5 species
rename domain6 disp
drop domain4

cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\RDM data and code\catch per trip data"
save "catch per trip1.dta", replace 


/*
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\RDM data and code\catch per trip data"
u "catch per trip1.dta", clear 
drop if disp=="catch"
drop domain
gen domain=state+"_"+mode+"_"+wave
gen domain2=state+"_"+mode+"_"+wave+"_"+species
order domain*

tempfile catches
save `catches', replace
*/





import delimited using  "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\RDM data and code\directed trips and regulations 2022_100 draws.csv", clear 
gen wave = 1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)
tostring month, gen(month1)
tostring wave, gen(wave1)

drop if dtrip==0

keep day_i day state mode month month1 dtrip wave wave1 draw

gen domain1=state+"_"+mode+"_"+day
gen domain=state+"_"+mode+"_"+wave1

cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\RDM data and code\"

encode domain1, gen(domain3)


tempfile trips
save `trips', replace

mata:
mata clear 
void testtest2(real scalar d) {
M = st_matrix("M")
	R = M[1,.] // Realisations  in row 1
	P = M[2,.] // Probabilities in row 2
	n = 1500
	D = rdiscrete(1,n,P)  // n discrete random variates from 1 to k
	V = R[1, D]           // V[i] = R[D[i]], 1 <= i <= n
	st_matrix("V", V')
}
end

local end_mata end
*global drawz

*forv i=1/2{

global domainz

levelsof domain3 if inlist(domain1, "CT_fh_29jul2022", "CT_fh_19jul2022"), local(doms) 
foreach d of local doms{

	u `trips', clear 
	local i=1
	*local d="CT_fh_29jul2022"
	keep if draw==`i'
	*local d=198
	keep if domain3==`d'
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	levelsof wave, local(wv) 
	levelsof dtrip, local(dtrip) 
	levelsof domain1, local(dom1) clean

	
	u "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\RDM data and code\catch per trip data\catch per trip1.dta", clear 
	drop if disp=="catch"
	drop domain
	gen domain=state+"_"+mode+"_"+wave
	gen domain2=state+"_"+mode+"_"+wave+"_"+species
	order domain*

	keep if domain=="`dom'"

	tempfile base`d' 
	save `base`d'', replace 
	

	**keep draws sf
	u `base`d'', clear 
	keep if species=="sf"
	
	ds domain domain2 state wave mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'
	
	keep if disp=="keep"
	
	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	mkmat nfish, mat(tots)
	mat  tots = tots'
	mat list tots

	mkmat prop , mat(prob_r1s)
	mat  prob_r1s = prob_r1s'
	mat list prob_r1s

	mat M = (tots\prob_r1s)
	mat list M
	mata: testtest2(`d')
	
	mat list V
	svmat double V, names(col)
	keep c1
	rename c1 nfish
	renvarlab nfish, postfix("`sp'")
	renvarlab nfish, postfix("`dsp'")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	tempfile keep
	save `keep', replace


	*release draws sf
	u `base`d'', clear 

	keep if species=="sf"
	keep if disp=="rel"
	
	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean
	
	ds domain domain2 state wave mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	mkmat nfish, mat(tots)
	mat  tots = tots'
	mat list tots

	mkmat prop , mat(prob_r1s)
	mat  prob_r1s = prob_r1s'
	mat list prob_r1s

	mat M = (tots\prob_r1s)
	mat list M
	
    mata: testtest2(`d')


	mat list V
	svmat double V, names(col)
	keep c1
	rename c1 nfish
	renvarlab nfish, postfix("`sp'")
	renvarlab nfish, postfix("`dsp'")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n

	merge 1:1 tripid catch_draw using `keep', nogen
	
	tempfile sf
	save `sf', replace
	
	
	
	**keep draws bsb
	u `base`d'', clear 

	keep if species=="bsb"
	
	ds domain domain2 state wave mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'
	
	keep if disp=="keep"
	
	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	mkmat nfish, mat(tots)
	mat  tots = tots'
	mat list tots

	mkmat prop , mat(prob_r1s)
	mat  prob_r1s = prob_r1s'
	mat list prob_r1s

	mat M = (tots\prob_r1s)
	mat list M
    mata: testtest2(`d')

	mat list V
	svmat double V, names(col)
	keep c1
	rename c1 nfish
	renvarlab nfish, postfix("`sp'")
	renvarlab nfish, postfix("`dsp'")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	
	tempfile keep
	save `keep', replace


	*release draws bsb
	u `base`d'', clear 

	keep if species=="bsb"
	keep if disp=="rel"
	
	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean
	
	ds domain domain2 state wave mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	mkmat nfish, mat(tots)
	mat  tots = tots'
	mat list tots

	mkmat prop , mat(prob_r1s)
	mat  prob_r1s = prob_r1s'
	mat list prob_r1s

	mat M = (tots\prob_r1s)
	mat list M
    mata: testtest2(`d')

	mat list V
	svmat double V, names(col)
	keep c1
	rename c1 nfish
	renvarlab nfish, postfix("`sp'")
	renvarlab nfish, postfix("`dsp'")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n

	merge 1:1 tripid catch_draw using `keep', nogen
	
	tempfile bsb
	save `bsb', replace
	
	**keep draws scup
	u `base`d'', clear 

	keep if species=="scup"
	
	ds domain domain2 state wave mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'
	
	keep if disp=="keep"
	
	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	mkmat nfish, mat(tots)
	mat  tots = tots'
	mat list tots

	mkmat prop , mat(prob_r1s)
	mat  prob_r1s = prob_r1s'
	mat list prob_r1s

	mat M = (tots\prob_r1s)
	mat list M
    mata: testtest2(`d')

	mat list V
	svmat double V, names(col)
	keep c1
	rename c1 nfish
	renvarlab nfish, postfix("`sp'")
	renvarlab nfish, postfix("`dsp'")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n
	tempfile keep
	save `keep', replace


	*release draws scup
	u `base`d'', clear 

	keep if species=="scup"
	ds domain domain2 state wave mode species disp nfish
	keep `r(varlist)' tot_ntrip`i'
	
	keep if disp=="rel"
	
	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean
	
	
	su tot_ntrip`i'
	gen double prop=tot_ntrip`i'/r(sum)

	keep if prop!=0
	mkmat nfish, mat(tots)
	mat  tots = tots'
	mat list tots

	mkmat prop , mat(prob_r1s)
	mat  prob_r1s = prob_r1s'
	mat list prob_r1s

	mat M = (tots\prob_r1s)
	mat list M

    mata: testtest2(`d')

	mat list V
	svmat double V, names(col)
	keep c1
	rename c1 nfish
	renvarlab nfish, postfix("`sp'")
	renvarlab nfish, postfix("`dsp'")
	egen tripid = seq(), f(1) t(50)
	bysort tripid: gen catch_draw=_n

	merge 1:1 tripid catch_draw using `keep', nogen
	
	***Merge to the other species 
	merge 1:1 tripid catch_draw using  `bsb', nogen
	merge 1:1 tripid catch_draw using  `sf', nogen
	gen domain3=`d'
	gen domain="`dom'"
	gen day_i=`day_i1'
	gen day ="`day1'"

	
	gen state="`st'"
	gen mode="`md'"
	gen wave=`wv'
	gen month=`mnth'
	
	gen draw = `i'
	gen dtrip=`dtrip'
	gen domain1="`dom1'"

	gen sf_catch=nfishsfrel+nfishsfkeep
	gen scup_catch=nfishscuprel+nfishscupkeep
	gen bsb_catch=nfishbsbrel+nfishbsbkeep

	rename nfishsfrel sf_rel
	rename nfishsfkeep sf_keep
	rename nfishscuprel scup_rel
	rename nfishscupkeep scup_keep
	rename nfishbsbrel bsb_rel
	rename nfishbsbkeep bsb_keep

	tempfile domainz`d'
	save `domainz`d'', replace
	global domainz "$domainz "`domainz`d''" " 

}


clear
dsconcat $domainz



tempfile drawz`i'
save `drawz`i'', replace
global drawz "$drawz "`drawz`i''" " 


clear 
dsconcat $drawz

	
	
	
	



//drop domain state wave mode species 
//reshape wide tot_ntrip*, i(domain3) j(domain2) string 
//reshape wide disp, i( tot_ntrip*) j(domain2) string 

//mvencode tot_ntrip*,mv(0) override
//rename domain2 domain
save "catch per trip2.dta", replace 

import delimited using  "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\RDM data and code\directed trips and regulations 2022_100 draws.csv", clear 

*Import the regualtions file, which contains the calender days which saw directed effort 
*For each day, draw 1500 (50 trips, 30 catch draws each) catches per trip by species 
import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen wave = 1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)
tostring month, gen(month1)
tostring wave, gen(wave1)


drop if dtrip==0
//keep if state=="MA"
keep day_i day state mode month month1 dtrip wave wave1

gen domain1=state+"_"+mode+"_"+day
gen domain=state+"_"+mode+"_"+wave1

drop wave dtrip 
rename waev1 wave 

merge m:1 domain using "catch per trip2.dta"

tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="CT_pr_15may2022"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	levelsof wave, local(wv) 

	
	
u "catch per trip2.dta", clear 
keep if domain=="`dom'"
//keep if domain=="CT_scup_3_fh_keep"

tempfile new2
save `new2', replace

levelsof domain2, local(domz)
foreach d of local domz{}
	
	u `new2', clear 
	keep if domain2=="CT_bsb_3_pr_rel"


	levelsof species, local(sp) clean
	levelsof disp, local(dsp) clean

ds domain domain2 state wave mode species disp nfish
di "`r(varlist)'"
//keep `r(varlist)' tot_ntrip1bsb tot_ntrip1scup tot_ntrip1sf
keep `r(varlist)' tot_ntrip1

/*
su tot_ntrip1sf
gen double prop_sf=tot_ntrip1sf/r(sum)

su tot_ntrip1bsb
gen double prop_bsb=tot_ntrip1bsb/r(sum)

su tot_ntrip1scup
gen double prop_scup=tot_ntrip1scup/r(sum)
*/

su tot_ntrip1
gen double prop=tot_ntrip1/r(sum)

preserve 
keep if prop!=0
mkmat nfish, mat(tots)
mat  tots = tots'
mat list tots

mkmat prop , mat(prob_r1s)
mat  prob_r1s = prob_r1s'
mat list prob_r1s

mat M = (tots\prob_r1s)
mat list M

mata:
M = st_matrix("M")
R = M[1,.] // Realisations  in row 1
P = M[2,.] // Probabilities in row 2
n = 1500
D = rdiscrete(1,n,P)  // n discrete random variates from 1 to k
V = R[1, D]           // V[i] = R[D[i]], 1 <= i <= n
st_matrix("V", V')
end

mat list V
svmat double V, names(col)
keep c1
rename c1 nfish
egen tripid = seq(), f(1) t(50)
bysort tripid: gen catch_draw=_n
tempfile sf
save `sf', replace
restore



/*
*Summer flounder
preserve 
keep if prop_sf!=0
mkmat tot_cat, mat(tots)
mat  tots = tots'
mat list tots

mkmat prop_sf , mat(prob_r1s)
mat  prob_r1s = prob_r1s'
mat list prob_r1s

mat M = (tots\prob_r1s)
mat list M

mata:
M = st_matrix("M")
R = M[1,.] // Realisations  in row 1
P = M[2,.] // Probabilities in row 2
n = 1500
D = rdiscrete(1,n,P)  // n discrete random variates from 1 to k
V = R[1, D]           // V[i] = R[D[i]], 1 <= i <= n
st_matrix("V", V')
end

mat list V
svmat double V, names(col)
keep c1
rename c1 tot_cat_sf
egen tripid = seq(), f(1) t(50)
bysort tripid: gen catch_draw=_n
tempfile sf
save `sf', replace
restore

*Bkack sea bass 
preserve 
keep if prop_bsb!=0
mkmat tot_cat, mat(tots)
mat  tots = tots'
mat list tots

mkmat prop_bsb, mat(prob_r1s)
mat  prob_r1s = prob_r1s'
mat list prob_r1s

mat M = (tots\prob_r1s)
mat list M

mata:
M = st_matrix("M")
R = M[1,.] // Realisations  in row 1
P = M[2,.] // Probabilities in row 2
n = 1500
D = rdiscrete(1,n,P)  // n discrete random variates from 1 to k
V = R[1, D]           // V[i] = R[D[i]], 1 <= i <= n
st_matrix("V", V')
end

mat list V
svmat double V, names(col)
keep c1
rename c1 tot_cat_bsb
egen tripid = seq(), f(1) t(50)
bysort tripid: gen catch_draw=_n
matrix drop V tots   M prob_r1s
tempfile bsb
save `bsb', replace
restore

*scup
preserve 
keep if prop_scup!=0
mkmat tot_cat, mat(tots)
mat  tots = tots'
mat list tots

mkmat prop_scup, mat(prob_r1s)
mat  prob_r1s = prob_r1s'
mat list prob_r1s

mat M = (tots\prob_r1s)
mat list M

mata:
M = st_matrix("M")
R = M[1,.] // Realisations  in row 1
P = M[2,.] // Probabilities in row 2
n = 1500
D = rdiscrete(1,n,P)  // n discrete random variates from 1 to k
V = R[1, D]           // V[i] = R[D[i]], 1 <= i <= n
st_matrix("V", V')
end

mat list V
svmat double V, names(col)
keep c1
rename c1 tot_cat_scup
egen tripid = seq(), f(1) t(50)
bysort tripid: gen catch_draw=_n
matrix drop V tots M prob_r1s
tempfile scup
save `scup', replace
restore

*/
u `sf', clear
merge 1:1 tripid catch_draw using `bsb', keep(3) nogen
merge 1:1 tripid catch_draw using `scup', keep(3) nogen
order tripid catch_draw
	
gen day="`day1'"
gen day_i=`day_i1'
gen state="`st'"
gen mode="`md'"
gen wave=`wv'
gen month=`mnth'

keep state mode month day_i  day tripid catch_draw tot* landing* release* 
	
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










/*
expand 2, gen(dup)
gen month=1 if wave=="1" & dup==0
replace month=2 if wave=="1" & dup==1

replace month=3 if wave=="2" & dup==0
replace month=4 if wave=="2" & dup==1

replace month=5 if wave=="3" & dup==0
replace month=6 if wave=="3" & dup==1

replace month=7 if wave=="4" & dup==0
replace month=8 if wave=="4" & dup==1

replace month=9 if wave=="5" & dup==0
replace month=10 if wave=="5" & dup==1

replace month=11 if wave=="6" & dup==0
replace month=12 if wave=="6" & dup==1
drop dup 
order month 
*/
keep if domain=="CT_3_fh_SF_bsb"
ds  domain state wave mode species tot_cat
di "`r(varlist)'"
keep `r(varlist)' tot_ntrip1
su tot_ntrip1
gen prop=tot_ntrip1/r(sum)




tempfile base
save `base', replace 



import delimited using  "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface\directed trips and regulations 2022_pstar.csv", clear 
gen domain1=state+"_"+mode+"_"+day
tostring month, gen(month1)
gen domain=state+"_"+mode+"_"+month1

drop if dtrip==0



keep tot_ntrip2 domain tot_cat
replace tot_ntrip2=round(tot_ntrip2)
su tot_ntrip2
gen prop=tot_ntrip2/`r(sum)'

nbreg tot_cat [fweight=tot_ntrip2] 


local mu = exp(_b[_cons])
local size = 1/e(alpha)
di `size'
local prob = `size'/(`size'+`mu')
di `prob'
local scale = (1-`prob')/`prob'


clear
set obs 10000
gen xrnbinom = rnbinomial(`size',`prob')
su xrnbinom
gen ntrip1=1
collapse (sum) ntrip1, by(xr)

gen tot_ntrips_round=round(tot_ntrips)
nbreg tot_cat [fweight=tot_ntrips_round] if domain=="NJ_4_pr_SF" & species=="sf"
svy: nbreg sum_sf_tot_cat if my_dom_id_string=="NJ_3_pr_SF"
svy: nbreg sum_sf_tot_cat if my_dom_id_string=="NJ_4_pr_SF"
svy: nbreg sum_sf_tot_cat if my_dom_id_string=="NJ_5_pr_SF"

di "CV=" 
svy: nbreg sum_sf_tot_cat if my_dom_id_string=="NJ_6_pr_SF"
svy: nbreg sum_sf_tot_cat if inlist(my_dom_id_string,"NJ_6_pr_SF", "NJ_5_pr_SF")



svy: nbreg sum_sf_tot_cat if my_dom_id_string=="NJ_4_pr_SF", dispersion(constant)
svy: nbreg sum_sf_tot_cat if my_dom_id_string=="NJ_4_pr_SF"

local mu = exp(_b[_cons])
local size = 1/e(alpha)
*local size = 1/_b[/lnalpha]
local prob = `size'/(`size'+`mu')
local scale = (1-`prob')/`prob'

gen xrnbinom = rnbinomial(`size',`prob')

svy: mean sum_sf_tot_cat if my_dom_id_string=="NJ_4_pr_SF"
*su sum_sf_tot_cat if my_dom_id_string=="NJ_4_pr_SF"

su xrnbinom 
if my_dom_id_string=="NJ_4_pr_SF"

nbreg xrnbinom
di "size = " 1/e(alpha) ", prob = " ///
    1/e(alpha)/(1/e(alpha)+exp(_b[_cons]))

local cons = _b[_cons]
di `cons'
di exp(`cons')

local alpha = _b[/lnalpha]
di `alpha'
di 1/exp(`alpha')


local cons_se1=e(V)[1,1]
di `cons_se1'
local cons_se2=sqrt(`cons_se1')
di `cons_se2'

rnbinomial(n,p)

local mu = exp(_b[_cons])
local size = 1/e(alpha)
local prob = `size'/(`size'+`mu')
local scale = (1-`prob')/`prob'

*original unadjusted catch datasets

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
svy:total tot_cat if my_dom_id_string=="NJ_3_pr_SF"

svy:nbreg tot_cat, irr
local mu = exp(_b[_cons])
local size = 1/e(alpha)
local prob = `size'/(`size'+`mu')
local scale = (1-`prob')/`prob'


mat list r(table), format(%12.0gc)
