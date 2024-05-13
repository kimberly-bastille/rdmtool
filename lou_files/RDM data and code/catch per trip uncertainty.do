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

gen sf_tot_cat_round=round(sum_sf_tot_cat)
gen bsb_tot_cat_round=round(sum_bsb_tot_cat)
gen scup_tot_cat_round=round(sum_scup_tot_cat)

//svy: total dtrip if my_dom_id_string=="VA_6_sh_SF" & sum_bsb_tot_cat==0

tempfile new 
save `new', replace

*Loop over domains to get estimated numbers of trips that caught X fish and se's
global catch 
levelsof my_dom_id_string, local(sts)
foreach s of local sts{
	
u `new', clear 

**SF
preserve
estpost svy: tab sf_tot_cat_round my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)

mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames tot_cat
rename b1 tot_ntrips
rename se se_ntrips
	
gen species="sf"
gen domain="`s'"
*gen year = 2021
tempfile sf`s'
save `sf`s'', replace 
restore 


*BSB
preserve
estpost svy: tab bsb_tot_cat_round my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames tot_cat
rename b1 tot_ntrips
rename se se_ntrips
	
gen species="bsb"
gen domain="`s'"
*gen year = 2021
tempfile bsb`s'
save `bsb`s'', replace 
restore 


*Scup
preserve
estpost svy: tab scup_tot_cat_round my_dom_id_string if my_dom_id_string=="`s'", count ci se format(%11.3g)
mat b = e(b)'
mat se = e(se)'
mat rownames = e(Row)'

clear 
svmat rownames
svmat b
svmat se
		
drop if rownames==.
rename rownames tot_cat
rename b1 tot_ntrips
rename se se_ntrips
	
gen species="scup"
gen domain="`s'"
*gen year = 2021
tempfile scup`s'
save `scup`s'', replace 
restore 

u `sf`s'', clear 
append using  `bsb`s''
append using  `scup`s''


tempfile catch`s'
save `catch`s'', replace
global catch "$catch "`catch`s''" " 

}	

clear
dsconcat $catch

sort domain species tot_cat

replace tot_n=round(tot_n)
replace se=round(se)

replace domain=domain+"_"+species 
drop species

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

levelsof tot_cat, local(levs)
foreach c of local levs{
	
u `new1', clear 

keep if tot_cat==`c'
//keep if tot_cat==0

su tot_ntrip
local mean=`r(mean)'

su se
local se=`r(mean)'

clear
set obs 150
gen tot_cat=`c'
gen tot_ntrip=max(0, rnormal(`mean', `se'))
gen draw=_n
reshape wide tot_ntrip, i(tot_cat) j(draw)

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
sort domain tot_cat

duplicates drop 


cd "C:\Users\andrew.carr-harris\Desktop\RDM data and code"

u "catch per trip1.dta", clear ///save catch per trip 
split domain, parse("_")
order domain*
rename domain1 state
rename domain2 wave
rename domain3 mode
rename domain5 species
drop domain4

gen domain2=state+"_"+mode+"_"+wave
drop domain
reshape wide tot_ntrip*, i(domain state wave mode tot_cat) j(species) string 
mvencode tot_ntrip*,mv(0) override
rename domain2 domain
save "catch per trip2.dta", replace 


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

tempfile base
save `base', replace

forv i=1/100{
u `base', clear 

global domainz

levelsof domain1, local(doms)
foreach d of local doms{
	u `base', clear 
	keep if domain1=="CT_fh_03jul2022"
	levelsof state, local(st) clean
	levelsof mode, local(md) clean
	levelsof month, local(mnth)
	levelsof day_i, local(day_i1)
	levelsof day, local(day1) clean
	levelsof domain, local(dom) clean
	levelsof wave, local(wv) 

	
	
u "catch per trip2.dta", clear 
keep if domain=="`dom'"

ds domain state wave mode tot_cat
di "`r(varlist)'"
keep `r(varlist)' tot_ntrip1bsb tot_ntrip1scup tot_ntrip1sf

su tot_ntrip1sf
gen double prop_sf=tot_ntrip1sf/r(sum)

su tot_ntrip1bsb
gen double prop_bsb=tot_ntrip1bsb/r(sum)

su tot_ntrip1scup
gen double prop_scup=tot_ntrip1scup/r(sum)

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
