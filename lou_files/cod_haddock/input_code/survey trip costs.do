
***This code creates trip cost distributions based on the Sabrina's 2017 trip expenditure survey data

*Enter a directory with the expenditure survey data 
*cd "C:\Users\andrew.carr-harris\Desktop\Fluke_MSE\MRIP_data"
u "$input_code_cd\atl_states_2017_expsurvey.dta", clear
renvarlab *, lower


*keep only the states we need (ME-NC) 
keep if inlist(st, 23, 33, 25)

egen n_missing_cats=rowmiss(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
drop if n_missing==16

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)
egen total_exp2=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp)

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

*check to see if we get the same results as Sabrina
svy: mean lodgexp if st==23 & mode_fx=="3"
svy: mean restexp if st==23 & mode_fx=="3"

/*
tostring st, gen(st2)
gen st_mode=mode+"_" +st2
encode st_mode, gen(st_mode2)
svy: mean total_exp2 , over(st_mode2)
*/

*^ this produdes the exact same results as in the report. I tested several exp. categories and they all mateched. 

gen st2 = string(st,"%02.0f")
gen state="CT" if st2=="09" 
replace state="DE" if st2=="10"
replace state="ME" if st2=="23"
replace state="MD" if st2=="24"
replace state="MA" if st2=="25"
replace state="NJ" if st2=="34"
replace state="NY" if st2=="36"
replace state="NC" if st2=="37"
replace state="RI" if st2=="44"
replace state="VA" if st2=="51"
replace state="NH" if st2=="33"

*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")


*Now generate total expenditures each state/mode combination

global costs

*Adjust for inflation
replace total_exp = total_exp*1.16

tempfile new
save `new', replace

levelsof mode1, local(modes)
foreach m of local modes{

u `new', clear 
svy: mean total_exp if mode1=="`m'"

mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
gen mode1="`m'"


tempfile costs`s'`m'
save `costs`s'`m'', replace
global costs "$costs "`costs`s'`m''" " 
}

clear
dsconcat $costs


su mean if mode1=="fh"
global fh_cost_est `r(mean)'
su st if mode1=="fh"
global fh_cost_sd `r(mean)'


su mean if mode1=="pr"
global pr_cost_est `r(mean)'
su st if mode1=="pr"
global pr_cost_sd `r(mean)'


su mean if mode1=="sh"
global sh_cost_est `r(mean)'
su st if mode1=="sh"
global sh_cost_sd `r(mean)'




