
cd "C:\Users\andrew.carr-harris\Desktop\Git\RDM 2024 testing"

**We need three dataset1:
*1) Differences in total keep and release by species between calibration and projection holding everything constant besides how to compute keep/release
*2) Differences in keep- and release-at-length " "
*3) Differences in CV and number of trips " "


*1)
import excel using "calibration_output_NJ.xlsx", clear first 

split period2, parse(_)
rename period21 month
rename period22 day
rename period23 mode1

collapse (sum) tot_keep_sf tot_rel_sf tot_keep_bsb tot_keep_scup tot_rel_bsb tot_rel_scup, by(mode n_cal)
rename n_cal draw

ds mode draw, not
renvarlab `r(varlist)', postfix("_calib")
*destring month, replace 
tempfile calib
save `calib', replace 

import excel using "prob_star_estimates_lengths_NJ.xlsx", clear first 
keep mode1 bsb_keep_sum bsb_rel_sum scup_keep_sum scup_rel_sum sf_keep_sum sf_rel_sum draw
merge 1:1 mode1 draw using `calib'

gen diff_sf_keep= tot_keep_sf_calib-sf_keep_sum
gen diff_bsb_keep= tot_keep_bsb_calib-bsb_keep_sum
gen diff_scup_keep= tot_keep_scup_calib-scup_keep_sum

gen diff_sf_rel=tot_rel_sf_calib- sf_rel_sum
gen diff_bsb_rel= tot_rel_bsb_calib-bsb_rel_sum
gen diff_scup_rel= tot_rel_scup_calib-scup_rel_sum

order mode draw diff*
drop _merge
browse mode draw  sf_keep_sum tot_keep_sf_calib diff_sf_keep
browse mode draw  bsb_keep_sum tot_keep_bsb_calib diff_bsb_keep
browse mode draw  scup_keep_sum tot_keep_scup_calib diff_scup_keep
browse

export delimited using "differences_total_catch_NJ.csv", replace 




*3) 
import excel using "calibration_output_NJ.xlsx", clear first 

split period2, parse(_)
rename period21 month
rename period22 day
rename period23 mode1

collapse (sum) estimated_trips, by(mode n_cal)
rename n_cal draw

tempfile calib
save `calib', replace 

cd "C:\Users\andrew.carr-harris\Desktop\Git\RDM 2024 testing"
import excel using "prob_star_estimates_CV_NJ.xlsx", clear first 
merge 1:1 draw mode using `calib'
drop _merge

order draw mode 
gen difference_ntrips = estimated_trips-round(ntrips)
gen difference_CV = CS_base-CS_alt

keep draw mode difference_ntrips difference_CV
export delimited using "differences_CV_trips_NJ.csv", replace 





*2)
/*
cd "C:\Users\andrew.carr-harris\Desktop\Git\RDM 2024 testing"
import excel using "calibration_output_NJ.xlsx", clear first 


split period2, parse(_)
rename period21 month
rename period22 day
rename period23 mode

collapse (sum) tot_keep_sf tot_rel_sf tot_keep_bsb tot_keep_scup tot_rel_bsb tot_rel_scup, by(month mode n_cal)
rename n_cal draw
destring month, replace 

gen tot_catch_sf=tot_keep_sf+tot_rel_sf

ds mode draw month, not
renvarlab `r(varlist)', postfix("_calib")


levelsof month, local(mnths)
tempfile cal 
save `cal', replace

global monthz
foreach m of local mnths{
	
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\RDM 2024 testing\size_data\fluke_prob_star_2022.csv", clear 
/*
egen sum_fitted_prob_harvest=sum(fitted_prob) if length>=17, by(draw mode)
replace fitted_prob=fitted_prob/sum_fitted_prob_harvest if length>=17

egen sum_fitted_prob_release=sum(fitted_prob) if length<17, by(draw mode)
replace fitted_prob=fitted_prob/sum_fitted_prob_release if length<17
*/
gen month=`m'

tempfile monthz`m'
save `monthz`m'', replace
global monthz "$monthz "`monthz`m''" " 

	
}

dsconcat $monthz

merge m:1 month mode draw using `cal'
drop _merge

sort draw mode month length

replace tot_catch_sf_calib=tot_catch_sf_calib*fitted
collapse (sum) tot_catch_sf_calib, by(length draw mode month)
sort draw mode month length

replace tot_keep_sf_calib=fitted*tot_keep_sf_calib if length>=17
replace tot_keep_sf_calib = 0 if length<17
replace tot_rel_sf_calib=fitted*tot_rel_sf_calib if length<17

collapse (sum) tot_keep_sf_calib  tot_rel_sf_calib, by(length draw mode month)
sort draw mode month length


collapse (sum) tot_keep_sf_calib  tot_rel_sf_calib, by(draw mode)
*/
