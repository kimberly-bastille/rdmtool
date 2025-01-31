
*Compare NAA/NAL for 2023, 2024, 2025
*Bottomtrawl survey data from 2021-2023 to form the age-length keys.


* NAA cod 
use "$input_data_cd/$historical_cod_NAA", clear 
keep if inlist(year, 2023, 2024) 
reshape long age, i(year) j(new)
rename age nfish
rename new age 
tempfile hist
save `hist', replace 

use "$input_data_cd/$projected_cod_NAA", clear 
keep if year==2025
collapse (mean) age*, by(year)
reshape long age, i(year) j(new)
rename age nfish
rename new age 
append using `hist'



twoway(scatter nfish age if year==2023, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter nfish age if year==2024, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter nfish age if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("cod numbers-at-age", size(small)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Age, size(small)) xlab(1(1)9, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2023" 2 "2024" 3 "2025") region(lcolor(none)) position(11) ring(0) rows(1) size(small)))

	
* CDF 
keep if year==2024
gen cumulative = .
levelsof year, local(years)

foreach yr of local years {
    gen temp = .
    replace temp = nfish if year == `yr'
    cumul temp, gen(cdf_`yr')
    drop temp
}			

cumul nfish if year==2024, gen(cdf_2024) 

twoway (line cdf_2023 age, sort lcolor(blue) lpattern(solid)) ///
       (line cdf_2024 age, sort lcolor(red) lpattern(dash)) ///
       (line cdf_2025 age, sort lcolor(green) lpattern(dot)), ///
       legend(order(1 "2023" 2 "2024" 3 "2025")) ///
       xlabel(, grid) ylabel(, grid) ///
       title("CDF Plot for Different Years") ///
       xtitle("Your Variable") ytitle("Cumulative Probability")
			
*****cod 
* for cod, there are few obs for age 7+
* combine these into 6+ category
**M-Y 2023 model:
	*Bottomtrawl survey data from 2021-2023 to form the age-length keys.

import excel using "$input_data_cd/fall_spring_cruises_12_3_24.xlsx", clear first
renvarlab, lower
tempfile cruises
sort year 
save `cruises', replace 

import excel using "$input_data_cd/cod_svspp_raw_12_3_24.xlsx", clear first
renvarlab, lower
rename count count 
merge m:1 cruise6 using `cruises'
keep if _merge==3
drop if age==0
*replace age=6 if age>=6
collapse (sum) count, by(year svspp age length)
destring year, replace
sort svspp year age length count

keep if year>=$trawl_svy_start_yr & year<=$trawl_svy_end_yr

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
di `min_svy_yr'

replace age=6 if age>=6
collapse (sum) count, by (age length)

tsset age length
tsfill, full

sort age length 
mvencode count, mv(0) override 


levelsof age, local(ages)
foreach a of local ages{
	
	*su length if age==`a' & count!=0
	*lowess count length if age==`a' & length>=`r(min)' & length<=`r(max)', adjust bwidth(.3) gen(s`a') nograph
	lowess count length if age==`a' , adjust bwidth(.3) gen(s`a') nograph

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
			ytitle("proportion of fish that are age-a", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cms, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames' 
graph export "$figure_cd/cod_prop_length_at_age.png", as(png) replace
*/

drop sum sum_raw
tempfile al_cod
save `al_cod', replace 

use "$input_data_cd/$historical_cod_NAA", clear 

egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
keep if year==2024
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
gen year=2024
tempfile nal2024
save `nal2024', replace 


use "$input_data_cd/$historical_cod_NAA", clear 
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
gen year=2023
tempfile nal2023
save `nal2023', replace 

use "$input_data_cd/$projected_cod_NAA", clear 
egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
keep if year==2025
collapse (mean) age*, by(year)
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_cod', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish
gen year=2025

append using `nal2023'
append using `nal2024'

replace length=length/2.54

collapse (sum) NaL_*, by(year length)
replace length=round(length)
collapse (sum) NaL_*, by(year length)

egen sum_NaL_raw=sum(NaL_from_raw_trawl), by(year)
gen prob_raw=NaL_from_raw_trawl/sum_NaL_raw

su prob_raw if year==2023 & length>=23
local sum23=round(`r(sum)'*100, .01)

su prob_raw if year==2024 & length>=23
local sum24=round(`r(sum)'*100, .01)

su prob_raw if year==2025 & length>=23
local sum25=round(`r(sum)'*100, .01)


twoway	(scatter NaL_from_raw_trawl length if year==2024, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Cod Length structure 2024", size(small)) xline(23, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 23 inches: `sum24'%" )) 
graph export "$figure_cd/cod_NaL_24.png", as(png) replace
			
twoway	(scatter NaL_from_raw_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Cod Length structure 2025", size(small)) xline(23, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 23 inches: `sum25'%" )) 		
graph export "$figure_cd/cod_NaL_25.png", as(png) replace
			

egen sum_NaL=sum(NaL_from_smooth_trawl), by(year)
gen prob=NaL_from_smooth_trawl/sum_NaL

su prob if year==2023 & length>=58.42
local sum23=round(`r(sum)'*100, .01)

su prob if year==2024 & length>=58.42
local sum24=round(`r(sum)'*100, .01)

su prob if year==2025 & length>=58.42
local sum25=round(`r(sum)'*100, .01)

twoway(scatter NaL_from_smooth_trawl length if year==2023, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_smooth_trawl length if year==2024, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_smooth_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("cod numbers-at-length", size(small)) xline(58.42, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2023" 2 "2024" 3 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 58.42 "2024 cod min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 23 inches:" "     2023: `sum23'%" "     2024: `sum24'%" "     2025: `sum25'%" ))
graph export "$figure_cd/cod_NaL.png", as(png) replace
			

egen sum_NaL_raw=sum(NaL_from_raw_trawl), by(year)
gen prob_raw=NaL_from_raw_trawl/sum_NaL_raw

su prob_raw if year==2023 & length>=58.42
local sum23=round(`r(sum)'*100, .01)

su prob_raw if year==2024 & length>=58.42
local sum24=round(`r(sum)'*100, .01)

su prob_raw if year==2025 & length>=58.42
local sum25=round(`r(sum)'*100, .01)
			
twoway(scatter NaL_from_raw_trawl length if year==2023, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_raw_trawl length if year==2024, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_raw_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("cod numbers-at-length, raw trawl survey age-length key", size(small)) xline(58.42, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2023" 2 "2024" 3 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 58.42 "2024 cod min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 23 inches:" "     2023: `sum23'%" "     2024: `sum24'%" "     2025: `sum25'%" ))
graph export "$figure_cd/cod_NaL_raw.png", as(png) replace



*****haddock 
import excel using "$input_data_cd/fall_spring_cruises_12_3_24.xlsx", clear first
renvarlab, lower
tempfile cruises
save `cruises', replace 

import excel using "$input_data_cd/haddock_svspp_raw_12_3_24.xlsx", clear first
renvarlab, lower
merge m:1 cruise6 using `cruises'
keep if _merge==3
drop if age==0
replace age=9 if age>=9
collapse (sum) count, by(year svspp age length)
destring year, replace
sort svspp year age length count

keep if year>=$trawl_svy_start_yr & year<=$trawl_svy_end_yr

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
replace age=9 if age>9
collapse (sum) count, by (age length)


tsset age length
tsfill, full

sort age length 
mvencode count, mv(0) override 


levelsof age, local(ages)
foreach a of local ages{
	*su length if age==`a' & count!=0
	*lowess count length if age==`a' & length>=`r(min)' & length<=`r(max)', adjust bwidth(.3) gen(s`a') nograph
	lowess count length if age==`a' , adjust bwidth(.3) gen(s`a') nograph

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
twoway(scatter prop_raw length if age==`a', connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i) ) ///
			(scatter prop_smoothed length if age==`a', connect(direct) lcol(blue) title("haddock age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr'", size(small)) ///
			ytitle("proportion of fish that are age-a", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cms, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i)  name(dom1`a', replace))
 local graphnames `graphnames' dom1`a'
}

grc1leg `graphnames' 
graph export "$figure_cd/hadd_prop_length_at_age.png", as(png) replace
*/


tempfile al_hadd
save `al_hadd', replace 

*historical data to compute rec selectivity
use "$input_data_cd/$historical_hadd_NAA", clear 
keep if year==2023
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish
collapse (sum) NaL*, by(length)

gen year=2023
tempfile nal2023
save `nal2023', replace 


use "$input_data_cd/$historical_hadd_NAA", clear 
keep if year==2024
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish
collapse (sum) NaL*, by(length)

gen year=2024
tempfile nal2024
save `nal2024', replace 


use "$input_data_cd/$projected_hadd_NAA", clear 
keep if year==2025
collapse (mean) age*, by(year)
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish
collapse (sum) NaL*, by(length)
gen year=2025

append using `nal2023'
append using `nal2024'

preserve
u "$input_data_cd/rec_selectivity_CaL_open_seasons_cm.dta", clear 
su length if domain=="cod_closed"
local cod_closed_min=`r(min)'
local cod_closed_max=`r(max)'

su length if domain=="cod_open"
local cod_open_min=`r(min)'
local cod_open_max=`r(max)'

su length if domain=="hadd_closed"
local hadd_closed_min=`r(min)'
local hadd_closed_max=`r(max)'

su length if domain=="hadd_open"
local hadd_open_min=`r(min)'
local hadd_open_max=`r(max)'
restore


keep if length>=18 & length <=77
			
replace length=round(length/2.54)

collapse (sum) NaL_*, by(year length)

egen sum_NaL_raw=sum(NaL_from_raw_trawl), by(year)
gen prob_raw=NaL_from_raw_trawl/sum_NaL_raw

su prob_raw if year==2023 & length>=18
local sum23=round(`r(sum)'*100, .01)

su prob_raw if year==2024 & length>=18
local sum24=round(`r(sum)'*100, .01)

su prob_raw if year==2025 & length>=18
local sum25=round(`r(sum)'*100, .01)


twoway	(scatter NaL_from_raw_trawl length if year==2024, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Haddock length structure 2024", size(small)) xline(18, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 18 inches: `sum24'%" )) 
*graph export "$figure_cd/hadd_NaL_24.png", as(png) replace
			
twoway	(scatter NaL_from_raw_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Haddock length structure 2025", size(small)) xline(18, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 18 inches: `sum25'%" )) 		
graph export "$figure_cd/hadd_NaL_25.png", as(png) replace


egen sum_NaL=sum(NaL_from_smooth_trawl), by(year)
gen prob=NaL_from_smooth_trawl/sum_NaL

su prob if year==2023 & length>=45.72
local sum23=round(`r(sum)'*100, .01)

su prob if year==2024 & length>=45.72
local sum24=round(`r(sum)'*100, .01)

su prob if year==2025 & length>=45.72
local sum25=round(`r(sum)'*100, .01)

twoway(scatter NaL_from_smooth_trawl length if year==2023, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_smooth_trawl length if year==2024, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_smooth_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("haddock numbers-at-length", size(small)) xline(45.72, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(500)3500, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2023" 2 "2024" 3 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 45.72 "2024 haddock min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 18 inches:" "     2023: `sum23'%" "     2024: `sum24'%" "     2025: `sum25'%" ))
graph export "$figure_cd/hadd_NaL.png", as(png) replace

egen sum_NaL_raw=sum(NaL_from_raw_trawl), by(year)
gen prob_raw=NaL_from_raw_trawl/sum_NaL_raw

su prob_raw if year==2023 & length>=45.72
local sum23=round(`r(sum)'*100, .01)

su prob_raw if year==2024 & length>=45.72
local sum24=round(`r(sum)'*100, .01)

su prob_raw if year==2025 & length>=45.72
local sum25=round(`r(sum)'*100, .01)

twoway(scatter NaL_from_raw_trawl length if year==2023, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_raw_trawl length if year==2024, connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter NaL_from_raw_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("haddock numbers-at-length, raw trawl survey age-length key", size(small)) xline(45.72, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(500)3500, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2023" 2 "2024" 3 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 45.72 "2024 haddock min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 18 inches:" "     2023: `sum23'%" "     2024: `sum24'%" "     2025: `sum25'%" ))
graph export "$figure_cd/hadd_NaL_raw.png", as(png) replace


*graph of total population numbers
use "$input_data_cd/$historical_hadd_NAA", clear 
egen total=rowtotal(age1-age9)
tempfile hist
save `hist', replace

use "$input_data_cd/$projected_hadd_NAA", clear 
egen total=rowtotal(age1-age9)
collapse (mean) age* total, by(year)
append using `hist'


tsset year
tsline total, xlabel(1977(1)2025, labsize(vsmall) angle(45) grid) ytitle("population numbers of fish ('000s)", size(small)) ///
			title("haddock population numbers", size(small)) ytick(, angle(horizontal) labsize(small))  ///
			ylabel(#20, labsize(vsmall) angle(horizontal))
graph export "$figure_cd/hadd_pop_number.png", as(png) replace


use "$input_data_cd/$historical_cod_NAA", clear 
egen total=rowtotal(age1-age9)
tempfile hist
save `hist', replace

use "$input_data_cd/$projected_cod_NAA", clear 
egen total=rowtotal(age1-age9)
collapse (mean) age* total, by(year)
append using `hist'


tsset year
tsline total, xlabel(1977(1)2025, labsize(vsmall) angle(45) grid) ytitle("population numbers of fish ('000s)", size(small)) ///
			title("cod population numbers", size(small)) ytick(, angle(horizontal) labsize(small))  ///
			ylabel(#20, labsize(vsmall) angle(horizontal))
graph export "$figure_cd/cod_pop_number.png", as(png) replace



*look at rec catch at length 2024/2025
import delimited using "$input_data_cd/projected_CaL_cod_hadd_cm.csv", clear
keep if draw<=100
mvencode proj*,mv(0) override
collapse (mean)  proj_cal_prob_smooth proj_cal_prob_raw, by(species season length)
gen year=2025
tempfile proj
save `proj', replace 

u "$input_data_cd/rec_selectivity_CaL_open_seasons_cm.dta", clear 
keep length fitted species season
gen year=2024
append using `proj'

gen prob=fitted if year==2024
replace prob=proj_cal_prob_smooth if year==2025

*replace observed=proj_cal_prob_raw if observed==.
drop proj_cal_prob_raw proj_cal_prob_smooth

*replace length=round(length/2.54)
collapse (sum) prob, by(length species season year )
gr drop _all

*su prob if species=="cod" & season=="open" & year==2024 & length>=45.72
su prob if species=="hadd" & season=="closed" & year==2024 & length>=45.72
local sum24=round(`r(sum)'*100, .01)

*su prob if species=="cod" & season=="open" & year==2025 & length>=45.72
su prob if species=="hadd" & season=="closed" & year==2025 & length>=45.72

local sum25=round(`r(sum)'*100, .01)

twoway(scatter prob length if species=="hadd" & season=="open" & year==2024, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter prob length if species=="hadd" & season=="open" & year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) ///
			title("haddock catch-at-length", size(small)) xline(45.72, lcol(gray)) ///
			ytitle("probability", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(.01).07, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2024" 2 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 45.72 "2024 haddock min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 18 inches:" "     2024: `sum24'%" "     2025: `sum25'%" ))


su fitted_prob if species=="hadd" & season=="open" & year==2024 & length>=18
local sum24=round(`r(sum)'*100, .01)

su fitted_prob if species=="hadd" & season=="open" & year==2025 & length>=18
local sum25=round(`r(sum)'*100, .01)

twoway(scatter fitted_prob length if species=="hadd" & season=="open" & year==2024, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter fitted_prob length if species=="hadd" & season=="open" & year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) ///
			title("haddock catch-at-length", size(small)) xline(18, lcol(gray)) ///
			ytitle("probability", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(.01).2, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2024" 2 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 18 "2024 haddock min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 18 inches:" "     2024: `sum24'%" "     2025: `sum25'%" ))			
			
			
su fitted_prob if species=="cod" & season=="closed" & year==2024 & length>=23
local sum24=round(`r(sum)'*100, .01)

su fitted_prob if species=="cod" & season=="closed" & year==2025 & length>=23
local sum25=round(`r(sum)'*100, .01)

twoway(scatter fitted_prob length if species=="cod" & season=="closed" & year==2024, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter fitted_prob length if species=="cod" & season=="closed" & year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) ///
			title("haddock catch-at-length", size(small)) xline(18, lcol(gray)) ///
			ytitle("probability", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(.01).2, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2024" 2 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 18 "2024 haddock min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 18 inches:" "     2024: `sum24'%" "     2025: `sum25'%" ))			
					
			
su fitted_prob if species=="hadd" & season=="open" & year==2024 & length>=43.18
local sum24=round(`r(sum)'*100, .01)

su fitted_prob if species=="hadd" & season=="open" & year==2025 & length>=43.18
local sum25=round(`r(sum)'*100, .01)

twoway(scatter fitted_prob length if species=="hadd" & season=="open" & year==2024, connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)) ///
			(scatter fitted_prob length if species=="hadd" & season=="open" & year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) ///
			title("haddock catch-at-length", size(small)) xline(43.18, lcol(gray)) ///
			ytitle("probability", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cm, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(.01).07, labsize(small) angle(horizontal)) xtick(#20, labsize(small)) lwidth(medthick)  lpat(solid) msymbol(i) ///
			legend(order(1 "2024" 2 "2025") region(lcolor(none)) position(12) ring(0) rows(1) size(small)) text(100 45.72 "2024 haddock min. size", size(small) place(e)) ///
			note("% fish equal to or longer than 18 inches:" "     2024: `sum24'%" "     2025: `sum25'%" ))			
			