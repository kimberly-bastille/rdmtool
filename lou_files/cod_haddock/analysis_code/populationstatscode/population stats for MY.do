*global input_data_cd "C:\Users\min-yang.lee\Desktop\populationstatscode\"


global input_data_cod "\\nefscfile\BLAST\READ-SSB-Lee-BLAST\cod_haddock_fy2025\source_data\cod\output\2025-01-03"
global input_data_haddock "\\nefscfile\BLAST\READ-SSB-Lee-BLAST\cod_haddock_fy2025\source_data\haddock\output\2025-01-03"
global figure_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\figures"
global input_data_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\input_data"

*global figure_cd $input_data_cd

global trawl_svy_start_yr 2019 
global trawl_svy_end_yr 2024

global historical_cod_NAA  "WGOM_Cod_historical_NAA_2024Assessment.dta"
global historical_hadd_NAA "GOM_Haddock_historical_NAA_2024Assessment.dta"

/*OLD 
global projected_cod_NAA "WGOM_Cod_projected_NAA_2024Assessment.dta"
global projected_hadd_NAA  "GOM_Haddock_projected_NAA_2024Assessment.dta"
*/		
global projected_cod_NAA "WGOM_Cod_historical_NAA_2024Assessment.dta"
global projected_hadd_NAA  "GOM_Haddock_historical_NAA_2024Assessment.dta"

		
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

use "$input_data_cod/$historical_cod_NAA", clear 

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

gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length)
gen year=2024
tempfile nal2024
save `nal2024', replace 


use "$input_data_cod/$historical_cod_NAA", clear 
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

gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length)
gen year=2023
tempfile nal2023
save `nal2023', replace 

use "$input_data_cod/$projected_cod_NAA", clear 
egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
keep if year==2025
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 

merge 1:m age using `al_cod', keep(3) nogen 
sort  age length

gen NaL_from_smooth_trawl = prop_smoothed*nfish
gen year=2025

append using `nal2023'
append using `nal2024'

replace length=round(length/2.54)

collapse (sum) NaL_*, by(year length)

egen sum_NaL_from_smooth_trawl=sum(NaL_from_smooth_trawl), by(year)
gen prob=NaL_from_smooth_trawl/sum_NaL_from_smooth_trawl

su prob if year==2023 & length>=23
local sum23=round(`r(sum)'*100, .01)

su prob if year==2024 & length>=23
local sum24=round(`r(sum)'*100, .01)




twoway	(scatter NaL_from_smooth_trawl length if year==2024, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Cod Length structure 2024", size(small)) xline(23, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(100)400, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 23 inches: `sum24'%" )) 
*graph export "$figure_cd/cod_NaL_24.png", as(png) replace

su prob if year==2025 & length>=23
local sum25=round(`r(sum)'*100, .01)
			
twoway	(scatter NaL_from_smooth_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Cod Length structure 2025", size(small)) xline(23, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(100)400, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 23 inches: `sum25'%" )) 		
*graph export "$figure_cd/cod_NaL_25.png", as(png) replace
			

			
			
			
			
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
use "$input_data_haddock/$historical_hadd_NAA", clear 
keep if year==2023
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_smooth_trawl = prop_smoothed*nfish
collapse (sum) NaL*, by(length)

gen year=2023
tempfile nal2023
save `nal2023', replace 



use "$input_data_haddock/$historical_hadd_NAA", clear 
keep if year==2024
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_smooth_trawl = prop_smoothed*nfish
collapse (sum) NaL*, by(length)

gen year=2024
tempfile nal2024
save `nal2024', replace 


use "$input_data_haddock/$projected_hadd_NAA", clear 
keep if year==2025
collapse (mean) age*, by(year)
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_smooth_trawl = prop_smoothed*nfish
collapse (sum) NaL*, by(length)
gen year=2025

append using `nal2023'
append using `nal2024'

			
replace length=round(length/2.54)
collapse (sum) NaL_*, by(year length)

egen sum_NaL_from_smooth_trawl=sum(NaL_from_smooth_trawl), by(year)
gen prob=NaL_from_smooth_trawl/sum_NaL_from_smooth_trawl


su prob if year==2024 & length>=18
local sum24=round(`r(sum)'*100, .01)

twoway	(scatter NaL_from_smooth_trawl length if year==2024, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Haddock length structure 2024", size(small)) xline(18, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(1000)4000, labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 18 inches: `sum24'%" )) 
*graph export "$figure_cd/hadd_NaL_24.png", as(png) replace

su prob if year==2025 & length>=18
local sum25=round(`r(sum)'*100, .01)
			
twoway	(scatter NaL_from_smooth_trawl length if year==2025, connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
			title("Haddock length structure 2025", size(small)) xline(18, lcol(gray)) ///
			ytitle("numbers of fish ('000s)", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(Length inches, size(small)) xlab(#20, labsize(small)) ///
			ylab(0(1000)4000,  labsize(small) angle(horizontal)) xtick(#20, labsize(small))  lpat(solid) msymbol(i) ///
			note("% fish equal to or longer than 18 inches: `sum25'%" )) 		
graph export "$figure_cd/hadd_NaL_25.png", as(png) replace




***Time series of total population numbers 
use "$input_data_haddock/$historical_hadd_NAA", clear 
egen nfish=rowtotal(age1-age9)
gen spec="hadd"
tempfile hadd
save `hadd', replace 



use "$input_data_cod/$historical_cod_NAA", clear 
egen nfish=rowtotal(age1-age9)
gen spec="cod"
append using `hadd'
encode spec, gen(species)

xtset species year
xtline nfish, tlabel(#50, labsize(vsmall) angle(45) grid) overlay

*reformat as wide
use "$input_data_haddock/$historical_hadd_NAA", clear 
egen nfish=rowtotal(age1-age9)
ds year, not
renvarlab `r(varlist)', postfix(_hadd)
tempfile hadd
save `hadd', replace 


use "$input_data_cod/$historical_cod_NAA", clear 
egen nfish=rowtotal(age1-age9)
ds year, not
renvarlab `r(varlist)', postfix(_cod)
merge 1:1 year using `hadd', nogen
drop age*
drop if nfish_cod==.
save "Z:\cod and haddock\sublegal harvest\cod_hadd_population_TS.dta", replace 

