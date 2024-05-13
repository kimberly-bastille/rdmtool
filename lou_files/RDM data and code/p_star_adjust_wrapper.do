



*First run the files that adjust the 2022 catch-at-length distribution based on the p-star values
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_MA.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_RI.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_CT.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_NY.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_NJ.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_DE.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_MD.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_VA.do"
do "C:\Users\andrew.carr-harris\Desktop\RDM data and code\prob_star draws_NC.do"




			 
**compute recreational selectivity 
*import the numbers at age, use age-length key to convert to inches, then merge back to catch-at-length 
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
/*
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
*/
save "fluke_age_length_conversion.dta", replace



**Here, pull in the Numbers-at-age data for the baseline year to compute baseline recreational selectivity. 
**Translate numbers-at-age to numbers-at-length using the (smoothed) conversion key above 
**We have one distribution of catch-at-length and have 1,000 draws of numbers-at-length.
**Compute rec. selectivity (q) for every draw of numbers-at-length and take the median value. 


cd "Z:\size data 2023 RDM"
import excel using "Fluke_N_at_age_2022.xlsx", clear first 
drop if Iter==.
sample 100, count
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )

graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 fluke numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 
replace nfish=nfish*1000
tabstat nfish, stat(sum) by(i) format(%12.0gc)

tempfile new
save `new', replace
levelsof iter, local(iters)
global sizes

foreach i of local iters{
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

/*
replace n_fish_population=n_fish_population/1000
graph box n_fish_population , over(length) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
egen draw=group(iteration)
keep iter length draw n_fish
collapse (sum) n_fish_population, by(iter length draw)
tempfile pop2022
save `pop2022', replace 


***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length

*pull in all the fluke 2022 p_star data 
cd "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data"
u "fluke_prob_star_2022_MA.dta", clear 
append using "fluke_prob_star_2022_RI.dta"
append using "fluke_prob_star_2022_CT.dta"
append using "fluke_prob_star_2022_NY.dta"
append using "fluke_prob_star_2022_NJ.dta"
append using "fluke_prob_star_2022_DE.dta"
append using "fluke_prob_star_2022_MD.dta"
append using "fluke_prob_star_2022_VA.dta"
append using "fluke_prob_star_2022_NC.dta"

replace length=31.5 if length>=31.5
replace length=6.5 if length<=6.5

collapse (sum) fitted, by(length state year mode draw)
sort state mode draw length 


preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_MA_test1.csv", clear  
keep if state=="MA"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_MA
save `totcat_MA', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_RI_test1.csv", clear  
keep if state=="RI"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_RI
save `totcat_RI', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_CT_test1.csv", clear  
keep if state=="CT"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_CT
save `totcat_CT', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_NY_test1.csv", clear  
keep if state=="NY"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_NY
save `totcat_NY', replace 
restore

preserve 
 u "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_NJ_test1_adj.dta", clear  
keep if state=="NJ"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_NJ
save `totcat_NJ', replace 
restore


preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_DE_test1.csv", clear  
keep if state=="DE"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_DE
save `totcat_DE', replace 
restore


preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_MD_test1.csv", clear  
keep if state=="MD"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_MD
save `totcat_MD', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_VA_test1.csv", clear  
keep if state=="VA"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_VA
save `totcat_VA', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_NC_test1.csv", clear  
keep if state=="NC"
keep if species=="SF"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_NC
save `totcat_NC', replace 
restore


preserve
u `totcat_MA', clear 
append using  `totcat_RI'
append using  `totcat_CT'
append using  `totcat_NY'
append using  `totcat_NJ'
append using  `totcat_DE'
append using  `totcat_MD'
append using  `totcat_VA'
append using  `totcat_NC'
tempfile totcat
save `totcat', replace 
restore


merge m:1 state mode draw using `totcat'

gen catch_at_length=catch*fitted 
drop _merge 

merge m:1  length draw using `pop2022'

gen rec_selec=catch_at_length/n_fish
drop _merge

tempfile selectivity
save `selectivity', replace //these are 2022 rec. selectivities
*now merge these to the 2024 population numbers 



cd "Z:\size data 2023 RDM"
import excel using "Fluke_N_at_age_2024.xlsx", clear first 
drop if Iter==.
sample 100, count
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )

graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 fluke numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 
tabstat nfish, stat(sum) by(i) format(%12.0gc)

tempfile new
save `new', replace
levelsof iter, local(iters)
global sizes

foreach i of local iters{
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


/*
replace n_fish_population=n_fish_population/1000
graph box n_fish_population , over(length) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
egen draw=group(iteration)
keep iter length draw n_fish
collapse (sum) n_fish_population, by(iter length draw)
rename iter iteration_2024
rename n_fish_population n_fish_population_2024

merge 1:m length draw using `selectivity'
drop _merge

gen catch_at_length_24=n_fish_population_2024*rec_selec

egen sum_catch_at_length_22=sum(catch_at_length), by(mode draw state)
egen sum_catch_at_length_24=sum(catch_at_length_24), by(mode draw state)
sort state mode draw length

gen fitted_prob_24=catch_at_length_24/sum_catch_at_length_24
replace fitted_prob_24=fitted_prob if fitted_prob_24==.
egen sum_prob=sum(fitted_prob_24), by(mode draw state)


keep iteration_2024 length draw n_fish_population_2024 state mode fitted_prob_24 state
rename fitted fitted_prob

save "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\fluke_projected_catch_at_lengths.dta", replace 
u "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\fluke_projected_catch_at_lengths.dta", clear 
export delimited "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\fluke_projected_catch_at_lengths.csv", replace 


**********************Black sea bass
*there is no stock data for black sea bass, so the adjusted p_stars from 2022 are used for 2024

u "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_MA.dta", clear
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_RI.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_CT.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_NY.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_NJ.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_DE.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_MD.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_VA.dta"
append using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_prob_star_2022_NC.dta"


keep  length draw state mode fitted_prob
save "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_projected_catch_at_lengths_all_states.dta", replace 
u "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_projected_catch_at_lengths_all_states.dta", clear 
export delimited "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\bsb_projected_catch_at_lengths_all_states.csv", replace 

**********************Scup
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
/*
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
*/


save "scup_age_length_conversion.dta", replace





cd "Z:\size data 2023 RDM"
import excel using "Scup_N_at_age_2022.xlsx", clear first 
drop if Iter==.
sample 100, count
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )

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

levelsof iter, local(iters)
global sizes
foreach i of local iters{
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


/*
replace n_fish_population=n_fish_population/1000
graph box n_fish_population , over(length, label(angle(45) labsize(vsmall))) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
  
egen draw=group(iteration)
keep iter length draw n_fish
collapse (sum) n_fish_population, by(iter length draw)
tempfile pop2022
save `pop2022', replace 




***Now compute C_l/N_l = q_l for each region and draw of numbers-at-length

*pull in all the fluke 2022 p_star data 
cd "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data"
u "scup_prob_star_2022_MA.dta", clear 
append using "scup_prob_star_2022_RI.dta"
append using "scup_prob_star_2022_CT.dta"
append using "scup_prob_star_2022_NY.dta"
append using "scup_prob_star_2022_NJ.dta"
append using "scup_prob_star_2022_DE.dta"
append using "scup_prob_star_2022_MD.dta"
append using "scup_prob_star_2022_VA.dta"
append using "scup_prob_star_2022_NC.dta"


collapse (sum) fitted, by(length state year mode draw)
sort state mode draw length 


preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_MA_test1.csv", clear  
keep if state=="MA"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_MA
save `totcat_MA', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_RI_test1.csv", clear  
keep if state=="RI"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_RI
save `totcat_RI', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_CT_test1.csv", clear  
keep if state=="CT"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_CT
save `totcat_CT', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_NY_test1.csv", clear  
keep if state=="NY"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_NY
save `totcat_NY', replace 
restore

preserve 
 u "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_NJ_test1_adj.dta", clear  
keep if state=="NJ"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_NJ
save `totcat_NJ', replace 
restore


preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_DE_test1.csv", clear  
keep if state=="DE"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_DE
save `totcat_DE', replace 
restore


preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_MD_test1.csv", clear  
keep if state=="MD"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_MD
save `totcat_MD', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_VA_test1.csv", clear  
keep if state=="VA"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_VA
save `totcat_VA', replace 
restore

preserve 
import delimited using "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\pstar_NC_test1.csv", clear  
keep if state=="NC"
keep if species=="SCUP"
keep run mode catch_mrip state
rename run_number draw 
tempfile totcat_NC
save `totcat_NC', replace 
restore


preserve
u `totcat_MA', clear 
append using  `totcat_RI'
append using  `totcat_CT'
append using  `totcat_NY'
append using  `totcat_NJ'
append using  `totcat_DE'
append using  `totcat_MD'
append using  `totcat_VA'
append using  `totcat_NC'
tempfile totcat
save `totcat', replace 
restore


merge m:1 state mode draw using `totcat'

gen catch_at_length=catch*fitted 
drop _merge 

merge m:1  length draw using `pop2022'

gen rec_selec=catch_at_length/n_fish
drop if _merge==2
drop _merge

tempfile selectivity
save `selectivity', replace //these are 2022 rec. selectivities



**now merge to 2024 pop numbes
cd "Z:\size data 2023 RDM"
import excel using "Scup_N_at_age_2024.xlsx", clear first 
drop if Iter==.
sample 100, count
renvarlab, lower
reshape long age, i(iteration) j(new) string
rename age nfish
rename new age
destring age, replace

/*
vioplot nfish, over(age)  $graphoptions title("Density of Jan. 1 2022 numbers-at-age", size(medium)) ytitle("N_a (000's of fish)", size(small))  xtitle("Age", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  xlab(, labsize(vsmall) )

graph box nfish , over(age) $graphoptions title("Distribution of Jan. 1 2022 fluke numbers-at-age", size(medium)) ytitle("Numbers-at-age (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
 */
 

tempfile new
save `new', replace
levelsof iter, local(iters)
global sizes

foreach i of local iters{
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


/*
replace n_fish_population=n_fish_population/1000
graph box n_fish_population , over(length) $graphoptions title("Distribution of Jan. 1 2022 numbers-at-length", size(medium)) ytitle("Numbers-at-length (000's of fish)", size(small)) ///
 ylab(, angle(horizontal) labsize(vsmall) )  nooutsides 
*/
 
egen draw=group(iteration)
keep iter length draw n_fish
collapse (sum) n_fish_population, by(iter length draw)
rename iter iteration_2024
rename n_fish_population n_fish_population_2024

merge 1:m length draw using `selectivity'
drop if _merge==1
drop _merge

gen catch_at_length_24=n_fish_population_2024*rec_selec

egen sum_catch_at_length_22=sum(catch_at_length), by(mode draw state)
egen sum_catch_at_length_24=sum(catch_at_length_24), by(mode draw state)
sort state mode draw length

gen fitted_prob_24=catch_at_length_24/sum_catch_at_length_24
replace fitted_prob_24=fitted_prob if fitted_prob_24==.

egen sum_prob=sum(fitted_prob_24), by(mode draw state)


keep iteration_2024 length draw n_fish_population_2024 state mode fitted_prob_24 state
rename fitted fitted_prob

save "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\scup_projected_catch_at_lengths.dta", replace 
export delimited "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\scup_projected_catch_at_lengths.csv", replace 
u "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data\scup_projected_catch_at_lengths.dta", clear




