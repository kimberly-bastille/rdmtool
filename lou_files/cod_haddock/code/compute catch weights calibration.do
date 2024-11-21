

u  "$input_data_cd/raw_props_at_length_ab1b2_season.dta", clear   //This file has raw proportions at length in the calibration period
drop if prop_ab1==. & prop_b2==.
sort species season l_cm_  

tempfile base 
save `base', replace


import delimited using "$input_data_cd\MRIP_catch_totals_month_calibration.csv", clear  

rename cod_keep_mrip  keep_cod
rename hadd_keep_mrip  keep_hadd
rename cod_rel_mrip  rel_cod
rename hadd_rel_mrip  rel_hadd
rename cod_catch_mrip  catch_cod
rename hadd_catch_mrip  catch_hadd

rename llcod_keep_mrip  llkeep_cod
rename llhadd_keep_mrip  llkeep_hadd
rename llcod_rel_mrip  llrel_cod
rename llhadd_rel_mrip  llrel_hadd
rename llcod_catch_mrip  llcatch_cod
rename llhadd_catch_mrip  llcatch_hadd

rename ulcod_keep_mrip  ulkeep_cod
rename ulhadd_keep_mrip  ulkeep_hadd
rename ulcod_rel_mrip  ulrel_cod
rename ulhadd_rel_mrip  ulrel_hadd
rename ulcod_catch_mrip  ulcatch_cod
rename ulhadd_catch_mrip  ulcatch_hadd

reshape long keep_  llkeep_ ulkeep_ ///
					rel_ llrel_ ulrel_ ///
					catch_ llcatch_  ulcatch_ , i( season) j(species) string

						
merge 1:m species season  using `base'
drop if _merge==2
drop  _merge
sort sp season  l_cm_

replace keep=keep*prop_ab1
replace rel=rel*prop_b2

replace llkeep=llkeep*prop_ab1
replace llrel=llrel*prop_b2

replace ulkeep=ulkeep*prop_ab1
replace ulrel=ulrel*prop_b2

replace catch=keep+rel
replace llcatch=llkeep+llrel
replace ulcatch=ulkeep+ulrel

gen length_cm=l_cm_

/*
cod_lw_a = 0.000005132
cod_lw_b = 3.1625
had_lw_a = 0.000009298
had_lw_b = 3.0205
*/

gen weight = 0.000005132*length_cm^3.1625 if species=="cod"
replace weight = 0.000009298*length_cm^3.0205 if species=="hadd"
replace weight = weight*2.20462262185, //convert to lbs

gen keep_wt=weight*keep
gen keep_wt_ll=weight*llkeep
gen keep_wt_ul=weight*ulkeep

gen rel_wt=weight*rel_
gen rel_wt_ll=weight*llrel
gen rel_wt_ul=weight*ulrel

gen cat_wt=keep_wt+rel_wt
gen cat_wt_ll=keep_wt_ll+rel_wt_ll
gen cat_wt_ul=keep_wt_ul+rel_wt_ul

gen spp2="had_lg" if species=="hadd"  & length_cm >  50
replace spp2="had_sm" if species=="hadd"  & length_cm <=   50
replace spp2="cod" if species=="cod"  

preserve
import delimited using  "$input_data_cd\Discard_Mortality.csv", clear 

gen season="JanJun" if inlist(month, 1, 2, 3, 4, 5, 6)
replace season="JulDec" if inlist(month, 7, 8, 9, 10, 11, 12)
drop month 
duplicates drop 
tempfile disc
save `disc', replace 
restore 

merge m:1 spp2 season using `disc'
drop if _merge==2
drop _merge

gen disc_wt = discard_mortality * rel_wt 
gen disc_wt_ll = discard_mortality * rel_wt_ll 
gen disc_wt_ul = discard_mortality * rel_wt_ul 

gen disc_num = discard_mortality * rel_
gen disc_num_ll = discard_mortality * llrel_
gen disc_num_ul = discard_mortality * ulrel_



ds catch_ llcatch_ ulcatch_ keep_ llkeep_ ulkeep_ rel_ llrel_ ulrel_
renvarlab `r(varlist)', postdrop(1)

collapse (sum)  catch llcatch ulcatch keep llkeep ulkeep rel llrel ulrel ///
	keep_wt keep_wt_ll keep_wt_ul rel_wt rel_wt_ll rel_wt_ul cat_wt cat_wt_ll ///
	cat_wt_ul disc_wt disc_wt_ll disc_wt_ul disc_num disc_num_ll disc_num_ul, by(species)

	     	
gen mortality_wt = disc_wt+keep_wt
gen mortality_wt_ll = disc_wt_ll+keep_wt_ll
gen mortality_wt_ul = disc_wt_ul+keep_wt_ul

gen mortality = disc_num+keep
gen mortality_ll = disc_num_ll+llkeep
gen mortality_ul = disc_num_ul+ulkeep


rename llcatch catch_ll
rename ulcatch catch_ul
rename llkeep keep_ll
rename ulkeep keep_ul
rename llrel rel_ll
rename ulrel rel_ul

local vars catch keep rel keep_wt rel_wt cat_wt disc_wt disc_num mortality_wt mortality
foreach v of local vars{
	rename `v' `v'_point

}  

ds species  *ll *ul, not
renvarlab `r(varlist)', prefix(point_)

ds species  *ll *ul, not
renvarlab `r(varlist)', postdrop(6)

ds species  point* *ul, not
renvarlab `r(varlist)', prefix(ll_)

ds species  point* *ul, not
renvarlab `r(varlist)', postdrop(3)

ds species  point* ll*, not
renvarlab `r(varlist)', prefix(ul_)

ds species  point* ll*, not
renvarlab `r(varlist)', postdrop(3)


keep species point_disc_wt ll_disc_wt ul_disc_wt point_disc_num ll_disc_num ul_disc_num
tempfile disc_mort
save `disc_mort', replace

/*
reshape long point_ ll_ ul_, i(species) j(disp) string
replace disp="catch_wt" if disp=="cat_wt"
gen source ="mrip"

drop if inlist(disp, "catch", "keep", "rel" )
append using `mrip_catch'
*/



*now compute catrch weights for keep, release, total catch at the annual level 
u  "$input_data_cd/raw_props_at_length_ab1b2_annual.dta", clear   //This file has raw proportions at length in the calibration period
drop if prop_ab1==. & prop_b2==.
sort species  l_cm_  

tempfile base 
save `base', replace


import delimited using "$input_data_cd\MRIP_catch_totals_annual_calibration.csv", clear  

rename cod_keep_mrip  keep_cod
rename hadd_keep_mrip  keep_hadd
rename cod_rel_mrip  rel_cod
rename hadd_rel_mrip  rel_hadd
rename cod_catch_mrip  catch_cod
rename hadd_catch_mrip  catch_hadd

rename llcod_keep_mrip  llkeep_cod
rename llhadd_keep_mrip  llkeep_hadd
rename llcod_rel_mrip  llrel_cod
rename llhadd_rel_mrip  llrel_hadd
rename llcod_catch_mrip  llcatch_cod
rename llhadd_catch_mrip  llcatch_hadd

rename ulcod_keep_mrip  ulkeep_cod
rename ulhadd_keep_mrip  ulkeep_hadd
rename ulcod_rel_mrip  ulrel_cod
rename ulhadd_rel_mrip  ulrel_hadd
rename ulcod_catch_mrip  ulcatch_cod
rename ulhadd_catch_mrip  ulcatch_hadd

reshape long keep_  llkeep_ ulkeep_ ///
					rel_ llrel_ ulrel_ ///
					catch_ llcatch_  ulcatch_ , i( tab) j(species) string
drop tab 
					
					
merge 1:m species   using `base'
drop if _merge==2
drop  _merge
sort sp   l_cm_

replace keep=keep*prop_ab1
replace rel=rel*prop_b2

replace llkeep=llkeep*prop_ab1
replace llrel=llrel*prop_b2

replace ulkeep=ulkeep*prop_ab1
replace ulrel=ulrel*prop_b2

replace catch=keep+rel
replace llcatch=llkeep+llrel
replace ulcatch=ulkeep+ulrel

gen length_cm=l_cm_

/*
cod_lw_a = 0.000005132
cod_lw_b = 3.1625
had_lw_a = 0.000009298
had_lw_b = 3.0205
*/

gen weight = 0.000005132*length_cm^3.1625 if species=="cod"
replace weight = 0.000009298*length_cm^3.0205 if species=="hadd"
replace weight = weight*2.20462262185, //convert to lbs

gen keep_wt=weight*keep
gen keep_wt_ll=weight*llkeep
gen keep_wt_ul=weight*ulkeep

gen rel_wt=weight*rel_
gen rel_wt_ll=weight*llrel
gen rel_wt_ul=weight*ulrel

gen cat_wt=keep_wt+rel_wt
gen cat_wt_ll=keep_wt_ll+rel_wt_ll
gen cat_wt_ul=keep_wt_ul+rel_wt_ul

ds catch_ llcatch_ ulcatch_ keep_ llkeep_ ulkeep_ rel_ llrel_ ulrel_
renvarlab `r(varlist)', postdrop(1)

collapse (sum)  keep_wt keep_wt_ll keep_wt_ul rel_wt rel_wt_ll rel_wt_ul cat_wt cat_wt_ll cat_wt_ul, by(species)
 
 
local vars    keep_wt rel_wt cat_wt    
foreach v of local vars{
	rename `v' `v'_point

}  

ds species  *ll *ul, not
renvarlab `r(varlist)', prefix(point_)

ds species  *ll *ul, not
renvarlab `r(varlist)', postdrop(6)

ds species  point* *ul, not
renvarlab `r(varlist)', prefix(ll_)

ds species  point* *ul, not
renvarlab `r(varlist)', postdrop(3)

ds species  point* ll*, not
renvarlab `r(varlist)', prefix(ul_)

ds species  point* ll*, not
renvarlab `r(varlist)', postdrop(3)

merge 1:1 species using `disc_mort'
drop _merge 
tempfile wts
save `wts', replace 

import delimited using "$input_data_cd\MRIP_catch_totals_annual_calibration.csv", clear  

rename cod_keep_mrip  keep_cod
rename hadd_keep_mrip  keep_hadd
rename cod_rel_mrip  rel_cod
rename hadd_rel_mrip  rel_hadd
rename cod_catch_mrip  catch_cod
rename hadd_catch_mrip  catch_hadd

rename llcod_keep_mrip  llkeep_cod
rename llhadd_keep_mrip  llkeep_hadd
rename llcod_rel_mrip  llrel_cod
rename llhadd_rel_mrip  llrel_hadd
rename llcod_catch_mrip  llcatch_cod
rename llhadd_catch_mrip  llcatch_hadd

rename ulcod_keep_mrip  ulkeep_cod
rename ulhadd_keep_mrip  ulkeep_hadd
rename ulcod_rel_mrip  ulrel_cod
rename ulhadd_rel_mrip  ulrel_hadd
rename ulcod_catch_mrip  ulcatch_cod
rename ulhadd_catch_mrip  ulcatch_hadd

reshape long keep_  llkeep_ ulkeep_ ///
					rel_ llrel_ ulrel_ ///
					catch_ llcatch_  ulcatch_ , i( tab) j(species) string
drop tab 



ds catch_ llcatch_ ulcatch_ keep_ llkeep_ ulkeep_ rel_ llrel_ ulrel_
renvarlab `r(varlist)', postdrop(1)



rename llcatch catch_ll
rename ulcatch catch_ul
rename llkeep keep_ll
rename ulkeep keep_ul
rename llrel rel_ll
rename ulrel rel_ul

local vars catch keep rel 
foreach v of local vars{
	rename `v' `v'_point

}  

ds species  *ll *ul, not
renvarlab `r(varlist)', prefix(point_)

ds species  *ll *ul, not
renvarlab `r(varlist)', postdrop(6)

ds species  point* *ul, not
renvarlab `r(varlist)', prefix(ll_)

ds species  point* *ul, not
renvarlab `r(varlist)', postdrop(3)

ds species  point* ll*, not
renvarlab `r(varlist)', prefix(ul_)

ds species  point* ll*, not
renvarlab `r(varlist)', postdrop(3)


merge 1:1 species using `wts'
drop _merge 

gen point_mortality_wt = point_keep_wt+point_disc_wt
gen ll_mortality_wt = ll_keep_wt+ll_disc_wt
gen ul_mortality_wt = ul_keep_wt+ul_disc_wt

gen point_mortality = point_keep+point_disc_num
gen ll_mortality = ll_keep+ll_disc_num
gen ul_mortality = ul_keep+ul_disc_num

reshape long point_ ll_ ul_, i(species) j(disp) string
gen source ="mrip"




tempfile mrip
save `mrip', replace 


*Import the calibration catch weights file 
import excel using "$input_data_cd\calibration_catch_weights_cm.xlsx", clear first 

renvarlab, lower

collapse (sum) total_number total_weight discmortality_total_weight discmortality_total_number, by(species keep_release  run)
collapse (mean) total_number total_weight discmortality_total_weight discmortality_total_number ///
			  (sd) sd_total_number=total_number sd_total_weight=total_weight sd_discmortality_total_weight=discmortality_total_weight ///
			  sd_discmortality_total_number=discmortality_total_number, by(species keep_release  )


local vars total_number total_weight discmortality_total_weight discmortality_total_number			
foreach v of local vars{
	gen `v'_ll=`v'-1.96*sd_`v'
	gen `v'_ul=`v'+1.96*sd_`v'

}  

			  
preserve
keep if keep_rel=="keep"
keep species   total_number total_number_ll total_number_ul
rename total_number keep
rename total_number_ll keep_ll
rename total_number_ul keep_ul
tempfile keep_model
save `keep_model', replace 
restore 

preserve
keep if keep_rel=="keep"
keep species   total_weight  total_weight_ll total_weight_ul
rename total_weight keep_wt
rename total_weight_ll keep_wt_ll
rename total_weight_ul keep_wt_ul
tempfile keep_model_wt
save `keep_model_wt', replace 
restore 

preserve
keep if keep_rel=="release"
keep species   total_number  total_number_ll total_number_ul
rename total_number rel
rename total_number_ll rel_ll
rename total_number_ul rel_ul
tempfile rel_model
save `rel_model', replace 
restore 

preserve
keep if keep_rel=="release"
keep species   total_weight total_weight_ll total_weight_ul
rename total_weight rel_wt
rename total_weight_ll rel_wt_ll
rename total_weight_ul rel_wt_ul
tempfile rel_wt_model
save `rel_wt_model', replace 
restore 


preserve
keep if keep_rel=="release"
keep species   discmortality_total_number discmortality_total_number_ll discmortality_total_number_ul
rename discmortality_total_number disc_num
rename discmortality_total_number_ll disc_num_ll
rename discmortality_total_number_ul disc_num_ul
tempfile discmortality_total_number
save `discmortality_total_number', replace 
restore 


keep if keep_rel=="release"
keep species   discmortality_total_weight discmortality_total_weight_ll discmortality_total_weight_ul
rename discmortality_total_weight disc_wt
rename discmortality_total_weight_ll disc_wt_ll 
rename discmortality_total_weight_ul disc_wt_ul 

merge 1:1 species  using `discmortality_total_number', keep(3) nogen
merge 1:1 species  using `rel_wt_model', keep(3) nogen
merge 1:1 species  using `rel_model', keep(3) nogen
merge 1:1 species  using `keep_model_wt', keep(3) nogen
merge 1:1 species  using `keep_model', keep(3) nogen


gen cat=keep+rel
gen cat_ul=keep_ul+rel_ul
gen cat_ll=keep_ll+rel_ll

gen cat_wt=keep_wt+rel_wt
gen cat_wt_ul=keep_wt_ul+rel_wt_ul
gen cat_wt_ll=keep_wt_ll+rel_wt_ll

gen mortality_wt = disc_wt+keep_wt
gen mortality_wt_ll = disc_wt_ll+keep_wt_ll
gen mortality_wt_ul = disc_wt_ul+keep_wt_ul

gen mortality = disc_num+keep
gen mortality_ll = disc_num_ll+keep_ll
gen mortality_ul = disc_num_ul+keep_ul


ds species  *ll *ul, not
renvarlab `r(varlist)', prefix(point_)

ds species  point* *ul, not
renvarlab `r(varlist)', prefix(ll_)

ds species  point* *ul, not
renvarlab `r(varlist)', postdrop(3)

ds species  point* ll*, not
renvarlab `r(varlist)', prefix(ul_)

ds species  point* ll*, not
renvarlab `r(varlist)', postdrop(3)

reshape long point_ ll_ ul_, i(species) j(disp) string

/*
gen season2="open" if season==1
replace season2="closed" if season==0

drop season
rename season2 season 
*/
replace species="hadd" if species=="had"
replace disp="catch" if disp=="cat"
replace disp="catch_wt" if disp=="cat_wt"
gen source ="model"

append using `mrip'

preserve
keep if species=="cod" & strmatch(disp, "*wt")==1


sort disp source
gen x = _n
replace x = x + 1 if x >= 3
replace x = x + 1 if x >= 6
replace x = x + 1 if x >= 9
replace x = x + 1 if x >= 12
twoway (scatter point_ x if source=="model",  msymbol(S) msize(medium) mcol(black)) ///
       (rcap ul_ ll_ x if source=="model" , lcol(black))	   ///
       (scatter point_ x if source=="mrip" , msymbol(S) msize(medium) mcol(gray)) ///
       (rcap ul_ ll_ x if source=="mrip" , lcol(gray) ///
	   	legend(order(1 "model" 3 "mrip" ) position(1) row(1) ring(0) ) ///
	   xlabel(none) xtitle(" ")  ylabel(-200000(500000)4050000, labsize(small) angle(45)) ///
	   ytitle("Pounds of fish", size(small) xoffset(-2)) ///
		text(-200000 1.5 "catch", place(c) size(small)) ///
	   text(-200000 4.5 "disc. mort", place(c) size(small)) ///
	   text(-200000 7.5 "harvest", place(c) size(small)) ///
	   text(-200000 10.5 "total. mort", place(c) size(small)) ///
	   text(-200000 13.5 "discards", place(c) size(small)) ///
	   title(Cod calibration statistics in pounds))
restore




preserve
keep if species=="cod" & strmatch(disp, "*wt")!=1
sort disp source
gen x = _n
replace x = x + 1 if x >= 3
replace x = x + 1 if x >= 6
replace x = x + 1 if x >= 9
replace x = x + 1 if x >= 12


twoway (scatter point_ x if source=="model",  msymbol(S) msize(medium) mcol(black)) ///
       (rcap ul_ ll_ x if source=="model" , lcol(black))	   ///
       (scatter point_ x if source=="mrip" , msymbol(S) msize(medium) mcol(gray)) ///
       (rcap ul_ ll_ x if source=="mrip" , lcol(gray) ///
	   	legend(order(1 "model" 3 "mrip" ) position(1) row(1) ring(0) ) ///
	   xlabel(none) xtitle(" ")  ylabel(-100000(100000)1100000, labsize(small) angle(45))  ///
	   ytitle("Numbers of fish", size(small) xoffset(-2)) ///
		text(-100000 1.5 "catch", place(c) size(small)) ///
	   text(-100000 4.5 "disc. mort", place(c) size(small)) ///
	   text(-100000 7.5 "harvest", place(c) size(small)) ///
	   text(-100000 10.5 "total. mort", place(c) size(small)) ///
	   text(-100000 13.5 "discards", place(c) size(small)) ///
	   title(Cod calibration statistics in numbers))
restore



preserve
keep if species=="hadd" & strmatch(disp, "*wt")==1


sort disp source
gen x = _n
replace x = x + 1 if x >= 3
replace x = x + 1 if x >= 6
replace x = x + 1 if x >= 9
replace x = x + 1 if x >= 12
twoway (scatter point_ x if source=="model",  msymbol(S) msize(medium) mcol(black)) ///
       (rcap ul_ ll_ x if source=="model" , lcol(black))	   ///
       (scatter point_ x if source=="mrip" , msymbol(S) msize(medium) mcol(gray)) ///
       (rcap ul_ ll_ x if source=="mrip" , lcol(gray) ///
	   	legend(order(1 "model" 3 "mrip" ) position(1) row(1) ring(0) ) ///
	   xlabel(none) xtitle(" ")  ylabel(-200000(500000)3000000, labsize(small) angle(45)) ///
	   ytitle("Pounds of fish", size(small) xoffset(-2)) ///
		text(-200000 1.5 "catch", place(c) size(small)) ///
	   text(-200000 4.5 "disc. mort", place(c) size(small)) ///
	   text(-200000 7.5 "harvest", place(c) size(small)) ///
	   text(-200000 10.5 "total. mort", place(c) size(small)) ///
	   text(-200000 13.5 "discards", place(c) size(small)) ///
	   title(Haddock calibration statistics in pounds))
restore




preserve
keep if species=="hadd" & strmatch(disp, "*wt")!=1
sort disp source
gen x = _n
replace x = x + 1 if x >= 3
replace x = x + 1 if x >= 6
replace x = x + 1 if x >= 9
replace x = x + 1 if x >= 12


twoway (scatter point_ x if source=="model",  msymbol(S) msize(medium) mcol(black)) ///
       (rcap ul_ ll_ x if source=="model" , lcol(black))	   ///
       (scatter point_ x if source=="mrip" , msymbol(S) msize(medium) mcol(gray)) ///
       (rcap ul_ ll_ x if source=="mrip" , lcol(gray) ///
	   	legend(order(1 "model" 3 "mrip" ) position(1) row(1) ring(0) ) ///
	   xlabel(none) xtitle(" ")  ylabel(-100000(500000)2000000, labsize(small) angle(45))  ///
	   ytitle("Numbers of fish", size(small) xoffset(-2)) ///
		text(-100000 1.5 "catch", place(c) size(small)) ///
	   text(-100000 4.5 "disc. mort", place(c) size(small)) ///
	   text(-100000 7.5 "harvest", place(c) size(small)) ///
	   text(-100000 10.5 "total. mort", place(c) size(small)) ///
	   text(-100000 13.5 "discards", place(c) size(small)) ///
	   title(Haddock calibration statistics in numbers))
restore


sort species disp source