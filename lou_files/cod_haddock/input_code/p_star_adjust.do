



global files 
forv i = 4/10{

local vars fh pr sh
foreach v of local vars{

import delimited using  "$age_pro_cd/pstar_`v'_`i'.csv", clear  

tempfile files`v'`i'
save `files`v'`i'', replace
global files "$files "`files`v'`i''" " 
}	
}

dsconcat $files



gen harv_diff2=tot_keep_model-harvest_mrip
gen perc_har=((tot_keep_model-harvest_mrip)/harvest_mrip)*100
browse if perc_har>2 & harv_diff2>500

levelsof run  if perc_har>2 & harv_diff2>500, sep(,)
drop if inlist(run, `r(levels)') 


drop v1

sort species mode month run

bysort species mode month: gen draw=_n
order run draw
keep if draw<=100

replace species="cod" if species=="COD"
replace species="hadd" if species=="HAD"

tostring month, gen(month2)
gen domain2=mode+"_"+ species+"_"+month2

tempfile pstars
save `pstars', replace

global drawz
levelsof species, local(specs)
foreach s of local specs{
	
	u `pstars', clear 
	keep if species=="`s'"
	
	tempfile pstars2
	save `pstars2', replace 
	
	levelsof month, local(mnths)
	foreach m of local mnths{
		
		u `pstars2', clear
		keep if month==`m'
		
		tempfile pstars3
		save `pstars3', replace
		
		levelsof mode, local(mds)
		foreach md of local mds{
			
			use `pstars3', clear
			keep if mode=="`md'"
			
			tempfile pstars4
			save `pstars4', replace 
			
			levelsof run, local(drws)
			foreach d of local drws{
				
			u `pstars4', clear
			keep if run==`d'
			su  p_star 
			local p_star=`r(mean)'
			
			u  "$age_pro_cd/rec_selectivity_CaL.dta", clear  
			gen mode="pr"

			expand 2, gen(dup)
			replace mode="fh" if dup==1
			drop dup

			expand 2 if mode=="fh", gen(dup)
			replace mode="sh" if dup==1
			drop dup 
			
			keep if species=="`s'" & month==`m' & mode=="`md'"
			
			levelsof species
			if "`s'"=="cod"{
			gen p_star=`p_star' if length== 21.5 
			}
			
			else{
			}
			
			if "`s'"=="hadd"{
			gen p_star=`p_star' if length== 16.5 
			} 
			
			else{
			}
			
			gen below=.
			gen above=.

			su length if p_star!=. 
			replace below=1 if length<= `r(max)' 
			replace above=1 if length> `r(max)' 

			egen sum_below=sum(fitted_prob), by( below)
			replace sum_below=. if below==.

			egen sum_above=sum(fitted_prob), by( above)
			replace sum_above=. if above==.

			sort length
			gen first=1 if _n==1
			gen cdf_star=fitted_prob if first==1
			egen pstar_all= sum(p_star)
			
			gen prob_below_adj=fitted_prob/sum_below if below==1 
			gen prob_above_adj=fitted_prob/sum_above if above==1 
			mvencode prob_below_adj prob_above_adj, mv(0) overr

			gen f_l=prob_below_adj *pstar_all if below==1 
			replace f_l=prob_above_adj *(1-pstar_all) if above==1  
			
			gen run_number=`d'
			
			tempfile drawz`s'`m'`md'`d'
			save `drawz`s'`m'`md'`d'', replace
			global drawz "$drawz "`drawz`s'`m'`md'`d''" " 

}
}
}
}
	
clear
dsconcat $drawz



/*
twoway 	(scatter fitted_prob length if draw==1 & species=="cod" & mode=="pr" & month==9, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter f_l length  if draw==1 & species=="cod" & mode=="pr" & month==9, connect(direct) lcol(gray)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) 
				
twoway 	(scatter fitted_prob length if draw==1 & species=="hadd" & mode=="pr" & month==9, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter f_l length  if draw==1 & species=="hadd" & mode=="pr" & month==9, connect(direct) lcol(gray)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) 
				
twoway 	(scatter fitted_prob length if draw==1 & species=="hadd" & mode=="pr" & month==6, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter f_l length  if draw==1 & species=="hadd" & mode=="pr" & month==6, connect(direct) lcol(gray)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) 
*/			

keep length fitted_prob species month  observed_prob  mode f_l run_number p_star 
order species mode month length	

tempfile base 
save `base', replace 

*Now that I've adjusted the (fitted) catch-at-length probabilty distributions based on the p-star values to create f(l), I compute recreational selectivity. 
*a) I merge f(l) to estimates of total catch and create catch-at-length distributions. These distributions cover the calibration FY (ex. May 1, 2022- April 31, 2023 )
*b) I merge catch-at-length distributions to the population numbers-at-length distribution on Jan. 1 of the nearest calendar year (ex. Jan. 1, 2022)


import delimited using  "$draw_file_cd\simulated_catch_totals_month.csv", clear  
keep mode month tot_cod_catch tot_hadd_catch draw
rename draw run_number 
reshape long tot_, i(mode month run) j(new) string
rename new species
rename tot_ tot_catch
replace species="cod" if species=="cod_catch"
replace species="hadd" if species=="hadd_catch"

tostring run, gen(run2)
gen domain=mode+"_"+ species+"_"+run2
encode domain, gen(domain2)
xtset domain2 month 
tsfill, full
mvencode tot, mv(0) override
decode domain2, gen(domain3)
split domain3, pars(_)
replace mode=domain31
replace species=domain32
destring domain33, replace
replace run_number=domain33
drop domain*
drop run2

merge 1:m mode month species run using `base'
drop if _merge==1 
sort mode month run species length

// browse if mode=="fh" & month==4 & species=="cod"
gen nfish_catch_fl=tot_catch*f_l

drop _merge 

save "$input_code_cd\catch_at_length_pstar.dta",  replace 
u "$input_code_cd\catch_at_length_pstar.dta", clear 

