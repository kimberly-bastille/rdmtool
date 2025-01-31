
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
/*
local vars Value_minus1 Value_minus5  Value_minus9  Value_min_plus1 Value_min_plus2 Value_min_minus2 Value_close_hadd
foreach v of local vars {
	
	gen perc_diff_`v'=((`v'-Value)/Value)*100
}
*/


***********
* The _v2 output datasets do not implement a cap on the number of cod that may be voluntarily released 
* The _v3 output datasets DO implement a cap on the number of cod that may be voluntarily released 
* The _v4 output datasets

*Total mortality
global regs

		
local regs 	SQ cod_min_minus1 cod_min_plus1 cod_min_minus2 cod_min_plus2 cod_bag_minus1 cod_bag_plus1 hadd_bag_minus1 ///
		hadd_bag_plus1 hadd_bag_minus3 hadd_bag_plus3 hadd_bag_minus5 hadd_bag_plus5 hadd_bag_minus9 hadd_bag_plus9 hadd_min_minus1 ///
		hadd_min_plus1  hadd_min_minus2
		
local regs 	SQ 
foreach r of local regs{
	
import excel using "`r'_v4.xlsx", clear first
keep if number=="Weight"
drop if catch=="release"
*keep if run<=10
collapse (sum) Value, by(Category catch run)
replace Value=Value/2205
collapse (sum) Value, by(Category run)
*egen lb = pctile(Value), p(2.5) by(Cat )
*egen ub = pctile(Value), p(97.5) by(Cat )
*collapse (median) Value (mean) lb ub, by(Category )
gen reg="`r'"

tempfile regs`r'
save `regs`r'', replace
global regs "$regs "`regs`r''" " 

}

dsconcat $regs
sort Cat  reg run
*encode reg, gen(reg_num)
gen catch_disposition="total_mort"
tempfile tot
save `tot', replace 


/*
twoway ///
    scatter Value reg_num if reg_num == 1 & Cat=="cod", legend(off) || ///
    rcap lb ub reg_num if reg_num == 1 & Cat=="cod", legend(off) || ///
    scatter Value reg_num if reg_num == 2 & Cat=="cod",  legend(off) || ///
    rcap lb ub reg_num if reg_num == 2 & Cat=="cod", legend(off) || ///
    scatter Value reg_num if reg_num == 3 & Cat=="cod", legend(off) || ///
    rcap lb ub  reg_num if reg_num == 3 & Cat=="cod", ///
    legend(order(1 "Group 1" 2 "Group 2" 3 "Group 3"))
*/
	
*Mortality by disposition
global regs2

/*local regs 	SQ cod_min_minus1 cod_min_plus1 cod_min_minus2 cod_min_plus2 cod_bag_minus1 cod_bag_plus1 hadd_bag_minus1 ///
		hadd_bag_plus1 hadd_bag_minus3 hadd_bag_plus3 hadd_bag_minus5 hadd_bag_plus5 hadd_bag_minus9 hadd_bag_plus9 */
foreach r of local regs{
	
import excel using "`r'_v4.xlsx", clear first
keep if number=="Weight"
drop if catch=="release"
*keep if run<=10
collapse (sum) Value, by(Category catch run)
replace Value=Value/2205
*egen lb = pctile(Value), p(2.5) by(Cat catch)
*egen ub = pctile(Value), p(97.5) by(Cat catch)
*collapse (median) Value (mean) lb ub, by(Category catch)

gen reg="`r'"

tempfile regs2`r'
save `regs2`r'', replace
global regs2 "$regs2 "`regs2`r''" " 

}
dsconcat $regs2
append using `tot'
sort Cat catch reg run
gen domain=Cat+"-"+catch+"-"+ reg

egen lb_med = pctile(Value), p(2.5) by(Cat catch reg)
egen ub_med = pctile(Value), p(97.5) by(Cat catch reg)
egen med = pctile(Value), p(50) by(Cat catch reg)
collapse (mean) Value lb_med ub_med med (sd) sd=Value (semean) se=Value, by(Category catch reg domain)
gen ub=Value+1.96*sd
gen lb=Value-1.96*sd
gen lb_semean = Value - invttail(e(df_r), 0.025) * se
gen ub_semean = Value + invttail(e(df_r), 0.025) * se


keep if Cat=="cod"
drop if strmatch(reg, "*hadd*")==1
encode reg, gen(reg_num)

* cod keep and disc. mortality 
gr drop _all
levelsof catch if Cat=="cod" & catch!="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local max = `r(max)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ"
local sq=round(`r(mean)')
local sq_place=`sq'+5


twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(horizontal) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 
	   

* cod total mortality 
gr drop _all
levelsof catch if Cat=="cod" & catch=="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local max = `r(max)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ"
local sq=round(`r(mean)')
local sq_place=`sq'+5


twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(horizontal) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   

	   
keep if Cat=="had"
drop if strmatch(reg, "*cod*")==1
encode reg, gen(reg_num)

* hadd keep and disc. mortality 
gr drop _all
levelsof catch if Cat=="had" & catch!="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local max= `r(max)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ"
local sq=round(`r(mean)')
local sq_place=`sq'+20

twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small))  ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   
	

* haddock total mortality 
gr drop _all
levelsof catch if Cat=="had" & catch=="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local max= `r(max)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ"
local sq=round(`r(mean)')
local sq_place=`sq'+20
twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   


* Check calibration statistics
import excel using "calibration_stats.xlsx", clear first
collapse (sum) tot* estimated n_, by(draw)

local vars tot_keep_cod_model tot_keep_hadd_model tot_rel_cod_model tot_rel_hadd_model tot_cat_hadd_model tot_cat_cod_model estimated_trips n_choice_occasions
foreach v of local vars{
egen lb_med_`v' = pctile(`v'), p(2.5) 
egen ub_med_`v'= pctile(`v'), p(97.5) 
egen med_`v' = pctile(`v'), p(50) 
}
collapse (mean) med* lb* ub* 






cod_bag_plus4   cod_open_aug

         cod_min_minus3   cod_min_minus5   hadd_min_plus2 

***
* Compare v2 to v3 regs
local regs 	SQ cod_min_minus1  cod_min_plus1 cod_min_minus2 cod_min_plus2 cod_bag_minus1 cod_bag_plus1 ///
                        hadd_bag_minus1 ///
                   hadd_bag_plus1 hadd_bag_minus3 hadd_bag_plus3 hadd_bag_minus5 hadd_bag_plus5 hadd_bag_minus9 ///
                   hadd_bag_plus9 hadd_min_minus1 hadd_min_plus1 hadd_min_minus2
				   
				   
*local regs 	SQ cod_min_minus1  cod_min_plus1  cod_min_minus2 cod_min_plus2 cod_bag_minus1 cod_bag_plus1 cod_bag_plus2 cod_bag_plus4 cod_open_aug
				   
global regs				   
foreach r of local regs{
/*
import excel using "`r'_v2.xlsx", clear first
gen source="_v2"
tempfile v2t
save `v2t', replace 

import excel using "`r'_v4.xlsx", clear first
gen source="_v4"
tempfile v4t
save `v4t', replace 
*/
import excel using "`r'_v3.xlsx", clear first
gen source="_v3"
*append using `v2t' 
*append using `v4t' 

keep if number=="Weight"
drop if catch=="release"
*keep if run<=10
collapse (sum) Value, by(Category catch run source)
replace Value=Value/2205
collapse (sum) Value, by(Category run source)
*egen lb = pctile(Value), p(2.5) by(Cat )
*egen ub = pctile(Value), p(97.5) by(Cat )
*collapse (median) Value (mean) lb ub, by(Category )

gen reg="`r'"+source

tempfile regs`r'
save `regs`r'', replace
global regs "$regs "`regs`r''" " 

}

dsconcat $regs
sort Cat  reg run
*encode reg, gen(reg_num)
gen catch_disposition="total_mort"
tempfile tot
save `tot', replace 


/*
twoway ///
    scatter Value reg_num if reg_num == 1 & Cat=="cod", legend(off) || ///
    rcap lb ub reg_num if reg_num == 1 & Cat=="cod", legend(off) || ///
    scatter Value reg_num if reg_num == 2 & Cat=="cod",  legend(off) || ///
    rcap lb ub reg_num if reg_num == 2 & Cat=="cod", legend(off) || ///
    scatter Value reg_num if reg_num == 3 & Cat=="cod", legend(off) || ///
    rcap lb ub  reg_num if reg_num == 3 & Cat=="cod", ///
    legend(order(1 "Group 1" 2 "Group 2" 3 "Group 3"))
*/
	
*Mortality by disposition
global regs2

/*local regs 	SQ cod_min_minus1 cod_min_plus1 cod_min_minus2 cod_min_plus2 cod_bag_minus1 cod_bag_plus1 hadd_bag_minus1 ///
		hadd_bag_plus1 hadd_bag_minus3 hadd_bag_plus3 hadd_bag_minus5 hadd_bag_plus5 hadd_bag_minus9 hadd_bag_plus9 */
foreach r of local regs{

/*
import excel using "`r'_v2.xlsx", clear first
gen source="_v2"
tempfile v2d
save `v2d', replace 

import excel using "`r'_v4.xlsx", clear first
gen source="_v4"
tempfile v4d
save `v4d', replace 
*/


import excel using "`r'_v3.xlsx", clear first
gen source="_v3"
*append using `v2d' 
*append using `v4d' 

keep if number=="Weight"
drop if catch=="release"
*keep if run<=10
collapse (sum) Value, by(Category catch run source)
replace Value=Value/2205
*egen lb = pctile(Value), p(2.5) by(Cat catch)
*egen ub = pctile(Value), p(97.5) by(Cat catch)
*collapse (median) Value (mean) lb ub, by(Category catch)

gen reg="`r'"+source

tempfile regs2`r'
save `regs2`r'', replace
global regs2 "$regs2 "`regs2`r''" " 

}
dsconcat $regs2
append using `tot'
sort Cat catch reg run
gen domain=Cat+"-"+catch+"-"+ reg

egen lb_med = pctile(Value), p(2.5) by(Cat catch reg)
egen ub_med = pctile(Value), p(97.5) by(Cat catch reg)
egen med = pctile(Value), p(50) by(Cat catch reg)
collapse (mean) Value lb_med ub_med med (sd) sd=Value (semean) se=Value, by(Category catch reg domain)
gen ub=Value+1.96*sd
gen lb=Value-1.96*sd
gen lb_semean = Value - invttail(e(df_r), 0.025) * se
gen ub_semean = Value + invttail(e(df_r), 0.025) * se


keep if Cat=="cod"
drop if strmatch(reg, "*hadd*")==1
*drop if strmatch(reg, "*v2*")==1
encode reg, gen(reg_num)

* cod keep and disc. mortality 
gr drop _all
levelsof catch if Cat=="cod" & catch!="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local max = `r(max)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v3"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v3"
local sq=round(`r(mean)')
local sq_place=`sq'+5


twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 
	   

* cod total mortality 
gr drop _all
levelsof catch if Cat=="cod" & catch=="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local max = `r(max)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v3"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v3"
local sq=round(`r(mean)')
local sq_place=`sq'+5


twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   

	   
keep if Cat=="had"
drop if strmatch(reg, "*cod*")==1
drop if strmatch(reg, "*v2*")==1
encode reg, gen(reg_num)

* hadd keep and disc. mortality 
gr drop _all
levelsof catch if Cat=="had" & catch!="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local max= `r(max)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ_v3"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ_v3"
local sq=round(`r(mean)')
local sq_place=`sq'+20

twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small))  ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   
	

* haddock total mortality 
gr drop _all
levelsof catch if Cat=="had" & catch=="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local max= `r(max)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ_v3"
local sq=round(`r(mean)')
local sq_place=`sq'+20
twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   





* Compare v4
local regs 	SQ cod_min_minus1  cod_min_plus1  cod_min_minus2 cod_min_plus2 cod_bag_plus1 cod_bag_plus2 cod_bag_minus1 cod_open_aug cod_min_minus3 cod_min_minus5 cod_bag_plus2_min_minus2
global regs				   
foreach r of local regs{
/*	
import excel using "`r'_v2.xlsx", clear first
gen source="_v2"
tempfile v2t
save `v2t', replace 
*/
import excel using "`r'_v4.xlsx", clear first
gen source="_v4"
*append using `v2t' 

keep if number=="Weight"
drop if catch=="release"
keep if run<=30
collapse (sum) Value, by(Category catch run source)
replace Value=Value/2205
collapse (sum) Value, by(Category run source)
*egen lb = pctile(Value), p(2.5) by(Cat )
*egen ub = pctile(Value), p(97.5) by(Cat )
*collapse (median) Value (mean) lb ub, by(Category )

gen reg="`r'"+source

tempfile regs`r'
save `regs`r'', replace
global regs "$regs "`regs`r''" " 

}

dsconcat $regs
sort Cat  reg run
*encode reg, gen(reg_num)
gen catch_disposition="total_mort"
tempfile tot
save `tot', replace 


/*
twoway ///
    scatter Value reg_num if reg_num == 1 & Cat=="cod", legend(off) || ///
    rcap lb ub reg_num if reg_num == 1 & Cat=="cod", legend(off) || ///
    scatter Value reg_num if reg_num == 2 & Cat=="cod",  legend(off) || ///
    rcap lb ub reg_num if reg_num == 2 & Cat=="cod", legend(off) || ///
    scatter Value reg_num if reg_num == 3 & Cat=="cod", legend(off) || ///
    rcap lb ub  reg_num if reg_num == 3 & Cat=="cod", ///
    legend(order(1 "Group 1" 2 "Group 2" 3 "Group 3"))
*/
	
*Mortality by disposition
global regs2

/*local regs 	SQ cod_min_minus1 cod_min_plus1 cod_min_minus2 cod_min_plus2 cod_bag_minus1 cod_bag_plus1 hadd_bag_minus1 ///
		hadd_bag_plus1 hadd_bag_minus3 hadd_bag_plus3 hadd_bag_minus5 hadd_bag_plus5 hadd_bag_minus9 hadd_bag_plus9 */
foreach r of local regs{

/*
import excel using "`r'_v2.xlsx", clear first
gen source="_v2"
tempfile v2d
save `v2d', replace 
*/
import excel using "`r'_v4.xlsx", clear first
gen source="_v4"
*append using `v2d' 

keep if number=="Weight"
drop if catch=="release"
keep if run<=30
collapse (sum) Value, by(Category catch run source)
replace Value=Value/2205
*egen lb = pctile(Value), p(2.5) by(Cat catch)
*egen ub = pctile(Value), p(97.5) by(Cat catch)
*collapse (median) Value (mean) lb ub, by(Category catch)

gen reg="`r'"+source

tempfile regs2`r'
save `regs2`r'', replace
global regs2 "$regs2 "`regs2`r''" " 

}
dsconcat $regs2
append using `tot'
sort Cat catch reg run
gen domain=Cat+"-"+catch+"-"+ reg

egen lb_med = pctile(Value), p(2.5) by(Cat catch reg)
egen ub_med = pctile(Value), p(97.5) by(Cat catch reg)
egen med = pctile(Value), p(50) by(Cat catch reg)
collapse (mean) Value lb_med ub_med med (sd) sd=Value (semean) se=Value, by(Category catch reg domain)
gen ub=Value+1.96*sd
gen lb=Value-1.96*sd
gen lb_semean = Value - invttail(e(df_r), 0.025) * se
gen ub_semean = Value + invttail(e(df_r), 0.025) * se

keep if strmatch(reg, "*SQ*")==1

keep if Cat=="cod"
drop if strmatch(reg, "*hadd*")==1
drop if strmatch(reg, "*v2*")==1
encode reg, gen(reg_num)

* cod keep and disc. mortality 
gr drop _all
levelsof catch if Cat=="cod" & catch!="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local max = `r(max)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v4"
local sq=`r(mean)'




twoway (rcap lb ub reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter Value reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 
	   

* cod total mortality 
gr drop _all
levelsof catch if Cat=="cod" & catch=="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1
local max = `r(max)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v4"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="cod" & strmatch(reg, "*hadd*")!=1 & reg=="SQ_v4"
local sq=round(`r(mean)')
local sq_place=`sq'+5


twoway (rcap lb ub reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter Value reg_num  if catch=="`d'" & Cat=="cod" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   

	   
keep if Cat=="had"
drop if strmatch(reg, "*cod*")==1
encode reg, gen(reg_num)

* hadd keep and disc. mortality 
gr drop _all
levelsof catch if Cat=="had" & catch!="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local max= `r(max)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ_v4"
local sq=`r(mean)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ_v4"
local sq=round(`r(mean)')
local sq_place=`sq'+20

twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small))  ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	   
	

* haddock total mortality 
gr drop _all
levelsof catch if Cat=="had" & catch=="total_mort", local(domains)
foreach d of local domains{
	
distinct reg if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local ndist=`r(ndistinct)'

su reg_num if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1
local max= `r(max)'

su med if catch=="`d'" & Cat=="had" & strmatch(reg, "*cod*")!=1 & reg=="SQ_v3"
local sq=round(`r(mean)')
local sq_place=`sq'+20
twoway (rcap lb_med ub_med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',  vertical lcolor(blue)) ///
       (scatter med reg_num  if catch=="`d'" & Cat=="had" & reg_num<=`max',   msymbol(circle) msize(vsmall) mcolor(blue) ///
	   ylabel(#10, angle(horizontal) labsize(small)) xlabel(#`ndist', valuelabel angle(45) labsize(vsmall)) name(dom_`d', replace) ///
	    yline(`sq') xtitle("", size(medium)) text(`sq_place' 1 "SQ=`sq'", place(e) size(small)) ///
       ytitle("`d' (mt)", size(medium)) legend(off)) 	  
	   
 local graphnames `graphnames' dom_`d'
}

gr combine `graphnames', cols(1) 	




*comapre catch per trip: mrip calibration period, model calibration period, model projection period    


**differences by season and mode 
import delimited using "$input_data_cd\MRIP_catch_totals_open_season.csv", clear 
merge 1:1 mode season using "$input_data_cd\MRIP_dtrip_totals_open_season.dta"
order mode season _ dtrip
mvencode dtrip, mv(0) override
drop _merge
rename dtrip dtrip_mrip
tempfile mrip 
save `mrip', replace 

collapse (sum) cod_catch_mrip hadd_catch_mrip dtrip_mrip 
gen cod_catch_trip_mrip=cod_catch_mrip/dtrip_mrip
gen hadd_catch_trip_mrip=hadd_catch_mrip/dtrip_mrip


import excel using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data\calibration_stats.xlsx", clear first
collapse (sum) tot_cat_hadd_model tot_cat_cod_model estimated, by(draw)
gen cod_catch_trip_calib=tot_cat_cod_model/estimated
gen hadd_catch_trip_calib=tot_cat_hadd_model/estimated
collapse (mean) cod_catch_trip_calib hadd_catch_trip_calib


import excel using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data\SQ_v2.xlsx", clear first

keep if (inlist(catch, "keep", "release") & number=="Number") | Cat=="ntrips"
replace catch="catch" if inlist(catch, "keep", "release")
replace catch="ntrip" if Cat=="ntrips"

collapse (sum) Value, by(Cat catch run)
collapse (mean) Value, by(Cat catch)

di "cod catch per trip=" 218591.2147888/152503.9221298
di "hadd catch per trip=" 1196109.040676/152503.9221298


collapse (mean) cod_catch_trip_calib hadd_catch_trip_calib




***Compare monthly output with Scott's numbers 
import excel using "SQ_v4_month.xlsx", clear first
keep if number=="Number"
drop if cat=="Discmortality"

destring month, replace
gen wave = 1 if inlist(month, 1, 2)
replace wave = 2 if inlist(month, 3, 4)
replace wave = 3 if inlist(month, 5, 6)
replace wave = 4 if inlist(month, 7, 8)
replace wave = 5 if inlist(month, 9, 10)
replace wave = 6 if inlist(month, 11, 12)
keep if catch!=""

preserve 
collapse (sum) Value, by(Cat mode wave run)
collapse (mean) Value, by(Cat mode wave)
gen catch_disposition="catch"
tempfile cat
save `cat', replace 
restore 

collapse (sum) Value, by(Cat mode wave catch  run)
collapse (mean) Value, by(Cat mode wave catch )
*replace Value=Value/2205 if number=="Weight"
append using `cat'
sort Cat mode cat wave   

tempfile lou
save `lou', replace 

import excel using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\FY24_WGOM_Cod_Haddock_Catch2.xlsx", clear first
drop F G H I J
collapse (sum) Value, by(wave mode Cat catch)
merge 1:1 Cat wave mode catch using  `lou'

gen diff=(Value-Value_scott)
gen perc_diff=((Value-Value_scott)/Value_scott)*100
sort diff

gen abs_diff=abs(diff)
sort abs_diff
browse if catch=="catch"



import excel using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\FY24_WGOM_Cod_Haddock_Catch3.xlsx", clear first
gen mode="fh" if inlist(MODE_FX, "4", "5")
replace mode="pr" if MODE_FX=="7"
replace mode="sh" if MODE_FX=="3"


collapse (sum) tot_cat landing RELEASE CLAIM HARVEST, by(YEAR WAVE mode my_common)
sort my mode YEAR WAVE






****New SQ predictions 12/31/2024
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first

keep if number=="Weight"
drop if catch=="release"
sort Cat mode run catch month 
collapse (sum) Value, by(Category catch run)
replace Value=Value/2205
collapse (sum) Value, by(Category run)

gen catch_disposition="total_mort"
gen ACL=99 if Cat=="cod"
replace ACL= 1075 if Cat=="had"
gen n_over=1 if Value>ACL
tabstat n_over, stat(sum) by(Cat)
tempfile tot
save `tot', replace 

	
*Mortality by disposition
import excel using "SQ_month.xlsx", clear first

keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value, by(Category catch run )
replace Value=Value/2205

append using `tot'
sort Cat catch  run
gen reg="SQ"
gen domain=Cat+"-"+catch+"-"+ reg

egen lb_med = pctile(Value), p(2.5) by(Cat catch reg)
egen ub_med = pctile(Value), p(97.5) by(Cat catch reg)
egen med = pctile(Value), p(50) by(Cat catch reg)
collapse (mean) Value lb_med ub_med med (sd) sd=Value (semean) se=Value, by(Category catch reg domain)
gen ub=Value+1.96*sd
gen lb=Value-1.96*sd
gen lb_semean = Value - invttail(e(df_r), 0.025) * se
gen ub_semean = Value + invttail(e(df_r), 0.025) * se
order Cat cat reg domain med lb_med ub_med


*numbers
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "model_predictions.xlsx", clear first

keep if number=="Number"
drop if catch=="Discmortality"
collapse (sum) Value, by(Category catch run)
collapse (sum) Value, by(Category run)

gen catch_disposition="total_catch"
tempfile tot
save `tot', replace 

	
*Mortality by disposition
import excel using "model_predictions.xlsx", clear first

keep if number=="Number"
drop if catch=="Discmortality"
collapse (sum) Value, by(Category catch run )

append using `tot'
sort Cat catch  run
gen reg="SQ"
gen domain=Cat+"-"+catch+"-"+ reg

egen lb_med = pctile(Value), p(2.5) by(Cat catch reg)
egen ub_med = pctile(Value), p(97.5) by(Cat catch reg)
egen med = pctile(Value), p(50) by(Cat catch reg)
collapse (mean) Value lb_med ub_med med (sd) sd=Value (semean) se=Value, by(Category catch reg domain)
gen ub=Value+1.96*sd
gen lb=Value-1.96*sd
gen lb_semean = Value - invttail(e(df_r), 0.025) * se
gen ub_semean = Value + invttail(e(df_r), 0.025) * se




****trips
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first

keep if number=="Ntrips"
sort Cat mode run catch month 
collapse (sum) Value, by(Category run)
egen med = pctile(Value), p(50) 
egen lb_med = pctile(Value), p(2.5) 
egen ub_med = pctile(Value), p(97.5) 



gen catch_disposition="total_mort"
gen ACL=99 if Cat=="cod"
replace ACL= 1075 if Cat=="had"
gen n_over=1 if Value>ACL
tabstat n_over, stat(sum) by(Cat)
tempfile tot
save `tot', replace 

	
*Mortality by disposition
import excel using "SQ_month.xlsx", clear first

keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value, by(Category catch run )
replace Value=Value/2205

append using `tot'
sort Cat catch  run
gen reg="SQ"
gen domain=Cat+"-"+catch+"-"+ reg

egen lb_med = pctile(Value), p(2.5) by(Cat catch reg)
egen ub_med = pctile(Value), p(97.5) by(Cat catch reg)
egen med = pctile(Value), p(50) by(Cat catch reg)
collapse (mean) Value lb_med ub_med med (sd) sd=Value (semean) se=Value, by(Category catch reg domain)
gen ub=Value+1.96*sd
gen lb=Value-1.96*sd
gen lb_semean = Value - invttail(e(df_r), 0.025) * se
gen ub_semean = Value + invttail(e(df_r), 0.025) * se
order Cat cat reg domain med lb_med ub_med