
import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_cm.xlsx", clear first 
renvarlab, lower
rename value value_lou
tempfile lou
save `lou', replace 


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\pred_test_KB_works.csv", clear 

drop v1 option 
rename draw_out run 
rename value value_kim
replace catch_d ="" if catch_d=="NA"
replace param="" if param=="NA"
merge 1:1 category mode catch_disposition param number_weight season run mrip_index using `lou'

sort mrip_index run
gen diff=value_kim-value_l
gen perc_diff= ((value_kim-value_l)/value_l)*100
sort perc

drop _merge




import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_SQ_20250103_193458_KB.csv", clear varname(1)
drop if mode=="NA"
destring value, replace
destring draw, replace

keep if number=="Weight"
drop if catch=="release"
collapse (sum) value, by(category catch draw_out )
replace value=value/2205
collapse (sum) value, by(category draw_out)
egen med = pctile(value), p(50) by(category)

egen lb_med = pctile(value), p(2.5) by(category)
egen ub_med = pctile(value), p(97.5) by(category)


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_SQ_20250103_193458_KB.csv", clear varname(1)
keep if mode=="NA"
sort categ

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_SQ_20250103_193458_KB.csv", clear varname(1)
keep if cate=="CV"
destring value, replace
destring draw, replace
collapse (sum) value, by(catego draw_out)
tempfile kim
save `kim', replace

cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first
keep if Cate=="CV"
renvarlab , lower
collapse (sum) value, by(run)
rename run draw_out
rename value value_lou
merge 1:1 draw_ou using `kim'


**compare 1st 8 
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "open_aug_draw100.xlsx", clear first
*keep if run==113
*keep if mrip_index==1

*keep if run<=3
renvarlab, lower
collapse (sum) value, by(category mode mode catch_disposition param number_weight season run mrip_index)
tempfile lou
save `lou', replace 


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_KB1_20250111_191143.csv", clear varname(1)
drop if mode=="NA"
destring value, replace
destring draw, replace
destring season, replace
destring mrip_index, replace

drop option
replace catch="" if catch=="NA"
replace param="" if param=="NA"
rename draw_ run
rename value value_kim 
merge 1:1 category mode mode catch_disposition param number_weight season run mrip_index using `lou'

gen diff=value_kim-value
gen perc_diff=((value_kim-value)/value)*100
sort perc_diff
order category mode mode catch_disposition param number_weight season run mrip_index value_kim value  diff perc_diff
export excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/open_aug_differences.xlsx",replace  


sort category mode mode catch_disposition param number_weight season draw_out mrip_index
collapse (sum) value, by(run)
rename run draw_out
rename value value_lou
merge 1:1 draw_ou using `kim'


destring value, replace
destring draw, replace

collapse (sum) value, by(catego draw_out)
keep if number=="Ntrips"
sort Cat mode run catch month 
collapse (sum) Value, by(Category run)
egen med = pctile(Value), p(50) 
egen lb_med = pctile(Value), p(2.5) 
egen ub_med = pctile(Value), p(97.5) 


cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first


collapse (sum) Value, by(Category mode catch_disposition param number_weight season run mrip_index)

cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
export excel using "SQ_final.xlsx", replace firstrow(variables)

import excel using "SQ_final.xlsx", clear first
keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value , by(Cat cat run)
gen diff=Value- Value_base
replace  Value=Value/2205
egen med = pctile(Value), p(50) 
egen lb_med = pctile(Value), p(2.5) 
egen ub_med = pctile(Value), p(97.5) 

replace  Value_base=Value_base/2205
collapse (sum) Value Value_base, by(Cat  run)

cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first
keep if run==1
rename Value Value_base

tempfile base 
save `base', replace 

import excel using "open_aug_draw1.xlsx", clear first
ds Value, not
merge 1:1 `r(varlist)' using `base'

gen diff=Value- Value_base
keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value Value_base, by(Cat month run)
gen diff=Value- Value_base
replace  Value=Value/2205
replace  Value_base=Value_base/2205
collapse (sum) Value Value_base, by(Cat  run)


cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first
keep if run==1
rename Value Value_base

tempfile base 
save `base', replace 

import excel using "open_aug_draw100.xlsx", clear first
ds Value, not
merge 1:1 `r(varlist)' using `base'
keep if run==1

keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value Value_base, by(Cat catch month run)
sort Cat catch run mon
replace  Value=Value/2205
replace  Value_base=Value_base/2205
gen diff=Value- Value_base

collapse (sum) Value Value_base diff, by(Cat catch run)
collapse (sum) Value Value_base diff, by(Cat run)
egen med = pctile(Value) if Cat, p(50) 
egen lb_med = pctile(Value), p(2.5) 
egen ub_med = pctile(Value), p(97.5) 




cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first
keep if run==1
keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value, by(Cat    run )
replace  Value=Value/2205



cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\input_data"
import delimited using "directed_trips_calib_150draws_cm.csv", clear 
keep if draw==1
keep if mode=="fh"

gen date=mdy(month1, day1, year_y)
format date %td
gen doy=doy(date)
distinct(doy)
distinct(date)

gen doy2=doy-121 if doy>=122
replace doy2=doy+244 if doy2==.


dtrips2<- dat %>% #dtrips_cm %>%
  dplyr::mutate(datetest = lubridate::as_date(paste0(year, "-", month1, "-", day1)),
                prep_doy =as.numeric(format(datetest, "%j")),
                doy =  ifelse(prep_doy >= 122,  # May 1st is day 121 in normal year
                              prep_doy - 121,     # Subtract days before May 1st
                              prep_doy + 244))
							  
							  
							  
cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\output_data"
import excel using "SQ_month.xlsx", clear first
keep if run<=6
rename Value Value_base

tempfile base 
save `base', replace 

import excel using "open_aug_cod20_draw5.xlsx", clear first
ds Value, not
merge 1:1 `r(varlist)' using `base'
keep if run<=6


keep if number=="Weight"
drop if catch=="release"
collapse (sum) Value Value_base, by(Cat   run)
replace  Value=Value/2205
replace  Value_base=Value_base/2205
gen diff=Value- Value_base



*Robin Frede's combined runs 

/*
MJP8 and MJP9 – cod season May 1-31, Sept 1-Oct 31, April 15-30, haddock 15 fish limit

MJP4 and MJP10 - cod season May 1-31, Sept 1-Oct 31, April 15-30, haddock 20 fish limit
*/
*MJP8 and MJP9 – cod season May 1-31, Sept 1-Oct 31, April 15-30, haddock 15 fish limit

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_MJP8_20250117_220429.csv", clear varnames(1)
drop if inlist(cate, "nchoiceoccasions", "ntrips", "CV")
browse if inlist(category, "Cod1_FH_Season", "Cod2_FH_Season", "Cod1_PR_Season", "Cod2_PR_Season")
tempfile a 
save `a', replace
/*
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_MJP9_20250117_230546.csv", clear varnames(1)
drop if inlist(cate, "had", "nchoiceoccasions", "ntrips", "CV")
browse if inlist(category, "Cod1_FH_Season", "Cod2_FH_Season", "Cod1_PR_Season", "Cod2_PR_Season")

append using `a'
*/
drop if mode=="NA"
destring value, replace
destring draw, replace
destring season, replace
destring mrip_index, replace

drop option
replace catch="" if catch=="NA"
replace param="" if param=="NA"
replace catch_disposition="CV" if cate=="CV"
replace catch_disposition="ntrips" if cate=="ntrips"

collapse (sum) value, by(category catch number draw)
drop if categ=="nchoiceoccasions"

*total mortality 
preserve 
keep if inlist(catch_disposition, "keep", "Discmortality") & number=="Weight"
collapse (sum) value, by(category number draw)
gen catch_disposition="total_mortality"
tempfile c
save `c', replace 
restore

append using `c'

replace  value=value/2205 if number_weight=="Weight"
gen under_acl=1 if catch_disposition=="total_mortality" & categ=="had" & value<1075
replace under_acl=1 if catch_disposition=="total_mortality" & categ=="cod" & value<99

egen med = pctile(value), by(cate catch number) p(50) 
egen lb_med = pctile(value), by(cate catch number)  p(5) 
egen ub_med = pctile(value),  by(cate catch number)  p(95) 
egen sum_acl=sum(under_acl) , by(cate catch number)

keep cate catch number med lb_med ub_med sum
duplicates drop 

*MJP4 and MJP10 - cod season May 1-31, Sept 1-Oct 31, April 15-30, haddock 20 fish limit
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_MJP4_20250116_220219.csv", clear varnames(1)
drop if inlist(cate, "nchoiceoccasions", "ntrips", "CV")
browse if inlist(category, "Cod1_FH_Season", "Cod2_FH_Season", "Cod1_PR_Season", "Cod2_PR_Season")
tempfile a 
save `a', replace
/*
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\analysis_code\output_MJP10_20250119_224713.csv", clear varnames(1)
drop if inlist(cate, "had", "nchoiceoccasions", "ntrips", "CV")
browse if inlist(category, "Cod1_FH_Season", "Cod2_FH_Season", "Cod1_PR_Season", "Cod2_PR_Season")

append using `a'
*/
drop if mode=="NA"
destring value, replace
destring draw, replace
destring season, replace
destring mrip_index, replace

drop option
replace catch="" if catch=="NA"
replace param="" if param=="NA"
replace catch_disposition="CV" if cate=="CV"
replace catch_disposition="ntrips" if cate=="ntrips"

collapse (sum) value, by(category catch number draw)
drop if categ=="nchoiceoccasions"

*total mortality 
preserve 
keep if inlist(catch_disposition, "keep", "Discmortality") & number=="Weight"
collapse (sum) value, by(category number draw)
gen catch_disposition="total_mortality"
tempfile c
save `c', replace 
restore

append using `c'

replace  value=value/2205 if number_weight=="Weight"
gen under_acl=1 if catch_disposition=="total_mortality" & categ=="had" & value<1075
replace under_acl=1 if catch_disposition=="total_mortality" & categ=="cod" & value<99

egen med = pctile(value), by(cate catch number) p(50) 
egen lb_med = pctile(value), by(cate catch number)  p(5) 
egen ub_med = pctile(value),  by(cate catch number)  p(95) 
egen sum_acl=sum(under_acl) , by(cate catch number)

keep cate catch number med lb_med ub_med sum
duplicates drop 

