

/*This code uses the MRIP data to 
	1) estimate dircetd trips and their standard error at the year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh) level during the calibraiton period, 
	2) use those paramters to create 150 random draws of directed trips for each stratum,
	3) divide each random draw by the number of days in that stratum to obtain an estimate of trips-per-day calibration period, 
	4) compute for each stratum an calender year adjustment = (# of calender days in that stratum for the projection period)/(# of calender days in that stratum for the calibration period), 
		that we will use to correct for differences in the number of calinder days in each stratum between the calibration and projection period, 
	5) set the baseline year and projection year regulations ("$input_code_cd/set regulations.do")
*/
		

cd $input_data_cd

clear
global fluke_effort

tempfile tl1 cl1
dsconcat $triplist

/*dtrip will be used to estimate total directed trips*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
keep if $calibration_year


/* THIS IS THE END OF THE DATA MERGING CODE */

 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)


 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 

tostring wave, gen(w2)
tostring year, gen(year2)
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
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")


*New MRIP site allocations
preserve 
import excel using "$input_data_cd/ma_site_list_updated_SS.xlsx", clear first
keep SITE_EXTERNAL_ID NMFS_STAT_AREA
renvarlab, lower
rename site_external_id intsite
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite using `mrip_sites',  keep(1 3)

/*classify into GOM or GBS */
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,511, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh)*/
gen my_dom_id_string=area_s+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se  ll ul
gen pse=(se/b)*100

split my, parse(_)
rename my_dom_id_string1 area_s
rename my_dom_id_string2 year
rename my_dom_id_string3 month1
rename my_dom_id_string4 kod
rename my_dom_id_string5 mode
rename my_dom_id_string6 dom_id
drop my_dom_id_string
rename b dtrip

keep if dom_id=="1"
keep if area_s=="GOM"

su dtrip
return list

replace se=dtrip if se==.
drop ll ul pse

*drop shore trips
drop if mode=="sh"


count
local num=`r(N)'
di `num'

tempfile new
save `new', replace 

global drawz
forv d = 1/`num'{
u `new', clear 

keep if _n==`d'
su dtrip
local est = `r(mean)'

su se
local sd = `r(mean)'

expand $ndraws
gen dtrip_not_trunc=rnormal(`est', `sd')
gen dtrip_new=max(dtrip_not_trunc, 0)

 
gen draw=_n

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

clear
dsconcat $drawz


su dtrip_not 
return list

su dtrip_new
return list
local new = `r(sum)'

local not_truc = `r(sum)'
di ((`new'-`not_truc')/`not_truc')*100


/*The following attempts to correct for bias that occurs when drawing from uncertain MRIP estimates. 
	*When an MRIP estimate is very uncertain, some draws of x from a normal distribution can result in x_i<0. Because trip outcomes cannot 
	*be negative, I change these to 0. But doing so results in an upwardly shifted mean across draws. To correct for this, I sum x_i
	*across draws where x_i<0, divide by the number of draws where x_i>0, and subtract this value from all draws where x_i>0. 
	*This partly corrects for the issue; however, subtracting a fixed value from x_i where x_i>0 leads to some of these x_i's now <0. I replace these values as 0. */

*I have tried paramaterizing non-negative distributions using the MRIP point estimate and SE, but couldn't quite figure it out. Can work on this in the future. 
 
gen domain=month1+"_"+kod+"_"+mode

gen tab=1 if dtrip_not<0
egen sum_neg=sum(dtrip_not) if tab==1, by(domain)
sort domain
egen mean_sum_neg=mean(sum_neg), by(domain)

egen sum_non_neg=sum(dtrip_not) if dtrip_not>0 , by(domain)
gen prop=dtrip_not/sum_non_neg
gen adjust=prop*mean_sum_neg

/*
egen pctile_x=pctile(dtrip_not) , p(10) by(domain)
gen tab2=1 if dtrip_not>0 & dtrip_not>pctile_x
egen sumtab2=sum(tab2), by(domain)
gen adjust=mean_sum_neg/sumtab2
*/

gen dtrip_new2=dtrip_new+adjust if dtrip_new!=0 & adjust !=.
replace dtrip_new2=dtrip_new if dtrip_new2==.
replace dtrip_new2=0 if dtrip_new2<0


*check differences between original and adjusted draws 
/*
su dtrip_new2 
return list
local new = `r(sum)'

su dtrip_not
return list
local not_truc = `r(sum)'
di ((`new'-`not_truc')/`not_truc')*100
*/

su dtrip_new2
return list
local new = `r(sum)'

su dtrip_not
return list
local old = `r(sum)'

di ((`new'-`old')/`old')*100

replace dtrip_new=dtrip_new2

drop area domain dom_id dtrip_not tab sum_neg sum_non_neg prop mean_sum_neg adjust dtrip_new2 
rename month1 month 

sort mode month kod draw 
tempfile new1
save `new1'

*now need to make a dataset for the calender year and average out the directed trips across days

global drawz2

forv d = 1/$ndraws{
	u `new1', clear 
	keep if draw==`d'

	tempfile dtrips`d'
	save `dtrips`d'', replace 
	
clear 
set obs 2
gen day=$calibration_date_start if _n==1
replace day=$calibration_date_end if _n==2
format day %td
drop if day==$leap_yr_days
tsset day
tsfill, full
gen day_i=_n

gen dow = dow(day)  //0=Sunday,...,6=Saturday

gen kod="we" if inlist(dow, 5, 6, 0)
replace kod="wd" if inlist(dow, 1, 2, 3, 4)

//add the 12 federal holidays as weekends	
replace kod="we" if $fed_holidays 

gen year=year(day)				
gen month=month(day)				
gen month2 = string(month,"%02.0f")
tostring year, replace
drop month
rename month2 month
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup

merge m:1  kod month mode using `dtrips`d''
*gen draw=`d'
tempfile drawz2`d'
save `drawz2`d'', replace
global drawz2 "$drawz2 "`drawz2`d''" " 

}
clear
dsconcat $drawz2
sort day  mode draw

bysort day mode: gen draw2=_n
order draw2
replace draw=draw2 if draw==.
drop draw2

mvencode dtrip dtrip_new, mv(0) override

*number of weekend/weekday days per state, month, and mode, and draw
gen tab=1
bysort month kod mode draw:egen sum_days=sum(tab)
order sum_days

sort draw mode day 
order draw
sort day
drop dtrip
rename dtrip_new dtrip
mvencode dtrip, mv(0) override
gen trips_per_day=dtrip/sum_days
mvencode trips_per_day, mv(0) override 
order dtrip trips_per_day

order mode year month kod dow day day_i trips_per_day draw
drop dtrip sum_days se _merge tab 

sort  draw mode day 
rename trips_per_day dtrip

sort day 

gen day1=day(day)
gen month1=month(day)


*call the regulations file	
do "$input_code_cd/set regulations.do"	

preserve
keep if cod_bag!=0
keep day
duplicates drop 
rename day date
gen cod_season_open=1
save  "$input_data_cd\cod_open_season_dates.dta",  replace 
restore 


export delimited using "$input_data_cd\directed_trips_calib_150draws_cm.csv",  replace 



**Now adjust for the differences in directed trips due to changes in kod between calibration year y and  y+1
import delimited using "$input_data_cd\directed_trips_calib_150draws_cm.csv",  clear  
tostring month, gen(month1_y1)
tostring month_y2, gen(month1_y2)

tempfile base 
save `base', replace 

global drawz

levelsof draw, local(drawss)
foreach d of local drawss{

u `base', clear

keep if draw==`d'
gen domain_y1=mode+"_"+month1_y1+"_"+kod
gen domain_y2=mode+"_"+month1_y2+"_"+kod_y2

gen dtrip_y2=dtrip if domain_y1==domain_y2 

levelsof domain_y2 if dtrip_y2==., local(domains)
foreach p of local domains{
	su dtrip if domain_y1=="`p'"
	return list
	replace dtrip_y2=`r(mean)' if  domain_y2=="`p'" & dtrip_y2==.
	
}
collapse (sum) dtrip dtrip_y2, by(month mode)
gen expansion_factor = dtrip_y2/dtrip
gen draw=`d'

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

dsconcat $drawz

mvencode expansion_factor, mv(1) override

su dtrip
return list

su dtrip_y2
return list

gen check =dtrip*expansion
su check
return list

drop check 

export delimited using "$input_data_cd\next year calendar adjustments.csv",  replace 


