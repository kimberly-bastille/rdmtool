

cd $mrip_data_cd

clear
global fluke_effort

tempfile tl1 cl1
dsconcat $triplist

/* *dtrip will be used to estimate total directed trips, do not change it*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
*drop strat_interval
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
*keep if strmatch(common, "summerflounder") | strmatch(common,"summerflounder")
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


/*This is the "full" mrip data */
*tempfile tc1
*save `tc1'

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

rename intsite SITE_ID
merge m:1 SITE_ID using "$input_code_cd/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
replace stock_region_calc="NORTH" if intsite==4434

drop _merge

/*classify into GOM or GBS */
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")

gen my_dom_id_string=area_s+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id

replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))
/*convert this string to a number */

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
svy: total dtrip if area_s=="GOM" & dom_id=="1"  
/*
--------------------------------------------------------------
             |             Linearized
             |      Total   std. err.     [95% conf. interval]
-------------+------------------------------------------------
       dtrip |     234459    8745.14      217174.6    251743.4
--------------------------------------------------------------
*/

svy: total dtrip if area_s=="GOM" & dom_id=="1", over(mode2)
/*
---------------------------------------------------------------
              |             Linearized
              |      Total   std. err.     [95% conf. interval]
--------------+------------------------------------------------
c.dtrip@mode2 |
          fh  |   57211.25   4593.586      48132.21    66290.28
          pr  |     161487   7441.536      146779.1    176194.9
          sh  |   15760.72          .             .           .
---------------------------------------------------------------

*/
/*
gen log_wp_int=log(wp_int)
collapse (sum) log_wp_int (sd) sd_log_wp_int=log_wp_int, by(my_dom_id)  
*/
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

expand 150
gen dtrip_not_trunc=rnormal(`est', `sd')
gen dtrip_new=max(dtrip_not_trunc, 0)

gen draw=_n

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

clear
dsconcat $drawz

su dtrip_new if mode=="sh"
return list
local new = `r(sum)'

su dtrip_not if mode=="sh"
return list
local not_truc = `r(sum)'
di ((`new'-`not_truc')/`not_truc')*100


*The following attempts to correct for bias that occurs when drawing from uncertain MRIP estimates. 
*When an MRIP estimate is very uncertain, some draws of x from a normal distribution can result in x_i<0. Because trip outcomes cannot 
*be negative, I change these to 0. But doing so results in an upwardly shifted mean across draws. To correct for this, I sum x_i
*across draws where x_i<0, divide by the number of draws where x_i>0, and subtract this value from all draws where x_i>0. 
*This partly corrects for the issue; however, subtracting a fixed value from x_i where x_i>0 leads to some of these x_i's now <0. I replace these values as 0. 
 
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

/*
su dtrip_new2 if mode=="fh"
return list
local new = `r(sum)'

su dtrip_not if mode=="fh"
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

forv d = 1/150{
	u `new1', clear 
	keep if draw==`d'

	tempfile dtrips`d'
	save `dtrips`d'', replace 
	
clear 
set obs 2
gen day=$calibration_date_start if _n==1
replace day=$calibration_date_end if _n==2
format day %td
tsset day
tsfill, full
gen day_i=_n

gen dow = dow(day)  //*0=Sunday,...,6=Saturday

gen kod="we" if inlist(dow, 5, 6, 0)
replace kod="wd" if inlist(dow, 1, 2, 3, 4)

*add the 12 federal holidays as weekends

							
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


*cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
*save "directed_trips_basefile2.dta", replace 


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
drop dtrip sum_days  se  _merge tab 

sort  draw mode day 
rename trips_per_day dtrip

browse if draw==1
sort day 
*collapse (mean) trips_per_day, by(mode day_i)
*su trips_per_day
*return list
 

 
 *make some plot of the data 
 /*
u "directed_trips_basefile_100draws.dta", clear 

keep if state=="MD" & mode=="pr" 
sort month day_i
bysort month kod draw (day_i): gen first=1 if _n==1
keep if first==1

gen domain=month+"_"+kod
egen mean_dtrip=mean(trips_per_day), by(domain)
drop if mean==0

encode domain, gen(domain2)

  vioplot trips_per_day  if state=="MD" & mode=="pr", over(domain2)

  
twoway scatter trips_per_day domain2  if state=="MD" & mode=="pr" , msize(vtiny) xlab(1(1)16,valuelabel angle(45) labsize(small)) , ///
			$graphoptions ylab(,labsize(small) angle(horizontal)) legend(off) title("Draws of directed trips per day" "Private boat mode, MD 2022", size(small)) xtitle("Month and kind-of-day (weekday or weekend)", size(small) yoffset(-2)) ytitle("# directed trips per day", size(small)) yline(0)


 
u "directed_trips_basefile_100draws.dta", clear 

keep if state=="NJ" & mode=="pr" 
sort month day_i
bysort month kod draw (day_i): gen first=1 if _n==1
keep if first==1

gen domain=month+"_"+kod
egen mean_dtrip=mean(trips_per_day), by(domain)
drop if mean==0

encode domain, gen(domain2)
scatter trips_per_day domain2  if state=="NJ" & mode=="pr" , msize(tiny)  xlab(1(1)17,valuelabel angle(45) labsize(vsmall)) , ///
			$graphoptions ylab(,labsize(small) angle(horizontal)) legend(off) title("", size(small)) xtitle("Month and kind-of-day (weekday or weekend)", size(small) yoffset(-2)) ytitle("# directed trips per day", size(small)) yline(0) 
 
 
 vioplot trips_per_day  if state=="NJ" & mode=="pr", over(domain2)
 
u "directed_trips_basefile_100draws.dta", clear 

keep if state=="MD" & mode=="pr" 
sort month day_i
bysort month kod draw (day_i): gen first=1 if _n==1
keep if first==1

gen domain=month+"_"+kod
egen mean_dtrip=mean(trips_per_day), by(domain)
drop if mean==0



encode domain, gen(domain2)
twoway scatter trips_per_day domain2  if state=="MD" & mode=="pr" , msize(tiny) xlab(1(1)16,valuelabel angle(45) labsize(vsmall)) , ///
			$graphoptions ylab(,labsize(small) angle(horizontal)) legend(off) title("Draws of directed trips per day" "Private boat mode, NJ 2022", size(small)) xtitle("Month and kind-of-day (we
*/		
			


*Now create the baseline regulations for the calibration period
gen cod_bag=0 
gen cod_min=100

gen hadd_bag=0
gen hadd_min=100


*Hadd regs 
replace hadd_bag=20 if  day>=$hadd_start_date1 & day<=$hadd_end_date1
replace hadd_min=17 if  day>=$hadd_start_date1 & day<=$hadd_end_date1

replace hadd_bag=20 if  day>=$hadd_start_date2 & day<=$hadd_end_date2
replace hadd_min=17 if  day>=$hadd_start_date2 & day<=$hadd_end_date2

replace hadd_bag=15 if  day>=$hadd_start_date3_fh & day<=$hadd_end_date3_fh & inlist(mode, "fh")
replace hadd_min=18 if  day>=$hadd_start_date3_fh & day<=$hadd_end_date3_fh  & inlist(mode, "fh")

replace hadd_bag=10 if  day>=$hadd_start_date3_pr & day<=$hadd_end_date3_pr & inlist(mode, "pr", "sh")
replace hadd_min=17 if  day>=$hadd_start_date3_pr & day<=$hadd_end_date3_pr  & inlist(mode, "pr", "sh")


*Cod regs
replace cod_bag=1 if  day>=$cod_start_date1 & day<=$cod_end_date1 
replace cod_min=22 if  day>=$cod_start_date1 & day<=$cod_end_date1 

replace cod_bag=1 if  day>=$cod_start_date2 & day<=$cod_end_date2 
replace cod_min=22 if  day>=$cod_start_date2 & day<=$cod_end_date2 


*gen doy = doy(day)
*drop day_i

gen day1=day(day)
gen month1=month(day)

tempfile regulations
save `regulations', replace 

*cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
*export delimited using "directed trips and regulations 2022_100 draws.csv",  replace 


*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full


gen day1=day(day_y2)
gen month1=month(day_y2)
gen year_y2=year(day_y2)

drop if day_y2==td(29feb2024)

*gen doy=doy(day_y2)

gen dow_y2 = dow(day_y2)  


gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		

replace kod_y2="we" if $fed_holidays_y2



gen month2_y2= string(month1,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup



merge 1:m  mode day1 month1 using `regulations'
drop _merge 
order year mode month kod dow day  draw cod_bag cod_min hadd_bag hadd_min day_y2 dow_y2 kod_y2 month_y2
sort  mode day draw


*Create status quo regualtions 

*Now create the baseline regulations for the calibration period
gen cod_bag_y2=0 
gen cod_min_y2=100

gen hadd_bag_y2=0
gen hadd_min_y2=100

/*
The recreational sub-ACL for GoM cod is 192 mt for FY 2023. The recreational sub-ACL for GOM haddock is 610 mt. Regulations were implemented in August of 2023, so part of the year had a haddock limit of 20 fish at 17".

Haddock:
For-Hire sector: 15 fish with an 18" minimum size.
Private Anglers: 10 fish with a 17" minimum size.
Open May1-Feb 28 and April 1- April 30. Closed for the month of March.

Cod:
1 fish, 22"
Open Sept 1-October 31 for all anglers.
*/
*Hadd regs 
replace hadd_bag_y2=15 if  day_y2>=td(01may2024) & day_y2<=td(28feb2025)  & inlist(mode, "fh")
replace hadd_bag_y2=15 if  day_y2>=td(01apr2025) & day_y2<=td(30apr2025) & inlist(mode, "fh")

replace hadd_bag_y2=10 if  day_y2>=td(01may2024) & day_y2<=td(28feb2025)  & inlist(mode, "pr", "sh")
replace hadd_bag_y2=10 if  day_y2>=td(01apr2025) & day_y2<=td(30apr2025) & inlist(mode, "pr", "sh")

replace hadd_min_y2=18 if  hadd_bag_y2!=0 & inlist(mode, "fh")
replace hadd_min_y2=17 if  hadd_bag_y2!=0 & inlist(mode, "pr", "sh")


*Cod regs
replace cod_bag_y2=1 if  day_y2>=td(01sep2024) & day_y2<=td(31oct2024) 
replace cod_min_y2=22 if  cod_bag_y2!=0 

*export delimited using "$input_code_cd\directed_trips_calib_150draws.csv",  replace 
*import delimited using "$input_code_cd\directed_trips_calib_150draws.csv",  clear  


replace cod_min = cod_min*2.54
replace hadd_min = hadd_min*2.54
replace cod_min_y2 = cod_min_y2*2.54
replace hadd_min_y2 = hadd_min_y2*2.54

export delimited using "$input_code_cd\directed_trips_calib_150draws_cm.csv",  replace 



**Now adjust for the differences in directed trips due to changes in kod between calibration year y and  y+1
import delimited using "$input_code_cd\directed_trips_calib_150draws_cm.csv",  clear  
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

browse if dtrip==0 & dtrip_y2!=0
browse
mvencode expansion_factor, mv(1) override

su dtrip
return list

su dtrip_y2
return list

gen check =dtrip*expansion
su check
return list

drop check 

export delimited using "$input_code_cd\next year calendar adjustments.csv",  replace 
*import delimited using "$input_code_cd\next year calendar adjustments.csv",  clear  




