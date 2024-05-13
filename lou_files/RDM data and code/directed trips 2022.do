

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

clear

global fluke_effort

*local yrs 2019
*foreach y of local yrs{

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
 
keep if inlist(year, 2022, 2021, 2020, 2019, 2018) 




/* THIS IS THE END OF THE DATA MERGING CODE */



 /* ensure only relevant states */
 keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
*tempfile tc1
*save `tc1'



 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 

replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

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
*destring month1, replace
destring day1, replace

/*
gen period ="1" if day1<=15 & month1==1
replace period ="2" if day1>15 & month1==1
replace period ="3" if day1<=15 & month1==2
replace period ="4" if day1>15 & month1==2
replace period ="5" if day1<=15 & month1==3
replace period ="6" if day1>15 & month1==3
replace period ="7" if day1<=15 & month1==4
replace period ="8" if day1>15 & month1==4
replace period ="9" if day1<=15 & month1==5
replace period ="10" if day1>15 & month1==5
replace period ="11" if day1<=15 & month1==6
replace period ="12" if day1>15 & month1==6
replace period ="13" if day1<=15 & month1==7
replace period ="14" if day1>15 & month1==7
replace period ="15" if day1<=15 & month1==8
replace period ="16" if day1>15 & month1==8
replace period ="17" if day1<=15 & month1==9
replace period ="18" if day1>15 & month1==9
replace period ="19" if day1<=15 & month1==10
replace period ="20" if day1>15 & month1==10
replace period ="21" if day1<=15 & month1==11
replace period ="22" if day1>15 & month1==11
replace period ="23" if day1<=15 & month1==12
replace period ="24" if day1>15 & month1==12

*/
/*
destring day1, replace
sort year month1 day1
gen date2=mdy(month1,day1,year)
format date2 %td
*/
*gen season="open" if date2>=td(24may2019) & date2<=td(21sep2019)
*replace season="closed" if season==""


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

*gen my_dom_id_string=state+"_"+period+"_"+dom_id
*gen my_dom_id_string=state+"_"+period+"_"+mode1+"_"+dom_id
gen my_dom_id_string=state+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id


*tostring intsite, gen(intsite2)
*gen my_dom_id_string=state+"_"+year2+"_"+month1+"_"+intsite2
*gen my_dom_id_string=state+"_"+year2+"_"+w2+"_"+intsite2
*gen my_dom_id_string=state+"_"+year2+"_"+"_"+intsite2

*gen my_dom_id_string=state+"_"+dom_id

replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))
/*convert this string to a number */

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n

keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

tempfile base 
save `base', replace

global trips
levelsof year, local(yrs)
foreach y of local yrs{
	u `base', replace 
	keep if year==`y'
	
	tempfile base2
	save `base2', replace 
	
	levelsof state, local(sts)
	foreach s of local sts{
		u `base2', clear 
		keep if state=="`s'"

		preserve
		keep my_dom_id my_dom_id_string
		duplicates drop 
		tostring my_dom_id, gen(my_dom_id2)
		keep my_dom_id2 my_dom_id_string
		tempfile domains
		save `domains', replace 
		restore

		svy: total dtrip , over(my_dom_id)  

		xsvmat, from(r(table)') rownames(rname) names(col) norestor
		split rname, parse("@")
		drop rname1
		split rname2, parse(.)
		drop rname2 rname22
		rename rname21 my_dom_id2
		merge 1:1 my_dom_id2 using `domains'
		drop rname my_dom_id2 _merge 
		order my_dom_id_string
	
	
		tempfile trips`y'`s'
		save `trips`y'`s'', replace
		global trips "$trips "`trips`y'`s''" " 

	}

	
}


clear
dsconcat $trips


keep my b se  ll ul
gen pse=(se/b)*100

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 month
rename my_dom_id_string4 kod
rename my_dom_id_string5 mode
keep if  my_dom_id_string6=="1"
drop my_dom_id_string6
drop my_dom_id_string
rename b dtrip

order state year month kod mode dtrip se pse ll ul

keep if year=="2022"
cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
save "directed_trips_basefile.dta", replace 
 
***************************
/*
**Here is where I retain the point estimates for use in the p-star routine
u  "directed_trips_basefile.dta", clear 

tempfile dtrips
save `dtrips', replace 
	
clear 
set obs 2
gen day=td(1jan2022) if _n==1
replace day=td(31dec2022) if _n==2
format day %td
tsset day
tsfill, full
gen day_i=_n

gen dow = dow(day)  //*0=Sunday,...,6=Saturday

gen kod="we" if inlist(dow, 5, 6, 0)
replace kod="wd" if inlist(dow, 1, 2, 3, 4)

*add the 12 federal holidays as weekends
replace kod="we" if inlist(day, td(01jan2022), td(17jan2022), td(21feb2022), td(30may2022), td(20jun2022), td(04jul2022), td(05sep2022), td(10oct2022), ///
							td(11nov2022), td(24nov2022), td(26dec2022))

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

expand 9, gen(dup)
sort day day_i dow kod year month mode	dup


bysort day day_i dow kod year month mode: gen tab=_n
gen state="MA" if tab==1
replace state="RI" if tab==2					
replace state="CT" if tab==3					
replace state="NY" if tab==4					
replace state="NJ" if tab==5					
replace state="DE" if tab==6					
replace state="MD" if tab==7					
replace state="VA" if tab==8					
replace state="NC" if tab==9		

drop tab
*merge m:1  state kod year month mode using "directed_trips_basefile.dta"
merge m:1  state kod year month mode using `dtrips'
 
bysort day state mode: gen draw2=_n
order draw2
gen draw=draw2
drop draw2

mvencode dtrip , mv(0) override
 
*number of weekend/weekday days per state, month, and mode, and draw
gen tab=1
bysort state month kod mode draw:egen sum_days=sum(tab)
order sum_days

sort state draw mode day 
order draw
sort day
mvencode dtrip, mv(0) override
gen trips_per_day=dtrip/sum_days
mvencode trips_per_day, mv(0) override 
order dtrip trips_per_day

order state mode year month kod dow day day_i trips_per_day draw
drop dtrip sum_days dup se pse ll ul _merge tab 

sort  state draw mode day 
 
rename trips_per_day dtrip
replace dtrip=round(dtrip)
gen dtrip_min=min(dtrip, 50)
su dtrip_m
return list


*Now create the baseline regulations for 2022
gen fluke_bag1=0
gen fluke_min1=100
gen fluke_max1=100

gen fluke_bag2=0
gen fluke_min2=100
gen fluke_max2=100

gen bsb_bag=0
gen bsb_min=100
gen bsb_max=100

gen scup_bag=0
gen scup_min=100

*Fluke regs 2022
replace fluke_bag1=5 if state=="MA" & day>=td(21may2022) & day<=td(29sep2022)
replace fluke_min1=16.5 if state=="MA" & day>=td(21may2022) & day<=td(29sep2022)

replace fluke_bag1=4 if state=="RI" & day>=td(03may2022) & day<=td(31dec2022)
replace fluke_min1=18 if state=="RI" & day>=td(03may2022) & day<=td(31dec2022)

replace fluke_bag1=4 if state=="CT" & day>=td(01may2022) & day<=td(09oct2022)
replace fluke_min1=18.5 if state=="CT" & day>=td(01may2022) & day<=td(09oct2022)

replace fluke_bag1=4 if state=="NY" & day>=td(01may2022) & day<=td(09oct2022)
replace fluke_min1=18.5 if state=="NY" & day>=td(01may2022) & day<=td(09oct2022)

replace fluke_bag1=2 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)
replace fluke_min1=17 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)
replace fluke_max1=18 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)

replace fluke_bag2=1 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)
replace fluke_min2=18 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)

replace fluke_bag1=4 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022)
replace fluke_min1=16 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022)

replace fluke_bag1=4 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022)
replace fluke_min1=16 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022)

replace fluke_bag1=4 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022)
replace fluke_min1=16 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022)

replace fluke_bag1=1 if state=="NC" & day>=td(01sep2022) & day<=td(30sep2022)
replace fluke_min1=15 if state=="NC" & day>=td(01sep2022) & day<=td(30sep2022)

*BSB regs 2022
replace bsb_bag=4 if state=="MA" & day>=td(21may2022) & day<=td(04sep2022)
replace bsb_min=16 if state=="MA" & day>=td(21may2022) & day<=td(04sep2022)

replace bsb_bag=2 if state=="RI" & day>=td(22may2022) & day<=td(31aug2022) & inlist(mode, "pr", "sh")
replace bsb_min=16 if state=="RI" & day>=td(22may2022) & day<=td(31aug2022) & inlist(mode, "pr", "sh")

replace bsb_bag=3 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace bsb_min=16 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace bsb_bag=2 if state=="RI" & day>=td(18jun2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="RI" & day>=td(18jun2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace bsb_bag=6 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace bsb_bag=5 if state=="CT" & day>=td(19may2022) & day<=td(01dec2022) & inlist(mode, "pr", "sh")
replace bsb_min=16 if state=="CT" & day>=td(19may2022) & day<=td(01dec2022) & inlist(mode, "pr", "sh")

replace bsb_bag=5 if state=="CT" & day>=td(19may2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="CT" & day>=td(19may2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace bsb_bag=7 if state=="CT" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="CT" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace bsb_bag=3 if state=="NY" & day>=td(23jun2022) & day<=td(31aug2022)
replace bsb_min=16 if state=="NY" & day>=td(23jun2022) & day<=td(31aug2022)

replace bsb_bag=6 if state=="NY" & day>=td(01sep2022) & day<=td(31dec2022)
replace bsb_min=16 if state=="NY" & day>=td(01sep2022) & day<=td(31dec2022)

replace bsb_bag=10 if state=="NJ" & day>=td(17may2022) & day<=td(19jun2022)
replace bsb_min=13 if state=="NJ" & day>=td(17may2022) & day<=td(19jun2022)

replace bsb_bag=2 if state=="NJ" & day>=td(01jul2022) & day<=td(31aug2022)
replace bsb_min=13 if state=="NJ" & day>=td(01jul2022) & day<=td(31aug2022)

replace bsb_bag=10 if state=="NJ" & day>=td(07oct2022) & day<=td(26oct2022)
replace bsb_min=13 if state=="NJ" & day>=td(07oct2022) & day<=td(26oct2022)

replace bsb_bag=15 if state=="NJ" & day>=td(01nov2022) & day<=td(31dec2022)
replace bsb_min=13 if state=="NJ" & day>=td(01nov2022) & day<=td(31dec2022)

replace bsb_bag=15 if inlist(state, "DE", "MD", "VA", "NC") & day>=td(15may2022) & day<=td(11dec2022)
replace bsb_min=13 if inlist(state, "DE", "MD", "VA", "NC") & day>=td(15may2022) & day<=td(11dec2022)


*scup regs 2022
replace scup_bag=30 if state=="MA" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="MA" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="MA" & day>=td(01jan2022) & day<=td(30apr2022) & inlist(mode, "fh")
replace scup_min=10 if state=="MA" & day>=td(01jan2022) & day<=td(30apr2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="MA" & day>=td(01jul2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="MA" & day>=td(01jul2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="MA" & day>=td(01may2022) & day<=td(30jun2022) & inlist(mode, "fh")
replace scup_min=10 if state=="MA" & day>=td(01may2022) & day<=td(30jun2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="RI" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="RI" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="RI" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace scup_min=10 if state=="RI" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="RI" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="RI" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="RI" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")
replace scup_min=10 if state=="RI" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="CT" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="CT" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="CT" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace scup_min=10 if state=="CT" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="CT" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="CT" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="CT" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")
replace scup_min=10 if state=="CT" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="NY" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="NY" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="NY" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace scup_min=10 if state=="NY" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="NY" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="NY" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="NY" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")
replace scup_min=10 if state=="NY" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="NJ" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=10 if state=="NJ" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=50 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=50 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=30 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=50 if state=="NC" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="NC" & day>=td(01jan2022) & day<=td(31dec2022) 
 
cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
export delimited using "directed trips and regulations 2022_pstar.csv",  replace 
*/
 
 
 
 *************************

 **make a plot illustrating the uncertainty 
 u  "directed_trips_basefile.dta", clear 	
 global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

sort state mode month kod
keep if state=="NJ" & mode=="pr"
gen domain=month+"_"+kod

encode domain, gen(domain2)

twoway scatter dtrip domain2  if state=="NJ" & mode=="pr" , xlab(1(1)17,valuelabel angle(45) labsize(vsmall)) || rcap ul ll domain2  if state=="NJ" & mode=="pr", ///
			$graphoptions ylab(,labsize(small) angle(horizontal)) legend(off) title("", size(small)) xtitle("Month and kind-of-day (weekday or weekend)", size(small) yoffset(-2)) ytitle("# directed trips", size(small)) note("Bars indicate 95% CI", size(small) yoffset(-2)) yline(0)
			

 **make a plot illustrating the uncertainty 
u  "directed_trips_basefile.dta", clear 	

sort state mode month kod
keep if state=="MD" & mode=="pr"
gen domain=month+"_"+kod

encode domain, gen(domain2)
/*
twoway scatter dtrip domain2  if state=="MD" & mode=="pr" , xlab(1(1)16,valuelabel angle(45) labsize(vsmall)) || rcap ul ll domain2  if state=="MD" & mode=="pr", ///
			$graphoptions ylab(,labsize(small) angle(horizontal)) legend(off) title("MRIP estimates of total directed trips for fluke, black sea bass, or scup" "Private boat mode MD, 2022", size(small)) xtitle("Month and kind-of-day (weekday or weekend)", size(small) yoffset(-2)) ytitle("# directed trips", size(small)) note("Bars indicate 95% CI", size(small) yoffset(-2))  yline(0)
*/			
		*/
 **Here is where i create the 100 draws of directed trips
u  "directed_trips_basefile.dta", clear 	
serrbar dtrip 
replace se=dtrip if se==.
su dtrip
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

expand 100
gen dtrip_new=max(rnormal(`est', `sd'), 0)
gen draw=_n

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

clear
dsconcat $drawz

su dtrip_new
return list
local new = `r(sum)'

su dtrip
return list
local old = `r(sum)'

di ((`new'-`old')/`old')*100


tempfile new1
save `new1'

	
*su dtrip if state=="NJ" & mode=="sh"
*return list //2,467,315.7734375 trips
su dtrip
return list // 20,431,260 trips

*now need to make a dataset for the calender year and average out the directed trips across days

global drawz2

forv d = 1/100{
	u `new1', clear 
	keep if draw==`d'

	tempfile dtrips`d'
	save `dtrips`d'', replace 
	
clear 
set obs 2
gen day=td(1jan2022) if _n==1
replace day=td(31dec2022) if _n==2
format day %td
tsset day
tsfill, full
gen day_i=_n

gen dow = dow(day)  //*0=Sunday,...,6=Saturday

gen kod="we" if inlist(dow, 5, 6, 0)
replace kod="wd" if inlist(dow, 1, 2, 3, 4)

*add the 12 federal holidays as weekends
replace kod="we" if inlist(day, td(01jan2022), td(17jan2022), td(21feb2022), td(30may2022), td(20jun2022), td(04jul2022), td(05sep2022), td(10oct2022), ///
							td(11nov2022), td(24nov2022), td(26dec2022))

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

expand 9, gen(dup)
sort day day_i dow kod year month mode	dup


bysort day day_i dow kod year month mode: gen tab=_n
gen state="MA" if tab==1
replace state="RI" if tab==2					
replace state="CT" if tab==3					
replace state="NY" if tab==4					
replace state="NJ" if tab==5					
replace state="DE" if tab==6					
replace state="MD" if tab==7					
replace state="VA" if tab==8					
replace state="NC" if tab==9		

drop tab
*merge m:1  state kod year month mode using "directed_trips_basefile.dta"
merge m:1  state kod year month mode using `dtrips`d''
*gen draw=`d'
tempfile drawz2`d'
save `drawz2`d'', replace
global drawz2 "$drawz2 "`drawz2`d''" " 

}
clear
dsconcat $drawz2
*sort day day_i dow kod year month mode draw	dup

*cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
*save "directed_trips_basefile2.dta", replace 


bysort day state mode: gen draw2=_n
order draw2
replace draw=draw2 if draw==.
drop draw2

mvencode dtrip dtrip_new, mv(0) override

*number of weekend/weekday days per state, month, and mode, and draw
gen tab=1
bysort state month kod mode draw:egen sum_days=sum(tab)
order sum_days

sort state draw mode day 
order draw
sort day
drop dtrip
rename dtrip_new dtrip
mvencode dtrip, mv(0) override
gen trips_per_day=dtrip/sum_days
mvencode trips_per_day, mv(0) override 
order dtrip trips_per_day

order state mode year month kod dow day day_i trips_per_day draw
drop dtrip sum_days dup se pse ll ul _merge tab 

sort  state draw mode day 

cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
save "directed_trips_basefile_100draws.dta", replace 
 

 
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
			
/*
*Now need to merge the trips per day to the calender year 
preserve 
keep month kod trips_per_day state mode 
 


sort state mode   day 
keep if draw==1

tempfile new3
save `new3', replace

global drawz3
levelsof draw, local(drs)
foreach d of local drs{
	u `new3', clear 
	keep if draw==`d'
	
	tempfile full_base
	save `full_base', replace

levelsof state, local(sts)
foreach s of local sts{
	u `full_base', clear
	keep if state=="`s'"
	
	tempfile full_base2
	save `full_base2', replace 
	
	levelsof mode, local(mds)
	foreach m of local mds{

	u `full_base2', clear
	keep if mode=="`m'"
	

	preserve
	keep day day_i month kod
	tempfile basefile
	save `basefile', replace
	restore

	sort state day 
	drop dup

	*need to see how many weekends/weekdays per month
	*bysort state mode month kod: gen tab=1 if _n==1
	gen tab=1
	bysort  month kod:egen sum_days=sum(tab)
	order sum_days
	sort day
	drop dtrip
	rename dtrip_new dtrip
	mvencode dtrip, mv(0) override
	gen trips_per_day=dtrip/sum_days
	mvencode trips_per_day, mv(0) override

	keep month kod trips_per_day
	duplicates drop 

	merge 1:m month kod  using `basefile'
	sort day_i
	su trips_per_day
	return list //2,467,315.774993896 trips 
	rename trips dtrip
	mvencode dtrip, mv(0) override

	gen state="`s'"
	gen mode="`m'"
	gen draw=`d'

	tempfile drawz3`m'`s'`d'
	save `drawz3`m'`s'`d'', replace
	global drawz3 "$drawz3 "`drawz3`m'`s'`d''" " 

	}
	
}
}

clear
dsconcat $drawz3
*/
*drop _merg 
*order state day day_i month kod mode dtrip

rename trips_per_day dtrip
replace dtrip=round(dtrip)
gen dtrip_min=min(dtrip, 50)
su dtrip_m
return list


*Now create the baseline regulations for 2022
gen fluke_bag1=0
gen fluke_min1=100
gen fluke_max1=100

gen fluke_bag2=0
gen fluke_min2=100
gen fluke_max2=100

gen bsb_bag=0
gen bsb_min=100
gen bsb_max=100

gen scup_bag=0
gen scup_min=100

*Fluke regs 2022
replace fluke_bag1=5 if state=="MA" & day>=td(21may2022) & day<=td(29sep2022)
replace fluke_min1=16.5 if state=="MA" & day>=td(21may2022) & day<=td(29sep2022)

replace fluke_bag1=4 if state=="RI" & day>=td(03may2022) & day<=td(31dec2022)
replace fluke_min1=18 if state=="RI" & day>=td(03may2022) & day<=td(31dec2022)

replace fluke_bag1=4 if state=="CT" & day>=td(01may2022) & day<=td(09oct2022)
replace fluke_min1=18.5 if state=="CT" & day>=td(01may2022) & day<=td(09oct2022)

replace fluke_bag1=4 if state=="NY" & day>=td(01may2022) & day<=td(09oct2022)
replace fluke_min1=18.5 if state=="NY" & day>=td(01may2022) & day<=td(09oct2022)

replace fluke_bag1=2 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)
replace fluke_min1=17 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)
replace fluke_max1=18 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)

replace fluke_bag2=1 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)
replace fluke_min2=18 if state=="NJ" & day>=td(02may2022) & day<=td(27sep2022)

replace fluke_bag1=4 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022)
replace fluke_min1=16 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022)

replace fluke_bag1=4 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022)
replace fluke_min1=16 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022)

replace fluke_bag1=4 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022)
replace fluke_min1=16 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022)

replace fluke_bag1=1 if state=="NC" & day>=td(01sep2022) & day<=td(30sep2022)
replace fluke_min1=15 if state=="NC" & day>=td(01sep2022) & day<=td(30sep2022)

*BSB regs 2022
replace bsb_bag=4 if state=="MA" & day>=td(21may2022) & day<=td(04sep2022)
replace bsb_min=16 if state=="MA" & day>=td(21may2022) & day<=td(04sep2022)

replace bsb_bag=2 if state=="RI" & day>=td(22may2022) & day<=td(31aug2022) & inlist(mode, "pr", "sh")
replace bsb_min=16 if state=="RI" & day>=td(22may2022) & day<=td(31aug2022) & inlist(mode, "pr", "sh")

replace bsb_bag=3 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace bsb_min=16 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace bsb_bag=2 if state=="RI" & day>=td(18jun2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="RI" & day>=td(18jun2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace bsb_bag=6 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="RI" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace bsb_bag=5 if state=="CT" & day>=td(19may2022) & day<=td(01dec2022) & inlist(mode, "pr", "sh")
replace bsb_min=16 if state=="CT" & day>=td(19may2022) & day<=td(01dec2022) & inlist(mode, "pr", "sh")

replace bsb_bag=5 if state=="CT" & day>=td(19may2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="CT" & day>=td(19may2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace bsb_bag=7 if state=="CT" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace bsb_min=16 if state=="CT" & day>=td(01sep2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace bsb_bag=3 if state=="NY" & day>=td(23jun2022) & day<=td(31aug2022)
replace bsb_min=16 if state=="NY" & day>=td(23jun2022) & day<=td(31aug2022)

replace bsb_bag=6 if state=="NY" & day>=td(01sep2022) & day<=td(31dec2022)
replace bsb_min=16 if state=="NY" & day>=td(01sep2022) & day<=td(31dec2022)

replace bsb_bag=10 if state=="NJ" & day>=td(17may2022) & day<=td(19jun2022)
replace bsb_min=13 if state=="NJ" & day>=td(17may2022) & day<=td(19jun2022)

replace bsb_bag=2 if state=="NJ" & day>=td(01jul2022) & day<=td(31aug2022)
replace bsb_min=13 if state=="NJ" & day>=td(01jul2022) & day<=td(31aug2022)

replace bsb_bag=10 if state=="NJ" & day>=td(07oct2022) & day<=td(26oct2022)
replace bsb_min=13 if state=="NJ" & day>=td(07oct2022) & day<=td(26oct2022)

replace bsb_bag=15 if state=="NJ" & day>=td(01nov2022) & day<=td(31dec2022)
replace bsb_min=13 if state=="NJ" & day>=td(01nov2022) & day<=td(31dec2022)

replace bsb_bag=15 if inlist(state, "DE", "MD", "VA", "NC") & day>=td(15may2022) & day<=td(11dec2022)
replace bsb_min=13 if inlist(state, "DE", "MD", "VA", "NC") & day>=td(15may2022) & day<=td(11dec2022)


*scup regs 2022
replace scup_bag=30 if state=="MA" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="MA" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="MA" & day>=td(01jan2022) & day<=td(30apr2022) & inlist(mode, "fh")
replace scup_min=10 if state=="MA" & day>=td(01jan2022) & day<=td(30apr2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="MA" & day>=td(01jul2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="MA" & day>=td(01jul2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="MA" & day>=td(01may2022) & day<=td(30jun2022) & inlist(mode, "fh")
replace scup_min=10 if state=="MA" & day>=td(01may2022) & day<=td(30jun2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="RI" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="RI" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="RI" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace scup_min=10 if state=="RI" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="RI" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="RI" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="RI" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")
replace scup_min=10 if state=="RI" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="CT" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="CT" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="CT" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace scup_min=10 if state=="CT" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="CT" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="CT" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="CT" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")
replace scup_min=10 if state=="CT" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="NY" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")
replace scup_min=10 if state=="NY" & day>=td(01jan2022) & day<=td(31dec2022) & inlist(mode, "pr", "sh")

replace scup_bag=30 if state=="NY" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")
replace scup_min=10 if state=="NY" & day>=td(01jan2022) & day<=td(31aug2022) & inlist(mode, "fh")

replace scup_bag=30 if state=="NY" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")
replace scup_min=10 if state=="NY" & day>=td(01nov2022) & day<=td(31dec2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="NY" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")
replace scup_min=10 if state=="NY" & day>=td(01sep2022) & day<=td(31oct2022) & inlist(mode, "fh")

replace scup_bag=50 if state=="NJ" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=10 if state=="NJ" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=50 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="DE" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=50 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="MD" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=30 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="VA" & day>=td(01jan2022) & day<=td(31dec2022) 

replace scup_bag=50 if state=="NC" & day>=td(01jan2022) & day<=td(31dec2022) 
replace scup_min=9 if state=="NC" & day>=td(01jan2022) & day<=td(31dec2022) 


cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
export delimited using "directed trips and regulations 2022_100 draws.csv",  replace 

/*
gen st_mode=state+"_"+mode
encode st_mode, gen(st_mode2)
xtset st_mode2 day

tsline dtrip if st_mode=="NJ_pr", title("Daily directed trips, NJ private boats")
browse if st_mode=="NJ_pr"
*/

*now add to this file the calender for 2024

*2024							
clear 
set obs 2
gen day_24=td(1jan2024) if _n==1
replace day_24=td(31dec2024) if _n==2
format day_24 %td
tsset day_24
tsfill, full

drop if day_24==td(29feb2024)

gen day_i=_n

gen dow_24 = dow(day_24)  


gen kod_24="we" if inlist(dow, 5, 6, 0)
replace kod_24="wd" if inlist(dow, 1, 2, 3, 4)		

replace kod_24="we" if inlist(day_24, td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), ///
							td(11nov2024), td(28nov2024), td(25dec2024))



gen month_24=month(day_24)				
gen month2_24= string(month,"%02.0f")
drop month_24
rename month2_24 month24
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup

expand 9, gen(dup)
sort day_24 day_i dow kod  month mode	dup


bysort day_24 day_i dow kod  month mode: gen tab=_n
gen state="MA" if tab==1
replace state="RI" if tab==2					
replace state="CT" if tab==3					
replace state="NY" if tab==4					
replace state="NJ" if tab==5					
replace state="DE" if tab==6					
replace state="MD" if tab==7					
replace state="VA" if tab==8					
replace state="NC" if tab==9		

drop tab
drop dup



tempfile calender24
save `calender24', replace

cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
import delimited using "directed trips and regulations 2022_100 draws.csv",  clear  

merge m:1 state mode day_i using `calender24'
sort  state draw mode day_i
drop _merge
export delimited using "directed trips and regulations 2022_100 draws.csv",  replace 



**Now adjust for the differences in directed trips due to changes in kod between 2022 and 2024
cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
import delimited using "directed trips and regulations 2022_100 draws.csv",  clear  


*gen day_month_22=substr(day, 1, 5)
*gen day_month_24=substr(day_24, 1, 5)

*gen dtrip_24=dtrip if day_month_22==day_month_24 & kod==kod_24
*browse draw state mode dtrip kod kod_24 day_month_22 day_month_24  

tempfile base 
save `base', replace 

global drawz

levelsof draw, local(drawss)

foreach d of local drawss{

u `base', clear

keep if draw==`d'
tostring month, gen(month1)
gen domain_22=state+"_"+mode+"_"+month1+"_"+kod
gen domain_24=state+"_"+mode+"_"+month1+"_"+kod_24

*browse state mode dtrip kod kod_24   domain_22 domain_24
*duplicates drop 

gen dtrip_24=dtrip if domain_22==domain_24 
*browse state mode dtrip kod kod_24   domain_22 domain_24 dtrip_24

levelsof domain_24 if dtrip_24==., local(domains)
foreach p of local domains{
	su dtrip if domain_22=="`p'"
	return list
	replace dtrip_24=`r(mean)' if  domain_24=="`p'" & dtrip_24==.
	
}
/*
su dtrip
return list

su dtrip_24
return list

gen kod_expansion = dtrip_24/dtrip
mvencode kod_expansion, mv(0) override

collapse (sum) dtrip dtrip_24, by(state month mode kod)
gen month_expansion = dtrip_24/dtrip
browse if dtrip==0 & dtrip_24!=0

*/

collapse (sum) dtrip dtrip_24, by(state month mode)
gen expansion_factor = dtrip_24/dtrip
gen draw=`d'

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

dsconcat $drawz

browse if dtrip==0 & dtrip_24!=0
browse
mvencode expansion_factor, mv(0) override

cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
export delimited using "calendar 2024 adjustments.csv",  replace 




/*
***Now add the 2023 regulations for simulation of the status quo scenario
cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
import delimited using "directed trips and regulations 2022_100 draws.csv",  clear  


destring day_24, replace

gen day24_str=substr(day_24, 1, 2)
destring day24_str, replace
gen date_24=mdy(month24, day24_str, year_24) 
format date_24 %td

*Fluke regulations staed the same in 2023
gen fluke_bag1_24=0
gen fluke_min1_24=100
gen fluke_max1_24=100

gen fluke_bag2_24=0
gen fluke_min2_24=100
gen fluke_max2_24=100

gen bsb_bag_24=0
gen bsb_min_24=100
gen bsb_max_24=100

gen scup_bag_24=0
gen scup_min_24=100

*Fluke regs 2024 (same as 2022 and 2023)
replace fluke_bag1_24=5 if state=="MA" & date_24>=td(21may2024) & date_24<=td(29sep2024)
replace fluke_min1_24=16.5 if state=="MA" & date_24>=td(21may2024) & date_24<=td(29sep2024)

replace fluke_bag1_24=4 if state=="RI" & date_24>=td(03may2024) & date_24<=td(31dec2024)
replace fluke_min1_24=18 if state=="RI" & date_24>=td(03may2024) & date_24<=td(31dec2024)

replace fluke_bag1_24=4 if state=="CT" & date_24>=td(01may2024) & date_24<=td(09oct2024)
replace fluke_min1_24=18.5 if state=="CT" & date_24>=td(01may2024) & date_24<=td(09oct2024)

replace fluke_bag1_24=4 if state=="NY" & date_24>=td(01may2024) & date_24<=td(09oct2024)
replace fluke_min1_24=18.5 if state=="NY" & date_24>=td(01may2024) & date_24<=td(09oct2024)

replace fluke_bag1_24=2 if state=="NJ" & date_24>=td(02may2024) & date_24<=td(27sep2024)
replace fluke_min1_24=17 if state=="NJ" & date_24>=td(02may2024) & date_24<=td(27sep2024)
replace fluke_max1=18 if state=="NJ" & date_24>=td(02may2024) & date_24<=td(27sep2024)

replace fluke_bag2=1 if state=="NJ" & date_24>=td(02may2024) & date_24<=td(27sep2024)
replace fluke_min2_24=18 if state=="NJ" & date_24>=td(02may2024) & date_24<=td(27sep2024)

replace fluke_bag1_24=4 if state=="DE" & date_24>=td(01jan2024) & date_24<=td(31dec2024)
replace fluke_min1_24=16 if state=="DE" & date_24>=td(01jan2024) & date_24<=td(31dec2024)

replace fluke_bag1_24=4 if state=="MD" & date_24>=td(01jan2024) & date_24<=td(31dec2024)
replace fluke_min1_24=16 if state=="MD" & date_24>=td(01jan2024) & date_24<=td(31dec2024)

replace fluke_bag1_24=4 if state=="VA" & date_24>=td(01jan2024) & date_24<=td(31dec2024)
replace fluke_min1_24=16 if state=="VA" & date_24>=td(01jan2024) & date_24<=td(31dec2024)

replace fluke_bag1_24=1 if state=="NC" & date_24>=td(01sep2024) & date_24<=td(30sep2024)
replace fluke_min1_24=15 if state=="NC" & date_24>=td(01sep2024) & date_24<=td(30sep2024)

*BSB regs 2024
replace bsb_bag_24=4 if state=="MA" & date_24>=td(21may2024) & date_24<=td(04sep2024)
replace bsb_min_24=16 if state=="MA" & date_24>=td(21may2024) & date_24<=td(04sep2024)

replace bsb_bag_24=2 if state=="RI" & date_24>=td(22may2024) & date_24<=td(31aug2024) & inlist(mode, "pr", "sh")
replace bsb_min_24=16 if state=="RI" & date_24>=td(22may2024) & date_24<=td(31aug2024) & inlist(mode, "pr", "sh")

replace bsb_bag_24=3 if state=="RI" & date_24>=td(01sep2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")
replace bsb_min_24=16 if state=="RI" & date_24>=td(01sep2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")

replace bsb_bag_24=2 if state=="RI" & date_24>=td(18jun2024) & date_24<=td(31aug2024) & inlist(mode, "fh")
replace bsb_min_24=16 if state=="RI" & date_24>=td(18jun2024) & date_24<=td(31aug2024) & inlist(mode, "fh")

replace bsb_bag_24=6 if state=="RI" & date_24>=td(01sep2024) & date_24<=td(31dec2024) & inlist(mode, "fh")
replace bsb_min_24=16 if state=="RI" & date_24>=td(01sep2024) & date_24<=td(31dec2024) & inlist(mode, "fh")

replace bsb_bag_24=5 if state=="CT" & date_24>=td(19may2024) & date_24<=td(01dec2024) & inlist(mode, "pr", "sh")
replace bsb_min_24=16 if state=="CT" & date_24>=td(19may2024) & date_24<=td(01dec2024) & inlist(mode, "pr", "sh")

replace bsb_bag_24=5 if state=="CT" & date_24>=td(19may2024) & date_24<=td(31aug2024) & inlist(mode, "fh")
replace bsb_min_24=16 if state=="CT" & date_24>=td(19may2024) & date_24<=td(31aug2024) & inlist(mode, "fh")

replace bsb_bag_24=7 if state=="CT" & date_24>=td(01sep2024) & date_24<=td(31dec2024) & inlist(mode, "fh")
replace bsb_min_24=16 if state=="CT" & date_24>=td(01sep2024) & date_24<=td(31dec2024) & inlist(mode, "fh")

replace bsb_bag_24=3 if state=="NY" & date_24>=td(23jun2024) & date_24<=td(31aug2024)
replace bsb_min_24=16 if state=="NY" & date_24>=td(23jun2024) & date_24<=td(31aug2024)

replace bsb_bag_24=6 if state=="NY" & date_24>=td(01sep2024) & date_24<=td(31dec2024)
replace bsb_min_24=16 if state=="NY" & date_24>=td(01sep2024) & date_24<=td(31dec2024)

replace bsb_bag_24=10 if state=="NJ" & date_24>=td(17may2024) & date_24<=td(19jun2024)
replace bsb_min_24=13 if state=="NJ" & date_24>=td(17may2024) & date_24<=td(19jun2024)

replace bsb_bag_24=2 if state=="NJ" & date_24>=td(01jul2024) & date_24<=td(31aug2024)
replace bsb_min_24=13 if state=="NJ" & date_24>=td(01jul2024) & date_24<=td(31aug2024)

replace bsb_bag_24=10 if state=="NJ" & date_24>=td(07oct2024) & date_24<=td(26oct2024)
replace bsb_min_24=13 if state=="NJ" & date_24>=td(07oct2024) & date_24<=td(26oct2024)

replace bsb_bag_24=15 if state=="NJ" & date_24>=td(01nov2024) & date_24<=td(31dec2024)
replace bsb_min_24=13 if state=="NJ" & date_24>=td(01nov2024) & date_24<=td(31dec2024)

replace bsb_bag_24=15 if inlist(state, "DE", "MD", "VA", "NC") & date_24>=td(15may2024) & date_24<=td(11dec2024)
replace bsb_min_24=13 if inlist(state, "DE", "MD", "VA", "NC") & date_24>=td(15may2024) & date_24<=td(11dec2024)


*scup regs 2024
replace scup_bag_24=30 if state=="MA" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")
replace scup_min_24=10 if state=="MA" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")

replace scup_bag_24=30 if state=="MA" & date_24>=td(01jan2024) & date_24<=td(30apr2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="MA" & date_24>=td(01jan2024) & date_24<=td(30apr2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="MA" & date_24>=td(01jul2024) & date_24<=td(31dec2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="MA" & date_24>=td(01jul2024) & date_24<=td(31dec2024) & inlist(mode, "fh")

replace scup_bag_24=50 if state=="MA" & date_24>=td(01may2024) & date_24<=td(30jun2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="MA" & date_24>=td(01may2024) & date_24<=td(30jun2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="RI" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")
replace scup_min_24=10 if state=="RI" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")

replace scup_bag_24=30 if state=="RI" & date_24>=td(01jan2024) & date_24<=td(31aug2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="RI" & date_24>=td(01jan2024) & date_24<=td(31aug2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="RI" & date_24>=td(01nov2024) & date_24<=td(31dec2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="RI" & date_24>=td(01nov2024) & date_24<=td(31dec2024) & inlist(mode, "fh")

replace scup_bag_24=50 if state=="RI" & date_24>=td(01sep2024) & date_24<=td(31oct2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="RI" & date_24>=td(01sep2024) & date_24<=td(31oct2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="CT" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")
replace scup_min_24=10 if state=="CT" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")

replace scup_bag_24=30 if state=="CT" & date_24>=td(01jan2024) & date_24<=td(31aug2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="CT" & date_24>=td(01jan2024) & date_24<=td(31aug2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="CT" & date_24>=td(01nov2024) & date_24<=td(31dec2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="CT" & date_24>=td(01nov2024) & date_24<=td(31dec2024) & inlist(mode, "fh")

replace scup_bag_24=50 if state=="CT" & date_24>=td(01sep2024) & date_24<=td(31oct2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="CT" & date_24>=td(01sep2024) & date_24<=td(31oct2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="NY" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")
replace scup_min_24=10 if state=="NY" & date_24>=td(01jan2024) & date_24<=td(31dec2024) & inlist(mode, "pr", "sh")

replace scup_bag_24=30 if state=="NY" & date_24>=td(01jan2024) & date_24<=td(31aug2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="NY" & date_24>=td(01jan2024) & date_24<=td(31aug2024) & inlist(mode, "fh")

replace scup_bag_24=30 if state=="NY" & date_24>=td(01nov2024) & date_24<=td(31dec2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="NY" & date_24>=td(01nov2024) & date_24<=td(31dec2024) & inlist(mode, "fh")

replace scup_bag_24=50 if state=="NY" & date_24>=td(01sep2024) & date_24<=td(31oct2024) & inlist(mode, "fh")
replace scup_min_24=10 if state=="NY" & date_24>=td(01sep2024) & date_24<=td(31oct2024) & inlist(mode, "fh")

replace scup_bag_24=50 if state=="NJ" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 
replace scup_min_24=10 if state=="NJ" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 

replace scup_bag_24=50 if state=="DE" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 
replace scup_min_24=9 if state=="DE" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 

replace scup_bag_24=50 if state=="MD" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 
replace scup_min_24=9 if state=="MD" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 

replace scup_bag_24=30 if state=="VA" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 
replace scup_min_24=9 if state=="VA" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 

replace scup_bag_24=50 if state=="NC" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 
replace scup_min_24=9 if state=="NC" & date_24>=td(01jan2024) & date_24<=td(31dec2024) 
*/



