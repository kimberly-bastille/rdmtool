cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

/*
foreach wave in 20001 20002 20003 20004 20005 20006 ///
						20011 20012 20013 20014 20015 20016 ///
						20021 20022 20023 20024 20025 20026 ///
						20031 20032 20033 20034 20035 20036 ///
						20041 20042 20043 20044 20045 20046 ///
						20051 20052 20053 20054 20055 20056 ///
						20061 20062 20063 20064 20065 20066 ///
						20071 20072 20073 20074 20075 20076 ///
						20081 20082 20083 20084 20085 20086 ///
						20091 20092 20093 20094 20095 20096 ///
						20101 20102 20103 20104 20105 20106 ///
						20111 20112 20113 20114 20115 20116 ///
						20121 20122 20123 20124 20125 20126 ///
						20131 20132 20133 20134 20135 20136 ///
						20141 20142 20143 20144 20145 20146 ///
						20151 20152 20153 20154 20155 20156 ///
						20161 20162 20163 20164 20165 20166 ///
						20171 20172 20173 20174 20175 20176 ///
						20181 20182 20183 20184 20185 20186 ///
						20191 20192 20193 20194 20195 20196 {
use trip_`wave'.dta, clear
renvars, lower
save trip_`wave'.dta, replace

/*
use size_b2_`wave'.dta, clear
renvars, lower
save size_b2_`wave'.dta, replace
*/
use size_`wave'.dta, clear
renvars, lower
save size_`wave'.dta, replace

use catch_`wave'.dta, clear
renvars, lower
save catch_`wave'.dta, replace

}
*/
set matsize 10000
/* Use these to control the years for which the MRIP data is polled/queried*/

global yearlist  2017 2022
global wavelist 1 2 3 4 5 6


/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

*B2 Files
global b2list
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}



/*SIZE_LIST */
global sizelist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}




clear

mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2017)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
gen common_dom="zzzzzz"
replace common_dom="SF" if strmatch(common, "summerflounder") 
 


tostring wave, gen(w2)
tostring year, gen(year2)


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")



gen my_dom_id_string=year2+"_"+ common_dom

gen my_dom_id_string=state+"_"+year2+"_"+mode1+"_"+ common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)



/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "summerflounder")==0

drop if strmatch(common, "summerflounder")==0


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)
encode  my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore


svy: mean wgt, over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor


split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string


split my, parse("_")
rename my_dom_id_string1 state
rename my_dom_id_string3 mode
drop my_dom_id_string my_dom_id_string2 my_dom_id_string4

gen state_mode = state+"_"+mode 
rename b mean_weight
rename se se_weight

mvencode se, mv(0)

tempfile base 
save `base', replace 

global drawz
levelsof state_mode, local(st_modes)
foreach s of local st_modes{
	u `base', clear 
	keep if state_mode=="`s'"
	
	su mean_weight
	local mean = `r(mean)'
	su se_weight
	local sd = `r(mean)'
	
	clear 
	set obs 100
	gen draw=_n
	
	if `sd'!=0{
	gen mean_weight= rnormal(`mean', `sd')
	 }
	
	else{
	gen mean_weight= `mean'
	 }
	
	
	gen state_mode="`s'"
	gen species="SF"
	
	tempfile drawz`s'
	save `drawz`s'', replace
	global drawz "$drawz "`drawz`s''" " 

}

dsconcat $drawz

tempfile sf
save `sf', replace 





**********BSB
clear

mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
gen common_dom="zzzzzz"
replace common_dom="SF" if strmatch(common, "blackseabass") 
 


tostring wave, gen(w2)
tostring year, gen(year2)


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")




gen my_dom_id_string=state+"_"+year2+"_"+mode1+"_"+ common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)



/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "blackseabass")==0

drop if strmatch(common, "blackseabass")==0


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)
encode  my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore


svy: mean wgt, over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor


split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string


split my, parse("_")
rename my_dom_id_string1 state
rename my_dom_id_string3 mode
drop my_dom_id_string my_dom_id_string2 my_dom_id_string4

gen state_mode = state+"_"+mode 
rename b mean_weight
rename se se_weight

mvencode se, mv(0)

tempfile base 
save `base', replace 

global drawz
levelsof state_mode, local(st_modes)
foreach s of local st_modes{
	u `base', clear 
	keep if state_mode=="`s'"
	
	su mean_weight
	local mean = `r(mean)'
	su se_weight
	local sd = `r(mean)'
	
	clear 
	set obs 100
	gen draw=_n
	
	if `sd'!=0{
	gen mean_weight= rnormal(`mean', `sd')
	 }
	
	else{
	gen mean_weight= `mean'
	 }
	
	
	gen state_mode="`s'"
	gen species="BSB"
	
	tempfile drawz`s'
	save `drawz`s'', replace
	global drawz "$drawz "`drawz`s''" " 

}

dsconcat $drawz

tempfile bsb
save `bsb', replace 



***SCUP
clear

mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 

dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year,  2022)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
*gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
*replace region="SO" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY")
replace region="NJ" if inlist(state, "NJ")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
gen common_dom="zzzzzz"
replace common_dom="SF" if strmatch(common, "scup") 
 


tostring wave, gen(w2)
tostring year, gen(year2)


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")




gen my_dom_id_string=state+"_"+year2+"_"+mode1+"_"+ common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)



/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "scup")==0

drop if strmatch(common, "scup")==0


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)
encode  my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore


svy: mean wgt, over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor


split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string


split my, parse("_")
rename my_dom_id_string1 state
rename my_dom_id_string3 mode
drop my_dom_id_string my_dom_id_string2 my_dom_id_string4

gen state_mode = state+"_"+mode 
rename b mean_weight
rename se se_weight

mvencode se, mv(0)

tempfile base 
save `base', replace 

global drawz
levelsof state_mode, local(st_modes)
foreach s of local st_modes{
	u `base', clear 
	keep if state_mode=="`s'"
	
	su mean_weight
	local mean = `r(mean)'
	su se_weight
	local sd = `r(mean)'
	
	clear 
	set obs 100
	gen draw=_n
	
	if `sd'!=0{
	gen mean_weight= rnormal(`mean', `sd')
	 }
	
	else{
	gen mean_weight= `mean'
	 }
	
	
	gen state_mode="`s'"
	gen species="SCUP"
	
	tempfile drawz`s'
	save `drawz`s'', replace
	global drawz "$drawz "`drawz`s''" " 

}

dsconcat $drawz

append using `bsb'
append using `sf'

replace mean_weight=mean_weight*2.20462
split state_mode, parse(_)
rename state_mode1 state
rename state_mode2 mode1
order state mode1 draw species 
drop state_mode

reshape wide mean_weight, i(state mode draw) j(species) string


export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\RDM 2024 testing\MRIP mean harvest weights.csv", replace 




**********Discard weights 
**SF
*MRIP release data 
clear 
mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeb2

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1', replace
clear

 

dsconcat $b2list
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen



gen my_dom_id_string=state+"_"+year2+"_"+mode1+"_"+ common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)



/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
*replace l_in_bin=0 if strmatch(common, "summerflounder")==0

drop if strmatch(common, "summerflounder")==0


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_catch], strata(strat_id) singleunit(certainty)
encode  my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore


svy: mean wgt, over(my_dom_id)
xsvmat, from(r(table)') rownames(rname) names(col) norestor


split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string


split my, parse("_")
rename my_dom_id_string1 state
rename my_dom_id_string3 mode
drop my_dom_id_string my_dom_id_string2 my_dom_id_string4

gen state_mode = state+"_"+mode 
rename b mean_weight
rename se se_weight

mvencode se, mv(0)

tempfile base 
save `base', replace 

global drawz
levelsof state_mode, local(st_modes)
foreach s of local st_modes{
	u `base', clear 
	keep if state_mode=="`s'"
	
	su mean_weight
	local mean = `r(mean)'
	su se_weight
	local sd = `r(mean)'
	
	clear 
	set obs 100
	gen draw=_n
	
	if `sd'!=0{
	gen mean_weight= rnormal(`mean', `sd')
	 }
	
	else{
	gen mean_weight= `mean'
	 }
	
	
	gen state_mode="`s'"
	gen species="SF"
	
	tempfile drawz`s'
	save `drawz`s'', replace
	global drawz "$drawz "`drawz`s''" " 

}

dsconcat $drawz

tempfile sf
save `sf', replace 









	svy: tab l_in_bin my_dom_id_string, count
	/*save some stuff  
	matrix of proportions, row names, column names, estimate of total population size*/
	mat eP=e(Prop)
	mat eR=e(Row)'
	mat eC=e(Col)
	local PopN=e(N_pop)

	local mycolnames: colnames(eC)
	mat colnames eP=`mycolnames'
	
	clear
	/*read the eP into a dataset and convert proportion of population into numbers*/
	svmat eP, names(col)
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}
	/*read in the "row" */
svmat eR
order eR
rename eR l_in_bin
	
ds l, not
renvarlab `r(varlist)', prefix(i)
	
reshape long i, i(l_in) j(new) string
split new, parse(_)
rename new1 region
rename new2 year
destring year, replace 
drop new
rename i harvest

*MRIP release data 
clear 
mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeb2

*local years  2019
*foreach y of local years{

*local y 2019
clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1', replace
clear

 

dsconcat $b2list
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year, 2022)
/* THIS IS THE END OF THE DATA MERGING CODE */

tostring wave, gen(w2)
tostring year, gen(year2)
 /* ensure only relevant states */
 keep if inlist(st,  25, 44, 9, 36, 34, 10, 24, 51, 37)


/*This is the "full" mrip data */
tempfile tc1
save `tc1', replace

gen st2 = string(st,"%02.0f")

gen state="NH" if st==33
replace state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

*Here, group together NC and VA
*replace state="VA" if st==37


 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
 gen common_dom="zzzzzz"
 replace common_dom="SF" if strmatch(common, "summerflounder") 
 
 keep if common_dom=="SF"
 
 
gen my_dom_id_string=state+"_"+year2+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)

encode  my_dom_id_string, gen(my_dom_id1)
 
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)

svy: mean wgt, over(my_dom_id1)

 
 
 
 
 keep state l_in_bin year
 rename l_in_bin length 
 gen source="MRIP"
