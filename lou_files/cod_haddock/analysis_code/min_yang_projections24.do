

***********Lou calibration stats
import excel using "C:\Users\andrew.carr-harris\Desktop\cod_hadd_RDM\calibration_stats.xlsx", clear first 
collapse (sum)  tot_keep_cod_model tot_keep_hadd_model tot_rel_cod_model tot_rel_hadd_model   ///
					    tot_cat_cod_model tot_cat_hadd_model estimated_trips , by(open draw mode)
rename estimated dtrip_model

collapse (mean)  tot_keep_cod_model tot_keep_hadd_model tot_rel_cod_model tot_rel_hadd_model   ///
					     tot_cat_cod_model tot_cat_hadd_model dtrip_model  ///
			 (sd)      sd_tot_keep_cod_model = tot_keep_cod_model  ///
						 sd_tot_keep_hadd_model = tot_keep_hadd_model  ///
						 sd_tot_rel_cod_model = tot_rel_cod_model  ///
						 sd_tot_rel_hadd_model = tot_rel_hadd_model  ///
					     sd_tot_cat_cod_model = tot_cat_cod_model  ///
						 sd_tot_cat_hadd_model = tot_cat_hadd_model  ///
						 sd_dtrip_model = dtrip_model, by(mode open)

local vars tot_keep_cod_model tot_keep_hadd_model tot_rel_cod_model tot_rel_hadd_model   ///
					     tot_cat_cod_model tot_cat_hadd_model dtrip_model
foreach v of local vars{
gen u_`v'= `v'+1.96*sd_`v' 
gen l_`v'= `v'-1.96*sd_`v' 
}	

ds mode open, not
renvarlab `r(varlist)', postdrop(6)
drop sd*
gen source="Lou"
tempfile model
save `model', replace 

import delimited using "$draw_file_cd\MRIP_catch_totals_open_season.csv", clear 
merge 1:1 mode season using "$draw_file_cd\MRIP_dtrip_totals_open_season.dta"
order mode season _ dtrip
mvencode dtrip, mv(0) override
gen open=1 if season=="op"
replace open=0 if season=="cl"
drop _merge
rename dtrip dtrip_mrip
drop season 
order mode open 

rename dtrip_mrip dtrip
rename ll_dtrip l_dtrip 
rename ul_dtrip u_dtrip
rename cod_catch_mrip tot_cat_cod
rename  llcod_catch_mrip l_tot_cat_cod
rename  ulcod_catch_mrip u_tot_cat_cod
rename  cod_keep_mrip tot_keep_cod
rename  llcod_keep_mrip l_tot_keep_cod
rename  ulcod_keep_mrip  u_tot_keep_cod
rename  cod_rel_mrip  tot_rel_cod
rename  llcod_rel_mrip  l_tot_rel_cod
rename  ulcod_rel_mrip u_tot_rel_cod
rename  hadd_catch_mrip  tot_cat_hadd
rename  llhadd_catch_mrip l_tot_cat_hadd
rename  ulhadd_catch_mrip u_tot_cat_hadd
rename  hadd_keep_mrip  tot_keep_hadd
rename  llhadd_keep_mrip l_tot_keep_hadd
rename  ulhadd_keep_mrip  u_tot_keep_hadd
rename  hadd_rel_mrip  tot_rel_hadd
rename  llhadd_rel_mrip  l_tot_rel_hadd
rename  ulhadd_rel_mrip u_tot_rel_hadd
gen source="MRIP"

append using `model'

tostring open, gen(open2)
gen domain=mode+"_"+open2
order domain

tempfile new 
save `new', replace

local vars dtrip tot_cat_cod tot_keep_cod tot_rel_cod tot_cat_hadd tot_keep_hadd tot_rel_hadd 

preserve
local v tot_rel_hadd

tempfile new 
save `new', replace

levelsof domain, local(domz)
foreach d of local domz{
	u `new', clear
	keep if domain=="`d'"
	sort source
	gen x=_n+1

	label  define x    2  "Lou"  3 "MRIP"
	label  value x x

twoway (scatter `v' x , msymbol(S) msize(vsmall)) ///
			 (rcap u_`v' l_`v' x, ), ///
			 xlabel(1(1)4, valuelabel labsize(small)) xtitle(" ") ///
			 title(`d', size(medium)) name(dom`d'`v', replace)  ytitle("") ylab(, labsize(vsmall) angle(45)) legend(off)
			 
local graphnames `graphnames' dom`d'`v'
			 
}
grc1leg `graphnames', title(`v')
gr play legendoff
graph export "C:\Users\andrew.carr-harris\Desktop\cod_hadd_RDM\tot_rel_hadd.png", as(png) name("Graph") replace

gr drop _all
restore

/****************************/
/****************************/
/* Haddock and Cod rec ACLs in mt*/
/****************************/
/****************************/
global haddock_mort2022=666


global cod_recACL2022=192
global hadd_recACL2022=3634

global cod_recACL2023=192
global hadd_recACL2023=610


global mrip_vintage "2024_01_02"

global hadd_recACL2024=759
global cod_recACL2024=192

global cod_calibration_adj= 12.7
global hadd_calibration_adj= 56.6


global project_dir  "//nefscfile/BLAST/READ-SSB-Lee-BLAST/cod_haddock_fy2024" 
global output_dir "${project_dir}/output"

/* Read in all the Econ model runs that match `stub' */
local estub "economic_data_2024_SQ_"

local estub2 "economic_data_2024_set1"
local estub3 "economic_data_2024_set5"
local estub4 "economic_data_2024_set3"
local estub5 "economic_data_2024_set4"

local estub6 "economic_data_2024_set6"

/*local estub3 "economic_data_2024_RAP_"
local estub4 "economic_data_2024_CMTE_" */

local efilelist1: dir "${output_dir}" files "`estub'*.dta"

local efilelist2: dir "${output_dir}" files "`estub2'*.dta"
local efilelist3: dir "${output_dir}" files "`estub3'*.dta"

local efilelist4: dir "${output_dir}" files "`estub4'*.dta"
local efilelist5: dir "${output_dir}" files "`estub5'*.dta"
local efilelist6: dir "${output_dir}" files "`estub6'*.dta"

*local ecombinedfiles `" `efilelist1' `efilelist2'  `efilelist3' `efilelist4' "'

local ecombinedfiles `" `efilelist1' `efilelist2'  `efilelist3' `efilelist4'  `efilelist5'  `efilelist6' "'


/****************************/
/****************************/
/* Read in results and parse the source  */
/****************************/
/****************************/
clear
gen str40 source=""
foreach file of local ecombinedfiles{
capture append using ${output_dir}/`file'
replace source="`file'"  if source==""
}
keep scenario scenario_num month replicate WTP


tempfile econ_data
save `econ_data', replace


global project_dir  "//nefscfile/BLAST/READ-SSB-Lee-BLAST/cod_haddock_fy2024" 
global output_dir "${project_dir}/output"

global working_year 2024
global previous=$working_year-1

/* Read in all the model runs that match `stub' */
local stub "recreational_catches_2024_SQ_"

local stub2 "recreational_catches_2024_set1"
local stub3 "recreational_catches_2024_set5"
local stub4 "recreational_catches_2024_set3"
local stub5 "recreational_catches_2024_set4"
local stub6 "recreational_catches_2024_set6"
/*
local stub3 "recreational_catches_2024_RAP_"
local stub4 "recreational_catches_2024_CMTE_" */

local filelist1: dir "${output_dir}" files "`stub'*.dta"

local filelist2: dir "${output_dir}" files "`stub2'*.dta"
local filelist3: dir "${output_dir}" files "`stub3'*.dta"
local filelist4: dir "${output_dir}" files "`stub4'*.dta" 
local filelist5: dir "${output_dir}" files "`stub5'*.dta" 
local filelist6: dir "${output_dir}" files "`stub6'*.dta" 
*local combinedfiles `" `filelist1' `filelist2' `filelist3' `filelist4' `filelist5' "'

local combinedfiles `" `filelist1' `filelist2' `filelist3' `filelist4' `filelist5' `filelist6' "'


/****************************/
/****************************/
/* Read in results and parse the source  */
/****************************/
/****************************/
clear
gen str40 source=""
foreach file of local combinedfiles{
capture append using ${output_dir}/`file'
replace source="`file'"  if source==""
}

merge 1:1 scenario scenario_num month replicate using `econ_data'

split source, parse("/") gen(ss)
scalar rnvars=r(nvars)
local all=r(varlist)
local m="ss"+scalar(rnvars)
local dropper : list all - m


/****************************/
/****************************/
/* Tidy ups
Drop the rows corresponding to the previous fishing year. 
Generate the fishing year variable
construct total catch (numbers) and mortality (mt)
 */
/****************************/
/****************************/

drop if month<=4
gen fishing_year=$working_year
gen year=fishing_year
gen month_of_fy=month-4

replace year=year+1 if month>=13
replace month=month-12 if month>=13


/* need to sum up the FH and PR modes, by "scenario" */

/* create the SQ with 23" cod scenario */
expand 2 if inlist(scenario_num, 9,12), gen(marker)
replace scenario_num=108 if inlist(scenario_num, 9,12) & marker==1
drop marker

/* 
110 October closed with the FH regulations
	Combine the FH part of 107 with Scenario 21
 */


expand 2 if inlist(scenario_num, 17), gen(marker)
replace scenario_num=110 if inlist(scenario_num, 17) & marker==1
drop marker


/*
111 October closed, FH and 23" cod  
	Combine the FH part of 108 with scenario 22 */

expand 2 if inlist(scenario_num, 9), gen(marker)
replace scenario_num=111 if inlist(scenario_num, 9) & marker==1
drop marker


replace scenario_num=0 if inlist(scenario_num, 3, 4)
replace scenario_num=101 if inlist(scenario_num, 5, 6)
replace scenario_num=102 if inlist(scenario_num, 7, 8)
replace scenario_num=103 if inlist(scenario_num, 9, 10)
replace scenario_num=104 if inlist(scenario_num, 11,12)
replace scenario_num=105 if inlist(scenario_num, 13, 14)
replace scenario_num=106 if inlist(scenario_num, 15,16)
replace scenario_num=107 if inlist(scenario_num, 17,18)
replace scenario_num=109 if inlist(scenario_num, 19,20)
replace scenario_num=110 if inlist(scenario_num, 21,110)
replace scenario_num=111 if inlist(scenario_num, 22,111)
replace scenario_num=112 if inlist(scenario_num, 27,28)
replace scenario_num=113 if inlist(scenario_num, 29,30)
replace scenario_num=114 if inlist(scenario_num, 23,24)
replace scenario_num=115 if inlist(scenario_num, 25,26)

/*I sum over source, but I'm not sure what will happen if I remove that. So I'm just relabeling it 'combined' for the simulations that patch together partial scenarios from different runs*/
replace source="combined" if inlist(scenario_num,110,111)

/* aggregate the fleet outcomes to the scenario level*/
collapse (sum) choice_occasions total_trips cod_num_kept cod_num_released haddock_num_kept haddock_num_released cod_kept_mt cod_released_mt cod_released_dead_mt hadd_kept_mt hadd_released_mt hadd_released_dead_mt crep hrep codbag_comply cod_sublegal_keep cod_release_mort hadd_release_mort WTP ,by(scenario_num fishing_year year month replicate month_of_fy source)

tempfile s_as_run
save `s_as_run', replace emptyok



use `s_as_run', clear

foreach var of varlist choice_occasions total_trips cod_num_kept cod_num_released haddock_num_kept haddock_num_released cod_kept_mt cod_released_mt cod_released_dead_mt hadd_kept_mt hadd_released_mt hadd_released_dead_mt WTP{
	replace `var'=`var'/2 if month==4
}

/* Leaving the machinery to weight different scenarios, if necessary*/

gen freq=1

/*compute averages if necessary*/
collapse (mean) choice_occasions total_trips cod_num_kept cod_num_released haddock_num_kept haddock_num_released cod_kept_mt cod_released_mt cod_released_dead_mt hadd_kept_mt hadd_released_mt hadd_released_dead_mt crep hrep codbag_comply cod_sublegal_keep cod_release_mort hadd_release_mort WTP [fweight=freq] ,by(scenario_num fishing_year year month replicate month_of_fy source)


tempfile smodified
save `smodified', replace emptyok

gen cod_tot_cat=cod_num_kept+cod_num_released
gen hadd_tot_cat=haddock_num_kept+haddock_num_released

sort month
order cod_tot_cat hadd_tot_cat, after(month)
format *num* %09.1gc
format *mt %06.1fc
format *tot_cat %09.1gc

gen cod_mort_mt=cod_kept_mt+cod_released_dead_mt
gen hadd_mort_mt=hadd_kept_mt+hadd_released_dead_mt

gen haddock_relptrip=haddock_num_released/total_trips
gen haddock_landptrip=haddock_num_kept/total_trips

gen cod_relptrip=cod_num_released/total_trips
gen cod_landptrip=cod_num_kept/total_trips

label var haddock_relptrip "Haddock released/ trip"
label var haddock_landptrip "Haddock landed/ trip"
label var cod_landptrip "Cod landed/trip"
label var cod_relptrip "Cod released/trip"
label var total_trips  "Trips"
label var cod_mort_mt  "Cod Mortality (mt)"
label var hadd_mort_mt "Haddock Mortality (mt)"
label var cod_kept_mt "Cod Kept (mt)"
label var cod_released_mt "Cod Released (mt)"
label var hadd_kept_mt "Haddock Kept (mt)"
label var hadd_released_mt "Haddock Released(mt)"
label var cod_num_kept "Cod kept (#)"
label var cod_num_released "Cod released (#)"
label var haddock_num_kept "Haddock kept (#)"
label var haddock_num_released "Haddock relased (#)"
label var WTP "Willingness-to-Pay"

tempfile raw_results
save `raw_results'


/* scenario names */

sort scenario_num
gen cod_limit=1 

gen str60 hadd_limit=`"FH 15, PR 10"' if inlist(scenario_num,0,107,108,109,114,115) 
replace hadd_limit=`"15"' if inlist(scenario_num,101,103,105,110,111)
replace hadd_limit=`"10"' if inlist(scenario_num,102,104,106,112,113)

gen str60 hadd_size=`"FH 18", PR 17""' if inlist(scenario_num,0,107,108,109)
replace hadd_size=`"18"' if inlist(scenario_num,101,103,105,110,111,112,113,114,115)
replace hadd_size=`"17"' if inlist(scenario_num,102,104,106)


gen cod_size=`"22""' 
replace cod_size=`"23""' if inlist(scenario_num,103,104,108,109,111,113,115)
replace cod_size=`"24""' if inlist(scenario_num,105,106)
gen str20 mode="Combined"

/*cod open seasons */
gen str60  cod_open_season="Sept 1 -Oct 31" if inlist(scenario_num,0,101,102,103,104,105,106,108,112,113,114,115)
replace cod_open_season="Sept 1 -Sept 30" if inlist(scenario_num,107,109,110,111)

/*haddock open seasons */
gen str60 had_open_season=" May-Feb28, Apr 1-Apr30" if inlist(scenario_num,0,101,102,103,104,105,106,107,108,109,110,112,113,114,115)

save `raw_results', replace


/****************************/
/****************************/
/* Contract to annual  */
/****************************/
/****************************/
collapse (sum) total_trips cod_num_kept cod_num_released haddock_num_kept haddock_num_released cod_mort_mt hadd_mort_mt cod_kept_mt cod_released_mt hadd_kept_mt hadd_released_mt  haddock_relptrip haddock_landptrip cod_landptrip cod_relptrip   hadd_released_dead_mt cod_released_dead_mt WTP (first) cod_limit cod_size hadd_limit hadd_size mode cod_open_season had_open_season, by(scenario source scenario_num replicate )

/* Flag replicates where mortality less than subacl*/

gen cod_ok=0
replace cod_ok=1 if cod_mort_mt<=$cod_recACL2024
gen adj_cod_ok=0

gen adj_cod_mort_mt=cod_mort_mt + $cod_calibration_adj
replace adj_cod_ok=1 if adj_cod_mort_mt<=$cod_recACL2024

gen hadd_ok=0 
replace hadd_ok=1 if hadd_mort_mt<=$hadd_recACL2024

gen adj_hadd_ok=0
gen adj_hadd_mort_mt=hadd_mort_mt+ $hadd_calibration_adj
replace adj_hadd_ok=1 if adj_hadd_mort_mt<=$hadd_recACL2024

format adj_hadd_mort_mt adj_cod_mort_mt %6.1gc

/*Adjust cod mortality and haddock mortality components. 
I've added amount of motality to the cod and haddock components. I need to allocate that to 
*/

gen adj_cod_kept_mt=cod_kept_mt + cod_kept_mt/(cod_kept_mt+cod_released_dead_mt)*$cod_calibration_adj
gen adj_cod_released_dead_mt=cod_released_dead_mt+ cod_released_dead_mt/(cod_kept_mt+cod_released_dead_mt)*$cod_calibration_adj
gen adj_cod_released_mt=cod_released_mt+ cod_released_mt/(cod_kept_mt+cod_released_dead_mt)*$cod_calibration_adj

gen adj_hadd_kept_mt=hadd_kept_mt + hadd_kept_mt/(hadd_kept_mt+hadd_released_dead_mt)*$hadd_calibration_adj
gen adj_hadd_released_dead_mt=hadd_released_dead_mt+ hadd_released_dead_mt/(hadd_kept_mt+hadd_released_dead_mt)*$hadd_calibration_adj
gen adj_hadd_released_mt=hadd_released_mt+ hadd_released_mt/(hadd_kept_mt+hadd_released_dead_mt)*$hadd_calibration_adj


label var haddock_relptrip "Haddock released/ trip"
label var haddock_landptrip "Haddock landed/ trip"
label var cod_landptrip "Cod landed/trip"
label var cod_relptrip "Cod released/trip"
label var total_trips  "Trips"
label var cod_mort_mt  "Cod Mortality (mt)"
label var hadd_mort_mt "Haddock Mortality (mt)"
label var cod_kept_mt "Cod Kept (mt)"
label var cod_released_mt "Cod Released (mt)"
label var hadd_kept_mt "Haddock Kept (mt)"
label var hadd_released_mt "Haddock Released(mt)"
label var cod_ok "% Under Cod subACL"
label var hadd_ok "% Under Haddock subACL"
label var adj_cod_mort_mt  "Adjusted Cod Mortality (mt)"
label var adj_hadd_mort_mt "Adjusted Haddock Mortality (mt)"
label var adj_cod_ok "Adjusted % Under Cod subACL"
label var adj_hadd_ok "Adjusted % Under Haddock subACL
label var source "Where is this data"
label var cod_num_kept "Cod kept (#)"
label var cod_num_released "Cod released (#)"
label var haddock_num_kept "Haddock kept (#)"
label var haddock_num_released "Haddock relased (#)"
label var adj_cod_kept_mt "Adjusted Cod Kept (mt)"
label var adj_cod_released_dead_mt  "Adjusted Cod Released Dead (mt)"
label var adj_hadd_kept_mt "Adjusted Haddock Kept (mt)"
label var adj_hadd_released_dead_mt "Adjusted Haddock Released Dead (mt)"

keep if scenario==0
drop source


keep scenario_num replicate total_trips cod_num_kept cod_num_released haddock_num_kept haddock_num_released adj_cod_mort_mt adj_hadd_mort_mt adj_cod_kept_mt adj_cod_released_dead_mt adj_hadd_kept_mt adj_hadd_released_dead_mt WTP cod_mort_mt hadd_mort_mt cod_kept_mt cod_released_mt hadd_kept_mt hadd_released_mt hadd_released_dead_mt cod_released_dead_mt adj_cod_released_mt adj_hadd_released_mt cod_ok  adj_cod_ok hadd_ok adj_hadd_ok
gen source="Min-Yang"


preserve
import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check.xlsx", clear first /*This is the first set of results - model run in inches */
gen source = "Lou (inches)"
tempfile a
save `a', replace

import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_cm.xlsx", clear first /*This is the 2nd set of results - model run in cm's*/ 
gen source = "Lou (cm)"
tempfile b
save `b', replace

import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_inches5.xlsx", clear first /*This is the 3rd set of results - model run in inches but add half inch after 
																																											  fitting gamma dist'n to rec. catch-at-length*/ 
gen source = "Lou (inches + 0.5)"

append using `a'
append using `b'

gen domain=""
replace domain = "cod_harvest_weight" if Category=="cod" & catch_disp=="keep" & param=="Total" & number_weight=="Weight"
replace domain = "hadd_harvest_weight" if Category=="had" & catch_disp=="keep"  & param=="Total" & number_weight=="Weight"
replace domain = "cod_release_weight" if Category=="cod" & catch_disp=="release" & param=="Total" & number_weight=="Weight"
replace domain = "hadd_release_weight" if Category=="had" & catch_disp=="release"  & param=="Total" & number_weight=="Weight"
replace domain = "cod_disc_mort_weight" if Category=="cod" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Weight"
replace domain = "hadd_disc_mort_weight" if Category=="had" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Weight"

replace domain = "cod_harvest_number" if Category=="cod" & catch_disp=="keep" & param=="Total" & number_weight=="Number"
replace domain = "hadd_harvest_number" if Category=="had" & catch_disp=="keep"  & param=="Total" & number_weight=="Number"
replace domain = "cod_release_number" if Category=="cod" & catch_disp=="release" & param=="Total" & number_weight=="Number"
replace domain = "hadd_release_number" if Category=="had" & catch_disp=="release"  & param=="Total" & number_weight=="Number"
replace domain = "cod_disc_mort_number" if Category=="cod" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Number"
replace domain = "hadd_disc_mort_number" if Category=="had" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Number"

replace domain = "CV" if Category=="CV"
replace domain = "ntrips" if Category=="ntrips"
replace domain = "nchoiceoccasions" if Category=="nchoiceoccasions"
replace domain = "codkeepsum" if Category=="codkeepsum"
replace domain = "codrelsum" if Category=="codrelsum"
replace domain = "haddkeepsum" if Category=="haddkeepsum"
replace domain = "haddrelsum" if Category=="haddrelsum"

collapse (sum) Value, by(domain run source)
reshape wide Value, i( run source) j(domain) string

ds run source, not
renvarlab `r(varlist)', predrop(5)

replace cod_harvest_weight=cod_harvest_weight/2205
replace hadd_harvest_weight=hadd_harvest_weight/2205

replace hadd_disc_mort_weight=hadd_disc_mort_weight/2205
replace cod_disc_mort_weight=cod_disc_mort_weight/2205

replace hadd_release_weight=hadd_release_weight/2205
replace cod_release_weight=cod_release_weight/2205

gen replicate=_n
gen scenario_num=0

        
rename ntrips total_trips
rename cod_harvest_number cod_num_kept
rename cod_release_number cod_num_released
rename hadd_harvest_number haddock_num_kept
rename hadd_release_number haddock_num_released

rename cod_harvest_weight cod_kept_mt
rename hadd_harvest_weight hadd_kept_mt

rename cod_release_weight cod_released_mt
rename hadd_release_weight hadd_released_mt

rename cod_disc_mort_weight cod_released_dead_mt
rename hadd_disc_mort_weight hadd_released_dead_mt

gen cod_mort_mt = cod_kept_mt+cod_released_dead_mt
gen hadd_mort_mt = hadd_kept_mt+hadd_released_dead_mt

gen adj_cod_released_dead_mt  =  cod_released_dead_mt
gen adj_hadd_released_dead_mt  =  hadd_released_dead_mt

gen adj_cod_kept_mt  =  cod_kept_mt
gen adj_hadd_kept_mt  =  hadd_kept_mt

gen adj_cod_mort_mt  =  cod_mort_mt
gen adj_hadd_mort_mt  =  hadd_mort_mt

gen adj_cod_released_mt  =  cod_released_mt    
gen adj_hadd_released_mt  =  hadd_released_mt    


gen cod_ok=0
replace cod_ok=1 if cod_mort_mt<=$cod_recACL2024

gen adj_cod_ok=0
replace adj_cod_ok=1 if adj_cod_mort_mt<=$cod_recACL2024

gen hadd_ok=0 
replace hadd_ok=1 if hadd_mort_mt<=$hadd_recACL2024

gen adj_hadd_ok=0
replace adj_hadd_ok=1 if adj_hadd_mort_mt<=$hadd_recACL2024


rename CV WTP
keep source replicate scenario total_trips adj_cod_released_mt  adj_hadd_released_mt cod_num_kept cod_num_released haddock_num_kept haddock_num_released adj_cod_released_dead_mt adj_hadd_released_dead_mt adj_cod_kept_mt adj_hadd_kept_mt adj_cod_mort_mt adj_hadd_mort_mt WTP cod_kept_mt hadd_kept_mt cod_released_mt hadd_released_mt cod_released_dead_mt hadd_released_dead_mt cod_mort_mt hadd_mort_mt cod_ok  adj_cod_ok hadd_ok adj_hadd_ok
  
*gen source="Lou"
tempfile lou
save `lou', replace
restore

append using `lou'

gen cod_tot_num_cat=cod_num_kept+cod_num_released
gen haddock_tot_num_cat=haddock_num_kept+haddock_num_released


gen adj_cod_avg_harvest_wt=adj_cod_kept_mt/cod_num_kept
gen adj_hadd_avg_harvest_wt=adj_hadd_kept_mt/haddock_num_kept

gen adj_cod_avg_release_wt=adj_cod_released_mt/cod_num_released
gen adj_hadd_avg_release_wt=adj_hadd_released_mt/haddock_num_released

gen adj_cod_avg_release_dead_wt=adj_cod_released_dead_mt/cod_num_released
gen adj_hadd_avg_release_dead_wt=adj_hadd_released_dead_mt/haddock_num_released



gen cod_avg_harvest_wt=cod_kept_mt/cod_num_kept
gen hadd_avg_harvest_wt=hadd_kept_mt/haddock_num_kept

gen cod_avg_release_wt=cod_released_mt/cod_num_released
gen had_avg_release_wt=hadd_released_mt/haddock_num_released

gen cod_avg_release_dead_wt=cod_released_dead_mt/cod_num_released
gen hadd_avg_release_dead_wt=hadd_released_dead_mt/haddock_num_released


gen haddock_relptrip=haddock_num_released/total_trips
gen haddock_landptrip=haddock_num_kept/total_trips

gen cod_relptrip=cod_num_released/total_trips
gen cod_landptrip=cod_num_kept/total_trips

gen haddock_catchptrip = haddock_tot_num_cat/total_trips
gen cod_catchptrip = cod_tot_num_cat/total_trips

tabstat cod_ok adj_cod_ok hadd_ok adj_hadd_ok, stat(sum) by(source)
tabstat adj_cod_mort_mt adj_hadd_mort_mt, stat(median) by(source)




**Min yang's unadjusted vars
local vars WTP total_trips 

local vars 	cod_num_kept cod_num_released cod_tot_num_cat cod_catchptrip ///
				haddock_num_kept haddock_num_released haddock_tot_num_cat  haddock_catchptrip 

local vars 	cod_kept_mt cod_released_mt cod_released_dead_mt cod_mort_mt ///
				hadd_kept_mt hadd_released_mt hadd_released_dead_mt hadd_mort_mt 
			
local vars  cod_avg_harvest_wt cod_avg_release_wt cod_avg_release_dead_wt   ///
	 			hadd_avg_harvest_wt had_avg_release_wt hadd_avg_release_dead_wt 

 **Min yang's adjusted vars
local vars 	adj_cod_kept_mt adj_cod_released_mt adj_cod_released_dead_mt adj_cod_mort_mt ///
				adj_hadd_kept_mt adj_hadd_released_mt adj_hadd_released_dead_mt adj_hadd_mort_mt 
		/*						
local vars  adj_cod_avg_harvest_wt adj_cod_avg_release_wt adj_cod_avg_release_dead_wt   ///
	 			adj_hadd_avg_harvest_wt adj_hadd_avg_release_wt adj_hadd_avg_release_dead_wt 
*/	 
tempfile new
save `new', replace 				
foreach v of local vars{
u `new', clear  

ttest `v' if inlist(source, "Min-Yang", "Lou (cm)") , by(source)
local diff_lou_cm = round(`r(mu_1)' - `r(mu_2)', .00001)

ttest `v' if inlist(source, "Min-Yang", "Lou (inches + 0.5)") , by(source)
local diff_lou_in = round(`r(mu_1)' - `r(mu_2)', .00001)

ttest `v' if inlist(source, "Min-Yang", "Lou (inches)") , by(source)
local diff_lou_in5 = round(`r(mu_1)' - `r(mu_2)', .00001)


summ `v' if source=="Lou (cm)", detail 
local lou_cm = `r(mean)'

summ `v' if source=="Lou (inches + 0.5)", detail 
local lou_in5 = `r(mean)'

summ `v' if source=="Lou (inches)", detail 
local lou_in= `r(mean)'

summ `v' if source=="Min-Yang", detail 
local MY = `r(mean)'

local perc_diff_cm = round(((`lou_cm'-`MY')/`MY')*100, .1)
local perc_diff_in = round(((`lou_in'-`MY')/`MY')*100, .1)
local perc_diff_in5 = round(((`lou_in5'-`MY')/`MY')*100, .1)


collapse (mean) `v' = `v' (sd) sd_`v' = `v', by(source)
gen x = 2 if source=="Lou (cm)"
replace x = 3 if source=="Lou (inches + 0.5)"
replace x = 4 if source=="Lou (inches)"
replace x=5 if source=="Min-Yang"
gen y_u = `v'+1.96*sd 
gen y_l = `v'-1.96*sd 
label  define x   2 "cm" 3  "inches.+0.5"  4 "inches" 5 "M-Y"
label  value x x

twoway (scatter `v' x , msymbol(S) msize(vsmall)) ///
			 (rcap y_u y_l x, ), ///
			 xlabel(1(1)6, valuelabel labsize(small)) xtitle(" ") ///
			 title(`v', size(medium)) name(dom`v', replace)  ytitle("") ylab(, labsize(vsmall) angle(45)) xlab(, angle(horizontal) labsize(vsmall) ) ///
			 note("perc. diff Lou (cm)  - Min-Yang = `perc_diff_cm'" ///
					 "perc. diff Lou (inches + 0.5)  - Min-Yang = `perc_diff_in5'"	///
					 "perc. diff Lou (inches)  - Min-Yang = `perc_diff_in'", size(vsmall)) ///
			 legend(off)
			 
local graphnames `graphnames' dom`v'
			 
}
grc1leg `graphnames'
gr play legendoff
u `new', clear  

*/
local vars WTP total_trips cod_num_kept cod_num_released cod_tot_num_cat cod_catchptrip ///
				haddock_num_kept haddock_num_released haddock_tot_num_cat  haddock_catchptrip ///
				cod_kept_mt cod_released_mt cod_released_dead_mt cod_mort_mt ///
				hadd_kept_mt hadd_released_mt hadd_released_dead_mt hadd_mort_mt ///			
				cod_avg_harvest_wt cod_avg_release_wt cod_avg_release_dead_wt   ///
	 			hadd_avg_harvest_wt had_avg_release_wt hadd_avg_release_dead_wt  ///
				adj_cod_kept_mt adj_cod_released_mt adj_cod_released_dead_mt adj_cod_mort_mt ///
				adj_hadd_kept_mt adj_hadd_released_mt adj_hadd_released_dead_mt adj_hadd_mort_mt ///
			 	adj_cod_avg_harvest_wt adj_cod_avg_release_wt adj_cod_avg_release_dead_wt   ///
	 			adj_hadd_avg_harvest_wt adj_hadd_avg_release_wt adj_hadd_avg_release_dead_wt 

tempfile new
save `new', replace 	

global c			
foreach v of local vars{
u `new', clear  

/*
ttest `v', by(source)
local diff = round(`r(mu_1)' - `r(mu_2)', .00001)
local p = round(`r(p)', .001) 

su `v' if source=="Lou" 
local mean_lou = `r(mean)'
su `v' if source=="Min-Yang" 
local mean_MY = `r(mean)'

local perc_diff = round(((`mean_lou'-`mean_MY')/`mean_MY')*100, .1)
collapse (mean) `v' = `v', by(source)
*/

summ `v' if source=="Lou (cm)", detail 
local lou_cm = `r(mean)'

summ `v' if source=="Lou (inches + 0.5)", detail 
local lou_in5 = `r(mean)'

summ `v' if source=="Lou (inches)", detail 
local lou_in= `r(mean)'

summ `v' if source=="Min-Yang", detail 
local mean_MY = `r(mean)'

local perc_diff_cm = round(((`lou_cm'-`mean_MY')/`mean_MY')*100, .1)
local perc_diff_in = round(((`lou_in'-`mean_MY')/`mean_MY')*100, .1)
local perc_diff_in5 = round(((`lou_in5'-`mean_MY')/`mean_MY')*100, .1)


clear
set obs 1
gen mean_MY = `mean_MY'
gen mean_lou_cm = `lou_cm'
gen mean_lou_in = `lou_in'
gen mean_lou_in5 = `lou_in5'
gen perc_diff_cm = `perc_diff_cm'
gen perc_diff_in = `perc_diff_in'
gen perc_diff_in5 = `perc_diff_in5'


*gen diff = `diff'
*gen p= `p'
gen stat="`v'"

			 

tempfile c`v'
save `c`v'', replace
global c "$c "`c`v''" " 

}

dsconcat $c		 
order stat 




**Comapre model results when estimating in centimeters and:
*		(a) making no adjustments to catch-at-length when converting lengths for weights and 
*		(b) add 0.5 cm before converting lengths to weights 
		
import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_cm.xlsx", clear first /*This is the 2nd set of results - model run in cm's*/ 
keep if run<=7
gen domain=""
replace domain = "cod_harvest_weight" if Category=="cod" & catch_disp=="keep" & param=="Total" & number_weight=="Weight"
replace domain = "hadd_harvest_weight" if Category=="had" & catch_disp=="keep"  & param=="Total" & number_weight=="Weight"
replace domain = "cod_release_weight" if Category=="cod" & catch_disp=="release" & param=="Total" & number_weight=="Weight"
replace domain = "hadd_release_weight" if Category=="had" & catch_disp=="release"  & param=="Total" & number_weight=="Weight"
replace domain = "cod_disc_mort_weight" if Category=="cod" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Weight"
replace domain = "hadd_disc_mort_weight" if Category=="had" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Weight"

replace domain = "cod_harvest_number" if Category=="cod" & catch_disp=="keep" & param=="Total" & number_weight=="Number"
replace domain = "hadd_harvest_number" if Category=="had" & catch_disp=="keep"  & param=="Total" & number_weight=="Number"
replace domain = "cod_release_number" if Category=="cod" & catch_disp=="release" & param=="Total" & number_weight=="Number"
replace domain = "hadd_release_number" if Category=="had" & catch_disp=="release"  & param=="Total" & number_weight=="Number"
replace domain = "cod_disc_mort_number" if Category=="cod" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Number"
replace domain = "hadd_disc_mort_number" if Category=="had" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Number"

replace domain = "CV" if Category=="CV"
replace domain = "ntrips" if Category=="ntrips"
replace domain = "nchoiceoccasions" if Category=="nchoiceoccasions"
replace domain = "codkeepsum" if Category=="codkeepsum"
replace domain = "codrelsum" if Category=="codrelsum"
replace domain = "haddkeepsum" if Category=="haddkeepsum"
replace domain = "haddrelsum" if Category=="haddrelsum"

collapse (sum) Value, by(domain run)
reshape wide Value, i( run) j(domain) string

ds run, not
renvarlab `r(varlist)', predrop(5)

replace cod_harvest_weight=cod_harvest_weight/2205
replace hadd_harvest_weight=hadd_harvest_weight/2205

replace hadd_disc_mort_weight=hadd_disc_mort_weight/2205
replace cod_disc_mort_weight=cod_disc_mort_weight/2205

replace hadd_release_weight=hadd_release_weight/2205
replace cod_release_weight=cod_release_weight/2205

gen replicate=_n
gen scenario_num=0

        
rename ntrips total_trips
rename cod_harvest_number cod_num_kept
rename cod_release_number cod_num_released
rename hadd_harvest_number haddock_num_kept
rename hadd_release_number haddock_num_released

rename cod_harvest_weight cod_kept_mt
rename hadd_harvest_weight hadd_kept_mt

rename cod_release_weight cod_released_mt
rename hadd_release_weight hadd_released_mt

rename cod_disc_mort_weight cod_released_dead_mt
rename hadd_disc_mort_weight hadd_released_dead_mt

gen cod_mort_mt = cod_kept_mt+cod_released_dead_mt
gen hadd_mort_mt = hadd_kept_mt+hadd_released_dead_mt

gen adj_cod_released_dead_mt  =  cod_released_dead_mt
gen adj_hadd_released_dead_mt  =  hadd_released_dead_mt

gen adj_cod_kept_mt  =  cod_kept_mt
gen adj_hadd_kept_mt  =  hadd_kept_mt

gen adj_cod_mort_mt  =  cod_mort_mt
gen adj_hadd_mort_mt  =  hadd_mort_mt

gen adj_cod_released_mt  =  cod_released_mt    
gen adj_hadd_released_mt  =  hadd_released_mt    


gen cod_ok=0
replace cod_ok=1 if cod_mort_mt<=$cod_recACL2024

gen adj_cod_ok=0
replace adj_cod_ok=1 if adj_cod_mort_mt<=$cod_recACL2024

gen hadd_ok=0 
replace hadd_ok=1 if hadd_mort_mt<=$hadd_recACL2024

gen adj_hadd_ok=0
replace adj_hadd_ok=1 if adj_hadd_mort_mt<=$hadd_recACL2024


rename CV WTP
keep replicate scenario total_trips adj_cod_released_mt  adj_hadd_released_mt cod_num_kept cod_num_released haddock_num_kept haddock_num_released adj_cod_released_dead_mt adj_hadd_released_dead_mt adj_cod_kept_mt adj_hadd_kept_mt adj_cod_mort_mt adj_hadd_mort_mt WTP cod_kept_mt hadd_kept_mt cod_released_mt hadd_released_mt cod_released_dead_mt hadd_released_dead_mt cod_mort_mt hadd_mort_mt cod_ok  adj_cod_ok hadd_ok adj_hadd_ok
  
gen source="cm"
tempfile lou
save `lou', replace


import excel using "C:/Users/andrew.carr-harris/Desktop/cod_hadd_RDM/predictions_check_cm_first5.xlsx", clear first /*This is the 2nd set of results - model run in cm's*/ 

gen domain=""
replace domain = "cod_harvest_weight" if Category=="cod" & catch_disp=="keep" & param=="Total" & number_weight=="Weight"
replace domain = "hadd_harvest_weight" if Category=="had" & catch_disp=="keep"  & param=="Total" & number_weight=="Weight"
replace domain = "cod_release_weight" if Category=="cod" & catch_disp=="release" & param=="Total" & number_weight=="Weight"
replace domain = "hadd_release_weight" if Category=="had" & catch_disp=="release"  & param=="Total" & number_weight=="Weight"
replace domain = "cod_disc_mort_weight" if Category=="cod" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Weight"
replace domain = "hadd_disc_mort_weight" if Category=="had" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Weight"

replace domain = "cod_harvest_number" if Category=="cod" & catch_disp=="keep" & param=="Total" & number_weight=="Number"
replace domain = "hadd_harvest_number" if Category=="had" & catch_disp=="keep"  & param=="Total" & number_weight=="Number"
replace domain = "cod_release_number" if Category=="cod" & catch_disp=="release" & param=="Total" & number_weight=="Number"
replace domain = "hadd_release_number" if Category=="had" & catch_disp=="release"  & param=="Total" & number_weight=="Number"
replace domain = "cod_disc_mort_number" if Category=="cod" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Number"
replace domain = "hadd_disc_mort_number" if Category=="had" & catch_disp=="Discmortality"  & param=="Total" & number_weight=="Number"

replace domain = "CV" if Category=="CV"
replace domain = "ntrips" if Category=="ntrips"
replace domain = "nchoiceoccasions" if Category=="nchoiceoccasions"
replace domain = "codkeepsum" if Category=="codkeepsum"
replace domain = "codrelsum" if Category=="codrelsum"
replace domain = "haddkeepsum" if Category=="haddkeepsum"
replace domain = "haddrelsum" if Category=="haddrelsum"

collapse (sum) Value, by(domain run)
reshape wide Value, i( run) j(domain) string

ds run, not
renvarlab `r(varlist)', predrop(5)

replace cod_harvest_weight=cod_harvest_weight/2205
replace hadd_harvest_weight=hadd_harvest_weight/2205

replace hadd_disc_mort_weight=hadd_disc_mort_weight/2205
replace cod_disc_mort_weight=cod_disc_mort_weight/2205

replace hadd_release_weight=hadd_release_weight/2205
replace cod_release_weight=cod_release_weight/2205

gen replicate=_n
gen scenario_num=0

        
rename ntrips total_trips
rename cod_harvest_number cod_num_kept
rename cod_release_number cod_num_released
rename hadd_harvest_number haddock_num_kept
rename hadd_release_number haddock_num_released

rename cod_harvest_weight cod_kept_mt
rename hadd_harvest_weight hadd_kept_mt

rename cod_release_weight cod_released_mt
rename hadd_release_weight hadd_released_mt

rename cod_disc_mort_weight cod_released_dead_mt
rename hadd_disc_mort_weight hadd_released_dead_mt

gen cod_mort_mt = cod_kept_mt+cod_released_dead_mt
gen hadd_mort_mt = hadd_kept_mt+hadd_released_dead_mt

gen adj_cod_released_dead_mt  =  cod_released_dead_mt
gen adj_hadd_released_dead_mt  =  hadd_released_dead_mt

gen adj_cod_kept_mt  =  cod_kept_mt
gen adj_hadd_kept_mt  =  hadd_kept_mt

gen adj_cod_mort_mt  =  cod_mort_mt
gen adj_hadd_mort_mt  =  hadd_mort_mt

gen adj_cod_released_mt  =  cod_released_mt    
gen adj_hadd_released_mt  =  hadd_released_mt    


gen cod_ok=0
replace cod_ok=1 if cod_mort_mt<=$cod_recACL2024

gen adj_cod_ok=0
replace adj_cod_ok=1 if adj_cod_mort_mt<=$cod_recACL2024

gen hadd_ok=0 
replace hadd_ok=1 if hadd_mort_mt<=$hadd_recACL2024

gen adj_hadd_ok=0
replace adj_hadd_ok=1 if adj_hadd_mort_mt<=$hadd_recACL2024


rename CV WTP
keep replicate scenario total_trips adj_cod_released_mt  adj_hadd_released_mt cod_num_kept cod_num_released haddock_num_kept haddock_num_released adj_cod_released_dead_mt adj_hadd_released_dead_mt adj_cod_kept_mt adj_hadd_kept_mt adj_cod_mort_mt adj_hadd_mort_mt WTP cod_kept_mt hadd_kept_mt cod_released_mt hadd_released_mt cod_released_dead_mt hadd_released_dead_mt cod_mort_mt hadd_mort_mt cod_ok  adj_cod_ok hadd_ok adj_hadd_ok
  
gen source="adj_cm"
append using `lou'


gen cod_tot_num_cat=cod_num_kept+cod_num_released
gen haddock_tot_num_cat=haddock_num_kept+haddock_num_released


gen adj_cod_avg_harvest_wt=adj_cod_kept_mt/cod_num_kept
gen adj_hadd_avg_harvest_wt=adj_hadd_kept_mt/haddock_num_kept

gen adj_cod_avg_release_wt=adj_cod_released_mt/cod_num_released
gen adj_hadd_avg_release_wt=adj_hadd_released_mt/haddock_num_released

gen adj_cod_avg_release_dead_wt=adj_cod_released_dead_mt/cod_num_released
gen adj_hadd_avg_release_dead_wt=adj_hadd_released_dead_mt/haddock_num_released



gen cod_avg_harvest_wt=cod_kept_mt/cod_num_kept
gen hadd_avg_harvest_wt=hadd_kept_mt/haddock_num_kept

gen cod_avg_release_wt=cod_released_mt/cod_num_released
gen had_avg_release_wt=hadd_released_mt/haddock_num_released

gen cod_avg_release_dead_wt=cod_released_dead_mt/cod_num_released
gen hadd_avg_release_dead_wt=hadd_released_dead_mt/haddock_num_released


gen haddock_relptrip=haddock_num_released/total_trips
gen haddock_landptrip=haddock_num_kept/total_trips

gen cod_relptrip=cod_num_released/total_trips
gen cod_landptrip=cod_num_kept/total_trips

gen haddock_catchptrip = haddock_tot_num_cat/total_trips
gen cod_catchptrip = cod_tot_num_cat/total_trips

tabstat cod_ok adj_cod_ok hadd_ok adj_hadd_ok, stat(sum) by(source)
tabstat adj_cod_mort_mt adj_hadd_mort_mt, stat(median) by(source)
tabstat cod_mort_mt hadd_mort_mt, stat(median) by(source)

local vars WTP total_trips cod_num_kept cod_num_released cod_tot_num_cat cod_catchptrip ///
				haddock_num_kept haddock_num_released haddock_tot_num_cat  haddock_catchptrip ///
				cod_kept_mt cod_released_mt cod_released_dead_mt cod_mort_mt ///
				hadd_kept_mt hadd_released_mt hadd_released_dead_mt hadd_mort_mt ///			
				cod_avg_harvest_wt cod_avg_release_wt cod_avg_release_dead_wt   ///
	 			hadd_avg_harvest_wt had_avg_release_wt hadd_avg_release_dead_wt  ///
				adj_cod_kept_mt adj_cod_released_mt adj_cod_released_dead_mt adj_cod_mort_mt ///
				adj_hadd_kept_mt adj_hadd_released_mt adj_hadd_released_dead_mt adj_hadd_mort_mt ///
			 	adj_cod_avg_harvest_wt adj_cod_avg_release_wt adj_cod_avg_release_dead_wt   ///
	 			adj_hadd_avg_harvest_wt adj_hadd_avg_release_wt adj_hadd_avg_release_dead_wt 
				
				
tempfile new
save `new', replace 	

global c			
foreach v of local vars{
u `new', clear  
ttest `v', by(source)
local diff = round(`r(mu_1)' - `r(mu_2)', .00001)
local p = round(`r(p)', .001) 

su `v' if source=="adj_cm" 
local mean_adj_cm = `r(mean)'
su `v' if source=="cm" 
local mean_cm = `r(mean)'

local perc_diff = round(((`mean_adj_cm'-`mean_cm')/`mean_cm')*100, .1)
collapse (mean) `v' = `v', by(source)

clear
set obs 1
gen mean_adj_cm = `mean_adj_cm'
gen mean_cm = `mean_cm'
gen perc_diff = `perc_diff'
gen diff = `diff'
gen p= `p'
gen stat="`v'"

			 

tempfile c`v'
save `c`v'', replace
global c "$c "`c`v''" " 

}

dsconcat $c		 
order stat 

