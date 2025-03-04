

/****RDM input code wrapper****/

*************************
****Data availability****
*************************

*We make projections for the next fishing year in Dec/Jan. 

*MRIP data: we will use in the model the most recent 6 waves of MRIP data. 
		*For FY 2025 projections we use MRIP data from 2024 waves1-5 and 2023 wave 6 to compute catch-per-trip, directed trips, and catch-at-length 
		
*Stock assessment projections data:
		*Jan 1 2024 NAA to compute historical rec. selectivity
		*Jan 1 2024 NAA to compute projected catch-at-length
		
*NEFSC trawl survey data from the most recent three years used to create age-length keys


*MRIP data is stored in  
	*"smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	*Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\

* Dependencies
*ssc install xsvmat 
*ssc install gammafit 

*************************
*****Set globals******** 	
*************************

*Years/waves of MRIP data
*FY2024
*global yr_wvs 20221 20222 20223 20224 20225 20226  20231 20232 20233 20234 20235 
*global yearlist 2022 2023 
*global wavelist 1 2 3 4 5 6

global yr_wvs 20231 20232 20233 20234 20235 20236  20241 20242 20243 20244 20245 
global yearlist 2023 2024 
global wavelist 1 2 3 4 5 6

/*
global calibration_year "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"
global rec_selectivity_year "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"

global calibration_date_start td(01nov2022)
global calibration_date_end td(31oct2023)

global projection_date_start td(01may2024)
global projection_date_end td(30apr2025)
*/

global calibration_year "(year==2024 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2023 & inlist(wave, 6))"
global rec_selectivity_year "(year==2024 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2023 & inlist(wave, 6))"

global calibration_date_start td(01nov2023)
global calibration_date_end td(31oct2024)

global projection_date_start td(01may2025)
global projection_date_end td(30apr2026)


*Add federal holidays, as these are considered "weekend" days by the MRIP and we estimate fishing effort at the month and kind-of-day level
/*
Federal Holidays Included:
New Year's Day: January 1 (observed on January 2 if it falls on Sunday).
Martin Luther King Jr. Day: Third Monday in January.
Presidents' Day: Third Monday in February.
Memorial Day: Last Monday in May.
Juneteenth National Independence Day: June 19.
Independence Day: July 4 (observed on July 5 if it falls on Sunday).
Labor Day: First Monday in September.
Columbus Day: Second Monday in October.
Veterans Day: November 11 (observed on November 10 if it falls on Sunday).
Thanksgiving Day: Fourth Thursday in November.
Christmas Day: December 25 (observed on December 26 if it falls on Sunday).
*/

/*
global fed_holidays "inlist(day, td(02jan2023), td(16jan2023), td(20feb2023), td(29may2023), td(19jun2023), td(04jul2023), td(04sep2023), td(09oct2023), td(10nov2023), td(23nov2023)," ///
 "td(25dec2023), td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024)," ///
" td(01jan2025), td(20jan2025), td(17feb2025), td(26may2025), td(19jun2025), td(04jul2025), td(01sep2025), td(13oct2025), td(11nov2025), td(27nov2025), td(25dec2025)," ///
" td(01jan2026), td(19jan2026), td(16feb2026), td(25may2026))"
*/
global fed_holidays "inlist(day, td(10nov2023), td(23nov2023), td(25dec2023), td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024))" 

global fed_holidays_y2 "inlist(day_y2,td(01jan2025), td(20jan2025), td(17feb2025), td(26may2025), td(19jun2025), td(04jul2025), td(01sep2025), td(13oct2025), td(11nov2025), td(27nov2025), td(25dec2025), td(01jan2026), td(19jan2026), td(16feb2026), td(25may2026))"

*Put leap-year days here
global leap_yr_days "td(29feb2024)" 

*Choose how many draws you want to create. Will create 150 for final version, from which 100 will be selected
global ndraws 2

*Set the global length to pull either ionches or centimeters from MRIP (l_in_bin or l_cm_bin)
global length_bin l_cm_bin

*set the year to use for historical numbers at age 
global calibration_year_NAA 2024

*set the year to use for projected numbers at age 
global projection_year_NAA 2025

*set years of which to pull the NEFSC trawl survey data
global trawl_svy_start_yr 2019
global trawl_svy_end_yr 2024

*


*set the correct historical and projected cod NAA datasets
*FY 2025 files: WGOM_Cod_historical_NAA_2024Assessment, GOM_Haddock_historical_NAA_2024Assessment, WGOM_Cod_projected_NAA_2024Assessment, GOM_Haddock_projected_NAA_2024Assessment
/*
global historical_cod_NAA  "historical_and_mean_projected_Cod_NAA.dta"
global historical_hadd_NAA "historical_and_mean_projected_Haddock_NAA.dta"

global projected_cod_NAA "cod_beginning_sorted2023.dta"
global projected_hadd_NAA  "haddock_beginning_sorted2023.dta"
*/
global historical_cod_NAA  "WGOM_Cod_historical_NAA_2024Assessment.dta"
global historical_hadd_NAA "GOM_Haddock_historical_NAA_2024Assessment.dta"

global projected_cod_NAA "WGOM_Cod_projected_NAA_2024Assessment.dta"
global projected_hadd_NAA  "GOM_Haddock_projected_NAA_2024Assessment.dta"


*Adjustment to 2017 survey trip costs to account for inflation
global inflation_expansion=1.27
global project_path "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock" /* Lou's project path */
global project_path "C:\Users\min-yang.lee\Documents\rdmtool\lou_files\cod_haddock"  /* Min-Yang's project path */

global input_data_cd "${project_path}\input_data"
global input_code_cd "${project_path}\\code"
global iterative_input_data_cd "${input_data_cd}\iterative_data"
global figure_cd  "${project_path}\figures"


***Calibration files*** 

// 1) Pull the MRIP data
do "$input_code_cd\MRIP data wrapper.do"


// 2) Estimate directed trips at the month, mode, kind-of day level
		*Note this file call "set regulations.do". You must enter the regulations in the calibration + projection year in "set regulations.do"
		*datasets needed: 
			*(old) ma site allocation.dta - allocating MRIP sites to GoM stock area. Note this will change in FY25+
			*(new) ma_site_list_updated_SS.xlsx - allocating MRIP sites to GoM stock area. 

do "$input_code_cd\directed_trips_calibration.do"


// 3) Estimates costs per trip 
do "$input_code_cd\survey trip costs.do"


// 4) Estimate catch-per- trips at the month and mode level
		*datasets needed:
			*atl_states_2017_expsurvey.dta - expenditure survey data
			*ma site allocation.dta - allocating MRIP sites to GoM stock area. Note this will change in FY25+
			*(new) ma_site_list_updated_SS.xlsx - allocating MRIP sites to GoM stock area. 

do "$input_code_cd\catch_per_trip_calibration.do"


// 5) Generate total harvest and catch estimates based on the directed trips/catch draws - will use this to calibrate the model
do "$input_code_cd\simulated_catch_keep_totals_open_seasons.do"



// 6) Estimate recreational selectivities at length/projected catch-at-length distribution
		*datasets needed:
				*ma site allocation.dta - allocating MRIP sites to GoM stock area. Note this will change in FY25+
				
				*(new) ma_site_list_updated_SS.xlsx - allocating MRIP sites to GoM stock area. 
		
				*Bottom trawl survey data for cod and haddock from 2021-2023 to form the age-length keys:
					*Created by merging all_spring_cruises_lou.xlsx, which contains cruise identifier, to cod_svspp_raw_lou.xlsx/hadd_svspp_raw_lou.xlsx, 
					*which contains ages and lengths in centimeters. These data must be replaced by more recent data in in FY25+. 
					*Note that I combine data for come age classes depending on the number of observations. 
						*-For cod, I drop age-0's and create an age 6+ age group. 
						*-For haddock, I drop age-0's and create an age 9+ group (although there is no data for age 10+)
	
				*Historical_and_mean_projected_Cod_NAA.dta and historical_and_mean_projected_Haddock_NAA- historical NAA to compute recreational selectivities:
					*These will be replaced by GOM_Haddock_historical_NAA_2024Assessment.dta and WGOM_Cod_historical_NAA_2024Assessment.dta
	
				*Projected stock strucutre for next year - cod_beginning_sorted2023.dta and haddock_beginning_sorted2023/dta, 
					*These will be replaced with WGOM_Cod_projected_NAA_2024Assessment.dta and GOM_Haddock_projected_NAA_2024Assessment.dta		

do "$input_code_cd\rec selectivity at length - open_seasons.do"


// Steps 7-10 are not necessary to run. They compare the disaggregated simulated catch and effort data to aggreagte MRIP estimates, and compute catch weight totals in the calibration year 

// 7) Pull total harvest and catch estimates based MRIP to compare to simulated harvest and catch totals
*do "$input_code_cd\catch_totals_calibration_open_seasons.do"

// 8) Pull total dtrip estimates based on MRIP to compare to simulated dtrips
*do "$input_code_cd\directed_trips_calibration_open_seasons.do"

// 9) A few checks to ensure simulated data looks good
	*a) compare simulated catch totals--based on multiplying draws of trips-per-day by mean catch-per trip-- to MRIP point estimates
*do "$input_code_cd\catch_totals_compare_model2mrip_open_seasons.do"

// 10) Estimate total weight of harvest and release in calibration year 
	*a) compute raw MRIP catch-at-length data (for cod, use unweighted b2 data and weighted a+b1; for haddock, use weighted b2 data and weighted a+b1) - I do this in step 6 
	*b) multiply these distributions by the total keep and release per period for each MRIP draw 

*do "$input_code_cd\raw_props_at_length_ab1b2_calibration.do"
*do "$input_code_cd\catch_totals_calibration_month1.do"
*do "$input_code_cd\compute catch weights calibration.do" - this file requires the R code output calibration_catch_weights_cm.xlsx










