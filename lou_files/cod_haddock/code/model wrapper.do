

/****RDM input code wrapper****/

**For 2025 regulation-setting, will use the following years of MRIP data:
	*Directed trips - 2023 waves 3-6, 2024 wave 1 and 2
	*Catch-per-trip for calibration - 2023 waves 3-6, 2024 wave 1 and 2
	*Catch-per-trip for projection - (one year data used) 2024 wave 1 through 5, 2023 wave 6 
	*Catch-at-length for calibration - Calender year 2023, stock assessment projections from Jan 1. 2023

**For toy example to get the model set up, will use data above but from the previous year 
	*Directed trips - 2022 waves 3-6, 2023 wave 1 and 2
	*Catch-per-trip for calibration - 2022 waves 3-6, 2023 wave 1 and 2
	*Catch-per-trip for projection - (one year data used) 2023 wave 1 through 5, 2022 wave 6 
	*Catch-at-length for calibration - Calender year 2022, stock assessment projections from Jan 1. 2022
	
	
global yr_wvs 20221 20222 20223 20224 20225 20226  20231 20232 20233 20234 20235 20236
global yearlist 2022 2023 
global wavelist 1 2 3 4 5 6

global calibration_year "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"
global rec_selectivity_year "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"

global calibration_date_start td(01nov2022)
global calibration_date_end td(31oct2023)

global projection_date_start td(01may2024)
global projection_date_end td(30apr2025)

global fed_holidays "inlist(day, td(01jan2022), td(17jan2022), td(21feb2022), td(30may2022), td(20jun2022), td(04jul2022), td(05sep2022), td(10oct2022),td (11nov2022), td(24nov2022), td(26dec2022), td(02jan2023), td(16jan2023), td(20feb2023), td(29may2023), td(19jun2023), td(04jul2023), td(04sep2023), td(9oct2023), td(10nov2023), td(23nov2023), td(25dec2023))"

global fed_holidays_y2 "inlist(day_y2, td(01jan2025), td(20jan2025), td(17feb2025), td(26may2025), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024),td (11nov2024), td(28nov2024), td(25dec2024))"

*Choose how many draws you want to create. Will create 150 for final version, from which 100 will be selected
global ndraws 2

*Set the global length to pull either ionches or centimeters from MRIP (l_in_bin or l_cm_bin)
global length_bin l_cm_bin

*set the year to use for historical numbers at age 
global calibration_year_NAA 2023

*set the year to use for projected numbers at age 
global projection_year_NAA 2024

*open seasons by fishing mode
global hadd_start_date1 td(1nov2022)
global hadd_end_date1 td(28feb2023)

global hadd_start_date2 td(01apr2023)
global hadd_end_date2 td(13aug2023)

global hadd_start_date3_pr td(14aug2023)
global hadd_end_date3_pr td(31oct2023)

global hadd_start_date3_fh td(14aug2023)
global hadd_end_date3_fh td(31oct2023)

global cod_start_date1 td(01apr2023)
global cod_end_date1 td(14apr2023)

global cod_start_date2 td(01sep2023)
global cod_end_date2 td(31oct2023)

global trawl_svy_start_yr 2021
global trawl_svy_end_yr 2023

*Adjustment to 2017 survey trip costs to account for inflation
global inflation_expansion=1.16

*global mrip_data_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\input_data"
global input_data_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\input_data"
*global input_code_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\input_code"
global input_code_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\code"

*global draw_file_cd "C:\Users\andrew.carr-harris\Desktop\cod_hadd_RDM"
*global age_pro_cd "C:\Users\andrew.carr-harris\Desktop\Git\rdmtool\lou_files\cod_haddock\input_code\agepro"

***Calibration files*** 

// 1) Pull the MRIP data
do "$input_code_cd\MRIP data wrapper.do"

// 2) Estimate directed trips at the month, mode, kind-of day level
*Note that in this file you must enter the regulations in the calibration year
do "$input_code_cd\directed_trips_calibration.do"
*datasets needed: 
	*(old) ma site allocation.dta - allocating MRIP sites to GoM stock area. Note this will change in FY25+
	*(new) ma_site_list_updated_SS.xlsx - allocating MRIP sites to GoM stock area. 

	// 3) Estimates costs per trip 
do "$input_code_cd\survey trip costs.do"

// 4) Estimate catch-per- trips at the month and mode level
do "$input_code_cd\catch_per_trip_calibration.do"
*datasets needed:
	*atl_states_2017_expsurvey.dta - expenditure survey data
	*ma site allocation.dta - allocating MRIP sites to GoM stock area. Note this will change in FY25+
	*(new) ma_site_list_updated_SS.xlsx - allocating MRIP sites to GoM stock area. 

// 5) Generate total harvest and catch estimates based on the directed trips/catch draws - will use this to calibrate the model
do "$input_code_cd\simulated_catch_keep_totals_open_seasons.do"

// 6) Estimate recreational selectivities at length/projected catch-at-length distribution
do "$input_code_cd\rec selectivity at length - open_seasons.do"
*datasets needed:
	*ma site allocation.dta - allocating MRIP sites to GoM stock area. Note this will change in FY25+
	*(new) ma_site_list_updated_SS.xlsx - allocating MRIP sites to GoM stock area. 

	*Bottom trawl survey data for cod and haddock from 2021-2023 to form the age-length keys:
			*Created by merging all_spring_cruises_lou.xlsx, which contains cruise identifier, to cod_svspp_raw_lou.xlsx/hadd_svspp_raw_lou.xlsx, 
			*which contains ages and lengths in centimeters. These data must be replaced by more recent data in in FY25+. Note that I combine 
			*data for come age classes depending on the number of observations. For cod, I drop age-0's and create an age 6+ age group. For haddock, I drop age-0's and 
			*create an age 9+ group (although there is no data for age 10+)
	*historical_and_mean_projected_Cod_NAA.dta and historical_and_mean_projected_Haddock_NAA- historical NAA to compute recreational selectivities:
			*These will be replaced by GOM_Haddock_historical_NAA_2024Assessment.dta and WGOM_Cod_historical_NAA_2024Assessment.dta
	*cod_beginning_sorted2023.dta and haddock_beginning_sorted2023/dta - projected stock strucutre for next year 
			*These will be replaced with WGOM_Cod_projected_NAA_2024Assessment.dta and GOM_Haddock_projected_NAA_2024Assessment.dta

	
	
	
// Steps 7-10 are not necessary to run. They compare the dissagregated simulated catch and effort data to aggreagte MRIP estimates, and compute catch weight totals in the calibration year 

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










