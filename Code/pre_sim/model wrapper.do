

/****RDM input code wrapper****/

*** Striper and blusfish RDM ***

*************************
****Data availability****
*************************

*MRIP catch and effort data: 
	* calibration: fishing year 2023 (Jan. - Dec.). Terminal year of last striped bass sock assessment. 
	* projection: most recent 6 waves of MRIP data. - TBD

*length data: 2023 from MRIP and various volunteer angler surveys
				
*Stock assessment projections data:
		*Jan 1 2023 NAA to compute historical rec. selectivity 
		*Jan 1 2024 NAA to compute projected catch-at-length 
		
*NEED: 
	*age-length conversions
	*length-weight conversions

*MRIP data is stored in  
	*"smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	*Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\

* Dependencies
*ssc install xsvmat 
*ssc install gammafit 



**************************************************ADJUST GLOBALS************************************************** 	
* these need to be changed every year 
global calibration_year "(year==2023)"  // For now, use catch data from 2023 as predictions for 2024. Alternative time periods can be tested.
global calibration_year_prev "(year==2023 | year==2022)"  // For now, use catch data from 2023 as predictions for 2024. Alternative time periods can be tested.


global calibration_date_start td(01jan2023)
global calibration_date_end td(31dec2023)

global projection_date_start td(01jan2024)
global projection_date_end td(31dec2024)

* add federal holidays, as these are considered "weekend" days by the MRIP and we need to account for this when estimating fishing effort at the month and kind-of-day level

* fed holidays in the calibration year 
global fed_holidays "inlist(day, td(02jan2023), td(16jan2023), td(20feb2023), td(29may2023), td(19jun2023), td(04jul2023), td(04sep2023), td(09oct2023), td(10nov2023), td(23nov2023), td(25dec2023))" 

* fed holidays in the projection year 
global fed_holidays_y2 "inlist(day, td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024))" 
* leap-year days here
global leap_yr_days "td(29feb2024)" 

* choose number of draws to create. Will ultimately select ~100 for final model
global ndraws 10

* adjust 2022 survey trip costs to account for inflation (January 2022 - January 2025)
* source =https://www.bls.gov/data/inflation_calculator.htm
global inflation_expansion=1.13 // CHECK

* adjust project paths based on user
global project_path "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM" /* Lou's project path */
global input_data_cd "E:\Lou_projects\striper_bluefish_RDM\input_data" /* Lou's local data path */
global input_code_cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"
global iterative_input_data_cd "E:\Lou_projects\striper_bluefish_RDM\process_data"
global figure_cd  "E:\Lou_projects\striper_bluefish_RDM\figures"

* set a global seed #
global seed 03211990

* years/waves of MRIP data. 
global yr_wvs 20221 20222 20223 20224 20225 20226  ///
					 20231 20232 20233 20234 20235 20236  ///
					 20241 20242 20243 20244 20245 20246  
					 
global yearlist 2022 2023 2024
global wavelist 1 2 3 4 5 6


**************************************************Model calibration ************************************************** 
// 1) Pull the MRIP data
do "$input_code_cd\MRIP data wrapper.do"

// 2) Estimate directed trips at the month, mode, kind-of day level
do "$input_code_cd\directed_trips_calibration.do"
		*This file calls "set regulations.do". In it you must enter the SQ regulations in the calibration and projection year. 
		*THIS NEEDS TO BE ADJUSTED EVERY YEAR. 

// 3) Create distributions of costs per trip across strata - only needs to be run once
*do "$input_code_cd\survey trip costs.do"

// 4) Create draw of angler preference parameters 
*do "$input_code_cd\estimate angler preferences.do" - only needs to be run once

// 5) Estimate catch-per-trip at the month and mode level
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		do "$input_code_cd\calibration_catch_per_trip_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		* run copula_modeling_calibration.R
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\calibration_catch_per_trip_part2.do"

// 6) compare calibration output to MRIP, and retain total simulated harvest and discards to apply to the baseline catch-at-length distribution
		do "$input_code_cd\compare calibration data to MRIP.do" 

// 7) add additonal angler demographics based on results of utilty model
		do "$input_code_cd\additional_angler_dems.do" 

// 8) Generate baseline-year catch-at-length, using the simulated harvest/discard totals from step 5
		do "$input_code_cd\catch_at_length_calibration.do"
		
// 9) Generate projection-year catch-at-length, incorporating the stock assessment data
		do "$input_code_cd\catch_at_length_projection.do"

// The calibration and projection routines can now be run in R. 		


		







