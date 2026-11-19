

* This file compiles angler demographics that enter the trip utility model 

set seed $seed

* Variables needed: 
	* male: choice experiment sample
	* age: MRIP FES 2023 *NOT SIGNIFICANT
	* fishing_skill_3_4:  choice experiment sample
	* likely: choice experiment sample 
	* days_str_blue12, days_non_str_blue12: choice experiment sample  

* choice experiment sample
u "$input_data_cd\striper_CE_data.dta", clear 

	
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost,  ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age)
			 
keep qtid lic_st likely_to_fish male  days_non_str_blue12  
duplicates drop 

gen region1="N" if inlist(lic, "ME", "NH", "MA", "RI", "CT", "NY")
replace region1="S" if !inlist(lic, "ME", "NH", "MA", "RI", "CT", "NY")

* expand to management regions
expand 7 if region1 == "N"	 
bysort qtid region1: gen tab=_n

*north
gen region="CTALL" if region1=="N" & tab==1
replace region="MAALL" if region1=="N" & tab==2
replace region="MEALL" if region1=="N" & tab==3
replace region="NHALL" if region1=="N" & tab==4
replace region="NYHUDN" if region1=="N" & tab==5
replace region="NYHUDS" if region1=="N" & tab==6
replace region="RIALL" if region1=="N" & tab==7

drop tab
expand 13 if region1 == "S"	 
bysort qtid region1: gen tab=_n

*South
replace region="DEDERIV" if region1=="S" & tab==1
replace region="DENANT" if region1=="S" & tab==2
replace region="DEOCN" if region1=="S" & tab==3
replace region="MDOCN" if region1=="S" & tab==4
replace region="MDPOT" if region1=="S" & tab==5
replace region="NCALB" if region1=="S" & tab==6
replace region="NCCNTRL" if region1=="S" & tab==7
replace region="NCOCN" if region1=="S" & tab==8
replace region="NJALL" if region1=="S" & tab==9
replace region="VAOCN" if region1=="S" & tab==10
replace region="VAPOTO" if region1=="S" & tab==11
replace region="MDCHES" if region1=="S" & tab==12
replace region="VACHES" if region1=="S" & tab==13

drop tab 
drop qtid region1
drop lic
order region

tempfile choice_exp_angler_dems
save `choice_exp_angler_dems', replace


local regions "CTALL DEDERIV DENANT DEOCN MAALL MDCHES MDOCN MDPOT MEALL NCALB NCCNTRL NCOCN NHALL NJALL NYHUDN NYHUDS RIALL VACHES VAOCN VAPOTO"

foreach r of local regions{
forvalues i = 1/$ndraws {
	
	*local r = "NCALB"
	*local i=1
	
	   use "$iterative_input_data_cd\calib_catch_draws_`r'`i'.dta", clear 
	   gen str_cat=str_keep + str_rel
	   gen blu_cat = blu_keep + blu_rel
	   drop str_keep str_rel blu_keep blu_rel total_trips_12 age
	   
	   compress
	   save "$iterative_input_data_cd\calib_catch_draws_processed_`r'_`i'.dta", replace 
	   
	   preserve 
	   keep mode date tripid cost
	   duplicates drop
	   count
	   local N_needed=`r(N)'
	   gen merge_id=_n
	   tempfile individuals 
	   save `individuals', replace 
	   restore 
	   
	   
	   tempfile base 
	   save `base', replace 
	   
	   u `choice_exp_angler_dems', clear 
	   keep if region=="`r'"
	   
	   count
	   local N=`r(N)'
	   local expand = (`N_needed'/`N')+1
	   expand `expand'
	   sample `N_needed', count
	   
	   gen merge_id=_n

	   merge 1:1 merge_id using `individuals'
	   drop _merge 
	   
	   tempfile individuals 
	   save `individuals', replace 
	   
	   use "$input_data_cd\preference_params.dta", clear 
	   keep if draw==`i'
	    
	   count
	   local N=`r(N)'
	   local expand = (`N_needed'/`N')+1
	   expand `expand'
	   sample `N_needed', count
	   
	   gen merge_id=_n
		
	   merge 1:1 merge_id using `individuals'
	   drop _merge  merge_id

	   
	   keep mode date tripid beta* likely_to_fish male  days_non_str_blue12    cost_sim
	   order mode date tripid beta* likely_to_fish male  days_non_str_blue12    cost_sim 
	   compress
	   
	   save "$iterative_input_data_cd\pref_params_and_dems_`r'_`i'.dta", replace 

	   
	     
	   
	}
}


