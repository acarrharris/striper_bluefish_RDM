


* First compare copula-simulated mean catch-per-trip to MRIP 
* Estimates were generated at the month and mode level

set seed $seed

*A) 
clear
tempfile master
save `master', emptyok

local statez "ME NH MA RI CT NY NJ DE MD VA NC"
foreach s of local statez {
forv i=1/$ndraws{

import excel using "$iterative_input_data_cd\calib_catch_draws_`s'`i'.xlsx", clear firstrow
gen str_cat_sim  = str_keep_sim + str_rel_sim
gen blu_cat_sim = blu_keep_sim + blu_rel_sim
collapse (mean) str_keep_sim str_rel_sim str_cat_sim blu_keep_sim blu_rel_sim blu_cat_sim , by(my_dom_id_string)
gen draw=`i'

append using `master'
save `master', replace
}
}

use `master', clear
save "$iterative_input_data_cd\simulated_means_copula.dta", replace 


u "$iterative_input_data_cd\simulated_means_copula.dta", clear 
ds draw my_dom_id, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

collapse (mean) str_keep_sim str_rel_sim str_cat_sim blu_keep_sim blu_rel_sim blu_cat_sim 	///
						(sd) sd_str_keep_sim=str_keep_sim sd_str_cat_sim=str_cat_sim sd_str_rel_sim=str_rel_sim ///
						sd_blu_keep_sim=blu_keep_sim sd_blu_rel_sim=blu_rel_sim sd_blu_cat_sim=blu_cat_sim, by(my)
						
renvarlab str* blu* , prefix(tot_)					
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 waters
rename my_dom_id_string4 month
rename my_dom_id_string5 mode
drop my_dom_id_string3 my_dom_id_string6

reshape long tot_ sd_, i(month  mode state waters) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
tempfile sim
save `sim', replace

*Pull in the MRIP means/SEs dataset
import excel using "$input_data_cd\baseline_mrip_catch_processed.xlsx", clear first 
keep my_dom_id_string-sestr_rel
duplicates drop
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 waters
rename my_dom_id_string4 month
rename my_dom_id_string5 mode
drop my_dom_id_string3 my_dom_id_string6

reshape long mean se, i(month  mode state waters) j(new) string
rename mean mrip_total 
rename se mrip_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new

*Join simulated means to MRIP means
merge 1:1 month mode my species disp using `sim'
*browse if _merge==1
*browse

gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
drop if disp=="cat"

gen domain=species+"_"+disp

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total
sort pct_diff
sort my_dom
replace my_dom_id_string=month+"_"+mode

sort month mode
tempfile new
save `new', replace 

levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"
	
	tempfile new1
	save `new1', replace
	
	levelsof domain, local(domain_list)
		
		foreach d in `domain_list' {
		
		u `new1', clear
		keep if domain=="`d'"
		sort month mode
		encode my_dom_id_string, gen(my_dom_id)
		gen my_dom_id_mrip = my_dom_id+0.1 
		gen my_dom_id_sim = my_dom_id-0.1  

* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}

qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("") xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  labsize(small) angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }
  
u `new', clear 

local statez "ME NH MA RI CT NY NJ  MD VA NC"
foreach s of local statez {
	
grc1leg  str_keep_`s' blu_keep_`s'  str_rel_`s' blu_rel_`s'    , cols(2) title("Mean catch-per-trip, MRIP vs. copula estimates `s'", size(small))
graph export "$figure_cd/mean_catch_MRIP_copula_`s'.png", as(png) replace

}

local statez "DE"
foreach s of local statez {
	
grc1leg  blu_keep_`s'  str_rel_`s' blu_rel_`s'    , cols(2) title("Mean catch-per-trip, MRIP vs. copula estimates `s'", size(small))
graph export "$figure_cd/mean_catch_MRIP_copula_`s'.png", as(png) replace

}


*B) The copula model data is used to generate daily catch-draw data, so here, I:
		*1) compute mean catch-per-trip from the daily catch-draw data
		*2) compute total catch/harvest/discards from the daily catch-draw data by multiplying
		*    mean catch/harvest/discards-per trip by the number of trips in that day
		*3) compare catch-per-trip means and total simulated catch from 2) with estimates from MRIP, both at the mode-wave level and the mode level

*B1 and B2)  

clear
tempfile master
save `master', emptyok
	
local regions "CTALL DEDERIV DENANT DEOCN MAALL MDCHES MDOCN MDPOT MEALL NCALB NCCNTRL NCOCN NHALL NJALL NYHUDN NYHUDS RIALL VACHES VAOCN VAPOTO"
foreach s of local regions{


forv i=1/$ndraws{
di "`i'"

*local i=1
*local s "CTALL"

use "$iterative_input_data_cd\calib_catch_draws_`s'`i'.dta", clear 

gen str_cat_sim=str_keep_sim+str_rel_sim
gen blu_cat_sim=blu_keep_sim+blu_rel_sim

gen month = month(date)
collapse (mean) str_keep_sim str_cat_sim str_rel_sim blu_keep_sim blu_rel_sim blu_cat_sim , by(state region waters mode month)

tempfile catch
save `catch', replace 

import delimited using "$iterative_input_data_cd\directed_trip_draws_`s'.csv",  clear 
drop if dtrip==0

keep if draw==`i'

gen date_num = date(date, "DMY")
gen month1 = month(date_num)	
drop date_num
gen month = string(month1, "%02.0f")

collapse (sum) dtrip, by(mode month)
destring month, replace 

merge 1:1 mode month  using `catch'
drop _merge



local vars str_keep_sim str_cat_sim str_rel_sim blu_keep_sim blu_rel_sim blu_cat_sim 
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

gen draw=`i'

append using `master'
save `master', replace
}
}

use `master', clear

save "$iterative_input_data_cd\simulated_catch_totals.dta", replace 



*B3 compare means @ mode month level
u "$iterative_input_data_cd\simulated_catch_totals.dta", clear 
rename dtrip tot_dtrip_sim

ds draw mode month, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

order mode month draw

collapse (mean) str_keep_sim str_rel_sim str_cat_sim blu_keep_sim blu_rel_sim blu_cat_sim  	///
						(sd) sd_str_keep_sim=str_keep_sim  ///
						sd_str_cat_sim=str_cat_sim  ///
						sd_str_rel_sim=str_rel_sim ///
						sd_blu_keep_sim=blu_keep_sim ///
						sd_blu_rel_sim=blu_rel_sim ///
						sd_blu_cat_sim=blu_cat_sim , by(mode month state waters)
						
renvarlab str* blu* , prefix(tot_)					

reshape long tot_ sd_, i(mode month state waters) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
destring month, replace 
tempfile sim
save `sim', replace


import excel using "$input_data_cd\baseline_mrip_catch_processed.xlsx", clear first 
keep my_dom_id_string-sestr_rel
duplicates drop
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 waters
rename my_dom_id_string4 month
rename my_dom_id_string5 mode
drop my_dom_id_string3 my_dom_id_string6

reshape long mean se, i(month mode state waters my) j(new) string
rename mean mrip_total 
rename se mrip_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new
destring month, replace 
merge 1:1 month mode state waters  species disp using `sim'


gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
drop if disp=="cat"
drop if mrip_total==. & sim_total==0

gen domain=species+"_"+disp

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total
sort pct_diff

tostring month, gen(month1)
sort month mode
gen region=state+"_"+waters

tempfile new
save `new', replace 

gr drop _all

levelsof region, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if region=="`s'"
	
	tempfile new1
	save `new1', replace
	
	levelsof domain, local(domain_list)
		
		foreach d in `domain_list' {
		
		u `new1', clear
		replace my_dom_id_string=mode+"_"+month1
		
		keep if domain=="`d'"
		sort month mode
		encode my_dom_id_string, gen(my_dom_id)
		gen my_dom_id_mrip = my_dom_id+0.1 
		gen my_dom_id_sim = my_dom_id-0.1  

* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}

qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("") xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  labsize(small) angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }
  
u `new', clear 
local statez "CT_CSTL MA_CSTL MD_CHES MD_CSTL ME_CSTL NC_CSTL NH_CSTL NJ_CSTL NY_CSTL RI_CSTL VA_CHES VA_CSTL"
foreach s of local statez {
	
grc1leg  str_keep_`s' blu_keep_`s'  str_rel_`s' blu_rel_`s'    , cols(2) title("Mean catch-per-trip, MRIP vs. simulated estimates `s'", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_`s'.png", as(png) replace

}

local statez "DE_CSTL"
foreach s of local statez {
	
grc1leg  blu_keep_`s'  str_rel_`s' blu_rel_`s'    , cols(2) title("Mean catch-per-trip, MRIP vs. simulated estimates `s'", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_`s'.png", as(png) replace

}






*B3 compare catch totals @ mode and month level
u "$iterative_input_data_cd\simulated_catch_totals.dta", clear 
rename dtrip tot_dtrip_sim
ds draw mode month state, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

	
collapse (sum) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim , by( mode  month draw region )

collapse (mean) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim ///
				(sd)	sd_str_keep_sim=tot_str_keep_sim sd_str_cat_sim =tot_str_cat_sim sd_str_rel_sim =tot_str_rel_sim ///
						  sd_blu_keep_sim=tot_blu_keep_sim sd_blu_rel_sim =tot_blu_rel_sim sd_blu_cat_sim =tot_blu_cat_sim ///
						  sd_dtrip_sim=tot_dtrip_sim, by( mode month region )
						  
reshape long tot_ sd_, i(mode month region ) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
replace disp="dtrip" if species=="dtrip"
replace species="NA" if disp=="dtrip"

preserve
keep if disp=="dtrip"
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd
tempfile simdtrip
save `simdtrip', replace
restore 

drop if disp=="dtrip"
destring month, replace
tempfile sim
save `sim', replace

u  "$input_data_cd\mrip_catch_by_region_mode_month.dta", clear 
rename tot mrip_total 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul

split var, parse(_)
rename varname1 species
rename varname2 disp
destring month, replace

drop varname 

merge 1:1  region mode month species disp  using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort  species disp mode month 
destring month, replace

tempfile catch
save `catch', replace 


u  "$input_data_cd\mrip_dtrip_by_region_mode_month.dta", clear 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul
rename dtrip mrip_total
gen disp="dtrip"
gen species="NA"
destring month, replace 
merge 1:1 region mode month species disp  using `simdtrip', keep(3)

append using `catch'

drop _merge mrip_se 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

ds mode month disp species domain region, not
local var = r(varlist)
foreach v of local var{
	format `v' %12.2gc
}

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total

sort pct_diff
sort diff
tostring month, gen(month1)

gen  my_dom_id_string=region+"_"+month1+"_"+mode

tempfile new
save `new', replace 

gr drop _all
levelsof region, local(state_list) 

foreach s in `state_list'{
	u `new', clear 
	*local s "NJALL"
	
	keep if region=="`s'"
	replace my_dom_id_string = month1+"_"+mode
	encode my_dom_id_string , gen(my_dom_id)  
	gen my_dom_id_mrip = my_dom_id+0.1 
	gen my_dom_id_sim = my_dom_id-0.1  
		
	levelsof domain, local(domain_list)
		foreach d in `domain_list' {


* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}


qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("# ('000s)", xoffset(-3)) xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  angle(45) labsize(small)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }

u `new', clear 

local statez "CTALL DEDERIV DENANT DEOCN MAALL MDCHES MDOCN MDPOT MEALL NCALB NCCNTRL NCOCN NHALL NJALL NYHUDN NYHUDS RIALL VACHES VAOCN VAPOTO"
foreach s of local statez {
	
grc1leg  str_harvest_`s' blu_harvest_`s'  str_discards_`s' blu_discards_`s'    , cols(2) title("Catch totals `s', MRIP vs. simulated estimates", size(small))
graph export "$figure_cd/total_catch_mode_month_MRIP_simulated_`s'.png", as(png) replace

}

	
	/*
*B3 compare catch totals @ mode  level
local s "MA"
u "$iterative_input_data_cd\simulated_catch_totals_`s'.dta", clear 
rename dtrip tot_dtrip_sim
drop my
ds draw mode month state, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

	
collapse (sum) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim , by( mode  draw state)

collapse (mean) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim ///
				(sd)	sd_str_keep_sim=tot_str_keep_sim sd_str_cat_sim =tot_str_cat_sim sd_str_rel_sim =tot_str_rel_sim ///
						  sd_blu_keep_sim=tot_blu_keep_sim sd_blu_rel_sim =tot_blu_rel_sim sd_blu_cat_sim =tot_blu_cat_sim ///
						  sd_dtrip_sim=tot_dtrip_sim, by( mode state)
						  
reshape long tot_ sd_, i(mode state) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
replace disp="dtrip" if species=="dtrip"
replace species="NA" if disp=="dtrip"

preserve
keep if disp=="dtrip"
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd
tempfile simdtrip
save `simdtrip', replace
restore 

drop if disp=="dtrip"
tempfile sim
save `sim', replace

u  "$input_data_cd\mrip_catch_by_state_mode.dta", clear 
keep if state=="`s'"
rename tot mrip_total 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul

split var, parse(_)
rename varname1 species
rename varname2 disp

drop varname 

merge 1:1  state mode  species disp  using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort  species disp mode
tempfile catch
save `catch', replace 


u  "$input_data_cd\mrip_dtrip_by_state_mode.dta", clear 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul
rename b mrip_total
sort state mode 
gen disp="dtrip"
gen species="NA"
keep if state=="`s'"
merge 1:1 state mode  species disp  using `simdtrip', keep(3)

append using `catch'

drop _merge mrip_se 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

ds mode disp species domain state, not
local var = r(varlist)
foreach v of local var{
	format `v' %12.2gc
}

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total

sort pct_diff
sort diff

tempfile new
save `new', replace 


levelsof disp, local(disp_list)

foreach s in `disp_list'{
	u `new', clear 
	keep if disp=="`s'"
	
	gen my_dom_id_string=mode
	encode my_dom_id_string , gen(my_dom_id)  
	gen my_dom_id_mrip = my_dom_id+0.1 
	gen my_dom_id_sim = my_dom_id-0.1  
		
	levelsof domain, local(domain_list)
		foreach d in `domain_list' {


* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}


qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("# ('000s)", xoffset(-3)) xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  angle(45) labsize(small)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }

u `new', clear 
grc1leg  str_catch_catch  blu_catch_catch str_harvest_harvest  blu_harvest_harvest , cols(2)  title("Catch totals `s', MRIP vs. simulated estimates", size(small))
*graph export "$figure_cd/mode_catch_total_MRIP_simulated.png", as(png) replace

grc1leg  dtrip_dtrip, cols(1)  title("Directed trip totals `s', MRIP vs. simulated estimates", size(small))
*graph export "$figure_cd/mode_dtrip_total_MRIP_simulated.png", as(png) replace
gr drop _all	
	
*/
** FINAL STEP

* Once the simulated totals approximate MRIP, save the data to be used in the R code simulation
u "$iterative_input_data_cd\simulated_catch_totals.dta", replace 
rename dtrip tot_dtrip_sim
ds draw mode month state region waters, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

preserve
collapse (sum) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim , by(state waters draw)				  
save "$iterative_input_data_cd\simulated_catch_totals_for_length_distn.dta", replace 
restore 


collapse (sum) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim , by( region mode draw)	
						  
save "$iterative_input_data_cd\simulated_catch_totals_final.dta", replace 


/*
* Remove extraneous columns from the catch-per-trip data
mata: mata clear
clear
local i 1
forvalues i = 1/$ndraws {
		use "$iterative_input_data_cd\calib_catch_draws_`s'`i'.dta", clear 
	   drop my_dom_id_string str_keep_sim str_rel_sim blu_keep_sim blu_rel_sim  dtrip
	   compress
	   save  "$iterative_input_data_cd\calib_catch_draws_`i'`s'.dta", replace
	}

*/
		

	
	
	