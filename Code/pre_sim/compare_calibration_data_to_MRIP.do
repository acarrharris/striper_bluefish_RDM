


*B) The copula model data is used to generate daily catch-draw data, so here, I:
		* 1) compute total catch/harvest/discards from the daily catch-draw data by multiplying
		*    mean catch/harvest/discards-per trip by the number of trips in that day
		* 2) compare catch-per-trip means and total simulated catch from 1) with estimates from MRIP at various levels of aggregation

*B1 and B2)  
clear
tempfile master
save `master', emptyok

local regions "CTALL DEDERIV DENANT DEOCN MAALL MDCHES MDOCN MDPOT MEALL NCALB NCCNTRL NCOCN NHALL NJALL NYHUDN NYHUDS RIALL VACHES VAOCN VAPOTO"

foreach s of local regions {

    forvalues i = 1/$ndraws {

	 *----------------------------------------------------------------------
        * Construct the complete annual date series from directed trips
        *----------------------------------------------------------------------

		*local i 1
		*local s "NCOCN"
		
        import delimited using "$iterative_input_data_cd\directed_trip_draws_`s'.csv", clear

        keep if draw == `i'

        gen double date_num = date(date, "DMY")
        format date_num %td

        drop date
        rename date_num date

        /*
        The directed-trip data provide the complete annual time series.
        There should be one observation per region × draw × mode × date.
        */
        isid region draw mode date

        /*
        Retain dates with zero trips. They are part of the complete annual
        time series and should not be dropped before the merge.
        */
        replace dtrip = 0 if missing(dtrip)
		
		preserve 
		keep date region mode
		duplicates drop 
		tempfile dates
		save `dates', replace 
		restore
		
		gen month = month(date)

		collapse (sum) dtrip, by(region mode month)
		
		tempfile trips
		save `trips', replace 
		
        *----------------------------------------------------------------------
        * Process simulated catch draws
        *----------------------------------------------------------------------

        use "$iterative_input_data_cd\calib_catch_draws_`s'`i'.dta", clear

        rename str_keep str_keep_sim
        rename str_rel  str_rel_sim
        rename blu_keep blu_keep_sim
        rename blu_rel  blu_rel_sim

        foreach v in str_keep_sim str_rel_sim blu_keep_sim blu_rel_sim {
            replace `v' = 0 if missing(`v')
            recast double `v'
        }

        /*
        Retain one simulated catch realization per synthetic trip and date.

        Delete this restriction if you instead want to average all catch_draw
        and tripid realizations within each date.
        */
        keep if catch_draw == 1 & tripid == 1
		merge m:1 region mode date using `dates'
		
		/*
        Directed-trip observations without a matching calibration catch draw
        represent zero simulated catch on that date.
        */
        foreach v in str_keep_sim str_rel_sim blu_keep_sim blu_rel_sim {
            replace `v' = 0 if _merge == 2  | missing(`v')
        }

		gen month = month(date)

		
        /*
        Reduce to one simulated catch observation per region × mode × month.
        */
        collapse (mean) str_keep_sim str_rel_sim blu_keep_sim blu_rel_sim, by(region mode month)
		
		
		 merge 1:1 region mode month using `trips'

		gen double tot_str_keep_sim = dtrip * str_keep_sim
        gen double tot_str_rel_sim  = dtrip * str_rel_sim

        gen double tot_blu_keep_sim = dtrip * blu_keep_sim
        gen double tot_blu_rel_sim  = dtrip * blu_rel_sim

        gen double tot_str_cat_sim = tot_str_keep_sim + tot_str_rel_sim
        gen double tot_blu_cat_sim = tot_blu_keep_sim + tot_blu_rel_sim
		
		drop _merge
		
        /*
        Monthly simulated catch per trip is monthly total catch divided by
        monthly total directed trips. This appropriately weights daily catch
        rates by daily trips.
        */
		
        gen draw = `i'

        append using `master'
        save `master', replace
    }
}

use `master', clear

isid  region  mode month draw

assert abs(tot_str_cat_sim - (tot_str_keep_sim + tot_str_rel_sim)) < 1e-8
assert abs(tot_blu_cat_sim - (tot_blu_keep_sim + tot_blu_rel_sim)) < 1e-8


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

reshape long tot_ sd_, i(mode month state water) j(new) string
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
drop if strmatch(my_dom, "*ZZZ*")==1
merge 1:1 month mode state waters  species disp using `sim'


gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
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

levelsof region if state=="NC", local(state_list) 

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

 twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
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
local statez "NC_CSTL"

foreach s of local statez {
	
grc1leg  str_keep_`s' blu_keep_`s'  str_rel_`s' blu_rel_`s'    , cols(2) title("Mean catch-per-trip, MRIP vs. simulated estimates `s'", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_`s'.png", as(png) replace

}
/*
local statez "DE_CSTL"
foreach s of local statez {
	
grc1leg  blu_keep_`s'  str_rel_`s' blu_rel_`s'    , cols(2) title("Mean catch-per-trip, MRIP vs. simulated estimates `s'", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_`s'.png", as(png) replace
*/
}



*B3) compare catch totals @ mode and month level
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



*B3 compare catch totals @ region mode  level
u "$iterative_input_data_cd\simulated_catch_totals.dta", clear 
rename dtrip tot_dtrip_sim
ds draw mode month state, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

	
collapse (sum) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim , by( mode  draw region )

collapse (mean) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim ///
				(sd)	sd_str_keep_sim=tot_str_keep_sim sd_str_cat_sim =tot_str_cat_sim sd_str_rel_sim =tot_str_rel_sim ///
						  sd_blu_keep_sim=tot_blu_keep_sim sd_blu_rel_sim =tot_blu_rel_sim sd_blu_cat_sim =tot_blu_cat_sim ///
						  sd_dtrip_sim=tot_dtrip_sim, by( mode region )
						  
reshape long tot_ sd_, i(mode region ) j(new) string
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

u  "$input_data_cd\mrip_catch_by_region_mode.dta", clear 
rename tot mrip_total 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul

split var, parse(_)
rename varname1 species
rename varname2 disp

drop varname 

merge 1:1  region mode species disp  using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort  species disp mode  

tempfile catch
save `catch', replace 


u  "$input_data_cd\mrip_dtrip_by_state_mode.dta", clear 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul
rename dtrip mrip_total
gen disp="dtrip"
gen species="NA"
merge 1:1 region mode species disp  using `simdtrip', keep(3)

append using `catch'

drop _merge mrip_se 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

ds mode disp species domain region, not
local var = r(varlist)
foreach v of local var{
	format `v' %12.2gc
}

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total

sort pct_diff
sort diff

gen  my_dom_id_string=region+"_"+mode

tempfile new
save `new', replace 

gr drop _all
levelsof region if inlist(region, "NCALB", "NCCNTRL", "NCOCN"), local(state_list) 

foreach s in `state_list'{
	u `new', clear 
	*local s "NJALL"
	
	keep if region=="`s'"
	replace my_dom_id_string = mode
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
graph export "$figure_cd/total_catch_mode_MRIP_simulated_`s'.png", as(png) replace

}


*B3 compare catch totals @ region mode  level
********************************************************************************
* Compare simulated annual state catch totals with MRIP state estimates
********************************************************************************

*------------------------------------------------------------------------------
* 1. Simulated totals
*------------------------------------------------------------------------------

use "$iterative_input_data_cd\simulated_catch_totals.dta", clear
drop str_keep_sim str_rel_sim str_cat_sim blu_keep_sim blu_rel_sim blu_cat_sim
rename dtrip tot_dtrip_sim

* These variables contain total fish, despite currently lacking the tot_ prefix.
* Create two-letter state codes.
* Prefer the region variable when it is available.
capture confirm variable region

if !_rc {
    replace state = substr(region, 1, 2)
}
else {
    replace state = substr(state, 1, 2)
}

* Replace missing simulated totals with zero.
local total_vars tot_str_keep_sim tot_str_rel_sim tot_str_cat_sim tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim tot_dtrip_sim

foreach v of local total_vars {
    replace `v' = 0 if missing(`v')
    recast double `v'
}

* Enforce catch accounting identities.
replace tot_str_cat_sim = tot_str_keep_sim + tot_str_rel_sim
replace tot_blu_cat_sim = tot_blu_keep_sim + tot_blu_rel_sim

* Sum months, modes, and management regions to annual state totals
* within each simulation draw.
collapse (sum) tot_str_keep_sim tot_str_rel_sim tot_str_cat_sim tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim tot_dtrip_sim, by(draw state)

isid draw state

* Calculate mean annual totals and SD across simulation draws.
collapse (mean) tot_str_keep_sim tot_str_rel_sim tot_str_cat_sim tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim tot_dtrip_sim ///
         (sd) sd_str_keep_sim=tot_str_keep_sim sd_str_rel_sim=tot_str_rel_sim sd_str_cat_sim=tot_str_cat_sim ///
              sd_blu_keep_sim=tot_blu_keep_sim sd_blu_rel_sim=tot_blu_rel_sim sd_blu_cat_sim=tot_blu_cat_sim ///
              sd_dtrip_sim=tot_dtrip_sim, by(state)
preserve

keep state tot_dtrip_sim sd_dtrip_sim

rename tot_dtrip_sim sim_total
rename sd_dtrip_sim  sim_sd

gen double sim_ll = sim_total - 1.96 * sim_sd
gen double sim_ul = sim_total + 1.96 * sim_sd

tempfile simulated_trips
save `simulated_trips', replace

restore

drop tot_dtrip_sim sd_dtrip_sim

rename tot_str_keep_sim str_keep
rename tot_str_rel_sim  str_rel
rename tot_str_cat_sim  str_cat

rename tot_blu_keep_sim blu_keep
rename tot_blu_rel_sim  blu_rel
rename tot_blu_cat_sim  blu_cat

rename sd_str_keep_sim sd_str_keep
rename sd_str_rel_sim  sd_str_rel
rename sd_str_cat_sim  sd_str_cat

rename sd_blu_keep_sim sd_blu_keep
rename sd_blu_rel_sim  sd_blu_rel
rename sd_blu_cat_sim  sd_blu_cat

reshape long str_ blu_ sd_str_ sd_blu_, i(state) j(disp) string

rename str_    sim_str
rename blu_    sim_blu
rename sd_str_ sd_str
rename sd_blu_ sd_blu

reshape long sim_ sd_, i(state disp) j(species) string

rename sim_ sim_total
rename sd_  sim_sd

gen double sim_ll = sim_total - 1.96 * sim_sd
gen double sim_ul = sim_total + 1.96 * sim_sd

keep state species disp sim_total sim_sd sim_ll sim_ul

isid state species disp

tempfile simulated_catch
save `simulated_catch', replace



*------------------------------------------------------------------------------
* 2. MRIP state estimates
*------------------------------------------------------------------------------

use "$input_data_cd\mrip_catch_state.dta", clear

rename tot mrip_total
rename se  mrip_se
rename ll  mrip_ll
rename ul  mrip_ul

* Harmonize species abbreviations.
replace species = "str" if species == "striper"
replace species = "blu" if species == "bluefish"

* Keep estimates in numbers of fish.
drop if strpos(lower(metric), "lbs") > 0

* Harmonize metric names with simulation outcomes.
replace metric = "cat"  if metric == "catch (#s)"
replace metric = "keep" if metric == "harvest (#s)"
replace metric = "rel"  if metric == "discards (#s)"

rename metric disp

* Confirm there is one MRIP observation per merge key.
duplicates report state species disp
isid state species disp

* Merge MRIP and simulated state estimates.
merge 1:1 state species disp using `simulated_catch'

tabulate _merge

* Retain matched estimates.
keep if _merge == 3
drop _merge

* Readable labels.
replace disp = "discards" if disp == "rel"
replace disp = "harvest"  if disp == "keep"
replace disp = "catch"    if disp == "cat"

gen str30 domain = species + "_" + disp

label variable mrip_total "MRIP estimate"
label variable sim_total  "Simulated estimate"
label variable mrip_ll    "MRIP lower 95% CI"
label variable mrip_ul    "MRIP upper 95% CI"
label variable sim_ll     "Simulation mean - 1.96 SD"
label variable sim_ul     "Simulation mean + 1.96 SD"

* Differences relative to MRIP.
gen double diff = sim_total - mrip_total

gen double pct_diff = ///
    100 * (sim_total - mrip_total) / mrip_total ///
    if mrip_total != 0 & !missing(mrip_total)

format mrip_total sim_total mrip_ll mrip_ul sim_ll sim_ul ///
    diff %12.0fc

format pct_diff %9.2f

order state species disp domain ///
    mrip_total sim_total diff pct_diff ///
    mrip_ll mrip_ul sim_ll sim_ul

sort state species disp

tempfile comparison
save `comparison', replace

********************************************************************************
* Domain-specific plots across states
********************************************************************************

use `comparison', clear

gen state_id = .

replace state_id = 1  if state=="ME"
replace state_id = 2  if state=="NH"
replace state_id = 3  if state=="MA"
replace state_id = 4  if state=="RI"
replace state_id = 5  if state=="CT"
replace state_id = 6  if state=="NY"
replace state_id = 7  if state=="NJ"
replace state_id = 8  if state=="DE"
replace state_id = 9  if state=="MD"
replace state_id = 10 if state=="VA"
replace state_id = 11 if state=="NC"

label define state_lbl ///
    1 "ME" ///
    2 "NH" ///
    3 "MA" ///
    4 "RI" ///
    5 "CT" ///
    6 "NY" ///
    7 "NJ" ///
    8 "DE" ///
    9 "MD" ///
    10 "VA" ///
    11 "NC", replace

label values state_id state_lbl

gen x_mrip = state_id - .10
gen x_sim  = state_id + .10
levelsof domain, local(domain_list)

quietly summarize state_id
local max_state_id = r(max)

foreach d of local domain_list {

    local graph_name = strtoname("`d'_state")

    twoway ///
        (rcap mrip_ul mrip_ll x_mrip if domain == "`d'", ///
            lcolor(blue)) ///
        (scatter mrip_total x_mrip if domain == "`d'", ///
            msymbol(O) ///
            mcolor(blue)) ///
        (rcap sim_ul sim_ll x_sim if domain == "`d'", ///
            lcolor(red)) ///
        (scatter sim_total x_sim if domain == "`d'", ///
            msymbol(O) ///
            mcolor(red)), ///
        legend( ///
            order(2 "MRIP estimate" 4 "Simulated estimate") ///
            rows(1) ///
            size(small) ///
        ) ///
xlabel(1 "ME" 2 "NH" 3 "MA" 4 "RI" 5 "CT" 6 "NY" 7 "NJ" ///
       8 "DE" 9 "MD" 10 "VA" 11 "NC", angle(45) labsize(small)) ///
        xtitle("") ///
        ytitle("Number of fish") ///
        ylabel(, ///
            angle(horizontal) ///
            format(%12.0gc) ///
        ) ///
        title("`d'", size(medium)) ///
        name(`graph_name', replace)

    graph export ///
        "$figure_cd/total_catch_state_MRIP_simulated_`graph_name'.png", ///
        as(png) ///
        replace
}




use "$iterative_input_data_cd\simulated_catch_totals.dta", clear

keep if state == "NC"

rename dtrip tot_dtrip_sim

/*
Combine NCALB, NCCNTRL, and NCOCN within each draw.
*/
collapse (sum) tot_dtrip_sim ///
    tot_str_keep_sim tot_str_rel_sim tot_str_cat_sim ///
    tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim, ///
    by(draw mode month state waters)
	
collapse (sum) tot_dtrip_sim ///
    tot_str_keep_sim tot_str_rel_sim tot_str_cat_sim ///
    tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim, ///
    by(draw )
/*
Calculate the trip-weighted statewide catch per trip for each draw.
*/
gen double str_keep_sim = tot_str_keep_sim / tot_dtrip_sim
gen double str_rel_sim  = tot_str_rel_sim  / tot_dtrip_sim
gen double str_cat_sim  = tot_str_cat_sim  / tot_dtrip_sim

gen double blu_keep_sim = tot_blu_keep_sim / tot_dtrip_sim
gen double blu_rel_sim  = tot_blu_rel_sim  / tot_dtrip_sim
gen double blu_cat_sim  = tot_blu_cat_sim  / tot_dtrip_sim

/*
Summarize across simulation draws.
*/
collapse (mean) str_keep_sim str_rel_sim str_cat_sim ///
                    blu_keep_sim blu_rel_sim blu_cat_sim ///
         (sd) sd_str_keep_sim=str_keep_sim ///
              sd_str_rel_sim=str_rel_sim ///
              sd_str_cat_sim=str_cat_sim ///
              sd_blu_keep_sim=blu_keep_sim ///
              sd_blu_rel_sim=blu_rel_sim ///
              sd_blu_cat_sim=blu_cat_sim, ///
         by(mode month state waters)
		 
		 
*B3 compare catch totals @ coastwide level
u "$iterative_input_data_cd\simulated_catch_totals.dta", clear 
rename dtrip tot_dtrip_sim
ds draw mode month state, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}


collapse (sum) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim , by(  draw  )

collapse (mean) tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim ///
						  tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim ///
						  tot_dtrip_sim ///
				(sd)	sd_str_keep_sim=tot_str_keep_sim sd_str_cat_sim =tot_str_cat_sim sd_str_rel_sim =tot_str_rel_sim ///
						  sd_blu_keep_sim=tot_blu_keep_sim sd_blu_rel_sim =tot_blu_rel_sim sd_blu_cat_sim =tot_blu_cat_sim ///
						  sd_dtrip_sim=tot_dtrip_sim
gen tab=1 						  
reshape long tot_ sd_, i(tab) j(new) string
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

u  "$input_data_cd\mrip_catch_coast.dta", clear 
rename tot mrip_total 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul

rename metric disp
replace disp = "cat" if disp=="catch (#s)"
replace disp = "keep" if disp=="harvest (#s)"
replace disp = "rel" if disp=="discards (#s)"
drop if disp=="harvest (lbs.)"
replace species="blu" if species=="bluefish"
replace species="str" if species=="striper"

merge 1:1  species disp  using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort  species disp   
tempfile catch
save `catch', replace 


u  "$input_data_cd\mrip_dtrip_by_coast.dta", clear 
rename se mrip_se
rename ll mrip_ll
rename ul mrip_ul
rename dtrip mrip_total
gen disp="dtrip"
gen species="NA"
gen tab=1
merge 1:1 tab species disp  using `simdtrip', keep(3)

append using `catch'

drop _merge mrip_se 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

ds  disp species domain , not
local var = r(varlist)
foreach v of local var{
	format `v' %12.2gc
}

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total

sort pct_diff
sort diff

gen region="coast"
gen mode="all"
gen  my_dom_id_string=region+"_"+mode

tempfile new
save `new', replace 

gr drop _all
levelsof region, local(state_list) 

foreach s in `state_list'{
	u `new', clear 
	*local s "NJALL"
	
	keep if region=="`s'"
	replace my_dom_id_string = mode
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




*/
** FINAL STEP

* Once the simulated totals approximate MRIP, save the data to be used in the R code simulation
u "$iterative_input_data_cd\simulated_catch_totals.dta", replace 
    gen str2 state = substr(region,1,2)
    gen str5 waters = "CSTL"
    replace waters  = "CHES" if inlist(region,"MDCHES","VACHES")

rename dtrip tot_dtrip_sim
ds draw mode month  region waters, not
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
		

	
	
	