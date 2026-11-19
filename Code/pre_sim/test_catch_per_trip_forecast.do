
* Pull in MRIP data



cd "E:\Lou_projects\mrip_data"

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")

tempfile base
save `base', replace

clear
tempfile master
save `master', emptyok
		
forv y=1982(1)2025{
	
* Format MRIP data for estimation 
u `base', clear
keep if inlist(year, `y')

* Ensure only relevant states 
keep if inlist(st, 25, 44, 9, 36, 34, 10, 24, 51, 37, 23, 33)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33


 
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

drop date 
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace
destring month, replace
gen date2=dmy(day1, month, year)
format date2 %td




* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZZ"
replace common_dom="STR" if inlist(common, "stripedbass") 
replace common_dom="STR" if inlist(common, "bluefish") 

replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="STR"  if inlist(prim1_common, "bluefish") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=common_dom

* Define the list of species to process
local species "stripedbass bluefish"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "stripedbass" local short "str"
    if "`s'" == "bluefish"     local short "blu"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)
	drop `short'_tot_cat
	
    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)
	drop `short'_harvest

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
	drop `short'_releases

}

rename sum_str_tot_cat str_cat
rename sum_str_harvest str_keep
rename sum_str_releases str_rel
rename sum_blu_tot_cat blu_cat
rename sum_blu_harvest blu_keep
rename sum_blu_releases blu_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
gen byte no_dup = !inlist(common, "stripedbass", "bluefish")

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

/*
local vars sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel  scup_catch scup_keep scup_rel
foreach v of local vars{
	replace `v'=round(`v')
}
*/

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
keep my_dom_id  my_dom_id_string
tempfile domains
save `domains', replace 
restore


tempfile base1 
save `base1', replace 


encode mode1, gen(mode2)


tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in  str_cat   blu_cat  {

    * Run svy mean for the variable by domain
    svy: mean `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 


keep varname mean se my_dom_id`d' my_dom_id_string`d'
gen pse=(se/mean)*100

format mean %12.0gc

gen year=`y'
rename mean estimate
gen statistic= "mean_catch_per_trip"


append using `master'
save `master', replace
clear       
   
u `base1', clear 

tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in  str_cat   blu_cat  {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 


keep varname mean se my_dom_id`d' my_dom_id_string`d'
gen pse=(se/mean)*100

format mean %12.0gc

gen year=`y'
rename mean estimate
gen statistic= "total_catch"


append using `master'
save `master', replace
clear       
   


                  
}

use `master', clear

save "historcial_str_blu_cpue.dta", replace




cd "E:\Lou_projects\mrip_data"
u "historcial_str_blu_cpue.dta", clear 
keep if strmatch(my_dom_id_string, "*STR*")==1
keep if varname=="str_cat"

*drop if statistic=="total_catch"
*drop if year>2023
*------------------------------------------------------------*
* Create historical striped bass recruitment and SSB dataset
* Values are approximate and digitized from the figure
*------------------------------------------------------------*

preserve

clear

input int year double(recruit ssb)
1982   40    38
1983   75    33
1984   65    34
1985   70    33
1986   70    34
1987   75    43
1988   90    62
1989  105    85
1990  115   101
1991  135   114
1992  115   143
1993  135   163
1994  290   187
1995  185   195
1996  235   216
1997  260   204
1998  145   195
1999  150   195
2000  125   223
2001  230   232
2002  195   257
2003  125   260
2004  305   251
2005  160   245
2006  135   236
2007   90   232
2008  135   241
2009   75   237
2010  100   234
2011  125   219
2012  200   213
2013   70   189
2014   85   167
2015  155   150
2016  225   146
2017  110   137
2018  130   134
2019  165   140
2020  125   145
2021   85   156
2022   75   184
2023   95   189
end

label variable recruit ///
    "Recruitment, millions of age-1 striped bass"

label variable ssb ///
    "Female spawning stock biomass, millions of pounds"

tempfile striped_bass_stock
save `striped_bass_stock'

* Return to the dataset that was originally open
restore

*------------------------------------------------------------*
* Merge stock variables onto the open dataset
*------------------------------------------------------------*

merge m:1 year using `striped_bass_stock'

egen max_ssb = max(ssb)
gen ssb_norm = ssb / max_ssb
drop max_ssb

*drop if year==2024
encode statistic, gen(stat1)

xtset stat1 year
gen rebuild_dum=1 if inrange(year, 1985, 1990)
mvencode rebuild_dum, mv(0) override


tsline estimate if year>2000 & statistic=="total_catch", ylabel(#8) tlabel(#20, labsize(small) angle(45)) ytitle("Number of fish") title("Striped bass coastwide catch")
tsline estimate if year>2000 & statistic=="mean_catch_per_trip", ylabel(#8) tlabel(#20, labsize(small) angle(45)) ytitle("Mean catch-per-trip") title("Striped bass catch-per-trip")
replace estimate=estimate/1000000 if statistic=="total_catch"

twoway ///
    (connected estimate year if year >= 2000 & statistic == "total_catch", sort yaxis(1) lwidth(medthick) msymbol(circle)) ///
    (connected estimate year if year >= 2000 & statistic == "mean_catch_per_trip", sort yaxis(2) lwidth(medthick) msymbol(circle)), ///
    ylabel(#8, axis(1) format(%12.0fc) labsize(small)) ///
    ylabel(#8, axis(2) format(%9.2f) labsize(small)) ///
    xlabel(2000(1)2025, labsize(small) angle(45)) ///
    ytitle("Number of fish (millions)", axis(1)) ///
    ytitle("Mean catch per trip", axis(2)) ///
    xtitle("") /*xline(2023 2024)*/ ///
    title("") ///
    legend(order(1 "Total catch" 2 "Mean catch-per-trip") rows(1) position(6))
	
gr export "E:\Lou_projects\striper_bluefish_RDM\striper_catch.png", replace
	
	
	
twoway ///
    (connected recruit year  , sort yaxis(1) lwidth(medthick) msymbol(circle) lcolor(green) mcolor(green)) ///
    (connected estimate year if statistic == "mean_catch_per_trip", sort yaxis(2) lwidth(medthick) msymbol(circle)) ///
	(connected ssb_norm year, sort yaxis(2) lwidth(medthick) msymbol(circle)), ///
    ylabel(#8, axis(1) format(%12.0fc) labsize(small)) ///
    ylabel(#8, axis(2) format(%9.2f) labsize(small)) ///
    xlabel(1982(2)2025, labsize(small) angle(45)) ///
    ytitle("Number of fish (millions)", axis(1)) ///
    ytitle("Mean catch-per-trip/normalized SSB", axis(2)) ///
    xtitle("")  ///
    title("") ///
    legend(order(1 "Recruitment" 2 "Mean catch-per-trip"  3 "normalized SSB") rows(1) position(6))
	
gr export "E:\Lou_projects\striper_bluefish_RDM\striper_recr.png", replace


keep if statistic=="mean_catch_per_trip"
sort year
tsset year

gen rec1 = L1.recruit
gen rec2 = L2.recruit
gen rec3 = L3.recruit
gen rec4 = L4.recruit
gen rec5 = L5.recruit
gen rec6 = L6.recruit
gen rec7 = L7.recruit
gen rec8 = L8.recruit



gen byte common_sample = year>=1990 & year<=2023 ///
    & !missing(rec8, L.ssb_norm, estimate)
	
	
* Set the rebuilding-regime indicator for 2024
* Change this value if your definition differs

replace rebuild_dum = 0 if year >1990

egen rec_lag1_8_mean = rowmean(rec1 rec2 rec3 rec4 rec5 rec6 rec7 rec8)

egen rec_lag1_4_mean = rowmean(rec1 rec2 rec3 rec4)

egen rec_lag5_8_mean = rowmean(rec5 rec6 rec7 rec8)

egen rec_lag1_3_mean = rowmean(rec1 rec2 rec3)
egen rec_lag4_6_mean = rowmean(rec4 rec5 rec6)

label variable rec_lag1_8_mean "Mean recruitment, lags 1-8"
label variable rec_lag1_4_mean "Mean recruitment, lags 1-4"
label variable rec_lag5_8_mean "Mean recruitment, lags 5-8"
label variable rec_lag1_3_mean "Mean recruitment, lags 1-3"
label variable rec_lag4_6_mean "Mean recruitment, lags 4-6"





cd "E:\Lou_projects\mrip_data"
save "mean_catch_per_trip.dta", replace 




u  "mean_catch_per_trip.dta", clear 
local first_test_year 2020
local last_test_year  2025
*replace estimate=log(estimate)
rename estimate mean 
tempfile backtest_results

tempname post_results

postfile `post_results' ///
    str25 model ///
    int forecast_year ///
    double actual predicted error abs_error sq_error pct_error ///
    int n_estimation ///
    using `backtest_results', replace
	
forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        L(1/8).recruit ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("lag8") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}

forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        L(1/2).recruit ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("lag12") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}

forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        L(1/3).recruit ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("lag13") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}

forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        L(1/4).recruit ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("lag14") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}

forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        rec_lag1_3_mean ///
		rec_lag4_6_mean ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("lag13_46_groups") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}


forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        rec_lag1_8_mean ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("rec_lag1_8_mean") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}

forvalues y = `first_test_year'/`last_test_year' {

    capture quietly regress mean ///
        rec_lag1_4_mean ///
        rec_lag5_8_mean ///
        L.ssb_norm ///
        rebuild_dum ///
        if year < `y'

    if _rc == 0 {

        quietly predict double pred_temp if year == `y', xb

        quietly summarize mean if year == `y', meanonly
        local actual = r(mean)

        quietly summarize pred_temp if year == `y', meanonly
        local predicted = r(mean)

        local error     = `actual' - `predicted'
        local abs_error = abs(`error')
        local sq_error  = `error'^2
        local pct_error = 100 * `error' / `actual'
        local n_est     = e(N)

        post `post_results' ///
            ("recgroups") ///
            (`y') ///
            (`actual') ///
            (`predicted') ///
            (`error') ///
            (`abs_error') ///
            (`sq_error') ///
            (`pct_error') ///
            (`n_est')

        drop pred_temp
    }
}

postclose `post_results'

use `backtest_results', clear

sort model forecast_year

gen ape = abs(pct_error)

collapse ///
    (count) n_forecasts = forecast_year ///
    (mean)  bias        = error ///
            mae         = abs_error ///
            mse         = sq_error ///
            mape        = ape, ///
    by(model)

gen rmse = sqrt(mse)

gen model_order = .
replace model_order = 1 if model=="lag8"
replace model_order = 2 if model=="lag12"
replace model_order = 3 if model=="lag13"
replace model_order = 4 if model=="lag14"
replace model_order = 5 if model=="rec_lag1_8_mean"
replace model_order = 6 if model=="lag13_46_groups"
replace model_order = 7 if model=="recgroups"

gen str45 model_label = ""
replace model_label = "Recruitment lags 1-8" if model=="lag8"
replace model_label = "Recruitment lags 1-2" if model=="lag12"
replace model_label = "Recruitment lags 1-3" if model=="lag13"
replace model_label = "Recruitment lags 1-4" if model=="lag14"
replace model_label = "Recruitment mean (lag 1 - 8)" if model=="rec_lag1_8_mean"
replace model_label = "Recruitment mean groups (lags 1-4, 5-8)" if model=="recgroups"
replace model_label = "Recruitment mean groups (lags 1-3, 4-6)" if model=="lag13_46_groups"

sort model_order
order model_label n_forecasts bias mae rmse mape

export excel model_label n_forecasts bias mae rmse mape ///
    using "catch_per_trip_backtest_results.xlsx", ///
    sheet("Backtest performance") firstrow(variables) replace
	
order model n_forecasts bias mae rmse mape
sort rmse

format bias mae rmse %9.3f
format mape %9.2f

list, noobs separator(0)

use `backtest_results', clear
*save "catch_model_backtest_predictions.dta", replace
*replace actual=exp(actual)
*replace predicted=exp(predicted)

twoway ///
    (line actual forecast_year if model == "lag8", ///
        lcolor(black) lwidth(medthick)) ///
	(line predicted forecast_year if model == "lag8") ///
    (line predicted forecast_year if model == "lag12") ///
	(line predicted forecast_year if model == "lag13") ///
    (line predicted forecast_year if model == "lag14") ///
	(line predicted forecast_year if model == "rec_lag1_8_mean") ///
    (line predicted forecast_year if model == "recgroups") ///
	(line predicted forecast_year if model == "lag13_46_groups") , ///
    xtitle("Forecast year") ///
    ytitle("Mean catch-per-trip") ///
    legend(order( ///
        1 "Observed" ///
        2 "Lags 1-8" ///
        3 "Lags 1-2" ///
	    4 "Lags 1-3" ///
		5 "Lags 1-4" ///
        6 "Recruitment mean (lag 1 - 8)" ///
        7 "Recruitment mean groups (lags 1-4, 5-8)" ///
		8 "Recruitment mean groups (lags 1-3, 4-6)" ) ///
        rows(4) position(6) size(small)) ///
    graphregion(color(white))

gr export "E:\Lou_projects\striper_bluefish_RDM\striper_cpt_forecasts.png", replace

*--------------------------------------------------------------------*
* Forecast performance table
*--------------------------------------------------------------------*

use `backtest_results', clear


*--------------------------------------------------------------------*
* Evaluate predictive performance using year-to-year percent changes
*--------------------------------------------------------------------*

use `backtest_results', clear

sort model forecast_year

* Previous-year actual and predicted values
by model: gen actual_previous    = actual[_n-1]    if forecast_year == forecast_year[_n-1] + 1
by model: gen predicted_previous = predicted[_n-1] if forecast_year == forecast_year[_n-1] + 1

* Actual and predicted year-to-year percent changes
gen actual_pct_change = 100 * (actual / actual_previous - 1) ///
    if !missing(actual_previous) & actual_previous != 0

gen predicted_pct_change = 100 * (predicted / predicted_previous - 1) ///
    if !missing(predicted_previous) & predicted_previous != 0

* Error in predicted annual change, measured in percentage points
gen pct_change_error = predicted_pct_change - actual_pct_change
gen abs_pct_change_error = abs(pct_change_error)
gen sq_pct_change_error = pct_change_error^2

* Whether the model predicts the correct direction of annual change
gen byte correct_direction = ///
    sign(predicted_pct_change) == sign(actual_pct_change) ///
    if !missing(predicted_pct_change, actual_pct_change)
	

collapse ///
    (count) n_change_forecasts = pct_change_error ///
    (mean) bias_pct_change = pct_change_error ///
           mae_pct_change = abs_pct_change_error ///
           mse_pct_change = sq_pct_change_error ///
           direction_accuracy = correct_direction, ///
    by(model)

gen rmse_pct_change = sqrt(mse_pct_change)
drop mse_pct_change

replace direction_accuracy = 100 * direction_accuracy

sort rmse_pct_change

list model n_change_forecasts bias_pct_change mae_pct_change ///
    rmse_pct_change direction_accuracy, noobs
	
export excel ///
   model  n_change_forecasts bias_pct_change mae_pct_change ///
    rmse_pct_change direction_accuracy ///
    using "catch_per_trip_forecast_performance_pct_change.xlsx", ///
    firstrow(variables) replace
	
	
* Absolute percentage error
gen ape = abs(pct_error)

* Collapse forecast errors across forecast years
collapse ///
    (count) n_forecasts = forecast_year ///
    (mean)  bias        = error ///
            mae         = abs_error ///
            mse         = sq_error ///
            mape        = ape, ///
    by(model)

gen rmse = sqrt(mse)
drop mse

* Order and label models
gen model_order = .
replace model_order = 1 if model == "lag8"
replace model_order = 2 if model == "lag12"
replace model_order = 3 if model == "lag13"
replace model_order = 4 if model == "lag14"
replace model_order = 5 if model == "rec_lag1_8_mean"
replace model_order = 6 if model == "lag13_46_groups"
replace model_order = 7 if model == "recgroups"

gen str60 model_label = ""
replace model_label = "Recruitment lags 1-8" if model == "lag8"
replace model_label = "Recruitment lags 1-2" if model == "lag12"
replace model_label = "Recruitment lags 1-3" if model == "lag13"
replace model_label = "Recruitment lags 1-4" if model == "lag14"
replace model_label = "Mean recruitment, lags 1-8" if model == "rec_lag1_8_mean"
replace model_label = "Mean recruitment groups, lags 1-3 and 4-6" if model == "lag13_46_groups"
replace model_label = "Mean recruitment groups, lags 1-4 and 5-8" if model == "recgroups"

sort model_order

* Formatting for display/export
format bias mae rmse %9.3f
format mape %9.1f

order model_label n_forecasts bias mae rmse mape

label variable model_label "Model"
label variable n_forecasts "Forecast years"
label variable bias "Bias"
label variable mae "MAE"
label variable rmse "RMSE"
label variable mape "MAPE (%)"

*--------------------------------------------------*
* Word/RTF table
*--------------------------------------------------*
*--------------------------------------------------*
* Word-ready table using collect
*--------------------------------------------------*

collect clear

table (model_label) (), ///
    statistic(mean bias) ///
    statistic(mean mae) ///
    statistic(mean rmse) ///
    statistic(mean mape) ///
    statistic(mean n_forecasts)

collect label levels result ///
    mean_bias       "Bias" ///
    mean_mae        "MAE" ///
    mean_rmse       "RMSE" ///
    mean_mape       "MAPE (%)" ///
    mean_n_forecasts "Forecast years", modify

collect style cell result[mean_bias mean_mae mean_rmse], nformat(%9.3f)
collect style cell result[mean_mape], nformat(%9.1f)
collect style cell result[mean_n_forecasts], nformat(%9.0f)

collect preview

collect export ///
    "E:\Lou_projects\striper_bluefish_RDM\catch_per_trip_forecast_performance.docx", ///
    replace
	
restore
	
	
	
*--------------------------------------------------------------------*
* Full-sample reduced-form catch-per-trip models
*--------------------------------------------------------------------*

* Data should already contain one observation per year for mean CPUE
keep if statistic=="mean_catch_per_trip"
sort year
tsset year

* Recruitment variables, if not already generated
capture confirm variable rec1
if _rc {
    gen rec1 = L1.recruit
    gen rec2 = L2.recruit
    gen rec3 = L3.recruit
    gen rec4 = L4.recruit
    gen rec5 = L5.recruit
    gen rec6 = L6.recruit
    gen rec7 = L7.recruit
    gen rec8 = L8.recruit

    egen rec_lag1_8_mean = rowmean(rec1 rec2 rec3 rec4 rec5 rec6 rec7 rec8)
    egen rec_lag1_4_mean = rowmean(rec1 rec2 rec3 rec4)
    egen rec_lag5_8_mean = rowmean(rec5 rec6 rec7 rec8)
	egen rec_lag1_3_mean = rowmean(rec1 rec2 rec3 )
    egen rec_lag4_6_mean = rowmean(rec4 rec5 rec6)
}

* Clear previously stored estimates
estimates clear

* Model 1: recruitment lags 1-8
regress estimate L(1/8).recruit L.ssb_norm rebuild_dum
estimates store lag8

* Model 2: recruitment lags 1-2
regress estimate L(1/2).recruit L.ssb_norm rebuild_dum
estimates store lag12

* Model 2: recruitment lags 2-5
regress estimate L(1/3).recruit L.ssb_norm rebuild_dum
estimates store lag13

* Model 2: recruitment lags 3-6
regress estimate L(1/4).recruit L.ssb_norm rebuild_dum
estimates store lag14

* Model 3: recruitment lag 3
*regress estimate L3.recruit L.ssb_norm rebuild_dum
*estimates store lag3

* Model 4: mean recruitment, lags 1-8 only
*regress estimate rec_lag1_8_mean
*estimates store rec_lag1_8

* Model 5: mean recruitment, lags 1-8 
regress estimate rec_lag1_8_mean L.ssb_norm rebuild_dum
estimates store recmean


* Model 5: mean recruitment, lags 1-8 
regress estimate rec_lag1_3_mean rec_lag4_6_mean  L.ssb_norm rebuild_dum
estimates store recgroups13


* Model 6: preferred grouped-recruitment specification
regress estimate rec_lag1_4_mean rec_lag5_8_mean L.ssb_norm rebuild_dum
estimates store recgroups


capture which esttab
if _rc {
    ssc install estout
}

esttab lag8 lag12 lag13 lag14 recmean recgroups13 recgroups ///
    using "catch_per_trip_regression_results.rtf", replace ///
    b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2_a, fmt(0 3) labels("Observations" "Adjusted R-squared")) ///
    mtitles("Lags 1-8" "Lags 1-2" "Lag 1-3" "Lag 1-4"  "Recruitment mean (lag 1 - 8)" "Recruitment mean groups (lags 1-3, 4-6)" "Recruitment mean groups (lags 1-4, 5-8)") ///
    varlabels( ///
        L1.recruit "Recruitment, lag 1" ///
        L2.recruit "Recruitment, lag 2" ///
        L3.recruit "Recruitment, lag 3" ///
        L4.recruit "Recruitment, lag 4" ///
        L5.recruit "Recruitment, lag 5" ///
        L6.recruit "Recruitment, lag 6" ///
        L7.recruit "Recruitment, lag 7" ///
        L8.recruit "Recruitment, lag 8" ///
        rec_lag1_8_mean "Mean recruitment, lags 1-8" ///
        rec_lag1_4_mean "Mean recruitment, lags 1-4" ///
        rec_lag5_8_mean "Mean recruitment, lags 5-8" ///
		rec_lag1_3_mean "Mean recruitment, lags 1-3" ///
        rec_lag4_6_mean "Mean recruitment, lags 4-6" ///
        L.ssb_norm "Lagged SSB" ///
        rebuild_dum "Rebuilding period" ///
        _cons "Constant") ///
    label compress nogaps