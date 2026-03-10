


 
* A) First generate 2024 catch-at-lengths. I do this by:
	* 1) Pull in simulated total catch by domain
	* 2) pull in the fitted catch-at-length probabilities. 
	* 3) multiply 2)  by each domain's total catch

* a1) 
* Import simulated total catch by season and species 	
set seed $seed

use "$input_data_cd\simulated_catch_totals.dta", clear 
keep if draw<= $ndraws

collapse (sum)  tot_cod_cat_sim tot_hadd_cat_sim, by(season draw)

reshape long tot_, i(draw season) j(species) string

split species, parse(_)
drop species species2 species3
rename species1 species
renam tot tot_catch 

order species season draw
format tot %12.0gc
sort draw species season

tempfile catch2024
save `catch2024', replace 

*a2) 
import delimited using "$input_data_cd/baseline_catch_at_length.csv", clear  
keep if draw<= $ndraws
sort draw season species length

merge m:1 species season draw using `catch2024'
drop _merge

*a3) 
gen cal=tot*fitted
sort draw season species length

gen domain=season+"_"+species

tempfile cal
save `cal', replace 


*B) Create age-length keys from NEFSC trawl survey data
	*b1) Pull in NEFSC trawl survey data from the last three years of data available
	*b2) Smooth counts across age classes over the range of observed catch-at-lengths for a given state-species using a LOWESS bandwidth=0.3
	*b3) Compute the proportion of fish of age a that are length l


* Cod 
* for cod, there are few obs for age 7+, combine these into 6+ category

*b1) 
import delimited using "$input_data_cd/NEFSC_cruises.csv", clear 
renvarlab, lower
tempfile cruises
sort year 
save `cruises', replace 

import delimited using "$input_data_cd/NEFSC_trawl_cod.csv", clear 
renvarlab, lower
rename count count 
merge m:1 cruise6 using `cruises'
collapse (sum) count, by(year season svspp age length)
tostring year, gen(year2)
gen yr_season=year2+"_"+season
tab yr_season if year>2020
keep if year>2022
collapse (sum) count, by(year age length)

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
di `min_svy_yr'
tabstat count, stat(sum) by(age)
replace age=6 if age>=6
collapse (sum) count, by (age length)
drop if age==. | length==.

tsset age length
tsfill, full

sort age length 
mvencode count, mv(0) override 

*b2) 
levelsof age, local(ages)
foreach a of local ages{
	lowess count length if age==`a' , adjust bwidth(.3) gen(s`a') nograph
	replace s`a'=0 if s`a'<=0
}

egen smoothed=rowtotal(s0-s6)
drop s0-s6

egen sum=sum(smoothed), by(age)	
gen prop_smoothed=smoothed/sum	

*b3) 
egen sum_raw=sum(count), by(age)	
gen prop_raw=count/sum_raw	

/*
levelsof age, local(ages)
foreach a of local ages{
twoway(scatter prop_raw length if age==`a',   connect(direct) lcol(red)   lpat(solid) msymbol(i) ) ///
			(scatter prop_smoothed length if age==`a', connect(direct) lcol(blue) title("cod age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr'", size(small)) ///
			ytitle("proportion of fish that are age-a", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cms, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames' 
graph export "$figure_cd/cod_prop_length_at_age.png", as(png) replace
*/

drop if age==0
drop sum sum_raw
tempfile al_cod
save `al_cod', replace 


* Haddock ALK - age 1 through 9 
*b1) 
import delimited using "$input_data_cd/NEFSC_cruises.csv", clear 
renvarlab, lower
tempfile cruises
sort year 
save `cruises', replace 

import delimited using "$input_data_cd/NEFSC_trawl_hadd.csv", clear 
renvarlab, lower
rename count count 
merge m:1 cruise6 using `cruises'
collapse (sum) count, by(year season svspp age length)
tostring year, gen(year2)
gen yr_season=year2+"_"+season
tab yr_season if year>2020
keep if year>2022
collapse (sum) count, by(year age length)

su year
local min_svy_yr=`r(min)'
local max_svy_yr=`r(max)'
di `min_svy_yr'
tabstat count, stat(sum) by(age)
replace age=9 if age>=9
collapse (sum) count, by (age length)
drop if age==. | length==.

tsset age length
tsfill, full

sort age length 
mvencode count, mv(0) override 

*b2) 
levelsof age, local(ages)
foreach a of local ages{
	lowess count length if age==`a' , adjust bwidth(.3) gen(s`a') nograph
	replace s`a'=0 if s`a'<=0
}

egen smoothed=rowtotal(s0-s9)
drop s0-s9

*b3) 
egen sum=sum(smoothed), by(age)	
gen prop_smoothed=smoothed/sum	

egen sum_raw=sum(count), by(age)	
gen prop_raw=count/sum_raw	

/*
levelsof age, local(ages)
foreach a of local ages{
twoway(scatter prop_raw length if age==`a',   connect(direct) lcol(red)   lpat(solid) msymbol(i) ) ///
			(scatter prop_smoothed length if age==`a', connect(direct) lcol(blue) title("haddock age `a' NEFSC trawl `min_svy_yr'-`max_svy_yr'", size(small)) ///
			ytitle("proportion of fish that are age-a", size(small)) ytick(, angle(horizontal) labsize(small)) xtitle(length cms, size(small)) xlab(, labsize(small)) ///
			ylab(, labsize(small) angle(horizontal)) xtick(, labsize(small)) lpat(solid) msymbol(i)  name(dom`a', replace))
 local graphnames `graphnames' dom`a'
}

grc1leg `graphnames' 
graph export "$figure_cd/hadd_prop_length_at_age.png", as(png) replace
*/

drop if age==0
drop sum sum_raw
tempfile al_hadd
save `al_hadd', replace 


*C) compute rec selectivity
	* c1) pull in historcial NAA
	* c2) translate ages to lengths using the age-length keys
	* c3) merge numbers-at-length to catch-at-length
	* c4) apply adjusmtnet code when catch-at-length is greater than numbers-at-length 
	* c5) compute rec selectivity ql=CAL/NAL
	
* c1) cod
use "$input_data_cd/WGOM_Cod_historical_NAA_from_2024Assessment.dta", clear 

egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
keep if year==2025
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 

* c2) cod
merge 1:m age using `al_cod', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length)

sort length 
gen species="cod"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

tempfile naa_cod
save `naa_cod', replace 

* c1) haddock 
use "$input_data_cd/GOM_Haddock_historical_NAA_2024Assessment.dta", clear 

keep if year==2025
reshape long age, i(year) j(new)
rename age nfish
rename new age 
drop year 

* c2) haddock
merge 1:m age using `al_hadd', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length)

sort length 
gen species="hadd"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

append using  `naa_cod'
replace NaL_from_raw_trawl=NaL_from_raw_trawl*1000
replace NaL_from_smooth_trawl=NaL_from_smooth_trawl*1000

* c3) both species
merge 1:m species season length using `cal', keep(2 3)
drop if draw==.

*c4)  Adjust the catch-at-length and population numbers-at-length data such that for a given length, 
	*    catch is not greater than the population number. I do this by creating plus groups of lengths until NaL>CaL. 
    *    For these plus groups, I retain the original proportion of fish caught by length and will merge this back into the project CaL. 

tostring draw, gen(draw2)
gen domain2=season+"_"+species+"_"+draw2
drop if length==.

mvencode NaL* cal, mv(0) override
gen tab=1 if cal>NaL_from_smooth_trawl & cal!=0
egen sumtab=sum(tab), by(domain2)
sort species season draw length 

gen length2=length 
levelsof domain2 if sumtab>0, local(domz)
foreach d of local domz{
	
	su length if domain2=="`d'" & NaL_from_smooth_trawl!=0
	local max=`r(max)'
	local min=`r(min)'

	replace length2=`max' if length>`max' & domain2=="`d'" 
	replace length2=`min' if length<`min' & domain2=="`d'" 
	
}
sort draw season species length 
egen cal2=sum(cal), by(domain2 length2)
egen nal2=sum(NaL_from_smooth_trawl), by(domain2 length2)
*drop tab sumtab

gen tab2=1 if cal2>nal2 & cal2!=0
egen sumtab2=sum(tab2), by(domain2)

drop if cal==0
gen cal_proportion=cal/cal2

preserve
keep domain2 length2 length cal_proportion
rename length length 
tempfile cal_proportion
save `cal_proportion', replace
restore

preserve
keep if sumtab2==0
tempfile okay
save `okay', replace
restore 

drop if sumtab2==0
append using `okay'	 

* c5)
collapse (sum) cal NaL_from_smooth_trawl, by(draw draw2 season species length2 domain2)
gen ql=cal/NaL_from_smooth_trawl

sort species season draw length
rename NaL_from_smooth_trawl naa_2025
drop  draw2 domain2
rename cal cal_2025

tempfile ql
save `ql', replace



*E) compute projected catch-at-length 
* 	1) Pull in the projected population numbers-at-age data
*	2) convert to lengths using the age-length keys from above
*	3) merge to the 2024 selectivities
* 	4) adjust for plus groups necessarily made when computing 2024 selectivities. 
*	5) compute 2026 catch-at-length numbers and probability distribution
	
* E1) cod
use "$input_data_cd/WGOM_Cod_projected_NAA_from_2024Assessment.dta", clear 

egen age6_plus=rowtotal(age6-age9)
drop age6 age7 age8 age9
rename age6 age6
sample $ndraws, count 
gen draw=_n
reshape long age, i(year draw replicate) j(new)
rename age nfish
rename new age 
drop year 
rename replicate cod_replicate

* e2) cod
preserve 
u `al_cod', clear 
expand $ndraws
bysort length age: gen draw=_n
tempfile al_cod_expand
save `al_cod_expand', replace
restore 

merge 1:m age draw using `al_cod_expand', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by(length draw cod_replicate)

sort length 
gen species="cod"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup

tempfile proj_naa_cod
save `proj_naa_cod', replace 

* e1) haddock 
use "$input_data_cd/GOM_Haddock_projected_NAA_2024Assessment.dta", clear 

sample $ndraws, count 
gen draw=_n
reshape long age, i(year draw replicate) j(new)
rename age nfish
rename new age 
drop year 
rename replicate hadd_replicate

preserve 
u `al_hadd', clear 
expand $ndraws
bysort length age: gen draw=_n
tempfile al_hadd_expand
save `al_hadd_expand', replace
restore 

* c2) haddock
merge 1:m age draw using `al_hadd_expand', keep(3) nogen 
sort  age length

gen NaL_from_raw_trawl = prop_raw*nfish
gen NaL_from_smooth_trawl = prop_smoothed*nfish

drop count  prop* nfish smoothed
collapse (sum) NaL*, by( length draw hadd_replicate)

sort length 
gen species="hadd"
expand 2, gen(dup)
gen season="winter" if dup==0
replace season="summer" if dup==1
drop dup


append using  `proj_naa_cod' 
rename length length2

replace NaL_from_raw_trawl=NaL_from_raw_trawl*1000
replace NaL_from_smooth_trawl=NaL_from_smooth_trawl*1000


merge 1:1 species season length2 draw using `ql', keep(3) nogen 


sort species season draw length 
order species season draw length 

rename NaL_from_smooth_trawl naa_2026
drop  NaL_from_raw_trawl

gen cal_2026= ql*naa_2026

tostring draw, gen(draw2)
gen domain2=season+"_"+species+"_"+draw2	
	
merge 1:m domain2 length2 using `cal_proportion'

sort draw species season length
*browse
replace cal_2026=cal_2026*cal_proportion
drop if cal_2026==. | cal_2026==0
	


keep draw season species length cal_* *replicate naa*
egen sum_cal=sum(cal_2026), by(draw season species)
gen fitted_prob=cal_2026/sum_cal
drop sum	
	
egen sum_cal=sum(cal_2025), by(draw season species)
gen fitted_prob_2025=cal_2025/sum_cal
drop sum	


rename fitted_prob fitted_prob_2026

* Plot catch-at-length probability distributions 2025 versus 2026
* 5cm length bins 
preserve
drop fitted_prob_2025 naa_2025 
rename fitted_prob_2026 fitted_prob
rename naa_2026 nal
gen year=2026
tempfile base
save `base'
restore

drop fitted_prob_2026 naa_2026
rename  fitted_prob_2025 fitted_prob
rename  naa_2025 nal

gen year=2025
append using `base'


gen length5_lo = floor(length/5)*5
gen length5_hi = length5_lo + 4

* String label: "20-24", "25-29", etc.
gen str20 length5_bin = string(length5_lo) + "-" + string(length5_hi)
label var length5_bin "Length bin (cm)"

preserve
collapse (sum) fitted_prob, by(year season species draw length5_bin)
egen sum_fitted_prob=sum(fitted_prob), by(year season species draw)
gen proportion=fitted_prob/sum
drop fitted_prob sum
reshape wide proportion , i(species season draw length ) j(year)


graph box proportion2025 proportion2026 if species =="cod" & season=="winter", ///
    over(length, label(labsize(small))) ///
    title("cod", size(medium)) ///
	ytitle(Proportion of fish that are length-{it:l}) ///
    ylab(, labsize(small)) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
    legend(order(1 "2025" 2 "2026") position(6) cols(2)) 

*graph export "$figure_cd/prop_nal_cod_by_year.png", as(png) replace


graph box proportion2025 proportion2026 if species =="hadd" & season=="winter", ///
    over(length, label(labsize(small))) ///
    title("haddock", size(medium)) ///
	ytitle(Proportion of fish that are length-{it:l}) ///
    ylab(, labsize(small)) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
    legend(order(1 "2025" 2 "2026") position(6) cols(2)) 

*graph export "$figure_cd/prop_nal_haddock_by_year.png", as(png) replace
restore

preserve
collapse (sum) fitted_prob, by(year season species draw length5_bin)
levelsof species, local(splist)
levelsof season,  local(seaslist)

foreach sp of local splist {
    foreach se of local seaslist {

        graph box fitted_prob if species=="`sp'" & season=="`se'", ///
            over(length5_bin, sort(length5_lo)) ///
            by(year, col(1) ///
                title("Catch-at-length probabilities: `sp', `se'") ///
                note("Boxplots of replicate probabilities by 5-cm length bin")) ///
            ytitle("Probability") ///
            name(box_`sp'_`se', replace)
		
        *graph export "$figure_cd/CAL_`sp'_`se'_byyear.png", replace
    }
}
restore 


drop length5_lo length5_hi length5_bin 
drop cal*
keep if year==2026
egen replicate=rowtotal(hadd_replicate - cod_replicate)
drop hadd_replicate cod_replicate
drop year 
drop nal
export delimited using "$input_data_cd/projected_catch_at_length.csv", replace 

