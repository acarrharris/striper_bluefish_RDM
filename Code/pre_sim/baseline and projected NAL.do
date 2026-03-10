
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

append using  `naa_cod'
replace NaL_from_raw_trawl=NaL_from_raw_trawl*1000
replace NaL_from_smooth_trawl=NaL_from_smooth_trawl*1000

gen year=2025
tempfile nal2025
save `nal2025'



*projected - 2026
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


append using  `proj_naa_cod' 

replace NaL_from_raw_trawl=NaL_from_raw_trawl*1000
replace NaL_from_smooth_trawl=NaL_from_smooth_trawl*1000
gen year =2026

append using `nal2025'

gen length5_lo = floor(length/5)*5
gen length5_hi = length5_lo + 4

* String label: "20-24", "25-29", etc.
gen str20 length5_bin = string(length5_lo) + "-" + string(length5_hi)
label var length5_bin "Length bin (cm)"
replace length5_bin="05-9" if length5_bin=="5-9"

collapse (sum)  NaL_from_smooth_trawl, by(year  species draw length5_bin)
egen sum_nal=sum(NaL_from_smooth_trawl), by(year species draw)
gen prop_nal=NaL_from_smooth_trawl/sum
drop NaL_from_smooth_trawl sum
reshape wide  prop_nal, i(species  draw length ) j(year)

graph box prop_nal2025 prop_nal2026 if species =="hadd" , ///
    over(length, label(labsize(small))) ///
    title("haddock proportions number at length", size(medium)) ///
	ytitle(Proportion of fish that are length-{it:l}) ///
    ylab(, labsize(small)) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
    legend(order(1 "2025" 2 "2026") position(6) cols(2)) 

graph box prop_nal2025 prop_nal2026 if species =="cod" , ///
    over(length, label(labsize(small))) ///
    title("cod proportions number at length", size(medium)) ///
	ytitle(Proportion of fish that are length-{it:l}) ///
    ylab(, labsize(small)) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
    legend(order(1 "2025" 2 "2026") position(6) cols(2)) 
