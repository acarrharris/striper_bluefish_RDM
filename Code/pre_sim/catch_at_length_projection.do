
** Steps to produce projected catch at length probabilty distribution
*1. Read baseline recreational catch-at-length 
*2. Build cod and haddock age-length keys (ALKs) from recent NEFSC trawl data.
*3. Convert baseline stock assessment numbers-at-age (NAA) into baseline numbers-at-length (NAL).
*4. Merge baseline NAL to observed baseline catch-at-length and compute an empirical recreational selectivity at length by species, season, draw, and length.
*5. Convert projected NAA into projected NAL using the same ALKs.
*6. Apply baseline selectivity-at-length to projected NAL to obtain projected catch-at-length.
*7. Convert projected catch-at-length into a probability distribution over lengths, using a gamma-smoothed fitted distribution.
*8. Export projected fitted probabilities by draw/species/season/length.

 global ndraws 5
* 1. Pull in baseline catch-at-lengths

set seed $seed
import delimited using "$input_data_cd/baseline_catch_at_length_data_rgns.csv", clear 
replace region=waters if region==""
drop waters
keep if draw<= $ndraws
sort draw region species length

tempfile cal
save `cal', replace 


*2. Pull in historical NAA data, convert to lengths
* striped bass
set seed 02311990

import delimited using "$length_data_cd/SB_NAA_2023.csv", clear
gen age = _n
rename n n_naa
tempfile sb_naa_base
save `sb_naa_base', replace

su n
return list 


import delimited using "$length_data_cd/SB_avg_LAA.csv", clear
rename plusage age   // or rename plusage age, depending on Stata import

drop n
merge 1:1 age using `sb_naa_base', nogen

gen n_expand = round(n_naa/1000)
levelsof age, local(ages)

tempfile base catchall
save `base', replace

clear
save `catchall', emptyok replace


qui foreach a of local ages {
    use `base', clear
    keep if age == `a'
	replace tlmean=tlmean*2.54
	replace tlvar=tlvar*2.54

    local mean = tlmean[1]
    local sd   = sqrt(tlvar[1])
    local n    = n_expand[1]

    expand `n'
    gen length_in = round(rnormal(`mean', `sd'))

    drop if length_in < 1

    append using `catchall'
    save `catchall', replace
}

use `catchall', clear
gen nal = 1000
rename length_in length
collapse (sum) nal, by(length)
gen species="str"
su nal
return list

tempfile sb_nal_base
save `sb_nal_base', replace


* Bluefish 
* create ALK from trawl survey data
import delimited using "$length_data_cd/hybridALKs_thru2024_final.csv", clear 
keep if year==2023
collapse (mean) a*, by(year flcm)
local vars a0 a1 a2 a3 a4 a5 a6
qui foreach v of local vars{
	replace `v'=`v'*1000
	egen sum`v'=sum(`v')
	gen prop_`v'=`v'/sum`v'
}
keep prop* fl 
reshape long prop_a, i(flcm) j(age) string
destring age, replace
sort age flcm   
rename flcm length

tempfile bluefish_alk
save `bluefish_alk', replace


* bluefish
clear
input year  age naa
2023    0  103519.7	
2023    1  48990.28
2023    2  34071.74
2023    3  6808.463
2023    4 3285.435
2023    5 3487.021
2023    6  3292.094
end

replace naa=naa*1000
su naa
return list
drop year 
merge 1:m age using  `bluefish_alk'

sort   length age

gen nal=prop*naa
collapse (sum) nal, by(length)
su nal
return list
gen species="blu"

append using  `sb_nal_base'

tempfile base_naa
save `base_naa', replace 


* 4. Merge baseline NAL to observed baseline catch-at-length and selectivity at length by species, season, draw, and length.
merge 1:m species length using `cal', keep(2 3)
drop if draw==.

mvencode catch nal , mv(0) override
sort species region  draw length
order species region  draw length nal catch observed fitted

gen frac_caught= catch / nal if nal > 0

sort species region  draw length
drop if catch==0
mvencode frac_caught*, mv(0) override

* catch_l > population_l adjustment 
* This block reassigns catch lengths where base_nal_smooth == 0 and the catch falls outside the population length support. 
* It does not  address cases where `catch > base_nal_smooth` at lengths where population is nonzero but small. This will produce `frac_caught_smooth > 1`.
* This is acceptable because "fraction caught" is only a scaling factor

egen min_length_pop=min(length) if nal!=0, by(species region draw)
egen max_length_pop=max(length) if nal!=0, by(species region draw)

egen min_length_catch=min(length) if catch!=0, by(species region draw)
egen max_length_catch=max(length) if catch!=0, by(species region draw)

local vars min_length_pop max_length_pop min_length_catch max_length_pop max_length_catch
foreach v of local vars{
	egen mean_`v'=mean(`v'), by(species region draw)
	replace `v'= mean_`v'
	drop mean_`v'
	
}

replace length=max_length_pop if catch>0 & nal==0 & length>max_length_pop
replace length=min_length_pop if catch>0 & nal==0 & length<min_length_pop

collapse (sum) catch nal*,  by(species region  draw length )
drop if catch==0

gen frac_caught = catch / nal if nal > 0

sort species region draw length

mvencode frac_caught*, mv(0) override
rename nal nal_base

tempfile selectivity
save `selectivity', replace


*5. Convert projected NAA into projected NAL
* striped bass
import delimited using "$length_data_cd/SB_NAA_2024_2027.csv", clear
keep if year==2024
sample $ndraws, count
gen draw=_n
reshape long v, i(year sim draw) j(age) string
destring age, replace 
rename v naa
drop year
sort draw age 

tempfile sb_naa_proj
save `sb_naa_proj', replace

import delimited using "$length_data_cd/SB_avg_LAA.csv", clear
rename plusage age   // or rename plusage age, depending on Stata import

drop n
merge 1:m age using `sb_naa_proj', nogen
gen n_expand = round(naa/1000)

tostring age, gen(age2)
tostring draw, gen(draw2)
gen domain=draw2+"_"+age2

levelsof domain, local(domainz)

tempfile proj catchall
save `proj', replace

clear
save `catchall', emptyok replace

qui foreach a of local domainz {
    use `proj', clear
    keep if domain == "`a'"
	replace tlmean=tlmean*2.54
	replace tlvar=tlvar*2.54

    local mean = tlmean[1]
    local sd   = sqrt(tlvar[1])
    local n    = n_expand[1]

    expand `n'
    gen length_in = round(rnormal(`mean', `sd'))

    drop if length_in < 1

    append using `catchall'
    save `catchall', replace
}

use `catchall', clear
gen nal = 1000
rename length_in length
collapse (sum) nal, by(length draw)

gen species="str"
su nal
return list

expand 3
bysort draw length:gen tab=_n
gen region="N" if tab==1
replace region="S" if tab==2
replace region="CHES" if tab==3
drop tab


tempfile sb_nal_proj
save `sb_nal_proj', replace

/*
### Enter seasonal L-W parameters
beta.s <- 2.983707
beta.f <- 3.019130
alpha.s <- 0.00001288211
alpha.f <- 0.00001233176
*/


* bluefish
clear
input year  age naa
2024    0  125408.3	
2024    1  41208.79
2024    2  40061.17
2024    3  15444.54
2024    4 3821.65	
2024    5 1959.825
2024    6  4722.792
end

replace naa=naa*1000
su naa
return list
drop year 
merge 1:m age using  `bluefish_alk'

sort   length age

gen nal=prop*naa
collapse (sum) nal, by(length)
su nal
return list
gen species="blu"
expand 5
bysort length: gen draw=_n

expand 2
bysort draw length:gen tab=_n
gen region="N" if tab==1
replace region="S" if tab==2
drop tab

append using  `sb_nal_proj'

* 6. Apply baseline empirical fraction-caught-at-length to projected population NAL.
		* This assumes that the length-specific recreational catchability/selectivity observed
		* in the baseline year remains constant in the projection year, while projected stock
		* composition changes according to projected NAA translated to NAL using the ALK.

merge 1:1 region species length draw using `selectivity'
keep if inlist(_merge, 2, 3)
sort species region draw length

egen min_length_pop=min(length) if nal!=., by(species region draw)
egen max_length_pop=max(length) if nal!=., by(species region draw)

egen min_length_catch=min(length) if !inlist(catch, ., 0), by(species region draw)
egen max_length_catch=max(length) if  !inlist(catch,., 0), by(species region draw)

local vars min_length_pop max_length_pop min_length_catch max_length_pop max_length_catch
foreach v of local vars{
	egen mean_`v'=mean(`v'), by(species region draw)
	replace `v'= mean_`v'
	drop mean_`v'
	
}

replace length=max_length_pop if catch>0 & nal==. & length>max_length_pop
replace length=min_length_pop if catch>0 & nal==. & length<min_length_pop

collapse (sum) catch nal*,  by(species region  draw length )
gen frac_caught=catch/nal_base

gen catch_proj= frac_caught*nal
mvencode catch*, mv(0)
sort species region draw length

tostring draw, gen(draw2)
gen domain=species+"_"+region+"_"+draw2

egen sum=sum(catch_at_length), by(species region draw domain)
gen observed_prob_base=catch_at_length/sum
egen sum_proj=sum(catch_proj), by(species region draw domain)
gen observed_prob_proj=catch_proj/sum_proj
format sum* %20.0gc
drop sum*
rename catch_at catch_base
rename nal nal_proj

preserve 
rename length fitted_length
keep fitted_length observed_prob*  species region domain draw catch* nal*
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


*7. Convert projected catch-at-length into a probability distribution over lengths, using gamma-smoothed fitted distribution.

* MOM approach to avoid non-convergence 
tempfile new
save `new', replace

global fitted_sizes

levelsof domain, local(regs)

qui foreach r of local regs {
    use `new', clear
    keep if domain=="`r'"
    di "`r'"

    keep length catch_proj
    drop if missing(length) | missing(catch_proj)
    drop if catch_proj<=0
	replace catch_proj=round(catch_proj)
	su catch_proj
	local tot_n_fish=`r(sum)'
	

    * Gamma needs strictly positive support
    drop if length<=0

	* --------
    * (A) Estimate gamma parameters robustly (MOM with freq weights)
    * --------
    quietly summarize length [fw=catch_proj], meanonly
    local mu = r(mean)
    local Nw = r(sum_w)
	
	
    * Weighted variance: Var = E[x^2] - (E[x])^2 using the same freq weights
    gen double length2 = length^2
    quietly summarize length2 [fw=catch_proj], meanonly
    local ex2 = r(mean)
    local v   = `ex2' - (`mu'^2)

    * Guard: if variance is 0 or numerically tiny, make it a near-degenerate gamma
    if (`v'<=1e-10 | missing(`v') | missing(`mu') | `mu'<=0) {
        * Put essentially all mass at mu by using huge alpha
        local alpha = 1e6
        local beta  = `mu'/`alpha'
    }
    else {
        local alpha = (`mu'^2)/`v'
        local beta  = `v'/`mu'
    }

    *Simulate a truncated gamma sample via rejection sampling
    local ndraw = `tot_n_fish'   // sample size for the simulated distribution
    clear
    set obs `ndraw'

    * draw
    gen double gammafit = rgamma(`alpha', `beta')
    replace gammafit = round(gammafit)


    * If rejection killed everything, try again with more draws (once)
    if _N==0 {
        clear
        set obs `=5*`ndraw''
        gen double gammafit = rgamma(`alpha', `beta')
        replace gammafit = round(gammafit)
        if _N==0 continue
    }

    gen nfish = 1
    collapse (sum) nfish, by(gammafit)
    egen sumnfish = total(nfish)
    gen double fitted_prob = nfish/sumnfish
    gen domain = "`r'"

    tempfile fitted_sizes_`=_N'   
    save `fitted_sizes_`=_N'', replace
    global fitted_sizes "$fitted_sizes `fitted_sizes_`=_N''"
}

clear
dsconcat $fitted_sizes
rename gammafit fitted_length

merge 1:1 fitted_length domain using `observed_prob'
sort domain fitted_length 
mvencode fitted_prob observed_prob*, mv(0) override 

split domain, parse(_)
replace region=domain2
replace species=domain1
drop draw
replace domain=species+"_"+region+"_"+domain3
rename domain3 draw
destring draw, replace 
rename fitted_l length

drop _merge nfish sum
order species region domain draw length
drop domain1 domain2 
rename fitted_prob fitted_prob_proj

preserve
import delimited using "$input_data_cd/baseline_catch_at_length_data_rgns.csv", clear  
replace region=waters if region==""
ds
keep if draw<= $ndraws
tempfile baseyr
save `baseyr', replace 
restore

merge 1:1  species region draw length using `baseyr'
sort species region draw length
drop observed_prob

keep species region domain draw length fitted* observed* nal* catch* waters
mvencode fitted* observed* nal* catch*, mv(0) override
rename fitted_prob fitted_prob_base
order species region waters domain draw length fitted* observed* nal* catch* 
*merge m:1 length species season using  `base_naa'

sort draw species season length
local vars nal_base proj_nal proj_nal
foreach v of local vars{
	egen sum_`v'=sum(`v'), by(draw species season)
	gen prop_`v'=`v'/sum_`v'
	drop sum_`v'
}


* truncate the fitted distribution to the observed range
levelsof domain, local(doms)
foreach d of local doms{
quietly summarize length if observed_prob_base!=0 & !missing(observed_prob_base) & domain=="`d'"
local minL = `r(min)'
local maxL = `r(max)'
drop if (length<`minL' | length>`maxL') & domain=="`d'"
}

egen sum_fitted_prob=sum(fitted_prob_proj), by(domain)
replace fitted_prob_proj=fitted_prob_proj/sum_fitted_prob

/*
* uncomment if you want plots of the resulting distributions, evaluated at mean by length
* plots of base and projected catch-at-length

collapse (mean) observed* fitted* nal* catch*, by(species region length)
gen domain=region+"_"+species

levelsof domain , local(domz)
foreach d of local domz{
	twoway (scatter observed_prob_base length if domain=="`d'" ,   cmissing(no) connect(direct) lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter observed_prob_proj length if  domain=="`d'"  , cmissing(no) connect(direct) lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "observed_catch_at_length_prob_base") lab(2 "observed_catch_at_length_prob_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)


levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter fitted_prob_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "fitted_catch_at_length_prob_base") lab(2 "fitted_catch_at_length_prob_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter observed_prob_proj length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "observed_catch_at_length_prob_proj") lab(2 "fitted_catch_at_length_prob_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter observed_prob_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_base length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "observed_catch_at_length_prob_base") lab(2 "fitted_catch_at_length_prob_base") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)



levelsof domain , local(domz)
foreach d of local domz{

twoway (scatter nal_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter nal_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (cm)", yoffset(-2)) ytitle("Prob")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "nal_base") lab(2 "nal_proj") cols() yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(2)

*/

* 8. Export projected fitted probabilities
keep draw length species season  fitted_prob_proj 
drop if missing(fitted_prob_proj) | fitted_prob_proj == 0
rename fitted_prob_proj fitted_prob
compress
export delimited using "$misc_data_cd/projected_catch_at_length.csv", replace 

