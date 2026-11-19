** =================================================================================
** catch_at_length_projection_striper_bluefish.do
** Steps to produce projected catch at length probability distribution
** Optimized Version: Uses analytical probability densities for Striped Bass NAL
** =================================================================================

set seed $seed

* ==============================================================================
* 1. Pull in baseline catch-at-lengths
* ==============================================================================
import delimited using "$input_data_cd/baseline_catch_at_length_data_rgns.csv", clear 
replace region = waters if region == ""
drop waters
keep if draw <= $ndraws
sort draw region species length

tempfile cal
save `cal', replace 


* ==============================================================================
* 2. Striped Bass: Baseline NAA -> Baseline NAL (Analytical Method)
* ==============================================================================
import delimited using "$length_data_cd/SB_NAA_2023.csv", clear
gen age = _n
rename n n_naa
tempfile sb_naa_base
save `sb_naa_base', replace

import delimited using "$length_data_cd/SB_avg_LAA.csv", clear 
rename plusage age   

drop n
merge 1:1 age using `sb_naa_base', nogen

* Convert inches to cm
replace tlmean=tlmean*2.54
replace tlvar=tlvar*(2.54^2)
gen sd=sqrt(tlvar)

* Analytical Probability approach: create a length grid (1 to 200 cm) for each age class
gen id = _n
expand 200
bysort id: gen length = _n

* Calculate probability mass in each 1-cm length bin using the Normal CDF
gen prob = normal((length + 0.5 - tlmean)/sd) - normal((length - 0.5 - tlmean)/sd)

* Normalize probability to ensure exactly 100% of the mass is captured within bounds
egen prob_sum = total(prob), by(id)
replace prob = prob / prob_sum

* Calculate NAL and collapse
gen nal = n_naa * prob
collapse (sum) nal, by(length)

gen species = "str"
tempfile sb_nal_base
save `sb_nal_base', replace


* ==============================================================================
* 3. Bluefish: Baseline NAA -> Baseline NAL 
* ==============================================================================
import delimited using "$length_data_cd/hybridALKs_thru2024_final.csv", clear 
keep if year == 2023
collapse (mean) a*, by(year flcm)

local vars a0 a1 a2 a3 a4 a5 a6
qui foreach v of local vars {
    replace `v' = `v' * 1000
    egen sum`v' = sum(`v')
    gen prop_`v' = `v' / sum`v'
}

keep prop* fl 
reshape long prop_a, i(flcm) j(age) string
destring age, replace
sort age flcm   
rename flcm length

tempfile bluefish_alk
save `bluefish_alk', replace

clear
input year age naa
2023 0 103519.7	
2023 1 48990.28
2023 2 34071.74
2023 3 6808.463
2023 4 3285.435
2023 5 3487.021
2023 6 3292.094
end

replace naa = naa * 1000
drop year 

merge 1:m age using `bluefish_alk', nogen
gen nal = prop * naa
collapse (sum) nal, by(length)

gen species = "blu"
append using `sb_nal_base'

tempfile base_naa
save `base_naa', replace 


* ==============================================================================
* 4. Selectivity: Merge baseline NAL to baseline catch-at-length
* ==============================================================================
merge 1:m species length using `cal', keep(2 3) nogen
drop if draw == .

mvencode catch nal, mv(0) override
sort species region draw length
order species region draw length nal catch observed fitted

gen frac_caught = catch / nal if nal > 0
drop if catch == 0
mvencode frac_caught*, mv(0) override

* Reassign catch lengths where baseline population == 0
egen min_length_pop = min(length) if nal != 0, by(species region draw)
egen max_length_pop = max(length) if nal != 0, by(species region draw)
egen min_length_catch = min(length) if catch != 0, by(species region draw)
egen max_length_catch = max(length) if catch != 0, by(species region draw)

foreach v in min_length_pop max_length_pop min_length_catch max_length_catch {
    egen mean_`v' = mean(`v'), by(species region draw)
    replace `v' = mean_`v'
    drop mean_`v'
}

replace length = max_length_pop if catch > 0 & nal == 0 & length > max_length_pop
replace length = min_length_pop if catch > 0 & nal == 0 & length < min_length_pop

collapse (sum) catch nal*, by(species region draw length)
drop if catch == 0

gen frac_caught = catch / nal if nal > 0
mvencode frac_caught*, mv(0) override
rename nal nal_base

tempfile selectivity
save `selectivity', replace


* ==============================================================================
* 5. Striped Bass: Projected NAA -> Projected NAL (Analytical Method)
* ==============================================================================
import delimited using "$length_data_cd/SB_NAA_2024_2027.csv", clear
keep if year == 2024
sample $ndraws, count
gen draw = _n
reshape long v, i(year sim draw) j(age) string
destring age, replace 
rename v n_naa
drop year
sort draw age 

*valiudation check -> increase age 11-15 by five times 
*replace n_naa=n_naa*5 if age>=11

tempfile sb_naa_proj
save `sb_naa_proj', replace

import delimited using "$length_data_cd/SB_avg_LAA.csv", clear
rename plusage age 
drop n
merge 1:m age using `sb_naa_proj', nogen

replace tlmean=tlmean*2.54
replace tlvar=tlvar*(2.54^2)
gen sd=sqrt(tlvar)

* Analytical expansion
gen id = _n
expand 200
bysort id: gen length = _n

gen prob = normal((length + 0.5 - tlmean)/sd) - normal((length - 0.5 - tlmean)/sd)
egen prob_sum = total(prob), by(id)
replace prob = prob / prob_sum

gen nal = n_naa * prob
collapse (sum) nal, by(draw length)
gen species = "str"

* Allocate to regions
expand 10
bysort draw length: gen tab = _n
gen region = "CHES" if tab == 1
replace region = "CT" if tab == 2
replace region = "DE" if tab == 3
replace region = "MA" if tab == 4
replace region = "MD" if tab == 5
replace region = "ME" if tab == 6
replace region = "NH" if tab == 7
replace region = "NJ" if tab == 8
replace region = "NY" if tab == 9
replace region = "RI" if tab == 10
drop tab

tempfile sb_nal_proj
save `sb_nal_proj', replace


* ==============================================================================
* 6. Bluefish: Projected NAA -> Projected NAL 
* ==============================================================================
clear
input year age naa
2024 0 125408.3	
2024 1 41208.79
2024 2 40061.17
2024 3 15444.54
2024 4 3821.65	
2024 5 1959.825
2024 6 4722.792
end

replace naa = naa * 1000
drop year 
merge 1:m age using `bluefish_alk', nogen

gen nal = prop * naa
collapse (sum) nal, by(length)

gen species = "blu"

expand $ndraws
bysort length: gen draw = _n

* Allocate to regions
expand 2
bysort draw length: gen tab = _n
gen region = "N" if tab == 1
replace region = "S" if tab == 2
drop tab

append using `sb_nal_proj'


* ==============================================================================
* 7. Projected Catch-at-Length
* ==============================================================================
merge 1:1 region species length draw using `selectivity', keep(2 3) nogen
sort species region draw length

* Reassign projection bounds to match population bounds
egen min_length_pop = min(length) if nal != ., by(species region draw)
egen max_length_pop = max(length) if nal != ., by(species region draw)

foreach v in min_length_pop max_length_pop {
    egen mean_`v' = mean(`v'), by(species region draw)
    replace `v' = mean_`v'
    drop mean_`v'
}

replace length = max_length_pop if catch > 0 & nal == . & length > max_length_pop
replace length = min_length_pop if catch > 0 & nal == . & length < min_length_pop

collapse (sum) catch nal*, by(species region draw length)
gen frac_caught = catch / nal_base
gen catch_proj = frac_caught * nal
mvencode catch*, mv(0) override

tostring draw, gen(draw2)
gen domain = species + "_" + region + "_" + draw2
drop draw2

egen sum_base = sum(catch_at_length), by(domain)
gen observed_prob_base = catch_at_length / sum_base

egen sum_proj = sum(catch_proj), by(domain)
gen observed_prob_proj = catch_proj / sum_proj

drop sum_*
rename catch_at catch_base
rename nal nal_proj

* Save lookup base for later merge
preserve 
rename length fitted_length
keep fitted_length observed_prob* species region domain draw catch* nal*
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore

/*
* uncomment if you want plots of the resulting distributions, evaluated at mean by length
* plots of base and projected catch-at-length
gen length_in=round(length/2.54)
collapse (mean) nal* catch*, by(species region length_in)
keep if (species=="str" & region=="CHES") | (species=="blu" & region=="N")

egen sumfish_base=sum(nal_base), by(species)
egen sumfish_proj=sum(nal_proj), by(species)
gen prop_nal_base=nal_base/sumfish_base
gen prop_nal_proj=nal_proj/sumfish_proj

levelsof species , local(domz)
foreach d of local domz{

twoway (scatter prop_nal_base length if species=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter prop_nal_proj length if  species=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (inches)", yoffset(-2)) ytitle("Proportion")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "2023 proportion numbers-at-length") lab(2 "2024 proportion numbers-at-length") position (6) cols(2) yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(1)
*/
* ==============================================================================
* 8. Gamma Smoothing (MOM Approach)
* ==============================================================================
tempfile new
save `new', replace

global fitted_sizes
levelsof domain, local(regs)

qui foreach r of local regs {
    use `new', clear
    keep if domain == "`r'"
    
    keep length catch_proj
    drop if missing(length) | missing(catch_proj) | catch_proj <= 0 | length <= 0
    replace catch_proj = round(catch_proj)
    
    su catch_proj, meanonly
    local tot_n_fish = `r(sum)'

    * (A) Estimate gamma parameters robustly (MOM with freq weights)
    quietly summarize length [fw=catch_proj], meanonly
    local mu = r(mean)
    
    gen double length2 = length^2
    quietly summarize length2 [fw=catch_proj], meanonly
    local ex2 = r(mean)
    local v   = `ex2' - (`mu'^2)

    * Guard: if variance is 0 or numerically tiny
    if (`v' <= 1e-10 | missing(`v') | missing(`mu') | `mu' <= 0) {
        local alpha = 1e6
        local beta  = `mu' / `alpha'
    }
    else {
        local alpha = (`mu'^2) / `v'
        local beta  = `v' / `mu'
    }

    * Simulate via rejection sampling
    local ndraw = `tot_n_fish'
    clear
    set obs `ndraw'
    gen double gammafit = rgamma(`alpha', `beta')
    replace gammafit = round(gammafit)

    * Fallback 1: Increase draws
    if _N == 0 {
        clear
        set obs `=5*`ndraw''
        gen double gammafit = rgamma(`alpha', `beta')
        replace gammafit = round(gammafit)
    }
	
    * Fallback 2: Deterministic mass at the mean to avoid dropping domain entirely
    if _N == 0 {
        clear
        set obs `ndraw'
        gen double gammafit = round(`mu')
    }

    gen nfish = 1
    collapse (sum) nfish, by(gammafit)
    egen sumnfish = total(nfish)
    gen double fitted_prob = nfish / sumnfish
    gen domain = "`r'"

    tempfile fitted_sizes_`=_N'   
    save `fitted_sizes_`=_N'', replace
    global fitted_sizes "$fitted_sizes `fitted_sizes_`=_N''"
}

clear
dsconcat $fitted_sizes
rename gammafit fitted_length

merge 1:1 fitted_length domain using `observed_prob', nogen
sort domain fitted_length 
mvencode fitted_prob observed_prob*, mv(0) override 

split domain, parse(_)
replace region = domain2
replace species = domain1
rename domain3 draw_str
drop draw
destring draw_str, gen(draw)
rename fitted_length length

drop nfish sum* domain1 domain2 draw_str
rename fitted_prob fitted_prob_proj


* ==============================================================================
* 9. Cleanup, Base Merge, and Export
* ==============================================================================
preserve
import delimited using "$input_data_cd/baseline_catch_at_length_data_rgns.csv", clear  
replace region = waters if region == ""
keep if draw <= $ndraws
tempfile baseyr
save `baseyr', replace 
restore

merge 1:1 species region draw length using `baseyr', nogen
mvencode fitted* observed* nal* catch*, mv(0) override

rename fitted_prob fitted_prob_base
order species region waters domain draw length fitted* observed* nal* catch* 
sort species region draw length

* Truncate the fitted distribution to the observed range
levelsof domain, local(doms)
foreach d of local doms {
    quietly summarize length if observed_prob_base != 0 & !missing(observed_prob_base) & domain == "`d'"
    local minL = r(min)
    local maxL = r(max)
    drop if (length < `minL' | length > `maxL') & domain == "`d'"
}

egen sum_fitted_prob = sum(fitted_prob_proj), by(domain)
replace fitted_prob_proj = fitted_prob_proj / sum_fitted_prob



* uncomment if you want plots of the resulting distributions, evaluated at mean by length
* plots of base and projected catch-at-length
/*
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


levelsof domain if species=="str", local(domz)
foreach d of local domz{

twoway (scatter fitted_prob_base length if domain=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter fitted_prob_proj length if  domain=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("") ytitle("Prob") xlab(#35, labsize(small))    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "fitted_catch_at_length_prob_base") lab(2 "fitted_catch_at_length_prob_proj") cols(2) yoffset(-2) size(small) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
}

grc1leg `graphnames', rows(3) xcommon

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

keep draw length species waters region fitted_prob_proj 
drop if missing(fitted_prob_proj) | fitted_prob_proj == 0
rename fitted_prob_proj fitted_prob
sort species region draw length

* expand to management regions
* bluefish

expand 13 if region == "S"	& species=="blu"
bysort draw species length region: gen tab=_n

gen region1="MDCHES" if region=="S" & species=="blu" &  tab==1 
replace region1="VACHES" if region=="S" & species=="blu" & tab==2
replace region1="DEDERIV" if region=="S" & species=="blu" & tab==3
replace region1="DENANT" if region=="S" & species=="blu" & tab==4
replace region1="DEOCN" if region=="S" & species=="blu" & tab==5
replace region1="MDOCN" if region=="S" & species=="blu" & tab==6
replace region1="MDPOT" if region=="S" & species=="blu" & tab==7
replace region1="NCALB" if region=="S" & species=="blu" & tab==8
replace region1="NCCNTRL" if region=="S" & species=="blu" & tab==9
replace region1="NCOCN" if region=="S" & species=="blu" & tab==10
replace region1="VAOCN" if region=="S" & species=="blu" & tab==11
replace region1="VAPOTO" if region=="S" & species=="blu" & tab==12
replace region1="NJALL" if region=="S" & species=="blu" & tab==13


drop tab
expand 7 if region == "N" & species=="blu"
bysort draw species length region: gen tab=_n
replace region1="CTALL" if region=="N" & species=="blu" & tab==1
replace region1="MAALL" if region=="N" & species=="blu" & tab==2
replace region1="MEALL" if region=="N" & species=="blu" & tab==3
replace region1="NHALL" if region=="N" & species=="blu" & tab==4
replace region1="RIALL" if region=="N" & species=="blu" & tab==5
replace region1="NYHUDN" if region=="N" & species=="blu" & tab==6
replace region1="NYHUDS" if region=="N" & species=="blu" & tab==7
drop tab 


sort species region1 draw fitted

*striped bass
*new 
* expand to management regions
expand 2 if region == "CHES"  & species=="str"		
bysort draw species length region: gen tab=_n
replace region1="MDCHES" if region=="CHES" & tab==1  & species=="str"	
replace region1="VACHES" if region=="CHES" & tab==2  & species=="str"	

drop tab
replace region1="CTALL" if region=="CT"  & species=="str"	
replace region1="MAALL" if region=="MA"  & species=="str"	
replace region1="MEALL" if region=="ME"  & species=="str"	
replace region1="NHALL" if region=="NH"  & species=="str"	
replace region1="RIALL" if region=="RI"  & species=="str"	
replace region1="NJALL" if region=="NJ"  & species=="str"	


expand 2 if region == "NY" & species=="str"		 
bysort draw species length region: gen tab=_n
replace region1="NYHUDN" if region=="NY" & tab==1  & species=="str"	
replace region1="NYHUDS" if region=="NY" & tab==2  & species=="str"	
drop tab

expand 3 if region == "DE"  & species=="str"		 
bysort draw species length region: gen tab=_n 
replace region1="DEDERIV" if region=="DE" & tab==1  & species=="str"	
replace region1="DENANT" if region=="DE" & tab==2  & species=="str"	
replace region1="DEOCN" if region=="DE" & tab==3  & species=="str"	
drop tab

expand 7 if region == "MD"  & species=="str"		 
bysort draw species length region: gen tab=_n
replace region1="MDOCN" if region=="MD" & tab==1  & species=="str"	
replace region1="MDPOT" if region=="MD" & tab==2  & species=="str"	
replace region1="NCALB" if region=="MD" & tab==3  & species=="str"	
replace region1="NCCNTRL" if region=="MD" & tab==4  & species=="str"	
replace region1="NCOCN" if region=="MD" & tab==5  & species=="str"	
replace region1="VAOCN" if region=="MD" & tab==6  & species=="str"	
replace region1="VAPOTO" if region=="MD" & tab==7  & species=="str"	
drop tab 

drop region 
rename region1 region

compress
sort species region draw length
*end new
drop waters
order species region draw length

export delimited using "$input_data_cd/projected_catch_at_length_multi_region.csv", replace


import delimited using "$input_data_cd/projected_catch_at_length_multi_region.csv", clear
/*
gen length_in=round(length/2.54)
collapse (mean) fitted*, by(species region length_in)
keep if (species=="str" & region=="CHES") | (species=="blu" & region=="N")

egen sumfish_base=sum(nal_base), by(species)
egen sumfish_proj=sum(nal_proj), by(species)
gen prop_nal_base=nal_base/sumfish_base
gen prop_nal_proj=nal_proj/sumfish_proj

levelsof species , local(domz)
foreach d of local domz{

twoway (scatter prop_nal_base length if species=="`d'" ,   connect(direct) cmissing(no)  lcol(gray) lwidth(med)  lpat(solid) msymbol(o) mcol(gray) $graphoptions) ///
		    (scatter prop_nal_proj length if  species=="`d'"  , connect(direct) cmissing(no)  lcol(black)   lwidth(med)  lpat(solid) msymbol(i)   ///
			xtitle("Length (inches)", yoffset(-2)) ytitle("Proportion")    ylab(, angle(horizontal) labsize(vsmall)) ///
			legend(lab(1 "2023 proportion numbers-at-length") lab(2 "2024 proportion numbers-at-length") position (6) cols(2) yoffset(-2) region(color(none)))   title("`d'", size(small))  name(dom`d', replace))
 local graphnames `graphnames' dom`d'
 
 */