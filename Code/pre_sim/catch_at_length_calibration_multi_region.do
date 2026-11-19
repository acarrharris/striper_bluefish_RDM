


set seed $seed
	
	
	
**************************
****** Bluefish ******
**************************

* Discard lengths
* Tony has already compiled bluefish discard length data used in the assessment. 

import delimited using "$length_data_cd/BLF_RECDISCLEN_2022_2024.csv", clear 

drop if state=="FL"

keep if year<2024
*replace length = length/2.54
*replace length = round(length)
drop region
drop if state=="FL"
gen region="N" if inlist(state, "ME", "NH", "MA", "RI", "CT" ,"NY")
replace region="S" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

collapse (sum) number, by(region length)

/*
su number if region=="N"
return list
local n_obs_rel_N=`r(sum)'

su number if region=="S"
return list
local n_obs_rel_S=`r(sum)'
*/

egen sum=sum(number), by(region)
gen prop_b2=number/sum
gen species="blu"
rename length l_cm_bin
drop number sum

tempfile blue_b2
save `blue_b2', replace 

**************************
* MRIP harvest lengths - bluefish
**************************

cd $input_data_cd

clear

mata: mata clear

tempfile tl1 sl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
*replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
drop if strmatch(id_code, "*xx*")==1
drop if strat_id==""
duplicates drop 
save `tl1'
clear
 
dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

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


keep if $calibration_year //ensure relevent year
 
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


* classify catch into the things I care about (common=="s" | "b") and things I don't care about "z" 
gen common_dom="z"
replace common_dom="s" if strmatch(sp_code,"8835020102")
replace common_dom="b" if strmatch(sp_code,"8835250101")

tostring wave, gen(w2)
tostring year, gen(year2)

gen season = "spr" if   inlist(wave, 1, 2, 3)
replace season = "fall" if  inlist(wave, 4, 5, 6)

destring month, gen(mymo)
drop month
tostring mymo, gen(month)
drop mymo

* this might speed things up if I re-classify all length=0 for the species I don't care about 
*replace l_cm_bin =0 if !inlist(common_dom, "s", "b")
replace l_in_bin =0 if !inlist(common_dom, "s", "b")

sort year w2 strat_id psu_id id_code

drop if common_dom=="z"

destring month, replace
gen month1 = string(month,"%02.0f")
drop month 
rename month month 

drop region
gen region="N" if inlist(state, "ME", "NH", "MA", "RI", "CT","NY")
replace region="S" if inlist(state,  "NJ", "DE", "MD", "VA", "NC")

gen my_dom_id_string=region+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

gen nfish_ab1=1
collapse (sum) nfish_ab1, by(common_dom l_cm_bin region)

gen species="str" if common=="s"
replace species="blu" if common=="b"

sort species l_cm_bin
keep if species=="blu"
egen sum=sum(nfish_ab1), by(region)
gen prop_ab1=nfish_ab1/sum
drop sum nfish_ab1


* merge harvest lengths to discards lengths
merge 1:1 l_cm_bin species region  using `blue_b2'

mvencode prop*, mv(0) override
drop _merge
sort region  l

expand $ndraws
bysort  l region: gen draw=_n

preserve
u "$iterative_input_data_cd\simulated_catch_totals_for_length_distn.dta", clear 
gen region="N" if inlist(state, "ME", "NH", "MA", "RI", "CT","NY")
replace region="S" if inlist(state,  "NJ", "DE", "MD", "VA", "NC")
collapse (sum) tot_blu_keep_sim tot_blu_rel_sim, by(region draw)
keep if draw<=$ndraws

tempfile regional_total_blue
save `regional_total_blue', replace
restore 

merge m:1 region draw using `regional_total_blue'
keep if draw<=$ndraws

drop _merge

gen ab1=prop_ab1*tot_blu_keep_sim
gen b2=prop_b2*tot_blu_rel_sim

gen catch_at_length=ab1+b2
collapse (sum) catch_at_length, by(region species l_cm draw)
sort region spec draw l_c

egen sumfish=sum(catch_at_length), by(region species draw)
gen observed_prob=catch_at_length/sum
drop sumfish
tostring draw, gen(draw1)
gen domain = region+"_"+species+"_"+draw1

drop if catch_at_length==0
rename l_cm length 
sort region spec draw length

preserve 
rename length fitted_length
keep fitted_length observed_prob catch_at_length species draw domain region
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


* Estimate gamma parameters for each distribution
* new code using MOM to avoid non-convergence 

rename catch_at_length  n_fish

tempfile new
save `new', replace
global fitted_sizes

levelsof domain, local(regs)

foreach r of local regs {
    use `new', clear
    keep if domain=="`r'"
    di "`r'"

    keep length n_fish
    drop if missing(length) | missing(n_fish)
    drop if n_fish<=0
	replace n_fish=round(n_fish)
	su n_fish
	local tot_n_fish=`r(sum)'
    * Gamma needs strictly positive support
    drop if length<=0

    * --------
    * (A) Estimate gamma parameters robustly (MOM with freq weights)
    * --------
    quietly summarize length [fw=n_fish], meanonly
    local mu = r(mean)
    local Nw = r(sum_w)

    * Weighted variance: Var = E[x^2] - (E[x])^2 using the same freq weights
    gen double length2 = length^2
    quietly summarize length2 [fw=n_fish], meanonly
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

    * --------
    * (B) Simulate a truncated gamma sample via rejection sampling
    * --------
    *local ndraw = min(1000000, `tot_n_fish')   // sample size for the simulated distribution
	local ndraw = `tot_n_fish'   // sample size for the simulated distribution

    clear
    set obs `ndraw'

    * draw
    gen double gammafit = rgamma(`alpha', `beta')
    replace gammafit = round(gammafit)

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
sort  fitted_length draw

mvencode fitted_prob observed_prob, mv(0) override 

split domain, parse(_)
replace region=domain1
replace species=domain2

destring domain3, replace
replace draw=domain3
sort species   draw fitted_length
drop _merge domain1 domain2 domain3 

keep fitted_length fitted_prob draw  species  observed_prob region catch_at_length
order draw  species fitted_length fitted_prob observed_prob	
rename fitted_length length 
sort species draw length

gen domain=region+"_"+species

levelsof domain, local(doms)
foreach d of local doms{
quietly summarize length if observed_prob!=0 & !missing(observed_prob) & domain=="`d'"
return list
local minL = `r(min)'
local maxL = `r(max)'
drop if (length<`minL' | length>`maxL' ) & domain=="`d'"
}

egen sum_fitted_prob=sum(fitted_prob), by(domain draw)
replace fitted_prob=fitted_prob/sum_fitted_prob
sort species region draw length
order species region draw length obs fitted 
drop domain sum

*save tempfile of probablites by data region before expanding to mgt. region 
tempfile bluefish_data_region
save `bluefish_data_region', replace

/****
levelsof draw , local(draws)

* Initialize an empty plot command
local plots
/*
* Build up one line per draw
foreach d of local draws {
    local plots `plots' ///
		(line fitted_prob length if draw==`d' & region=="N" & season=="spr", ///
        lcolor(red) lwidth(thin) lpattern(solid)) ///
		(line fitted_prob length if draw==`d' & region=="N" & season=="fall", ///
        lcolor(blue) lwidth(thin) lpattern(solid) ) ///
		(line fitted_prob length if draw==`d' & region=="S" & season=="spr", ///
        lcolor(green) lwidth(thin) lpattern(solid) ) ///
		(line fitted_prob length if draw==`d' & region=="S" & season=="fall", ///
        lcolor(black) lwidth(thin) lpattern(solid) )
}

twoway `plots', ///
    ylabel(, labsize(small)) ///
	xlabel(#20, labsize(small)) ///
    title("Fitted catch-at-length probabilities by length (Bluefish)", size(medium)) ///
    ytitle("Probability", size(medium)) xtitle("Length (cm)", size(medium))  ///
	legend(order(1 "ME-NY spring" 2 "ME-NY fall" 3 "NJ-NC spring" 4 "NJ-NC fall"  ) position(6) cols(2) )
	
*/
foreach d of local draws {
    local plots `plots' ///
		(line fitted_prob length if draw==`d' & region=="N", ///
        lcolor(red) lwidth(thin) lpattern(solid)) ///
		(line fitted_prob length if draw==`d' & region=="S" , ///
        lcolor(green) lwidth(thin) lpattern(solid) ) 
}

* Draw combined graph
twoway `plots', ///
    ylabel(, labsize(small)) ///
	xlabel(#20, labsize(small)) ///
    title("Fitted catch-at-length probabilities by length (Bluefish)", size(medium)) ///
    ytitle("Probability", size(medium)) xtitle("Length (cm)", size(medium))  ///
	legend(order(1 "ME-NY" 2 "NJ-NC"  ) position(6) cols(2) )
	
*export delimited using "$input_data_cd/baseline_catch_at_length.csv", replace 
*/

* expand to management regions
expand 13 if region == "S"	
bysort draw species length region: gen tab=_n

gen region1="MDCHES" if region=="S" &  tab==1 
replace region1="VACHES" if region=="S" & tab==2
replace region1="DEDERIV" if region=="S" & tab==3
replace region1="DENANT" if region=="S" & tab==4
replace region1="DEOCN" if region=="S" & tab==5
replace region1="MDOCN" if region=="S" & tab==6
replace region1="MDPOT" if region=="S" & tab==7
replace region1="NCALB" if region=="S" & tab==8
replace region1="NCCNTRL" if region=="S" & tab==9
replace region1="NCOCN" if region=="S" & tab==10
replace region1="VAOCN" if region=="S" & tab==11
replace region1="VAPOTO" if region=="S" & tab==12
replace region1="NJALL" if region=="S" & tab==13


drop tab
expand 7 if region == "N"	
bysort draw species length region: gen tab=_n
replace region1="CTALL" if region=="N" & tab==1
replace region1="MAALL" if region=="N" & tab==2
replace region1="MEALL" if region=="N" & tab==3
replace region1="NHALL" if region=="N" & tab==4
replace region1="RIALL" if region=="N" & tab==5
replace region1="NYHUDN" if region=="N" & tab==6
replace region1="NYHUDS" if region=="N" & tab==7

drop tab 
drop region 
rename region region
sort region draw fitted
tempfile bluefish
save `bluefish', replace
	


	
**************************
****** Striped bass  ******
**************************

* Discard lengths from the assessment (Katie Drew)
import delimited using "$length_data_cd/SB_deadB2_LFs_2023_formatted.csv", clear 
*translate inches to cms
replace length=length*2.54
*replace waters="N" if inlist(waters, "ME", "NH", "MA", "RI", "CT", "NY")
*replace waters="S" if inlist(waters, "NJ", "DE", "MD", "VA", "NC")

drop if waters=="CSTL"
replace length=round(length)
collapse (sum) nfish, by(length waters)
sort waters leng


egen sum=sum(nfish), by(waters)
gen prop_b2=nfish/sum
gen species="str"
rename length l_cm_bin
drop nfish sum

tempfile striper_b2
save `striper_b2', replace 

**************************
* MRIP harvest lengths - striped bass
**************************

cd $input_data_cd

clear

mata: mata clear

tempfile tl1 sl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
*replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
drop if strmatch(id_code, "*xx*")==1
drop if strat_id==""
duplicates drop 
save `tl1'
clear
 
dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if $calibration_year
 
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace
destring month, replace
gen date2=dmy(day1, month, year)
format date2 %td

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


preserve
import excel using "$input_data_cd/SB_MRIP_Management_Areas_DE_NC_NY_VA_reformat.xlsx", clear firstrow
duplicates drop 
drop if intsite==483 & state=="NC" & mgt=="CENTRAL SOUTHERN"
tempfile sites
save `sites', replace
restore
merge m:1 state intsite using `sites', keep(1 3)

*browse if _merge==1
replace mgt = "DE_RIVER_BAY_PRIMARY_AREA" if mgt=="" & state=="DE" & inlist(intsite, 65, 73, 106) 
replace mgt= "CHESAPEAKE BAY" if mgt=="" & state=="VA" & inlist(intsite, 212, 995) 

*rename mgt areas 
replace mgt="ALB"  if mgt=="ALBEMARLE SOUND" 
replace mgt="OCN"  if mgt=="ATLANTIC OCEAN" 
replace mgt="CNTRL"  if mgt=="CENTRAL SOUTHERN" 
replace mgt="CHES"  if mgt=="CHESAPEAKE BAY" 
replace mgt="NANT"  if mgt=="DE_NATICOKE_RIVER_SPAWNING_AREA" 
replace mgt="DERIV"  if mgt=="DE_RIVER_BAY_PRIMARY_AREA" 
replace mgt="HUDN"  if mgt=="HUDSON_NORTH_OF_GW_BRIDGE" 
replace mgt="HUDS"  if mgt=="MARINE_AND_SOUTH_OF_GW_BRIDGE" 
replace mgt="POTO"  if mgt=="POTOMAC RIVER TRIBUTARIES" 

replace mgt="OCN" if state=="DE" & area=="1"

**Deal with Maryland**

* First, identify Potomac River sites. 
* There are a handful of sites along the Potomoac River, but near a striper-regs-map mgt. area
* If a trip is a boat trip, classify it as POT
* If a trip is a shore, classify it as CHES, unless it is one of the few sites where a shore trip would likely be fishing in the POT
replace mgt="POT" if state=="MD" & inlist(intsite, 788, 1969, 1865, 0107, 3076, 3077, 3774, 0233) & inlist(mode1, "pr", "fh")
replace mgt="POT" if state=="MD" & inlist(intsite, 1865) & inlist(mode1, "sh")
replace mgt="CHES" if state=="MD" & inlist(intsite, 788, 1969, 0107, 3076, 3077, 3774, 0233) & inlist(mode1, "sh")

* Next, MD coastal sites
replace mgt="OCN"  if state=="MD" & inlist(intsite,3097, 853, 4419, 24, 856, 1493, 858, 860,  906, 4349, 1658, 152, 1183, 3442)

* Lastly, merge MD march-may sites to regulation region site list 
replace mgt="CHES" if state=="MD"  & mgt==""

replace mgt="ALL" if mgt==""

* create strata for CHES. vs non-CHES
gen waters="CHES" if inlist(mgt, "CHES")
replace waters=state if !inlist(mgt, "CHES")  & inlist(state, "ME", "NH", "MA", "RI", "CT", "NY")
replace waters=state if !inlist(mgt, "CHES")  & inlist(state, "NJ", "DE", "MD", "VA", "NC")

keep if $calibration_year //ensure relevent year
 

 * classify catch into the things I care about (common=="s" | "b") and things I don't care about "z" 
gen common_dom="z"
replace common_dom="s" if strmatch(sp_code,"8835020102")
replace common_dom="b" if strmatch(sp_code,"8835250101")


* this might speed things up if I re-classify all length=0 for the species I don't care about 
replace l_cm_bin =0 if !inlist(common_dom, "s", "b")
replace l_in_bin =0 if !inlist(common_dom, "s", "b")

sort year  strat_id psu_id id_code

drop if common_dom=="z"
preserve
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

keep if inlist(common_dom, "s", "b")
replace wgt=wgt* 2.20462
gen ln_wt = ln(wgt)
gen ln_len = ln(l_in_bin)

gen lw_region = ""
replace lw_region = "north" if inlist(state, "ME", "NH", "MA", "RI", "CT", "NY")
replace lw_region = "south" if inlist(state, "NJ", "DE", "MD", "VA", "NC")

gen species="str" if common_dom=="s"
replace species="blu" if common_dom=="b"
postfile lw str20 species str10 lw_region double alpha beta sigma2 using "$input_data_cd/mrip_lw_params.dta", replace

levelsof species, local(species)
levelsof lw_region, local(regions)

foreach sp of local species {
    foreach r of local regions {
        quietly count if species == "`sp'" & lw_region == "`r'"
        if r(N) >= 30 {
            svy: reg ln_wt ln_len if species == "`sp'" & lw_region == "`r'"
			predict resid, resid
			gen resid2 = resid^2
			sum resid2 [aw = wp_int]
			local sigma2 = r(mean)
			drop resid resid2 
            post lw ("`sp'") ("`r'") (_b[_cons]) (_b[ln_len]) (`sigma2')
        }
    }
}

postclose lw
restore 


/*
drop region
gen region="N" if inlist(state, "ME", "NH", "MA", "RI", "CT", "NY")
replace region="S" if inlist(state, "NJ", "DE", "MD", "VA", "NC")
*/
gen my_dom_id_string=waters+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)



gen nfish_ab1=1
collapse (sum) nfish_ab1, by(common_dom waters l_cm_bin)


gen species="str" if common_dom=="s"
replace species="blu" if common_dom=="b"

sort waters species l_cm_bin
keep if species=="str"

egen sum=sum(nfish_ab1), by(waters)
gen prop_ab1=nfish_ab1/sum
drop sum nfish_ab1

* merge harvest lengths to discards lengths
merge 1:1 l_cm_bin species waters using `striper_b2'


mvencode prop*, mv(0) override
drop _merge
sort waters l

expand $ndraws
bysort species l waters: gen draw=_n
sort species waters draw l

preserve
u "$iterative_input_data_cd\simulated_catch_totals_for_length_distn.dta", clear 
replace waters=state if waters!="CHES"
replace waters=state  if !inlist(waters, "CHES")  & inlist(state, "ME", "NH", "MA", "RI", "CT", "NY")
replace waters=state if !inlist(waters, "CHES")  & inlist(state, "NJ", "DE", "MD", "VA", "NC")
collapse (sum) tot_str_keep_sim tot_str_rel_sim, by(waters draw)
keep if draw<=$ndraws

tempfile regional_total_str
save `regional_total_str', replace
restore 

merge m:1 waters draw using `regional_total_str'
keep if draw<=$ndraws
drop _merge

gen ab1=prop_ab1*tot_str_keep_sim
gen b2=prop_b2*tot_str_rel_sim

gen catch_at_length=ab1+b2
collapse (sum) catch_at_length ab1 b2, by(waters species l_cm draw)
sort waters spec draw l_c

egen sumfish=sum(catch_at_length), by(waters species draw)
gen observed_prob=catch_at_length/sum
drop sumfish

egen sumfish=sum(ab1), by(waters species draw)
gen observed_prob_ab1=ab1/sum
drop sumfish

tostring draw, gen(draw1)
gen domain = waters+"_"+species+"_"+draw1

drop if catch_at_length==0
rename l_cm length 
sort waters spec draw length

preserve 
rename length fitted_length
keep fitted_length observed_prob* catch_at_length species draw domain waters catch_at_length
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


* Estimate gamma parameters for each distribution
* new code using MOM to avoid non-convergence 


rename catch_at_length  n_fish

tempfile new
save `new', replace
global fitted_sizes

levelsof domain, local(regs)

foreach r of local regs {
    use `new', clear
    keep if domain=="`r'"
    di "`r'"

    keep length n_fish
    drop if missing(length) | missing(n_fish)
    drop if n_fish<=0
	replace n_fish=round(n_fish)
	su n_fish
	local tot_n_fish=`r(sum)'
    * Gamma needs strictly positive support
    drop if length<=0

    * --------
    * (A) Estimate gamma parameters robustly (MOM with freq weights)
    * --------
    quietly summarize length [fw=n_fish], meanonly
    local mu = r(mean)
    local Nw = r(sum_w)

    * Weighted variance: Var = E[x^2] - (E[x])^2 using the same freq weights
    gen double length2 = length^2
    quietly summarize length2 [fw=n_fish], meanonly
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

    * --------
    * (B) Simulate a truncated gamma sample via rejection sampling
    * --------
	local ndraw = min(5000000, `tot_n_fish')   // sample size for the simulated distribution
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
        keep if gammafit>=`minL' & gammafit<=`maxL'
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
sort  fitted_length draw

mvencode fitted_prob* observed_prob, mv(0) override 

split domain, parse(_)
replace waters=domain1
replace species=domain2
destring domain3, replace
replace draw=domain3
sort species  draw fitted_length
drop _merge domain1 domain2 domain3

keep fitted_length fitted_prob* draw  species observed_prob waters catch_at_length
order draw  species fitted_length fitted_prob observed_prob	
rename fitted_length length 
sort species waters draw length

*truncate to observed range of catch lengths
gen domain=waters+"_"+species

levelsof domain, local(doms)
foreach d of local doms{
quietly summarize length if observed_prob!=0 & !missing(observed_prob) & domain=="`d'"
return list
local minL = `r(min)'
local maxL = `r(max)'
drop if (length<`minL' | length>`maxL' ) & domain=="`d'"
}

egen sum_fitted_prob=sum(fitted_prob), by(domain draw)
replace fitted_prob=fitted_prob/sum_fitted_prob
sort species waters draw length
order species waters draw length obs fitted 
drop domain sum
drop if inlist(waters, "VA", "NC")


preserve
append using `bluefish_data_region'
export delimited using "$input_data_cd/baseline_catch_at_length_data_rgns.csv", replace 
restore

****
/*
levelsof draw , local(draws)

* Initialize an empty plot command
local plots
foreach d of local draws {
    local plots `plots' ///
		(line fitted_prob length if draw==`d' & waters=="N", ///
        lcolor(blue) lwidth(thin) lpattern(solid)) ///
		(line fitted_prob length if draw==`d' & waters=="S", ///
        lcolor(red) lwidth(thin) lpattern(solid)) ///
		(line fitted_prob length if draw==`d' & waters=="CHES" , ///
        lcolor(green) lwidth(thin) lpattern(solid) ) 
}

* Draw combined graph
twoway `plots', ///
    ylabel(, labsize(small)) ///
	xlabel(#20, labsize(small)) ///
    title("Fitted catch-at-length probabilities by length (Striped bass)", size(medium)) ///
    ytitle("Probability", size(medium)) xtitle("Length (cm)", size(medium))  ///
	legend(order(1 "MA-NY" 2 "NJ-NC" 3 "Chesapeake Bay" ) position(6) cols(3) )

*/
* expand to management regions
expand 2 if waters == "CHES"	
bysort draw species length water: gen tab=_n
gen region="MDCHES" if water=="CHES" & tab==1
replace region="VACHES" if water=="CHES" & tab==2

drop tab
replace region="CTALL" if waters=="CT"
replace region="MAALL" if waters=="MA"
replace region="MEALL" if waters=="ME"
replace region="NHALL" if waters=="NH"
replace region="RIALL" if waters=="RI"
replace region="NJALL" if waters=="NJ"


expand 2 if waters == "NY"	 
bysort draw species length water: gen tab=_n
replace region="NYHUDN" if water=="NY" & tab==1
replace region="NYHUDS" if water=="NY" & tab==2
drop tab

expand 3 if waters == "DE"	 
bysort draw species length water: gen tab=_n
replace region="DEDERIV" if water=="DE" & tab==1
replace region="DENANT" if water=="DE" & tab==2
replace region="DEOCN" if water=="DE" & tab==3
drop tab

expand 7 if waters == "MD"	 
bysort draw species length water: gen tab=_n
replace region="MDOCN" if water=="MD" & tab==1
replace region="MDPOT" if water=="MD" & tab==2
replace region="NCALB" if water=="MD" & tab==3
replace region="NCCNTRL" if water=="MD" & tab==4
replace region="NCOCN" if water=="MD" & tab==5
replace region="VAOCN" if water=="MD" & tab==6
replace region="VAPOTO" if water=="MD" & tab==7
drop tab

	
append using `bluefish'	
drop waters
order region  species draw length 
sort  region species draw length 



****
/*
levelsof draw , local(draws)

* Initialize an empty plot command
local plots
foreach d of local draws {
    local plots `plots' ///
		(line fitted_prob_ab1 length if draw==`d' & species=="str" & region=="CTALL", ///
        lcolor(red) lwidth(thin) lpattern(solid)) ///
		(line fitted_prob_b2 length if draw==`d' & species=="str" & region=="CTALL", ///
        lcolor(green) lwidth(thin) lpattern(solid) ) 
}

* Draw combined graph
twoway `plots', ///
    ylabel(, labsize(small)) ///
	xlabel(#20, labsize(small)) ///
    title("Fitted catch-at-length probabilities by length (Striped bass)", size(medium)) ///
    ytitle("Probability", size(medium)) xtitle("Length (cm)", size(medium))  ///
	legend(order(1 "Fitted" 2 "Observed"  ) position(6) cols(2) )
*/
export delimited using "$input_data_cd/baseline_catch_at_length_multi_region.csv", replace 


import delimited using "$input_data_cd/baseline_catch_at_length_multi_region.csv", clear 

/*
gen n_regions="multi"
tempfile base 
save `base', replace


import delimited using "$input_data_cd/baseline_catch_at_length.csv", clear 
gen n_regions="three"
tempfile base 
save `base', replace


import delimited using "$input_data_cd/baseline_catch_at_length_multi_region.csv", clear 
gen n_regions="multi"
append using `base'


collapse (mean) fitted_prob, by(region species length n_regions)

			
twoway ///
			(line fitted_prob length if species=="str"  & n_regions=="multi", by(region) lwidth(thin) lpattern(solid)) 
			
twoway ///
			(line fitted_prob length if species=="blu"  & n_regions=="multi", by(region) lwidth(thin) lpattern(solid)) 
			
			
			
			
twoway ///
			(line fitted_prob length if species=="blu" & region=="CTALL" & n_regions=="multi",  lwidth(thin) lpattern(solid)) ///
			(line fitted_prob length if species=="blu" & region=="VAOCN" & n_regions=="multi", lwidth(thin) lpattern(solid) ///
			legend(order(1 "CTALL" 2 "VAOCN" ) position(6) cols(3) ))
twoway ///
			(line fitted_prob length if species=="str" & region=="NJALL" & n_regions=="multi",  lwidth(thin) lpattern(solid)) ///
			(line fitted_prob length if species=="str" & region=="NJALL" & n_regions=="one", lwidth(thin) lpattern(solid) ///
			legend(order(1 "NJALL" 2 "NS" ) position(6) cols(3) ))			

			
			
twoway (line fitted_prob length if species=="blu" & region=="CTALL" & n_regions=="three",  lcolor(red) lwidth(thin) lpattern(solid)) ///
			(line fitted_prob length if species=="blu" & region=="MDCHES" & n_regions=="three", lcolor(blue) lwidth(thin) lpattern(solid))  ///
			(line fitted_prob length if species=="blu" & region=="CTALL" & n_regions=="one", lcolor(black) lwidth(thin) lpattern(solid) ///
			legend(order(1 "N" 2 "S" 3 "NS" ) position(6) cols(2) ))
			
export delimited using "$input_data_cd/mean_catch_at_length_check.csv", replace 
*/