
***This code creates trip cost distributions based on the Sabrina's 2017 trip expenditure survey data

set seed $seed

*Enter a directory with the expenditure survey data 
u "$input_data_cd\gulf_atl_2022.dta", clear
renvarlab *, lower


* As per Sabrina, run the following code before using the 2022 data. This code sets certain expenditure variables to missing depending on the trip mode. 
* For-Hire trips: set boat fuel and boat rental to missing
replace bfuelexp = . if mode == "For-Hire"
replace brentexp = . if mode == "For-Hire"

* Private Boat trips: set guide costs and crew tips to missing
replace guideexp = . if mode == "Private Boat"
replace crewexp  = . if mode == "Private Boat"

* Shore trips: set all of those to missing
replace bfuelexp = . if mode == "Shore"
replace crewexp  = . if mode == "Shore"
replace guideexp = . if mode == "Shore"
replace brentexp = . if mode == "Shore"


*keep only the states we need (MA-NC) 
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

mvencode afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp  othexp, mv(0) override


*replace some non-trip expenses included in "other" category as zero
replace othexp=0 if inlist(oth_cat, "2 LICENSES", "BOAT REPAIR", "Boat Towing", "CART", "FISHING LICENSE")
replace othexp=0 if inlist(oth_cat,"LICENSE", "LICENSES", "MONEY SPENT AT CASINO", "NEW ROD", "SEATOW", "SPA", "HAT")


* Compute total trip expenditure
egen total_exp=rowtotal(afuelexp arentexp ptransexp lodgexp grocexp restexp baitexp iceexp parkexp bfuelexp brentexp guideexp crewexp procexp feesexp giftsexp othexp) 

svyset psu_id [pweight= sample_wt], strata(var_id) singleunit(certainty)

merge m:1 prim1 using "$input_data_cd\prim1.dta", keep(1 3) nogen 
merge m:1 prim2 using "$input_data_cd\prim2.dta", keep(1 3) nogen 


*Sabrina's definition of for-hire mode include both headboat and charter boats
*Survey mode definitions:
	*3=shore
	*4=headboat
	*5=charter
	*7=private boat
/*
svy: tabstat total_exp, stat(mean sd) by(state)
svy: mean total_exp if state=="MA"
svy: mean total_exp if state=="RI"
svy: mean total_exp if state=="CT"
svy: mean total_exp if state=="NY"
svy: mean total_exp if state=="NJ"
svy: mean total_exp if state=="DE"
svy: mean total_exp if state=="MD"
svy: mean total_exp if state=="VA"
svy: mean total_exp if state=="NC"
*/
/*
mat b=e(b)'
mat v= e(V)

clear 
svmat b
rename b1 mean
svmat v
rename v1 st_error
replace st_error=sqrt(st_error)
*/

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="fh" if inlist(mode_fx, "4", "5")
replace mode1="pr" if inlist(mode_fx,  "7")

*Adjust for inflation
replace total_exp = total_exp*$inflation_expansion

gen common_dom="1" if inlist(prim1_common, "stripedbass", "bluefish")
replace common_dom="2" if common_dom==""

keep if common_dom=="1"

gen domain=state+"_"+mode1
encode domain, gen(domain2)


preserve
keep domain domain2
duplicates drop 
tempfile domains
save `domains', replace 
restore


preserve
svy: mean total_exp, over(domain2)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 domain2
destring domain2, replace
merge 1:1 domain2 using `domains'

drop rname domain2 _merge 
order domain

split domain, parse(_)
rename domain1 state
rename domain2 mode
renam b cost 
keep state mode cost se  ll ul
order state mode cost se  ll ul
tempfile observed 
save `observed', replace 
restore


*Two-part ("hurdle") simulation with a calibrated lognormal for positive costs, by state×mode domain.
drop domain
egen str5 domain = concat(state mode1), punct("_")
encode domain, gen(dom2)

*keep if domain=="CT_pr"
svy: mean total_exp, over(dom2)
gen cost=total_exp

* Observed cap (e.g., 99th percentile) for positive costs
/*
preserve
keep if cost>0 & !missing(cost, dom2)

tempfile caps
postfile C int dom2 double cap99 using `caps', replace

levelsof dom2, local(domlist)
foreach d of local domlist {
     _pctile cost [pw=sample_wt] if dom2==`d', p(99)
    scalar cap =  r(r1)
    post C (`d') (cap)
}
postclose C
use `caps', clear
save `caps', replace
restore
*/
*----------------------------
* Cost indicators
*----------------------------
gen byte pos_cost = cost > 0 if !missing(cost)

gen double lncost  = ln(cost)  if cost > 0
gen double lncost2 = lncost^2  if cost > 0

svy: mean pos_cost, over(dom2)



*Estimate the mean positive cost by domain (survey-weighted)
*used to calibrate the lognormal so the simulated positive-cost mean matches the survey positive-cost mean.
preserve
keep if cost>0 & !missing(dom2)

tempfile meanpos
postfile M int dom2 double mean_pos using `meanpos', replace

levelsof dom2, local(domlist)
foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean cost
    matrix b = e(b)
    post M (`d') (b[1,1])
}
postclose M
use `meanpos', clear
save `meanpos', replace
restore



*estimate survey conditional mean of positive costs by domain
*provides Bernoulli probability used later in simulation: spend = (runiform() < p_hat)
preserve
tempfile p_pos
postfile P int dom2 str5 domain double p_hat se_p long N using `p_pos', replace

levelsof dom2, local(domlist)

foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean pos_cost
    matrix b = e(b)
    matrix V = e(V)

    scalar p  = b[1,1]
    scalar se = sqrt(V[1,1])

    quietly count if dom2==`d'
    local domname : label (dom2) `d'

    post P (`d') ("`domname'") (p) (se) (r(N))
}
postclose P
restore


*Estimate lognormal dispersion for positive costs by domain (survey-weighted)
*gives the shape/variance of the positive-cost distribution on the log scale.
preserve
keep if cost > 0 & !missing(dom2, lncost, lncost2)

tempfile ln_parms
postfile L int dom2 str5 domain double mu_hat m2_hat sig2_hat double v11 v22 v12 long N using `ln_parms', replace

levelsof dom2, local(domlist)

foreach d of local domlist {
    quietly svy, subpop(if dom2==`d'): mean lncost lncost2
    matrix b = e(b)
    matrix V = e(V)

    scalar mu  = b[1,1]
    scalar m2  = b[1,2]
    scalar s2  = m2 - mu^2
    if (s2 < 1e-10) scalar s2 = 1e-10

    quietly count if dom2==`d'
    local domname : label (dom2) `d'

    post L (`d') ("`domname'") ///
        (mu) (m2) (s2) ///
        (V[1,1]) (V[2,2]) (V[1,2]) ///
        (r(N))
}
postclose L
restore

use `p_pos', clear
merge 1:1 dom2 using `ln_parms', nogen
*merge 1:1 dom2 using `caps', nogen
merge 1:1 dom2 using `meanpos', nogen

*calibrate the lognormal mean to match mean_pos
*simulated positive-cost mean should line up with the survey-estimated positive-cost mean (up to Monte Carlo error), while keeping the estimated log-variance sig2_hat
gen double mu_adj = ln(mean_pos) - 0.5*sig2_hat

*keep if domain=="DE_fh"
local n_draws = 50000

expand `n_draws'
bysort dom2: gen long draw = _n


*Simulate trip costs 
*Part A - zero costs:
gen byte spend = runiform() < p_hat

* Part B  - Positive costs
gen double cost_sim = 0
replace cost_sim = exp(rnormal(mu_adj, sqrt(sig2_hat))) if spend==1

* Check mass at zero
by dom2: egen share_zero = mean(cost_sim==0)
list dom2 domain p_hat share_zero in 1/10
*replace cost_sim = cap99 if cost_sim > cap99 & spend==1

split domain, parse(_)
rename domain1 state
rename domain2 mode
rename draw tripid
keep mode cost tripid state
compress

format cost %9.2f
order state mode tripid cost

*compare simulated versus observed
/*
collapse (mean) cost_sim=cost (sd) sd_cost=cost, by(state mode)
merge 1:1 state mode using `observed'
gen se_sim=sqrt(sd)

order state mode cost_sim cost se_sim se
gen pct_dif=((cost_sim-cost)/cost)*100

su pct_dif
*/

save "$input_data_cd\trip_costs.dta", replace 

/*
levelsof domain, local(domains)

tempfile new
save `new', replace

mata: mata clear
clear
tempfile master
save `master', emptyok
foreach d of local domains{
	u `new', clear 
	keep if domain=="`d'"
	collapse (mean) cost_sim (sd) sd_cost=cost_sim, by(domain)
    append using `master'
    save `master', replace
    clear                            
        }


use `master', clear
sort domain
*/

