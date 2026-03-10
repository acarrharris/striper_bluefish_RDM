

* This file compiles angler demographics that enter the trip utility model 

set seed $seed

* Variables needed: 
	* # days fished past 12 months - I get this from the MRIP FES 
	* Do you like saltwater fishing more or less than other recreational activities? - I get this below from the new choice experiment 
	* Education - I get this below from sabrina's 2019 durable survey data 
	* Do you own a boat? - I get this from the choice experiment sample
	

* random draws of edication levels 
* set up the population education information: 
local p1  = 5.1869    // percent in education level 1
local se1 = 0.7127    // SE of percent in education level 1

local p2  = 18.9534
local se2 = 1.2590

local p3  = 28.1474
local se3 = 1.4768

local p4  = 25.8252
local se4 = 1.4009

local p5  = 21.8872
local se5 = 1.3107

foreach i in 1 2 3 4 5 {
    scalar p`i'  = `p`i''/100
    scalar v`i'  = (`se`i''/100)^2   // variance of proportion
    scalar a0`i' = p`i'*(1-p`i')/v`i' - 1   // alpha0 candidate from var(p_i)=p_i(1-p_i)/(α0+1)
}

* Inverse-variance–weighted estimate of alpha0 (ignore any nonpositive candidates)
scalar w1 = 1/v1
scalar w2 = 1/v2
scalar w3 = 1/v3
scalar w4 = 1/v4
scalar w5 = 1/v5
scalar a0 = (w1*a01 + w2*a02 + w3*a03 + w4*a04 + w5*a05) / (w1 + w2 + w3 + w4 + w5)

* 5-category Dirichlet alpha's
forvalues i=1/5 {
    scalar a`i' = p`i' * a0
}

* Group to 3 bins: (1-2), (3-4), (5)
scalar a12 = a1 + a2
scalar a34 = a3 + a4
scalar a5g = a5     // rename to avoid clash with a5 scalar



forvalues i = 1/$ndraws {
	
		*local i=1
	   use "$iterative_input_data_cd\calib_catch_draws_`i'.dta", clear 
	   
	   preserve 
	   keep mode day tripid
	   duplicates drop
	   count
	   local N_needed=`r(N)'
	   gen merge_id=_n
	   tempfile individuals 
	   save `individuals', replace 
	   restore 
	   
	   
	   tempfile base 
	   save `base', replace 
	   
	   u "$input_data_cd\choice_exp_angler_dems.dta", clear 
	   count
	   local N=`r(N)'
	   local expand = (`N_needed'/`N')+1
	   expand `expand'
	   sample `N_needed', count
	   
	   set seed `i'
	   scalar g1 = rgamma(a12,1)
	   scalar g2 = rgamma(a34,1)
	   scalar g3 = rgamma(a5g,1)
	   scalar S  = g1+g2+g3
	   scalar p12 = g1/S
	   scalar p34 = g2/S
	   scalar p5b = g3/S
	   
	   gen rand= runiform()
	   gen byte edu_grp = cond(rand < p12, 1, cond(rand < p12 + p34, 2, 3))
	   label define edu3 1 "Ed 1–2" 2 "Ed 3–4" 3 "Ed 5", replace
	   label values edu_grp edu3
	   tab edu_grp, gen(educ)
	   drop edu_grp rand qtid
	   gen merge_id=_n

	   merge 1:1 merge_id using `individuals'
	   drop _merge  merge_id
	   merge 1:m mode day tripid using `base'
	   drop _merge
	   order draw mode day tripid catch_draw
	   
	   save  "$iterative_input_data_cd\calib_catch_draws_`i'.dta", replace
	}


			
	

