

global input_data_cd "E:\Lou_projects\striper_bluefish_RDM\input_data" /* Lou's local data path */

qui{
use "$input_data_cd\striper_CE_data.dta", clear

gen log_striper_length=log(striper_length) if inlist(alt, 1,2)
mvencode log_striper_length, mv(0)
gen sqrt_striper_length=sqrt(striper_length)

gen striper_catch=striper_keep+striper_rel
gen sqrt_striper_catch=sqrt(striper_catch)

gen sqrt_striper_keep_log_length=sqrt_striper_keep*log_striper_length
gen sqrt_striper_rel_log_length=sqrt_striper_rel*log_striper_length

gen sqrt_striper_keep_sqrt_length=sqrt_striper_keep*sqrt_striper_length
gen sqrt_striper_rel_sqrt_length=sqrt_striper_rel*sqrt_striper_length

gen sqrt_striper_catch_log_length=sqrt_striper_rel*log_striper_length
gen sqrt_striper_catch_sqrt_length=sqrt_striper_catch*sqrt_striper_length

gen sqrt_striper_catch_length=sqrt_striper_catch*striper_length
gen sqrt_striper_catch_length2=sqrt_striper_catch*striper_length^2

split version, parse(" ")
rename version1 svy_version
drop version2 version3 version4

gen striper_keep_18_23=striper_keep if inlist(striper_length, 18, 23) & striper_keep>0
gen striper_keep_28_33=striper_keep if inlist(striper_length, 28, 33) & striper_keep>0
gen striper_keep_38_43=striper_keep if inlist(striper_length, 38, 43) & striper_keep>0

gen striper_rel_18_23=striper_rel if inlist(striper_length, 18, 23) & striper_rel>0
gen striper_rel_28_33=striper_rel if inlist(striper_length, 28, 33) & striper_rel>0
gen striper_rel_38_43=striper_rel if inlist(striper_length, 38, 43) & striper_rel>0

gen striper_keep_18_28=striper_keep if inlist(striper_length, 18, 23, 28) & striper_keep>0
gen striper_keep_18_33=striper_keep if inlist(striper_length, 18, 23, 28, 33) & striper_keep>0
gen striper_keep_18_38=striper_keep if inlist(striper_length, 18, 23, 28, 33, 38) & striper_keep>0
gen striper_keep_33_43=striper_keep if inlist(striper_length, 33, 38, 43) & striper_keep>0
gen striper_keep_23_38=striper_keep if inlist(striper_length, 23, 28, 33, 38) & striper_keep>0
gen striper_keep_23_33=striper_keep if inlist(striper_length, 23, 28, 33) & striper_keep>0

gen striper_rel_18_28=striper_rel if inlist(striper_length, 18, 23, 28) & striper_rel>0
gen striper_rel_18_33=striper_rel if inlist(striper_length, 18, 23, 28, 33) & striper_rel>0
gen striper_rel_18_38=striper_rel if inlist(striper_length, 18, 23, 28, 33, 38) & striper_rel>0
gen striper_rel_33_43=striper_rel if inlist(striper_length, 33, 38, 43) & striper_rel>0

local vars striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 striper_keep_18_33 ///
striper_keep_33_43  striper_keep_18_28 striper_rel_18_28 striper_rel_33_43 striper_keep_18_38 striper_rel_18_38 striper_rel_18_33 striper_keep_23_38 striper_keep_23_33

foreach v of local vars{
	gen sqrt_`v'= sqrt(`v')
}

mvencode striper_rel_* striper_keep_* striper_catch* sqrt_striper_rel_* sqrt_striper_keep_* sqrt_striper_catch_* , mv(0) override

foreach L in 18 23 28 33 38 43 {
    gen keep_`L'=striper_keep if striper_length==`L' & striper_keep>0
	gen rel_`L'=striper_rel if striper_length==`L' & striper_rel>0
	gen catch_`L'= striper_catch if striper_length==`L' & (striper_rel>0 | striper_rel>0) 
	mvencode keep_`L' rel_`L' catch_`L', mv(0)
	gen sqrt_striper_keep_`L'=sqrt(keep_`L')
	gen sqrt_striper_rel_`L'=sqrt(rel_`L')
	gen sqrt_striper_catch_`L'=sqrt(catch_`L')
}				

gen striper_length2=striper_length^2
gen sqrt_kp_18_33_length=sqrt_striper_keep_18_33*striper_length
gen sqrt_kp_18_38_length=sqrt_striper_keep_18_38*striper_length
gen sqrt_kp_18_33_log_length=sqrt_striper_keep_18_33*log_striper_length
gen sqrt_kp_23_33_log_length=sqrt_striper_keep_18_33*log_striper_length
gen sqrt_kp_18_38_log_length=sqrt_striper_keep_18_38*log_striper_length
gen sqrt_kp_18_33_sqrt_length=sqrt_striper_keep_18_33*sqrt_striper_length
gen sqrt_kp_18_38_sqrt_length=sqrt_striper_keep_18_38*sqrt_striper_length
gen sqrt_kp_18_33_length2=sqrt_striper_keep_18_33*striper_length2
gen sqrt_kp_18_38_length2=sqrt_striper_keep_18_38*striper_length2


save "$input_data_cd\striper_CE_data_formatted.dta", replace

}


* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 

preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
        alt1_choice_protest!=1 & alt2_choice_protest!=1  /// 
				& !missing(opt_other_likely) ///
				& !missing(opt_other_days_str_blue12) ///
				& !missing(opt_other_days_non_str_blue12) ///
				& !missing(opt_out_male) ///
				& !missing(opt_out_age)

* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

cmset qtid scenario alt
sort qtid scenario alt

global ifsamp ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age)

global max_options ///
    difficult intmethod(halton) intpoints(500) ///
    technique(bhhh 15 nr 5) iterate(25) ///
    nrtolerance(1e-3)

set seed 03211990

* common estimation sample
gen byte est_samp = $ifsamp

* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length
set seed 03211990



*Esimtation 
sort qtid scenario alt
set seed 03211990

*m0
eststo m0_cmmx_wts: cmxtmixlogit choice cost ///
    sqrt_striper_keep_length ///
    sqrt_striper_rel_length ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estimates save m0_cmmx_wts, replace


sort qtid scenario alt
set seed 03211990

*m0 - centered
eststo m0_cmmx_wts_c: cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c ///
    sqrt_striper_rel_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estimates save m0_cmmx_wts_c, replace

*model D
set seed 03211990

eststo mD_cmmx_wts: cmxtmixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel_log_length ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estimates save mD_cmmx_wts, replace


*model D - centered
set seed 03211990
eststo mD_cmmx_wts_c: cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estimates save mD_cmmx_wts_c, replace

*model A
sort qtid scenario alt
set seed 03211990
eststo mA_cmmx_wts: cmxtmixlogit choice cost  ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 ///
				opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
								sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant $max_options
											
estimates save mA_cmmx_wts, replace 			


esttab  m0_cmmx_wts_c  mA_cmmx_wts  mD_cmmx_wts_c  using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  	///
				order(cost sqrt_striper_keep sqrt_striper_keep_length_c sqrt_striper_keep_length2_c sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
				sqrt_striper_rel sqrt_striper_rel_length_c sqrt_striper_rel_log_length_c sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
				opt_out opt_out_male opt_out_age)	

*model A2
sort qtid scenario alt
set seed 03211990
eststo mA2_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_18 sqrt_striper_keep_23 sqrt_striper_keep_28 ///
				sqrt_striper_keep_33  sqrt_striper_keep_38 sqrt_striper_keep_43 ///
				sqrt_striper_rel_18  sqrt_striper_rel_23 sqrt_striper_rel_28 ///
				sqrt_striper_rel_33 sqrt_striper_rel_38 sqrt_striper_rel_43 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 ///
				opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant $max_options
											
estimates save mA2_cmmx_wts, replace 			

*model A3
sort qtid scenario alt
set seed 03211990
eststo mA3_cmmx_wts: cmxtmixlogit choice cost  ///
				keep_18 keep_23 keep_28 keep_33 keep_38 keep_43 rel_18 rel_23 rel_28 rel_33 rel_38 rel_43 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 ///
				opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(bluefish_keep bluefish_rel opt_other opt_out) noconstant $max_options
											
estimates save mA3_cmmx_wts, replace 			
		
/*--------------------------------------------------
* Trophy harvest specification
*  - Trophy keep: length == 43
*  - Small/medium keep: length < 43
*  - Small/medium keep interacted with centered log(length)
*--------------------------------------------------

capture drop striper_keep_sm striper_keep_trophy ///
             sqrt_striper_keep_sm sqrt_striper_keep_trophy ///
             log_striper_keep_sm_length ///
             c_log_striper_keep_sm_length ///
             sqrt_striper_keep_sm_log_length_c

gen striper_keep_sm = striper_keep if inlist(alt,1,2) ///
    & striper_keep > 0 ///
    & striper_length < 43

gen striper_keep_trophy = striper_keep if inlist(alt,1,2) ///
    & striper_keep > 0 ///
    & striper_length == 43

mvencode striper_keep_sm striper_keep_trophy, mv(0) override

gen sqrt_striper_keep_sm     = sqrt(striper_keep_sm)
gen sqrt_striper_keep_trophy = sqrt(striper_keep_trophy)

gen log_striper_keep_sm_length = log(striper_length) if inlist(alt,1,2) ///
    & striper_keep > 0 ///
    & striper_length < 43

sum log_striper_keep_sm_length if est_samp, meanonly
local mean_log_keep_sm = r(mean)

gen c_log_striper_keep_sm_length = ///
    log_striper_keep_sm_length - `mean_log_keep_sm' ///
    if inlist(alt,1,2) ///
    & striper_keep > 0 ///
    & striper_length < 43

mvencode c_log_striper_keep_sm_length, mv(0) override

gen sqrt_str_kp_sm_logL_c = ///
    sqrt_striper_keep_sm * c_log_striper_keep_sm_length				
				
				
sort qtid scenario alt
set seed 03211990

eststo mD_trophy_wts: cmxtmixlogit choice cost ///
    sqrt_str_kp_sm_logL_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep_sm sqrt_striper_keep_trophy ///
           sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estimates save mD_trophy_wts, replace


estimates use mD_trophy_wts

* Recompute centering mean used for small/medium keep log length
gen log_striper_keep_sm_length_tmp = log(striper_length) if inlist(alt,1,2) ///
    & striper_keep > 0 & striper_length < 43

sum log_striper_keep_sm_length_tmp if est_samp, meanonly
local mean_log_keep_sm = r(mean)

* Recompute release log-length centering mean
gen log_striper_rel_length_tmp = log(striper_length) if inlist(alt,1,2) ///
    & striper_rel > 0

sum log_striper_rel_length_tmp if est_samp, meanonly
local mean_log_rel = r(mean)

global params
qui forv x = 1/5000 {

    matrix bfull = e(b)
    local K = e(k)
    matrix b = bfull[1,1..`K']

    matrix Vfull = e(V)
    matrix Ve = Vfull[1..`K',1..`K']
    matrix cholV = cholesky(Ve)

    matrix iid_err = J(`K',1,0)
    forvalues i = 1/`K' {
        matrix iid_err[`i',1] = rnormal()
    }

    matrix beta_draw = b' + cholV * iid_err

    clear
    set obs 1

    gen beta_cost        = beta_draw[1,1]
    gen beta_keep_sm     = beta_draw[colnumb(bfull, "sqrt_striper_keep_sm"),1]
    gen beta_keep_sm_log = beta_draw[colnumb(bfull, "sqrt_str_kp_sm_logL_c"),1]
    gen beta_keep_trophy = beta_draw[colnumb(bfull, "sqrt_striper_keep_trophy"),1]

    gen beta_rel         = beta_draw[colnumb(bfull, "sqrt_striper_rel"),1]
    gen beta_rel_log     = beta_draw[colnumb(bfull, "sqrt_striper_rel_log_length_c"),1]

    gen draw = `x'

    forv L = 18/43 {

        local keep_log_c = log(`L') - `mean_log_keep_sm'
        local rel_log_c  = log(`L') - `mean_log_rel'

        if `L' < 43 {
            gen mwtp_str_keep_`L' = ///
                - (beta_keep_sm + beta_keep_sm_log * `keep_log_c') ///
                / (2 * sqrt(1) * beta_cost)
        }

        if `L' >= 43 {
            gen mwtp_str_keep_`L' = ///
                - beta_keep_trophy / (2 * sqrt(1) * beta_cost)
        }

        gen mwtp_str_rel_`L' = ///
            - (beta_rel + beta_rel_log * `rel_log_c') ///
            / (2 * sqrt(1) * beta_cost)

    }

    tempfile params`x'
    save `params`x'', replace
    global params "$params `params`x''"
}

clear
dsconcat $params

keep draw mwtp_str_keep_* mwtp_str_rel_* 

reshape long mwtp_str_keep_ mwtp_str_rel_ , ///
    i(draw) j(length)

collapse ///
    (p50) mwtp_str_kp_med   = mwtp_str_keep_ ///
          mwtp_str_rel_med  = mwtp_str_rel_ ///
    (p5)  mwtp_str_kp_lb    = mwtp_str_keep_ ///
          mwtp_str_rel_lb   = mwtp_str_rel_ ///
    (p95) mwtp_str_kp_ub    = mwtp_str_keep_ ///
          mwtp_str_rel_ub   = mwtp_str_rel_, ///
    by(length)

gen x_keep = length - 0.15
gen x_rel  = length + 0.15

keep if inlist(length, 18, 23, 28, 33, 38, 43)
twoway ///
    (rcap mwtp_str_kp_lb mwtp_str_kp_ub x_keep if length < 43 & inlist(length, 18, 23, 28, 33, 38, 43), ///
        lwidth(medthin) lcolor(navy)) ///
    (scatter mwtp_str_kp_med x_keep if length < 43 & inlist(length, 18, 23, 28, 33, 38, 43)  , ///
        msymbol(circle) msize(small) mcolor(navy) connect(l) lcolor(navy)) ///
    (rcap mwtp_str_kp_lb mwtp_str_kp_ub x_keep if length == 43, ///
        lwidth(medthin) lcolor(forest_green)) ///
    (scatter mwtp_str_kp_med x_keep if length == 43, ///
        msymbol(diamond) msize(medsmall) mcolor(forest_green)) ///
    (rcap mwtp_str_rel_lb mwtp_str_rel_ub x_rel if inlist(length, 18, 23, 28, 33, 38, 43), ///
        lwidth(medthin) lcolor(maroon)) ///
    (scatter mwtp_str_rel_med x_rel if inlist(length, 18, 23, 28, 33, 38, 43), ///
        msymbol(circle) msize(small) mcolor(maroon)), ///
    legend(order(2 "Harvested striped bass <43 in." ///
                 4 "Harvested trophy striped bass =43 in." ///
                 6 "Released striped bass") ///
           rows(2) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("MWTP ($)", size(small)) ///
    xlabel(18(2)44, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_trophy_keep_rel, replace)

twoway ///
    (rcap mwtp_str_kp_lb mwtp_str_kp_ub x_keep if length < 43 & inlist(length, 18, 23, 28, 33, 38, 43), ///
        lwidth(medthin) lcolor(navy)) ///
    (scatter mwtp_str_kp_med x_keep if length < 43 & inlist(length, 18, 23, 28, 33, 38, 43)  , ///
        msymbol(circle) msize(small) mcolor(navy) connect(l) lcolor(navy)) ///
    (rcap mwtp_str_kp_lb mwtp_str_kp_ub x_keep if length == 43, ///
        lwidth(medthin) lcolor(navy)) ///
    (scatter mwtp_str_kp_med x_keep if length == 43, ///
        msymbol(circle) msize(small) mcolor(navy) ) ///
    (rcap mwtp_str_rel_lb mwtp_str_rel_ub x_rel if inlist(length, 18, 23, 28, 33, 38, 43), ///
        lwidth(medthin) lcolor(maroon)) ///
    (scatter mwtp_str_rel_med x_rel if inlist(length, 18, 23, 28, 33, 38, 43), ///
        msymbol(circle) msize(small) mcolor(maroon) connect(l) lcolor(maroon)), ///
    legend(order(2 "Harvestable striped bass" ///
                 6 "Released striped bass") ///
           rows(2) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("MWTP ($)", size(small)) ///
    xlabel(18(2)44, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_trophy_keep_rel, replace)
*/



* demographics
/*
keep if e(sample)
distinct qtid
keep qtid fishing_skill days_any_salt12 pct_days_stb_blu mode_main stb_harvest_type blu_harvest_type likely_next12 male educ inc age
duplicates drop 

tab fishing_skill, missing
su days_any_salt12
su pct_days_stb_blu
tab mode_main, missing
tab stb_harvest_type, missing
tab blu_harvest_type, missing
tab likely_next12, missing
tab male, missing
tab educ, missing
tab inc, missing
su age
*/


* MWTP plots
*m0
estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m0_cmmx_wts.ster"
local cnames : colnames e(b)
display "`cnames'"

global params
qui forv x=1/5000{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }

        * generate draws from vector beta - sampling uncertainty
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		
		clear 
		set obs 1
		gen beta_cost=beta_draw[1,1]
		gen beta_str_kp=beta_draw[9,1]
		gen beta_str_kp_L=beta_draw[2,1]
		
		gen beta_str_rel=beta_draw[11,1]
		gen beta_str_rel_L=beta_draw[3,1]

		gen beta_blu_keep=beta_draw[12,1]
		gen beta_blu_rel=beta_draw[13,1]
		
		gen draw=`x'

	forv L=18/43 {
          gen mwtp_str_keep_`L'=  -(beta_str_kp + beta_str_kp_L*`L' ) / (sqrt(1)*beta_cost)
          gen mwtp_str_rel_`L'=  -(beta_str_rel + beta_str_rel_L*`L' ) / (sqrt(1)*beta_cost)
	}
      		
		  gen mwtp_blue_keep_`L'= - (beta_blu_keep ) / (sqrt(1)*beta_cost)
          gen mwtp_blue_rel= - (beta_blu_rel ) / (sqrt(1)*beta_cost)
		  
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_str_keep_ mwtp_str_rel_, i(draw) j(new)

rename new length

collapse ///
    (p50) mwtp_str_kp_med = mwtp_str_keep_ ///
          mwtp_str_rel_med  = mwtp_str_rel_ ///
		  mwtp_blu_kp_med=mwtp_blue_keep ///
		  mwtp_blu_rel_med=mwtp_blue_rel ///
    (p5) mwtp_str_kp_lb = mwtp_str_keep_ ///
           mwtp_str_rel_lb  = mwtp_str_rel_ ///
		   mwtp_blu_kp_med_lb=mwtp_blue_keep ///
		  mwtp_blu_rel_med_lb=mwtp_blue_rel ///
    (p95) mwtp_str_kp_ub = mwtp_str_keep_ ///
           mwtp_str_rel_ub  = mwtp_str_rel_ ///
		   mwtp_blu_kp_med_ub=mwtp_blue_keep ///
		  mwtp_blu_rel_med_ub=mwtp_blue_rel , ///
    by(length)
	

* Side-by-side x positions
gen x_keep = length - 0.15
gen x_rel  = length + 0.15

twoway ///
    (rcap mwtp_str_kp_lb mwtp_str_kp_ub x_keep if inlist(length, 18, 23, 28, 33, 38, 43), lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_str_kp_med x_keep if inlist(length, 18, 23, 28, 33, 38, 43), ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy)  connect(l) lcolor(navy)) ///
    (rcap mwtp_str_rel_lb mwtp_str_rel_ub x_rel if inlist(length, 18, 23, 28, 33, 38, 43), lwidth(medthin) lcol(maroon) ) ///
    (scatter mwtp_str_rel_med x_rel  if inlist(length, 18, 23, 28, 33, 38, 43), ///
        msymbol(circle) msize(small) mcolor(maroon) connect(l) lcolor(maroon)) , ///
    legend(order(2 "Harvestable striped bass" 4 "Released striped bass") rows(1) position(6) size(small) yoffset(-3) ) ///
    xtitle("Length (inches)", yoffset(-3) size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small) xoffset(-2)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
	title("") ///
    plotregion(color(white)) name(mwtp_keep_rel_m1, replace)
	
gr export "E:\Lou_projects\striper_bluefish_RDM\wtp_m0_cmmx_wts.pdf", replace

	
****************	
*mA
estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\survey_data_analysis\mA_cmmx.ster"
local cnames : colnames e(b)
display "`cnames'"

global params
qui forv x=1/5000{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }

        * generate draws from vector beta - sampling uncertainty
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		
		clear 
		set obs 1
		gen beta_cost=beta_draw[1,1]
		gen beta_str_kp_sm=beta_draw[10,1]
		gen beta_str_kp_md=beta_draw[11,1]
		gen beta_str_kp_tr=beta_draw[12,1]

		gen beta_str_rel_sm=beta_draw[7,1]
		gen beta_str_rel_md=beta_draw[8,1]
		gen beta_str_rel_tr=beta_draw[9,1]

		gen beta_blu_keep=beta_draw[13,1]
		gen beta_blu_rel=beta_draw[14,1]
		
		gen draw=`x'

        gen mwtp_str_kp_sm=-(beta_str_kp_sm) / (sqrt(1)*beta_cost)
        gen mwtp_str_kp_md= -(beta_str_kp_md) / (sqrt(1)*beta_cost)
        gen mwtp_str_kp_tr= -(beta_str_kp_tr) / (sqrt(1)*beta_cost)
	
        gen mwtp_str_rel_sm= -(beta_str_rel_sm) / (sqrt(1)*beta_cost)
        gen mwtp_str_rel_md= -(beta_str_rel_md) / (sqrt(1)*beta_cost)
        gen mwtp_str_rel_tr=  -(beta_str_rel_tr) / (sqrt(1)*beta_cost)
		 
	    gen mwtp_blue_keep=  -(beta_blu_keep ) / (sqrt(1)*beta_cost)
         gen mwtp_blue_rel= - (beta_blu_rel ) / (sqrt(1)*beta_cost)
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*

collapse ///
    (p50) kp_sm_med=mwtp_str_kp_sm kp_md_med=mwtp_str_kp_md kp_tr_med=mwtp_str_kp_tr ///
          rel_sm_med=mwtp_str_rel_sm rel_md_med=mwtp_str_rel_md rel_tr_med=mwtp_str_rel_tr ///
    (p5)  kp_sm_lb=mwtp_str_kp_sm kp_md_lb=mwtp_str_kp_md kp_tr_lb=mwtp_str_kp_tr ///
          rel_sm_lb=mwtp_str_rel_sm rel_md_lb=mwtp_str_rel_md rel_tr_lb=mwtp_str_rel_tr ///
    (p95) kp_sm_ub=mwtp_str_kp_sm kp_md_ub=mwtp_str_kp_md kp_tr_ub=mwtp_str_kp_tr ///
          rel_sm_ub=mwtp_str_rel_sm rel_md_ub=mwtp_str_rel_md rel_tr_ub=mwtp_str_rel_tr

gen id = 1
reshape long kp_sm_ kp_md_ kp_tr_ rel_sm_ rel_md_ rel_tr_, i(id) j(stat) string

rename kp_sm_  kp_sm
rename kp_md_  kp_md
rename kp_tr_  kp_tr
rename rel_sm_ rel_sm
rename rel_md_ rel_md
rename rel_tr_ rel_tr

reshape long kp_ rel_, i(stat) j(cat) string

rename kp_  mwtp_kp
rename rel_ mwtp_rel

gen stat_id = .
replace stat_id = 1 if stat == "lb"
replace stat_id = 2 if stat == "med"
replace stat_id = 3 if stat == "ub"

gen cat_id = .
replace cat_id = 1 if cat == "sm"
replace cat_id = 2 if cat == "md"
replace cat_id = 3 if cat == "tr"

drop stat
reshape wide mwtp_kp mwtp_rel, i(cat cat_id) j(stat_id)

rename mwtp_kp1  mwtp_kp_lb
rename mwtp_kp2  mwtp_kp_med
rename mwtp_kp3  mwtp_kp_ub

rename mwtp_rel1 mwtp_rel_lb
rename mwtp_rel2 mwtp_rel_med
rename mwtp_rel3 mwtp_rel_ub

label define catlbl 1 "schoolie" 2 "medium" 3 "trophy"
label values cat_id catlbl

* Compressed category spacing
gen x_cat = .
replace x_cat = 1.30 if cat_id == 1
replace x_cat = 2.00 if cat_id == 2
replace x_cat = 2.70 if cat_id == 3

gen x_keep = x_cat - 0.08
gen x_rel  = x_cat + 0.08

* Fake empty endpoints widen the x-axis
local xmin = 0.90
local xmax = 3.10


twoway ///
    (rcap mwtp_kp_lb mwtp_kp_ub x_keep, lcolor(navy)) ///
    (scatter mwtp_kp_med x_keep, msymbol(circle) mcolor(navy)) ///
    (rcap mwtp_rel_lb mwtp_rel_ub x_rel, lcolor(maroon)) ///
    (scatter mwtp_rel_med x_rel, msymbol(circle) mcolor(maroon)) , ///
    xscale(range(`xmin' `xmax')) ///
    xlabel(1.30 `"schoolie (18" & 23")"' 2.00 `"medium (28" & 33")"' 2.70 `"trophy (38" & 43")"', labsize(small)) ///
    legend(order(2 "Harvestable striped bass" 4 "Released striped bass") rows(1) position(6) size(small) yoffset(-3) ) ///
    xtitle("Length category", size(small) yoffset(-3) ) ///
    ytitle("Willingness-to-pay ($)", size(small) xoffset(-2)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    title("") ///
    name(mwtp_keep_rel_m2, replace)

gr export "E:\Lou_projects\striper_bluefish_RDM\mA_cmmx.pdf", replace

	
*mD
estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\survey_data_analysis\mD_cmmx_wts.ster"
local cnames : colnames e(b)
display "`cnames'"

global params
qui forv x=1/5000{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }

        * generate draws from vector beta - sampling uncertainty
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		
		clear 
		set obs 1
		gen beta_cost=beta_draw[1,1]
		gen beta_str_kp=beta_draw[10,1]
		gen beta_str_kp_L=beta_draw[2,1]
		gen beta_str_kp_L2=beta_draw[3,1]

		gen beta_str_rel=beta_draw[11,1]
		gen beta_str_rel_L=beta_draw[4,1]

		gen beta_blu_keep=beta_draw[12,1]
		gen beta_blu_rel=beta_draw[13,1]
		
		gen draw=`x'

	forv L=18/44 {
          gen mwtp_str_keep_`L'= -(beta_str_kp + beta_str_kp_L*`L' + beta_str_kp_L2*(`L'^2) ) / (sqrt(1)*beta_cost)
          gen mwtp_str_rel_`L'= -(beta_str_rel + beta_str_rel_L*log(`L' )) / (sqrt(1)*beta_cost)
		  gen mwtp_str_diff_`L' = mwtp_str_keep_`L'- mwtp_str_rel_`L' 

	}
      		
		  gen mwtp_blue_keep= -(beta_blu_keep ) / (sqrt(1)*beta_cost)
          gen mwtp_blue_rel= -(beta_blu_rel ) / (sqrt(1)*beta_cost)
		  		  
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_str_keep_ mwtp_str_rel_ mwtp_str_diff_, i(draw) j(new)

rename new length

collapse ///
    (p50) mwtp_str_kp_med = mwtp_str_keep_ ///
          mwtp_str_rel_med  = mwtp_str_rel_ ///
		  mwtp_str_diff_med  = mwtp_str_diff_ ///
		  mwtp_blu_kp_med=mwtp_blue_keep ///
		  mwtp_blu_rel_med=mwtp_blue_rel ///
    (p5) mwtp_str_kp_lb = mwtp_str_keep_ ///
           mwtp_str_rel_lb  = mwtp_str_rel_ ///
		   mwtp_str_diff_lb  = mwtp_str_diff_ ///
		   mwtp_blu_kp_lb=mwtp_blue_keep ///
		  mwtp_blu_rel_lb=mwtp_blue_rel ///
    (p95) mwtp_str_kp_ub = mwtp_str_keep_ ///
           mwtp_str_rel_ub  = mwtp_str_rel_ ///
		  mwtp_str_diff_ub  = mwtp_str_diff_ ///
		   mwtp_blu_kp_ub=mwtp_blue_keep ///
		  mwtp_blu_rel_ub=mwtp_blue_rel , ///
    by(length)
	

* Side-by-side x positions
gen x_keep = length - 0.15
gen x_rel  = length + 0.15

twoway ///
    (rcap mwtp_str_kp_lb mwtp_str_kp_ub x_keep if inlist(length, 18, 23, 28, 33, 38, 43),  lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_str_kp_med x_keep if inlist(length, 18, 23, 28, 33, 38, 43), ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy) connect(l) lcolor(navy)) ///
    (rcap mwtp_str_rel_lb mwtp_str_rel_ub x_rel if inlist(length, 18, 23, 28, 33, 38, 43), lwidth(medthin) lcol(maroon)) ///
    (scatter mwtp_str_rel_med x_rel if inlist(length, 18, 23, 28, 33, 38, 43), ///
        msymbol(circle) msize(small) mcolor(maroon) connect(l) lcolor(maroon)) , ///
    legend(order(2 "Harvestable striped bass" 4 "Released striped bass") rows(1) position(6) size(small) yoffset(-3) ) ///
    xtitle("Length (inches)", yoffset(-3) size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small) xoffset(-2)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
	title("") ///
    plotregion(color(white)) name(mwtp_keep_rel_m3, replace)
	
gr export "E:\Lou_projects\striper_bluefish_RDM\mD_cmmx_wts.pdf", replace


***WTP for changes in catch number
estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\survey_data_analysis\mD_cmmx_wts.ster"
local cnames : colnames e(b)
display "`cnames'"

* Choose representative striped bass lengths
local L_keep = 28
local L_rel  = 28

tempfile krdraws

postfile handle ///
    draw str20 species str20 outcome n mwtp ///
    using `krdraws', replace

set seed 03211990

qui forv x = 1/5000 {

    local K = e(k)
    mat bfull = e(b)
    mat b = bfull[1,1..`K']

    mat Vfull = e(V)
    mat Ve = Vfull[1..`K',1..`K']
    mat cholV = cholesky(Ve)

    mat iid_err = J(`K',1,0)
    forvalues i = 1/`K' {
        mat iid_err[`i',1] = rnormal()
    }

    mat beta_draw = b' + cholV*iid_err

    scalar beta_cost      = beta_draw[1,1]

    scalar beta_str_kp    = beta_draw[10,1]
    scalar beta_str_kp_L  = beta_draw[2,1]
    scalar beta_str_kp_L2 = beta_draw[3,1]

    scalar beta_str_rel   = beta_draw[11,1]
    scalar beta_str_rel_L = beta_draw[4,1]

    scalar beta_blu_kp    = beta_draw[12,1]
    scalar beta_blu_rel   = beta_draw[13,1]

    * Effective striped bass coefficients at selected length
    scalar b_str_kp_eff  = beta_str_kp  + beta_str_kp_L*`L_keep' + beta_str_kp_L2*(`L_keep'^2)
    scalar b_str_rel_eff = beta_str_rel + beta_str_rel_L*log(`L_rel')

    * Harvestable striped bass: N = 1 to 3
    forv n = 1/7 {
        scalar dsqrt = sqrt(`n') - sqrt(`n' - 1)
        scalar wtp =  -(b_str_kp_eff * dsqrt) / beta_cost
		*scalar wtp = - (b_str_kp_eff ) / (2*sqrt(`n')*beta_cost)
        post handle (`x') ("Striped bass") ("Harvestable") (`n') (wtp)
    }

    * Released striped bass: N = 1 to 7
    forv n = 1/7 {
        scalar dsqrt = sqrt(`n') - sqrt(`n' - 1)
        scalar wtp =  -(b_str_rel_eff * dsqrt) / beta_cost
		*scalar wtp = - (b_str_rel_eff) / (2*sqrt(`n')*beta_cost)
        post handle (`x') ("Striped bass") ("Released") (`n') (wtp)
    }

    * Harvestable bluefish: N = 1 to 7
    forv n = 1/7 {
        scalar dsqrt = sqrt(`n') - sqrt(`n' - 1)
        scalar wtp =  -(beta_blu_kp * dsqrt) / beta_cost
		*scalar wtp = - (beta_blu_kp ) / (2*sqrt(`n')*beta_cost)
        post handle (`x') ("Bluefish") ("Harvestable") (`n') (wtp)
    }

    * Released bluefish: N = 1 to 7
    forv n = 1/7 {
        scalar dsqrt = sqrt(`n') - sqrt(`n' - 1)
        scalar wtp = -(beta_blu_rel * dsqrt) / beta_cost
		*scalar wtp = - (beta_blu_rel ) / (2*sqrt(`n')*beta_cost)
        post handle (`x') ("Bluefish") ("Released") (`n') (wtp)
    }
}

postclose handle

use `krdraws', clear

collapse ///
    (p50) mwtp_med = mwtp ///
    (p5)  mwtp_lb  = mwtp ///
    (p95) mwtp_ub  = mwtp, ///
    by(species outcome n)

gen x = n
replace x = n - 0.10 if outcome == "Harvestable"
replace x = n + 0.10 if outcome == "Released"

twoway ///
    (rcap mwtp_lb mwtp_ub x if species == "Striped bass" & outcome == "Harvestable", ///
        lcolor(navy) lwidth(medthin)) ///
    (scatter mwtp_med x if species == "Striped bass" & outcome == "Harvestable", ///
        mcolor(navy) msymbol(circle) connect(l) lcolor(navy)) ///
    (rcap mwtp_lb mwtp_ub x if species == "Striped bass" & outcome == "Released", ///
        lcolor(maroon) lwidth(medthin)) ///
    (scatter mwtp_med x if species == "Striped bass" & outcome == "Released", ///
        mcolor(maroon) msymbol(circle) connect(l) lcolor(maroon)), ///
    legend(order(2 "Harvestable" 4 "Released") rows(1) position(6) size(small) yoffset(-3)) ///
    xtitle("N{superscript:th} striped bass", yoffset(-3) size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small) xoffset(-2)) ///
    xlabel(1(1)7) ///
	ylabel(#20) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
	xlabel(, labsize(small)) ///
	ylabel(, labsize(small)) ///
    title("") ///
    name(mwtp_striper_counts, replace)

gr export "E:\Lou_projects\striper_bluefish_RDM\mD_cmmx_wts_striper.pdf", replace
	
twoway ///
    (rcap mwtp_lb mwtp_ub x if species == "Bluefish" & outcome == "Harvestable", ///
        lcolor(navy) lwidth(medthin)) ///
    (scatter mwtp_med x if species == "Bluefish" & outcome == "Harvestable", ///
        mcolor(navy) msymbol(circle) connect(l) lcolor(navy)) ///
    (rcap mwtp_lb mwtp_ub x if species == "Bluefish" & outcome == "Released", ///
        lcolor(maroon) lwidth(medthin)) ///
    (scatter mwtp_med x if species == "Bluefish" & outcome == "Released", ///
        mcolor(maroon) msymbol(circle) connect(l) lcolor(maroon)), ///
    legend(order(2 "Harvestable" 4 "Released") rows(1) position(6) size(small) yoffset(-3)) ///
    xtitle("N{superscript:th} bluefish", yoffset(-3) size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small) xoffset(-2)) ///
    xlabel(1(1)7) ///
	ylabel(#20) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
	xlabel(, labsize(small)) ///
	ylabel(, labsize(small)) ///
    title("") ///
    name(mwtp_bluefish_counts, replace)

gr export "E:\Lou_projects\striper_bluefish_RDM\mD_cmmx_wts_blue.pdf", replace
	
	

***
*Heterogeity based on catch and release, primary mode, fishing skill
* only interact the striper variables 
*--------------------------------------------------
* 1. catch-and-release interaction
* non catch and release is the omitted category 
*--------------------------------------------------
u "$input_data_cd\striper_CE_data_formatted.dta", replace


* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 

preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
        alt1_choice_protest!=1 & alt2_choice_protest!=1  /// 
				& !missing(opt_other_likely) ///
				& !missing(opt_other_days_str_blue12) ///
				& !missing(opt_other_days_non_str_blue12) ///
				& !missing(opt_out_male) ///
				& !missing(opt_out_age) & !missing(striper_c_and_r_only)

* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

*cmset qtid scenario alt
sort qtid scenario alt

global ifsamp ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age, striper_c_and_r_only)

global max_options ///
    difficult intmethod(halton) intpoints(500) ///
    technique(bhhh 15 nr 5) iterate(25) ///
    nrtolerance(1e-3)

set seed 03211990

* common estimation sample
gen byte est_samp = $ifsamp

* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length

gen cr_only = striper_c_and_r_only

gen cr_sqrt_str_kp      = cr_only * sqrt_striper_keep
gen cr_sqrt_str_kp_L_c     = cr_only * sqrt_striper_keep_length_c
gen cr_sqrt_str_kp_L2_c     = cr_only * sqrt_striper_keep_length2_c

gen cr_sqrt_str_rel             = cr_only * sqrt_striper_rel
gen cr_sqrt_str_rel_log_L  = cr_only * sqrt_striper_rel_log_length_c

sort qtid scenario alt
set seed 03211990
/*
cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
	cr_sqrt_str_kp  cr_sqrt_str_kp_L_c cr_sqrt_str_kp_L2_c ///
	cr_sqrt_str_rel cr_sqrt_str_rel_log_L ///	
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options
	

estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo het1
estimates save het1, replace 
*/

* Recompute the same centering means used in estimation
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

sum log_striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\survey_data_analysis\het1.ster"
local cnames : colnames e(b)
display "`cnames'"

global params
qui forv x = 1/5000 {

    matrix bfull = e(b)
    local K = e(k)
    matrix b = bfull[1,1..`K']

    matrix Vfull = e(V)
    matrix Ve = Vfull[1..`K',1..`K']
    matrix cholV = cholesky(Ve)

    matrix iid_err = J(`K',1,0)
    forvalues i = 1/`K' {
        matrix iid_err[`i',1] = rnormal()
    }

    matrix beta_draw = b' + cholV*iid_err

    clear
    set obs 1

local cnames : colnames e(b)

local mean_cnames

foreach p of local cnames {
    if strpos("`p'", "sd(") == 0 {
        local mean_cnames `mean_cnames' `p'
    }
}

foreach p of local mean_cnames {
    local j = colnumb(bfull, "`p'")
    gen b_`p' = beta_draw[`j',1]
}
    gen draw = `x'

    forv L = 18/44 {

        local Lc    = `L' - `mean_keep_L'
        local Lc2   = (`L' - `mean_keep_L')^2
        local logLc = log(`L') - `mean_rel_logL'

        * omitted group: not catch-and-release only
        gen mwtp_keep_other_`L' = -  (b_sqrt_striper_keep  + b_sqrt_striper_keep_length_c*`Lc' + b_sqrt_striper_keep_length2_c*`Lc2')  / (sqrt(1)*b_cost)

        gen mwtp_rel_other_`L' =  - (b_sqrt_striper_rel   + b_sqrt_striper_rel_log_length_c*`logLc')   / (sqrt(1)*b_cost)

        * catch-and-release only group
        gen mwtp_keep_cr_`L' =    -(b_sqrt_striper_keep   + b_cr_sqrt_str_kp   + (b_sqrt_striper_keep_length_c + b_cr_sqrt_str_kp_L_c)*`Lc' ///
		+ (b_sqrt_striper_keep_length2_c + b_cr_sqrt_str_kp_L2_c)*`Lc2')   / (sqrt(1)*b_cost)

        gen mwtp_rel_cr_`L' =     -(b_sqrt_striper_rel  + b_cr_sqrt_str_rel + (b_sqrt_striper_rel_log_length_c + b_cr_sqrt_str_rel_log_L)*`logLc')  / (sqrt(1)*b_cost)
    }

    tempfile params`x'
    save `params`x'', replace
    global params "$params `params`x''"
}

clear
dsconcat $params
keep draw mwtp_*

reshape long ///
    mwtp_keep_other_ mwtp_rel_other_ ///
    mwtp_keep_cr_    mwtp_rel_cr_, ///
    i(draw) j(length)

rename mwtp_keep_other_ keep_other
rename mwtp_rel_other_  rel_other
rename mwtp_keep_cr_    keep_cr
rename mwtp_rel_cr_     rel_cr

reshape long keep_ rel_, i(draw length) j(group) string

rename keep_ mwtp_keep
rename rel_  mwtp_rel

collapse ///
    (p50) keep_med = mwtp_keep rel_med = mwtp_rel ///
    (p5)  keep_lb  = mwtp_keep rel_lb  = mwtp_rel ///
    (p95) keep_ub  = mwtp_keep rel_ub  = mwtp_rel, ///
    by(length group)

* Convert marginal WTP at N=1 to CV for the discrete change from 0 to 1 fish

gen x_keep = length - 0.15
gen x_rel  = length + 0.15

gen x_keep_cr = length - 0.15
gen x_keep_other = length + 0.15

gen x_rel_cr = length - 0.15
gen x_rel_other = length + 0.15

	

	
	

twoway ///
    (rcap keep_lb keep_ub x_keep_other if group=="other" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(black)) ///
    (scatter keep_med x_keep_other if group=="other" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(black) msymbol(circle) msize(small) connect(l) lcolor(black) ) ///
    (rcap keep_lb keep_ub x_keep_cr if group=="cr" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(gs12)) ///
    (scatter keep_med x_keep_cr if group=="cr" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(gs12) msymbol(circle) msize(small) connect(l) lcolor(gs12)) , ///
    legend(order(2 "sometimes/always harvest legal catch" 4 "catch-and-release only") ///
           rows(2) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small)) ///
	title("Harvestable striped bass", size(medium)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(0(25)200, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_cr_keep, replace)
	

twoway ///
    (rcap rel_lb rel_ub x_rel_other if group=="other" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(black)) ///
    (scatter rel_med x_rel_other if group=="other" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(black) msymbol(circle) msize(small)  connect(l) lcolor(black)) ///
    (rcap rel_lb rel_ub x_rel_cr if group=="cr" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(gs12) ) ///
    (scatter rel_med x_rel_cr if group=="cr" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(gs12)msymbol(circle) msize(small)  connect(l) lcolor(gs12)), ///
    legend(order(2 "sometimes/always harvest legal catch" 4 "catch-and-release only") ///
           rows(2) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small)) ///
	title("Released striped bass", size(medium)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(0(25)200, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_cr_rel, replace)
	
gr	combine mwtp_cr_keep mwtp_cr_rel, cols(2) ycommon 
gr export "E:\Lou_projects\striper_bluefish_RDM\mD_cmmx_wts_c_and_r.pdf", replace

*--------------------------------------------------
* 2. Fishing skill interaction 
*    Beginner is omitted/reference group
*--------------------------------------------------
/*
u "$input_data_cd\striper_CE_data_formatted.dta", replace


* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 

preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
        alt1_choice_protest!=1 & alt2_choice_protest!=1  /// 
				& !missing(opt_other_likely) ///
				& !missing(opt_other_days_str_blue12) ///
				& !missing(opt_other_days_non_str_blue12) ///
				& !missing(opt_out_male) ///
				& !missing(opt_out_age) & !missing(fishing_skill)

* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

cmset qtid scenario alt
sort qtid scenario alt

global ifsamp ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age, fishing_skill)

global max_options ///
    difficult intmethod(halton) intpoints(500) ///
    technique(bhhh 15 nr 5) iterate(25) ///
    nrtolerance(1e-3)

set seed 03211990

* common estimation sample
gen byte est_samp = $ifsamp

* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length


gen skill3 = .
replace skill3 = 1 if fishing_skill == 1
replace skill3 = 2 if fishing_skill == 2
replace skill3 = 3 if inlist(fishing_skill, 3, 4)

label define skill3lbl 1 "Beginner" 2 "Intermediate" 3 "Advanced/Professional"
label values skill3 skill3lbl

gen skill_intermediate = skill3 == 2
gen skill_advprof      = skill3 == 3

gen int_sqrt_str_kp = skill_intermediate * sqrt_striper_keep
gen int_sqrt_str_kp_L_c = skill_intermediate * sqrt_striper_keep_length_c
gen int_sqrt_str_kp_L2_c = skill_intermediate * sqrt_striper_keep_length2_c

gen adv_sqrt_str_kp = skill_advprof * sqrt_striper_keep
gen adv_sqrt_str_kp_L_c = skill_advprof * sqrt_striper_keep_length_c
gen adv_sqrt_str_kp_L2_c = skill_advprof * sqrt_striper_keep_length2_c

gen int_sqrt_str_rel             = skill_intermediate * sqrt_striper_rel
gen int_sqrt_str_rel_log_L  = skill_intermediate * sqrt_striper_rel_log_length_c

gen adv_sqrt_str_rel             = skill_advprof * sqrt_striper_rel
gen adv_sqrt_str_rel_log_L  = skill_advprof * sqrt_striper_rel_log_length_c

/*	
 cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
	int_sqrt_str_kp int_sqrt_str_kp_L_c int_sqrt_str_kp_L2_c ///
	adv_sqrt_str_kp adv_sqrt_str_kp_L_c adv_sqrt_str_kp_L2_c ///
	int_sqrt_str_rel  int_sqrt_str_rel_log_L ///
	adv_sqrt_str_rel  adv_sqrt_str_rel_log_L ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options	
	
estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo het2
estimates save het2, replace 
*/

***	

sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)


sum log_striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

estimates use mD_wts_c_skill.ster
local cnames : colnames e(b)
display "`cnames'"

global params
qui forv x = 1/5000 {

    matrix bfull = e(b)
    local K = e(k)
    matrix b = bfull[1,1..`K']

    matrix Vfull = e(V)
    matrix Ve = Vfull[1..`K',1..`K']
    matrix cholV = cholesky(Ve)

    matrix iid_err = J(`K',1,0)
    forvalues i = 1/`K' {
        matrix iid_err[`i',1] = rnormal()
    }

    matrix beta_draw = b' + cholV*iid_err

    clear
    set obs 1

local cnames : colnames e(b)

local mean_cnames

foreach p of local cnames {
    if strpos("`p'", "sd(") == 0 {
        local mean_cnames `mean_cnames' `p'
    }
}

foreach p of local mean_cnames {

    local j = colnumb(bfull, "`p'")

    gen b_`p' = beta_draw[`j',1]
}
    gen draw = `x'

    forv L = 18/44 {

        local Lc    = `L' - `mean_keep_L'
        local Lc2   = (`L' - `mean_keep_L')^2
        local logLc = log(`L') - `mean_rel_logL'

        * omitted group: not catch-and-release only
        gen mwtp_keep_beg_`L' =  - (b_sqrt_striper_keep  + b_sqrt_striper_keep_length_c*`Lc' + b_sqrt_striper_keep_length2_c*`Lc2')  / (2*sqrt(1)*b_cost)

        gen mwtp_rel_beg_`L' =  - (b_sqrt_striper_rel   + b_sqrt_striper_rel_log_length_c*`logLc')   / (2*sqrt(1)*b_cost)

        * intermediate group
        gen mwtp_keep_int_`L' =   - (b_sqrt_striper_keep   + b_int_sqrt_str_kp   + (b_sqrt_striper_keep_length_c + b_int_sqrt_str_kp_L_c)*`Lc' ///
		+ (b_sqrt_striper_keep_length2_c + b_int_sqrt_str_kp_L2_c)*`Lc2')   / (2*sqrt(1)*b_cost)

        gen mwtp_rel_int_`L' =    - (b_sqrt_striper_rel  + b_int_sqrt_str_rel + (b_sqrt_striper_rel_log_length_c + b_int_sqrt_str_rel_log_L)*`logLc')  / (2*sqrt(1)*b_cost)
		
		   * advanced group
        gen mwtp_keep_adv_`L' =   - (b_sqrt_striper_keep   + b_adv_sqrt_str_kp   + (b_sqrt_striper_keep_length_c + b_adv_sqrt_str_kp_L_c)*`Lc' ///
		+ (b_sqrt_striper_keep_length2_c + b_adv_sqrt_str_kp_L2_c)*`Lc2')   / (2*sqrt(1)*b_cost)

        gen mwtp_rel_adv_`L' =    - (b_sqrt_striper_rel  + b_adv_sqrt_str_rel + (b_sqrt_striper_rel_log_length_c + b_adv_sqrt_str_rel_log_L)*`logLc')  / (2*sqrt(1)*b_cost)
    }

    tempfile params`x'
    save `params`x'', replace
    global params "$params `params`x''"
}

clear
dsconcat $params
keep draw mwtp_*

reshape long ///
    mwtp_keep_beg_ mwtp_rel_beg_ ///
    mwtp_keep_int_    mwtp_rel_int_ ///
	mwtp_keep_adv_    mwtp_rel_adv_, ///
    i(draw) j(length)

rename mwtp_keep_beg_ keep_beginner
rename mwtp_rel_beg_  rel_beginner
rename mwtp_keep_int_    keep_int
rename mwtp_rel_int_     rel_int
rename mwtp_keep_adv_    keep_adv
rename mwtp_rel_adv_     rel_adv


reshape long keep_ rel_, i(draw length) j(group) string

rename keep_ mwtp_keep
rename rel_  mwtp_rel

collapse ///
    (p50) keep_med = mwtp_keep rel_med = mwtp_rel ///
    (p5)  keep_lb  = mwtp_keep rel_lb  = mwtp_rel ///
    (p95) keep_ub  = mwtp_keep rel_ub  = mwtp_rel, ///
    by(length group)


gen x_keep_beg = length - 0.2
gen x_keep_adv = length + 0.2

gen x_rel_begr = length - 0.2
gen x_rel_adv = length + 0.2



twoway ///
    (rcap keep_lb keep_ub x_keep_beg if group=="beginner"  &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(navy)) ///
    (scatter keep_med x_keep_beg if group=="beginner"  &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(navy) msymbol(circle) msize(small) connect(l) lcolor(navy) ) ///
	   (rcap keep_lb keep_ub length if group=="int"  &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(teal)) ///
    (scatter keep_med length if group=="int"  &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(teal) msymbol(circle) msize(small) connect(l) lcolor(teal) ) ///
    (rcap keep_lb keep_ub x_keep_adv if group=="adv"  &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(forest_green)) ///
    (scatter keep_med x_keep_adv if group=="adv" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(forest_green) msymbol(circle) msize(small) connect(l) lcolor(forest_green)) , ///
    legend(order(2 "beginner" 4 "intermediate" 6 "advanced") ///
           rows(1) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("WTP ($)", size(small)) ///
	title("WTP for harvestable striped bass", size(medium)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_skill_keep, replace)
	

twoway ///
    (rcap rel_lb rel_ub x_rel_beg if group=="beginner"  &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(maroon)) ///
    (scatter rel_med x_rel_beg if group=="beginner"  &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(maroon) msymbol(circle) msize(small) connect(l) lcolor(maroon)) ///
	   (rcap rel_lb rel_ub length if group=="int"  &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(cranberry)) ///
    (scatter rel_med length if group=="int"  &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(cranberry) msymbol(circle) msize(small) connect(l) lcolor(cranberry)) ///
    (rcap rel_lb rel_ub x_rel_adv if group=="adv" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(orange_red)) ///
    (scatter rel_med x_rel_adv if group=="adv" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(orange_red) msymbol(circle) msize(small) connect(l) lcolor(orange_red)) , ///
    legend(order(2 "beginner" 4 "intermediate" 6 "advanced") ///
           rows(1) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("WTP ($)", size(small)) ///
	title("WTP for released striped bass", size(medium)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_skill_rel, replace)
	
gr	combine mwtp_skill_keep mwtp_skill_rel, cols(2) ycommon

*/

*--------------------------------------------------
* 3. primary mode interaction 
  * Shore/kayak is the omitted/reference group
*--------------------------------------------------
u "$input_data_cd\striper_CE_data_formatted.dta", replace


* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 

preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
        alt1_choice_protest!=1 & alt2_choice_protest!=1  /// 
				& !missing(opt_other_likely) ///
				& !missing(opt_other_days_str_blue12) ///
				& !missing(opt_other_days_non_str_blue12) ///
				& !missing(opt_out_male) ///
				& !missing(opt_out_age) & !missing(mode_main)

* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

cmset qtid scenario alt
sort qtid scenario alt

global ifsamp ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age, mode_main)

global max_options ///
    difficult intmethod(halton) intpoints(500) ///
    technique(bhhh 15 nr 5) iterate(25) ///
    nrtolerance(1e-3)

set seed 03211990

* common estimation sample
gen byte est_samp = $ifsamp

* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length



gen main_mode3 = .
replace main_mode3 = 1 if inlist(mode_main, 1, 2)   // shore/surf + kayak/non-motorized
replace main_mode3 = 2 if mode_main == 3            // motorized boat
replace main_mode3 = 3 if inlist(mode_main, 4, 5)   // charter/guide + party/head

label define main_mode3lbl ///
    1 "Shore/kayak" ///
    2 "Motorized boat" ///
    3 "Charter/party/head"
label values main_mode3 main_mode3lbl

gen mode_motorized = main_mode3 == 2
gen mode_forhire   = main_mode3 == 3

gen pr_sqrt_str_kp = mode_motorized * sqrt_striper_keep
gen pr_sqrt_str_kp_L_c = mode_motorized * sqrt_striper_keep_length_c
gen pr_sqrt_str_kp_L2_c = mode_motorized * sqrt_striper_keep_length2_c

gen fh_sqrt_str_kp = mode_forhire * sqrt_striper_keep
gen fh_sqrt_str_kp_L_c = mode_forhire * sqrt_striper_keep_length_c
gen fh_sqrt_str_kp_L2_c = mode_forhire * sqrt_striper_keep_length2_c

gen pr_sqrt_str_rel             = mode_motorized * sqrt_striper_rel
gen pr_sqrt_str_rel_log_L  = mode_motorized * sqrt_striper_rel_log_length_c

gen fh_sqrt_str_rel             = mode_forhire * sqrt_striper_rel
gen fh_sqrt_str_rel_log_L  = mode_forhire * sqrt_striper_rel_log_length_c

/*
 cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
	pr_sqrt_str_kp  pr_sqrt_str_kp_L_c pr_sqrt_str_kp_L2_c ///
	pr_sqrt_str_rel  pr_sqrt_str_rel_log_L ///
	fh_sqrt_str_kp  fh_sqrt_str_kp_L_c fh_sqrt_str_kp_L2_c ///
	fh_sqrt_str_rel  fh_sqrt_str_rel_log_L ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options	
	
	
estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo het3
estimates save het3, replace 

 esttab het1 het2 het3 ///
 using "regression_results.csv", replace nobase noomitted ///
 stats(N_obs N_cases N_panels ll aic bic, fmt(0 0 0 3 3 3)) ///
 se b(%9.3f) label nogaps ///
 starlevels(* 0.10 ** 0.05 *** 0.010)
 */
 
 
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)


sum log_striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\survey_data_analysis\mD_wts_c_mode.ster"
local cnames : colnames e(b)
display "`cnames'"

global params
qui forv x = 1/5000 {

    matrix bfull = e(b)
    local K = e(k)
    matrix b = bfull[1,1..`K']

    matrix Vfull = e(V)
    matrix Ve = Vfull[1..`K',1..`K']
    matrix cholV = cholesky(Ve)

    matrix iid_err = J(`K',1,0)
    forvalues i = 1/`K' {
        matrix iid_err[`i',1] = rnormal()
    }

    matrix beta_draw = b' + cholV*iid_err

    clear
    set obs 1

local cnames : colnames e(b)

local mean_cnames

foreach p of local cnames {
    if strpos("`p'", "sd(") == 0 {
        local mean_cnames `mean_cnames' `p'
    }
}

foreach p of local mean_cnames {

    local j = colnumb(bfull, "`p'")

    gen b_`p' = beta_draw[`j',1]
}
    gen draw = `x'

    forv L = 18/44 {

        local Lc    = `L' - `mean_keep_L'
        local Lc2   = (`L' - `mean_keep_L')^2
        local logLc = log(`L') - `mean_rel_logL'

        * omitted group: not catch-and-release only
        gen mwtp_keep_shore_`L' =   -(b_sqrt_striper_keep  + b_sqrt_striper_keep_length_c*`Lc' + b_sqrt_striper_keep_length2_c*`Lc2')  / (sqrt(1)*b_cost)

        gen mwtp_rel_shore_`L' =  - (b_sqrt_striper_rel   + b_sqrt_striper_rel_log_length_c*`logLc')   / (sqrt(1)*b_cost)

        * intermediate group
        gen mwtp_keep_pr_`L' =  -  (b_sqrt_striper_keep   + b_pr_sqrt_str_kp   + (b_sqrt_striper_keep_length_c + b_pr_sqrt_str_kp_L_c)*`Lc' ///
		+ (b_sqrt_striper_keep_length2_c + b_pr_sqrt_str_kp_L2_c)*`Lc2')   / (sqrt(1)*b_cost)

        gen mwtp_rel_pr_`L' =  -   (b_sqrt_striper_rel  + b_pr_sqrt_str_rel + (b_sqrt_striper_rel_log_length_c + b_pr_sqrt_str_rel_log_L)*`logLc')  / (sqrt(1)*b_cost)
		
		   * advanced group
        gen mwtp_keep_fh_`L' = -   (b_sqrt_striper_keep   + b_fh_sqrt_str_kp   + (b_sqrt_striper_keep_length_c + b_fh_sqrt_str_kp_L_c)*`Lc' ///
		+ (b_sqrt_striper_keep_length2_c + b_fh_sqrt_str_kp_L2_c)*`Lc2')   / (sqrt(1)*b_cost)

        gen mwtp_rel_fh_`L' =   -  (b_sqrt_striper_rel  + b_fh_sqrt_str_rel + (b_sqrt_striper_rel_log_length_c + b_fh_sqrt_str_rel_log_L)*`logLc')  / (sqrt(1)*b_cost)
    }

    tempfile params`x'
    save `params`x'', replace
    global params "$params `params`x''"
}

clear
dsconcat $params
keep draw mwtp_*

reshape long ///
    mwtp_keep_shore_ mwtp_rel_shore_ ///
    mwtp_keep_pr_    mwtp_rel_pr_ ///
	mwtp_keep_fh_    mwtp_rel_fh_, ///
    i(draw) j(length)

rename mwtp_keep_shore_ keep_shore
rename mwtp_rel_shore_  rel_shore
rename mwtp_keep_pr_    keep_pr
rename mwtp_rel_pr_     rel_pr
rename mwtp_keep_fh_    keep_fh
rename mwtp_rel_fh_     rel_fh


reshape long keep_ rel_, i(draw length) j(group) string

rename keep_ mwtp_keep
rename rel_  mwtp_rel

collapse ///
    (p50) keep_med = mwtp_keep rel_med = mwtp_rel ///
    (p5)  keep_lb  = mwtp_keep rel_lb  = mwtp_rel ///
    (p95) keep_ub  = mwtp_keep rel_ub  = mwtp_rel, ///
    by(length group)


gen x_keep_shore = length - 0.2
gen x_keep_fh = length + 0.2

gen x_rel_shore = length - 0.2
gen x_rel_fh = length + 0.2



twoway ///
    (rcap keep_lb keep_ub x_keep_shore if group=="shore" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(black)) ///
    (scatter keep_med x_keep_shore if group=="shore" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(black) msymbol(circle) msize(small) connect(l) lcolor(black) ) ///
	 (rcap keep_lb keep_ub length if group=="pr" &  inlist(length, 18, 23, 28, 33, 38, 43),  lcolor(gs12)) ///
    (scatter keep_med length if group=="pr" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(gs12)  msymbol(circle) msize(small) connect(l) lcolor(gs12)) ///
    /*(rcap keep_lb keep_ub x_keep_fh if group=="fh", lcolor(purple)) ///
    (scatter keep_med x_keep_fh if group=="fh", mcolor(purple) msymbol(circle) msize(small)) */ , ///
    legend(order(2 "shore-based/kayak anglers" 4 "private boat anglers" ) ///
           rows(1) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small)) ///
	title("Harvestable striped bass", size(medium)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_mode_keep, replace)
	

twoway ///
    (rcap rel_lb rel_ub x_rel_shore if group=="shore" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(black)) ///
    (scatter rel_med x_rel_shore if group=="shore"&  inlist(length, 18, 23, 28, 33, 38, 43), mcolor(black) msymbol(circle) msize(small) connect(l) lcolor(black)) ///
	   (rcap rel_lb rel_ub length if group=="pr" &  inlist(length, 18, 23, 28, 33, 38, 43), lcolor(gs12)) ///
    (scatter rel_med length if group=="pr" &  inlist(length, 18, 23, 28, 33, 38, 43), mcolor( gs12 )  msymbol(circle) msize(small) connect(l) lcolor(gs12)) ///
   /* (rcap rel_lb rel_ub x_rel_fh if group=="fh", lcolor(purple)) ///
    (scatter rel_med x_rel_fh if group=="fh", mcolor(purple) msymbol(circle) msize(small)) */, ///
    legend(order(2 "shore-based/kayak anglers" 4 "private boat anglers" ) ///
           rows(1) position(6) size(small)) ///
    xtitle("Length (inches)", size(small)) ///
    ytitle("Willingness-to-pay ($)", size(small)) ///
	title("Released striped bass", size(medium)) ///
    xlabel(18(5)43, labsize(small)) ///
	ylabel(#20, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(mwtp_mode_rel, replace)
	
gr	combine mwtp_mode_keep mwtp_mode_rel, cols(2) ycommon 
gr export "E:\Lou_projects\striper_bluefish_RDM\mD_cmmx_wts_mode.pdf", replace






				
** Appendix specifications		
*no protest response adjustments 

* sample weighting  -state mode
use "$input_data_cd\striper_CE_data_formatted.dta", clear
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 


preserve 

keep if !missing(choice, cost, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age)
			 
* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

global ifsamp !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age)
			 

* common estimation sample
gen byte est_samp = $ifsamp

cmset qtid scenario alt
sort qtid scenario alt

global max_options ///
    difficult intmethod(halton) intpoints(500) ///
    technique(bhhh 15 nr 5) iterate(25) ///
    nrtolerance(1e-3)

set seed 03211990


* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length

*Estimation 
sort qtid scenario alt
set seed 03211990

cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age if est_samp ///
     [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options


estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	

eststo A1
estimates save A1, replace


**Protest only option 3 or 4 
use "$input_data_cd\striper_CE_data_formatted.dta", clear

* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 


preserve 
keep if !missing(choice, cost,  ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age) & no_choice_protest!=1 & alt3_choice_protest!=1 

* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

global ifsamp !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age) & no_choice_protest!=1 & alt3_choice_protest!=1 

* common estimation sample
gen byte est_samp = $ifsamp
 
cmset qtid scenario alt
sort qtid scenario alt


* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)

capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length

gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length

*Estimation
set seed 03211990

cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age if est_samp ///
     [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo A2
estimates save A2, replace 


*no survey weights
use "$input_data_cd\striper_CE_data_formatted.dta", clear
cmset qtid scenario alt

global ifsamp ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost,  ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age)

* common estimation sample
gen byte est_samp = $ifsamp

* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length


*Estimation 
sort qtid scenario alt
set seed 03211990

cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age if est_samp, ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo A3
estimates save A3, replace 



*all demographics
use "$input_data_cd\striper_CE_data_formatted.dta", clear


* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 



preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost,  ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other_likely, ///
			opt_other_days_str_blue12, ///
			opt_other_days_non_str_blue12, opt_out_male, opt_out_age,  ///
			opt_out_fishing_skill_3_4,  opt_out_educ_coll , opt_out_educ_grad, ///
			opt_out_inc_med, opt_out_inc_high)

				
* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

global ifsamp ///
     no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost, weight_state_md_norm,  ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other_likely, ///
			opt_other_days_str_blue12, ///
			opt_other_days_non_str_blue12, opt_out_male, opt_out_age,  ///
			opt_out_fishing_skill_3_4,  opt_out_educ_coll , opt_out_educ_grad, ///
			opt_out_inc_med, opt_out_inc_high)


* common estimation sample
gen byte est_samp = $ifsamp		 

cmset qtid scenario alt
sort qtid scenario alt

set seed 03211990

* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length

*Esimtation 
set seed 03211990

cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
	opt_out_fishing_skill_3_4  opt_out_educ_coll  opt_out_educ_grad ///
	opt_out_inc_med opt_out_inc_high ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo A4
estimates save A4, replace 


*addiditonal education variables
use "$input_data_cd\striper_CE_data_formatted.dta", clear

* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 


preserve
clear
input str5 st_mode target_share
"CT_fh"	    .000921
"CT_pr"		.0254005
"CT_sh"	.0259132
"DE_fh"		.0000171
"DE_pr"	.0045613
"DE_sh"	.0137618
"MA_fh"	.0027537
"MA_pr"	.0656039
"MA_sh"	.0895793
"MD_fh"	.0046252
"MD_pr"	.048189
"MD_sh"	.0243315
"ME_fh"	.0005505
"ME_pr"	.0207001
"ME_sh"	.0401165
"NC_fh" 	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"		.0001924
"NH_pr"		.0080706
"NH_sh"	.0138468
"NJ_fh"		.0014005
"NJ_pr"		.0780562
"NJ_sh"		.1227003
"NY_fh"		.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"		.0002559
"RI_pr"		.0174389
"RI_sh"		.0257576
"VA_fh"		.000365
"VA_pr"		.0169814
"VA_sh"	.0104897
end

tempfile targets
save `targets', replace
restore 

preserve 
keep if ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost,  ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age, ///
			 opt_out_educ_coll, ///
			 opt_out_educ_grad) 
				
* Keep one row per respondent (important!)
bysort qtid: keep if _n==1
contract st_mode
rename _freq n

egen total = total(n)
gen sample_share = n / total

merge 1:1 st_mode using `targets'

gen weight_state_md = target_share / sample_share
egen wsum = total(weight_state_md * n)
egen N = total(n)

gen weight_state_md_norm = weight_state_md * (N / wsum)

tempfile weights
keep st_mode weight_state_md_norm
save `weights', replace
restore 

merge m:1 st_mode using `weights', nogen


* estimation 
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

global ifsamp ///
    no_choice_protest!=1 & alt3_choice_protest!=1 & ///
    alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
    !missing(choice, cost, weight_state_md_norm, ///
             sqrt_striper_keep, sqrt_striper_rel, ///
             sqrt_bluefish_keep, sqrt_bluefish_rel, ///
             opt_other, opt_out, ///
             opt_other_likely, ///
             opt_other_days_str_blue12, ///
             opt_other_days_non_str_blue12, ///
             opt_out_male, opt_out_age, ///
			 opt_out_educ_coll, ///
			 opt_out_educ_grad) 

* common estimation sample
gen byte est_samp = $ifsamp
cmset qtid scenario alt
sort qtid scenario alt


* keep-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_L = r(mean)

* release-length center
sum striper_length if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_L = r(mean)

* release log-length center
gen log_striper_length_tmp = log(striper_length) if inlist(alt,1,2)
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_rel > 0, meanonly
local mean_rel_logL = r(mean)

* keep log-length center
sum log_striper_length_tmp if est_samp & inlist(alt,1,2) & striper_keep > 0, meanonly
local mean_keep_logL = r(mean)


capture drop c_striper_kp_length c_striper_kp_length2 ///
             c_striper_rl_length c_striper_rl_log_length ///
             sqrt_striper_keep_length_c ///
             sqrt_striper_keep_length2_c ///
             sqrt_striper_rel_length_c ///
             sqrt_striper_rel_log_length_c

gen c_striper_kp_length = striper_length - `mean_keep_L' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_length, mv(0) override

gen c_striper_kp_length2 = c_striper_kp_length^2

gen sqrt_striper_keep_length_c  = sqrt_striper_keep * c_striper_kp_length
gen sqrt_striper_keep_length2_c = sqrt_striper_keep * c_striper_kp_length2


gen c_striper_rl_length = striper_length - `mean_rel_L' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_length, mv(0) override

gen sqrt_striper_rel_length_c = sqrt_striper_rel * c_striper_rl_length


gen c_striper_rl_log_length = log(striper_length) - `mean_rel_logL' ///
    if inlist(alt,1,2) & striper_rel > 0
mvencode c_striper_rl_log_length, mv(0) override

gen sqrt_striper_rel_log_length_c = sqrt_striper_rel * c_striper_rl_log_length

gen c_striper_kp_log_length = log(striper_length) - `mean_keep_logL' ///
    if inlist(alt,1,2) & striper_keep > 0
mvencode c_striper_kp_log_length, mv(0) override

gen sqrt_striper_kp_log_length_c = sqrt_striper_keep * c_striper_kp_log_length


*Estimation 
eststo mD_cmmx_educ_c: cmxtmixlogit choice cost ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
	opt_out_educ_coll  opt_out_educ_grad ///
    if est_samp [pweight = weight_state_md_norm], ///
    random(sqrt_striper_keep sqrt_striper_rel ///
           sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    noconstant $max_options

estadd scalar N_obs = e(N), replace
estadd scalar N_cases = e(N_case), replace
estadd scalar N_panels = e(N_panel), replace

estadd scalar aic = -2*e(ll) + 2*e(k), replace
estadd scalar bic = -2*e(ll) + log(e(N))*e(k), replace	


eststo A5
estimates save A5, replace 

 esttab A1 A2 A3 A4 A5 ///
 using "regression_results.csv", replace nobase noomitted ///
 stats(N_obs N_cases N_panels ll aic bic, fmt(0 0 0 3 3 3)) ///
 se b(%9.3f) label nogaps ///
 starlevels(* 0.10 ** 0.05 *** 0.010)
 