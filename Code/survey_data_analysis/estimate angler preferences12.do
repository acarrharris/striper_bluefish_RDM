
global input_data_cd "E:\Lou_projects\striper_bluefish_RDM\input_data" /* Lou's local data path */

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
	gen sqrt_keep_`L'=sqrt(keep_`L')
	gen sqrt_rel_`L'=sqrt(rel_`L')
	gen sqrt_catch_`L'=sqrt(catch_`L')
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



* model 3 and model 4 with sample weights

* sample weighting  -state mode
gen main_mode_str = ""
replace main_mode_str = "sh" if inlist(mode_main, 1)   // shore/surf 
replace main_mode_str = "pr" if inlist(mode_main, 2, 3)         // motorized boat + kayak/non-motorized
replace main_mode_str = "fh" if inlist(mode_main, 4, 5)   // charter/guide + party/head
gen st_mode=lic_st+"_"+main_mode_str 

preserve
clear
input str5 st_mode target_share
"CT_fh"	.000921
"CT_pr"	.0254005
"CT_sh"	.0259132
"DE_fh"	.0000171
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
"NC_fh"	.0021628
"NC_pr"	.0264514
"NC_sh"	.0761711
"NH_fh"	.0001924
"NH_pr"	.0080706
"NH_sh"	.0138468
"NJ_fh"	.0014005
"NJ_pr"	.0780562
"NJ_sh"	.1227003
"NY_fh"	.0020241
"NY_pr"	.0849696
"NY_sh"	.145641
"RI_fh"	.0002559
"RI_pr"	.0174389
"RI_sh"	.0257576
"VA_fh"	.000365
"VA_pr"	.0169814
"VA_sh"	.0104897
end


tempfile targets
save `targets', replace
restore 

preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
        alt1_choice_protest!=1 & alt2_choice_protest!=1

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


cmset qtid scenario alt
eststo m3_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_kp_18_33_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep_18_33 sqrt_striper_keep_38_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m3_cmmx_wts, replace




eststo m4_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12  opt_out_male opt_out_age ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m4_cmmx_wts, replace

esttab m1_cmmx m2_cmmx m3_cmmx m3_cmmx_wts  m4_cmmx  m4_cmmx_wts  m5_cmmx m6_cmmx using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  			
				
				
*Re-estimate final specs using centering of length
clogit choice cost  ///
				sqrt_kp_18_38_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12  opt_out_male opt_out_age  sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid)
					
summ striper_length if inlist(alt,1,2) & e(sample) & striper_keep>0
gen log_length_keep_c = log(striper_length) - log(r(mean)) if inlist(alt,1,2) & e(sample) & striper_keep>0
mvencode log_length_keep_c, mv(0)
sort qtid scenario alt
gen sqrt_kp_18_38_log_length_c = sqrt_striper_keep_18_38 * log_length_keep_c

summ striper_length if inlist(alt,1,2) & e(sample) & striper_rel>0
gen log_length_rel_c = log(striper_length) - log(r(mean)) if inlist(alt,1,2) & e(sample) & striper_rel>0
mvencode log_length_rel_c, mv(0)
gen sqrt_rel_log_length_c = sqrt_striper_rel * log_length_rel_c

					
eststo m4_cmmx_c: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_log_length_c  ///
				sqrt_rel_log_length_c ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12  opt_out_male opt_out_age ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant
estimates save m4_cmmx_c, replace 

eststo m4_cmmx_wts_c: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_log_length_c  ///
				sqrt_rel_log_length_c ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12  opt_out_male opt_out_age ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant				
estimates save m4_cmmx_wts_c, replace 
	
	
	
esttab m4_cmmx m4_cmmx_c m4_cmmx_wts m4_cmmx_wts_c using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  			
				

				

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m4_cmmx_wts.ster"
local cnames : colnames e(b)
display "`cnames'"


1. cost 
2. sqrt_kp_18_38_log_length 
3. sqrt_striper_rel_log_length 
4. opt_other_likely_to_fish 
5. opt_other_days_str_blue12 
6. opt_other_days_non_str_blue12 
7. opt_out_male 
8. opt_out_age 
9. sqrt_striper_keep_18_38 
10. sqrt_keep_43 
11. sqrt_striper_rel 
12. sqrt_bluefish_keep 
13. sqrt_bluefish_rel 
14. opt_other 
15. opt_out 
16. sd(sqrt_striper_keep_18_38) 
17. sd(sqrt_keep_43) 
18. sd(sqrt_striper_rel) 
19. sd(sqrt_bluefish_keep) 
20. sd(sqrt_bluefish_rel) 
21. sd(opt_other) 
22. sd(opt_out)

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m3_cmmx.ster"
local cnames : colnames e(b)
display "`cnames'"
global params
forv x=1/1000{
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
		gen beta_str_kp_sm=beta_draw[9,1]
		*gen beta_str_kp_sm=rnormal(beta_draw[9,1], abs(beta_draw[16,1]))

		gen beta_str_kp_sm_L=beta_draw[2,1]

		gen beta_str_kp_tr=beta_draw[10,1]
		*gen beta_str_kp_tr=rnormal(beta_draw[10,1], abs(beta_draw[17,1]))

		gen beta_str_rel=beta_draw[11,1]
		*gen beta_str_rel=rnormal(beta_draw[11,1],  abs(beta_draw[18,1]))

		gen beta_str_rel_L=beta_draw[3,1]

		gen beta_blu_keep=beta_draw[12,1]
		*gen beta_blu_keep=rnormal(beta_draw[12,1],  abs(beta_draw[19,1]))

		gen beta_blu_rel=beta_draw[13,1]
		*gen beta_blu_rel=rnormal(beta_draw[13,1], abs(beta_draw[20,1]))

		gen beta_optout=beta_draw[15,1]
		*gen beta_optout=rnormal(beta_draw[15,1], abs(beta_draw[22,1]))

		gen beta_optoth=beta_draw[14,1]
		*gen beta_optoth=rnormal(beta_draw[14,1], abs(beta_draw[21,1]))

		gen beta_optout_male=beta_draw[7,1]
		gen beta_optout_age=beta_draw[8,1]
		gen beta_optoth_likely=beta_draw[4,1]
		gen beta_optoth_days_str=beta_draw[5,1]
		gen beta_optoth_days_non_str=beta_draw[6,1]
			
		gen draw=`x'

	forv L=18/43 {
        local logL = log(( `L'))

        * ---- KEEP ----
            if `L' < 38 {
              gen mwtp_keep_`L'= - ( beta_str_kp_sm + beta_str_kp_sm_L*`logL' ) / (2*sqrt(1)*beta_cost)
            }
			
            else {
                gen mwtp_keep_`L'= - ( beta_str_kp_tr ) / (2*sqrt(1)*beta_cost)
            }


		gen mwtp_rel_`L'= - (  beta_str_rel + beta_str_rel_L*`logL' ) / (2*sqrt(1)*beta_cost)
	}
      		
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_keep_ mwtp_rel_, i(draw) j(new)

rename new length

* Optional: collapse 41, 42, 43 into one 40+ bin
gen length_bin = length
replace length_bin = 38 if inlist(length, 38, 39, 40, 41, 42, 43)


collapse ///
    (p50) mwtp_keep_med = mwtp_keep_ ///
          mwtp_rel_med  = mwtp_rel_ ///
    (p5) mwtp_keep_lb = mwtp_keep_ ///
           mwtp_rel_lb  = mwtp_rel_ ///
    (p95) mwtp_keep_ub = mwtp_keep_ ///
           mwtp_rel_ub  = mwtp_rel_, ///
    by(length_bin)
	
	

* Side-by-side x positions
gen x_keep = length_bin - 0.15
gen x_rel  = length_bin + 0.15

twoway ///
    (rcap mwtp_keep_lb mwtp_keep_ub x_keep, ///
        lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_keep_med x_keep, ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy)) ///
    (rcap mwtp_rel_lb mwtp_rel_ub x_rel, ///
        lwidth(medthin) lcol(maroon)) ///
    (scatter mwtp_rel_med x_rel, ///
        msymbol(circle) msize(small) mcolor(maroon)) , ///
    legend(order(2 "Kept" 4 "Released") rows(1) position(6)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18(2)43, labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) name(mwtp_keep_rel_logL, replace)
	
****************	

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m2_cmmx.ster"
local cnames : colnames e(b)
display "`cnames'"

global params
forv x=1/1000{
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
		gen beta_str_kp_sm=beta_draw[9,1]
		*gen beta_str_kp_sm=rnormal(beta_draw[9,1], abs(beta_draw[16,1]))

		gen beta_str_kp_sm_L=beta_draw[2,1]

		gen beta_str_kp_tr=beta_draw[10,1]
		*gen beta_str_kp_tr=rnormal(beta_draw[10,1], abs(beta_draw[17,1]))

		gen beta_str_rel=beta_draw[11,1]
		*gen beta_str_rel=rnormal(beta_draw[11,1],  abs(beta_draw[18,1]))

		gen beta_str_rel_L=beta_draw[3,1]

		gen beta_blu_keep=beta_draw[12,1]
		*gen beta_blu_keep=rnormal(beta_draw[12,1],  abs(beta_draw[19,1]))

		gen beta_blu_rel=beta_draw[13,1]
		*gen beta_blu_rel=rnormal(beta_draw[13,1], abs(beta_draw[20,1]))

		gen beta_optout=beta_draw[15,1]
		*gen beta_optout=rnormal(beta_draw[15,1], abs(beta_draw[22,1]))

		gen beta_optoth=beta_draw[14,1]
		*gen beta_optoth=rnormal(beta_draw[14,1], abs(beta_draw[21,1]))

		gen beta_optout_male=beta_draw[7,1]
		gen beta_optout_age=beta_draw[8,1]
		gen beta_optoth_likely=beta_draw[4,1]
		gen beta_optoth_days_str=beta_draw[5,1]
		gen beta_optoth_days_non_str=beta_draw[6,1]
			
		gen draw=`x'

	forv L=18/43 {
        local logL = ( `L')

        * ---- KEEP ----
            if `L' < 40 {
              gen mwtp_keep_`L'= - ( beta_str_kp_sm + beta_str_kp_sm_L*`logL' ) / (2*sqrt(1)*beta_cost)
            }
			
            else {
                gen mwtp_keep_`L'= - ( beta_str_kp_tr ) / (2*sqrt(1)*beta_cost)
            }


		gen mwtp_rel_`L'= - (  beta_str_rel + beta_str_rel_L*`logL' ) / (2*sqrt(1)*beta_cost)
	}
      		
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_keep_ mwtp_rel_, i(draw) j(new)

rename new length

* Optional: collapse 41, 42, 43 into one 40+ bin
gen length_bin = length
replace length_bin = 40 if inlist(length, 40, 41, 42, 43)


collapse ///
    (p50) mwtp_keep_med = mwtp_keep_ ///
          mwtp_rel_med  = mwtp_rel_ ///
    (p5) mwtp_keep_lb = mwtp_keep_ ///
           mwtp_rel_lb  = mwtp_rel_ ///
    (p95) mwtp_keep_ub = mwtp_keep_ ///
           mwtp_rel_ub  = mwtp_rel_, ///
    by(length_bin)
	
	

* Side-by-side x positions
gen x_keep = length_bin - 0.15
gen x_rel  = length_bin + 0.15

twoway ///
    (rcap mwtp_keep_lb mwtp_keep_ub x_keep, ///
        lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_keep_med x_keep, ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy)) ///
    (rcap mwtp_rel_lb mwtp_rel_ub x_rel, ///
        lwidth(medthin) lcol(maroon)) ///
    (scatter mwtp_rel_med x_rel, ///
        msymbol(circle) msize(small) mcolor(maroon)) , ///
    legend(order(2 "Kept" 4 "Released") rows(1) position(6)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18(2)38 40 "40+", labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) name(mwtp_keep_rel_L)
	
	
****************	
	
	
****************	

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m6_cmmx.ster"
local cnames : colnames e(b)
display "`cnames'"
global params
forv x=1/1000{
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
		gen beta_str_kp_sm=beta_draw[9,1]
		*gen beta_str_kp_sm=rnormal(beta_draw[9,1], abs(beta_draw[16,1]))

		gen beta_str_kp_sm_L=beta_draw[2,1]

		gen beta_str_kp_tr=beta_draw[10,1]
		*gen beta_str_kp_tr=rnormal(beta_draw[10,1], abs(beta_draw[17,1]))

		gen beta_str_rel=beta_draw[11,1]
		*gen beta_str_rel=rnormal(beta_draw[11,1],  abs(beta_draw[18,1]))

		gen beta_str_rel_L=beta_draw[3,1]

		gen beta_blu_keep=beta_draw[12,1]
		*gen beta_blu_keep=rnormal(beta_draw[12,1],  abs(beta_draw[19,1]))

		gen beta_blu_rel=beta_draw[13,1]
		*gen beta_blu_rel=rnormal(beta_draw[13,1], abs(beta_draw[20,1]))

		gen beta_optout=beta_draw[15,1]
		*gen beta_optout=rnormal(beta_draw[15,1], abs(beta_draw[22,1]))

		gen beta_optoth=beta_draw[14,1]
		*gen beta_optoth=rnormal(beta_draw[14,1], abs(beta_draw[21,1]))

		gen beta_optout_male=beta_draw[7,1]
		gen beta_optout_age=beta_draw[8,1]
		gen beta_optoth_likely=beta_draw[4,1]
		gen beta_optoth_days_str=beta_draw[5,1]
		gen beta_optoth_days_non_str=beta_draw[6,1]
			
		gen draw=`x'

	forv L=18/43 {
        local logL = sqrt(( `L'))

        * ---- KEEP ----
            if `L' < 40 {
              gen mwtp_keep_`L'= - ( beta_str_kp_sm + beta_str_kp_sm_L*`logL' ) / (2*sqrt(1)*beta_cost)
            }
			
            else {
                gen mwtp_keep_`L'= - ( beta_str_kp_tr ) / (2*sqrt(1)*beta_cost)
            }


		gen mwtp_rel_`L'= - (  beta_str_rel + beta_str_rel_L*`logL' ) / (2*sqrt(1)*beta_cost)
	}
      		
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_keep_ mwtp_rel_, i(draw) j(new)

rename new length

* Optional: collapse 41, 42, 43 into one 40+ bin
gen length_bin = length
replace length_bin = 40 if inlist(length, 40, 41, 42, 43)


collapse ///
    (p50) mwtp_keep_med = mwtp_keep_ ///
          mwtp_rel_med  = mwtp_rel_ ///
    (p5) mwtp_keep_lb = mwtp_keep_ ///
           mwtp_rel_lb  = mwtp_rel_ ///
    (p95) mwtp_keep_ub = mwtp_keep_ ///
           mwtp_rel_ub  = mwtp_rel_, ///
    by(length_bin)
	
	

* Side-by-side x positions
gen x_keep = length_bin - 0.15
gen x_rel  = length_bin + 0.15

twoway ///
    (rcap mwtp_keep_lb mwtp_keep_ub x_keep, ///
        lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_keep_med x_keep, ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy)) ///
    (rcap mwtp_rel_lb mwtp_rel_ub x_rel, ///
        lwidth(medthin) lcol(maroon)) ///
    (scatter mwtp_rel_med x_rel, ///
        msymbol(circle) msize(small) mcolor(maroon)) , ///
    legend(order(2 "Kept" 4 "Released") rows(1) position(6)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18(2)38 40 "40+", labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) name(mwtp_keep_rel_sqrtL, replace)
	
	
****************			


****************	

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m8_cmmx.ster"
local cnames : colnames e(b)
display "`cnames'"
global params
forv x=1/1000{
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
		gen beta_str_kp_sm=beta_draw[9,1]
		*gen beta_str_kp_sm=rnormal(beta_draw[9,1], abs(beta_draw[16,1]))

		gen beta_str_kp_sm_L=beta_draw[2,1]

		gen beta_str_kp_tr=beta_draw[10,1]
		*gen beta_str_kp_tr=rnormal(beta_draw[10,1], abs(beta_draw[17,1]))

		gen beta_str_rel=beta_draw[11,1]
		*gen beta_str_rel=rnormal(beta_draw[11,1],  abs(beta_draw[18,1]))

		gen beta_str_rel_L=beta_draw[3,1]

		gen beta_blu_keep=beta_draw[12,1]
		*gen beta_blu_keep=rnormal(beta_draw[12,1],  abs(beta_draw[19,1]))

		gen beta_blu_rel=beta_draw[13,1]
		*gen beta_blu_rel=rnormal(beta_draw[13,1], abs(beta_draw[20,1]))

		gen beta_optout=beta_draw[15,1]
		*gen beta_optout=rnormal(beta_draw[15,1], abs(beta_draw[22,1]))

		gen beta_optoth=beta_draw[14,1]
		*gen beta_optoth=rnormal(beta_draw[14,1], abs(beta_draw[21,1]))

		gen beta_optout_male=beta_draw[7,1]
		gen beta_optout_age=beta_draw[8,1]
		gen beta_optoth_likely=beta_draw[4,1]
		gen beta_optoth_days_str=beta_draw[5,1]
		gen beta_optoth_days_non_str=beta_draw[6,1]
			
		gen draw=`x'

	forv L=18/43 {
        local logL = ( `L')^2

        * ---- KEEP ----
            if `L' < 40 {
              gen mwtp_keep_`L'= - ( beta_str_kp_sm + beta_str_kp_sm_L*`logL' ) / (2*sqrt(1)*beta_cost)
            }
			
            else {
                gen mwtp_keep_`L'= - ( beta_str_kp_tr ) / (2*sqrt(1)*beta_cost)
            }


		gen mwtp_rel_`L'= - (  beta_str_rel + beta_str_rel_L*`logL' ) / (2*sqrt(1)*beta_cost)
	}
      		
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_keep_ mwtp_rel_, i(draw) j(new)

rename new length

* Optional: collapse 41, 42, 43 into one 40+ bin
gen length_bin = length
replace length_bin = 40 if inlist(length, 40, 41, 42, 43)


collapse ///
    (p50) mwtp_keep_med = mwtp_keep_ ///
          mwtp_rel_med  = mwtp_rel_ ///
    (p5) mwtp_keep_lb = mwtp_keep_ ///
           mwtp_rel_lb  = mwtp_rel_ ///
    (p95) mwtp_keep_ub = mwtp_keep_ ///
           mwtp_rel_ub  = mwtp_rel_, ///
    by(length_bin)
	
	

* Side-by-side x positions
gen x_keep = length_bin - 0.15
gen x_rel  = length_bin + 0.15

twoway ///
    (rcap mwtp_keep_lb mwtp_keep_ub x_keep, ///
        lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_keep_med x_keep, ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy)) ///
    (rcap mwtp_rel_lb mwtp_rel_ub x_rel, ///
        lwidth(medthin) lcol(maroon)) ///
    (scatter mwtp_rel_med x_rel, ///
        msymbol(circle) msize(small) mcolor(maroon)) , ///
    legend(order(2 "Kept" 4 "Released") rows(1) position(6)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18(2)38 40 "40+", labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) name(mwtp_keep_rel_L2)
	
	
****************			


****************	

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data\m1_xtmix.ster"
local cnames : colnames e(b)
display "`cnames'" "    "
global params
forv x=1/1000{
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
		gen beta_str_kp=beta_draw[11,1]

		gen beta_str_kp_L=beta_draw[2,1]
		gen beta_str_kp_L2=beta_draw[3,1]

		
		gen beta_str_rel=beta_draw[12,1]
		*gen beta_str_rel=rnormal(beta_draw[11,1],  abs(beta_draw[18,1]))

		gen beta_str_rel_L=beta_draw[4,1]

			
		gen draw=`x'

	forv L=18/44 {
        local logL = log(`L')

        * ---- KEEP ----
 
              gen mwtp_keep_`L'= - ( beta_str_kp + beta_str_kp_L*`L' +beta_str_kp_L2*(`L'^2)) / (2*sqrt(1)*beta_cost)
            

		gen mwtp_rel_`L'= - (  beta_str_rel + beta_str_rel_L*`logL' ) / (2*sqrt(1)*beta_cost)
	}
      		
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params
keep draw mwt*
reshape long mwtp_keep_ mwtp_rel_, i(draw) j(new)

rename new length



collapse ///
    (p50) mwtp_keep_med = mwtp_keep_ ///
          mwtp_rel_med  = mwtp_rel_ ///
    (p5) mwtp_keep_lb = mwtp_keep_ ///
           mwtp_rel_lb  = mwtp_rel_ ///
    (p95) mwtp_keep_ub = mwtp_keep_ ///
           mwtp_rel_ub  = mwtp_rel_, ///
    by(length)
	
	

* Side-by-side x positions
gen x_keep = length - 0.15
gen x_rel  = length + 0.15

twoway ///
    (rcap mwtp_keep_lb mwtp_keep_ub x_keep, ///
        lwidth(medthin) lcol(navy)) ///
    (scatter mwtp_keep_med x_keep, ///
        msymbol(circle) msize(small) lcol(navy) mcolor(navy)) ///
    (rcap mwtp_rel_lb mwtp_rel_ub x_rel, ///
        lwidth(medthin) lcol(maroon)) ///
    (scatter mwtp_rel_med x_rel, ///
        msymbol(circle) msize(small) mcolor(maroon)) , ///
    legend(order(2 "Kept" 4 "Released") rows(1) position(6)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18(2)44 , labsize(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) name(mwtp_keep_rel_L_L2, replace)
	
	
****************			