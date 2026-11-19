

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

eststo m0_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_length  ///
				sqrt_striper_rel_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep sqrt_striper_rel ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant
estimates save m0_cmmx_wts, replace 



eststo mC_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep sqrt_striper_rel ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant
estimates save mC_cmmx_wts, replace 


eststo mD_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep sqrt_striper_rel ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant
estimates save mD_cmmx_wts, replace 



eststo mE_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_sqrt_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep sqrt_striper_rel ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant 
estimates save mE_cmmx_wts, replace 


global max_options difficult intmethod(halton) intpoints(500) technique(bhhh 15 nr 5) iterate(25) nrtolerance(1e-3) 
eststo mA_cmmx_wts: cmxtmixlogit choice cost  ///
				sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
				sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random( sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant $max_options
estimates save mA_cmmx_wts, replace 




eststo mA_cmmx: cmxtmixlogit choice cost  ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
								sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant $max_options
	
	
estimates save mA_cmmx, replace 


eststo mA_linear_cmmx_wts: cmxtmixlogit choice cost  ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
								striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 ///
								bluefish_keep bluefish_rel opt_other opt_out) noconstant $max_options
	
	
estimates save mA_linear_cmmx_wts, replace 


eststo mA_linear_cmmx: cmxtmixlogit choice cost  ///
				striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 ///
				striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(bluefish_keep bluefish_rel opt_other opt_out) noconstant $max_options

		
clogit choice cost  ///
								sqrt_striper_rel_18 ///
								sqrt_striper_rel_23 ///
								sqrt_striper_rel_28 ///
								sqrt_striper_rel_33 ///
								sqrt_striper_rel_38 ///
								sqrt_striper_rel_43 ///
								sqrt_striper_keep_18 ///
								sqrt_striper_keep_23 ///
								sqrt_striper_keep_28 ///
								sqrt_striper_keep_33 ///
								sqrt_striper_keep_38 ///
								sqrt_striper_keep_43 ///
								bluefish_keep bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
								if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1  [pweight = weight_state_md_norm], group(gid)

								
clogit choice cost  ///
					sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
					sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
								if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1  [pweight = weight_state_md_norm], group(gid)
								
eststo mA_sample: clogit choice cost  ///
					sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
					sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age , group(gid)
				
eststo mB_sample: clogit choice cost  ///
					sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
					sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
								if no_choice_protest!=1 , group(gid)

eststo mC_sample: clogit choice cost  ///
					sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
					sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
								if no_choice_protest!=1 & alt3_choice_protest!=1 , group(gid)				
								
eststo mD_sample: clogit choice cost  ///
					sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
					sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
								if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 , group(gid)

eststo mE_sample: clogit choice cost  ///
					sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 ///
					sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
								if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)

								
esttab mA_sample mB_sample mC_sample mD_sample  mE_sample  using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  			
				
				
estimates save mA_linear_cmmx, replace 


eststo mB_cmmx_wts: cmxtmixlogit choice cost  ///
								sqrt_striper_rel_18 ///
								sqrt_striper_rel_23 ///
								sqrt_striper_rel_28 ///
								sqrt_striper_rel_33 ///
								sqrt_striper_rel_38 ///
								sqrt_striper_rel_43 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(sqrt_striper_keep_18 ///
								sqrt_striper_keep_23 ///
								sqrt_striper_keep_28 ///
								sqrt_striper_keep_33 ///
								sqrt_striper_keep_38 ///
								sqrt_striper_keep_43 ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant $max_options
estimates save mB_cmmx_wts, replace 
estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim\mB_cmmx_wts.ster"



eststo mB_linear_cmmx_wts: cmxtmixlogit choice cost  ///
				rel_18 rel_23 rel_28 rel_33 rel_38 rel_43 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
					random(keep_18 keep_23 keep_28 keep_33 keep_38 keep_43 ///
								sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant $max_options
estimates save mB_cmmx_wts, replace 



