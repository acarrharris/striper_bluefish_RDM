

set seed 03211990

u "$input_data_cd\striper_CE_data.dta", clear 

gen striper_rel_18_23=striper_rel if inlist(striper_length, 18, 23) & striper_rel>0
gen striper_rel_28_33=striper_rel if inlist(striper_length, 28, 33) & striper_rel>0
gen striper_rel_38_43=striper_rel if inlist(striper_length, 38, 43) & striper_rel>0

gen striper_keep_18_23=striper_keep if inlist(striper_length, 18, 23) & striper_keep>0
gen striper_keep_28_33=striper_keep if inlist(striper_length, 28, 33) & striper_keep>0
gen striper_keep_38_43=striper_keep if inlist(striper_length, 38, 43) & striper_keep>0

mvencode striper_rel_* striper_keep_*, mv(0) override

gen striper_18_23_interact=striper_keep_18_23*striper_rel_18_23
gen striper_28_33_interact=striper_keep_28_33*striper_rel_28_33
gen striper_38_43_interact=striper_keep_38_43*striper_rel_38_43

split version, parse(" ")
rename version1 svy_version
drop version2 version3 version4


tab striper_length, gen(L_)
foreach l in 18 23 28 33 38 43 {
    gen rel_L`l' =striper_rel if striper_length==`l' & striper_rel>0
	gen keep_L`l' =striper_keep if striper_length==`l' & striper_keep>0
	gen sqrt_rel_L`l' =sqrt(striper_rel) if striper_length==`l' & striper_rel>0
	gen sqrt_keep_L`l' =sqrt(striper_keep) if striper_length==`l' & striper_keep>0

}
mvencode rel_L* keep_L* sqrt_rel_L* sqrt_keep_L*, mv(0) override

tab striper_rel striper_length  if inlist(alt, 1, 2) & striper_rel>0
corr striper_rel striper_length if inlist(alt, 1, 2) & striper_rel>0
corr striper_keep striper_length if inlist(alt, 1, 2) & striper_keep>0


clogit choice cost  striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
					striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 
	
clogit choice cost  keep_L18 keep_L23 keep_L28 keep_L33 keep_L38 keep_L43 rel_L18 rel_L23 rel_L28 rel_L33 rel_L38 rel_L43 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 
					
clogit choice cost  sqrt_keep_L18 sqrt_keep_L23 sqrt_keep_L28 sqrt_keep_L33 sqrt_keep_L38 sqrt_keep_L43 sqrt_rel_L18 sqrt_rel_L23 sqrt_rel_L28 sqrt_rel_L33 sqrt_rel_L38 sqrt_rel_L43 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 	
					
clogit choice cost  striper_keep striper_keep_length striper_rel striper_rel_length ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 	
					
clogit choice cost  striper_keep striper_keep_length striper_keep_length2 striper_rel striper_rel_length striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 	

clogit choice cost  striper_keep striper_keep_length striper_keep_length2 striper_rel striper_rel_length striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_out_educ_coll opt_out_educ_grad ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 	
					
					
gen striper_keep_new=striper_keep if striper_c_and_r_only==0
gen striper_rel_new=striper_rel if striper_c_and_r_only==0
replace striper_keep_new= 0 if striper_c_and_r_only==1
replace striper_rel_new=striper_keep + striper_rel if striper_c_and_r_only==1

gen striper_keep_new_length=striper_keep_new*striper_length
gen striper_keep_new_length2=striper_keep_new*striper_length^2
gen striper_rel_new_length=striper_rel_new*striper_length
gen striper_rel_new_length2=striper_rel_new*striper_length^2		

clogit choice cost  striper_keep_new striper_keep_new_length  ///
						striper_rel_new striper_rel_new_length  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) 	

								
preserve	
keep if e(sample)
keep qtid lic_st
duplicates drop 
gen n_anglers=1
collapse (sum) n_anglers, by(lic_st)
egen sum=sum(n)
gen prop=n_anglers/sum
restore 

					
clogit choice cost  striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
					striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & inlist(svy_version, "CHARTER", "PARTY") , ///
					group(gid) 
					
				
clogit choice cost  striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
					striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & svy_version=="PRIVATE", ///
					group(gid) 		
	
clogit choice cost  striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
					striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					bluefish_keep bluefish_rel ///
					opt_other opt_out ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & inlist(svy_version, "SHORE", "KAYAK") , ///
					group(gid) 		
					
					
	
gen one=1

eststo clear							
			
** Model 3 ** 
*LL with no parameters 	
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m3: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250) 

estimates save m3, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


** Model 4 
*LL with no parameters
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m4: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)  

estimates save m4, replace

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


****model 5 
gen sqrt_striper_rel_cnr      = sqrt_striper_rel * striper_c_and_r_only
gen sqrt_striper_rel_len_cnr  = sqrt_striper_rel_length * striper_c_and_r_only
gen sqrt_striper_rel_len2_cnr = sqrt_striper_rel_length2 * striper_c_and_r_only
gen keep_rel_interact = sqrt_striper_keep * sqrt_striper_rel
mvencode sqrt_striper_rel_cnr sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr keep_rel_interact if !missing(striper_c_and_r_only), mv(0) override
mvencode keep_rel_interact, mv(0) override

eststo m5: mixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
    & !missing(striper_c_and_r_only), ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)  

estimates save m5, replace

local ll = e(ll) 

distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'	
			

	
estadd scalar r2_p = 1-`ll'/`l0'



* Model 6 
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 


eststo m6: mixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
    & striper_c_and_r_only==1, ///
	group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)  

estimates save m6, replace

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


* Model 7 
eststo m7: mixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
    & striper_c_and_r_only==0, ///
	group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)  

estimates save m7, replace

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


* Model 8 
gen sqrt_striper_rel_18_28=sqrt_striper_rel if inlist(striper_length, 18, 23, 28)
gen sqrt_striper_rel_33_43=sqrt_striper_rel if inlist(striper_length, 33,38,43)
gen sqrt_striper_rel_18_28_length = sqrt_striper_rel_18_28*striper_length
gen sqrt_striper_rel_33_43_length = sqrt_striper_rel_33_43*striper_length
gen sqrt_striper_rel_18_28_length2 = sqrt_striper_rel_18_28*striper_length^2
gen sqrt_striper_rel_33_43_length2 = sqrt_striper_rel_33_43*striper_length^2

gen sqrt_striper_keep_18_28=sqrt_striper_keep if inlist(striper_length, 18, 23, 28)
gen sqrt_striper_keep_33_43=sqrt_striper_keep if inlist(striper_length, 33,38,43)
gen sqrt_striper_keep_18_28_length = sqrt_striper_keep_18_28*striper_length
gen sqrt_striper_keep_33_43_length = sqrt_striper_keep_33_43*striper_length
gen sqrt_striper_keep_18_28_length2 = sqrt_striper_keep_18_28*striper_length^2
gen sqrt_striper_keep_33_43_length2 = sqrt_striper_keep_33_43*striper_length^2


mvencode sqrt_striper_rel_18_28* sqrt_striper_rel_33_43* sqrt_striper_keep_18_28* sqrt_striper_keep_33_43*, mv(0) override

eststo m8: mixlogit choice  cost ///
					sqrt_striper_keep_18_28_length sqrt_striper_keep_18_28_length2 ///
					sqrt_striper_keep_33_43_length sqrt_striper_keep_33_43_length2 ///
					sqrt_striper_rel_18_28_length sqrt_striper_rel_18_28_length2 ///
					sqrt_striper_rel_33_43_length sqrt_striper_rel_33_43_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) id(qtid) rand(sqrt_striper_keep_18_28 sqrt_striper_keep_33_43 sqrt_striper_rel_18_28 sqrt_striper_rel_33_43 sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(100)	  

estimates save m8
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
*estadd scalar r2_p = 1-`ll'/`l0'


eststo m8b: mixlogit choice  cost ///
					sqrt_striper_keep_18_28_length  ///
					sqrt_striper_keep_33_43_length  ///
					sqrt_striper_rel_18_28_length  ///
					sqrt_striper_rel_33_43_length  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) id(qtid) rand(sqrt_striper_keep_18_28 sqrt_striper_keep_33_43 sqrt_striper_rel_18_28 sqrt_striper_rel_33_43 sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  

estimates save m8b, replace 
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'		



/*
****model 9 
gen sqrt_striper_keep_cnr      = sqrt_striper_keep * striper_c_and_r_only
gen sqrt_striper_keep_len_cnr  = sqrt_striper_keep_length * striper_c_and_r_only
gen sqrt_striper_keep_len2_cnr = sqrt_striper_keep_length2 * striper_c_and_r_only
mvencode sqrt_striper_keep_cnr sqrt_striper_keep_len_cnr sqrt_striper_keep_len2_cnr if !missing(striper_c_and_r_only), mv(0) override 

eststo m9: mixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
	sqrt_striper_keep_cnr ///
	sqrt_striper_keep_len_cnr sqrt_striper_keep_len2_cnr ///
    sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
    & !missing(striper_c_and_r_only), ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)  

estimates save m9, replace

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'
*/



* Model 10 -threshold 28 
* striped bass keep
gen keep_len_base28 = sqrt_striper_keep * striper_length
gen keep_len_above28 = sqrt_striper_keep * cond(striper_length > 28, striper_length - 28, 0)

* striped bass release
gen rel_len_base28 = sqrt_striper_rel * striper_length
gen rel_len_above28 = sqrt_striper_rel * cond(striper_length > 28, striper_length - 28, 0)


eststo m10: mixlogit choice cost ///
    keep_len_base28 keep_len_above28 ///
    rel_len_base28  rel_len_above28 ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid) id(qtid) ///
    rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    nrep(250)

estimates save m10, replace 
	
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
*estadd scalar r2_p = 1-`ll'/`l0'
	
* does the slope change above 28?
test keep_len_above28
test rel_len_above28

* implied slope above 28
lincom keep_len_base28 + keep_len_above28
lincom rel_len_base28  + rel_len_above28


* Model 11 - threshold 33 
* striped bass keep
gen keep_len_base33 = sqrt_striper_keep * striper_length
gen keep_len_above33 = sqrt_striper_keep * cond(striper_length > 33, striper_length - 33, 0)

* striped bass release
gen rel_len_base33 = sqrt_striper_rel * striper_length
gen rel_len_above33 = sqrt_striper_rel * cond(striper_length > 33, striper_length - 33, 0)

eststo m11: mixlogit choice cost ///
    keep_len_base33 keep_len_above33 ///
    rel_len_base33  rel_len_above33 ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid) id(qtid) ///
    rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    nrep(250)

estimates save m11, replace 

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
*estadd scalar r2_p = 1-`ll'/`l0'

test keep_len_above33
test rel_len_above33

lincom keep_len_base33 + keep_len_above33
lincom rel_len_base33  + rel_len_above33


* Model 12 - keep quadtraitc on keep, threshold release
eststo m12: mixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    rel_len_base28 rel_len_above28 ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid) id(qtid) ///
    rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) ///
    nrep(250)


estimates save m12, replace 
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'				




esttab m3 m4 m5 m6 m7 m9 using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length  sqrt_striper_keep_length2  ///
				sqrt_striper_rel sqrt_striper_rel_length  sqrt_striper_rel_length2  ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 

				
				
			
				
* WTP curves (non-K & R)	
************************************************************

* Model 3, 6, 7
u "$input_data_cd\striper_CE_data.dta", clear 

local model m7
estimates use `model'
local minL = 18
local maxL = 43

************************************************************
* B. MWTP for one additional striped bass
* evaluated at baseline keep/release levels
************************************************************

tempfile wtp_fish
postfile F double striper_length cL basecatch ///
    wtp_keep1 se_keep1 lb_keep1 ub_keep1 ///
    wtp_rel1  se_rel1  lb_rel1  ub_rel1 ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {
        local z = `L'

quietly nlcom ///
        (wtp_keep: -(_b[Mean:sqrt_striper_keep] ///
                     + _b[Mean:sqrt_striper_keep_length]*(`z') ///
                     + _b[Mean:sqrt_striper_keep_length2]*(`z'^2)) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[Mean:sqrt_striper_rel] ///
                     + _b[Mean:sqrt_striper_rel_length]*(`z') ///
                     + _b[Mean:sqrt_striper_rel_length2]*(`z'^2)) ///
                     / (2*sqrt(`K')*_b[cost])) 
			
        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar wr  = b[1,2]
        scalar sek = sqrt(V[1,1])
        scalar ser = sqrt(V[2,2])

        post F (`L') (`z') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `wtp_fish', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl

twoway ///
    (rarea lb_keep1 ub_keep1 striper_length if basecatch==1, sort  ///
        color(navy%30) lcolor(navy%40)) ///
    (line  wtp_keep1 striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("MWTP for first kept striped bass ($)") ///
    title("MWTP for first kept striped bass") ///
    xlabel(18 23 28 33 38 43) ylab(#15) xlab(#15))
	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("MWTP for first released striped bass ($)") ///
    title("MWTP for first released striped bass") ///
    xlabel(18 23 28 33 38 43) ylab(#15) xlab(#15))
	
	
* Model 5 
use "$input_data_cd\striper_CE_data.dta", clear
estimates restore m5

local minL = 18
local maxL = 43

tempfile wtp_fish
postfile F double striper_length basecatch ///
    wtp_keep se_keep lb_keep ub_keep ///
    wtp_noncnr se_noncnr lb_noncnr ub_noncnr ///
    wtp_cnr se_cnr lb_cnr ub_cnr ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {

        quietly nlcom ///
            (wtp_keep: ///
                -(_b[sqrt_striper_keep] ///
                + _b[sqrt_striper_keep_length]*(`L') ///
                + _b[sqrt_striper_keep_length2]*(`L'^2)) ///
                /(2*sqrt(`K')*_b[cost])) ///
            (wtp_rel_noncnr: ///
                -(_b[sqrt_striper_rel] ///
                + _b[sqrt_striper_rel_length]*(`L') ///
                + _b[sqrt_striper_rel_length2]*(`L'^2)) ///
                /(2*sqrt(`K')*_b[cost])) ///
            (wtp_rel_cnr: ///
                -(_b[sqrt_striper_rel] + _b[sqrt_striper_rel_cnr] ///
                + (_b[sqrt_striper_rel_length]  + _b[sqrt_striper_rel_len_cnr]) * (`L') ///
                + (_b[sqrt_striper_rel_length2] + _b[sqrt_striper_rel_len2_cnr]) * (`L'^2)) ///
                /(2*sqrt(`K')*_b[cost]))

        matrix b = r(b)
        matrix V = r(V)

        scalar wk   = b[1,1]
        scalar wr0  = b[1,2]
        scalar wr1  = b[1,3]

        scalar sek  = sqrt(V[1,1])
        scalar ser0 = sqrt(V[2,2])
        scalar ser1 = sqrt(V[3,3])

        post F (`L') (`K') ///
            (wk)  (sek)  (wk  - 1.96*sek)  (wk  + 1.96*sek) ///
            (wr0) (ser0) (wr0 - 1.96*ser0) (wr0 + 1.96*ser0) ///
            (wr1) (ser1) (wr1 - 1.96*ser1) (wr1 + 1.96*ser1)
    }
}
postclose F

use `wtp_fish', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort  ///
        color(navy%30) lcolor(navy%40)) ///
    (line  wtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("MWTP for first kept striped bass ($)") ///
    title("MWTP for first kept striped bass") ///
    xlabel(18 23 28 33 38 43) ylab(#15) xlab(#15))
	
twoway ///
    (rarea lb_noncnr ub_noncnr striper_length if basecatch==1, sort ///
        color(maroon%30) lcolor(maroon%40)) ///
    (rarea lb_cnr ub_cnr striper_length if basecatch==1, sort ///
        color(green%30) lcolor(green%40)) ///
    (line  wtp_noncnr striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick)) ///
    (line  wtp_cnr striper_length if basecatch==1, sort ///
        lcolor(green) lwidth(medthick)), ///
    legend(order(3 "Release: not C&R only" 4 "Release: C&R only") ///
           position(6) rows(1) region(lstyle(none))) ///
    xtitle("Length (inches)") ///
    ytitle("MWTP for first released striped bass ($)") ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#15)  xlab(#15)
	
	
* Model 9 
*--------------------------------------------------*
* MWTP for kept striped bass from mixlogit m9
* by harvest orientation
*--------------------------------------------------*
use "$input_data_cd\striper_CE_data.dta", clear
estimates restore m9

local minL = 18
local maxL = 43

tempfile wtp_keep_groups
postfile F double striper_length basecatch ///
    wtp_noncnr se_noncnr lb_noncnr ub_noncnr ///
    wtp_cnr    se_cnr    lb_cnr    ub_cnr ///
    using `wtp_keep_groups', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {

        quietly nlcom ///
            (wtp_keep_noncnr: ///
                -(_b[Mean:sqrt_striper_keep] ///
                + _b[Mean:sqrt_striper_keep_length]*(`L') ///
                + _b[Mean:sqrt_striper_keep_length2]*(`L'^2)) ///
                /(2*sqrt(`K')*_b[Mean:cost])) ///
            (wtp_keep_cnr: ///
                -(_b[Mean:sqrt_striper_keep] + _b[Mean:sqrt_striper_keep_cnr] ///
                + (_b[Mean:sqrt_striper_keep_length]  + _b[Mean:sqrt_striper_keep_len_cnr]) * (`L') ///
                + (_b[Mean:sqrt_striper_keep_length2] + _b[Mean:sqrt_striper_keep_len2_cnr]) * (`L'^2)) ///
                /(2*sqrt(`K')*_b[Mean:cost]))

        matrix b = r(b)
        matrix V = r(V)

        scalar w0  = b[1,1]
        scalar w1  = b[1,2]
        scalar se0 = sqrt(V[1,1])
        scalar se1 = sqrt(V[2,2])

        post F (`L') (`K') ///
            (w0) (se0) (w0 - 1.96*se0) (w0 + 1.96*se0) ///
            (w1) (se1) (w1 - 1.96*se1) (w1 + 1.96*se1)
    }
}
postclose F

use `wtp_keep_groups', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl	


twoway ///
    (rarea lb_noncnr ub_noncnr striper_length if basecatch==1, sort ///
        color(navy%25) lcolor(navy%35)) ///
    (rarea lb_cnr ub_cnr striper_length if basecatch==1, sort ///
        color(green%25) lcolor(green%35)) ///
    (line  wtp_noncnr striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (line  wtp_cnr striper_length if basecatch==1, sort ///
        lcolor(green) lwidth(medthick)), ///
    legend(order(3 "Keep: not C&R only" 4 "Keep: C&R only") ///
           position(6) rows(1) region(lstyle(none))) ///
    xtitle("Length (inches)") ///
    ytitle("MWTP for first kept striped bass ($)") ///
    title("MWTP by striped bass length and harvest orientation") ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#15) xlab(#15)

use "$input_data_cd\striper_CE_data.dta", clear
estimates restore m9

local minL = 18
local maxL = 43
	
tempfile wtp_rel_groups
postfile R double striper_length basecatch ///
    wtp_noncnr se_noncnr lb_noncnr ub_noncnr ///
    wtp_cnr    se_cnr    lb_cnr    ub_cnr ///
    using `wtp_rel_groups', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {

        quietly nlcom ///
            (wtp_rel_noncnr: ///
                -(_b[Mean:sqrt_striper_rel] ///
                + _b[Mean:sqrt_striper_rel_length]*(`L') ///
                + _b[Mean:sqrt_striper_rel_length2]*(`L'^2)) ///
                /(2*sqrt(`K')*_b[Mean:cost])) ///
            (wtp_rel_cnr: ///
                -(_b[Mean:sqrt_striper_rel] + _b[Mean:sqrt_striper_rel_cnr] ///
                + (_b[Mean:sqrt_striper_rel_length]  + _b[Mean:sqrt_striper_rel_len_cnr]) * (`L') ///
                + (_b[Mean:sqrt_striper_rel_length2] + _b[Mean:sqrt_striper_rel_len2_cnr]) * (`L'^2)) ///
                /(2*sqrt(`K')*_b[Mean:cost]))

        matrix b = r(b)
        matrix V = r(V)

        scalar w0  = b[1,1]
        scalar w1  = b[1,2]
        scalar se0 = sqrt(V[1,1])
        scalar se1 = sqrt(V[2,2])

        post R (`L') (`K') ///
            (w0) (se0) (w0 - 1.96*se0) (w0 + 1.96*se0) ///
            (w1) (se1) (w1 - 1.96*se1) (w1 + 1.96*se1)
    }
}
postclose R

use `wtp_rel_groups', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl	


twoway ///
    (rarea lb_noncnr ub_noncnr striper_length if basecatch==1, sort ///
        color(maroon%25) lcolor(maroon%35)) ///
    (rarea lb_cnr ub_cnr striper_length if basecatch==1, sort ///
        color(green%25) lcolor(green%35)) ///
    (line  wtp_noncnr striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick)) ///
    (line  wtp_cnr striper_length if basecatch==1, sort ///
        lcolor(green) lwidth(medthick)), ///
    legend(order(3 "Release: not C&R only" 4 "Release: C&R only") ///
           position(6) rows(1) region(lstyle(none))) ///
    xtitle("Length (inches)") ///
    ytitle("MWTP for first released striped bass ($)") ///
    title("MWTP by striped bass length and harvest orientation") ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#15) xlab(#15)
	