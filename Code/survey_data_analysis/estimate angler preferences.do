

set seed 03211990

u "$input_data_cd\striper_CE_data.dta", clear 

/*
*missing ages or genders
keep qtid age male educ inc 
duplicates drop 
keep if age==. | male==.
count if age==.
count if male==.
keep qtid age male
*/
* scale variables for convergence purposes
/*
replace cost=cost/100		
replace age=age/100					
replace days_str_blue12=days_str_blue12/100			
replace days_non_str_blue12=days_non_str_blue12/100				

replace opt_out_age = opt_out*age
replace opt_other_days_str_blue12 = opt_out*days_str_blue12
replace opt_other_days_non_str_blue12 = opt_out*days_non_str_blue12

replace striper_length=striper_length/10	
replace bluefish_length=bluefish_length/10	
*/
foreach v in age male  educ_coll educ_grad inc_med inc_high fishing_skill_3_4 likely_to_fish days_str_blue12 days_non_str_blue12{
	di "`v'"
	distinct qtid if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & `v'==.
}


 
*baseline MNL	
clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_fishing_skill_cat2 opt_out_fishing_skill_cat3 opt_out_fishing_skill_cat4  ///
	opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	

* interactions with length
clogit choice cost sqrt_striper_keep c.sqrt_striper_keep#c.striper_length ///
					       sqrt_striper_rel c.sqrt_striper_rel#c.striper_length ///
							sqrt_bluefish_keep c.sqrt_bluefish_keep#c.bluefish_length ///
							sqrt_bluefish_rel c.sqrt_bluefish_rel#c.bluefish_length ///
							opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_fishing_skill_cat2 opt_out_fishing_skill_cat3 opt_out_fishing_skill_cat4  ///
	opt_other_likely opt_other_days_str_blue12  opt_other_days_non_str_blue12 ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	
	
* interactions with length, centered at mean
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'

su bluefish_length if inlist(alt, 1,2) & (bluefish_keep>0 | bluefish_rel>0) & e(sample), meanonly 
gen c_bluefish_length= bluefish_length-`r(mean)'

 *quadratic length terms
gen c_striper_length2 = c_striper_length^2
gen c_bluefish_length2 = c_bluefish_length^2


clogit choice cost sqrt_striper_keep c.sqrt_striper_keep#c.c_striper_length ///
					       sqrt_striper_rel c.sqrt_striper_rel#c.c_striper_length ///
							sqrt_bluefish_keep c.sqrt_bluefish_keep#c.c_bluefish_length ///
							sqrt_bluefish_rel c.sqrt_bluefish_rel#c.c_bluefish_length ///
							opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_fishing_skill_3_4  ///
	opt_other_likely opt_other_days_str_blue12  opt_other_days_non_str_blue12 opt_out_inc_med opt_out_inc_high ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		

	
	****************************************************
* interactions with length, centered at mean, mixed logit
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_bluefish_keep_clength=sqrt_bluefish_keep*c_bluefish_length
gen sqrt_bluefish_rel_clength=sqrt_bluefish_rel*c_bluefish_length

gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2




clogit choice cost  sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_bluefish_keep sqrt_bluefish_keep_length sqrt_bluefish_keep_length2 ///
					sqrt_bluefish_rel sqrt_bluefish_rel_length  sqrt_bluefish_rel_length2 ///
					sqrt_striper_blue_keep ///
					opt_other opt_out ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  opt_out_inc_med opt_out_inc_high ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		

clogit choice cost  striper_keep striper_keep_length striper_keep_length2 ///
					striper_rel striper_rel_length striper_rel_length2 ///
					bluefish_keep bluefish_keep_length bluefish_keep_length2 ///
					bluefish_rel bluefish_rel_length  bluefish_rel_length2 ///
					striper_blue_keep ///
					opt_other opt_out ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  opt_out_inc_med opt_out_inc_high ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	
	
clogit choice cost  sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_bluefish_keep  ///
					sqrt_bluefish_rel ///
					sqrt_striper_blue_keep ///
					opt_other opt_out ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  opt_out_inc_med opt_out_inc_high ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	
clogit choice cost  striper_keep striper_keep_length striper_keep_length2 ///
					striper_rel striper_rel_length striper_rel_length2 ///
					bluefish_keep  ///
					bluefish_rel ///
					striper_blue_keep ///
					opt_other opt_out ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  opt_out_inc_med opt_out_inc_high ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	
	
clogit choice cost  sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_bluefish_keep  ///
					sqrt_bluefish_rel ///
					sqrt_striper_blue_keep ///
					opt_other opt_out ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4   ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	
clogit choice cost  sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_bluefish_keep  ///
					sqrt_bluefish_rel ///
					sqrt_striper_blue_keep ///
					opt_other opt_out ///
					opt_out_male opt_out_age  ///
					opt_out_fishing_skill_3_4   ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	

clogit choice cost  sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_bluefish_keep  ///
					sqrt_bluefish_rel ///
					opt_other opt_out ///
					opt_out_male opt_out_age  ///
					opt_out_fishing_skill_3_4   ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	



/*
mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_rel_clength ///
				sqrt_bluefish_keep_clength sqrt_bluefish_rel_clength ///
				opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
				opt_out_fishing_skill_cat2 opt_out_fishing_skill_cat3 opt_out_fishing_skill_cat4  ///
				opt_other_likely opt_other_days_str_blue12  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)
					

mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_rel_clength  ///
					sqrt_bluefish_keep_clength sqrt_bluefish_rel_clength ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
					
					
* interactions with bluefish lengths never significant, omit them
mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_rel_clength  ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_cat2 opt_out_fishing_skill_cat3 opt_out_fishing_skill_cat4  ///
					opt_other_likely opt_other_days_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)
*/

* interactions with length, centered at mean, mixed logit				
mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_rel_clength  ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12  opt_other_days_non_str_blue12 ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(10)	  


gen one=1

eststo clear							
** Model 1 **

*LL with no parameters 
clogit choice one if opt_out_male!=. & opt_out_age!=. & opt_out_educ_coll!=. & opt_out_educ_grad!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 
		
eststo m1: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
					
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

* center length variables 
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2

gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m1a: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
					
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


** Model 2 **
*LL with no parameters 
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll)

eststo m2: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'
	
* center length variables 	
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m2a: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  	

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

					
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
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

/*
* center length variables 	
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m3a: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

*/

clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m3b: mixlogit choice cost  striper_keep_length striper_keep_length2 striper_rel_length striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(striper_keep striper_rel bluefish_keep bluefish_rel opt_other opt_out ) nrep(500)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m3c: mixlogit choice cost  sqrt_striper_keep_length  sqrt_striper_rel_length  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'



** Model 4 
*LL with no parameters
clogit choice one if opt_out_male!=. & opt_out_age!=. & opt_out_educ_coll!=. & opt_out_educ_grad!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0_no = e(ll) 

eststo m4: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'



* center length variables 	
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m4a: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(500)	  

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

/*
*** try 250 draws
*LL with no parameters 
clogit choice one if opt_out_male!=. & opt_out_age!=. & opt_out_educ_coll!=. & opt_out_educ_grad!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 
		
eststo m1_250: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  
					
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

* center length variables 
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2

gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m1a_250: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  
					
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


** Model 2 **
*LL with no parameters 
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll)

eststo m2_250: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'
	
* center length variables 	
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m2a_250: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					sqrt_striper_blue_keep ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  	

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

					
** Model 3 ** 
*LL with no parameters 	
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m3_250: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


* center length variables 	
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m3a_250: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'



** Model 4 
*LL with no parameters
clogit choice one if opt_out_male!=. & opt_out_age!=. & opt_out_educ_coll!=. & opt_out_educ_grad!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0_no = e(ll) 

eststo m4_250: mixlogit choice cost  sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'



* center length variables 	
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2
gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m4a_250: mixlogit choice cost  sqrt_striper_keep_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(250)	  

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'
*/

gen rel_len1 = sqrt_striper_rel * min(striper_length, 33)
gen rel_len2 = sqrt_striper_rel * max(striper_length - 33, 0)


esttab m1 m1a m2 m2a m3 m3a m4 m4a m1_250 m1a_250 m2_250 m2a_250 m3_250 m3a_250 m4_250 m4a_250 using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_clength sqrt_striper_keep_length2 sqrt_striper_keep_clength2 ///
				sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_clength sqrt_striper_rel_length2 sqrt_striper_rel_clength2 ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 

esttab m3 m3b using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_clength sqrt_striper_keep_length2 sqrt_striper_keep_clength2 ///
				sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_clength sqrt_striper_rel_length2 sqrt_striper_rel_clength2 ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 
				
esttab m3c using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_clength sqrt_striper_keep_length2 sqrt_striper_keep_clength2 ///
				sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_clength sqrt_striper_rel_length2 sqrt_striper_rel_clength2 ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 
				
				
gen rel_len_low  = sqrt_striper_rel * min(striper_length, 35)
gen rel_len_high = sqrt_striper_rel * max(striper_length - 35, 0)

eststo m3b: mixlogit choice cost ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    rel_len_low rel_len_high ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid) id(qtid) ///
    rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) nrep(50)				

eststo m3: mixlogit choice cost   sqrt_striper_keep_length2 ///
					 sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_rel sqrt_striper_rel_length sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(100)	  
					
eststo m3: mixlogit choice cost    ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(100)	  
					
					
					
* model 3 - linear catch variables 
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m3b: mixlogit choice cost  striper_keep_length striper_keep_length2 striper_rel_length striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(striper_keep striper_rel bluefish_keep bluefish_rel opt_other opt_out ) nrep(100)	  	

					
					
					
					
gen rel1=min(striper_length, 28)				
gen rel2= max(striper_length - 28, 0)	
tab striper_length, gen(L_)
foreach l in 18 23 28 33 38 43 {
    gen rel_L`l' = sqrt_striper_rel * (striper_length == `l')
}

clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 
				
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2

gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

clogit choice  cost sqrt_striper_keep sqrt_striper_keep_clength sqrt_striper_keep_clength2 ///
					sqrt_striper_rel sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 
				
				
clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel rel_L23 rel_L28 rel_L33 rel_L38 rel_L43 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 

set seed 03211990
u "$input_data_cd\striper_CE_data.dta", clear 
			
local knots 23 28 33 38				
tempname results
postfile `results' str10 model double ll aic bic using knot_results, replace

foreach k of local knots {

	di "knot placement: `k' "
    * spline variables
    gen L_low  = min(striper_length, `k')
    gen L_high = max(striper_length - `k', 0)

    gen rel_Llow  = sqrt_striper_rel * L_low
    gen rel_Lhigh = sqrt_striper_rel * L_high

    * estimate model
     clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel rel_Llow rel_Lhigh ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 

    * store fit stats
    local ll = e(ll)
    local kpar = e(k)

    local aic = -2*`ll' + 2*`kpar'
    local bic = -2*`ll' + `kpar'*ln(e(N))

    post `results' ("k=`k'") (`ll') (`aic') (`bic')

    drop L_low L_high rel_Llow rel_Lhigh
}
postclose `results'				
	
use knot_results, clear
list, clean	

gen sqrt_striper_rel_18_28=sqrt(sqrt_striper_rel) if inlist(striper_length, 18, 23, 28)
gen sqrt_striper_rel_33_43=sqrt(sqrt_striper_rel) if inlist(striper_length, 33,38,43)
mvencode sqrt_striper_rel_18_28 sqrt_striper_rel_33_43, mv(0) override

   clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel_18_28  c.sqrt_striper_rel_18_28#c.striper_length ///
					sqrt_striper_rel_33_43  c.sqrt_striper_rel_33_43#c.striper_length ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 
				
mkspline L1 28 L2 35 L3 = striper_length
gen rel_L1 = sqrt_striper_rel * L1
gen rel_L2 = sqrt_striper_rel * L2
gen rel_L3 = sqrt_striper_rel * L3

gen sqrt_striper_catch=sqrt(striper_rel+striper_keep)
gen striper_length2=striper_length^2

gen L_base = striper_length
gen L_trophy = max(striper_length - 35, 0)
gen sqrt_rel_L      = sqrt_striper_rel * L_base
gen sqrt_rel_Ltrophy = sqrt_striper_rel * L_trophy

clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_rel_L sqrt_rel_Ltrophy ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid)
	
gen sqrt_striper_rel_cnr      = sqrt_striper_rel * striper_c_and_r_only
gen sqrt_striper_rel_len_cnr  = sqrt_striper_rel_length * striper_c_and_r_only
gen sqrt_striper_rel_len2_cnr = sqrt_striper_rel_length2 * striper_c_and_r_only
gen keep_rel_interact = sqrt_striper_keep * sqrt_striper_rel

eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
    & !missing(striper_c_and_r_only), ///
    group(gid)
	
	
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
	sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
	keep_rel_interact ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)

	
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
	keep_rel_interact ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)	


eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length  ///
	sqrt_striper_rel sqrt_striper_rel_length  ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)	
	
	
	test sqrt_striper_rel_cnr sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr
	test sqrt_striper_rel_len2_cnr
	lincom sqrt_striper_rel_length2 + sqrt_striper_rel_len2_cnr
	lincom sqrt_striper_rel_length + sqrt_striper_rel_len_cnr
	
	
set seed 03211990
u "$input_data_cd\striper_CE_data.dta", clear 
gen sqrt_striper_rel_cnr      = sqrt_striper_rel * striper_c_and_r_only
gen sqrt_striper_rel_len_cnr  = sqrt_striper_rel_length * striper_c_and_r_only
gen sqrt_striper_rel_len2_cnr = sqrt_striper_rel_length2 * striper_c_and_r_only
gen keep_rel_interact = sqrt_striper_keep * sqrt_striper_rel	
gen sqrt_striper_keep_cnr      = sqrt_striper_keep * striper_c_and_r_only
gen sqrt_striper_keep_len_cnr  = sqrt_striper_keep_length * striper_c_and_r_only
gen sqrt_striper_keep_len2_cnr = sqrt_striper_keep_length2 * striper_c_and_r_only
gen rel_len_keep = sqrt_striper_rel_length * sqrt_striper_keep



gen sqrt_striper_keep_new=sqrt_striper_keep if striper_c_and_r_only==0
gen sqrt_striper_rel_new=sqrt_striper_rel if striper_c_and_r_only==0
replace sqrt_striper_keep_new= 0 if striper_c_and_r_only==1
replace sqrt_striper_rel_new=sqrt(striper_keep + striper_rel) if striper_c_and_r_only==1

gen sqrt_striper_keep_new_length=sqrt_striper_keep_new*striper_length
gen sqrt_striper_keep_new_length2=sqrt_striper_keep_new*striper_length^2
gen sqrt_striper_rel_new_length=sqrt_striper_rel_new*striper_length
gen sqrt_striper_rel_new_length2=sqrt_striper_rel_new*striper_length^2


gen sqrt_striper_catch=sqrt(striper_keep + striper_rel)
gen sqrt_striper_catch_length=sqrt_striper_catch*striper_length
gen sqrt_striper_catch_length2=sqrt_striper_catch*striper_length^2

eststo cnr1: clogit choice cost ///
    sqrt_striper_catch sqrt_striper_catch_length sqrt_striper_catch_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)

	gen keep_rel_interact_new=sqrt_striper_keep_new*sqrt_striper_rel_new
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep_new sqrt_striper_keep_new_length sqrt_striper_keep_new_length2 ///
    sqrt_striper_rel_new sqrt_striper_rel_new_length sqrt_striper_rel_new_length2 ///
	striper_length ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
	keep_rel_interact_new ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)
	
eststo cnr1: clogit choice cost ///
     sqrt_striper_keep_new_length sqrt_striper_keep_new_length2 ///
     sqrt_striper_rel_new_length sqrt_striper_rel_new_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
	keep_rel_interact_new ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)
	
	eststo cnr1: clogit choice cost ///
     sqrt_striper_keep_length sqrt_striper_keep_length2 ///
     sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)
	
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)	
	
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 & striper_c_and_r_only==1, ///
    group(gid)

eststo cnr1: clogit choice cost ///
    sqrt_striper_catch sqrt_striper_catch_length sqrt_striper_catch_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 & striper_c_and_r_only==1, ///
    group(gid)
		
	
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 rel_len_keep ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)	

eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2  ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 & striper_keep==0, ///
    group(gid)	
	
	
eststo cnr1: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
	sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)
	
eststo cnr2: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
	sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)	

	
eststo cnr3: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
	sqrt_striper_keep_cnr sqrt_striper_keep_len_cnr sqrt_striper_keep_len2_cnr ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
	sqrt_striper_rel_cnr sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)	
	
	
eststo cnr4: clogit choice cost ///
    sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
	sqrt_striper_keep_cnr sqrt_striper_keep_len_cnr sqrt_striper_keep_len2_cnr ///
    sqrt_striper_rel sqrt_striper_rel_length sqrt_striper_rel_length2 ///
    sqrt_bluefish_keep sqrt_bluefish_rel ///
    opt_other opt_out ///
    opt_out_male opt_out_age ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)		
	
*--------------------------------------------
* Restore the fitted clogit model
*--------------------------------------------
estimates restore .

* If you stored it under a name, use that instead, e.g.
* estimates restore m_release_cnr

*--------------------------------------------
* Create grid of striped bass lengths
*--------------------------------------------
tempfile relcurve
postfile H double striper_length ///
    bundle_noncnr se_noncnr lb_noncnr ub_noncnr ///
    bundle_cnr    se_cnr    lb_cnr    ub_cnr ///
    using `relcurve', replace

forvalues L = 18/43 {


    quietly nlcom ///
        (bundle_noncnr: ///
            _b[sqrt_striper_rel] ///
          + _b[sqrt_striper_rel_length]*(`L') ///
          + _b[sqrt_striper_rel_length2]*(`L'^2)) ///
        (bundle_cnr: ///
            _b[sqrt_striper_rel] + _b[sqrt_striper_rel_cnr] ///
          + (_b[sqrt_striper_rel_length] + _b[sqrt_striper_rel_len_cnr])*(`L') ///
          + (_b[sqrt_striper_rel_length2] + _b[sqrt_striper_rel_len2_cnr])*(`L'^2))
	/*
    quietly nlcom ///
        (bundle_noncnr: ///
            _b[sqrt_striper_rel] ///
          + _b[sqrt_striper_rel_length]*(`L') ///
          + _b[sqrt_striper_rel_length2]*(`L'^2)) ///
        (bundle_cnr: ///
            _b[sqrt_striper_rel]   ///
          + (_b[sqrt_striper_rel_length] + _b[sqrt_striper_rel_len_cnr])*(`L') ///
          + (_b[sqrt_striper_rel_length2] + _b[sqrt_striper_rel_len2_cnr])*(`L'^2))
			*/  
    matrix b = r(b)
    matrix V = r(V)

    scalar b0  = b[1,1]
    scalar b1  = b[1,2]
    scalar se0 = sqrt(V[1,1])
    scalar se1 = sqrt(V[2,2])

    post H (`L') ///
        (b0) (se0) (b0-1.96*se0) (b0+1.96*se0) ///
        (b1) (se1) (b1-1.96*se1) (b1+1.96*se1)
}
postclose H

use `relcurve', clear

twoway ///
    (rarea lb_noncnr ub_noncnr striper_length, sort) ///
    (line  bundle_noncnr striper_length, sort) ///
    (rarea lb_cnr ub_cnr striper_length, sort) ///
    (line  bundle_cnr striper_length, sort), ///
    legend(order(2 "Sometimes/usually harvest" 4 "Catch-and-release only") rows(1)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("Release utility bundle") ///
    title("Utility contribution of released striped bass by harvest orientation")
	
twoway ///
     (rarea lb_cnr ub_cnr striper_length, sort) ///
    (line  bundle_cnr striper_length, sort), ///
    legend(order(1 "Catch-and-release only") rows(1)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("Release utility bundle") ///
    title("Utility contribution of released striped bass by harvest orientation")
	
	
twoway ///
    (line  bundle_noncnr striper_length, sort) ///
    (line  bundle_cnr striper_length, sort), ///
    legend(order(1 "Harvest" 2 "C&R only") rows(2)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("Release utility bundle") ///
    title("Utility contribution of released striped bass by harvest orientation")
	

*--------------------------------------------
* MWTP for one more released striped bass
* evaluated at baseline release count R = 1
*--------------------------------------------
estimates restore .

tempfile relwtp
postfile W double striper_length ///
    wtp_noncnr se_noncnr lb_noncnr ub_noncnr ///
    wtp_cnr    se_cnr    lb_cnr    ub_cnr ///
    using `relwtp', replace

local R 1

forvalues L = 18/43 {

    quietly nlcom ///
        (wtp_noncnr: ///
           -(_b[sqrt_striper_rel] ///
           + _b[sqrt_striper_rel_length]*(`L') ///
           + _b[sqrt_striper_rel_length2]*(`L'^2)) ///
           /(2*sqrt(`R')*_b[cost])) ///
        (wtp_cnr: ///
           -( _b[sqrt_striper_rel_cnr] + _b[sqrt_striper_rel] ///
           + (_b[sqrt_striper_rel_length] + _b[sqrt_striper_rel_len_cnr])*(`L') ///
           + (_b[sqrt_striper_rel_length2] + _b[sqrt_striper_rel_len2_cnr])*(`L'^2)) ///
           /(2*sqrt(`R')*_b[cost]))
/*
quietly nlcom ///
        (wtp_noncnr: ///
           -(_b[sqrt_striper_rel] ///
           + _b[sqrt_striper_rel_length]*(`L') ///
           + _b[sqrt_striper_rel_length2]*(`L'^2)) ///
           /(2*sqrt(`R')*_b[cost])) ///
        (wtp_cnr: ///
           -(_b[sqrt_striper_rel]  ///
           + (_b[sqrt_striper_rel_length] + _b[sqrt_striper_rel_len_cnr])*(`L') ///
           + (_b[sqrt_striper_rel_length2] + _b[sqrt_striper_rel_len2_cnr])*(`L'^2)) ///
           /(2*sqrt(`R')*_b[cost])) 
		   
		   */
		   
    matrix b = r(b)
    matrix V = r(V)

    scalar w0  = b[1,1]
    scalar w1  = b[1,2]
    scalar se0 = sqrt(V[1,1])
    scalar se1 = sqrt(V[2,2])

    post W (`L') ///
        (w0) (se0) (w0-1.96*se0) (w0+1.96*se0) ///
        (w1) (se1) (w1-1.96*se1) (w1+1.96*se1)
}
postclose W

use `relwtp', clear

twoway ///
    (line  wtp_noncnr striper_length, sort) ///
    (line  wtp_cnr striper_length, sort), ///
    legend(order(1 "Harvest" 2 "C&R only") rows(2)) ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP for one additional released striped bass ($)") ///
    title("MWTP by striped bass length and harvest orientation")	
	
	 clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel sqrt_striper_rel_length c.sqrt_striper_rel_length#c.striper_c_and_r_only sqrt_striper_rel_length2 c.sqrt_striper_rel_length2#c.striper_c_and_r_only ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 
				
 clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel sqrt_striper_rel_length c.sqrt_striper_rel_length#c.striper_c_and_r_only sqrt_striper_rel_length2 c.sqrt_striper_rel_length2#c.striper_c_and_r_only ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 
				
drop L* rel_L*				
mkspline L1 23 L2 33 L3 = striper_length
gen rel_L1 = sqrt_striper_rel * L1
gen rel_L2 = sqrt_striper_rel * L2
gen rel_L3 = sqrt_striper_rel * L3
				
clogit choice  cost sqrt_striper_keep sqrt_striper_keep_length sqrt_striper_keep_length2 ///
					sqrt_striper_rel rel_L1 rel_L2 rel_L3 ///
					sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
				group(gid) 
* WTP (non-K & R)	

				
* WTP (non-K & R)	
************************************************************
* MWTP from centered striped-bass specification
* Correctly accounts for sqrt(catch) regressors
************************************************************

local model m3
estimates restore `model'
/*egen rowmss=rowmiss(opt_out_male opt_out_age   ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12)
*/					
* Mean displayed striped-bass length in estimation sample
summ striper_length if e(sample) & inlist(alt,1,2) & (striper_keep>0 | striper_rel>0), meanonly
*summ striper_length if  no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & rowmss==0 & inlist(alt,1,2) & (striper_keep>0 | striper_rel>0), meanonly

return list
*local mu   = r(mean)
local minL = floor(r(min))
local maxL = ceil(r(max))

display "Mean displayed striped-bass length = `mu'"

************************************************************
* B. MWTP for one additional striped bass
* evaluated at baseline keep/release levels
************************************************************
*estimates restore `model'

*nlcom (x: -_b[Mean:sqrt_striper_keep]/_b[Mean:cost])

tempfile wtp_fish
postfile F double striper_length cL basecatch ///
    wtp_keep1 se_keep1 lb_keep1 ub_keep1 ///
    wtp_rel1  se_rel1  lb_rel1  ub_rel1 ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {
       * local z = `L' - `mu'
        local z = `L'

/*
quietly nlcom ///
        (wtp_keep: -(_b[Mean:sqrt_striper_keep] ///
                     + _b[Mean:sqrt_striper_keep_length]*(`z') ///
                     + _b[Mean:sqrt_striper_keep_length2]*(`z'^2)) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[Mean:sqrt_striper_rel] ///
                     + _b[Mean:sqrt_striper_rel_length]*(`z') ///
                     + _b[Mean:sqrt_striper_rel_length2]*(`z'^2)) ///
                     / (2*sqrt(`K')*_b[cost])) 
		*/			 
quietly nlcom ///
        (wtp_keep: -(_b[sqrt_striper_keep] ///
                     + _b[sqrt_striper_keep_length]*(`z')) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[sqrt_striper_rel] ///
                     + _b[sqrt_striper_rel_length]*(`z') ) ///
                     / (2*sqrt(`K')*_b[cost])) 
					 
/*		
*exclude quadratic

quietly nlcom ///
        (wtp_keep: -(_b[Mean:sqrt_striper_keep] ///
                     + _b[Mean:sqrt_striper_keep_length]*(`z') ) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[Mean:sqrt_striper_rel] ///
                     + _b[Mean:sqrt_striper_rel_length]*(`z') ) ///
                     / (2*sqrt(`K')*_b[cost])) 
					 
					 */
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
    (line wtp_keep1 striper_length if basecatch==1, sort) ///
    (line wtp_keep1 striper_length if basecatch==2, sort), ///
    legend(order(1 "Keep: base=1" 2 "Keep: base=2") rows(1)) ///
    xtitle("Striped bass length shown in choice experiment") ///
    ytitle("MWTP for one additional kept striped bass ($)") ///
    title("MWTP for one more kept striped bass")
graph export "wtp_striped_bass_keep_per_fish.png", replace

twoway ///
    (line wtp_rel1 striper_length if basecatch==1, sort) ///
    (line wtp_rel1 striper_length if basecatch==2, sort), ///
    legend(order(1 "Release: base=1" 2 "Release: base=2") rows(1)) ///
    xtitle("Striped bass length shown in choice experiment") ///
    ytitle("MWTP for one additional released striped bass ($)") ///
    title("MWTP for one more released striped bass")
graph export "wtp_striped_bass_release_per_fish.png", replace

			
				
				
global params
forv x=1/$ndraws{
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
		
		
		* generate 5000 draws based on the drawn mean and SD above for the betas specified as random - preference heterogeneity
		* enter zeroes for parameter estimates above the 10% level of significance
		
		clear 
		set obs 5000
		
		gen beta_cost=beta_draw[1,1]
		gen beta_str_keep=rnormal(beta_draw[12,1], abs(beta_draw[18,1]))
		gen beta_str_rel=rnormal(beta_draw[13,1],  abs(beta_draw[19,1]))
		gen beta_blu_keep=rnormal(beta_draw[14,1],  abs(beta_draw[20,1]))
		gen beta_blu_rel=rnormal(0, abs(beta_draw[21,1]))
		gen beta_str_keep_l=rnormal(beta_draw[2,1], 0)
		gen beta_str_keep_l2=rnormal(beta_draw[3,1], 0)
		gen beta_str_rel_l=rnormal(beta_draw[4,1], 0)
		gen beta_str_rel_l2=rnormal(beta_draw[5,1], 0)
		gen beta_opt_out=rnormal(beta_draw[17,1], abs(beta_draw[23,1]))
		gen beta_opt_oth=rnormal(0, abs(beta_draw[22,1]))
		gen beta_opt_out_male=beta_draw[6,1]
		gen beta_opt_out_age=0
		gen beta_opt_out_skill=beta_draw[8,1]
		gen beta_opt_oth_likely=beta_draw[9,1]
		gen beta_opt_oth_days_str_blue=beta_draw[10,1]
		gen beta_opt_oth_days_non_str_blue=beta_draw[11,1]

		return list
			
	gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params


* keep only non-zero parameters 
drop  beta_opt_out_age
compress


save  "$input_data_cd\preference_params.dta", replace




/*
u "$input_data_cd\preference_params.dta", clear 

ds draw, not
return list
local vars `r(varlist)'
di `vars'
foreach v of local vars {
	di "`v'"
	su `v'
	di `r(mean)'
	di `r(sd)'
}
*/
