

cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\pre_sim"

estimates use "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\Code\survey_data_analysis\mD_cmmx_wts.ster"
local cnames : colnames e(b)
display "`cnames'"

/*
1. cost x
2. sqrt_striper_keep_length x
3. sqrt_striper_keep_length2 x
4. sqrt_striper_rel_log_length  x
5. opt_other_likely_to_fish x
6. opt_other_days_str_blue12 x
7. opt_other_days_non_str_blue12 x
8. opt_out_male x
9. opt_out_age x
10. sqrt_striper_keep x
11. sqrt_striper_rel x
12. sqrt_bluefish_keep x
13. sqrt_bluefish_rel x
14. opt_other x
15. opt_out x
16. sd(sqrt_striper_keep) x
17. sd(sqrt_striper_rel) x
18. sd(sqrt_bluefish_keep) x
19. sd(sqrt_bluefish_rel) x
20. sd(opt_other) x
21. sd(opt_out) x
*/

global params
qui forv x=1/$ndraws{
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
		set obs 5000
		gen beta_cost=beta_draw[1,1]
		gen beta_str_kp=rnormal(beta_draw[10,1], beta_draw[16,1])
		gen beta_str_kp_L=beta_draw[2,1]
		gen beta_str_kp_L2=beta_draw[3,1]

		gen beta_str_rel=rnormal(beta_draw[11,1], beta_draw[17,1]) 
		gen beta_str_rel_L=beta_draw[4,1]

		gen beta_blu_keep=rnormal(beta_draw[12,1], beta_draw[18,1])
		gen beta_blu_rel=rnormal(beta_draw[13,1], beta_draw[19,1])
		
		gen beta_optout=rnormal(beta_draw[15,1], abs(beta_draw[21,1]))
		gen beta_optoth=rnormal(0, abs(beta_draw[20,1]))
		gen beta_optout_male=beta_draw[8,1]
		gen beta_optout_age=0
		gen beta_optoth_likely=beta_draw[5,1]
		gen beta_optoth_days_str=0
		gen beta_optoth_days_non_str=beta_draw[7,1]
		
		
		gen draw=`x'

	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params



* keep only non-zero parameters 
drop  beta_optout_age beta_optoth_days_str
compress

save  "$input_data_cd\preference_params.dta", replace

