

set seed 03211990

u "$input_data_cd\striper_CE_data.dta", clear 

				
		
clogit choice cost sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep_length  ///
	sqrt_striper_keep_cnr ///
	sqrt_striper_keep_len_cnr  ///
    sqrt_striper_rel_length  ///
    sqrt_striper_rel_cnr ///
	sqrt_striper_rel_len_cnr  ///
    opt_out_fishing_skill_3_4 ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
    & !missing(striper_c_and_r_only), ///
					group(gid)
					
					
keep if inlist(alt, 1, 2) 
tab striper_length if striper_keep>0
tab striper_length if striper_rel>0

collapse (mean) striper_keep striper_rel, by(striper_length)

gen log_striper_length=log(striper_length) if inlist(alt, 1,2)
mvencode log_striper_length, mv(0)
gen sqrt_striper_length=sqrt(striper_length)

gen log_striper_keep=log(striper_keep) if inlist(alt, 1,2)
gen log_striper_rel=log(striper_rel) if inlist(alt, 1,2)
gen log_bluefish_keep=log(bluefish_keep) if inlist(alt, 1,2)
gen log_bluefish_rel=log(bluefish_rel) if inlist(alt, 1,2)

mvencode log_striper_keep log_striper_rel log_bluefish_keep log_bluefish_rel, mv(0) override

gen striper_catch=striper_keep+striper_rel
gen sqrt_striper_catch=sqrt(striper_catch)

gen sqrt_striper_keep_log_length=log_striper_length*sqrt_striper_keep
gen sqrt_striper_rel_log_length=log_striper_length*sqrt_striper_rel

gen sqrt_striper_keep_sqrt_length=sqrt_striper_length*sqrt_striper_keep
gen sqrt_striper_rel_sqrt_length=sqrt_striper_length*sqrt_striper_rel

gen sqrt_striper_catch_log_length=log_striper_length*sqrt_striper_catch
gen sqrt_striper_catch_sqrt_length=sqrt_striper_length*sqrt_striper_catch

gen sqrt_striper_catch_length=striper_length*sqrt_striper_catch
gen sqrt_striper_catch_length2=striper_length^2*sqrt_striper_catch

gen striper_keep1=1 if striper_keep==1 & inlist(alt, 1, 2)
gen striper_keep2=1 if striper_keep==2 & inlist(alt, 1, 2)
gen striper_keep3=1 if striper_keep==3 & inlist(alt, 1, 2)
mvencode striper_keep*, mv(0) override 

split version, parse(" ")
rename version1 svy_version
drop version2 version3 version4

gen opt_other_bag	= 	opt_other*striper_bag
gen opt_other_min	= 	opt_other*striper_min
gen opt_other_min_max	= 	opt_other*striper_min*max_dummy
gen opt_other_min_max_slot	= 	opt_other*striper_min*max_dummy*slot_length
					
gen opt_out_bag	= 	opt_out*striper_bag
gen opt_out_min	= 	opt_out*striper_min
gen opt_out_min_max	= 	opt_out*striper_min*max_dummy
gen opt_out_min_max_slot	= 	opt_out*striper_min*max_dummy*slot_length

gen striper_rel_18_23=striper_rel if inlist(striper_length, 18, 23) & striper_rel>0
gen striper_rel_28_33=striper_rel if inlist(striper_length, 28, 33) & striper_rel>0
gen striper_rel_38_43=striper_rel if inlist(striper_length, 38, 43) & striper_rel>0

gen striper_keep_18_23=striper_keep if inlist(striper_length, 18, 23) & striper_keep>0
gen striper_keep_28_33=striper_keep if inlist(striper_length, 28, 33) & striper_keep>0
gen striper_keep_38_43=striper_keep if inlist(striper_length, 38, 43) & striper_keep>0
gen striper_keep_18_33=striper_keep if inlist(striper_length, 18, 23, 28, 33) & striper_keep>0

gen striper_keep_18=striper_keep if inlist(striper_length, 18) & striper_keep>0
gen striper_keep_28=striper_keep if inlist(striper_length, 28) & striper_keep>0
gen striper_keep_38=striper_keep if inlist(striper_length, 38) & striper_keep>0
gen striper_keep_23=striper_keep if inlist(striper_length, 23) & striper_keep>0
gen striper_keep_33=striper_keep if inlist(striper_length, 33) & striper_keep>0
gen striper_keep_43=striper_keep if inlist(striper_length, 43) & striper_keep>0

gen striper_rel_18=striper_rel if inlist(striper_length, 18) & striper_rel>0
gen striper_rel_28=striper_rel if inlist(striper_length, 28) & striper_rel>0
gen striper_rel_38=striper_rel if inlist(striper_length, 38) & striper_rel>0
gen striper_rel_23=striper_rel if inlist(striper_length, 23) & striper_rel>0
gen striper_rel_33=striper_rel if inlist(striper_length, 33) & striper_rel>0
gen striper_rel_43=striper_rel if inlist(striper_length, 43) & striper_rel>0

gen striper_catch_18=striper_catch if inlist(striper_length, 18) & striper_catch>0
gen striper_catch_28=striper_catch if inlist(striper_length, 28) & striper_catch>0
gen striper_catch_38=striper_catch if inlist(striper_length, 38) & striper_catch>0
gen striper_catch_23=striper_catch if inlist(striper_length, 23) & striper_catch>0
gen striper_catch_33=striper_catch if inlist(striper_length, 33) & striper_catch>0
gen striper_catch_43=striper_catch if inlist(striper_length, 43) & striper_catch>0

local vars striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 striper_keep_18_33 ///
striper_keep_18  striper_keep_28 striper_keep_38 striper_keep_23 striper_keep_33 striper_keep_43 ///
striper_rel_18  striper_rel_28 striper_rel_38 striper_rel_23 striper_rel_33 striper_rel_43 ///
striper_catch_18  striper_catch_28 striper_catch_38 striper_catch_23 striper_catch_33 striper_catch_43


foreach v of local vars{
	gen sqrt_`v'= sqrt(`v')
}
mvencode striper_rel_* striper_keep_* striper_catch* sqrt_striper_rel_* sqrt_striper_keep_* sqrt_striper_catch_* , mv(0) override

gen striper_keep_18_33_log_length=striper_keep_18_33*log_striper_length
mvencode striper_keep_18_33_log_length, mv(0) override 

clogit  choice cost    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				sqrt_striper_keep sqrt_striper_keep_sqrt_length   ///
				sqrt_striper_rel  ///
				sqrt_striper_rel_sqrt_length  ///
				striper_length ///
				sqrt_striper_catch ///
				opt_out_male opt_out_age ///
				opt_out_fishing_skill_3_4  ///
				opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
				if second_choice_protest!=1, ///
				group(gid)
				
*model 1 - baseline:
	*sqrt(striper_keep), sqrt(striper_keep)*length, sqrt(striper_keep)*length2
	*sqrt(striper_rel), sqrt(striper_rel)*log(length)

cmset qtid scenario alt
cmxtmixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_log_length ///
				opt_out_male opt_out_age ///
				opt_out_fishing_skill_3_4  ///
				opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
				if second_choice_protest!=1, ///
				random(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m1_xtmix, replace


estimates use m1_xtmix
local minL = 18
local maxL = 43			
tempfile wtp_fish
postfile F double striper_length cL basecatch ///
    wtp_keep1 se_keep1 lb_keep1 ub_keep1 ///
    wtp_rel1  se_rel1  lb_rel1  ub_rel1 ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {
        local z = `L'

quietly nlcom ///
		        (wtp_keep:  -(_b[alt:sqrt_striper_keep] ///
                     + _b[alt:sqrt_striper_keep_length]*(`z') ///
					 + _b[alt:sqrt_striper_keep_length2]*(`z'^2)) ///
                     / (2*sqrt(`K')*_b[alt:cost])) ///
			(wtp_rel:  -(_b[alt:sqrt_striper_rel] ///
                     + _b[sqrt_striper_rel_log_length]*log(`z')) ///
                     / (2*sqrt(`K')*_b[alt:cost])) 
					 
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
    ytitle("$") ///
    title("MWTP keep, keep*length, keep*length2", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))

	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP rel, rel*log(length)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
gr combine wtp_keep wtp_rel, cols(2) ycommon		


estimates use m1_xtmix
eststo m1_xtmix
esttab m1_xtmix using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length  sqrt_striper_keep_length2  ///
				sqrt_striper_rel sqrt_striper_rel_log_length    ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 	
		
* krinksy and robb procedure for WTP calculations in paper. 
* Generate random b
estimates use m1_xtmix

global params
qui forv x=1/1000{
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
    
        * Generate draws from vector beta
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw

		clear 
		set obs 1
		gen mean_beta_striper_keep = beta_draw[11,1]
		gen mean_beta_striper_rel =  beta_draw[12,1]
		gen mean_beta_striper_keep_L =  beta_draw[2,1]
		gen mean_beta_striper_keep_L2 =  beta_draw[3,1]
		gen mean_beta_striper_keep_logR =  beta_draw[4,1]
		gen mean_beta_cost =  beta_draw[1,1]
		
		* generate wtp values based on the random params

		*wtp for first fish, increasing keep of other species 
		forv i =18(1)50{
			*gen wtp_striper_keep`i'=-(mean_beta_striper_keep+mean_beta_striper_keep_L*(`i')+mean_beta_striper_keep_L2*(`i'^2))/(2*mean_beta_cost*1)
			gen wtp_striper_keep`i'=-(mean_beta_striper_keep+mean_beta_striper_keep_L*(`i'))/(2*mean_beta_cost*1)

			gen wtp_striper_rel`i'=-(mean_beta_striper_rel+mean_beta_striper_keep_logR*(log(`i')))/(2*mean_beta_cost*1)

		}
		

	gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params

tempfile base 
save `base', replace 

*Cod WTP 
global wtp_keep
global wtp_rel

qui forv i =18(1)50{
	u `base', clear 
	keep wtp_striper_keep`i'
	
	su wtp_striper_keep`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort wtp_striper_keep`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su wtp_striper_keep`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen length = `i'
	gen disp="keep"

	tempfile wtp_keep`i'
	save `wtp_keep`i'', replace
	global wtp_keep "$wtp_keep "`wtp_keep`i''" " 

	u `base', clear 
	keep wtp_striper_rel`i'
	
	su wtp_striper_rel`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort wtp_striper_rel`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su wtp_striper_rel`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen length = `i'
	gen disp="release"
	tempfile wtp_rel`i'
	save `wtp_rel`i'', replace
	global wtp_rel "$wtp_rel "`wtp_rel`i''" " 
	
	
	
}	

clear
dsconcat $wtp_keep
tempfile wtp_keep_med
save `wtp_keep_med', replace 

dsconcat $wtp_rel
append using `wtp_keep_med'

*WTP plot in paper 
global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway rcap upper lower length  if disp=="keep" || scatter wtp_med length  if disp=="keep", $graphoptions xlab(#30, labsize(small))
twoway rcap upper lower length if disp=="release" || scatter wtp_med length  if disp=="release", $graphoptions xlab(#30, labsize(small))


*WTP plot in paper 
global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway rcap upper lower wtp_n_secondary if wtp_n_secondary<=14 || scatter wtp_med wtp_n_secondary  if wtp_n_secondary<=14, $graphoptions






*model 2 - baseline + skill interactions:
local vars sqrt_striper_keep  sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel sqrt_striper_rel_log_length
foreach v of local vars{
	gen `v'_adv=`v'*fishing_skill_3_4
}
mvencode *_adv, mv(0) override


cmxtmixlogit choice cost  ///
				sqrt_striper_keep_adv ///
				sqrt_striper_keep_length sqrt_striper_keep_length_adv /// 
				sqrt_striper_keep_length2 sqrt_striper_keep_length2_adv ///
				sqrt_striper_rel_adv ///
				sqrt_striper_rel_log_length sqrt_striper_rel_log_length_adv ///
				opt_out_male opt_out_age ///
				opt_out_fishing_skill_3_4  ///
				opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
				if second_choice_protest!=1, ///
				random(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m2_xtmix, replace


estimates use m2_xtmix

local minL = 18
local maxL = 43			
tempfile wtp_fish
postfile F double striper_length basecatch ///
    wtp_keep se_keep lb_keep ub_keep ///
	wtp_keep_adv se_keep_adv lb_keep_adv ub_keep_adv ///
    wtp_rel se_rel lb_rel ub_rel ///
	wtp_rel_adv se_rel_adv lb_rel_adv ub_rel_adv ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {
        local z = `L'

quietly nlcom ///
            (wtp_keep: ///
                -(_b[alt:sqrt_striper_keep] ///
                + _b[alt:sqrt_striper_keep_length]*(`L') ///
                + _b[alt:sqrt_striper_keep_length2]*(`L'^2)) ///
                /(2*sqrt(`K')*_b[alt:cost])) ///
            (wtp_keep_adv: ///
                -(_b[alt:sqrt_striper_keep] + _b[alt:sqrt_striper_keep_adv] ///
                + (_b[alt:sqrt_striper_keep_length]  + _b[alt:sqrt_striper_keep_length_adv]) * (`L') ///
                + (_b[alt:sqrt_striper_keep_length2] + _b[alt:sqrt_striper_keep_length2_adv]) * (`L'^2)) ///
                /(2*sqrt(`K')*_b[alt:cost])) ///	
			(wtp_rel: ///
                -(_b[alt:sqrt_striper_rel] ///
                + _b[alt:sqrt_striper_rel_log_length]*log(`L')) ///
                /(2*sqrt(`K')*_b[alt:cost])) ///
            (wtp_rel_adv: ///
                -(_b[alt:sqrt_striper_rel] + _b[alt:sqrt_striper_rel_adv] ///
                + (_b[alt:sqrt_striper_rel_log_length]  + _b[alt:sqrt_striper_rel_log_length_adv]) * log(`L')) ///
                /(2*sqrt(`K')*_b[alt:cost]))

        matrix b = r(b)
        matrix V = r(V)

        scalar w0  = b[1,1]
        scalar w1  = b[1,2]
		scalar w2  = b[1,3]
        scalar w3  = b[1,4]
        scalar se0 = sqrt(V[1,1])
        scalar se1 = sqrt(V[2,2])
        scalar se2 = sqrt(V[3,3])
        scalar se3 = sqrt(V[4,4])
		
        post F (`L') (`K') ///
            (w0) (se0) (w0 - 1.96*se0) (w0 + 1.96*se0) ///
            (w1) (se1) (w1 - 1.96*se1) (w1 + 1.96*se1) ///
		    (w2) (se2) (w2 - 1.96*se2) (w2 + 1.96*se2) ///
            (w3) (se3) (w3 - 1.96*se3) (w3 + 1.96*se3) 
			
        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar wk_adv  = b[1,2]
		scalar wr  = b[1,3]
        scalar wr_adv  = b[1,4]
        scalar sek = sqrt(V[1,1])
        scalar sek_adv = sqrt(V[2,2])
        scalar ser = sqrt(V[3,3])
        scalar ser_adv = sqrt(V[4,4])
		
        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
			(wk_adv) (sek_adv) (wk_adv-1.96*sek_adv) (wk_adv+1.96*sek_adv) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser) ///
            (wr_adv) (ser_adv) (wr_adv-1.96*ser_adv) (wr_adv+1.96*ser_adv) 
    }
}
postclose F
use `wtp_fish', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl	

twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%25) lcolor(navy%35)) ///
    (rarea lb_keep_adv ub_keep_adv striper_length if basecatch==1, sort ///
        color(green%25) lcolor(green%35)) ///
    (line  wtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (line  wtp_keep_adv striper_length if basecatch==1, sort ///
        lcolor(green) lwidth(medthick)), ///
    legend(order(3 "beginner/intermediate" 4 "advanced/professional") ///
           position(6) rows(1) region(lstyle(none))) ///
    xtitle("Length (inches)") ///
    ytitle("($)") ///
    title("MWTP for keep by length skill", size(medium)) ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#10) xlab(#15) name(wtp_keep, replace)

	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(navy%25) lcolor(navy%35)) ///
    (rarea lb_rel_adv ub_rel_adv striper_length if basecatch==1, sort ///
        color(green%25) lcolor(green%35)) ///
    (line  wtp_rel striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (line  wtp_rel_adv striper_length if basecatch==1, sort ///
        lcolor(green) lwidth(medthick)), ///
    legend(order(3 "beginner/intermediate" 4 "advanced/professional") ///
           position(6) rows(1) region(lstyle(none))) ///
    xtitle("Length (inches)") ///
    ytitle("($)") ///
    title("MWTP for rel by length skill", size(medium)) ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#10) xlab(#15) name(wtp_rel, replace)

gr combine wtp_keep wtp_rel, cols(2) ycommon
	

clogit  choice cost  sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				sqrt_striper_keep_adv ///
				sqrt_striper_keep_length sqrt_striper_keep_length_adv ///
				sqrt_striper_keep_length2 sqrt_striper_keep_length2_adv ///
				sqrt_striper_rel_adv ///
				sqrt_striper_rel_log_length sqrt_striper_rel_log_length_adv ///
				opt_out_male opt_out_age ///
				opt_out_fishing_skill_3_4  ///
				opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
				if second_choice_protest!=1, ///
				group(gid)
*/