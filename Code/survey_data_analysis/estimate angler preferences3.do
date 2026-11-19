

set seed 03211990

u "$input_data_cd\striper_CE_data.dta", clear 



gen one=1
eststo clear		
					
** Model 1 ** 
*LL with no parameters 	
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  opt_out_educ_coll!=. & opt_out_educ_grad!=. & ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

eststo m1: mixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_out_educ_coll opt_out_educ_grad ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m1, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m1.ster				
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
    ytitle("$") ///
     title("MWTP for first kept striped bass, m1", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_m1.jpg", replace
	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
     title("MWTP for first released striped bass, m1", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_rel_m1.jpg", replace

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m1.jpg", replace


restore	
	
** Model 1 centered** 
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2

gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m1_c: mixlogit choice cost  ///
					sqrt_striper_keep_clength sqrt_striper_keep_clength2 ///
					sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					opt_out_male opt_out_age ///
					opt_out_educ_coll opt_out_educ_grad ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m3_c, replace
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
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m3, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m3.ster	
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
    ytitle("$") ///
    title("MWTP for first kept striped bass, m3", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_m3.jpg", replace
	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP for first released striped bass, m3", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_rel_m3.jpg", replace

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m3.jpg", replace
	
restore 

** Model 3 centered** 
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2

gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m3_c: mixlogit choice cost  ///
					sqrt_striper_keep_clength sqrt_striper_keep_clength2 ///
					sqrt_striper_rel_clength sqrt_striper_rel_clength2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m3_c, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


** Model 4 ** 
*LL with no parameters 	
eststo m4: mixlogit choice cost  sqrt_striper_keep_length  sqrt_striper_rel_length  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m4, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m4.ster	
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
        (wtp_keep: -(_b[Mean:sqrt_striper_keep] ///
                     + _b[Mean:sqrt_striper_keep_length]*(`z') ) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[Mean:sqrt_striper_rel] ///
                     + _b[Mean:sqrt_striper_rel_length]*(`z') ) ///
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
    ytitle("$") ///
    title("MWTP for first kept striped bass, m4", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_m4.jpg", replace
	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP for first released striped bass, m4", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_rel_m4.jpg", replace

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m4.jpg", replace

restore 

** Model 4 centered** 
drop c_striper_length c_striper_length2  sqrt_striper_keep_clength sqrt_striper_rel_clength sqrt_striper_keep_clength2 sqrt_striper_rel_clength2
su striper_length if inlist(alt, 1,2) & (striper_keep>0 | striper_rel>0) & e(sample), meanonly 
gen c_striper_length= striper_length-`r(mean)'
gen c_striper_length2 = c_striper_length^2

gen sqrt_striper_keep_clength=sqrt_striper_keep*c_striper_length
gen sqrt_striper_rel_clength=sqrt_striper_rel*c_striper_length
gen sqrt_striper_keep_clength2=sqrt_striper_keep*c_striper_length2
gen sqrt_striper_rel_clength2=sqrt_striper_rel*c_striper_length2

eststo m4_c: mixlogit choice cost  ///
					sqrt_striper_keep_clength  ///
					sqrt_striper_rel_clength  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m4_c, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'



** Model 5 ** 
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. & no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1	,  group(gid) 
local l0 = e(ll) 

gen striper_rel_18_23=striper_rel if inlist(striper_length, 18, 23) & striper_rel>0
gen striper_rel_28_33=striper_rel if inlist(striper_length, 28, 33) & striper_rel>0
gen striper_rel_38_43=striper_rel if inlist(striper_length, 38, 43) & striper_rel>0

gen striper_keep_18_23=striper_keep if inlist(striper_length, 18, 23) & striper_keep>0
gen striper_keep_28_33=striper_keep if inlist(striper_length, 28, 33) & striper_keep>0
gen striper_keep_38_43=striper_keep if inlist(striper_length, 38, 43) & striper_keep>0

local vars striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 striper_rel_18_23 striper_rel_28_33 striper_rel_38_43
foreach v of local vars{
	gen sqrt_`v'= sqrt(`v')
}
mvencode striper_rel_* striper_keep_* sqrt_striper_rel_* sqrt_striper_keep_*, mv(0) override


eststo m5: mixlogit choice cost  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
													   striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 bluefish_keep bluefish_rel  ///
													   opt_other opt_out) nrep(300) 

estimates save m5, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

*-----------------------------*
* MWTP for Model 5
*-----------------------------*

preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m5.ster

clear
set obs 3
gen cat = _n

gen striper_length_cat = ""
replace striper_length_cat = "18 & 23" in 1
replace striper_length_cat = "28 & 33" in 2
replace striper_length_cat = "38 & 43" in 3

gen wtp_keep = .
gen se_keep  = .
gen lb_keep  = .
gen ub_keep  = .

gen wtp_rel = .
gen se_rel  = .
gen lb_rel  = .
gen ub_rel  = .

forvalues i = 1/3 {

    if `i' == 1 {
        quietly nlcom ///
            (wtp_keep: -_b[Mean:striper_keep_18_23] / _b[Mean:cost]) ///
            (wtp_rel:  -_b[Mean:striper_rel_18_23]  / _b[Mean:cost])
    }
    else if `i' == 2 {
        quietly nlcom ///
            (wtp_keep: -_b[Mean:striper_keep_28_33] / _b[Mean:cost]) ///
            (wtp_rel:  -_b[Mean:striper_rel_28_33]  / _b[Mean:cost])
    }
    else if `i' == 3 {
        quietly nlcom ///
            (wtp_keep: -_b[Mean:striper_keep_38_43] / _b[Mean:cost]) ///
            (wtp_rel:  -_b[Mean:striper_rel_38_43]  / _b[Mean:cost])
    }

    matrix b = r(b)
    matrix V = r(V)

    replace wtp_keep = b[1,1] in `i'
    replace se_keep  = sqrt(V[1,1]) in `i'
    replace lb_keep  = b[1,1] - 1.96*sqrt(V[1,1]) in `i'
    replace ub_keep  = b[1,1] + 1.96*sqrt(V[1,1]) in `i'

    replace wtp_rel = b[1,2] in `i'
    replace se_rel  = sqrt(V[2,2]) in `i'
    replace lb_rel  = b[1,2] - 1.96*sqrt(V[2,2]) in `i'
    replace ub_rel  = b[1,2] + 1.96*sqrt(V[2,2]) in `i'
}


twoway ///
    (rcap lb_keep ub_keep cat , lcolor(navy) lwidth(medium)) ///
    (scatter wtp_keep cat , mcolor(navy) msymbol(circle) msize(medlarge)), ///
    legend(off) ///
    xtitle("Striped bass length category (inches)") ///
    ytitle("$") ///
    title("Keep MWTP by length category, m5", size(medium)) ///
    xlabel(1 "18 & 23" 2 "28 & 33" 3 "38 & 43") ///
    ylab(#10) name(wtp_keep, replace)

twoway ///
    (rcap lb_rel ub_rel cat, lcolor(maroon) lwidth(medium)) ///
    (scatter wtp_rel cat, mcolor(maroon) msymbol(circle) msize(medlarge)), ///
    legend(off) ///
    xtitle("Striped bass length category (inches)") ///
    ytitle("($)") ///
    title("Release MWTP by length category, m5", size(medium)) ///
    xlabel(1 "18 & 23" 2 "28 & 33" 3 "38 & 43") ///
    ylab(#10) name(wtp_rel, replace)

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m5.jpg", replace

restore


** Model 6 ** 
*LL with no parameters 	
eststo m6: mixlogit choice cost  ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					group(gid) id(qtid) rand(sqrt_striper_keep_18_23 sqrt_striper_keep_28_33 sqrt_striper_keep_38_43 ///
													   sqrt_striper_rel_18_23 sqrt_striper_rel_28_33 sqrt_striper_rel_38_43 sqrt_bluefish_keep sqrt_bluefish_rel  ///
													   opt_other opt_out) nrep(300) 

estimates save m6, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

*-----------------------------*
* MWTP for Model 6
*-----------------------------*
preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m6.ster

clear
set obs 3
gen cat = _n

gen striper_length_cat = ""
replace striper_length_cat = "18 & 23" in 1
replace striper_length_cat = "28 & 33" in 2
replace striper_length_cat = "38 & 43" in 3

gen wtp_keep = .
gen se_keep  = .
gen lb_keep  = .
gen ub_keep  = .

gen wtp_rel = .
gen se_rel  = .
gen lb_rel  = .
gen ub_rel  = .

forvalues i = 1/3 {
 local K 1
    if `i' == 1 {
        quietly nlcom ///
            (wtp_keep: -_b[Mean:sqrt_striper_keep_18_23] / (2*sqrt(`K')*_b[Mean:cost])) ///
            (wtp_rel:  -_b[Mean:sqrt_striper_rel_18_23]  / (2*sqrt(`K')*_b[Mean:cost]))
    }
    else if `i' == 2 {
        quietly nlcom ///
            (wtp_keep: -_b[Mean:sqrt_striper_keep_28_33] / (2*sqrt(`K')*_b[Mean:cost])) ///
            (wtp_rel:  -_b[Mean:sqrt_striper_rel_28_33]  / (2*sqrt(`K')*_b[Mean:cost]))
    }
    else if `i' == 3 {
        quietly nlcom ///
            (wtp_keep: -_b[Mean:sqrt_striper_keep_38_43] / (2*sqrt(`K')*_b[Mean:cost])) ///
            (wtp_rel:  -_b[Mean:sqrt_striper_rel_38_43]  / (2*sqrt(`K')*_b[Mean:cost]))
    }

   matrix b = r(b)
    matrix V = r(V)

    replace wtp_keep = b[1,1] in `i'
    replace se_keep  = sqrt(V[1,1]) in `i'
    replace lb_keep  = b[1,1] - 1.96*sqrt(V[1,1]) in `i'
    replace ub_keep  = b[1,1] + 1.96*sqrt(V[1,1]) in `i'

    replace wtp_rel = b[1,2] in `i'
    replace se_rel  = sqrt(V[2,2]) in `i'
    replace lb_rel  = b[1,2] - 1.96*sqrt(V[2,2]) in `i'
    replace ub_rel  = b[1,2] + 1.96*sqrt(V[2,2]) in `i'
}


twoway ///
    (rcap lb_keep ub_keep cat , lcolor(navy) lwidth(medium)) ///
    (scatter wtp_keep cat , mcolor(navy) msymbol(circle) msize(medlarge)), ///
    legend(off) ///
    xtitle("Striped bass length category (inches)") ///
    ytitle("$") ///
    title("Keep (first fish) MWTP by length category, m6", size(medium)) ///
    xlabel(1 "18 & 23" 2 "28 & 33" 3 "38 & 43") ///
    ylab(#10) name(wtp_keep, replace)

twoway ///
    (rcap lb_rel ub_rel cat, lcolor(maroon) lwidth(medium)) ///
    (scatter wtp_rel cat, mcolor(maroon) msymbol(circle) msize(medlarge)), ///
    legend(off) ///
    xtitle("Striped bass length category (inches)") ///
    ytitle("($)") ///
    title("Release (first fish) MWTP by length category, m6", size(medium)) ///
    xlabel(1 "18 & 23" 2 "28 & 33" 3 "38 & 43") ///
    ylab(#10) name(wtp_rel, replace)

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m6.jpg", replace
restore

** Model 7 - charter/party** 
*LL with no parameters 	
split version, parse(" ")
rename version1 svy_version
drop version2 version3 version4

clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. ///
					& no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
					& inlist(svy_version, "CHARTER", "PARTY"),  group(gid) 
local l0 = e(ll) 

eststo m7: mixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & inlist(svy_version, "CHARTER", "PARTY"), ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out )  nrep(300)

estimates save m7, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m7.ster	
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
    ytitle("$") ///
    title("MWTP for first kept striped bass, m7 (party/charter)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_m4.jpg", replace
	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP for first released striped bass, m7 (party/charter)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_rel_m4.jpg", replace

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m7.jpg", replace
restore 

** Model 8 - private** 
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. ///
					& no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
					& inlist(svy_version, "PRIVATE"),  group(gid) 
local l0 = e(ll) 


eststo m8: mixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & inlist(svy_version, "PRIVATE"), ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300) 

estimates save m8, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m8.ster	
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
    ytitle("$") ///
    title("MWTP for first kept striped bass, m8 (private)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_m4.jpg", replace
	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP for first released striped bass, m8 (private)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
*graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_rel_m4.jpg", replace

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m8.jpg", replace
restore 

** Model 9 - shore/kayak** 
clogit choice opt_out opt_other if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. ///
					& no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
					& inlist(svy_version, "SHORE", "KAYAK"),  group(gid) 
local l0 = e(ll) 


eststo m9: mixlogit choice cost  ///
				sqrt_striper_keep_length sqrt_striper_keep_length2 ///
				sqrt_striper_rel_length sqrt_striper_rel_length2 ///
					opt_out_male opt_out_age ///
					opt_out_fishing_skill_3_4  ///
					opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
					if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & inlist(svy_version, "SHORE", "KAYAK") , ///
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out )  nrep(300) 

estimates save m9, replace
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'

preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m9.ster	
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
    ytitle("$") ///
    title("MWTP for first kept striped bass, m9 (shore/kayak)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))

	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP for first released striped bass, m9 (shore/kayak)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))

gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m9.jpg", replace
restore 


*model 10 threshold
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
    nrep(300)

estimates save m10, replace 
	
local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'				
estadd scalar r2_p = 1-`ll'/`l0'

*-----------------------------*
* MWTP for Model 10
*-----------------------------*


preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m10.ster	
local minL = 18
local maxL = 43			
clear
set obs 6
gen striper_length = .
replace striper_length = 18 in 1
replace striper_length = 23 in 2
replace striper_length = 28 in 3
replace striper_length = 33 in 4
replace striper_length = 38 in 5
replace striper_length = 43 in 6

gen above28 = max(striper_length - 28, 0)

local K 1

gen wtp_keep = .
gen se_keep  = .
gen lb_keep  = .
gen ub_keep  = .

gen wtp_rel = .
gen se_rel  = .
gen lb_rel  = .
gen ub_rel  = .

forvalues i = 1/6 {
    local L = striper_length[`i']
    local A = above28[`i']

    quietly nlcom ///
        (wtp_keep: ///
            -(_b[Mean:sqrt_striper_keep] ///
            + _b[Mean:keep_len_base28]*(`L') ///
            + _b[Mean:keep_len_above28]*(`A')) ///
            /(2*sqrt(`K')*_b[Mean:cost])) ///
        (wtp_rel: ///
            -(_b[Mean:sqrt_striper_rel] ///
            + _b[Mean:rel_len_base28]*(`L') ///
            + _b[Mean:rel_len_above28]*(`A')) ///
            /(2*sqrt(`K')*_b[Mean:cost]))

    matrix b = r(b)
    matrix V = r(V)

    replace wtp_keep = b[1,1] in `i'
    replace se_keep  = sqrt(V[1,1]) in `i'
    replace lb_keep  = b[1,1] - 1.96*sqrt(V[1,1]) in `i'
    replace ub_keep  = b[1,1] + 1.96*sqrt(V[1,1]) in `i'

    replace wtp_rel = b[1,2] in `i'
    replace se_rel  = sqrt(V[2,2]) in `i'
    replace lb_rel  = b[1,2] - 1.96*sqrt(V[2,2]) in `i'
    replace ub_rel  = b[1,2] + 1.96*sqrt(V[2,2]) in `i'
}

twoway ///
    (rarea lb_keep ub_keep striper_length, sort color(navy%25) lcolor(navy%35)) ///
    (line  wtp_keep striper_length, sort lcolor(navy) lwidth(medthick)), ///
    legend(off) ///
    xtitle("Length (inches)") ///
    ytitle("($)") ///
    title("Keep MWTP with 28-inch threshold, m10", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) name(wtp_keep, replace)

twoway ///
    (rarea lb_rel ub_rel striper_length, sort color(maroon%25) lcolor(maroon%35)) ///
    (line  wtp_rel striper_length, sort lcolor(maroon) lwidth(medthick)), ///
    legend(off) ///
    xtitle("Length (inches)") ///
    ytitle("($)") ///
    title("Release MWTP with 28-inch threshold, m10", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) name(wtp_rel, replace)
	
gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m10.jpg", replace

restore


*model 11 - catch and release interactions
clogit choice one if opt_out_male!=. & opt_out_age!=. & ///
					opt_out_fishing_skill_3_4!=. &  ///
					opt_other_likely!=. & opt_other_days_str_blue12!=. & opt_other_days_non_str_blue12!=. ///
					& no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 ///
					& !missing(striper_c_and_r_only),  group(gid) 
local l0 = e(ll) 

gen sqrt_striper_rel_cnr      = sqrt_striper_rel * striper_c_and_r_only
gen sqrt_striper_rel_len_cnr  = sqrt_striper_rel_length * striper_c_and_r_only
gen sqrt_striper_rel_len2_cnr = sqrt_striper_rel_length2 * striper_c_and_r_only
gen sqrt_striper_keep_cnr      = sqrt_striper_keep * striper_c_and_r_only
gen sqrt_striper_keep_len_cnr  = sqrt_striper_keep_length * striper_c_and_r_only
gen sqrt_striper_keep_len2_cnr = sqrt_striper_keep_length2 * striper_c_and_r_only
mvencode sqrt_striper_keep_cnr sqrt_striper_keep_len_cnr sqrt_striper_keep_len2_cnr sqrt_striper_rel_cnr sqrt_striper_rel_len_cnr sqrt_striper_rel_len2_cnr  if !missing(striper_c_and_r_only), mv(0) override 

eststo m11: mixlogit choice cost ///
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
					group(gid) id(qtid) rand(sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) nrep(300)  

estimates save m11, replace

local ll = e(ll) 
distinct qtid if e(sample)
estadd scalar n_anglers = `r(ndistinct)'					
estadd scalar r2_p = 1-`ll'/`l0'


preserve
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"
estimates use m11.ster	
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
    ytitle("($)") ///
    title("MWTP for keep by length and harvest orientation, m11", size(medium)) ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#10) xlab(#15) name(wtp_keep, replace)
	

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
    ytitle("($)") ///
    title("MWTP for release by length and harvest orientation, m11", size(medium)) ///
    xlabel(18 23 28 33 38 43) ///
    ylab(#10) xlab(#15) name(wtp_rel, replace)
	
gr combine wtp_keep wtp_rel, cols(2) ycommon
graph export "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\results\wtp_striper_keep_rel_m11.jpg", replace
restore 
	
	
esttab m1 m1_c m3 m3_c m4 m4_c m5 m6 m7 m8 m9 m10 m11 using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length  sqrt_striper_keep_length2  ///
				sqrt_striper_rel sqrt_striper_rel_length  sqrt_striper_rel_length2  ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 	
				
		
esttab m5 m6  using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 3 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_striper_keep sqrt_striper_keep_length  sqrt_striper_keep_length2  ///
				sqrt_striper_rel sqrt_striper_rel_length  sqrt_striper_rel_length2  ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ) 				
			
				
