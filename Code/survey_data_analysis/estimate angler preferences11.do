
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

* Keep 18-33, length
/*
eststo m1: clogit choice cost  ///
				sqrt_striper_keep_18_33 ///
				sqrt_kp_18_33_length ///
				sqrt_striper_keep_38_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)			
*/
				
cmset qtid scenario alt
eststo m1_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_33_length ///
				sqrt_striper_rel_length ///
				opt_other_likely opt_out_male opt_out_age ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_33 sqrt_striper_keep_38_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m1_cmmx, replace

					
preserve
estimates use m1_cmmx	   
local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = `L'

        * ---- KEEP ----
        quietly {
            if `L' < 38 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[alt:sqrt_striper_keep_18_33] + ///
                    _b[alt:sqrt_kp_18_33_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[alt:cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[alt:sqrt_striper_keep_38_43] ///
                ) / (2*sqrt(`K')*_b[alt:cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)
	
restore	   
*/	   				
				
				
*Keep 18-38, length
/*
eststo m2: clogit choice cost  ///
				sqrt_striper_keep_18_38 ///
				sqrt_kp_18_38_length ///
				sqrt_keep_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)

*/	   			

eststo m2_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_length  ///
				sqrt_striper_rel_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m2_cmmx, replace

	

preserve
estimates use m2_cmmx	   
	   
local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = `L'

        * ---- KEEP ----
        quietly {
            if `L' < 43 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[alt:sqrt_striper_keep_18_38] + ///
                    _b[alt:sqrt_kp_18_38_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[alt:cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[alt:sqrt_keep_43] ///
                ) / (2*sqrt(`K')*_b[alt:cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[alt:sqrt_striper_rel] + ///
                    _b[alt:sqrt_striper_rel_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[alt:cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)

twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1, mcolor(navy) msymbol(O) msize(medium)) ///
	(line mwtp_keep striper_length if basecatch==1, lcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1, lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1, mcolor(forest_green) msymbol(D) msize(medium)) ///
	(line mwtp_rel striper_length if basecatch==1, lcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass by length bin, base=1", size(medium)) ///
    name(mwtp_bins_combined, replace)	
	
restore	   
*/	   				

	   				
*Keep 18-33, log length
/*
eststo m3: clogit choice cost  ///
				sqrt_striper_keep_18_33 ///
				sqrt_kp_18_33_log_length ///
				sqrt_striper_keep_38_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_log_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)
*/	   				

eststo m3_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_33_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_33 sqrt_striper_keep_38_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m3_cmmx, replace

		
preserve	   
estimates use m3_cmmx	   

local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = log(`L')

        * ---- KEEP ----
        quietly {
            if `L' < 38 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_18_33] + ///
                    _b[sqrt_kp_18_33_log_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_38_43] ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_log_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1, sort lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1, ///
        mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1, sort lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1, ///
        mcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base=1)", size(medium)) ///
    name(mwtp_combined, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)	
	
twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1, mcolor(navy) msymbol(O) msize(medium)) ///
	(line mwtp_keep striper_length if basecatch==1, lcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1, lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1, mcolor(forest_green) msymbol(D) msize(medium)) ///
	(line mwtp_rel striper_length if basecatch==1, lcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass by length bin, base=1", size(medium)) ///
    name(mwtp_bins_combined, replace)	
	
restore	   				
*/	   				

				
*Keep 18-38, log length
/*
eststo m4: clogit choice cost  ///
				sqrt_striper_keep_18_38 ///
				sqrt_kp_18_38_log_length ///
				sqrt_keep_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_log_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)

*/	   				

eststo m4_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12  opt_out_male opt_out_age ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m4_cmmx, replace

preserve	  
estimates use m4_cmmx	   
 
local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = log(`L')

        * ---- KEEP ----
        quietly {
            if `L' < 43 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_18_38] + ///
                    _b[sqrt_kp_18_38_log_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_keep_43] ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_log_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1, sort lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1, ///
        mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1, sort lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1, ///
        mcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base=1)", size(medium)) ///
    name(mwtp_combined, replace)
	
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)	
	
twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1, mcolor(navy) msymbol(O) msize(medium)) ///
	(line mwtp_keep striper_length if basecatch==1, lcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1, lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1, mcolor(forest_green) msymbol(D) msize(medium)) ///
	(line mwtp_rel striper_length if basecatch==1, lcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass by length bin, base=1", size(medium)) ///
    name(mwtp_bins_combined, replace)	
	
restore	   			  
 */	   				
 
 
*Keep 18-33, sqrt length
/*
eststo m5: clogit choice cost  ///
				sqrt_striper_keep_18_33 ///
				sqrt_kp_18_33_sqrt_length ///
				sqrt_striper_keep_38_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_sqrt_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)
*/	   				

eststo m5_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_33_sqrt_length  ///
				sqrt_striper_rel_sqrt_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_33 sqrt_striper_keep_38_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m5_cmmx, replace



preserve	   
estimates use m5_cmmx	   

local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = sqrt(`L')

        * ---- KEEP ----
        quietly {
            if `L' < 38 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_18_33] + ///
                    _b[sqrt_kp_18_33_sqrt_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_38_43] ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_sqrt_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)
	
restore	   	
*/	   				

				
*Keep 18-38, sqrt length
/*
eststo m6: clogit choice cost  ///
				sqrt_striper_keep_18_38 ///
				sqrt_kp_18_38_sqrt_length ///
				sqrt_keep_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_sqrt_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)
*/		

eststo m6_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_sqrt_length  ///
				sqrt_striper_rel_sqrt_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12  opt_out_male opt_out_age ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m6_cmmx, replace

		

preserve	   
estimates use m6_cmmx	   

local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = sqrt(`L')

        * ---- KEEP ----
        quietly {
            if `L' < 43 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_18_38] + ///
                    _b[sqrt_kp_18_38_sqrt_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_keep_43] ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_sqrt_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)
	
restore	   	
*/	   				

*Keep 18-33, length^2
/*
eststo m7: clogit choice cost  ///
				sqrt_striper_keep_18_33 ///
				sqrt_kp_18_33_length2 ///
				sqrt_striper_keep_38_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_length2 ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)
				
	*/
	
eststo m7_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_33_length2  ///
				sqrt_striper_rel_length2 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_33 sqrt_striper_keep_38_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m7_cmmx, replace

				
		

preserve	   
estimates use m7_cmmx	    
local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = (`L')^2

        * ---- KEEP ----
        quietly {
            if `L' < 38 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_18_33] + ///
                    _b[sqrt_kp_18_33_length2]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_38_43] ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_length2]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)
	
restore	   					
*/	   					

			
*Keep 18-38,length^2
/*
eststo m8: clogit choice cost  ///
				sqrt_striper_keep_18_38 ///
				sqrt_kp_18_38_length2 ///
				sqrt_keep_43 ///
				sqrt_striper_rel ///
				sqrt_striper_rel_length2 ///
				sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12   ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, group(gid)
*/	   				

eststo m8_cmmx: cmxtmixlogit choice cost  ///
				sqrt_kp_18_38_length2  ///
				sqrt_striper_rel_length2 ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_38 sqrt_keep_43 sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m8_cmmx, replace

		

preserve	   
estimates use m8_cmmx	   
local minL = 18
local maxL = 43

tempfile mwtp
postfile F double striper_length basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp', replace

qui foreach K in 1 2 {

    foreach L in 18 23 28 33 38 43 {

        local logL = (`L')^2

        * ---- KEEP ----
        quietly {
            if `L' < 43 {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_striper_keep_18_38] + ///
                    _b[sqrt_kp_18_38_length2]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                nlcom ///
                (mwtp_keep: - ( ///
                    _b[sqrt_keep_43] ///
                ) / (2*sqrt(`K')*_b[cost]))
            }
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        * ---- RELEASE ----
        quietly {
                nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_length2]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))
        }

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post F (`L') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `mwtp', clear
label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%20) lcolor(navy%40)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort ///
        color(maroon%20) lcolor(maroon%40)) ///
    (line  mwtp_keep striper_length if basecatch==2, sort ///
        lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep (base=1)" 4 "Keep (base=2)")) ///
    title("MWTP for kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%20) lcolor(forest_green%40)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort ///
        color(orange%20) lcolor(orange%40)) ///
    (line  mwtp_rel striper_length if basecatch==2, sort ///
        lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Release (base=1)" 4 "Release (base=2)")) ///
    title("MWTP for released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort ///
        color(navy%15) lcolor(navy%30)) ///
    (line  mwtp_keep striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort ///
        color(forest_green%15) lcolor(forest_green%30)) ///
    (line  mwtp_rel striper_length if basecatch==1, sort ///
        lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass (base catch = 1)", size(medium)) ///
    name(mwtp_combined, replace)
	
twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1, mcolor(navy) msymbol(O) msize(medium)) ///
	(line mwtp_keep striper_length if basecatch==1, lcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1, lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1, mcolor(forest_green) msymbol(D) msize(medium)) ///
	(line mwtp_rel striper_length if basecatch==1, lcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass by length bin, base=1", size(medium)) ///
    name(mwtp_bins_combined, replace)	
restore	   					
*/	   				
/*				
esttab m1 m2 m3 m4 m5 m6 m7 m8 using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  
*/			
								
				
esttab m1_cmmx m2_cmmx m3_cmmx m4_cmmx m5_cmmx m6_cmmx m7_cmmx m8_cmmx using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  					

eststo m9_cmmx: cmxtmixlogit choice cost  ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
					random(sqrt_striper_keep_18_23  ///
								sqrt_striper_keep_28_33 ///
								sqrt_striper_keep_38_43 ///
								sqrt_striper_rel_18_23 ///
								sqrt_striper_rel_28_33  ///
								sqrt_striper_rel_38_43  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m9_cmmx, replace				
			
preserve	  
estimates use m9_cmmx
matrix list e(b)

tempfile mwtp_bins
postfile F double bin_id basecatch ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_bins', replace

foreach K in 1 2 {

    * 18-23
    quietly nlcom ///
        (mwtp_keep: -(_b[sqrt_striper_keep_18_23])/(2*sqrt(`K')*_b[cost])) ///
        (mwtp_rel:  -(_b[sqrt_striper_rel_18_23]) /(2*sqrt(`K')*_b[cost]))
    matrix b = r(b)
    matrix V = r(V)
    post F (1) (`K') ///
        (b[1,1]) (sqrt(V[1,1])) (b[1,1]-1.96*sqrt(V[1,1])) (b[1,1]+1.96*sqrt(V[1,1])) ///
        (b[1,2]) (sqrt(V[2,2])) (b[1,2]-1.96*sqrt(V[2,2])) (b[1,2]+1.96*sqrt(V[2,2]))

    * 28-33
    quietly nlcom ///
        (mwtp_keep: -(_b[sqrt_striper_keep_28_33])/(2*sqrt(`K')*_b[cost])) ///
        (mwtp_rel:  -(_b[sqrt_striper_rel_28_33]) /(2*sqrt(`K')*_b[cost]))
    matrix b = r(b)
    matrix V = r(V)
    post F (2) (`K') ///
        (b[1,1]) (sqrt(V[1,1])) (b[1,1]-1.96*sqrt(V[1,1])) (b[1,1]+1.96*sqrt(V[1,1])) ///
        (b[1,2]) (sqrt(V[2,2])) (b[1,2]-1.96*sqrt(V[2,2])) (b[1,2]+1.96*sqrt(V[2,2]))

    * 38-43
    quietly nlcom ///
        (mwtp_keep: -(_b[sqrt_striper_keep_38_43])/(2*sqrt(`K')*_b[cost])) ///
        (mwtp_rel:  -(_b[sqrt_striper_rel_38_43]) /(2*sqrt(`K')*_b[cost]))
    matrix b = r(b)
    matrix V = r(V)
    post F (3) (`K') ///
        (b[1,1]) (sqrt(V[1,1])) (b[1,1]-1.96*sqrt(V[1,1])) (b[1,1]+1.96*sqrt(V[1,1])) ///
        (b[1,2]) (sqrt(V[2,2])) (b[1,2]-1.96*sqrt(V[2,2])) (b[1,2]+1.96*sqrt(V[2,2]))
}

postclose F

use `mwtp_bins', clear

label define binlbl 1 "18–23 in" 2 "28–33 in" 3 "38–43 in"
label values bin_id binlbl

label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl

twoway ///
    (rcap lb_keep ub_keep bin_id if basecatch==1, lcolor(navy)) ///
    (scatter mwtp_keep bin_id if basecatch==1, mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel bin_id if basecatch==1, lcolor(forest_green)) ///
    (scatter mwtp_rel bin_id if basecatch==1, mcolor(forest_green) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    xlabel(1 "18–23" 2 "28–33" 3 "38–43") ///
    legend(order(2 "Keep" 4 "Release")) ///
    title("MWTP for striped bass by length bin, base=1", size(medium)) ///
    name(mwtp_bins_combined, replace)
	
restore	   				
			
esttab m1_cmmx m2_cmmx m3_cmmx m4_cmmx m5_cmmx m6_cmmx m7_cmmx m8_cmmx m9_cmmx using "regression_results.csv", replace nobase noomitted  /// 
				stats(N n_anglers r2_p ll ll_0  aic bic, fmt(0 0 5 3 3 3 3))  se b(%9.3f) label nogaps starlevels( * 0.10 ** 0.05 *** 0.010)  		
	
	
				
gen tab=1 if ((striper_keep>0 & striper_length==43) | (striper_rel>0 & striper_length==43))
egen sumtab=sum(tab), by(gid)				
sort qtid scenario alt

clogit choice cost  ///
				sqrt_striper_keep ///
				sqrt_striper_keep_log_length  ///
				sqrt_striper_rel ///
				sqrt_striper_rel_log_length ///
				sqrt_bluefish_keep sqrt_bluefish_rel ///
				opt_other opt_out ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & sumtab==0, ///
				group(gid) 		
				
				
eststo m3_cmmx_drop43: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & sumtab==0, ///
					random(sqrt_striper_keep sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m3_cmmx_drop43, replace


eststo m1_cmmx_drop43: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_length  ///
				sqrt_striper_rel_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 & sumtab==0, ///
					random(sqrt_striper_keep sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m1_cmmx_drop43, replace



**weight by regulation occurrence 

	/*
keep if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1

*---------------------------------------
* 1. Create gid-level weights
*---------------------------------------
preserve
    keep gid striper_regs_clean
    duplicates drop

    bysort striper_regs_clean: gen n_reg = _N
    gen wt_reg = 1 / n_reg

    * total number of gids
    count
    local N_gid = r(N)

    * sum of current weights
    sum wt_reg
    local sum_w = r(sum)

    * rescale so sum(weights) = N_gid
    gen wt_reg_scaled = wt_reg * (`N_gid' / `sum_w')

    tempfile gid_weights
    save `gid_weights'
restore

*---------------------------------------
* 2. Merge back
*---------------------------------------
merge m:1 gid using `gid_weights', nogen

eststo m9_cmmx_wts: cmxtmixlogit choice cost  ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = wt_reg_scaled], ///
					random(sqrt_striper_keep_18_23  ///
								sqrt_striper_keep_28_33 ///
								sqrt_striper_keep_38_43 ///
								sqrt_striper_rel_18_23 ///
								sqrt_striper_rel_28_33  ///
								sqrt_striper_rel_38_43  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant

estimates save m9_cmmx_wts, replace				
			
*/


*Catch and release specificaiton 
* Interaction variables
gen cr_only = striper_c_and_r_only

gen cr_sqrt_striper_keep_18_38      = cr_only * sqrt_striper_keep_18_38
gen cr_sqrt_keep_43                 = cr_only * sqrt_keep_43
gen cr_sqrt_striper_rel             = cr_only * sqrt_striper_rel

gen cr_sqrt_kp_18_38_log_length     = cr_only * sqrt_kp_18_38_log_length
gen cr_sqrt_striper_rel_log_length  = cr_only * sqrt_striper_rel_log_length


eststo m4_cr_full_cmmx: cmxtmixlogit choice cost ///
    sqrt_kp_18_38_log_length ///
    sqrt_striper_rel_log_length ///
    cr_sqrt_striper_keep_18_38 ///
    cr_sqrt_keep_43 ///
    cr_sqrt_striper_rel ///
    cr_sqrt_kp_18_38_log_length ///
    cr_sqrt_striper_rel_log_length ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    random(sqrt_striper_keep_18_38 ///
           sqrt_keep_43 ///
           sqrt_striper_rel ///
           sqrt_bluefish_keep ///
           sqrt_bluefish_rel ///
           opt_other opt_out) ///
    noconstant		
estimates save m4_cr_full_cmmx, replace	

preserve
estimates use m4_cr_full_cmmx

tempfile mwtp_cr
postfile F double striper_length basecatch cr_group ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_cr', replace

foreach CR in 0 1 {
    foreach K in 1 2 {
        foreach L in 18 23 28 33 38 43 {

            local logL = log(`L')

            * -------------------------
            * KEEP MWTP
            * -------------------------
            if `L' < 43 {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_striper_keep_18_38] + ///
                        `CR' * _b[cr_sqrt_striper_keep_18_38] + ///
                        (_b[sqrt_kp_18_38_log_length] + ///
                         `CR' * _b[cr_sqrt_kp_18_38_log_length]) * `logL' ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_keep_43] + ///
                        `CR' * _b[cr_sqrt_keep_43] ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }

            matrix b = r(b)
            matrix V = r(V)
            scalar wk  = b[1,1]
            scalar sek = sqrt(V[1,1])

            * -------------------------
            * RELEASE MWTP
            * -------------------------
            quietly nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    `CR' * _b[cr_sqrt_striper_rel] + ///
                    (_b[sqrt_striper_rel_log_length] + ///
                     `CR' * _b[cr_sqrt_striper_rel_log_length]) * `logL' ///
                ) / (2*sqrt(`K')*_b[cost]))

            matrix b = r(b)
            matrix V = r(V)
            scalar wr  = b[1,1]
            scalar ser = sqrt(V[1,1])

            post F (`L') (`K') (`CR') ///
                (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
                (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
        }
    }
}

postclose F
use `mwtp_cr', clear

label define crlbl 0 "Other anglers" 1 "Catch-and-release only"
label values cr_group crlbl

label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl


twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & cr_group==0, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & cr_group==0, ///
        mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & cr_group==1, lcolor(maroon)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & cr_group==1, ///
        mcolor(maroon) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Other anglers" 4 "Catch-and-release only")) ///
    title("MWTP for kept striped bass, base catch = 1", size(medium)) ///
    name(mwtp_keep_cr_base1, replace)
	
twoway ///
    (rcap lb_rel ub_rel striper_length if basecatch==1 & cr_group==0, lcolor(forest_green)) ///
    (scatter mwtp_rel striper_length if basecatch==1 & cr_group==0, ///
        mcolor(forest_green) msymbol(O) msize(medium)) ///
    (rcap lb_rel ub_rel striper_length if basecatch==1 & cr_group==1, lcolor(orange)) ///
    (scatter mwtp_rel striper_length if basecatch==1 & cr_group==1, ///
        mcolor(orange) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Other anglers" 4 "Catch-and-release only")) ///
    title("MWTP for released striped bass, base catch = 1", size(medium)) ///
    name(mwtp_rel_cr_base1, replace)
	
restore




eststo m4_cr_harv_cmmx: cmxtmixlogit choice cost ///
    sqrt_kp_18_38_log_length ///
    sqrt_striper_rel_log_length ///
    cr_sqrt_keep_43 ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    random(sqrt_striper_keep_18_38 ///
           sqrt_keep_43 ///
           sqrt_striper_rel ///
           sqrt_bluefish_keep ///
           sqrt_bluefish_rel ///
           opt_other opt_out) ///
    noconstant			
			
estimates save m4_cr_harv_cmmx, replace		


preserve 
estimates use m4_cr_harv_cmmx
* After estimating m4_cr_harv_cmmx

tempfile mwtp_crharv
postfile F double striper_length basecatch cr_group ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_crharv', replace

foreach CR in 0 1 {
    foreach K in 1 2 {
        foreach L in 18 23 28 33 38 43 {

            local logL = log(`L')

            * -------------------------
            * KEEP MWTP
            * 18-38: baseline random keep + common log-length interaction
            * 43: baseline random 43 keep + CR-only shift
            * -------------------------
            if `L' < 43 {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_striper_keep_18_38] + ///
                        _b[sqrt_kp_18_38_log_length]*`logL' ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_keep_43] + ///
                        `CR' * _b[cr_sqrt_keep_43] ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }

            matrix b = r(b)
            matrix V = r(V)
            scalar wk  = b[1,1]
            scalar sek = sqrt(V[1,1])

            * -------------------------
            * RELEASE MWTP
            * Same for both CR groups in this specification
            * -------------------------
            quietly nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    _b[sqrt_striper_rel_log_length]*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))

            matrix b = r(b)
            matrix V = r(V)
            scalar wr  = b[1,1]
            scalar ser = sqrt(V[1,1])

            post F (`L') (`K') (`CR') ///
                (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
                (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
        }
    }
}

postclose F
use `mwtp_crharv', clear

label define crlbl 0 "Other anglers" 1 "Catch-and-release only"
label values cr_group crlbl

label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl
	twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & cr_group==0, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & cr_group==0, ///
        mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & cr_group==1, lcolor(maroon)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & cr_group==1, ///
        mcolor(maroon) msymbol(D) msize(medium)), ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Other anglers" 4 "Catch-and-release only")) ///
    title("MWTP for kept striped bass, base catch = 1", size(medium)) ///
    name(mwtp_keep_crharv_base1, replace)
restore 
	
	

*fishing skill interaction
*--------------------------------------------------
* 1. Create three-level fishing skill variable
*--------------------------------------------------
gen skill3 = .
replace skill3 = 1 if fishing_skill == 1
replace skill3 = 2 if fishing_skill == 2
replace skill3 = 3 if inlist(fishing_skill, 3, 4)

label define skill3lbl 1 "Beginner" 2 "Intermediate" 3 "Advanced/Professional"
label values skill3 skill3lbl

* Skill dummies for interactions
gen skill_intermediate = skill3 == 2
gen skill_advprof      = skill3 == 3

*--------------------------------------------------
* 2. Create interaction variables
*    Beginner is omitted/reference group
*--------------------------------------------------
gen int_sqrt_keep_18_38_intermediate = skill_intermediate * sqrt_striper_keep_18_38
gen int_sqrt_keep_43_intermediate    = skill_intermediate * sqrt_keep_43
gen int_sqrt_rel_intermediate        = skill_intermediate * sqrt_striper_rel

gen int_kp_log_18_38_intermediate    = skill_intermediate * sqrt_kp_18_38_log_length
gen int_rel_log_intermediate         = skill_intermediate * sqrt_striper_rel_log_length

gen int_sqrt_keep_18_38_advprof = skill_advprof * sqrt_striper_keep_18_38
gen int_sqrt_keep_43_advprof    = skill_advprof * sqrt_keep_43
gen int_sqrt_rel_advprof        = skill_advprof * sqrt_striper_rel

gen int_kp_log_18_38_advprof    = skill_advprof * sqrt_kp_18_38_log_length
gen int_rel_log_advprof         = skill_advprof * sqrt_striper_rel_log_length


eststo m4_skill_cmmx: cmxtmixlogit choice cost ///
    sqrt_kp_18_38_log_length ///
    sqrt_striper_rel_log_length ///
    int_sqrt_keep_43_intermediate ///
    int_sqrt_keep_43_advprof ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
       !missing(skill3), ///
    random(sqrt_striper_keep_18_38 ///
           sqrt_keep_43 ///
           sqrt_striper_rel ///
           sqrt_bluefish_keep ///
           sqrt_bluefish_rel ///
           opt_other opt_out) ///
    noconstant

estimates save m4_skill_cmmx

matrix list e(b)
preserve
tempfile mwtp_skill
postfile F double striper_length basecatch skill3 ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_skill', replace

foreach S in 1 2 3 {
    foreach K in 1 2 {
        foreach L in 18 23 28 33 38 43 {

            local logL = log(`L')

            * Skill-specific additions
            if `S' == 1 {
                local add_keep18 = "0"
                local add_keep43 = "0"
                local add_rel    = "0"
                local add_kplog  = "0"
                local add_rellog = "0"
            }
            else if `S' == 2 {
                local add_keep18 = "_b[int_sqrt_keep_18_38_intermediate]"
                local add_keep43 = "_b[int_sqrt_keep_43_intermediate]"
                local add_rel    = "_b[int_sqrt_rel_intermediate]"
                local add_kplog  = "_b[int_kp_log_18_38_intermediate]"
                local add_rellog = "_b[int_rel_log_intermediate]"
            }
            else if `S' == 3 {
                local add_keep18 = "_b[int_sqrt_keep_18_38_advprof]"
                local add_keep43 = "_b[int_sqrt_keep_43_advprof]"
                local add_rel    = "_b[int_sqrt_rel_advprof]"
                local add_kplog  = "_b[int_kp_log_18_38_advprof]"
                local add_rellog = "_b[int_rel_log_advprof]"
            }

            * KEEP MWTP
            if `L' < 43 {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_striper_keep_18_38] + ///
                        (_b[sqrt_kp_18_38_log_length])*`logL' ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_keep_43] + `add_keep43' ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }

            matrix b = r(b)
            matrix V = r(V)
            scalar wk  = b[1,1]
            scalar sek = sqrt(V[1,1])

            * RELEASE MWTP
            quietly nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel] + ///
                    (_b[sqrt_striper_rel_log_length] )*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))

            matrix b = r(b)
            matrix V = r(V)
            scalar wr  = b[1,1]
            scalar ser = sqrt(V[1,1])

            post F (`L') (`K') (`S') ///
                (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
                (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
        }
    }
}

postclose F
use `mwtp_skill', clear

label define skill3lbl2 1 "Beginner" 2 "Intermediate" 3 "Advanced/Professional"
label values skill3 skill3lbl2

label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl

twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & skill3==1, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & skill3==1, ///
        mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & skill3==2, lcolor(maroon)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & skill3==2, ///
        mcolor(maroon) msymbol(D) msize(medium)) ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & skill3==3, lcolor(forest_green)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & skill3==3, ///
        mcolor(forest_green) msymbol(T) msize(medium)), ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Beginner" 4 "Intermediate" 6 "Advanced/Professional")) ///
    title("MWTP for kept striped bass by fishing skill", size(medium)) ///
    name(mwtp_keep_skill_base1, replace)
	
	restore
	
	
	
*--------------------------------------------------
* 1. Create three-level primary mode variable
*--------------------------------------------------

gen main_mode3 = .
replace main_mode3 = 1 if inlist(mode_main, 1, 2)   // shore/surf + kayak/non-motorized
replace main_mode3 = 2 if mode_main == 3            // motorized boat
replace main_mode3 = 3 if inlist(mode_main, 4, 5)   // charter/guide + party/head

label define main_mode3lbl ///
    1 "Shore/kayak" ///
    2 "Motorized boat" ///
    3 "Charter/party/head"
label values main_mode3 main_mode3lbl


*--------------------------------------------------
* 2. Create interaction variables
*    Shore/kayak is the omitted/reference group
*--------------------------------------------------
gen mode_motorized = main_mode3 == 2
gen mode_forhire   = main_mode3 == 3

gen int_sqrt_keep_18_38_motorized = mode_motorized * sqrt_striper_keep_18_38
gen int_sqrt_keep_43_motorized    = mode_motorized * sqrt_keep_43
gen int_sqrt_rel_motorized        = mode_motorized * sqrt_striper_rel
gen int_kp_log_18_38_motorized    = mode_motorized * sqrt_kp_18_38_log_length
gen int_rel_log_motorized         = mode_motorized * sqrt_striper_rel_log_length

gen int_sqrt_keep_18_38_forhire = mode_forhire * sqrt_striper_keep_18_38
gen int_sqrt_keep_43_forhire    = mode_forhire * sqrt_keep_43
gen int_sqrt_rel_forhire        = mode_forhire * sqrt_striper_rel
gen int_kp_log_18_38_forhire    = mode_forhire * sqrt_kp_18_38_log_length
gen int_rel_log_forhire         = mode_forhire * sqrt_striper_rel_log_length


eststo m4_mode_cmmx: cmxtmixlogit choice cost ///
    sqrt_kp_18_38_log_length ///
    sqrt_striper_rel_log_length ///
    int_sqrt_keep_43_motorized ///
    int_sqrt_keep_43_forhire ///
    opt_other_likely ///
    opt_other_days_str_blue12 ///
    opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
       !missing(main_mode3), ///
    random(sqrt_striper_keep_18_38 ///
           sqrt_keep_43 ///
           sqrt_striper_rel ///
           sqrt_bluefish_keep ///
           sqrt_bluefish_rel ///
           opt_other opt_out) ///
    noconstant
	
estimates save m4_mode_cmmx	


matrix list e(b)
preserve
estimates use m4_mode_cmmx
tempfile mwtp_mode
postfile F double striper_length basecatch main_mode3 ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_mode', replace

foreach M in 1 2 3 {
    foreach K in 1 2 {
        foreach L in 18 23 28 33 38 43 {

            local logL = log(`L')

            if `M' == 1 {
                local add_keep18 = "0"
                local add_keep43 = "0"
                local add_rel    = "0"
                local add_kplog  = "0"
                local add_rellog = "0"
            }
            else if `M' == 2 {
                local add_keep18 = "_b[int_sqrt_keep_18_38_motorized]"
                local add_keep43 = "_b[int_sqrt_keep_43_motorized]"
                local add_rel    = "_b[int_sqrt_rel_motorized]"
                local add_kplog  = "_b[int_kp_log_18_38_motorized]"
                local add_rellog = "_b[int_rel_log_motorized]"
            }
            else if `M' == 3 {
                local add_keep18 = "_b[int_sqrt_keep_18_38_forhire]"
                local add_keep43 = "_b[int_sqrt_keep_43_forhire]"
                local add_rel    = "_b[int_sqrt_rel_forhire]"
                local add_kplog  = "_b[int_kp_log_18_38_forhire]"
                local add_rellog = "_b[int_rel_log_forhire]"
            }

            *-------------------------
            * KEEP MWTP
            * 18-38 uses log-length curve
            * 43 uses separate 43 coefficient
            *-------------------------
            if `L' < 43 {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_striper_keep_18_38] + ///
                        (_b[sqrt_kp_18_38_log_length])*`logL' ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }
            else {
                quietly nlcom ///
                    (mwtp_keep: - ( ///
                        _b[sqrt_keep_43] + `add_keep43' ///
                    ) / (2*sqrt(`K')*_b[cost]))
            }

            matrix b = r(b)
            matrix V = r(V)
            scalar wk  = b[1,1]
            scalar sek = sqrt(V[1,1])

            *-------------------------
            * RELEASE MWTP
            *-------------------------
            quietly nlcom ///
                (mwtp_rel: - ( ///
                    _b[sqrt_striper_rel]  + ///
                    (_b[sqrt_striper_rel_log_length])*`logL' ///
                ) / (2*sqrt(`K')*_b[cost]))

            matrix b = r(b)
            matrix V = r(V)
            scalar wr  = b[1,1]
            scalar ser = sqrt(V[1,1])

            post F (`L') (`K') (`M') ///
                (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
                (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
        }
    }
}

postclose F
use `mwtp_mode', clear

label define main_mode3lbl2 ///
    1 "Shore/kayak" ///
    2 "Motorized boat" ///
    3 "Charter/party/head"
label values main_mode3 main_mode3lbl2

label define basecatchlbl 1 "Base catch = 1" 2 "Base catch = 2"
label values basecatch basecatchlbl

twoway ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & main_mode3==1, lcolor(navy)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & main_mode3==1, ///
        mcolor(navy) msymbol(O) msize(medium)) ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & main_mode3==2, lcolor(maroon)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & main_mode3==2, ///
        mcolor(maroon) msymbol(D) msize(medium)) ///
    (rcap lb_keep ub_keep striper_length if basecatch==1 & main_mode3==3, lcolor(forest_green)) ///
    (scatter mwtp_keep striper_length if basecatch==1 & main_mode3==3, ///
        mcolor(forest_green) msymbol(T) msize(medium)), ///
    xtitle("Striped bass length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Shore/kayak" 4 "Motorized boat" 6 "Charter/party/head")) ///
    title("MWTP for kept striped bass by primary mode", size(medium)) ///
    name(mwtp_keep_mode_base1, replace)
	
restore 



*Interactions on grouped harvest specification
gen cr_only = catch_and_release_only

gen cr_keep_18_23 = cr_only * sqrt_striper_keep_18_23
gen cr_keep_28_33 = cr_only * sqrt_striper_keep_28_33
gen cr_keep_38_43 = cr_only * sqrt_striper_keep_38_43

eststo m_cr_clogit: clogit choice cost ///
    sqrt_striper_keep_18_23 ///
    sqrt_striper_keep_28_33 ///
    sqrt_striper_keep_38_43 ///
    cr_keep_18_23 cr_keep_28_33 cr_keep_38_43 ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid)

estimates save m_cr_clogit
preserve
estimates use m_cr_clogit
tempfile mwtp_cr
postfile F double bin_id basecatch cr_group ///
    mwtp se lb ub using `mwtp_cr', replace

foreach G in 0 1 {
    foreach K in 1 2 {

        * 18-23
        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_18_23] + `G'*_b[cr_keep_18_23]) ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (1) (`K') (`G') (w) (se) (w-1.96*se) (w+1.96*se)

        * 28-33
        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_28_33] + `G'*_b[cr_keep_28_33]) ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (2) (`K') (`G') (w) (se) (w-1.96*se) (w+1.96*se)

        * 38-43
        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_38_43] + `G'*_b[cr_keep_38_43]) ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (3) (`K') (`G') (w) (se) (w-1.96*se) (w+1.96*se)
    }
}
postclose F

use `mwtp_cr', clear
label define binlbl 1 "18-23" 2 "28-33" 3 "38-43"
label values bin_id binlbl
label define crlbl 0 "Other anglers" 1 "Catch-and-release only"
label values cr_group crlbl

twoway ///
    (rcap lb ub bin_id if basecatch==1 & cr_group==0, lcolor(navy)) ///
    (scatter mwtp bin_id if basecatch==1 & cr_group==0, mcolor(navy) msymbol(O)) ///
    (rcap lb ub bin_id if basecatch==1 & cr_group==1, lcolor(maroon)) ///
    (scatter mwtp bin_id if basecatch==1 & cr_group==1, mcolor(maroon) msymbol(D)), ///
    xlabel(1 "18-23" 2 "28-33" 3 "38-43") ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    legend(order(2 "Other anglers" 4 "Catch-and-release only")) ///
    title("MWTP for kept striped bass by catch-and-release orientation") ///
    name(mwtp_cr_keep, replace)
	
restore 


* 2. Fishing skill interaction model
gen skill3 = .
replace skill3 = 1 if fishing_skill == 1
replace skill3 = 2 if fishing_skill == 2
replace skill3 = 3 if inlist(fishing_skill, 3, 4)

label define skill3lbl 1 "Beginner" 2 "Intermediate" 3 "Advanced/Professional"
label values skill3 skill3lbl

gen skill_intermediate = skill3 == 2
gen skill_advprof      = skill3 == 3

gen int_keep_18_23_intermediate = skill_intermediate * sqrt_striper_keep_18_23
gen int_keep_28_33_intermediate = skill_intermediate * sqrt_striper_keep_28_33
gen int_keep_38_43_intermediate = skill_intermediate * sqrt_striper_keep_38_43

gen int_keep_18_23_advprof = skill_advprof * sqrt_striper_keep_18_23
gen int_keep_28_33_advprof = skill_advprof * sqrt_striper_keep_28_33
gen int_keep_38_43_advprof = skill_advprof * sqrt_striper_keep_38_43

eststo m_skill_clogit: clogit choice cost ///
    sqrt_striper_keep_18_23 ///
    sqrt_striper_keep_28_33 ///
    sqrt_striper_keep_38_43 ///
    int_keep_18_23_intermediate ///
    int_keep_28_33_intermediate ///
    int_keep_38_43_intermediate ///
    int_keep_18_23_advprof ///
    int_keep_28_33_advprof ///
    int_keep_38_43_advprof ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
       !missing(skill3), ///
    group(gid)
	
estimates save m_skill_clogit

preserve 
estimates use m_skill_clogit

	tempfile mwtp_skill
postfile F double bin_id basecatch skill3 ///
    mwtp se lb ub using `mwtp_skill', replace

foreach S in 1 2 3 {
    foreach K in 1 2 {

        if `S' == 1 {
            local a18 "0"
            local a28 "0"
            local a38 "0"
        }
        else if `S' == 2 {
            local a18 "_b[int_keep_18_23_intermediate]"
            local a28 "_b[int_keep_28_33_intermediate]"
            local a38 "_b[int_keep_38_43_intermediate]"
        }
        else {
            local a18 "_b[int_keep_18_23_advprof]"
            local a28 "_b[int_keep_28_33_advprof]"
            local a38 "_b[int_keep_38_43_advprof]"
        }

        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_18_23] + `a18') ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (1) (`K') (`S') (w) (se) (w-1.96*se) (w+1.96*se)

        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_28_33] + `a28') ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (2) (`K') (`S') (w) (se) (w-1.96*se) (w+1.96*se)

        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_38_43] + `a38') ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (3) (`K') (`S') (w) (se) (w-1.96*se) (w+1.96*se)
    }
}
postclose F

use `mwtp_skill', clear
label define binlbl 1 "18-23" 2 "28-33" 3 "38-43", replace
label values bin_id binlbl
label define skill3lbl2 1 "Beginner" 2 "Intermediate" 3 "Advanced/Professional"
label values skill3 skill3lbl2

twoway ///
    (rcap lb ub bin_id if basecatch==1 & skill3==1, lcolor(navy)) ///
    (scatter mwtp bin_id if basecatch==1 & skill3==1, mcolor(navy) msymbol(O)) ///
    (rcap lb ub bin_id if basecatch==1 & skill3==2, lcolor(maroon)) ///
    (scatter mwtp bin_id if basecatch==1 & skill3==2, mcolor(maroon) msymbol(D)) ///
    (rcap lb ub bin_id if basecatch==1 & skill3==3, lcolor(forest_green)) ///
    (scatter mwtp bin_id if basecatch==1 & skill3==3, mcolor(forest_green) msymbol(T)), ///
    xlabel(1 "18-23" 2 "28-33" 3 "38-43") ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    legend(order(2 "Beginner" 4 "Intermediate" 6 "Advanced/Professional")) ///
    title("MWTP for kept striped bass by fishing skill") ///
    name(mwtp_skill_keep, replace)
	
restore

* . Primary fishing mode interaction model

gen int_keep_18_23_motorized = mode_motorized * sqrt_striper_keep_18_23
gen int_keep_28_33_motorized = mode_motorized * sqrt_striper_keep_28_33
gen int_keep_38_43_motorized = mode_motorized * sqrt_striper_keep_38_43

gen int_keep_18_23_forhire = mode_forhire * sqrt_striper_keep_18_23
gen int_keep_28_33_forhire = mode_forhire * sqrt_striper_keep_28_33
gen int_keep_38_43_forhire = mode_forhire * sqrt_striper_keep_38_43

eststo m_mode_clogit: clogit choice cost ///
    sqrt_striper_keep_18_23 ///
    sqrt_striper_keep_28_33 ///
    sqrt_striper_keep_38_43 ///
    int_keep_18_23_motorized ///
    int_keep_28_33_motorized ///
    int_keep_38_43_motorized ///
    int_keep_18_23_forhire ///
    int_keep_28_33_forhire ///
    int_keep_38_43_forhire ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 & ///
       !missing(main_mode3), ///
    group(gid)
	
estimates save m_mode_clogit

preserve 
estimates use m_mode_clogit
tempfile mwtp_mode
postfile F double bin_id basecatch main_mode3 ///
    mwtp se lb ub using `mwtp_mode', replace

foreach M in 1 2 3 {
    foreach K in 1 2 {

        if `M' == 1 {
            local a18 "0"
            local a28 "0"
            local a38 "0"
        }
        else if `M' == 2 {
            local a18 "_b[int_keep_18_23_motorized]"
            local a28 "_b[int_keep_28_33_motorized]"
            local a38 "_b[int_keep_38_43_motorized]"
        }
        else {
            local a18 "_b[int_keep_18_23_forhire]"
            local a28 "_b[int_keep_28_33_forhire]"
            local a38 "_b[int_keep_38_43_forhire]"
        }

        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_18_23] + `a18') ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (1) (`K') (`M') (w) (se) (w-1.96*se) (w+1.96*se)

        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_28_33] + `a28') ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (2) (`K') (`M') (w) (se) (w-1.96*se) (w+1.96*se)

        quietly nlcom ///
            (mwtp: -(_b[sqrt_striper_keep_38_43] + `a38') ///
                /(2*sqrt(`K')*_b[cost]))
        matrix b = r(b)
        matrix V = r(V)
        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])
        post F (3) (`K') (`M') (w) (se) (w-1.96*se) (w+1.96*se)
    }
}
postclose F

use `mwtp_mode', clear
label define binlbl 1 "18-23" 2 "28-33" 3 "38-43", replace
label values bin_id binlbl
label define main_mode3lbl2 1 "Shore/kayak" 2 "Motorized boat" 3 "Charter/party/head"
label values main_mode3 main_mode3lbl2

twoway ///
    (rcap lb ub bin_id if basecatch==1 & main_mode3==1, lcolor(navy)) ///
    (scatter mwtp bin_id if basecatch==1 & main_mode3==1, mcolor(navy) msymbol(O)) ///
    (rcap lb ub bin_id if basecatch==1 & main_mode3==2, lcolor(maroon)) ///
    (scatter mwtp bin_id if basecatch==1 & main_mode3==2, mcolor(maroon) msymbol(D)) ///
    (rcap lb ub bin_id if basecatch==1 & main_mode3==3, lcolor(forest_green)) ///
    (scatter mwtp bin_id if basecatch==1 & main_mode3==3, mcolor(forest_green) msymbol(T)), ///
    xlabel(1 "18-23" 2 "28-33" 3 "38-43") ///
    xtitle("Striped bass length bin") ///
    ytitle("MWTP ($)") ///
    legend(order(2 "Shore/kayak" 4 "Motorized boat" 6 "Charter/party/head")) ///
    title("MWTP for kept striped bass by primary mode") ///
    name(mwtp_mode_keep, replace)
restore 



* sample weighting 
preserve
clear
input str2 lic_st target_share
"CT" 5.22
"DE" 1.83
"MA" 15.79
"MD" 7.71
"ME" 6.14
"NC" 10.48
"NH" 2.21
"NJ" 20.22
"NY" 23.26
"RI" 4.35
"VA" 2.78
end

replace target_share = target_share / 100

tempfile targets
save `targets'
restore 

preserve 
keep if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
        alt1_choice_protest!=1 & alt2_choice_protest!=1

* Keep one row per respondent (important!)
bysort qtid: keep if _n==1

contract lic_st
rename _freq n

egen total = total(n)
gen sample_share = n / total

tempfile sample
save `sample'

merge 1:1 lic_st using `targets'

gen weight_state = target_share / sample_share

egen wsum = total(weight_state * n)
egen N = total(n)

gen weight_state_norm = weight_state * (N / wsum)
tempfile weights
keep lic_st weight_state_norm
save `weights'
restore 


merge m:1 lic_st using `weights', nogen


 clogit choice cost ///
    sqrt_striper_keep_18_23 ///
    sqrt_striper_keep_28_33 ///
    sqrt_striper_keep_38_43 ///
    cr_keep_18_23 cr_keep_28_33 cr_keep_38_43 ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_norm], ///
    group(gid)
	
 clogit choice cost ///
    sqrt_striper_keep ///
	sqrt_striper_keep_length  ///
    sqrt_striper_rel sqrt_striper_rel_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_norm], ///
    group(gid)
	
eststo cmmx_wts_base: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_norm], ///
					random(sqrt_striper_keep sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant
					
					

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


 clogit choice cost ///
    sqrt_keep_18 ///
    sqrt_keep_23 ///
    sqrt_keep_28 ///
	sqrt_keep_33 ///
    sqrt_keep_38 ///
    sqrt_keep_43 ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
    group(gid)

 clogit choice cost ///
    sqrt_keep_18 ///
    sqrt_keep_23 ///
    sqrt_keep_28 ///
	sqrt_keep_33 ///
    sqrt_keep_38 ///
    sqrt_keep_43 ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)
	
clogit choice cost ///
    sqrt_striper_keep_18_23 ///
    sqrt_striper_keep_28_33 ///
    sqrt_striper_keep_38_43 ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_md_norm], ///
    group(gid)

clogit choice cost ///
    sqrt_striper_keep_18_23 ///
    sqrt_striper_keep_28_33 ///
    sqrt_striper_keep_38_43 ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1 , ///
    group(gid)


 clogit choice cost ///
    sqrt_striper_keep ///
	sqrt_striper_keep_log_length  ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_other_likely ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    opt_out_male opt_out_age ///
    if no_choice_protest!=1 & alt3_choice_protest!=1 & ///
       alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
    group(gid)
	
eststo cmmx_wts_base: cmxtmixlogit choice cost  ///
				sqrt_striper_keep_log_length  ///
				sqrt_striper_rel_log_length ///
				opt_other_likely ///
				opt_other_days_str_blue12 ///
				opt_other_days_non_str_blue12 opt_out_male opt_out_age  ///
				if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 [pweight = weight_state_norm], ///
					random(sqrt_striper_keep sqrt_striper_rel  sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out) noconstant