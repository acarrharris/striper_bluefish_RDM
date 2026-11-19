
use "$input_data_cd\striper_CE_data.dta", clear

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


clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		sqrt_striper_keep ///
		sqrt_striper_rel ///
		c.sqrt_striper_catch#c.striper_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
	  opt_out_min_max opt_out_min_max_slot ///
    if second_choice_protest!=1 ,  group(gid)	
	

* Estimate model
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep ///
    sqrt_striper_rel ///
    c.sqrt_striper_catch#c.log_striper_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep ///
    sqrt_striper_rel ///
    c.sqrt_striper_catch#c.striper_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)
	
gen sqrt_striper_keep_rel=sqrt_striper_keep*sqrt_striper_rel

clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep ///
    sqrt_striper_rel ///
    c.sqrt_striper_catch#c.log_striper_length ///
	sqrt_striper_keep_rel ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

matrix list e(b)
preserve
tempfile wtp_keep_steps
postfile KFILE double striper_length step wtp se lb ub using `wtp_keep_steps', replace

foreach L in 18 23 28 33 38 43 {

    * 0 -> 1 kept fish, holding releases at 0
    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_keep]*(sqrt(1)-sqrt(0)) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*(log(`L'))*(sqrt(1)-sqrt(0)) ///
        ) / _b[cost])
    matrix b = r(b)
    matrix V = r(V)
    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])
    post KFILE (`L') (1) (w) (se) (w-1.96*se) (w+1.96*se)

    * 1 -> 2 kept fish
    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_keep]*(sqrt(2)-sqrt(1)) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*(log(`L'))*(sqrt(2)-sqrt(1)) ///
        ) / _b[cost])
    matrix b = r(b)
    matrix V = r(V)
    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])
    post KFILE (`L') (2) (w) (se) (w-1.96*se) (w+1.96*se)

    * 2 -> 3 kept fish
    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_keep]*(sqrt(3)-sqrt(2)) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*(log(`L'))*(sqrt(3)-sqrt(2)) ///
        ) / _b[cost])
    matrix b = r(b)
    matrix V = r(V)
    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])
    post KFILE (`L') (3) (w) (se) (w-1.96*se) (w+1.96*se)
}
postclose KFILE

use `wtp_keep_steps', clear
label define steplbl 1 "0 to 1 fish" 2 "1 to 2 fish" 3 "2 to 3 fish"
label values step steplbl

twoway ///
    (line wtp striper_length if step==1, sort lcolor(navy) lwidth(medthick)) ///
    (line wtp striper_length if step==2, sort lcolor(maroon) lpattern(dash) lwidth(medthick)) ///
    (line wtp striper_length if step==3, sort lcolor(forest_green) lpattern(dot) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(1 "0 to 1 fish" 2 "1 to 2 fish" 3 "2 to 3 fish")) ///
    title("WTP for kept striped bass by length", size(medium)) ///
    name(wtp_keep_steps, replace)
	
restore

preserve
tempfile wtp_rel_steps
postfile RFILE double striper_length step wtp se lb ub using `wtp_rel_steps', replace

foreach L in 18 23 28 33 38 43 {

    * 0 -> 1 released fish, holding keeps at 0
    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_rel]*(sqrt(1)-sqrt(0)) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*(log(`L'))*(sqrt(1)-sqrt(0)) ///
        ) / _b[cost])
    matrix b = r(b)
    matrix V = r(V)
    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])
    post RFILE (`L') (1) (w) (se) (w-1.96*se) (w+1.96*se)

    * 1 -> 2 released fish
    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_rel]*(sqrt(2)-sqrt(1)) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*(log(`L'))*(sqrt(2)-sqrt(1)) ///
        ) / _b[cost])
    matrix b = r(b)
    matrix V = r(V)
    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])
    post RFILE (`L') (2) (w) (se) (w-1.96*se) (w+1.96*se)

    * 2 -> 3 released fish
    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_rel]*(sqrt(3)-sqrt(2)) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*(log(`L'))*(sqrt(3)-sqrt(2)) ///
        ) / _b[cost])
    matrix b = r(b)
    matrix V = r(V)
    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])
    post RFILE (`L') (3) (w) (se) (w-1.96*se) (w+1.96*se)
}
postclose RFILE

use `wtp_rel_steps', clear
label values step steplbl

twoway ///
    (line wtp striper_length if step==1, sort lcolor(navy) lwidth(medthick)) ///
    (line wtp striper_length if step==2, sort lcolor(maroon) lpattern(dash) lwidth(medthick)) ///
    (line wtp striper_length if step==3, sort lcolor(forest_green) lpattern(dot) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(1 "0 to 1 fish" 2 "1 to 2 fish" 3 "2 to 3 fish")) ///
    title("WTP for released striped bass by length", size(medium)) ///
    name(wtp_rel_steps, replace)

	use `wtp_keep_steps', clear
gen type = "Keep"
append using `wtp_rel_steps'
replace type = "Release" if missing(type)

twoway ///
    (line wtp striper_length if step==1 & type=="Keep",    sort lcolor(navy)        lwidth(medthick)) ///
    (line wtp striper_length if step==2 & type=="Keep",    sort lcolor(navy)        lpattern(dash) lwidth(medthick)) ///
    (line wtp striper_length if step==3 & type=="Keep",    sort lcolor(navy)        lpattern(dot)  lwidth(medthick)) ///
    (line wtp striper_length if step==1 & type=="Release", sort lcolor(maroon)      lwidth(medthick)) ///
    (line wtp striper_length if step==2 & type=="Release", sort lcolor(maroon)      lpattern(dash) lwidth(medthick)) ///
    (line wtp striper_length if step==3 & type=="Release", sort lcolor(maroon)      lpattern(dot)  lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(1 "Keep: 0 to 1" 2 "Keep: 1 to 2" 3 "Keep: 2 to 3" 4 "Release: 0 to 1" 5 "Release: 1 to 2" 6 "Release: 2 to 3")) ///
    title("WTP for striped bass by length", size(medium)) ///
    name(wtp_keep_release_steps, replace)
restore



replace striper_length=log(striper_length)
mvencode striper_length, mv(0) override
replace striper_length=striper_length*striper_length
* Estimate model
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep ///
    sqrt_striper_rel ///
    c.sqrt_striper_catch#c.striper_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

matrix list e(b)
preserve
tempfile mwtp_fish
postfile F double striper_length ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_fish', replace

* Baseline values
local K_keep = 1
local R_keep = 0

local K_rel  = 0
local R_rel  = 1

foreach L in 18 23 28 33 38 43 {

    quietly nlcom ///
        (mwtp_keep: - ( ///
            _b[sqrt_striper_keep]/(2*sqrt(`K_keep')) + ///
            _b[c.sqrt_striper_catch#c.striper_length]*(log(`L'))/(2*sqrt(`K_keep' + `R_keep')) ///
        ) / _b[cost]) ///
        (mwtp_rel: - ( ///
            _b[sqrt_striper_rel]/(2*sqrt(`R_rel')) + ///
            _b[c.sqrt_striper_catch#c.striper_length]*(log(`L'))/(2*sqrt(`K_rel' + `R_rel')) ///
        ) / _b[cost])

    matrix b = r(b)
    matrix V = r(V)

    scalar wk  = b[1,1]
    scalar wr  = b[1,2]
    scalar sek = sqrt(V[1,1])
    scalar ser = sqrt(V[2,2])

    post F (`L') ///
        (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
        (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
}
postclose F

use `mwtp_fish', clear

twoway ///
    (line mwtp_keep striper_length, sort lcolor(navy) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(off) ///
    title("Marginal WTP for an additional kept striped bass", size(medium)) ///
    name(mwtp_keep, replace)
	
twoway ///
    (line mwtp_rel striper_length, sort lcolor(maroon) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(off) ///
    title("Marginal WTP for an additional released striped bass", size(medium)) ///
    name(mwtp_rel, replace)
	
twoway ///
    (line mwtp_keep striper_length, sort lcolor(navy) lwidth(medthick)) ///
    (line mwtp_rel  striper_length, sort lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(1 "Keep" 2 "Release")) ///
    title("Marginal WTP for an additional striped bass", size(medium)) ///
    name(mwtp_keep_rel, replace)
restore



replace log_striper_length=striper_length*striper_length

* Estimate model
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep ///
    sqrt_striper_rel ///
    c.sqrt_striper_catch#c.log_striper_length ///
    sqrt_striper_keep_rel ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

matrix list e(b)

preserve
tempfile mwtp_fish
postfile F double striper_length ///
    mwtp_keep se_keep lb_keep ub_keep ///
    mwtp_rel  se_rel  lb_rel  ub_rel ///
    using `mwtp_fish', replace

* Baseline values
local K0 = 1
local R0 = 1

foreach L in 18 23 28 33 38 43 {
    local logL = (`L')^2

    quietly nlcom ///
        (mwtp_keep: - ( ///
            _b[sqrt_striper_keep]/(2*sqrt(`K0')) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*`logL'/(2*sqrt(`K0' + `R0')) + ///
            _b[sqrt_striper_keep_rel]*sqrt(`R0')/(2*sqrt(`K0')) ///
        ) / _b[cost]) ///
        (mwtp_rel: - ( ///
            _b[sqrt_striper_rel]/(2*sqrt(`R0')) + ///
            _b[c.sqrt_striper_catch#c.log_striper_length]*`logL'/(2*sqrt(`K0' + `R0')) + ///
            _b[sqrt_striper_keep_rel]*sqrt(`K0')/(2*sqrt(`R0')) ///
        ) / _b[cost])

    matrix b = r(b)
    matrix V = r(V)

    scalar wk  = b[1,1]
    scalar wr  = b[1,2]
    scalar sek = sqrt(V[1,1])
    scalar ser = sqrt(V[2,2])

    post F (`L') ///
        (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
        (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
}
postclose F

use `mwtp_fish', clear
twoway ///
    (line mwtp_keep striper_length, sort lcolor(navy) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(off) ///
    title("Marginal WTP for an additional kept striped bass", size(medium)) ///
    subtitle("Evaluated at baseline keep=1, release=1") ///
    name(mwtp_keep, replace)
	
twoway ///
    (line mwtp_rel striper_length, sort lcolor(maroon) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(off) ///
    title("Marginal WTP for an additional released striped bass", size(medium)) ///
    subtitle("Evaluated at baseline keep=1, release=1") ///
    name(mwtp_rel, replace)

twoway ///
    (line mwtp_keep striper_length, sort lcolor(navy) lwidth(medthick)) ///
    (line mwtp_rel  striper_length, sort lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("MWTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(1 "Keep" 2 "Release")) ///
    title("Marginal WTP for an additional striped bass", size(medium)) ///
    subtitle("Evaluated at baseline keep=1, release=1") ///
    name(mwtp_keep_rel, replace)
restore 
