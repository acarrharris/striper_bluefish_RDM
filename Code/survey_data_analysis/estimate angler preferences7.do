
use "$input_data_cd\striper_CE_data.dta", clear
keep if second_choice_protest!=1

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


gen log_striper_length=log(striper_length) if inlist(alt, 1,2)
mvencode log_striper_length, mv(0)
gen sqrt_striper_length=sqrt(striper_length)

gen log_striper_keep=log(1+striper_keep) if inlist(alt, 1,2)
gen log_striper_rel=log(1+striper_rel) if inlist(alt, 1,2)
gen log_bluefish_keep=log(1+bluefish_keep) if inlist(alt, 1,2)
gen log_bluefish_rel=log(1+bluefish_rel) if inlist(alt, 1,2)

mvencode log_striper_keep log_striper_rel log_bluefish_keep log_bluefish_rel, mv(0) override

gen log_striper_keep_log_length=log_striper_keep*log_striper_length
gen log_striper_rel_log_length=log_striper_rel*log_striper_length


gen striper_keep_log_length=striper_keep*log_striper_length
gen striper_rel_log_length=striper_rel*log_striper_length

gen sqrt_striper_keep_log_length=sqrt_striper_keep*log_striper_length
gen sqrt_striper_rel_log_length=sqrt_striper_rel*log_striper_length

clogit choice cost ///
    log_bluefish_keep log_bluefish_rel opt_other opt_out ///
		log_striper_keep  log_striper_keep_log_length ///
    log_striper_rel log_striper_rel_log_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1 &  striper_c_and_r_only==1 [pweight = wt_reg_scaled],  group(gid)	
	
clogit choice cost ///
    bluefish_keep bluefish_rel opt_other opt_out ///
		striper_keep  striper_keep_log_length ///
    striper_rel striper_rel_log_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1 [pweight = wt_reg_scaled],  group(gid)	
	
	
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		sqrt_striper_keep  sqrt_striper_keep_log_length ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1 [pweight = wt_reg_scaled] ,  group(gid)	
	
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		striper_keep  striper_keep_log_length ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1,  group(gid)	
	
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		i.striper_keep ///
		i.striper_keep#c.log_striper_length ///
    sqrt_striper_rel sqrt_striper_rel_log_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
	  opt_out_min_max opt_out_min_max_slot ///
    if second_choice_protest!=1  [pweight = wt_reg_scaled],  group(gid)	
	
gen catch_pos=1 if inlist(alt,1, 2) & (striper_keep>0	| striper_rel>0)
mvencode catch_pos, mv(0)
gen sqrt_striper_catch_length

gen striper_catch=striper_keep+striper_rel

* After your clogit:
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
	sqrt_striper_keep ///
    sqrt_striper_rel  ///
	catch_pos#c.log_striper_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

matrix list e(b)

tempfile wtp_keep
postfile F double striper_length keep_level ///
    wtp se lb ub using `wtp_keep', replace

* Edit this list if your model estimated more/fewer keep levels
foreach K in 1 2 3 {
    foreach L in 18 23 28 33 38 43 {
        local logL = log(`L')

        quietly nlcom ///
            (wtp: - ( ///
                _b[`K'.striper_keep] + ///
                _b[`K'.striper_keep#c.striper_length] * `L' ///
            ) / _b[cost])

        matrix b = r(b)
        matrix V = r(V)

        scalar w  = b[1,1]
        scalar se = sqrt(V[1,1])

        post F (`L') (`K') ///
            (w) (se) (w - 1.96*se) (w + 1.96*se)
    }
}
postclose F


	
use `wtp_keep', clear
label define keeplbl 1 "1 kept fish" 2 "2 kept fish" 3 "3 kept fish"
label values keep_level keeplbl
	
twoway ///
    (rarea lb ub striper_length if keep_level==1, sort color(navy%18) lcolor(navy%35)) ///
    (line  wtp   striper_length if keep_level==1, sort lcolor(navy) lwidth(medthick)) ///
    (rarea lb ub striper_length if keep_level==2, sort color(maroon%18) lcolor(maroon%35)) ///
    (line  wtp   striper_length if keep_level==2, sort lcolor(maroon) lpattern(dash) lwidth(medthick)) ///
    (rarea lb ub striper_length if keep_level==3, sort color(forest_green%18) lcolor(forest_green%35)) ///
    (line  wtp   striper_length if keep_level==3, sort lcolor(forest_green) lpattern(dot) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "1 kept fish" 4 "2 kept fish" 6 "3 kept fish")) ///
    title("WTP for legal kept striped bass by length", size(medium)) ///
    name(wtp_keep_all, replace)

	
twoway ///
    (rarea lb ub striper_length if keep_level==1, sort color(navy%20) lcolor(navy%40)) ///
    (line  wtp   striper_length if keep_level==1, sort lcolor(navy) lwidth(medthick)), ///
    xtitle("Length (inches)") ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    title("WTP for 1 legal kept striped bass", size(medium)) ///
    legend(off) name(wtp_keep1, replace)

twoway ///
    (rarea lb ub striper_length if keep_level==2, sort color(maroon%20) lcolor(maroon%40)) ///
    (line  wtp   striper_length if keep_level==2, sort lcolor(maroon) lwidth(medthick)), ///
    xtitle("Length (inches)") ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    title("WTP for 2 legal kept striped bass", size(medium)) ///
    legend(off) name(wtp_keep2, replace)

twoway ///
    (rarea lb ub striper_length if keep_level==3, sort color(forest_green%20) lcolor(forest_green%40)) ///
    (line  wtp   striper_length if keep_level==3, sort lcolor(forest_green) lwidth(medthick)), ///
    xtitle("Length (inches)") ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    title("WTP for 3 legal kept striped bass", size(medium)) ///
    legend(off) name(wtp_keep3, replace)
	

	
	
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep ///
    sqrt_striper_rel ///
    catch_pos#c.log_striper_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

matrix list e(b)

tempfile wtp_keep_trip
postfile F double striper_length ///
    wtp_keep1 se_keep1 lb_keep1 ub_keep1 ///
    using `wtp_keep_trip', replace

foreach L in 18 23 28 33 38 43 {
    local logL = log(`L')

    quietly nlcom ///
        (wtp_keep1: - ( ///
            _b[sqrt_striper_keep]*sqrt(1) + ///
            _b[1.catch_pos#c.log_striper_length]*`logL' ///
        ) / _b[cost])

    matrix b = r(b)
    matrix V = r(V)

    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])

    post F (`L') ///
        (w) (se) (w-1.96*se) (w+1.96*se)
}
postclose F

use `wtp_keep_trip', clear

twoway ///
    (rarea lb_keep1 ub_keep1 striper_length, sort color(navy%25) lcolor(navy%40)) ///
    (line  wtp_keep1 striper_length, sort lcolor(navy) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(off) ///
    title("WTP for trip with 1 kept striped bass vs no striped bass catch", size(medium)) ///
    name(wtp_keep1_trip, replace)	
	

gen striper_catch = striper_keep + striper_rel
gen sqrt_striper_catch = sqrt(striper_catch)
gen sqrt_striper_catch_log_length = sqrt_striper_catch * log_striper_length
	
gen opt_other_bag	= 	opt_other*striper_bag
gen opt_other_min	= 	opt_other*striper_min
gen opt_other_min_max	= 	opt_other*striper_min*max_dummy
gen opt_other_min_max_slot	= 	opt_other*striper_min*max_dummy*slot_length

					
gen opt_out_bag	= 	opt_out*striper_bag
gen opt_out_min	= 	opt_out*striper_min
gen opt_out_min_max	= 	opt_out*striper_min*max_dummy
gen opt_out_min_max_slot	= 	opt_out*striper_min*max_dummy*slot_length