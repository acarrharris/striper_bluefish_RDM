
global input_data_cd "E:\Lou_projects\striper_bluefish_RDM\input_data" /* Lou's local data path */

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
gen striper_rel_18_28=striper_rel if inlist(striper_length, 18, 23, 28) & striper_rel>0
gen striper_rel_33_43=striper_rel if inlist(striper_length, 33, 38, 43) & striper_rel>0


gen striper_keep_18_23=striper_keep if inlist(striper_length, 18, 23) & striper_keep>0
gen striper_keep_28_33=striper_keep if inlist(striper_length, 28, 33) & striper_keep>0
gen striper_keep_38_43=striper_keep if inlist(striper_length, 38, 43) & striper_keep>0
gen striper_keep_18_33=striper_keep if inlist(striper_length, 18, 23, 28, 33) & striper_keep>0
gen striper_keep_18_28=striper_keep if inlist(striper_length, 18, 23, 28) & striper_keep>0
gen striper_keep_33_43=striper_keep if inlist(striper_length, 33, 38, 43) & striper_keep>0

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
striper_catch_18  striper_catch_28 striper_catch_38 striper_catch_23 striper_catch_33 striper_catch_43 striper_keep_33_43 striper_keep_18_28 striper_rel_18_28 striper_rel_33_43


foreach v of local vars{
	gen sqrt_`v'= sqrt(`v')
}
mvencode striper_rel_* striper_keep_* striper_catch* sqrt_striper_rel_* sqrt_striper_keep_* sqrt_striper_catch_* , mv(0) override

gen striper_keep_18_33_log_length=striper_keep_18_33*log_striper_length
mvencode striper_keep_18_33_log_length, mv(0) override 


foreach L in 18 23 28 33 38 43 {
    gen keep_`L'=striper_keep if striper_length==`L' & striper_keep>0
	gen rel_`L'=striper_rel if striper_length==`L' & striper_rel>0
	mvencode keep_`L' rel_`L', mv(0)

	gen sqrt_keep_`L'=sqrt(keep_`L')
	gen sqrt_rel_`L'=sqrt(rel_`L')

}				

clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    striper_keep ///
	c.striper_keep#c.striper_length ///
    striper_rel ///
	c.striper_rel#c.striper_length ///
	c.striper_keep#c.striper_rel ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

clogit choice cost ///
   		striper_keep_18_23 ///
		striper_keep_28_33 ///
		striper_keep_38_43 ///
		striper_rel_18_23 ///
		striper_rel_28_33 ///
		striper_rel_38_43 ///
		c.striper_keep_18_23#c.striper_rel_18_23 ///
		c.striper_keep_28_33#c.striper_rel_28_33 ///
		c.striper_keep_38_43#c.striper_rel_38_43 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)
	
clogit choice cost ///
   		sqrt_striper_keep_18_23 ///
		sqrt_striper_keep_28_33 ///
		sqrt_striper_keep_38_43 ///
		sqrt_striper_rel_18_23 ///
		sqrt_striper_rel_28_33 ///
		sqrt_striper_rel_38_43 ///
		c.sqrt_striper_keep_18_23#c.sqrt_striper_rel_18_23 ///
		c.sqrt_striper_keep_28_33#c.sqrt_striper_rel_28_33 ///
		c.sqrt_striper_keep_38_43#c.sqrt_striper_rel_38_43 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)
	

	
	
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
clogit choice cost ///
   		sqrt_striper_keep_18_28 ///
		sqrt_striper_keep_33_43 ///
		sqrt_striper_rel_18_28 ///
		sqrt_striper_rel_33_43 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1  [pweight = wt_reg_scaled], group(gid)
	
clogit choice cost ///
sqrt_keep_18 sqrt_keep_23 sqrt_keep_28 sqrt_keep_33 sqrt_keep_38 sqrt_keep_43 sqrt_rel_18 sqrt_rel_23 sqrt_rel_28 sqrt_rel_33 sqrt_rel_38 sqrt_rel_43 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)	
	
clogit choice cost ///
   		sqrt_striper_keep_18_23 ///
		sqrt_striper_keep_28_33 ///
		sqrt_striper_keep_38_43 ///
		sqrt_striper_rel_18_23 ///
		sqrt_striper_rel_28_33 ///
		sqrt_striper_rel_38_43 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1  [pweight = wt_reg_scaled], group(gid)
	
clogit choice cost ///
   		sqrt_striper_keep_18_28 ///
		sqrt_striper_keep_33_43 ///
		sqrt_striper_rel_18_28 ///
		sqrt_striper_rel_33_43 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1  [pweight = wt_reg_scaled], group(gid)
	
	
* Estimate model
clogit choice cost ///
    sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep c.sqrt_striper_keep ///
    sqrt_striper_rel ///
    sqrt_striper_catch_log_length ///
    opt_out_male opt_out_age ///
    opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 ///
    if second_choice_protest!=1, group(gid)

matrix list e(b)
preserve
tempfile wtp_keep
postfile F double striper_length basecatch ///
    wtp_keep se_keep lb_keep ub_keep ///
    using `wtp_keep', replace

* basecatch = total striped bass catch before adding one more kept fish
foreach C in 1 2 {
    foreach L in 18 23 28 33 38 43 {
        local logL = log(`L')

        quietly nlcom ///
            (wtp_keep: - ( ///
                _b[sqrt_striper_keep]/(2*sqrt(`C')) + ///
                _b[sqrt_striper_catch_log_length]*`logL'/(2*sqrt(`C')) ///
            ) / _b[cost])

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar sek = sqrt(V[1,1])

        post F (`L') (`C') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek)
    }
}
postclose F

use `wtp_keep', clear
label define basecatchlbl 1 "Base striped bass catch = 1" 2 "Base striped bass catch = 2"
label values basecatch basecatchlbl


twoway ///
    (rarea lb_keep ub_keep striper_length if basecatch==1, sort color(navy%25) lcolor(navy%40)) ///
    (line  wtp_keep striper_length if basecatch==1, sort lcolor(navy) lwidth(medthick)) ///
    (rarea lb_keep ub_keep striper_length if basecatch==2, sort color(maroon%20) lcolor(maroon%35)) ///
    (line  wtp_keep striper_length if basecatch==2, sort lcolor(maroon) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("Marginal WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Base catch = 1" 4 "Base catch = 2")) ///
    title("MWTP for an additional kept striped bass", size(medium)) ///
    name(wtp_keep, replace)
restore	

preserve	
tempfile wtp_rel
postfile G double striper_length basecatch ///
    wtp_rel se_rel lb_rel ub_rel ///
    using `wtp_rel', replace

* basecatch = total striped bass catch before adding one more released fish
foreach C in 1 2 {
    foreach L in 18 23 28 33 38 43 {
        local logL = log(`L')

        quietly nlcom ///
            (wtp_rel: - ( ///
                _b[sqrt_striper_rel]/(2*sqrt(`C')) + ///
                _b[sqrt_striper_catch_log_length]*`logL'/(2*sqrt(`C')) ///
            ) / _b[cost])

        matrix b = r(b)
        matrix V = r(V)

        scalar wr  = b[1,1]
        scalar ser = sqrt(V[1,1])

        post G (`L') (`C') ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose G

use `wtp_rel', clear
label define basecatchlbl 1 "Base striped bass catch = 1" 2 "Base striped bass catch = 2"
label values basecatch basecatchlbl	

twoway ///
    (rarea lb_rel ub_rel striper_length if basecatch==1, sort color(forest_green%25) lcolor(forest_green%40)) ///
    (line  wtp_rel striper_length if basecatch==1, sort lcolor(forest_green) lwidth(medthick)) ///
    (rarea lb_rel ub_rel striper_length if basecatch==2, sort color(orange%20) lcolor(orange%35)) ///
    (line  wtp_rel striper_length if basecatch==2, sort lcolor(orange) lpattern(dash) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("Marginal WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(order(2 "Base catch = 1" 4 "Base catch = 2")) ///
    title("MWTP for an additional released striped bass", size(medium)) ///
    name(wtp_rel, replace)

restore 


preserve
tempfile wtp_keep01
postfile F double striper_length ///
    wtp se lb ub using `wtp_keep01', replace

foreach L in 18 23 28 33 38 43 {
    local logL = log(`L')

    quietly nlcom ///
        (wtp: - ( ///
            _b[sqrt_striper_keep] + ///
            _b[sqrt_striper_catch_log_length]*`logL' ///
        ) / _b[cost])

    matrix b = r(b)
    matrix V = r(V)

    scalar w  = b[1,1]
    scalar se = sqrt(V[1,1])

    post F (`L') ///
        (w) (se) (w-1.96*se) (w+1.96*se)
}
postclose F

use `wtp_keep01', clear

twoway ///
    (rarea lb ub striper_length, sort color(navy%25) lcolor(navy%40)) ///
    (line  wtp striper_length, sort lcolor(navy) lwidth(medthick)), ///
    xtitle("Length (inches)") ///
    ytitle("WTP ($)") ///
    xlabel(18 23 28 33 38 43) ///
    legend(off) ///
    title("WTP for first kept striped bass (0 → 1)", size(medium)) ///
    name(wtp_keep01, replace)
restore