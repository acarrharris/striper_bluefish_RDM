

/****************************************************************************************
 estimate_angler_preferences_refactored.do
 Purpose: Estimate striped bass / bluefish choice models and produce WTP plots.
 Refactor notes:
   - Centralizes data preparation, weights, sample definitions, and common model options.
   - Keeps model specifications in short globals.
   - Provides reusable Krinsky-Robb routines for the preferred centered specification.
   - Appendix models reuse the same prepared dataset instead of reloading/recreating variables.
****************************************************************************************/

version 18
clear all
set more off
set seed 03211990


global input_data_cd  "E:/Lou_projects/striper_bluefish_RDM/input_data"
global output_cd      "C:/Users/andrew.carr-harris/Desktop/Git/striper_bluefish_RDM/Code/pre_sim"
cd "$output_cd"


* Common programs

capture program drop prep_striper_ce
program define prep_striper_ce
    syntax [, IFCOND(string)]

    use "$input_data_cd/striper_CE_data.dta", clear

    * Core transformations
    gen double log_striper_length  = log(striper_length) if inlist(alt, 1, 2)
    gen double sqrt_striper_length = sqrt(striper_length)
    mvencode log_striper_length sqrt_striper_length, mv(0) override

    gen double striper_catch      = striper_keep + striper_rel
    gen double sqrt_striper_catch = sqrt(striper_catch)

    gen double striper_length2 = striper_length^2

    * Length interactions
    gen double sqrt_striper_keep_log_length  = sqrt_striper_keep  * log_striper_length
    gen double sqrt_striper_rel_log_length   = sqrt_striper_rel   * log_striper_length
    gen double sqrt_striper_keep_sqrt_length = sqrt_striper_keep  * sqrt_striper_length
    gen double sqrt_striper_rel_sqrt_length  = sqrt_striper_rel   * sqrt_striper_length

    gen double sqrt_striper_catch_log_length  = sqrt_striper_catch * log_striper_length
    gen double sqrt_striper_catch_sqrt_length = sqrt_striper_catch * sqrt_striper_length
    gen double sqrt_striper_catch_length      = sqrt_striper_catch * striper_length
    gen double sqrt_striper_catch_length2     = sqrt_striper_catch * striper_length2

    gen double sqrt_striper_keep_length  = sqrt_striper_keep * striper_length
    gen double sqrt_striper_keep_length2 = sqrt_striper_keep * striper_length2
    gen double sqrt_striper_rel_length   = sqrt_striper_rel  * striper_length
    gen double sqrt_striper_rel_length2  = sqrt_striper_rel  * striper_length2

    * Survey version
    capture confirm variable version
    if !_rc {
        capture drop svy_version version1-version4
        split version, parse(" ")
        rename version1 svy_version
        capture drop version2-version4
    }

    * Length-bin variables
    foreach typ in keep rel {
        local base = cond("`typ'" == "keep", "striper_keep", "striper_rel")

        gen double striper_`typ'_18_23 = `base' if inlist(striper_length, 18, 23)           & `base' > 0
        gen double striper_`typ'_28_33 = `base' if inlist(striper_length, 28, 33)           & `base' > 0
        gen double striper_`typ'_38_43 = `base' if inlist(striper_length, 38, 43)           & `base' > 0
        gen double striper_`typ'_18_28 = `base' if inlist(striper_length, 18, 23, 28)       & `base' > 0
        gen double striper_`typ'_18_33 = `base' if inlist(striper_length, 18, 23, 28, 33)   & `base' > 0
        gen double striper_`typ'_18_38 = `base' if inlist(striper_length, 18, 23, 28, 33, 38) & `base' > 0
        gen double striper_`typ'_33_43 = `base' if inlist(striper_length, 33, 38, 43)       & `base' > 0
    }

    gen double striper_keep_23_38 = striper_keep if inlist(striper_length, 23, 28, 33, 38) & striper_keep > 0
    gen double striper_keep_23_33 = striper_keep if inlist(striper_length, 23, 28, 33)     & striper_keep > 0

    local binvars striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 ///
                  striper_rel_18_23  striper_rel_28_33  striper_rel_38_43  ///
                  striper_keep_18_28 striper_keep_18_33 striper_keep_18_38 ///
                  striper_keep_33_43 striper_keep_23_38 striper_keep_23_33 ///
                  striper_rel_18_28  striper_rel_18_33  striper_rel_18_38  ///
                  striper_rel_33_43

    foreach v of local binvars {
        gen double sqrt_`v' = sqrt(`v')
    }

    * Exact length variables
    foreach L in 18 23 28 33 38 43 {
        gen double keep_`L'  = striper_keep  if striper_length == `L' & striper_keep  > 0
        gen double rel_`L'   = striper_rel   if striper_length == `L' & striper_rel   > 0
        gen double catch_`L' = striper_catch if striper_length == `L' & (striper_keep > 0 | striper_rel > 0)

        mvencode keep_`L' rel_`L' catch_`L', mv(0) override
        gen double sqrt_striper_keep_`L'  = sqrt(keep_`L')
        gen double sqrt_striper_rel_`L'   = sqrt(rel_`L')
        gen double sqrt_striper_catch_`L' = sqrt(catch_`L')
    }

    mvencode striper_rel_* striper_keep_* striper_catch* ///
             sqrt_striper_rel_* sqrt_striper_keep_* sqrt_striper_catch_*, mv(0) override

    gen double sqrt_kp_18_33_length      = sqrt_striper_keep_18_33 * striper_length
    gen double sqrt_kp_18_38_length      = sqrt_striper_keep_18_38 * striper_length
    gen double sqrt_kp_18_33_log_length  = sqrt_striper_keep_18_33 * log_striper_length
    gen double sqrt_kp_23_33_log_length  = sqrt_striper_keep_23_33 * log_striper_length
    gen double sqrt_kp_18_38_log_length  = sqrt_striper_keep_18_38 * log_striper_length
    gen double sqrt_kp_18_33_sqrt_length = sqrt_striper_keep_18_33 * sqrt_striper_length
    gen double sqrt_kp_18_38_sqrt_length = sqrt_striper_keep_18_38 * sqrt_striper_length
    gen double sqrt_kp_18_33_length2     = sqrt_striper_keep_18_33 * striper_length2
    gen double sqrt_kp_18_38_length2     = sqrt_striper_keep_18_38 * striper_length2

    * State-mode weights
    gen str2 main_mode_str = ""
    replace main_mode_str = "sh" if inlist(mode_main, 1)
    replace main_mode_str = "pr" if inlist(mode_main, 2, 3)
    replace main_mode_str = "fh" if inlist(mode_main, 4, 5)
    gen str8 st_mode = lic_st + "_" + main_mode_str

    preserve
        clear
        input str5 st_mode target_share
        "CT_fh" .000921
        "CT_pr" .0254005
        "CT_sh" .0259132
        "DE_fh" .0000171
        "DE_pr" .0045613
        "DE_sh" .0137618
        "MA_fh" .0027537
        "MA_pr" .0656039
        "MA_sh" .0895793
        "MD_fh" .0046252
        "MD_pr" .048189
        "MD_sh" .0243315
        "ME_fh" .0005505
        "ME_pr" .0207001
        "ME_sh" .0401165
        "NC_fh" .0021628
        "NC_pr" .0264514
        "NC_sh" .0761711
        "NH_fh" .0001924
        "NH_pr" .0080706
        "NH_sh" .0138468
        "NJ_fh" .0014005
        "NJ_pr" .0780562
        "NJ_sh" .1227003
        "NY_fh" .0020241
        "NY_pr" .0849696
        "NY_sh" .145641
        "RI_fh" .0002559
        "RI_pr" .0174389
        "RI_sh" .0257576
        "VA_fh" .000365
        "VA_pr" .0169814
        "VA_sh" .0104897
        end
        tempfile targets
        save `targets', replace
    restore

    * Estimation sample used for weights and preferred models
    global base_exclusions no_choice_protest != 1 & alt3_choice_protest != 1 & ///
        alt1_choice_protest != 1 & alt2_choice_protest != 1

    global required_vars !missing(choice, cost, sqrt_striper_keep, sqrt_striper_rel, ///
        sqrt_bluefish_keep, sqrt_bluefish_rel, opt_other, opt_out, ///
        opt_other_likely, opt_other_days_str_blue12, opt_other_days_non_str_blue12, ///
        opt_out_male, opt_out_age)

    gen byte est_samp = ($base_exclusions & $required_vars)

    preserve
        keep if est_samp
        bysort qtid: keep if _n == 1
        contract st_mode
        rename _freq n
        egen double total = total(n)
        gen double sample_share = n / total
        merge 1:1 st_mode using `targets'

        * Leave unmatched sample cells missing; these should be inspected.
        gen double weight_state_md = target_share / sample_share if _merge == 3
        egen double wsum = total(weight_state_md * n)
        egen double N    = total(n)
        gen double weight_state_md_norm = weight_state_md * (N / wsum)

        keep st_mode weight_state_md_norm _merge
        tempfile weights
        save `weights', replace
    restore

    merge m:1 st_mode using `weights', nogen

    * Centered length terms used in preferred specification
    quietly summarize striper_length if est_samp & inlist(alt, 1, 2) & striper_keep > 0, meanonly
    scalar mean_keep_L = r(mean)

    quietly summarize striper_length if est_samp & inlist(alt, 1, 2) & striper_rel > 0, meanonly
    scalar mean_rel_L = r(mean)

    quietly summarize log_striper_length if est_samp & inlist(alt, 1, 2) & striper_keep > 0, meanonly
    scalar mean_keep_logL = r(mean)

    quietly summarize log_striper_length if est_samp & inlist(alt, 1, 2) & striper_rel > 0, meanonly
    scalar mean_rel_logL = r(mean)

    gen double c_striper_kp_length  = striper_length - scalar(mean_keep_L) if inlist(alt,1,2) & striper_keep > 0
    gen double c_striper_kp_length2 = c_striper_kp_length^2
    gen double c_striper_rl_length  = striper_length - scalar(mean_rel_L)  if inlist(alt,1,2) & striper_rel  > 0
    gen double c_striper_kp_log_length = log(striper_length) - scalar(mean_keep_logL) if inlist(alt,1,2) & striper_keep > 0
    gen double c_striper_rl_log_length = log(striper_length) - scalar(mean_rel_logL)  if inlist(alt,1,2) & striper_rel  > 0

    mvencode c_striper_kp_length c_striper_kp_length2 c_striper_rl_length ///
             c_striper_kp_log_length c_striper_rl_log_length, mv(0) override

    gen double sqrt_striper_keep_length_c      = sqrt_striper_keep * c_striper_kp_length
    gen double sqrt_striper_keep_length2_c     = sqrt_striper_keep * c_striper_kp_length2
    gen double sqrt_striper_kp_log_length_c    = sqrt_striper_keep * c_striper_kp_log_length
    gen double sqrt_striper_rel_length_c       = sqrt_striper_rel  * c_striper_rl_length
    gen double sqrt_striper_rel_log_length_c   = sqrt_striper_rel  * c_striper_rl_log_length

    cmset qtid scenario alt
    sort qtid scenario alt

    if "`ifcond'" != "" {
        replace est_samp = est_samp & (`ifcond')
    }

    tempfile prepared
    save `prepared', replace
end


capture program drop store_fit_stats
program define store_fit_stats, eclass
    * Add consistent reporting statistics after cmxtmixlogit.
    quietly count if e(sample)
    estadd scalar N_obs = r(N)

    preserve
        keep if e(sample)
        bysort qtid: keep if _n == 1
        quietly count
        estadd scalar n_anglers = r(N)
    restore

    capture estadd scalar aic = -2*e(ll) + 2*e(k)
    capture estadd scalar bic = -2*e(ll) + log(e(N))*e(k)
end


capture program drop run_mxl
program define run_mxl
    syntax name(name=estname), RHS(string) [IFCOND(string) WEIGHTED]

    local wgt ""
    if "`weighted'" != "" local wgt "[pweight = weight_state_md_norm]"

    local ifuse "if est_samp"
    if "`ifcond'" != "" local ifuse "if est_samp & (`ifcond')"

    eststo `estname': cmxtmixlogit choice cost `rhs' `ifuse' `wgt', ///
        random($random_terms) noconstant $max_options

    store_fit_stats
    estimates save "`estname'", replace
end


capture program drop make_kr_draws_base
program define make_kr_draws_base
    syntax, ESTimate(string) OUTfile(string) [NDRAWS(integer 5000)]

    estimates use "`estimate'.ster"
    tempname bfull Vfull b V cholV iid beta

    local params
    local cnames : colnames e(b)
    foreach p of local cnames {
        if strpos("`p'", "sd(") == 0 local params `params' `p'
    }

    global krfiles
    quietly forvalues x = 1/`ndraws' {
        matrix `bfull' = e(b)
        local K = e(k)
        matrix `b' = `bfull'[1, 1..`K']

        matrix `Vfull' = e(V)
        matrix `V' = `Vfull'[1..`K', 1..`K']
        matrix `cholV' = cholesky(`V')

        matrix `iid' = J(`K', 1, 0)
        forvalues i = 1/`K' {
            matrix `iid'[`i', 1] = rnormal()
        }

        matrix `beta' = `b'' + `cholV' * `iid'

        clear
        set obs 1
        gen int draw = `x'

        foreach p of local params {
            local j = colnumb(`bfull', "`p'")
            gen double b_`p' = `beta'[`j', 1]
        }

        forvalues L = 18/44 {
            local Lc_keep = `L' - scalar(mean_keep_L)
            local Lc2_keep = (`L' - scalar(mean_keep_L))^2
            local logLc_rel = log(`L') - scalar(mean_rel_logL)

            gen double keep_`L' = - (b_sqrt_striper_keep + ///
                b_sqrt_striper_keep_length_c * `Lc_keep' + ///
                b_sqrt_striper_keep_length2_c * `Lc2_keep') / (2 * b_cost)

            gen double rel_`L' = - (b_sqrt_striper_rel + ///
                b_sqrt_striper_rel_log_length_c * `logLc_rel') / (2 * b_cost)
        }

        tempfile kr`x'
        save `kr`x'', replace
        global krfiles "$krfiles `kr`x''"
    }

    clear
    dsconcat $krfiles
    keep draw keep_* rel_*
    reshape long keep_ rel_, i(draw) j(length)
    rename keep_ mwtp_keep
    rename rel_  mwtp_rel

    collapse (p50) keep_med=mwtp_keep rel_med=mwtp_rel ///
             (p5)  keep_lb=mwtp_keep  rel_lb=mwtp_rel  ///
             (p95) keep_ub=mwtp_keep  rel_ub=mwtp_rel, by(length)

    save "`outfile'", replace
end


capture program drop plot_base_wtp
program define plot_base_wtp
    syntax using/

    use "`using'", clear

    twoway ///
        (rcap keep_lb keep_ub length if inlist(length, 18, 23, 28, 33, 38, 43), lcolor(navy)) ///
        (scatter keep_med length if inlist(length, 18, 23, 28, 33, 38, 43), ///
            mcolor(navy) msymbol(circle) msize(small) connect(l) lcolor(navy)), ///
        legend(off) xtitle("Length (inches)", size(small)) ytitle("WTP ($)", size(small)) ///
        title("WTP for harvestable striped bass", size(medium)) ///
        xlabel(18(5)43, labsize(small)) ylabel(#20, labsize(small)) ///
        graphregion(color(white)) plotregion(color(white)) name(mwtp_keep_base, replace)

    twoway ///
        (rcap rel_lb rel_ub length if inlist(length, 18, 23, 28, 33, 38, 43), lcolor(maroon)) ///
        (scatter rel_med length if inlist(length, 18, 23, 28, 33, 38, 43), ///
            mcolor(maroon) msymbol(circle) msize(small) connect(l) lcolor(maroon)), ///
        legend(off) xtitle("Length (inches)", size(small)) ytitle("WTP ($)", size(small)) ///
        title("WTP for released striped bass", size(medium)) ///
        xlabel(18(5)43, labsize(small)) ylabel(#20, labsize(small)) ///
        graphregion(color(white)) plotregion(color(white)) name(mwtp_rel_base, replace)

    graph combine mwtp_keep_base mwtp_rel_base, cols(2) ycommon ///
        graphregion(color(white)) name(mwtp_base, replace)
end


*-----------------------------*
* 2. Prepare data
*-----------------------------*
prep_striper_ce

* Common model pieces
global random_terms sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out
global max_options difficult intmethod(halton) intpoints(500) technique(bhhh 15 nr 5) iterate(25) nrtolerance(1e-3)

global opt_controls opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 opt_out_male opt_out_age

global spec_m0 sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out $opt_controls

global spec_mD_centered ///
    sqrt_striper_keep_length_c sqrt_striper_keep_length2_c ///
    sqrt_striper_rel_log_length_c ///
    $opt_controls

global spec_mD_uncentered ///
    sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
    sqrt_striper_keep_length sqrt_striper_keep_length2 ///
    sqrt_striper_rel_log_length ///
    $opt_controls

*-----------------------------*
* 3. Main model specifications
*-----------------------------*

run_mxl m0_cmmx_wts,   rhs("$spec_m0")         weighted
run_mxl mD_cmmx_wts_c, rhs("$spec_mD_centered") weighted

* Optional main alternatives; uncomment as needed after confirming variable names.
* run_mxl m0_cmmx_wts_c, rhs("sqrt_striper_keep sqrt_striper_rel sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out $opt_controls") weighted
* run_mxl mD_cmmx_wts,   rhs("$spec_mD_uncentered") weighted

esttab m0_cmmx_wts mD_cmmx_wts_c using "regression_results_main.csv", replace ///
    nobase noomitted se b(%9.3f) label nogaps ///
    starlevels(* 0.10 ** 0.05 *** 0.010) ///
    stats(N_obs n_anglers ll aic bic, fmt(0 0 3 3 3))

*-----------------------------*
* 4. Preferred-model WTP curves
*-----------------------------*

make_kr_draws_base, estimate("mD_cmmx_wts_c") outfile("mD_cmmx_wts_c_wtp.dta") ndraws(5000)
plot_base_wtp using "mD_cmmx_wts_c_wtp.dta"
graph export "mD_cmmx_wts_c_wtp.png", replace width(2400)

*-----------------------------*
* 5. Heterogeneity specifications
*-----------------------------*

* 5a. Catch-and-release-only anglers
capture drop cr_sqrt_*
gen byte cr_only = striper_c_and_r_only if !missing(striper_c_and_r_only)

gen double cr_sqrt_str_kp      = cr_only * sqrt_striper_keep
gen double cr_sqrt_str_kp_L_c  = cr_only * sqrt_striper_keep_length_c
gen double cr_sqrt_str_kp_L2_c = cr_only * sqrt_striper_keep_length2_c
gen double cr_sqrt_str_rel     = cr_only * sqrt_striper_rel
gen double cr_sqrt_str_rel_log_L = cr_only * sqrt_striper_rel_log_length_c

run_mxl mD_wts_c_cr, rhs("$spec_mD_centered cr_sqrt_str_kp cr_sqrt_str_kp_L_c cr_sqrt_str_kp_L2_c cr_sqrt_str_rel cr_sqrt_str_rel_log_L") weighted

* 5b. Fishing skill: beginner omitted
capture drop skill3 skill_intermediate skill_advprof int_sqrt_* adv_sqrt_*
gen byte skill3 = .
replace skill3 = 1 if fishing_skill == 1
replace skill3 = 2 if fishing_skill == 2
replace skill3 = 3 if inlist(fishing_skill, 3, 4)

gen byte skill_intermediate = (skill3 == 2) if !missing(skill3)
gen byte skill_advprof      = (skill3 == 3) if !missing(skill3)

foreach g in int adv {
    local flag = cond("`g'" == "int", "skill_intermediate", "skill_advprof")
    gen double `g'_sqrt_str_kp      = `flag' * sqrt_striper_keep
    gen double `g'_sqrt_str_kp_L_c  = `flag' * sqrt_striper_keep_length_c
    gen double `g'_sqrt_str_kp_L2_c = `flag' * sqrt_striper_keep_length2_c
    gen double `g'_sqrt_str_rel     = `flag' * sqrt_striper_rel
    gen double `g'_sqrt_str_rel_log_L = `flag' * sqrt_striper_rel_log_length_c
}

run_mxl mD_wts_c_skill, rhs("$spec_mD_centered int_sqrt_str_kp int_sqrt_str_kp_L_c int_sqrt_str_kp_L2_c int_sqrt_str_rel int_sqrt_str_rel_log_L adv_sqrt_str_kp adv_sqrt_str_kp_L_c adv_sqrt_str_kp_L2_c adv_sqrt_str_rel adv_sqrt_str_rel_log_L") weighted

* 5c. Primary mode: shore omitted, private/rental and for-hire interactions
capture drop mode_motorized mode_forhire pr_sqrt_* fh_sqrt_*
gen byte mode_motorized = inlist(mode_main, 2, 3) if !missing(mode_main)
gen byte mode_forhire   = inlist(mode_main, 4, 5) if !missing(mode_main)

foreach g in pr fh {
    local flag = cond("`g'" == "pr", "mode_motorized", "mode_forhire")
    gen double `g'_sqrt_str_kp      = `flag' * sqrt_striper_keep
    gen double `g'_sqrt_str_kp_L_c  = `flag' * sqrt_striper_keep_length_c
    gen double `g'_sqrt_str_kp_L2_c = `flag' * sqrt_striper_keep_length2_c
    gen double `g'_sqrt_str_rel     = `flag' * sqrt_striper_rel
    gen double `g'_sqrt_str_rel_log_L = `flag' * sqrt_striper_rel_log_length_c
}

run_mxl mD_wts_c_mode, rhs("$spec_mD_centered pr_sqrt_str_kp pr_sqrt_str_kp_L_c pr_sqrt_str_kp_L2_c pr_sqrt_str_rel pr_sqrt_str_rel_log_L fh_sqrt_str_kp fh_sqrt_str_kp_L_c fh_sqrt_str_kp_L2_c fh_sqrt_str_rel fh_sqrt_str_rel_log_L") weighted

esttab mD_wts_c_cr mD_wts_c_skill mD_wts_c_mode using "regression_results_heterogeneity.csv", replace ///
    nobase noomitted se b(%9.3f) label nogaps ///
    starlevels(* 0.10 ** 0.05 *** 0.010) ///
    stats(N_obs n_anglers ll aic bic, fmt(0 0 3 3 3))

*-----------------------------*
* 6. Appendix / robustness specifications
*-----------------------------*

* Alternative protest exclusion: only remove protest of alternatives 3 or 4
gen byte est_samp_prot2 = (no_choice_protest != 1 & alt3_choice_protest != 1 & $required_vars)

run_mxl mD_cmmx_wts_c_prot2, rhs("$spec_mD_centered") ifcond("est_samp_prot2") weighted

* Unweighted version
run_mxl mD_cmmx_nowts_c, rhs("$spec_mD_centered")

* Additional opt-out demographics
global opt_controls_alldems $opt_controls opt_out_fishing_skill_3_4 opt_out_educ_coll opt_out_educ_grad opt_out_inc_med opt_out_inc_high
run_mxl mD_cmmx_alldems_c, rhs("sqrt_striper_keep_length_c sqrt_striper_keep_length2_c sqrt_striper_rel_log_length_c opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 opt_out_male opt_out_age opt_out_fishing_skill_3_4 opt_out_educ_coll opt_out_educ_grad opt_out_inc_med opt_out_inc_high") weighted

run_mxl mD_cmmx_educ_c, rhs("sqrt_striper_keep_length_c sqrt_striper_keep_length2_c sqrt_striper_rel_log_length_c opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12 opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad") weighted

esttab mD_cmmx_wts_c_prot2 mD_cmmx_nowts_c mD_cmmx_alldems_c mD_cmmx_educ_c ///
    using "regression_results_appendix.csv", replace nobase noomitted ///
    se b(%9.3f) label nogaps starlevels(* 0.10 ** 0.05 *** 0.010) ///
    stats(N_obs n_anglers ll aic bic, fmt(0 0 3 3 3))

display as text "Done: model estimation, WTP output, and result tables completed."
