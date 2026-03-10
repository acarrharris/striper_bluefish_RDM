****************************************************
* ASMFC STB/Bluefish CE – Prep for logit (with key)
* Files:
*   Sample.csv
*   SurveyData.csv
*   CE_variables_key.csv   (design matrix; merge on version)
****************************************************
cd "C:\Users\andrew.carr-harris\Desktop\Git\striper_bluefish_RDM\survey_data"

clear all
set more off

*---------------------------------------------------
* 1) Import sample + survey and merge
*---------------------------------------------------
import delimited using "Sample.csv", clear 
tempfile sample
save `sample', replace

import delimited using "SurveyData.csv", clear 

* Ensure qtid is numeric for merge
capture confirm numeric variable qtid
if _rc destring qtid, replace ignore(" ")

merge m:1 qtid using `sample', keep(3) nogen

*---------------------------------------------------
* 2) Basic recodes (edit to taste)
*---------------------------------------------------
rename a1 fishing_skill
rename a2 days_any_salt12
rename a3 stb_blu_target3y
rename a4 pct_days_stb_blu
rename a5 mode_main
rename a6 stb_harvest_type
rename a7 blu_harvest_type
rename a8 likely_next12

rename d1 gender
rename d2 educ
rename d3 inc
rename d4 year_born

gen byte likely_to_fish = inlist(likely_next12, 1, 2) if !missing(likely_next12)
gen byte male = (gender==1) if !missing(gender)

gen double survey_dt = date(surveydate, "MDY")
format survey_dt %td
gen int survey_yr = year(survey_dt)
gen int age = survey_yr - year_born if !missing(survey_yr, year_born)

gen byte educ_coll = inlist(educ,3,4,5) if !missing(educ)
gen byte educ_grad = inlist(educ,6,7,8) if !missing(educ)
gen byte educ_coll_grad= inlist(educ,3,4,5, 6, 7, 8) if !missing(educ)


gen byte inc_med  = inlist(inc,4,5,6) if !missing(inc)
gen byte inc_high = (inc>=7)          if !missing(inc)

* impute # days past 12 months fishing that were for striped bass 
* first check the percentile of total days 12
tab days_any_salt12  
su days_any_salt12, detail  

egen p995days_any_salt12 = pctile(days_any_salt12), p(99.5)
tab fishing_skill if days_any_salt12>p995days_any_salt12, missing
replace days_any_salt12 =. if days_any_salt12>p995days_any_salt12 // replace as missing if total_trips_12 above the 99.5 percentile (200 days) because I don't trust that response

tab days_any_salt12, missing
gen days_str_blue12 = (pct_days_stb_blu/100) * days_any_salt12

gen byte fishing_skill_1_2= inlist(fishing_skill, 1, 2) & !missing(fishing_skill)
gen byte fishing_skill_3_4=inlist(fishing_skill, 3, 4) & !missing(fishing_skill)



* Eligibility
gen byte eligible = (stb_blu_target3y != 4) if !missing(stb_blu_target3y)

*---------------------------------------------------
* 3) Reshape choices wide -> long (6 scenarios)
*---------------------------------------------------

forvalues s = 1/6 {
    capture confirm variable b`s'_first
    if !_rc rename b`s'_first b`s'_first
    capture confirm variable b`s'_second
    if !_rc rename b`s'_second b`s'_second
}

drop block token selectionprob samplingweight
preserve
keep qtid version ///
     eligible ///
     fishing_skill* days_any_salt12 days_str_blue12 pct_days_stb_blu mode_main ///
     stb_harvest_type blu_harvest_type likely_next12 likely_to_fish ///
     male age educ* inc* 
tempfile ind_vars
save `ind_vars'	, replace 
restore 
keep qtid b1_first b2_first b3_first b4_first b5_first b6_first ///
     b1_second b2_second b3_second b4_second b5_second b6_second

renvarlab *first, postdrop(6)
renvarlab b1 b2 b3 b4 b5 b6, prefix(first_)
renvarlab *second, postdrop(7)
renvarlab b1 b2 b3 b4 b5 b6, prefix(second_)
reshape long first second, i(qtid) j(scenario) string

replace scenario =substr(scenario, 3, 1)
destring scenario, replace
merge m:1 qtid using `ind_vars', keep(3) nogen


* Create 4 alternatives per scenario
expand 4
bysort qtid scenario: gen byte alt = _n   // 1..4

gen byte choice  = (first==alt)  if !missing(first)
replace choice = 0 if missing(choice)

gen byte choice2 = (second==alt) if !missing(second)
replace choice2 = 0 if missing(choice2)

egen long gid = group(qtid scenario)
order qtid gid version scenario alt first second choice choice2

* Sanity check: exactly one first-choice per choice set (when answered)
bysort gid: egen nchoice = total(choice)
assert nchoice==1 if !missing(first)
replace version = strtrim(version)


*---------------------------------------------------
* 4) Merge in alternative-specific CE attributes from key
*---------------------------------------------------
preserve
    import delimited using "CE_variables_key.csv", clear varn(1) 
	sort version block question alt
	drop mode
    * Align names to your long file keys:
	replace question=question-8
	rename question scenario

    * Clean version strings (important if there are trailing spaces)
    replace version = strtrim(version)

    tempfile key
    save `key', replace
restore

replace version = strtrim(version)

merge m:1 version scenario alt using `key', keep(1 3) nogen

* Post-merge checks: no missing attributes in estimation sample
foreach v in cost striper_length bluefish_length striper_keep striper_rel bluefish_keep bluefish_rel opt_out fish_other_species mode {
    assert !missing(`v') if !missing(first)
}

rename fish_other_species opt_other


keep if eligible==1


* Identify "protest" responses
*a) never chose a fishing alternative
egen sum_choices=sum(choice), by(qtid)

*drop if no choice experiment questions answered
drop if sum_choices==0

*drop if choice experiment question not answered (after answer at least one)
drop if first==.


order sum
gen choice_alt_4=1 if choice==1 & alt==4
egen sum_choice_alt_4=sum(choice_alt_4), by(qtid)
order sum_choices choice_alt_4 sum_choice_alt_4
gen no_choice_protest=1 if sum_choices==sum_choice_alt_4 & sum_choices>2
order no_choice_protest
tab sum_choices if no_choice_protest==1
mvencode no_choice_protest, mv(0)
sort qtid scenario alt
tab sum_choices if no_choice_protest==1
distinct qtid if no_choice_protest==1

* b) only chose the "other" fishing alternative
gen choice_alt_3=1 if choice==1 & alt==3
egen sum_choice_alt_3=sum(choice_alt_3), by(qtid)
gen alt3_choice_protest=1 if sum_choices==sum_choice_alt_3 & sum_choices>2
order sum_choice_alt_3 alt3_choice_protest //no alt3 only choice
sort qtid scenario alt
tab sum_choices if alt3_choice_protest==1
distinct qtid if alt3_choice_protest==1

* c) always chose option A fishing alternative
gen choice_alt_1=1 if choice==1 & alt==1
egen sum_choice_alt_1=sum(choice_alt_1), by(qtid)
gen alt1_choice_protest=1 if sum_choices==sum_choice_alt_1 & sum_choices>2
order sum_choice_alt_1 alt1_choice_protest //no alt3 only choice
browse if alt1_choice_protest==1
distinct qtid if alt1_choice_protest==1
tab sum_choices if alt1_choice_protest==1

* c) always chose option B fishing alternative
gen choice_alt_2=1 if choice==1 & alt==2
egen sum_choice_alt_2=sum(choice_alt_2), by(qtid)
gen alt2_choice_protest=1 if sum_choices==sum_choice_alt_2 & sum_choices>2
order sum_choice_alt_2 alt2_choice_protest //no alt3 only choice
distinct qtid if alt2_choice_protest==1
tab sum_choices if alt2_choice_protest==1

distinct qtid if no_choice_protest==1 | alt3_choice_protest==1 |  alt1_choice_protest==1 | alt2_choice_protest==1

drop sum_choice_alt_2 sum_choice_alt_1 sum_choice_alt_3 sum_choices choice_alt_4 choice_alt_3 choice_alt_1 choice_alt_2 sum_choice_alt_4



*interactions
foreach v in fishing_skill_1_2 fishing_skill_3_4  days_any_salt12 days_str_blue12  likely_to_fish male age educ_coll educ_grad educ_coll_grad inc_med inc_high {
				  	
    capture confirm variable `v'
    if !_rc gen opt_out_`v' = opt_out * `v'
	if !_rc gen opt_other_`v' = opt_other * `v'

}

foreach v in striper_keep striper_rel  bluefish_keep bluefish_rel {
			gen sqrt_`v'=sqrt(`v')
	 }

	 
	 
****************************************************
* Example estimation
****************************************************
clogit choice cost striper_keep striper_rel striper_length bluefish_keep bluefish_rel bluefish_length opt_other opt_out,  ///
      group(gid) vce(cluster qtid)
	  
clogit choice cost striper_keep striper_rel striper_length bluefish_keep bluefish_rel bluefish_length opt_other opt_out  ///
	  if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
      group(gid) vce(cluster qtid)
distinct qtid if e(sample)	  

	  
clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out, ///
     group(gid) vce(cluster qtid)	  

clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)	

distinct qtid if e(sample)	  
	
* candidate variables to interact with opt_out (demographics):
	* opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_inc_med opt_out_inc_high
	
* candidate variables to interatc with opt_other (fishing variables):
 * opt_other_fishing_skill  opt_other_days_str_blue12 opt_other_likely_to_fish
 
clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_inc_med opt_out_inc_high ///
	opt_other_fishing_skill_3_4  opt_other_days_str_blue12 opt_other_likely_to_fish ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)	
	
clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_inc_med opt_out_inc_high ///
	opt_other_male opt_other_age opt_other_educ_coll opt_other_educ_grad opt_other_inc_med opt_other_inc_high ///
	opt_other_fishing_skill_3_4    opt_other_days_str_blue12 opt_other_likely_to_fish ///
	opt_out_fishing_skill_3_4  opt_out_days_str_blue12 opt_out_likely_to_fish ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		
	
clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_inc_med opt_out_inc_high ///
	opt_out_fishing_skill_3_4  opt_out_days_str_blue12 opt_out_likely_to_fish ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		

clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_other_male opt_other_age opt_other_educ_coll opt_other_educ_grad opt_other_inc_med opt_other_inc_high ///
	opt_other_fishing_skill_3_4  opt_other_days_str_blue12 opt_other_likely_to_fish ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		

clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll opt_out_educ_grad opt_out_fishing_skill_3_4  ///
	opt_other_likely opt_other_days_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)			
	
clogit choice cost sqrt_striper_keep sqrt_striper_rel striper_length sqrt_bluefish_keep sqrt_bluefish_rel bluefish_length opt_other opt_out ///
	opt_out_male opt_out_age opt_out_educ_coll_grad opt_out_fishing_skill_3_4  ///
	opt_other_likely opt_other_days_str_blue12  ///
	if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1, ///
	group(gid) vce(cluster qtid)		


gen total_str_keep_length=striper_length*striper_keep
gen total_str_rel_length=striper_length*striper_rel
gen total_blu_keep_length=bluefish_length*bluefish_keep
gen total_blu_rel_length=bluefish_length*bluefish_rel	

su total_str_keep_length if inlist(alt, 1, 2)  & e(sample)
local total_str_keep_length=`r(sum)'

su striper_keep if inlist(alt, 1, 2)  & e(sample)
local total_str_keep=`r(sum)'

local c_len_striper_kept = len_striper_keep -`total_str_keep_length'/`total_str_keep'

su striper_length if inlist(alt, 1, 2) & e(sample)
local c_len_striper_released = len_striper_rel - r(mean)

su bluefish_length if inlist(alt, 1, 2) & e(sample)
local c_len_blue_kept = len_blue_keep - r(mean)

su bluefish_length if inlist(alt, 1, 2) & e(sample)
local c_len_blue_released = len_blue_rel - r(mean)

gen c_striper_keep_length= striper_keep*`c_len_striper_kept'
gen c_striper_rel_length=  striper_rel*`c_len_striper_released'
gen c_blue_keep_length=  blue_keep*`c_len_blue_kept'
gen c_blue_rel_length=  blue_rel*`c_len_blue_released'	
	
	
****************************************************






	  