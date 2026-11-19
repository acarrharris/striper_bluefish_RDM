

cd $input_data_cd

clear

mata: mata clear

tempfile tl1 sl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
*replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
drop if strmatch(id_code, "*xx*")==1
drop if strat_id==""
duplicates drop 
save `tl1'
clear
 
dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if $calibration_year

gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace
destring month, replace
gen date2=dmy(day1, month, year)
format date2 %td

* Ensure only relevant states 
keep if inlist(st, 25, 44, 9, 36, 34, 10, 24, 51, 37, 23, 33)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33


preserve
import excel using "$input_data_cd/SB_MRIP_Management_Areas_DE_NC_NY_VA_reformat.xlsx", clear firstrow
duplicates drop 
drop if intsite==483 & state=="NC" & mgt=="CENTRAL SOUTHERN"
tempfile sites
save `sites', replace
restore
merge m:1 state intsite using `sites', keep(1 3)

*browse if _merge==1
replace mgt = "DE_RIVER_BAY_PRIMARY_AREA" if mgt=="" & state=="DE" & inlist(intsite, 65, 73, 106) 
replace mgt= "CHESAPEAKE BAY" if mgt=="" & state=="VA" & inlist(intsite, 212, 995) 

*rename mgt areas 
replace mgt="ALB"  if mgt=="ALBEMARLE SOUND" 
replace mgt="OCN"  if mgt=="ATLANTIC OCEAN" 
replace mgt="CNTRL"  if mgt=="CENTRAL SOUTHERN" 
replace mgt="CHES"  if mgt=="CHESAPEAKE BAY" 
replace mgt="NANT"  if mgt=="DE_NATICOKE_RIVER_SPAWNING_AREA" 
replace mgt="DERIV"  if mgt=="DE_RIVER_BAY_PRIMARY_AREA" 
replace mgt="HUDN"  if mgt=="HUDSON_NORTH_OF_GW_BRIDGE" 
replace mgt="HUDS"  if mgt=="MARINE_AND_SOUTH_OF_GW_BRIDGE" 
replace mgt="POTO"  if mgt=="POTOMAC RIVER TRIBUTARIES" 

replace mgt="OCN" if state=="DE" & area=="1"

**Deal with Maryland**

* First, identify Potomac River sites. 
* There are a handful of sites along the Potomoac River, but near a striper-regs-map mgt. area
* If a trip is a boat trip, classify it as POT
* If a trip is a shore, classify it as CHES, unless it is one of the few sites where a shore trip would likely be fishing in the POT
replace mgt="POT" if state=="MD" & inlist(intsite, 788, 1969, 1865, 0107, 3076, 3077, 3774, 0233) & inlist(mode1, "pr", "fh")
replace mgt="POT" if state=="MD" & inlist(intsite, 1865) & inlist(mode1, "sh")
replace mgt="CHES" if state=="MD" & inlist(intsite, 788, 1969, 0107, 3076, 3077, 3774, 0233) & inlist(mode1, "sh")

* Next, MD coastal sites
replace mgt="OCN"  if state=="MD" & inlist(intsite,3097, 853, 4419, 24, 856, 1493, 858, 860,  906, 4349, 1658, 152, 1183, 3442)

* Lastly, merge MD march-may sites to regulation region site list 
replace mgt="CHES" if state=="MD"  & mgt==""

replace mgt="ALL" if mgt==""

* create strata for CHES. vs non-CHES
gen waters="CHES" if inlist(mgt, "CHES")
replace waters="N"  if !inlist(mgt, "CHES")  & inlist(state, "ME", "NH", "MA", "RI", "CT", "NY")
replace waters="S" if !inlist(mgt, "CHES")  & inlist(state, "NJ", "DE", "MD", "VA", "NC")

keep if $calibration_year //ensure relevent year
 
drop region
gen region=state+mgt



*New calibration regions
* by state, group areas with similar regs-map
gen mgt_new="ALL" if inlist(state, "ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE")


 * classify catch into the things I care about (common=="s" | "b") and things I don't care about "z" 
gen common_dom="z"
replace common_dom="s" if strmatch(sp_code,"8835020102")
replace common_dom="b" if strmatch(sp_code,"8835250101")


* this might speed things up if I re-classify all length=0 for the species I don't care about 
replace l_cm_bin =0 if !inlist(common_dom, "s", "b")
replace l_in_bin =0 if !inlist(common_dom, "s", "b")

sort year  strat_id psu_id id_code

drop if common_dom=="z"
keep if common_dom=="s"
levelsof region, local(regz)

tempfile base 
save `base', replace

clear 
tempfile master
save `master', emptyok

foreach r of local regz{
	
	u `base', clear 
	keep if region=="`r'"

/*
drop region
gen region="N" if inlist(state, "ME", "NH", "MA", "RI", "CT", "NY")
replace region="S" if inlist(state, "NJ", "DE", "MD", "VA", "NC")
*/
gen my_dom_id_string=region
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)
encode my_dom_id_string, gen(my_dom_id)

svyset psu_id [pweight= wp_size], strata(var_id) singleunit(certainty)


svy: tab l_in my_dom_id_string, count

mat eP=e(Prop)
mat eR=e(Row)'
mat eC=e(Col)
local PopN=e(N_pop)

local mycolnames: colnames(eC)
mat colnames eP=`mycolnames'
	
clear
svmat eP, names(col)
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}
svmat eR
order eR
rename eR l_in_bin

ds l_in_bin, not
renvarlab `r(varlist)', prefix(tab_)
reshape long tab_, i(l_in_bin) j(new) string	

append using `master'
save `master', replace
}

u `master', clear 
/*
split new, parse(_)
gen mode="fh" if inlist(new2, "f", "fh")
replace mode="sh" if inlist(new2, "s", "sh")
replace mode="pr" if inlist(new2, "p", "pr")
rename new1 region

*/
rename new region

rename tab nfish_ab1
sort region  l_in_bin


*for each mgt region, calculate the percent of total harvest that is under- and over-sized
gen min=28 if inlist(region, "MEALL", "NHALL", "MAALL", "RIALL", "CTALL")
replace min=28 if inlist(region, "NYHUDS", "NJALL", "DEOCN", "MDOCN", "VAOCN", "NCOCN")
gen max=31 if inlist(region, "MEALL", "NHALL", "MAALL", "RIALL", "CTALL") 
replace max=31 if inlist(region, "NYHUDS", "NJALL", "DEOCN", "MDOCN", "VAOCN", "NCOCN")

replace min=19 if inlist(region, "MDCHES")
replace min=20 if inlist(region, "MDPOT")
replace max=31 if inlist(region, "MDCHES","MDPOT" )
replace min=18 if inlist(region, "NCALB")
replace max=25 if inlist(region, "NCALB" )
replace min=18 if inlist(region, "NYHUDN")
replace max=28 if inlist(region, "NYHUDN")
replace min=20 if inlist(region, "VACHES", "VAPOTO")
replace max=28 if inlist(region, "VACHES")
replace max=31 if inlist(region, "VAPOTO")

gen under=1 if l_in<min-1
gen over=1 if l_in>max+1
gen legal=1 if l_in>=min-1 & l_in<=max+1
collapse (sum) nfish, by(region under legal over)
replace under=under*nfish
replace legal=legal*nfish
replace over=over*nfish

collapse (sum) under legal over, by(region)
egen total=rowtotal(under legal over)

gen pct_under=under/total
gen pct_legal=legal/total
gen pct_over=over/total
merge 1:m region using "$iterative_input_data_cd\simulated_catch_totals_final.dta"
gen no_measured_sizes =1 if _merge==2
sort region draw

gen tot_str_keep_sim_under =tot_str_keep_sim*pct_under
gen tot_str_keep_sim_legal =tot_str_keep_sim*pct_legal
gen tot_str_keep_sim_over =tot_str_keep_sim*pct_over

mvencode tot_str_keep_sim* no_measured_sizes, mv(0) override
keep region draw mode tot_str_keep_sim tot_str_cat_sim tot_str_rel_sim tot_blu_keep_sim tot_blu_rel_sim tot_blu_cat_sim tot_dtrip_sim no_measured_sizes tot_str_keep_sim_under tot_str_keep_sim_legal tot_str_keep_sim_over
drop _merge
save "$iterative_input_data_cd\simulated_catch_totals_by_size_final.dta", replace
export delimited using "$iterative_input_data_cd\simulated_catch_totals_by_size_final.csv", replace


*gen l_in_bin=l_cm_bin/2.54
gen size_class="a_u24" if l_in_bin<24
replace size_class="b_24_26" if l_in_bin>=24 & l_in_bin<27
replace size_class="c_27_32" if l_in_bin>=27 & l_in_bin<32
replace size_class="d_o32" if l_in_bin>=32 
drop if nfish_ab1==0

sort region  l_in_bin

bysort region :egen min_l_in=min(l_in_bin)
collapse (sum) nfish_ab1, by(region  size min_l_in)
sort region  size
egen sum=sum(nfish_ab1), by(region min_l_in)
gen prop_ab1=nfish_ab1/sum
sort region size
drop nfish sum
reshape wide prop, i(region min) j(size) string
mvencode prop*, mv(0) override
merge 1:m region using "$iterative_input_data_cd\simulated_catch_totals_final.dta"

gen tot_str_keep_sim_u24 =tot_str_keep_sim*prop_ab1a_u24
gen tot_str_keep_sim_24_26 =tot_str_keep_sim*prop_ab1b_24_26
gen tot_str_keep_sim_27_32 =tot_str_keep_sim*prop_ab1c_27_32
gen tot_str_keep_sim_o32 =tot_str_keep_sim*prop_ab1d_o32

gen no_measured_sizes =1 if _merge==2
sort region draw

mvencode tot_str_keep_sim* no_measured_sizes, mv(0) override
drop _merge

save "$iterative_input_data_cd\simulated_catch_totals_by_size_final.dta", replace
export delimited using "$iterative_input_data_cd\simulated_catch_totals_by_size_final.csv", replace



import delimited using "$input_data_cd/baseline_catch_at_length.csv", clear 
keep if species=="str"
gen mode="pr"
expand 2, gen(dup)
replace mode="sh" if dup==1
drop dup
expand 2 if mode=="sh", gen(dup)
replace mode="fh" if dup==1
drop dup

gen l_in_bin=length/2.54

gen size_class="a_u24" if l_in_bin<24
replace size_class="b_24_26" if l_in_bin>=24 & l_in_bin<27
replace size_class="c_27_32" if l_in_bin>=27 & l_in_bin<32
replace size_class="d_o32" if l_in_bin>=32 
collapse (sum) observed_prob_ab1 fitted_prob_ab1, by(mode region draw size)
reshape wide observed_prob_ab1 fitted_prob_ab1, i(region draw mode) j(size) string

merge 1:1 region draw mode using "$iterative_input_data_cd\simulated_catch_totals_final.dta", keep(3) nogen

gen tot_str_keep_sim_u24 =tot_str_keep_sim*observed_prob_ab1a_u24
gen tot_str_keep_sim_24_26 =tot_str_keep_sim*observed_prob_ab1b_24_26
gen tot_str_keep_sim_27_32 =tot_str_keep_sim*observed_prob_ab1c_27_32
gen tot_str_keep_sim_o32 =tot_str_keep_sim*observed_prob_ab1d_o32

gen tot_str_rel_sim_u24 =tot_str_rel_sim*fitted_prob_b2a_u24
gen tot_str_rel_sim_24_26 =tot_str_rel_sim*fitted_prob_b2b_24_26
gen tot_str_rel_sim_27_32 =tot_str_rel_sim*fitted_prob_b2c_27_32
gen tot_str_rel_sim_o32 =tot_str_rel_sim*fitted_prob_b2d_o32
drop fitted_prob*

save "$iterative_input_data_cd\simulated_catch_totals_by_size_final.dta", replace
export delimited using "$iterative_input_data_cd\simulated_catch_totals_by_size_final.csv", replace

