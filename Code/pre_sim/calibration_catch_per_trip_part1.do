


/*This code uses the MRIP data to: 
 
	* Part A)  
		1) estimate mean harvest-, discards-, and catch-per-trip and their standard errors at the state, wave, and fishing mode level in the calibration period
		2) For some combinations of state-wave-mode, there is only a single PSU and thus no standard error available for the mean estimates. In these cases, I impute 
			a standard error based on other recent data or difference levels of aggregation. 
		3) Once a mean and standard errors have been estimated for all strata, I save the file and run the "copula_loop.R" in R. This file simulates random 
			draws of harvest and discards-per trip, accounting for possible intra-species correlation in harvest and discards
			
	* Part B)  
		1) Compute catch'harvest totals by state/mode/etc.in the caliabration year to compare with simulated caliabration-year fishery data 
	
*/
		
************** Part A  **************
set seed $seed

* Pull in MRIP data

cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")


* Format MRIP data for estimation 

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


keep if $calibration_year_prev
 
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
replace waters="CSTL"  if !inlist(mgt, "CHES")


* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZZ"
replace common_dom="STR" if inlist(common, "stripedbass") 
replace common_dom="STR" if inlist(common, "bluefish") 

replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="STR"  if inlist(prim1_common, "bluefish") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string0=state+"_"+waters+"_"+yr2+"_"+month1+"_"+mode1+"_"+common_dom

* Define the list of species to process
local species "stripedbass bluefish"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "stripedbass" local short "str"
    if "`s'" == "bluefish"     local short "blu"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)
	drop `short'_tot_cat
	
    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)
	drop `short'_harvest

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
	drop `short'_releases

}

rename sum_str_tot_cat str_cat
rename sum_str_harvest str_keep
rename sum_str_releases str_rel
rename sum_blu_tot_cat blu_cat
rename sum_blu_harvest blu_keep
rename sum_blu_releases blu_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "stripedbass")==0
replace no_dup=1 if strmatch(common, "bluefish")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="STR"

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

/*
local vars sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel  scup_catch scup_keep scup_rel
foreach v of local vars{
	replace `v'=round(`v')
}
*/

drop if wp_int==0
encode my_dom_id_string0, gen(my_dom_id0)

preserve
keep if $calibration_year
drop _merge
tempfile basefile
save `basefile', replace 
restore

* Here I will estimate mean catch/harvest/discards per trip for each strata in order to identify strata with missing SE

/* For strata with missing SE's, I'll follow similar approch to MRIP's hot and cold deck imputation for observations with missing lengths and weights

From the MRIP data handbook:

"For intercepted angler trips with landings where both length and weight measurements are missing, paired length and weight observations are imputed from complete cases using hot and cold deck imputation. (Complete cases include records with both length and weight data available, as well as records where we were able to compute a missing length or weight using the length-weight modeling described above.) Up to five rounds of imputation are conducted in an attempt to fill in missing values. These rounds begin with imputation cells that correspond to the most detailed MRIP estimation cells, but are aggregated to higher levels in subsequent rounds to bring in more length-weight data. 
	- Round 1: Current year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	- Round 2: Current year, half-year, sub-region, state, mode, species. 
	- Round 3: Current + most recent prior year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	- Round 4: Current + most recent prior year, sub-region, state, mode, species. 
	- Round 5: Current + most recent prior year, sub-region, species."
	
* The calibration estimation strata is: state+"_"+waters+"_"+yr2+"_"+month1+"_"+mode1+"_"+common_dom

* For strata with missing, I'll impute a PSE from other strata and apply it to the missing-SE strata. 
	- Round 1: (current year) state+"_"+waters+"_"+"_"+WAVE+"_"+"_"+mode1
	- Round 2: (current + MOST RECENT PRIOR YEAR) state+"_"+waters+"_"+month1 +"_"+kod+"_"+mode1
	- Round 3: (current + MOST RECENT PRIOR YEAR) state+"_"+waters+"_"+"_"+WAVE +"_"+kod+"_"+mode1

* If after Round 3, there is still no available PSE for strata with missing values, then set the SE=MEAN (high uncertainty)
*/


/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh), for each round of SE-imputation*/
*gen my_dom_id_string0=state+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id
*replace my_dom_id_string0=ltrim(rtrim(my_dom_id_string0))

gen my_dom_id_string1=state+"_"+waters+"_"+yr2+"_"+wv2+"_"+mode1+"_"+ common_dom
replace my_dom_id_string1=ltrim(rtrim(my_dom_id_string1))

gen my_dom_id_string2=state+"_"+waters+"_"+month1+"_"+mode1+"_"+ common_dom
replace my_dom_id_string2=ltrim(rtrim(my_dom_id_string2))

gen my_dom_id_string3=state+"_"+waters+"_"+wv2+"_"+mode1+"_"+ common_dom
replace my_dom_id_string3=ltrim(rtrim(my_dom_id_string3))

drop my_dom_id0

tempfile base 
save `base', replace 

clear
tempfile master
save `master', emptyok
		
		
local domainz 0 1 2 3

foreach d of local domainz{
	
	u `base', clear 
	*local d 0
/* total with over(<overvar>) requires a numeric variable */
	encode my_dom_id_string`d', gen(my_dom_id`d')

preserve
keep my_dom_id`d' my_dom_id_string`d'
duplicates drop 
keep my_dom_id`d'  my_dom_id_string`d'
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

tempfile base_data
save `base_data', replace 

tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in str_keep str_rel str_cat blu_keep blu_rel blu_cat  {

    * Run svy mean for the variable by domain
    svy: mean `var', over(my_dom_id`d')

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id`d'
merge m:1 my_dom_id`d' using `domains' 

keep varname mean se my_dom_id`d' my_dom_id_string`d'
gen pse=(se/mean)*100

append using `master'
save `master', replace
clear                            
}

use `master', clear

order var my* mean pse
split my_dom_id_string0, parse(_)

rename my_dom_id_string01 state
rename my_dom_id_string02 waters
rename my_dom_id_string03 year
rename my_dom_id_string04 month1
rename my_dom_id_string05 mode
rename my_dom_id_string06 dom_id
drop if my_dom_id_string0!="" & year!="2023"

destring month1, gen(month)
gen wave=1 if inlist(month, 1, 2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5, 6)
replace wave=4 if inlist(month, 7, 8)
replace wave=5 if inlist(month, 9, 10)
replace wave=6 if inlist(month, 11, 12)
tostring wave, gen(w2)
drop wave

preserve
keep if my_dom_id_string0!=""
drop my_dom_id_string1 my_dom_id_string2 my_dom_id_string3 my_dom_id1 my_dom_id2 my_dom_id3 

gen my_dom_id_string1=state+"_"+waters+"_"+year+"_"+w2+"_"+mode+"_"+ dom_id
replace my_dom_id_string1=ltrim(rtrim(my_dom_id_string1))

gen my_dom_id_string2=state+"_"+waters+"_"+month1+"_"+mode+"_"+ dom_id
replace my_dom_id_string2=ltrim(rtrim(my_dom_id_string2))

gen my_dom_id_string3=state+"_"+waters+"_"+w2+"_"+mode+"_"+ dom_id
replace my_dom_id_string3=ltrim(rtrim(my_dom_id_string3))

keep varname my_dom_id_string0* mean pse se
tempfile round0
save `round0', replace
restore 

preserve
keep if my_dom_id_string1!=""
drop my_dom_id_string0 my_dom_id_string2 my_dom_id_string3
drop  state year waters month1 month  mode dom_id 
split my_dom_id_string1, parse(_)
rename my_dom_id_string11 state
rename my_dom_id_string12 waters
rename my_dom_id_string13 year
rename my_dom_id_string14 wave
rename my_dom_id_string15 mode
rename my_dom_id_string16 dom_id

destring year, replace
keep if $calibration_year
expand 2, gen(dup)
bysort state waters year wave  mode dom_id var: gen tab=_n
gen month=1 if wave=="1" & tab==1
replace month=2 if wave=="1" & tab==2
replace month=3 if wave=="2" & tab==1
replace month=4 if wave=="2" & tab==2
replace month=5 if wave=="3" & tab==1
replace month=6 if wave=="3" & tab==2
replace month=7 if wave=="4" & tab==1
replace month=8 if wave=="4" & tab==2
replace month=9 if wave=="5" & tab==1
replace month=10 if wave=="5" & tab==2
replace month=11 if wave=="6" & tab==1
replace month=12 if wave=="6" & tab==2
gen month1 = string(month,"%02.0f")
tostring year, gen(year2)

gen my_dom_id_string0=state+"_"+waters+"_"+year2+"_"+month1+"_"+mode+"_"+ dom_id
keep my_dom_id_string0 pse varname 
duplicates drop 
rename pse pse_round1
tempfile round1
save `round1', replace
restore 

preserve
keep if my_dom_id_string2!=""
drop my_dom_id_string0 my_dom_id_string1 my_dom_id_string3
drop  state waters year month1 month  mode dom_id w2
split my_dom_id_string2, parse(_)
rename my_dom_id_string21 state
rename my_dom_id_string22 waters
rename my_dom_id_string23 month
rename my_dom_id_string24 mode
rename my_dom_id_string25 dom_id
gen year="2023"

gen my_dom_id_string0=state+"_"+waters+"_"+year+"_"+month+"_"+mode+"_"+ dom_id
keep my_dom_id_string0 pse varname
rename pse pse_round2
duplicates drop 
tempfile round2
save `round2', replace
restore 

preserve
keep if my_dom_id_string3!=""
drop my_dom_id_string0 my_dom_id_string1 my_dom_id_string2
drop  state waters year month1 month  mode dom_id w2
split my_dom_id_string3, parse(_)
rename my_dom_id_string31 state
rename my_dom_id_string32 waters
rename my_dom_id_string33 wave
rename my_dom_id_string34 mode
rename my_dom_id_string35 dom_id
gen year="2023"
expand 2, gen(dup)
bysort state waters year wave  mode dom_id varname: gen tab=_n
gen month=1 if wave=="1" & tab==1
replace month=2 if wave=="1" & tab==2
replace month=3 if wave=="2" & tab==1
replace month=4 if wave=="2" & tab==2
replace month=5 if wave=="3" & tab==1
replace month=6 if wave=="3" & tab==2
replace month=7 if wave=="4" & tab==1
replace month=8 if wave=="4" & tab==2
replace month=9 if wave=="5" & tab==1
replace month=10 if wave=="5" & tab==2
replace month=11 if wave=="6" & tab==1
replace month=12 if wave=="6" & tab==2
gen month1 = string(month,"%02.0f")

gen my_dom_id_string0=state+"_"+waters+"_"+year+"_"+month1+"_"+mode+"_"+ dom_id
keep my_dom_id_string0 pse varname
rename pse pse_round3
tempfile round3
save `round3', replace
restore 

u `round0', clear 
merge 1:1 my_dom_id_string0 varname using `round1', keep(1 3)
rename _merge merge1
merge 1:1 my_dom_id_string0 varname using `round2', keep(1 3)
rename _merge merge2
merge 1:1 my_dom_id_string0 varname using `round3', keep(1 3)
rename _merge merge3

split my_dom_id_string0, parse(_)
rename my_dom_id_string01 state
rename my_dom_id_string02 waters
rename my_dom_id_string03 year
rename my_dom_id_string04 month1
rename my_dom_id_string05 mode
drop my_dom_id_string06

drop merge*

* 5.6% of the strata with positive mean catch-per-trip does not have a standard error

replace pse=pse_round1 if pse==. | pse==100
replace pse=pse_round2 if pse==. | pse==100
replace pse=pse_round3 if pse==. | pse==100

* after imputation, 1.7% of the strata still has missing standard errors. Set the pse's for these strata to 100
replace pse=100 if pse==.
replace se= (pse/100)*mean
drop pse* 

* Stop code if non-value mean harvest/discards/catch-per trip are missing standard errors
* Check condition across the dataset
summarize if mean != 0 & missing(se)

* If any observations meet the condition, stop
if r(N) > 0 {
    display "Stopping: mean is not zero and se is missing for some observations."
    exit 1
}


sort my_dom_id_string var
keep varname my mean se 
reshape wide mean se , i(my) j(varname) string

* make indicator variables for whether each domain contains keep, discards, or keep and discards of each species 
mvencode meanstr_keep meanstr_rel  meanblu_keep meanblu_rel, mv(0) override

gen blu_only_keep=1 if meanblu_keep>0 & meanblu_rel==0
gen blu_only_rel=1 if meanblu_rel>0 & meanblu_keep==0
gen blu_keep_and_rel=1 if meanblu_rel>0 & meanblu_keep>0
gen blu_no_catch=1 if meanblu_rel==0 & meanblu_keep==0

gen str_only_keep=1 if meanstr_keep>0 & meanstr_rel==0
gen str_only_rel=1 if meanstr_rel>0 & meanstr_keep==0
gen str_keep_and_rel=1 if meanstr_rel>0 & meanstr_keep>0
gen str_no_catch=1 if meanstr_rel==0 & meanstr_keep==0


mvencode blu_only_keep blu_only_rel blu_keep_and_rel blu_no_catch str_only_keep str_only_rel str_keep_and_rel str_no_catch, mv(0) override

merge 1:m my_dom_id_string0 using `basefile'

*condition for when keep and release are both positive for a stratum, but they never occur on the same trip
*Will model these distributions as independent
gen tab=1 if blu_keep>0 & blu_keep!=. & blu_rel>0 & blu_rel!=.
egen sumtab=sum(tab), by(my_dom_id_string)
gen blu_keep_and_rel_ind=1 if blu_keep_and_rel==1 & sumtab==0
replace blu_keep_and_rel=0 if blu_keep_and_rel_ind==1
drop tab sumtab

gen tab=1 if str_keep>0 & str_keep!=. & str_rel>0 & str_rel!=.
egen sumtab=sum(tab), by(my_dom_id_string)
gen str_keep_and_rel_ind=1 if str_keep_and_rel==1 & sumtab==0
replace str_keep_and_rel=0 if str_keep_and_rel_ind==1
drop tab sumtab

*condition for when keep and release are both positive for a stratum, but occured together on only one trip so that the correlation==1.
*Will model these distributions as independent
*bluefish
gen perfect_corr=.
levelsof my_dom_id_string if blu_keep_and_rel==1, local(doms)
foreach d of local doms{
di "`d'" 
egen rank_keep = rank(blu_keep) if my_dom_id_string=="`d'" 
egen rank_rel  = rank(blu_rel) if my_dom_id_string=="`d'" 
count if  my_dom_id_string=="`d'" 
if `r(N)'>1{
corr rank_keep rank_rel if my_dom_id_string=="`d'"  [aw=wp_int]
if `r(rho)'==1 | `r(rho)'==. {
	replace perfect_corr=1 if my_dom_id_string=="`d'"
}
}
	drop rank*

}

replace blu_keep_and_rel=0 if blu_keep_and_rel==1 & perfect_corr==1
replace blu_keep_and_rel_ind=1 if perfect_corr==1

*condition for perfect correlation in pseudo observations (rank-based empirical CDF)
replace blu_keep_and_rel=0 if my_dom_id_string=="VA_CSTL_2023_08_sh_STR"
replace blu_keep_and_rel_ind=1 if my_dom_id_string=="VA_CSTL_2023_08_sh_STR"


drop perfect_corr

*striper
gen perfect_corr=.
levelsof my_dom_id_string if str_keep_and_rel==1, local(doms)
foreach d of local doms{
di "`d'" 
egen rank_keep = rank(str_keep) if my_dom_id_string=="`d'" 
egen rank_rel  = rank(str_rel) if my_dom_id_string=="`d'" 
count if  my_dom_id_string=="`d'" 
if `r(N)'>1{
corr rank_keep rank_rel if my_dom_id_string=="`d'"   [aw=wp_int]
if `r(rho)'==1 | `r(rho)'==. {
	replace perfect_corr=1 if my_dom_id_string=="`d'" 
}
}
	drop rank*

}

replace str_keep_and_rel=0 if str_keep_and_rel==1 & perfect_corr==1
replace str_keep_and_rel_ind=1 if perfect_corr==1
drop perfect_corr

rename my_dom_id_string0 my_dom_id_string
split my_dom_id_string, parse(_)
replace state=my_dom_id_string1
replace waters=my_dom_id_string2
replace yr2=my_dom_id_string3
replace month1=my_dom_id_string4
replace mode1=my_dom_id_string5


keep wp_int my_dom_id_string meanblu_cat-id_code year str_cat-blu_rel blu_keep_and_rel_ind str_keep_and_rel_ind state waters mode1 month 

mvencode se*, mv(0) override
export excel "$input_data_cd\baseline_mrip_catch_processed.xlsx", firstrow(variables) replace

import excel using "$input_data_cd\baseline_mrip_catch_processed.xlsx", clear first


************** Part B  **************
* Compute MRIP estimates for comparison with simulated estimates 

* Estimates by state mode
set seed $seed

* Pull in MRIP data

cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")

* Format MRIP data for estimation 
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
replace waters="CSTL"  if !inlist(mgt, "CHES")

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZZ"
replace common_dom="STR" if inlist(common, "stripedbass") 
replace common_dom="STR" if inlist(common, "bluefish") 

replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="STR"  if inlist(prim1_common, "bluefish") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

* Define the list of species to process
local species "stripedbass bluefish"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "stripedbass" local short "str"
    if "`s'" == "bluefish"     local short "blu"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)
	drop `short'_tot_cat
	
    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)
	drop `short'_harvest

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
	drop `short'_releases

}

rename sum_str_tot_cat str_cat
rename sum_str_harvest str_keep
rename sum_str_releases str_rel
rename sum_blu_tot_cat blu_cat
rename sum_blu_harvest blu_keep
rename sum_blu_releases blu_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "stripedbass")==0
replace no_dup=1 if strmatch(common, "bluefish")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

gen my_dom_id_string=state+"_"+waters+"_"+mode1+"_"+common_dom
encode my_dom_id_string, gen(my_dom_id)

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n
keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="STR"

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
drop if wp_int==0



preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in str_keep str_rel str_cat blu_keep blu_rel blu_cat  {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 

keep varname mean se ll95 ul95  my_dom_id_string
rename mean tot
split my, parse("_")
rename my_dom_id_string1 state
rename my_dom_id_string2 waters
rename my_dom_id_string3 mode 
rename my_dom_id_string4 common_dom 
keep if common=="STR"
drop common
drop my_dom

save "$input_data_cd\mrip_catch_by_state_mode.dta", replace 

* Estimates by state mode month
set seed $seed

* Pull in MRIP data

cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")

* Format MRIP data for estimation 
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
replace waters="CSTL"  if !inlist(mgt, "CHES")

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZZ"
replace common_dom="STR" if inlist(common, "stripedbass") 
replace common_dom="STR" if inlist(common, "bluefish") 

replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="STR"  if inlist(prim1_common, "bluefish") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

* Define the list of species to process
local species "stripedbass bluefish"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "stripedbass" local short "str"
    if "`s'" == "bluefish"     local short "blu"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)
	drop `short'_tot_cat
	
    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)
	drop `short'_harvest

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
	drop `short'_releases

}

rename sum_str_tot_cat str_cat
rename sum_str_harvest str_keep
rename sum_str_releases str_rel
rename sum_blu_tot_cat blu_cat
rename sum_blu_harvest blu_keep
rename sum_blu_releases blu_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "stripedbass")==0
replace no_dup=1 if strmatch(common, "bluefish")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

gen my_dom_id_string=state+"_"+waters+"_"+mode1+"_"+common_dom+"_"+month1
encode my_dom_id_string, gen(my_dom_id)

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n
keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="STR"

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
drop if wp_int==0



preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in str_keep str_rel str_cat blu_keep blu_rel blu_cat  {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 

keep varname mean se ll95 ul95  my_dom_id_string
rename mean tot
split my, parse("_")
rename my_dom_id_string1 state
rename my_dom_id_string2 waters
rename my_dom_id_string3 mode 
rename my_dom_id_string4 common_dom 
rename my_dom_id_string5 month 

keep if common=="STR"
drop common
drop my_dom

save "$input_data_cd\mrip_catch_by_state_mode_month.dta", replace 




* Estimates by region mode month
set seed $seed

* Pull in MRIP data

cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")

* Format MRIP data for estimation 
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

drop region 
gen region=state+mgt

* create strata for CHES. vs non-CHES
gen waters="CHES" if inlist(mgt, "CHES")
replace waters="CSTL"  if !inlist(mgt, "CHES")

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZZ"
replace common_dom="STR" if inlist(common, "stripedbass") 
replace common_dom="STR" if inlist(common, "bluefish") 

replace common_dom="STR"  if inlist(prim1_common, "stripedbass") 
replace common_dom="STR"  if inlist(prim1_common, "bluefish") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

* Define the list of species to process
local species "stripedbass bluefish"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "stripedbass" local short "str"
    if "`s'" == "bluefish"     local short "blu"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)
	drop `short'_tot_cat
	
    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)
	drop `short'_harvest

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
	drop `short'_releases

}

rename sum_str_tot_cat str_cat
rename sum_str_harvest str_keep
rename sum_str_releases str_rel
rename sum_blu_tot_cat blu_cat
rename sum_blu_harvest blu_keep
rename sum_blu_releases blu_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "stripedbass")==0
replace no_dup=1 if strmatch(common, "bluefish")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

gen my_dom_id_string=region+"_"+mode1+"_"+common_dom+"_"+month1
encode my_dom_id_string, gen(my_dom_id)

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n
keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
keep if common_dom=="STR"

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
drop if wp_int==0



preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in str_keep str_rel str_cat blu_keep blu_rel blu_cat  {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 

keep varname mean se ll95 ul95  my_dom_id_string
rename mean tot
split my, parse("_")
rename my_dom_id_string1 region
rename my_dom_id_string2 mode
rename my_dom_id_string4 month 
rename my_dom_id_string3 common_dom 
drop my_dom_id_string 
keep if common=="STR"
drop common

save "$input_data_cd\mrip_catch_by_region_mode_month.dta", replace 
