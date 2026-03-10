
/*This code identifies MRIP sites in MD. I then delineate sites across regulatory regions in the Chespaeake Bay. */
		
set seed $seed 

global calibration_year "(year==2023)"  // For now, use catch data from 2023 as predictions for 2024. Alternative time periods can be tested.
global calibration_year_prev "(year==2023 | year==2022)"  // For now, use catch data from 2023 as predictions for 2024. Alternative time periods can be tested.


cd $input_data_cd

clear
tempfile tl1 cl1
dsconcat $triplist

/*dtrip will be used to estimate total directed trips*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge


keep if $calibration_year_prev

 /* ensure only relevant states */
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

tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace


*keep if inlist(state, "DE", "NC", "NY", "VA")
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

replace mgt="ALL" if mgt==""

drop region
gen region=state+mgt


*browse if mgt==""
drop _merge
destring month1, gen(month2)
gen date2=dmy(day1, month2, year)
format date2 %td


*MD sample site - May 15-31
preserve
keep if $calibration_year
*keep if date2>=td(01jan2023) & date2<=td(28feb2023)

*keep if date2>=td(01mar2023) & date2<=td(31mar2023)

*keep if date2>=td(01apr2023) & date2<=td(15may2023)

*keep if date2>=td(16may2023) & date2<=td(31may2023)

*keep if date2>=td(01jun2023) & date2<=td(15jul2023)

*keep if date2>=td(16jul2023) & date2<=td(31jul2023)

*keep if date2>=td(01aug2023) & date2<=td(10dec2023)

keep if date2>=td(11dec2023) & date2<=td(31dec2023)
/*
*/
*keep if month=="05" & day1>=16
keep if region=="MDALL"
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "stripedbass") 
replace dom_id="1" if strmatch(prim1_common, "stripedbass") 
replace dom_id="1" if strmatch(common, "bluefish") 
replace dom_id="1" if strmatch(prim1_common, "bluefish") 
replace claim=0 if claim==.
gen domain_claim=claim if inlist(common, "stripedbass", "bluefish") 
mvencode domain_claim, mv(0) override
bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")
keep if dom_id=="1"
keep intsite
replace intsite=1866 if intsite==145 //I used a PRESSURE.csv sent to me by R. Kitts-Jensen in 2023 to reclassify site 145, which was not listed in the MRIP site register as of 2026. 
duplicates drop
rename intsite site_external_id
renvarlab, lower
tempfile md_sample
save `md_sample', replace 
restore 

preserve 
import delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\SITE.csv", clear 
*import delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\PRESSURE.csv", clear 
*keep if state=="MD"
*keep site_internal_id site_external_id site_name
duplicates drop 
sort site_external_id
renvarlab, lower
tempfile sites
save `sites', replace
restore 

u `md_sample', clear
merge 1:1 site_external_id using `sites', keep(3) nogen 
keep site_external_id site_internal_id site_lat site_long
renvarlab, upper
*export delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\MD_sample_sites.csv", replace 
export delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\MD_sample_sites_dec11_dec31.csv", replace 





*MD sample site - coastal sites, pull for both years
preserve
keep if region=="MDALL"
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "stripedbass") 
replace dom_id="1" if strmatch(prim1_common, "stripedbass") 
replace dom_id="1" if strmatch(common, "bluefish") 
replace dom_id="1" if strmatch(prim1_common, "bluefish") 
replace claim=0 if claim==.
gen domain_claim=claim if inlist(common, "stripedbass", "bluefish") 
mvencode domain_claim, mv(0) override
bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")
keep if dom_id=="1"
keep intsite
replace intsite=1866 if intsite==145 //I used a PRESSURE.csv sent to me by R. Kitts-Jensen in 2023 to reclassify site 145, which was not listed in the MRIP site register as of 2026. 
duplicates drop
rename intsite site_external_id
renvarlab, lower
tempfile md_sample
save `md_sample', replace 
restore 

preserve 
import delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\SITE.csv", clear 
*import delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\PRESSURE.csv", clear 
*keep if state=="MD"
*keep site_internal_id site_external_id site_name
duplicates drop 
sort site_external_id
renvarlab, lower
tempfile sites
save `sites', replace
restore 

u `md_sample', clear
merge 1:1 site_external_id using `sites', keep(3) nogen 
keep site_external_id site_internal_id site_lat site_long
renvarlab, upper
*export delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\MD_sample_sites.csv", replace 
export delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\MD_sample_sites_all_22_23.csv", replace 




clear
tempfile master
save `master', emptyok

local dates Jan1_Feb28 Mar1_Mar31 Apr1_May15 May16_May31 Jun1_Jul15 Jul16_Jul31 Aug1_Dec10 Dec11_Dec31
foreach d of local dates{
	import delimited using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\md_sites_alloc_`d'.csv", clear 
	gen date="`d'"
	append using `master'
	save `master', replace
	clear                            
}

use `master', clear
drop geometry


preserve
keep if inlist(date, "May16_31")


levelsof site_external_id, local(sites)
tempfile new
save `new', replace 

clear
tempfile master
save `master', emptyok
foreach s of local sites{
	u `new', clear 
	keep if site_ex==`s'
	levelsof nearest_reg_status, local(shore_reg) clean
	levelsof nearest_reg_status_boat, local(boat_reg) clean

	expand 2, gen(dup)
	gen date2=td(16may2023) if dup==1
	replace date2=td(31may2023) if dup==0
	format date2 %td
	
	xtset site_ex date2
	tsfill, full
	gen mode="sh"
	gen reg="`shore_reg'"
	drop dup

	expand 3, gen(dup)
	
	bysort site_ex date2 mode: gen n=_n
	replace mode="pr" if n==2
	replace mode="fh" if n==3

	replace reg= "`boat_reg'" if inlist(mode, "pr", "fh")
	
	drop site_int nearest_reg_status* is_ date dup n
	
	append using `master'
	save `master', replace
	clear                            
}

use `master', clear

tempfile may 
save `may', replace 
restore


preserve
keep if inlist(date, "March1_31")
levelsof site_external_id, local(sites)
tempfile new
save `new', replace 

clear
tempfile master
save `master', emptyok
foreach s of local sites{
	u `new', clear 
	keep if site_ex==`s'
	levelsof nearest_reg_status, local(shore_reg) clean
	levelsof nearest_reg_status_boat, local(boat_reg) clean

	expand 2, gen(dup)
	gen date2=td(01mar2023) if dup==1
	replace date2=td(31mar2023) if dup==0
	format date2 %td
	
	xtset site_ex date2
	tsfill, full
	gen mode="sh"
	gen reg="`shore_reg'"
	drop dup

	expand 3, gen(dup)
	
	bysort site_ex date2 mode: gen n=_n
	replace mode="pr" if n==2
	replace mode="fh" if n==3

	replace reg= "`boat_reg'" if inlist(mode, "pr", "fh")
	
	drop site_int nearest_reg_status* is_ date dup n
	
	append using `master'
	save `master', replace
	clear                            
}

use `master', clear

tempfile march 
save `march', replace 
restore

u `march', clear 
append using `may'
rename site_ex intsite
rename mode mode1
save "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\MD_ches_march_may_sites.dta", replace 



* now, for all of march and from may16-31 (separately) in the calibration year,  estimate the proportion of sh, pr, and fh trips subject to the different regulations

set seed $seed 

global calibration_year "(year==2023)"  // For now, use catch data from 2023 as predictions for 2024. Alternative time periods can be tested.
global calibration_year_prev "(year==2023 | year==2022)"  // For now, use catch data from 2023 as predictions for 2024. Alternative time periods can be tested.


cd $input_data_cd

clear
tempfile tl1 cl1
dsconcat $triplist

/*dtrip will be used to estimate total directed trips*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge

keep if $calibration_year

 /* ensure only relevant states */
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


tostring wave, gen(w2)
tostring year, gen(year2)
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

*keep if inlist(state, "DE", "NC", "NY", "VA")
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


keep if state=="MD" & mgt=="CHES"

 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "stripedbass") 
replace dom_id="1" if strmatch(prim1_common, "stripedbass") 

replace dom_id="1" if strmatch(common, "bluefish") 
replace dom_id="1" if strmatch(prim1_common, "bluefish") 


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "stripedbass", "bluefish") 

mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")
keep if dom_id=="1"
keep if inrange(date2, td(01mar2023), td(31mar2023)) | inrange(date2, td(16may2023), td(31may2023))
drop _merge
merge m:1 date2 mode1 intsite using "E:\Lou_projects\striper_bluefish_RDM\input_data\MD_shapefiles\MD_ches_march_may_sites.dta", keep(1 3)

browse date2 intsite reg mode1
sort intsite
gen period="march" if inrange(date2, td(01mar2023), td(31mar2023)) 
replace period="may" if inrange(date2, td(16may2023), td(31may2023))
replace reg=subinstr(lower(reg)," ","",.)

tostring intsite, gen(intsite2)

gen my_dom_id_string=period+"_"+mode1+"_"+ reg+"_"+intsite2
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))
encode my_dom_id_string, gen(my_dom_id)

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

svy: total dtrip, over(my_dom_id)  
/*
Number of strata = 18                            Number of obs   =        164
Number of PSUs   = 31                          Population size = 179,833.46
															Design df       =         13
	
-----------------------------------------------------------------------------
											|             
											|      Total   			std. err.     [95% conf. interval]
----------------------------+------------------------------------------------
c.dtrip@my_dom_id |
march_pr_catch&releaseonly  |    73592.4   	21821.14       26450.7    120734.1
march_sh_catch&releaseonly  |   19847.83      	    .             .           .
may_fh_opentofishing  			|   9473.036  	 3231.191      2492.473     16453.6
may_pr_opentofishing  			|   51647.85 	  	 23824.07      179.0799    103116.6
may_sh_closedtofishing  		|   17714.42    	      .             .           .
may_sh_opentofishing  			|    7557.93   	       .             .           .
*/

* This tells us that:
	* in March, 100% of all private and shore trips occurred in "catch and release only" regions. No for hire-trips.
	* from May 16-31, 100% of all for-hire and private boat trips occurred in "open to fishing" regions.
	* from May 16-31, 70% (17714.42/(17714.42 +7557.93)) of all shore trips occurred in "closed to fishing" regions. - this could be a an artifact of using the 2025 maps for allocating 2023 trips
	
	













