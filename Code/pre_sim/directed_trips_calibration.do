
/*This code uses the MRIP data to 
	1) estimate dirrected trips and their standard error during the calibration period, 
	2) use those paramters to create random draws of directed trips for each stratum,
	3) divide each random draw by the number of days in that stratum to obtain an estimate of trips-per-day, 
	4) compute for each stratum a calendar year adjustment = (# of calendar days in that stratum for the projection period)/(# of calendar days in that stratum for the calibration period). 
		We will multiply projection results by this scalar to account for differences in the number of calibration- versus projection-year calendar days in each stratum
	5) set the baseline year and projection year regulations ("$input_code_cd/set regulations.do")
*/
		
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

* store prim --> prim_common key for trip cost calculation 
preserve 
keep prim1 prim1_common
duplicates drop 
drop if prim1=="" | prim1_common==""
duplicates tag prim1, gen(dup)
*browse if dup>0
drop if dup>0
drop dup
save "$input_data_cd/prim1.dta", replace
restore 

preserve 
keep prim2 prim2_common
duplicates drop 
drop if prim2=="" | prim2_common==""
duplicates drop 
duplicates tag prim2, gen(dup)
*browse if dup>0
drop if dup>0
drop dup
save "$input_data_cd/prim2.dta", replace
restore 


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
destring month, replace
gen date2=dmy(day1, month, year)
format date2 %td


* Assign managament areas
preserve
import excel using "$input_data_cd/SB_MRIP_Management_Areas_DE_NC_NY_VA_reformat.xlsx", clear firstrow
duplicates drop 
drop if intsite==483 & state=="NC" & mgt=="CENTRAL SOUTHERN"
tempfile sites
save `sites', replace
restore
merge m:1 state intsite using `sites', keep(1 3)

replace mgt = "DE_RIVER_BAY_PRIMARY_AREA" if mgt=="" & state=="DE" & inlist(intsite, 65, 73, 106) 
replace mgt= "CHESAPEAKE BAY" if mgt=="" & state=="VA" & inlist(intsite, 212, 995) 

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

* Deal with Maryland management areas

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


drop _merge

tempfile base1
save `base1', replace 

levelsof region /*if state=="MA"*/, local(sts)
foreach s of local sts{
	*local s="DEDERIV"
	u `base1', clear 
	
	keep if region=="`s'"

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

* generate estimation strata
* HERE is where to use an MRIP site list to determine managament regions


/* For strata with missing SE's, I'll follow similar approch to MRIP's hot and cold deck imputation for observations with missing lengths and weights

From the MRIP data handbook:

"For intercepted angler trips with landings where both length and weight measurements are missing, paired length and weight observations are imputed from complete cases using hot and cold deck imputation. (Complete cases include records with both length and weight data available, as well as records where we were able to compute a missing length or weight using the length-weight modeling described above.) Up to five rounds of imputation are conducted in an attempt to fill in missing values. These rounds begin with imputation cells that correspond to the most detailed MRIP estimation cells, but are aggregated to higher levels in subsequent rounds to bring in more length-weight data. 
	- Round 1: Current year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	- Round 2: Current year, half-year, sub-region, state, mode, species. 
	- Round 3: Current + most recent prior year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	- Round 4: Current + most recent prior year, sub-region, state, mode, species. 
	- Round 5: Current + most recent prior year, sub-region, species."
	
* The calibration estimation strata is: state+"_"+year2+"_"+month1+"_"+kod+"_"+mode1

* For strata with missing, I'll impute a PSE from other strata and apply it to the missing-SE strata. 
	- Round 1: (current year) state+"_"+year2+"_"+WAVE+"_"+kod+"_"+mode1
	- Round 2: (current + most recent prior year) state+"_"+year2+"_"+month1 +"_"+kod+"_"+mode1
	- Round 3: (current + most recent prior year) state+"_"+year2+"_"+WAVE +"_"+kod+"_"+mode1

* If after Round 3, there is still no available PSE for strata with missing values, then set the SE=MEAN (high uncertainty)
*/

/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh), for each round of SE-imputation*/
gen my_dom_id_string0=region+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id
replace my_dom_id_string0=ltrim(rtrim(my_dom_id_string0))

gen my_dom_id_string1=region+"_"+year2+"_"+w2+"_"+kod+"_"+mode1+"_"+ dom_id
replace my_dom_id_string1=ltrim(rtrim(my_dom_id_string1))

gen my_dom_id_string2=region+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id
replace my_dom_id_string2=ltrim(rtrim(my_dom_id_string2))

gen my_dom_id_string3=region+"_"+w2+"_"+kod+"_"+mode1+"_"+ dom_id
replace my_dom_id_string3=ltrim(rtrim(my_dom_id_string3))

tempfile base 
save `base', replace 

clear
tempfile master
save `master', emptyok
		
		
local domainz 0 1 2 3

foreach d of local domainz{
	
	u `base', clear 

/* total with over(<overvar>) requires a numeric variable */
	encode my_dom_id_string`d', gen(my_dom_id`d')

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

preserve
keep my_dom_id`d' my_dom_id_string`d'
duplicates drop 
tostring my_dom_id`d', gen(my_dom_id`d'_2)
keep my_dom_id`d'_2  my_dom_id_string`d'
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

tempfile base_data
save `base_data', replace 

svy: total dtrip, over(my_dom_id`d')  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id`d'_2
merge 1:1 my_dom_id`d'_2 using `domains'
drop rname my_dom_id`d'_2 _merge 
order my_dom_id_string`d'

keep my b se  ll ul
gen pse=(se/b)*100


append using `master'
save `master', replace
clear                            
}

use `master', clear

order my*
split my_dom_id_string0, parse(_)

rename my_dom_id_string01 region
rename my_dom_id_string02 year
rename my_dom_id_string03 month1
rename my_dom_id_string04 kod
rename my_dom_id_string05 mode
rename my_dom_id_string06 dom_id
drop if my_dom_id_string0!="" & year!="2023"

destring month, gen(month)
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
drop my_dom_id_string1 my_dom_id_string2 my_dom_id_string3

gen my_dom_id_string1=region+"_"+year+"_"+w2+"_"+kod+"_"+mode+"_"+ dom_id
replace my_dom_id_string1=ltrim(rtrim(my_dom_id_string1))

gen my_dom_id_string2=region+"_"+month1+"_"+kod+"_"+mode+"_"+ dom_id
replace my_dom_id_string2=ltrim(rtrim(my_dom_id_string2))

gen my_dom_id_string3=region+"_"+w2+"_"+kod+"_"+mode+"_"+ dom_id
replace my_dom_id_string3=ltrim(rtrim(my_dom_id_string3))

keep my_dom_id_string0* b pse se
tempfile round0
save `round0', replace
restore 

preserve
keep if my_dom_id_string1!=""
drop my_dom_id_string0 my_dom_id_string2 my_dom_id_string3
drop  region year month1 month kod mode dom_id 
split my_dom_id_string1, parse(_)
rename my_dom_id_string11 region
rename my_dom_id_string12 year
rename my_dom_id_string13 wave
rename my_dom_id_string14 kod
rename my_dom_id_string15 mode
rename my_dom_id_string16 dom_id

destring year, replace
keep if $calibration_year
expand 2, gen(dup)
bysort region year wave kod mode dom_id: gen tab=_n
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

gen my_dom_id_string0=region+"_"+year2+"_"+month1+"_"+kod+"_"+mode+"_"+ dom_id
keep my_dom_id_string0 pse
rename pse pse_round1
tempfile round1
save `round1', replace
restore 

preserve
keep if my_dom_id_string2!=""
drop my_dom_id_string0 my_dom_id_string1 my_dom_id_string3
drop  region year month1 month kod mode dom_id w2
split my_dom_id_string2, parse(_)
rename my_dom_id_string21 region
rename my_dom_id_string22 month
rename my_dom_id_string23 kod
rename my_dom_id_string24 mode
rename my_dom_id_string25 dom_id
gen year="2023"

gen my_dom_id_string0=region+"_"+year+"_"+month+"_"+kod+"_"+mode+"_"+ dom_id
keep my_dom_id_string0 pse
rename pse pse_round2
tempfile round2
save `round2', replace
restore 

preserve
keep if my_dom_id_string3!=""
drop my_dom_id_string0 my_dom_id_string1 my_dom_id_string2
drop  region year month1 month kod mode dom_id w2
split my_dom_id_string3, parse(_)
rename my_dom_id_string31 region
rename my_dom_id_string32 wave
rename my_dom_id_string33 kod
rename my_dom_id_string34 mode
rename my_dom_id_string35 dom_id
gen year="2023"
expand 2, gen(dup)
bysort region year wave kod mode dom_id: gen tab=_n
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

gen my_dom_id_string0=region+"_"+year+"_"+month1+"_"+kod+"_"+mode+"_"+ dom_id
keep my_dom_id_string0 pse
rename pse pse_round3
tempfile round3
save `round3', replace
restore 

u `round0', clear 
merge 1:1 my_dom_id_string0 using `round1', keep(1 3)
rename _merge merge1
merge 1:1 my_dom_id_string0 using `round2', keep(1 3)
rename _merge merge2
merge 1:1 my_dom_id_string0 using `round3', keep(1 3)
rename _merge merge3
drop merge*

split my_dom_id_string0, parse(_)

rename my_dom_id_string01 region
rename my_dom_id_string02 year
rename my_dom_id_string03 month1
rename my_dom_id_string04 kod
rename my_dom_id_string05 mode
rename my_dom_id_string06 dom_id 
keep if dom_id=="1"

replace pse=pse_round1 if pse==. | pse==100
replace pse=pse_round2 if pse==. | pse==100
replace pse=pse_round3 if pse==. | pse==100
replace pse=100 if pse==.
replace se= (pse/100)*b
rename b dtrip 
drop pse* dom_id
drop if dtrip==0 | dtrip==.
count
local num=`r(N)'
di `num'

tempfile new
save `new', replace 

global drawz
forv d = 1/`num'{
u `new', clear 

keep if _n==`d'
su dtrip
local est = `r(mean)'

su se
local sd = `r(mean)'

expand $ndraws
gen dtrip_not_trunc=rnormal(`est', `sd')
gen dtrip_new=max(dtrip_not_trunc, 0)

 
gen draw=_n

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

clear
dsconcat $drawz

su dtrip_not 
return list

su dtrip_new
return list
local new = `r(sum)'

local not_truc = `r(sum)'
di ((`new'-`not_truc')/`not_truc')*100


/*The following attempts to correct for bias that occurs when drawing from uncertain MRIP estimates. 
	*When an MRIP estimate is very uncertain, some draws of x from a normal distribution can result in x_i<0. Because trip outcomes cannot 
	*be negative, I change these to 0. But doing so results in an upwardly shifted mean across draws. To correct for this, I sum x_i
	*across draws where x_i<0, and subtract this value from each draw where x_i>0 in proportion to the amount that each x_i>0 contributes to the total 
	*number of trips across x_i's>0.
	*This partly corrects for the issue; however, subtracting a fixed value from x_i where x_i>0 leads to some of these x_i's now <0. I replace these values as 0. */

*I have tried paramaterizing non-negative distributions using the MRIP point estimate and SE, but these resulted in larger differences in the mean trip estimates across all draws by domain (month, kindo-of-day, and mode) than the approach used here. Can work on this in the future. 
 
gen domain=month1+"_"+kod+"_"+mode

gen tab=1 if dtrip_not<0
egen sum_neg=sum(dtrip_not) if tab==1, by(domain)
sort domain
egen mean_sum_neg=mean(sum_neg), by(domain)

egen sum_non_neg=sum(dtrip_not) if dtrip_not>0 , by(domain)
gen prop=dtrip_not/sum_non_neg
gen adjust=prop*mean_sum_neg

/*
egen pctile_x=pctile(dtrip_not) , p(10) by(domain)
gen tab2=1 if dtrip_not>0 & dtrip_not>pctile_x
egen sumtab2=sum(tab2), by(domain)
gen adjust=mean_sum_neg/sumtab2
*/

gen dtrip_new2=dtrip_new+adjust if dtrip_new!=0 & adjust !=.
replace dtrip_new2=dtrip_new if dtrip_new2==.
replace dtrip_new2=0 if dtrip_new2<0


*check differences between original and adjusted draws 
/*
su dtrip_new2 
return list
local new = `r(sum)'

su dtrip_not
return list
local not_truc = `r(sum)'
di ((`new'-`not_truc')/`not_truc')*100
*/

su dtrip_new2
return list
local new = `r(sum)'

su dtrip_not
return list
local old = `r(sum)'

di ((`new'-`old')/`old')*100

replace dtrip_new=dtrip_new2

drop  domain  dtrip_not tab sum_neg sum_non_neg prop mean_sum_neg adjust dtrip_new2 
rename month1 month 

sort region mode month kod draw 

tempfile new1
save `new1'

*now need to make a dataset for the calender year and average out the directed trips across days

global drawz2

forv d = 1/$ndraws{
	u `new1', clear 

keep if draw==`d'
tempfile dtrips`d'
save `dtrips`d'', replace 
	
clear 
set obs 2
gen day=$calibration_date_start if _n==1
replace day=$calibration_date_end if _n==2
format day %td
drop if day==$leap_yr_days
tsset day
tsfill, full
gen day_i=_n

gen dow = dow(day)  //0=Sunday,...,6=Saturday

gen kod="we" if inlist(dow, 5, 6, 0)
replace kod="wd" if inlist(dow, 1, 2, 3, 4)

//add the 12 federal holidays as weekends	
replace kod="we" if $fed_holidays 

gen year=year(day)				
gen month=month(day)				
gen month2 = string(month,"%02.0f")
tostring year, replace
drop month
rename month2 month
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup

merge m:1  kod month mode using `dtrips`d''
*gen draw=`d'
tempfile drawz2`d'
save `drawz2`d'', replace
global drawz2 "$drawz2 "`drawz2`d''" " 

}
clear
dsconcat $drawz2
sort day  mode draw

bysort day mode: gen draw2=_n
order draw2
replace draw=draw2 if draw==.
drop draw2

mvencode dtrip dtrip_new, mv(0) override

*number of weekend/weekday days per region, month, and mode, and draw
gen tab=1
bysort month kod mode draw:egen sum_days=sum(tab)
order sum_days

sort draw mode day 
order draw
sort day
drop dtrip
rename dtrip_new dtrip
mvencode dtrip, mv(0) override
gen trips_per_day=dtrip/sum_days
mvencode trips_per_day, mv(0) override 
order dtrip trips_per_day

order mode year month kod dow day day_i trips_per_day draw
drop dtrip sum_days se _merge tab 

sort  draw mode day 
rename trips_per_day dtrip

sort day 

gen day1=day(day)
gen month1=month(day)
drop my_dom_id_string0
drop region
gen region="`s'"

*call the regulations file	
do "$input_code_cd/set regulations.do"	

		
preserve
keep mode date draw dtrip region ///
	striper_min striper_max striper_bag ///
	bluefish_min bluefish_bag  ///
	striper_min_y2 striper_max_y2 striper_bag_y2  ///
	bluefish_min_y2 bluefish_bag_y2 

order region mode date draw dtrip
sort  region mode date draw 

compress
export delimited using "$iterative_input_data_cd\directed_trip_draws_`s'.csv",  replace 
restore

**Now adjust for the differences in directed trips due to changes in kod between calibration year y and  y+1 
keep mode date date_y2 draw dtrip region kod kod_y2
			
gen month_y1=month(date)
gen month_y2=month(date_y2)
tostring month_y1, replace
tostring month_y2, replace

tempfile base 
save `base', replace 

global drawz

levelsof draw, local(drawss)
foreach d of local drawss{

u `base', clear

keep if draw==`d'
gen domain_y1=mode+"_"+month_y1+"_"+kod
gen domain_y2=mode+"_"+month_y2+"_"+kod_y2

gen dtrip_y2=dtrip if domain_y1==domain_y2 

levelsof domain_y2 if dtrip_y2==., local(domains)
foreach p of local domains{
	su dtrip if domain_y1=="`p'"
	return list
	replace dtrip_y2=`r(mean)' if  domain_y2=="`p'" & dtrip_y2==.
	
}
collapse (sum) dtrip dtrip_y2, by(month_y1 mode)
gen expansion_factor = dtrip_y2/dtrip
gen draw=`d'

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

dsconcat $drawz

mvencode expansion_factor, mv(1) override

su dtrip
return list

su dtrip_y2
return list

gen check =dtrip*expansion
su check
return list

drop check 
rename month month 
destring month, replace
compress
drop dtrip*

export delimited using "$iterative_input_data_cd\next year calendar adjustments `s'.csv",  replace 
}



****Part C****
*Compute totals estimates to compare with simulated calibration output
* estimates by region and mode 
set seed $seed 

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
*keep if inlist(state, "DE")

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

gen my_dom_id_string=region+"_"+mode1+"_"+ dom_id
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore



svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id
destring my_dom_id, replace
merge 1:1 my_dom_id using `domains'
drop rname my_dom_id _merge 
order my_dom_id_string

split my, parse(_)
rename my_dom_id_string1 region
rename my_dom_id_string2 mode
rename my_dom_id_string3 common_dom

keep if common=="1"
keep region mode b se  ll ul
rename b dtrip 
renvarlab dtrip se ll ul, postfix(_mrip)

save "$input_data_cd\mrip_dtrip_by_state_mode.dta", replace 


*estimates by state mode month 
set seed $seed 

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
*keep if inlist(state, "DE")

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

gen my_dom_id_string=region+"_"+mode1+"_"+ dom_id+"_"+month1
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id
destring my_dom_id, replace
merge 1:1 my_dom_id using `domains'
drop rname my_dom_id _merge 
order my_dom_id_string

split my, parse(_)
rename my_dom_id_string1 region
rename my_dom_id_string2 mode
rename my_dom_id_string3 common_dom
rename my_dom_id_string4 month

keep if common=="1"
keep region mode b se  ll ul month
rename b dtrip 
renvarlab dtrip se ll ul, postfix(_mrip)

save "$input_data_cd\mrip_dtrip_by_region_mode_month.dta", replace 


