


***********Set regulations for the calibration period and status-quo regulations for projection period***********
* These need to be changed every year 


* Generate regulations for the calibration period
* Calibration period covers (year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))
* The regulations coded below are for FY 2024 and 2025, so do not have to change them for the FY2026 model
* Only need to change the input MRIP data when running the final model



/********************************************************************
  Code daily recreational regs in 2023 by region & fishing mode
  Creates:
    striper_min, striper_max, striper_bag
    bluefish_min, bluefish_bag
********************************************************************/
*----------------------------
* 0) Safety checks (optional)
*----------------------------
* describe region fishing_mode day
* assert inrange(day, td(01jan2023), td(31dec2023))

*----------------------------
* 1) Initialize regulation vars
*----------------------------
gen double striper_min  = 100
gen double striper_max  = 100
gen int    striper_bag  = 0

gen double bluefish_min = 0
gen int    bluefish_bag = 0

*----------------------------
* 2) Standardize fishing mode labels
*    (edit these mappings if your dataset uses different codes)
*----------------------------

replace mode = "Private" if inlist(mode, "pr","priv","private")
replace mode = "For-hire" if inlist(mode, "fh","forhire","for-hire")
replace mode = "Shore"   if inlist(mode, "sh","shore")

*----------------------------
* 3) Extract month/day for season logic
*----------------------------
capture drop month dom
drop month
gen int month = month(day)
gen int dom   = day(day)

*========================================================
* 4) BLUEFISH rules (from spreadsheet)
*
*   Bag:
*     - Most regions: private/shore=3, for-hire=5, year-round
*     - ME: private/shore=3, for-hire=3, year-round
*     - NH: private/shore=3, for-hire=5, open Jan 1–Sep 30
*   Size:
*     - MD regions (MDCB, MDOC, MDPOT): min = 8
*========================================================

* Default bluefish bag: private/shore 3, for-hire 5, year-round
replace bluefish_bag = 3 if inlist(mode,"Private","Shore")
replace bluefish_bag = 5 if mode=="For-hire"

* Maine exception: for-hire = 3
replace bluefish_bag = 3 if region=="ME" & mode=="For-hire"

* New Hampshire season Jan 1–Sep 30 (set closed outside)
replace bluefish_bag = 0 if region=="NH" & (month>=10 | month<=12)

* Bluefish min size: MD areas only
replace bluefish_min = 8 if inlist(region,"MDCB","MDOC","MDPOT")



*========================================================
* 5) STRIPER rules (from spreadsheet)
*
*  Most regions: bag=1 and slot limits, with closures in some regions.
*  NOTE: Any day that is "closed" here is treated as *no possession*:
*        striper_bag=0, striper_min/max left at defaults (100/100).
*========================================================

*----------------------------
* A) Start with a common default where applicable:
*    bag=1, 28–31 slot for many Atlantic coastal regions
*----------------------------
replace striper_bag = 1 if inlist(region,"NH","MA","RI","CT","NYOC")
replace striper_bag = 1 if inlist(region,"NJ","DEOC","MDOC","VAOC","NCOC")

replace striper_min = 28 if striper_bag==1 & inlist(region,"NH","MA","RI","CT","NYOC","NJ")
replace striper_min = 28 if striper_bag==1 & inlist(region,"DEOC","MDOC","VAOC","NCOC")

replace striper_max = 31 if striper_bag==1 & inlist(region,"NH","MA","RI","CT","NYOC","NJ")
replace striper_max = 31 if striper_bag==1 & inlist(region,"DEOC","MDOC","VAOC","NCOC")

*----------------------------
* B) Region-specific overrides / closures
*----------------------------

*** ME: bag=1, 28–31 slot, CLOSED Dec 1–Jun 30 (treat as no possession)
replace striper_bag = 1  if region=="ME"
replace striper_min = 28 if region=="ME"
replace striper_max = 31 if region=="ME"
* closed Dec 1–Dec 31 OR Jan 1–Jun 30
replace striper_bag = 0 if region=="ME" & (month==12 | inrange(month,1,6))
replace striper_min = 100 if region=="ME" & striper_bag==0
replace striper_max = 100 if region=="ME" & striper_bag==0

*** NYHR: bag=1, 28–31 slot, CLOSED Dec 1–Mar 31
replace striper_bag = 1  if region=="NYHR"
replace striper_min = 28 if region=="NYHR"
replace striper_max = 31 if region=="NYHR"
replace striper_bag = 0 if region=="NYHR" & (month==12 | inrange(month,1,3))
replace striper_min = 100 if region=="NYHR" & striper_bag==0
replace striper_max = 100 if region=="NYHR" & striper_bag==0

*** DEBAY: bag=1 with TWO seasonal slot limits:
*    Jan 1–Jun 30 and Sep 1–Dec 31: 28–31
*    Jul 1–Aug 31: 20–25
replace striper_bag = 1 if region=="DEBAY"
* default (Jan-Jun, Sep-Dec) 28–31
replace striper_min = 28 if region=="DEBAY"
replace striper_max = 31 if region=="DEBAY"
* Jul-Aug override 20–25
replace striper_min = 20 if region=="DEBAY" & inrange(month,7,8)
replace striper_max = 25 if region=="DEBAY" & inrange(month,7,8)

*** VACB: bag=1, 19–24 slot, CLOSED May 16–Jun 15 and Oct 4–Dec 31
replace striper_bag = 1  if region=="VACB"
replace striper_min = 19 if region=="VACB"
replace striper_max = 24 if region=="VACB"
* closed May16-Jun15
replace striper_bag = 0 if region=="VACB" & ( (month==5 & dom>=16) | (month==6 & dom<=15) )
* closed Oct4-Dec31
replace striper_bag = 0 if region=="VACB" & ( (month==10 & dom>=4) | inrange(month,11,12) )
replace striper_min = 100 if region=="VACB" & striper_bag==0
replace striper_max = 100 if region=="VACB" & striper_bag==0

*** VAOC: bag=1, 28–31 slot, CLOSED Apr 1–May 15
replace striper_bag = 1  if region=="VAOC"
replace striper_min = 28 if region=="VAOC"
replace striper_max = 31 if region=="VAOC"
replace striper_bag = 0 if region=="VAOC" & ( (month==4) | (month==5 & dom<=15) )
replace striper_min = 100 if region=="VAOC" & striper_bag==0
replace striper_max = 100 if region=="VAOC" & striper_bag==0

*** VAPOT: bag=1, 19–24 slot, CLOSED Apr 1–May 15 and Jul 7–Aug 20
replace striper_bag = 1  if region=="VAPOT"
replace striper_min = 19 if region=="VAPOT"
replace striper_max = 24 if region=="VAPOT"
* Apr1-May15
replace striper_bag = 0 if region=="VAPOT" & ( (month==4) | (month==5 & dom<=15) )
* Jul7-Aug20
replace striper_bag = 0 if region=="VAPOT" & ( (month==7 & dom>=7) | (month==8 & dom<=20) )
replace striper_min = 100 if region=="VAPOT" & striper_bag==0
replace striper_max = 100 if region=="VAPOT" & striper_bag==0

*** MDCB: bag=1; Base slot 19–24.
*    Trophy season May 1–May 15: >=35 (no max)
*    Targeting closures: Apr 1–Apr 30 and Jul 16–Jul 31  (treat as no possession)
replace striper_bag = 1  if region=="MDCB"
replace striper_min = 19 if region=="MDCB"
replace striper_max = 24 if region=="MDCB"
* closures
replace striper_bag = 0 if region=="MDCB" & month==4
replace striper_bag = 0 if region=="MDCB" & (month==7 & dom>=16)
replace striper_min = 100 if region=="MDCB" & striper_bag==0
replace striper_max = 100 if region=="MDCB" & striper_bag==0
* trophy overrides (May 1–15)
replace striper_bag = 1  if region=="MDCB" & month==5 & inrange(dom,1,15)
replace striper_min = 35 if region=="MDCB" & month==5 & inrange(dom,1,15)
replace striper_max = 100 if region=="MDCB" & month==5 & inrange(dom,1,15)

*** MDPOT: bag=1; Base slot 19–24.
*    Trophy season May 1–May 15: >=35 (no max)
*    Seasonal closures: Apr 1–May 15, Jul 16–Jul 31 (treat as no possession)
*    We treat trophy season as overriding the Apr-May closure (as implied by spreadsheet text).
replace striper_bag = 1  if region=="MDPOT"
replace striper_min = 19 if region=="MDPOT"
replace striper_max = 24 if region=="MDPOT"
* closures (apply first)
replace striper_bag = 0 if region=="MDPOT" & ( month==4 | (month==5 & dom<=15) )
replace striper_bag = 0 if region=="MDPOT" & (month==7 & dom>=16)
replace striper_min = 100 if region=="MDPOT" & striper_bag==0
replace striper_max = 100 if region=="MDPOT" & striper_bag==0
* trophy override May 1–15
replace striper_bag = 1  if region=="MDPOT" & month==5 & inrange(dom,1,15)
replace striper_min = 35 if region=="MDPOT" & month==5 & inrange(dom,1,15)
replace striper_max = 100 if region=="MDPOT" & month==5 & inrange(dom,1,15)

*** NCSOUND: bag=1, 18–25 slot, CLOSED March 13–Dec 31
replace striper_bag = 1  if region=="NCSOUND"
replace striper_min = 18 if region=="NCSOUND"
replace striper_max = 25 if region=="NCSOUND"
* closed from Mar 13 onward
replace striper_bag = 0 if region=="NCSOUND" & ( month>3 | (month==3 & dom>=13) )
replace striper_min = 100 if region=="NCSOUND" & striper_bag==0
replace striper_max = 100 if region=="NCSOUND" & striper_bag==0

*** NCCENTRAL: "No possession" and "Closed year-round"
replace striper_bag = 0   if region=="NCCENTRAL"
replace striper_min = 100 if region=="NCCENTRAL"
replace striper_max = 100 if region=="NCCENTRAL"

/*
*** NCROANOKE: bag=1 with a protected slot (22–27) and >=18 otherwise.
* WARNING: This cannot be represented perfectly with ONLY (min,max).
* Here we:
*   - set overall min=18 and max=100 (open-ended), and
*   - create OPTIONAL protected slot vars you can use later in catch logic.

replace striper_bag = 1   if region=="NCROANOKE"
replace striper_min = 18  if region=="NCROANOKE"
replace striper_max = 100 if region=="NCROANOKE"

capture drop striper_protect_min striper_protect_max
gen double striper_protect_min = .
gen double striper_protect_max = .
replace striper_protect_min = 22 if region=="NCROANOKE"
replace striper_protect_max = 27 if region=="NCROANOKE"

* Closures for NCROANOKE: Jan1-Apr13, Apr18-Apr21, Apr24-Dec31 (treat as no possession)
replace striper_bag = 0 if region=="NCROANOKE" & ( inrange(month,1,3) | (month==4 & dom<=13) )
replace striper_bag = 0 if region=="NCROANOKE" & (month==4 & inrange(dom,18,21))
replace striper_bag = 0 if region=="NCROANOKE" & ( (month==4 & dom>=24) | inrange(month,5,12) )
replace striper_min = 100 if region=="NCROANOKE" & striper_bag==0
replace striper_max = 100 if region=="NCROANOKE" & striper_bag==0
*/

*----------------------------
* 6) Final quick sanity checks (optional)
*----------------------------
 tab region if striper_bag==.
 su striper_min striper_max striper_bag bluefish_min bluefish_bag


* Drop temporary variables
replace mode="fh" if mode=="For-hire"
replace mode="pr" if mode=="Private"
replace mode="sh" if mode=="Shore"

rename day date
rename day1 day
*************************

tempfile regulations
save `regulations', replace 

*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full

gen day=day(day_y2)
gen month=month(day_y2)
gen year_y2=year(day_y2)
drop if day_y2==$leap_yr_days
gen dow_y2 = dow(day_y2)  

gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		
replace kod_y2="we" if $fed_holidays_y2

gen month2_y2= string(month,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup


merge 1:m  mode day month using `regulations'
drop if date==$leap_yr_days
drop _merge 

sort draw mode date 
order draw year mode month kod dow day  date draw  striper_min striper_max striper_bag bluefish_min bluefish_bag 


*************************
*Create status-quo regualtions for projection period here: td(01jan2026)  -  td(31dec2026)
gen  striper_min_y2  = striper_min
gen  striper_max_y2 = striper_max
gen  striper_bag_y2  = striper_bag

gen  bluefish_min_y2 =bluefish_min
gen  bluefish_bag_y2 = bluefish_bag
*************************


*translate minimum sizes to cm
local vars striper_min striper_max  bluefish_min  striper_min_y2 striper_max_y2  bluefish_min_y2 

foreach v of local vars{
	replace `v'=`v'*2.54
}
replace fluke_min = fluke_min*2.54


