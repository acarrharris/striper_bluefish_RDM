


***********Set regulations for the calibration period and status-quo regulations for projection period***********
* These need to be changed every year 


* Generate regulations for the calibration period
* Calibration period covers (year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))
* The regulations coded below are for FY 2024 and 2025, so do not have to change them for the FY2026 model
* Only need to change the input MRIP data when running the final model

/********************************************************************
  Code daily recreational STRIPED BASS regs in 2023 by region & mode
  (Updated to match: Striped_bass_regulations 2022 to 2025_EF_formatted.xlsx
   + mid-year 31" max implementation dates from your image)

  Creates:
    striper_min, striper_max, striper_bag
    bluefish_min, bluefish_bag   (left as your prior defaults)
********************************************************************/

*----------------------------
* 0) Expected inputs
*----------------------------
* region : string (e.g., "MEALL","NYHUDS","DENANT","MDCHES","VAOCN","NCOCN",...)
* mode   : string ("pr","fh","sh" OR already "Private"/"For-hire"/"Shore")
* day    : Stata daily date (%td) for calendar year 2023

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
*----------------------------
replace mode = "Private"  if inlist(mode, "pr","priv","private")
replace mode = "For-hire" if inlist(mode, "fh","forhire","for-hire")
replace mode = "Shore"    if inlist(mode, "sh","shore")

*----------------------------
* 3) Month / day-of-month for season logic
*----------------------------
 drop month 
gen int month = month(day)
gen int dom   = day(day)

*========================================================
* 4) BLUEFISH rules (same as your prior template)
*========================================================
* Bag: 
* - Most regions: private/shore=3, for-hire=5, year-round 
* - ME: private/shore=3, for-hire=3, year-round 
* - NH: private/shore=3, for-hire=5, open Jan 1–Sep 30 
* Size: 
* - MD regions (MDCB, MDOC, MDPOT): min = 8 

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
* 5) STRIPER rules (region set matches your Excel)
*========================================================

/*** A) Start with "open possession" defaults by region ***/

* Most "coastal/ocean" style regions: 1 fish, 28–31 slot (post-emergency)
replace striper_bag = 1 if inlist(region,  "MEALL","NHALL","MAALL","RIALL","CTALL") 
replace striper_bag = 1 if inlist(region, "NJALL","DEOCN","MDOCN","VAOCN","NCOCN","NYHUDS")

replace striper_min = 28 if striper_bag==1 & inlist(region, "MEALL","NHALL","MAALL","RIALL","CTALL")
replace striper_min = 28 if striper_bag==1 & inlist(region, "NJALL","DEOCN","MDOCN","VAOCN","NCOCN","NYHUDS")

replace striper_max = 31 if striper_bag==1 & inlist(region, "MEALL","NHALL","MAALL","RIALL","CTALL")
replace striper_max = 31 if striper_bag==1 & inlist(region, "NJALL","DEOCN","MDOCN","VAOCN","NCOCN","NYHUDS")

* NYHUDN (Hudson River upstream of GWB): 1 fish, 18–28 slot
replace striper_bag = 1  if region=="NYHUDN"
replace striper_min = 18 if region=="NYHUDN"
replace striper_max = 28 if region=="NYHUDN"

* Delaware Bay-style seasonal slot: DENANT, DEDERIV
*   Jan–Jun & Sep–Dec: 28–31
*   Jul–Aug: 20–25
replace striper_bag = 1  if inlist(region,"DENANT","DEDERIV")
replace striper_min = 28 if inlist(region,"DENANT","DEDERIV")
replace striper_max = 31 if inlist(region,"DENANT","DEDERIV")
replace striper_min = 20 if inlist(region,"DENANT","DEDERIV") & inrange(month,7,8)
replace striper_max = 25 if inlist(region,"DENANT","DEDERIV") & inrange(month,7,8)

* Maryland Chesapeake: 1 fish, 19–31 slot (trophy handled below)
replace striper_bag = 1  if region=="MDCHES"
replace striper_min = 19 if region=="MDCHES"
replace striper_max = 31 if region=="MDCHES"

* PRFC mainstem Potomac (MDPOT, VAPOTO): 2 fish, 20–31 slot (trophy handled below)
replace striper_bag = 2  if inlist(region,"MDPOT","VAPOTO")
replace striper_min = 20 if inlist(region,"MDPOT","VAPOTO")
replace striper_max = 31 if inlist(region,"MDPOT","VAPOTO")

* Virginia Chesapeake has TWO open seasons with different max:
*   Spring: May 16–Jun 15, 20–28
*   Fall:   Oct 4–Dec 31, 20–31
*   Otherwise closed to harvest
replace striper_bag = 1  if region=="VACHES"
replace striper_min = 20 if region=="VACHES"
replace striper_max = 28 if region=="VACHES"   // default to spring max; we'll close & reopen explicitly below

* NCALB: 1 fish, 18–25 but "closed to harvest" March 13–Dec 31
replace striper_bag = 1  if region=="NCALB"
replace striper_min = 18 if region=="NCALB"
replace striper_max = 25 if region=="NCALB"

* NCCNTRL: no possession year-round
replace striper_bag = 0   if region=="NCCNTRL"
replace striper_min = 100 if region=="NCCNTRL"
replace striper_max = 100 if region=="NCCNTRL"


/*** B) Closures / C&R-only / targeting closures → no possession ***/

* MEALL: spawning areas closed Dec 1–Jun 30 (possession closed)
replace striper_bag = 0 if region=="MEALL" & (month==12 | inrange(month,1,6))

* NYHUDS: closed to harvest Dec 16–Apr 14
replace striper_bag = 0 if region=="NYHUDS" & ( (month==12 & dom>=16) | inrange(month,1,3) | (month==4 & dom<=14) )

* NYHUDN: closed to harvest Dec 1–Mar 31
replace striper_bag = 0 if region=="NYHUDN" & (month==12 | inrange(month,1,3))

* MDCHES: C&R only Jan 1–Mar 31; targeting closures Apr 1–Apr 30 and Jul 16–Jul 31; plus Dec 11–Dec 31
replace striper_bag = 0 if region=="MDCHES" & inrange(month,1,3)
replace striper_bag = 0 if region=="MDCHES" & month==4
replace striper_bag = 0 if region=="MDCHES" & (month==7 & dom>=16)
replace striper_bag = 0 if region=="MDCHES" & (month==12 & dom>=11)

* PRFC mainstem Potomac (MDPOT, VAPOTO): C&R only Jan 1–Apr 30; targeting closure Jul 7–Aug 20
replace striper_bag = 0 if inlist(region,"MDPOT","VAPOTO") & inrange(month,1,4)
replace striper_bag = 0 if inlist(region,"MDPOT","VAPOTO") & ( (month==7 & dom>=7) | (month==8) | (month==9 & dom<=20) ) 
*///    if 0  // (DO NOT USE) placeholder to avoid accidental wrong logic

* Correct PRFC closure Jul 7–Aug 20
replace striper_bag = 0 if inlist(region,"MDPOT","VAPOTO") & ( (month==7 & dom>=7) | (month==8 & dom<=20) )

* VACHES: closed to harvest Jan 1–May 15 and Jun 16–Oct 3
*   Implement as: set closed everywhere, then reopen in the two open windows
replace striper_bag = 0 if region=="VACHES"
replace striper_min = 100 if region=="VACHES"
replace striper_max = 100 if region=="VACHES"

*   Spring open May 16–Jun 15 (20–28)
replace striper_bag = 1  if region=="VACHES" & ( (month==5 & dom>=16) | (month==6 & dom<=15) )
replace striper_min = 20 if region=="VACHES" & ( (month==5 & dom>=16) | (month==6 & dom<=15) )
replace striper_max = 28 if region=="VACHES" & ( (month==5 & dom>=16) | (month==6 & dom<=15) )

*   Fall open Oct 4–Dec 31 (20–31)
replace striper_bag = 1  if region=="VACHES" & ( (month==10 & dom>=4) | inrange(month,11,12) )
replace striper_min = 20 if region=="VACHES" & ( (month==10 & dom>=4) | inrange(month,11,12) )
replace striper_max = 31 if region=="VACHES" & ( (month==10 & dom>=4) | inrange(month,11,12) )

* NJALL: your spreadsheet notes targeting closures; if you want to treat them as "no possession", uncomment:
* replace striper_bag = 0 if region=="NJALL" & (inrange(month,1,2))   // Jan 1–Feb 28 (coarse)
* (and/or add the Delaware River tributary closure Apr–May if you have a separate NJ-DELR region)

* NCALB: closed to harvest March 13–Dec 31
replace striper_bag = 0 if region=="NCALB" & (month>3 | (month==3 & dom>=13))

* Push min/max back to defaults when bag==0 (no possession)
replace striper_min = 100 if striper_bag==0
replace striper_max = 100 if striper_bag==0


/*** C) Trophy seasons (occur before mid-year 31" implementation dates) ***/

* MDCHES trophy: May 1–May 15: >=35 (no max)
* (Only if possession is open in that window)
replace striper_bag = 1    if region=="MDCHES" & month==5 & inrange(dom,1,15)
replace striper_min = 35   if region=="MDCHES" & month==5 & inrange(dom,1,15)
replace striper_max = 100  if region=="MDCHES" & month==5 & inrange(dom,1,15)

* PRFC trophy: May 1–May 15: >=35 (no max), bag=2
replace striper_bag = 2    if inlist(region,"MDPOT","VAPOTO") & month==5 & inrange(dom,1,15)
replace striper_min = 35   if inlist(region,"MDPOT","VAPOTO") & month==5 & inrange(dom,1,15)
replace striper_max = 100  if inlist(region,"MDPOT","VAPOTO") & month==5 & inrange(dom,1,15)


/*** D) Mid-year emergency action: 31" maximum size limit implementation dates ***/
* For any region that is coded with max==31, set it back to 35 BEFORE the state's effective date.
* (This preserves regions with smaller max already, e.g., NYHUDN max 28; VACHES spring max 28; DE summer max 25.)

* Effective dates (from your image)
local eff_ME   = td(18may2023)
local eff_NH   = td(26may2023)
local eff_MA   = td(26may2023)
local eff_RI   = td(27may2023)
local eff_CT   = td(26may2023)
local eff_DE   = td(21may2023)
local eff_MD   = td(16may2023)
local eff_PRFC = td(16may2023)
local eff_NY   = td(20jun2023)
local eff_NJ   = td(02jul2023)
local eff_VA   = td(01jul2023)
local eff_NC   = td(01jun2023)

* ME
replace striper_max = 35 if region=="MEALL"  & day < `eff_ME'   & striper_bag>0 & striper_max==31
* NH
replace striper_max = 35 if region=="NHALL"  & day < `eff_NH'   & striper_bag>0 & striper_max==31
* MA
replace striper_max = 35 if region=="MAALL"  & day < `eff_MA'   & striper_bag>0 & striper_max==31
* RI
replace striper_max = 35 if region=="RIALL"  & day < `eff_RI'   & striper_bag>0 & striper_max==31
* CT
replace striper_max = 35 if region=="CTALL"  & day < `eff_CT'   & striper_bag>0 & striper_max==31
* DE (ocean + bay regions)
replace striper_max = 35 if inlist(region,"DEOCN","DENANT","DEDERIV") & day < `eff_DE' & striper_bag>0 & striper_max==31
* MD (ocean + Chesapeake)
replace striper_max = 35 if inlist(region,"MDOCN","MDCHES") & day < `eff_MD' & striper_bag>0 & striper_max==31
* PRFC mainstem Potomac
replace striper_max = 35 if inlist(region,"MDPOT","VAPOTO") & day < `eff_PRFC' & striper_bag>0 & striper_max==31
* NY (Hudson south only has 31; north is 28 so unaffected)
replace striper_max = 35 if region=="NYHUDS" & day < `eff_NY' & striper_bag>0 & striper_max==31
* NJ
replace striper_max = 35 if region=="NJALL"  & day < `eff_NJ'   & striper_bag>0 & striper_max==31
* VA (ocean only needs this; VACHES fall open is after July 1 anyway, but harmless)
replace striper_max = 35 if region=="VAOCN"  & day < `eff_VA'   & striper_bag>0 & striper_max==31
replace striper_max = 35 if region=="VACHES" & day < `eff_VA'   & striper_bag>0 & striper_max==31
* NC ocean
replace striper_max = 35 if region=="NCOCN"  & day < `eff_NC'   & striper_bag>0 & striper_max==31


*----------------------------
* 6) Optional quick checks
*----------------------------
* tab region if missing(striper_bag)
* tab region if striper_bag==0
* su striper_min striper_max striper_bag

*----------------------------
* 7) Restore compact mode codes (if needed)
*----------------------------
replace mode="fh" if mode=="For-hire"
replace mode="pr" if mode=="Private"
replace mode="sh" if mode=="Shore"

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

gen dom=day(day_y2)
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




merge 1:m  mode dom month  using `regulations'
rename day date 
drop if date==$leap_yr_days
drop _merge 

rename day_y2 date_y2

sort draw mode date 

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


