*------------------------------------------------------------------------------
* Project paths
*------------------------------------------------------------------------------

cd "E:\Lou_projects\mrip_data"

clear 
clear mata
global seed 03211990
set seed $seed


*------------------------------------------------------------------------------
* Years and waves
*------------------------------------------------------------------------------

global yr_wvs ///
    19821 19822 19823 19824 19825 19826 ///
    19831 19832 19833 19834 19835 19836 ///
    19841 19842 19843 19844 19845 19846 ///
    19851 19852 19853 19854 19855 19856 ///
    19861 19862 19863 19864 19865 19866 ///
    19871 19872 19873 19874 19875 19876 ///
    19881 19882 19883 19884 19885 19886 ///
    19891 19892 19893 19894 19895 19896 ///
    19901 19902 19903 19904 19905 19906 ///
    19911 19912 19913 19914 19915 19916 ///
    19921 19922 19923 19924 19925 19926 ///
    19931 19932 19933 19934 19935 19936 ///
    19941 19942 19943 19944 19945 19946 ///
    19951 19952 19953 19954 19955 19956 ///
    19961 19962 19963 19964 19965 19966 ///
    19971 19972 19973 19974 19975 19976 ///
    19981 19982 19983 19984 19985 19986 ///
    19991 19992 19993 19994 19995 19996 ///
    20001 20002 20003 20004 20005 20006 ///
    20011 20012 20013 20014 20015 20016 ///
    20021 20022 20023 20024 20025 20026 ///
    20031 20032 20033 20034 20035 20036 ///
    20041 20042 20043 20044 20045 20046 ///
    20051 20052 20053 20054 20055 20056 ///
    20061 20062 20063 20064 20065 20066 ///
    20071 20072 20073 20074 20075 20076 ///
    20081 20082 20083 20084 20085 20086 ///
    20091 20092 20093 20094 20095 20096 ///
    20101 20102 20103 20104 20105 20106 ///
    20111 20112 20113 20114 20115 20116 ///
    20121 20122 20123 20124 20125 20126 ///
    20131 20132 20133 20134 20135 20136 ///
    20141 20142 20143 20144 20145 20146 ///
    20151 20152 20153 20154 20155 20156 ///
    20161 20162 20163 20164 20165 20166 ///
    20171 20172 20173 20174 20175 20176 ///
    20181 20182 20183 20184 20185 20186 ///
    20191 20192 20193 20194 20195 20196 ///
    20201 20202 20203 20204 20205 20206 ///
    20211 20212 20213 20214 20215 20216 ///
    20221 20222 20223 20224 20225 20226 ///
    20231 20232 20233 20234 20235 20236 ///
    20241 20242 20243 20244 20245 20246 ///
    20251 20252 20253 20254 20255 20256

global yearlist ///
    1982 1983 1984 1985 1986 1987 1988 1989 ///
    1990 1991 1992 1993 1994 1995 1996 1997 ///
    1998 1999 2000 2001 2002 2003 2004 2005 ///
    2006 2007 2008 2009 2010 2011 2012 2013 ///
    2014 2015 2016 2017 2018 2019 2020 2021 ///
    2022 2023 2024 2025

global wavelist 1 2 3 4 5 6


*------------------------------------------------------------------------------
* 1. Pull MRIP data and construct $triplist and $catchlist
*------------------------------------------------------------------------------



/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year in $yearlist{
	foreach wave in  $wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}



*------------------------------------------------------------------------------
* 2. Concatenate trip and catch files
*------------------------------------------------------------------------------

cd "E:\Lou_projects\mrip_data"

tempfile trip_data catch_data merged_data compiled_results

clear
dsconcat $triplist
*drop if strmatch(id_code, "*xx*")==1
duplicates drop 
gen byte dtrip = 1

sort year strat_id psu_id id_code
save `trip_data', replace


clear
dsconcat $catchlist
sort year strat_id psu_id id_code

replace common = subinstr(lower(common), " ", "", .)

save `catch_data', replace


*------------------------------------------------------------------------------
* 3. Merge trips and catch records
*------------------------------------------------------------------------------

use `trip_data', clear

merge 1:m year strat_id psu_id id_code using `catch_data', ///
    keep(1 3)

drop _merge

foreach var in common prim1_common prim2_common {
    replace `var' = subinstr(lower(`var'), " ", "", .)
}

save `merged_data', replace


*------------------------------------------------------------------------------
* 4. Estimate monthly directed trips separately by year
*------------------------------------------------------------------------------

local first_result = 1
foreach yr of global yearlist {
*local yr 2024
    display as text _newline ///
        "------------------------------------------------------------"
    display as text "Processing MRIP year `yr'"
    display as text ///
        "------------------------------------------------------------"

    use `merged_data', clear

    keep if year == `yr'


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


 
gen st2 = string(st,"%02.0f")


    * Assumes mode_fx is stored as a string.
    gen str2 mode1 = ""
    replace mode1 = "sh" if inlist(mode_fx, "1", "2", "3")
    replace mode1 = "pr" if mode_fx == "7"
    replace mode1 = "fh" if inlist(mode_fx, "4", "5")


    * Extract interview month and day from id_code.
    *
    * This preserves your original position assumptions:
    *   positions 10-11 = month
    *   positions 12-13 = day
    *
    * Verify these positions against your id_code format.
	gen str2 month_string = substr(id_code, 10, 2)
	gen str2 day_string   = substr(id_code, 12, 2)

	drop if inlist(lower(month_string), "9x", "xx")
	drop if inlist(lower(day_string),   "9x", "xx")

	destring month_string, gen(interview_month) force
	destring day_string,   gen(interview_day) force


    gen str2 month1 = string(interview_month, "%02.0f")
    gen str4 year2  = string(year, "%04.0f")


    *--------------------------------------------------------------------------
    * Classify trips into:
    *   dom_id = 1: species domain of interest
    *   dom_id = 2: all other trips
    *--------------------------------------------------------------------------

    gen str1 dom_id = "2"

    local domain_species stripedbass bluefish
		
		/* ///
        summerflounder ///
        blackseabass ///
        scup ///
        northernkingfish ///
        northernpuffer */
		
    foreach species of local domain_species {

        replace dom_id = "1" if common       == "`species'"
        replace dom_id = "1" if prim1_common == "`species'"
    }


    *--------------------------------------------------------------------------
    * Group-catch adjustment
    *--------------------------------------------------------------------------

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



    *--------------------------------------------------------------------------
    * Construct monthly domain
    *--------------------------------------------------------------------------

    gen str20 my_dom_id_string = ///
        dom_id + "_" + year2 

    replace my_dom_id_string = strtrim(my_dom_id_string)

    encode my_dom_id_string, gen(my_dom_id)


    *--------------------------------------------------------------------------
    * Retain one record per intercepted trip
    *
    * Sorting dom_id ascending means domain 1 is retained before domain 2.
    *--------------------------------------------------------------------------

    bysort year wave strat_id psu_id id_code (dom_id): keep if _n == 1


    *--------------------------------------------------------------------------
    * Survey design
    *--------------------------------------------------------------------------

    /*
    Survey weights must be positive. Setting invalid weights to zero can cause
    survey-estimation errors, so drop records with missing or nonpositive
    weights instead.
    */

    drop if missing(wp_int) | wp_int <= 0
    drop if missing(psu_id) | missing(strat_id)

    quietly count
    if r(N) == 0 {
        display as error ///
            "No observations with valid survey weights for `yr'; skipping."
        continue
    }

    svyset psu_id [pweight = wp_int], ///
        strata(strat_id) ///
        singleunit(certainty)


    *--------------------------------------------------------------------------
    * Save lookup between encoded and string domain identifiers
    *--------------------------------------------------------------------------

    preserve

        keep my_dom_id my_dom_id_string
        duplicates drop

        tempfile domains
        save `domains', replace

    restore


    *--------------------------------------------------------------------------
    * Estimate total directed trips
    *--------------------------------------------------------------------------

    capture quietly svy: total dtrip, over(my_dom_id)

    if _rc {
        display as error ///
            "Survey estimation failed for `yr' with return code " _rc
        continue
    }


    *--------------------------------------------------------------------------
    * Convert survey results matrix to a dataset
    *--------------------------------------------------------------------------

    xsvmat, from(r(table)') rownames(rname) names(col) norestor

    split rname, parse("@") gen(rname_part)
    drop rname_part1

    split rname_part2, parse(".") gen(domain_part)

    drop rname rname_part2 domain_part2

    rename domain_part1 my_dom_id
    destring my_dom_id, replace

    merge 1:1 my_dom_id using `domains', ///
        keep(match) ///
        nogen


    *--------------------------------------------------------------------------
    * Recover domain, year, and month from the domain string
    *--------------------------------------------------------------------------

    split my_dom_id_string, parse("_") gen(dompart)

    rename dompart1 common_dom
    rename dompart2 year


    destring year , replace

    order year  common_dom  my_dom_id_string


    *--------------------------------------------------------------------------
    * Append this year's estimates to the compiled results
    *--------------------------------------------------------------------------

    if `first_result' {

        save `compiled_results', replace
        local first_result = 0

    }
    else {

        append using `compiled_results'
        save `compiled_results', replace

    }
}


*------------------------------------------------------------------------------
* 5. Save compiled annual results
*------------------------------------------------------------------------------

if `first_result' {

    display as error ///
        "No yearly survey estimates were successfully generated."

}
else {

    use `compiled_results', clear

    sort year common_dom 

    compress

}


keep if common_dom=="1"
rename b trips

keep  year  trips 
format trips %12.0gc
gen target="striper_blue"
save "historcial_str_blu_dtrips.dta", replace




* striper only 
cd "E:\Lou_projects\mrip_data"

tempfile trip_data catch_data merged_data compiled_results

clear
dsconcat $triplist
*drop if strmatch(id_code, "*xx*")==1
duplicates drop 
gen byte dtrip = 1

sort year strat_id psu_id id_code
save `trip_data', replace


clear
dsconcat $catchlist
sort year strat_id psu_id id_code

replace common = subinstr(lower(common), " ", "", .)

save `catch_data', replace


*------------------------------------------------------------------------------
* 3. Merge trips and catch records
*------------------------------------------------------------------------------

use `trip_data', clear

merge 1:m year strat_id psu_id id_code using `catch_data', ///
    keep(1 3)

drop _merge

foreach var in common prim1_common prim2_common {
    replace `var' = subinstr(lower(`var'), " ", "", .)
}

save `merged_data', replace


*------------------------------------------------------------------------------
* 4. Estimate monthly directed trips separately by year
*------------------------------------------------------------------------------

local first_result = 1
foreach yr of global yearlist {
*local yr 2024
    display as text _newline ///
        "------------------------------------------------------------"
    display as text "Processing MRIP year `yr'"
    display as text ///
        "------------------------------------------------------------"

    use `merged_data', clear

    keep if year == `yr'


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


 
gen st2 = string(st,"%02.0f")


    * Assumes mode_fx is stored as a string.
    gen str2 mode1 = ""
    replace mode1 = "sh" if inlist(mode_fx, "1", "2", "3")
    replace mode1 = "pr" if mode_fx == "7"
    replace mode1 = "fh" if inlist(mode_fx, "4", "5")


    * Extract interview month and day from id_code.
    *
    * This preserves your original position assumptions:
    *   positions 10-11 = month
    *   positions 12-13 = day
    *
    * Verify these positions against your id_code format.
	gen str2 month_string = substr(id_code, 10, 2)
	gen str2 day_string   = substr(id_code, 12, 2)

	drop if inlist(lower(month_string), "9x", "xx")
	drop if inlist(lower(day_string),   "9x", "xx")

	destring month_string, gen(interview_month) force
	destring day_string,   gen(interview_day) force


    gen str2 month1 = string(interview_month, "%02.0f")
    gen str4 year2  = string(year, "%04.0f")


    *--------------------------------------------------------------------------
    * Classify trips into:
    *   dom_id = 1: species domain of interest
    *   dom_id = 2: all other trips
    *--------------------------------------------------------------------------

    gen str1 dom_id = "2"

    local domain_species stripedbass
		
		/* ///
        summerflounder ///
        blackseabass ///
        scup ///
        northernkingfish ///
        northernpuffer */
		
    foreach species of local domain_species {

        replace dom_id = "1" if common       == "`species'"
        replace dom_id = "1" if prim1_common == "`species'"
    }


    *--------------------------------------------------------------------------
    * Group-catch adjustment
    *--------------------------------------------------------------------------

	// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "stripedbass") 

mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")



    *--------------------------------------------------------------------------
    * Construct monthly domain
    *--------------------------------------------------------------------------

    gen str20 my_dom_id_string = ///
        dom_id + "_" + year2 

    replace my_dom_id_string = strtrim(my_dom_id_string)

    encode my_dom_id_string, gen(my_dom_id)


    *--------------------------------------------------------------------------
    * Retain one record per intercepted trip
    *
    * Sorting dom_id ascending means domain 1 is retained before domain 2.
    *--------------------------------------------------------------------------

    bysort year wave strat_id psu_id id_code (dom_id): keep if _n == 1


    *--------------------------------------------------------------------------
    * Survey design
    *--------------------------------------------------------------------------

    /*
    Survey weights must be positive. Setting invalid weights to zero can cause
    survey-estimation errors, so drop records with missing or nonpositive
    weights instead.
    */

    drop if missing(wp_int) | wp_int <= 0
    drop if missing(psu_id) | missing(strat_id)

    quietly count
    if r(N) == 0 {
        display as error ///
            "No observations with valid survey weights for `yr'; skipping."
        continue
    }

    svyset psu_id [pweight = wp_int], ///
        strata(strat_id) ///
        singleunit(certainty)


    *--------------------------------------------------------------------------
    * Save lookup between encoded and string domain identifiers
    *--------------------------------------------------------------------------

    preserve

        keep my_dom_id my_dom_id_string
        duplicates drop

        tempfile domains
        save `domains', replace

    restore


    *--------------------------------------------------------------------------
    * Estimate total directed trips
    *--------------------------------------------------------------------------

    capture quietly svy: total dtrip, over(my_dom_id)

    if _rc {
        display as error ///
            "Survey estimation failed for `yr' with return code " _rc
        continue
    }


    *--------------------------------------------------------------------------
    * Convert survey results matrix to a dataset
    *--------------------------------------------------------------------------

    xsvmat, from(r(table)') rownames(rname) names(col) norestor

    split rname, parse("@") gen(rname_part)
    drop rname_part1

    split rname_part2, parse(".") gen(domain_part)

    drop rname rname_part2 domain_part2

    rename domain_part1 my_dom_id
    destring my_dom_id, replace

    merge 1:1 my_dom_id using `domains', ///
        keep(match) ///
        nogen


    *--------------------------------------------------------------------------
    * Recover domain, year, and month from the domain string
    *--------------------------------------------------------------------------

    split my_dom_id_string, parse("_") gen(dompart)

    rename dompart1 common_dom
    rename dompart2 year


    destring year , replace

    order year  common_dom  my_dom_id_string


    *--------------------------------------------------------------------------
    * Append this year's estimates to the compiled results
    *--------------------------------------------------------------------------

    if `first_result' {

        save `compiled_results', replace
        local first_result = 0

    }
    else {

        append using `compiled_results'
        save `compiled_results', replace

    }
}


*------------------------------------------------------------------------------
* 5. Save compiled annual results
*------------------------------------------------------------------------------

if `first_result' {

    display as error ///
        "No yearly survey estimates were successfully generated."

}
else {

    use `compiled_results', clear

    sort year common_dom 

    compress

}


keep if common_dom=="1"
rename b trips

keep  year  trips 
format trips %12.0gc
gen target="striper"
save "historcial_str_dtrips.dta", replace


append using "historcial_str_blu_dtrips.dta"


format trips %12.0gc
replace trips=trips/1000000


u "historcial_str_blu_dtrips.dta", clear 
u "historcial_str_dtrips.dta", clear 

preserve

keep if inlist(target, "striper", "striper_blue")

* Ensure one observation per year and target
collapse (sum) trips, by(year target)

* Convert to one observation per year
reshape wide trips, i(year) j(target) string

* Rename variables for clarity
rename tripsstriper striper_trips
rename tripsstriper_blue striper_blue_total

* Calculate trips included in striper_blue but not striper
gen striper_blue_only = striper_blue_total - striper_trips

* Check that the subset relationship holds
assert striper_blue_only >= 0 if !missing(striper_blue_only)
graph bar striper_trips striper_blue_only if year>=2000, over(year, label(angle(45) labsize(small))) stack ///
    ytitle("Number of trips (millions)") ylab(#10) ///
    title("Striped bass and striped bass/bluefish trips*") ///
    legend(order(1 "Striped bass" 2 "Striped bass/bluefish") rows(1) position(6)) ///
	note("*Caught or primary target")

restore