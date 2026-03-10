


* This script create calibration catch draw files for each state that contains:
	* 50 trips in each day of the calibration year in which there were directed trips, each with 30 draws of catch-per-trip
	* demographics for each trip that are constant across catch draws
	
set seed $seed


*********Angler demographics (age and avidity - # trips past 12 months) *************

	* Ages and avidity come from the fishing effort survey 12 MONTH files. 
	* These data are NOT publicly available and the data have not been processeed for QA/QC like the publicly available 2-month files. 
	* Data from 2018-2023 was delivered by Lucas Johanssen on 4/23/2025. A few notes/caveats from Lucas:
		* "FES QC processes focus on the 2-month reference periods, and we do very little evaluation and editing of 12-month effort responses.  
		* Responses for these fields are essentially unedited, raw data.  
		* The final weight trimming procedures focus on reducing the impacts of outlier values on wave-level estimates. 
		* The data may include records that are highly influential with respect to 12-month effort and any estimates may be highly variable.  
		* Wave data will produce independent estimates of 12-month effort."
		
*I will use the most recent year of FES survey data available (2023)

/*
global dems
local wvs 1 2 3 4 5 6
foreach w of local wvs{
	
u  "$input_data_cd\fes_person_final_2023`w'.dta", clear 


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

keep if state!=""

tempfile dems`w'
save `dems`w'', replace
global dems "$dems "`dems`w''" " 

}
clear
dsconcat $dems

gen total_trips_12=boat_trips_12+shore_trips_12
gen total_trips_2=boat_trips+shore_trips

* Lou's QA/QC on the FES data 

keep if age>=16 // drop anglers below the minimum age required for license to align the age distribution with choice experiment sampling frame, which is based on licensees (16+)

replace total_trips_2=round(total_trips_2)
replace total_trips_12=round(total_trips_12)
drop if total_trips_2>total_trips_12 // drop if total trips 2 months>total trips 12 months

drop if total_trips_2>=62 // drop if total trips 2 months>60 
drop if total_trips_12>=365 // drop if total trips 12 months>365 

replace final=final/100 // sum of weights is almost 300 million, so I proportionally reduce the weights so my Stata doesn't blow up
replace final=round(final)

expand final 
su total_trips_12, detail  

egen p9995 = pctile(total_trips_12), p(99.95) // drop total_trips_12 above the 99.95 percentile
drop if total_trips_12>p9995

levelsof state, local(sts)

tempfile new
save `new', replace

clear
tempfile master
save `master', emptyok
		
foreach s of local sts{
	u `new', clear
	
	keep if state=="`s'"
	
	tempfile new2
	save `new2', replace 
	
	forv w=1/6{
		u `new2', clear 
		keep if wave==`w'
	
		sample 10000, count
		append using `master'
		save `master', replace
		clear    
	}
}

use `master', clear
compress               

keep age total_trips_12 wave state
save "$input_data_cd\angler_dems.dta", replace 


*/

********** Ensure copula model did not produce NAs **********
/*
local sts "ME NH MA RI CT NY NJ DE MD VA NC"
foreach s of local sts{
	
forv i=1/10{
import excel using "$iterative_input_data_cd\calib_catch_draws_`s'`i'.xlsx", clear firstrow
gen str_cat=str_keep_sim+str_rel_sim
gen blu_cat=blu_keep_sim+blu_rel_sim
*di "`s'"
*di `i'
qui{
	 count if str_keep_sim==. | str_rel_sim==. | blu_keep_sim==. | blu_rel_sim==. | str_cat==. | blu_cat ==. 
}

if `r(N)'>0{
	di `r(N)'
	levelsof my_dom if  str_keep_sim==. | str_rel_sim==. | blu_keep_sim==. | blu_rel_sim==. | str_cat==. | blu_cat ==. 
	
}
}
}
*/


************** Generate catch draw files ******************

*Original version - very long computing time 
/*
local regions "CTALL DEDERIV DENANT DEOCN MAALL MDCHES MDOCN MDPOT MEALL NCALB NCCNTRL NCOCN NHALL NJALL NYHUDN NYHUDS RIALL VACHES VAOCN VAPOTO"

foreach s of local regions{
	
 forvalues i = 1/1{
	
	*test values 
	*local s "MDCHES"
	*local i 1
	
    import delimited using "$iterative_input_data_cd\directed_trip_draws_`s'.csv", clear
	keep if draw == `i'

		gen date_num = date(date, "DMY")
		gen month = month(date_num)	
		drop date_num
		
		gen month1 = string(month, "%02.0f")
		
    gen byte wave = cond(inlist(month,1,2),1, ///
                  cond(inlist(month,3,4),2, ///
                  cond(inlist(month,5,6),3, ///
                  cond(inlist(month,7,8),4, ///
                  cond(inlist(month,9,10),5,6)))))
				  				  
        drop if dtrip == 0
		
        keep  mode month month1 dtrip draw date wave region

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + month1
        encode domain1, gen(domain3)
		
		gen waters="CHES" if inlist(region, "MDCHES", "VACHES")
		replace waters="CSTL" if !inlist(region, "MDCHES", "VACHES")
		
        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n
		drop domain1 domain  month 
		gen state=substr(region, 1,2)

		levelsof domain3, local(domz)
		levelsof state, local(st) clean 
		levelsof waters, local(wtrs) clean
		
		tempfile trips
		save `trips', replace 
				 
		clear
		tempfile master
		save `master', emptyok
		
	foreach d of local domz{
		u `trips', clear
		
		*test values
		*local d 400
		*local wtrs = "CHES"
		*local st "MD"
		*local i 1
		
		keep if domain3==`d'
		count
		local n_needed=`r(N)'
		levelsof month1, local(mnth) clean 
		levelsof mode, local(md) clean 

		
		import excel using "$iterative_input_data_cd\calib_catch_draws_`st'`i'.xlsx", clear firstrow
		
		split my_dom_id_string, parse(_)
		rename my_dom_id_string1 state
		rename my_dom_id_string2 waters
		rename my_dom_id_string4 month
		rename my_dom_id_string5 mode
		drop my_dom_id_string3 my_dom_id_string6
		keep if waters=="`wtrs'"
		keep if month=="`mnth'"
		keep if mode=="`md'"
		
		expand 2 
		sample `n_needed', count 
		gen tripid = ceil(_n/30)
		gen catch_draw = mod(_n-1,30) + 1
		drop id sim_id month
		gen domain3=`d'
		
		merge 1:1 domain3  tripid catch_draw using `trips', keep(3) nogen 
		
		
		* trip costs
		 preserve
                use "$input_data_cd\trip_costs.dta", clear
				keep if state=="`st'"
                keep if mode == "`md'"
                count
				return list		
				sample 50, count
                replace tripid = _n
                tempfile costs
                save `costs', replace
          restore
			
          merge m:1 tripid using `costs', keep(3) nogen
		
		levelsof wave, local(wv) 
		
		drop my domain3 month1  
		gen date_num = date(date, "DMY")
		format date_num %td
		drop date
		rename date_num date
		order  state waters region mode date trip catch  str_* blu*
		
		* age and avidity 
		preserve
                use "$input_data_cd\angler_dems.dta", clear
				keep if state=="`st'"
                keep if wave == `wv'
					
				sample 50, count
                gen tripid = _n
                tempfile dems
                save `dems', replace
			restore
				
			merge m:1 tripid using `dems', keep(3) nogen
		
        append using `master'
        save `master', replace
        clear                            
        }

		use `master', clear
		compress                             

        save "$iterative_input_data_cd\calib_catch_draws_`s'`i'.dta", replace
                   
		}

 }
 
 */

 
* faster version 
local regions "CTALL DEDERIV DENANT DEOCN MAALL MDCHES MDOCN MDPOT MEALL NCALB NCCNTRL NCOCN NHALL NJALL NYHUDN NYHUDS RIALL VACHES VAOCN VAPOTO"

set more off
set rmsg off
	
foreach s of local regions {
	
	*local s "VAOCN"
    import delimited using "$iterative_input_data_cd\directed_trip_draws_`s'.csv", clear

    gen double date_num = date(date, "DMY")
    gen byte   month    = month(date_num)
    gen str2   month1   = string(month, "%02.0f")
    gen byte   wave     = cond(inlist(month,1,2),1, ///
                        cond(inlist(month,3,4),2, ///
                        cond(inlist(month,5,6),3, ///
                        cond(inlist(month,7,8),4, ///
                        cond(inlist(month,9,10),5,6)))))
    drop date_num

    drop if dtrip==0

    gen str2 state = substr(region,1,2)
    gen str5 waters = "CSTL"
    replace waters  = "CHES" if inlist(region,"MDCHES","VACHES")

	drop dtrip striper_min striper_max striper_bag bluefish_min bluefish_bag striper_min_y2 striper_max_y2 striper_bag_y2 bluefish_min_y2 bluefish_bag_y2   
	
	tempfile base
    save `base', replace

    *-----------------------------------------
    * 2) Loop draws
    *-----------------------------------------
    forvalues i=1/$ndraws {
		*local i 1
        use `base', clear
        keep if draw==`i'

        * Expand to 50 trips x 30 catch draws within each (mode,date)
        egen long dom = group(mode date)   // replaces encode(domain1)
        expand 50
        bysort mode date: gen int tripid = _n
        expand 30
        bysort mode date tripid: gen byte catch_draw = _n

		egen group=group(date tripid mode)
		
		qui distinct group if mode=="pr"
		local n_pr = `r(ndistinct)'
		
		qui distinct group if mode=="fh"
		local n_fh = `r(ndistinct)'
		
		qui distinct group if mode=="sh"
		local n_sh = `r(ndistinct)'
		
		preserve 
		keep date mode tripid
		duplicates drop 
		by mode: gen mode_id=_n
		tempfile mode_id
		save `mode_id', replace
		restore 
		
		merge m:1 date mode tripid using `mode_id', keep(3) nogen  
		
		qui distinct group if wave==1
		local n_wave1 = `r(ndistinct)'
		
		qui distinct group if wave==2
		local n_wave2 = `r(ndistinct)'
		
		qui distinct group if wave==3
		local n_wave3 = `r(ndistinct)'
		
		qui distinct group if wave==4
		local n_wave4 = `r(ndistinct)'
		
		qui distinct group if wave==5
		local n_wave5 = `r(ndistinct)'
		
		qui distinct group if wave==6
		local n_wave6 = `r(ndistinct)'
		
		preserve 
		keep date wave tripid
		duplicates drop 
		sort date wave tripid
		bysort wave: gen wave_id=_n
		tempfile wave_id
		save `wave_id', replace
		restore 
		
		merge m:1 date wave tripid using `wave_id', keep(3) nogen  
		
		
        *-------------------------------
        * Costs: resample ONCE per draw
        *-------------------------------
        preserve
            use "$input_data_cd\trip_costs.dta", clear
            keep if state == substr("`s'",1,2)   // or use trips' state; see note below
        restore
        * Better: use state/waters from the trips file (more robust):
        local st = state[1]

        preserve
            use "$input_data_cd\trip_costs.dta", clear
            keep if state=="`st'"
            keep state mode cost  // whatever your cost var is called
            tempfile costspool
            save `costspool', replace
        restore

       preserve
            clear
            tempfile costs50
            save `costs50', emptyok replace
			
            foreach md in fh pr sh{   // use your actual 3 modes
                use `costspool', clear
                keep if mode=="`md'"
				
				local n_needed = cond("`md'"=="pr", `n_pr', ///
                         cond("`md'"=="fh", `n_fh', `n_sh'))

				quietly count
				local mult = ceil(`n_needed'/r(N))
				expand `mult'
				sample `n_needed', count
                gen int mode_id = _n
				
                keep mode mode_id cost
                append using `costs50'
                save `costs50', replace
            }	
        restore

        merge m:1 mode mode_id using `costs50', keep(3) nogen

		
        *--------------------------------
        * Dems: resample ONCE per draw
        *--------------------------------
        preserve
            use "$input_data_cd\angler_dems.dta", clear
            keep if state=="`st'"
            tempfile demspool
            save `demspool', replace
        restore

        preserve
            clear
            tempfile dems50
            save `dems50', emptyok replace

            forvalues w=1/6 {
                use `demspool', clear
                keep if wave==`w'
				
				local n_needed = cond(`w'==1, `n_wave1', ///
                 cond(`w'==2, `n_wave2', ///
				 cond(`w'==3, `n_wave3', ///
				 cond(`w'==4, `n_wave4', ///
				 cond(`w'==5, `n_wave5', `n_wave6')))))
				
				quietly count
				local mult = ceil(`n_needed'/r(N))
				expand `mult'
				sample `n_needed', count
				
                gen wave_id = _n
                keep wave wave_id age total_trips_12 /* other vars */
                append using `dems50'
                save `dems50', replace
            }
        restore

        merge m:1 wave wave_id using `dems50', keep(3) nogen


        preserve
            import excel using "$iterative_input_data_cd\calib_catch_draws_`st'`i'.xlsx", clear firstrow
            split my_dom_id_string, parse(_)
            rename my_dom_id_string1 state
            rename my_dom_id_string2 waters
            rename my_dom_id_string4 month
            rename my_dom_id_string5 mode
            drop my_dom_id_string3 my_dom_id_string6
            keep my_dom_id_string state waters month mode  str_* blu_*
            tempfile excelpool
            save `excelpool', replace
        restore

        *---------------------------------------
        * BIG SPEEDUP:
        * sample catch outcomes by (mode,month1)
        *---------------------------------------
        egen long g = group(mode month1)
        bysort g: gen long gid = _n
        bysort g: gen long n_g = _N
        levelsof g, local(gs)

        tempfile trips_expanded
        save `trips_expanded', replace

        * Build catch outcomes dataset with keys (g, gid)
        clear
        tempfile catchall
        save `catchall', emptyok replace
        local seeded 0

        foreach gg of local gs {

            use `trips_expanded', clear
            keep if g==`gg'
            keep mode month1 waters
            local md  = mode[1]
            local mn  = month1[1]
            local wtr = waters[1]
            local n_needed = _N

            use `excelpool', clear
            keep if waters=="`wtr'" & month=="`mn'" & mode=="`md'"

			quietly count
			local mult = ceil(`n_needed'/r(N))
			expand `mult'
			sample `n_needed', count
			
            * If you need more control: ensure enough rows before sampling
            quietly count
            if (r(N) < `n_needed') {
                di as error "Not enough catch rows for st=`st' draw=`i' mode=`md' month=`mn' waters=`wtr' need=`n_needed' have=" r(N)
                continue
            }

            gen long g   = `gg'
            gen long gid = _n
			destring month, replace

            tempfile chunk
            save `chunk', replace

            if (`seeded'==0) {
                use `chunk', clear
				destring month, replace
                save `catchall', replace
                local seeded 1
            }
            else {
                use `catchall', clear
                append using `chunk'
				destring month, replace
                save `catchall', replace
            }
        }
		
        * Merge sampled catch onto trips by (g,gid)
        use `trips_expanded', clear
		destring month, replace 
        merge 1:1 g gid using `catchall', keep(3) nogen

        drop g gid n_g
        compress
		
		sort date tripid catch_
		drop month1 month wave dom group mode_id wave_id my_dom_id_string 
		gen double date_num = date(date, "DMY")
		format date_num %td
		drop date 
		rename date_num date 
		order state region waters mode date tripid catch 
		compress

		save "$iterative_input_data_cd\calib_catch_draws_`s'`i'.dta", replace
		
}		
}

			
			
			
			
			
			
			
			
			
			
			
			
************** Generate catch draw files ******************
local s "MA"
* This code pulls in the catch-per-trip data estimated by the copula function in R
 forvalues i = 1/$ndraws {

		*----------------------------------------------------
        * Read simulated catch draws
        *----------------------------------------------------
		local i=1
		local s="MD"
		import excel using "$iterative_input_data_cd\calib_catch_draws_`s'`i'.xlsx", clear firstrow

        distinct id
        local n_simulated_draw = r(ndistinct) //number of catch draws simulation per strata
		di `n_simulated_draw'
		
		replace str_rel_sim=round(str_rel_sim)
		replace blu_rel_sim=round(blu_rel_sim)

        gen str_cat  = str_keep_sim + str_rel_sim
        gen blu_cat = blu_keep_sim + blu_rel_sim

        mvencode str_keep_sim str_cat str_rel_sim blu_keep_sim blu_rel_sim blu_cat ,  mv(0) override
		split my_dom_id_string, parse(_)
		
		rename my_dom_id_string1 state
		rename my_dom_id_string2 waters
		rename my_dom_id_string4 month
		rename my_dom_id_string5 mode
		drop my_dom_id_string3 my_dom_id_string6
        replace my_dom_id_string = state+"_"+waters+"_"+month + "_" + mode + "_STR"

        order  my_dom_id_string sim_id id str_keep_sim str_cat str_rel_sim blu_keep_sim blu_rel_sim blu_cat 
		drop state month mode water 
        compress                     
        tempfile base
        save `base', replace

        *----------------------------------------------------
        * Pull directed trips for draw `i'
        *----------------------------------------------------
		local i=1
		local s="MA"
		
        import delimited using "$iterative_input_data_cd\directed_trip_draws_`s'.csv", clear
		keep if draw == `i'

		gen date_num = date(date, "DMY")
		gen month = month(date_num)	
		drop date_num
		
		gen month1 = string(month, "%02.0f")

		gen wave = 1 if inlist(month,1,2)
        replace wave=2 if inlist(month,3,4)
        replace wave=3 if inlist(month,5,6)
        replace wave=4 if inlist(month,7,8)
        replace wave=5 if inlist(month,9,10)
        replace wave=6 if inlist(month,11,12)
		
        drop if dtrip == 0
		
        keep  mode month month1 dtrip draw date wave state

        gen domain1 = mode + "_" + date
        gen domain  = mode + "_" + month1
        encode domain1, gen(domain3)

        expand 50
        bysort domain1 : gen tripid = _n
        expand 30
        bysort domain1 tripid: gen catch_draw = _n
		drop domain1 domain domain3 month 

        gen my_dom_id_string = state+"_"+month1 + "_" + mode + "_STR"
        levelsof my_dom_id_string, local(domains)

        compress                     
        tempfile trips
        save `trips', replace
        clear                        

        *----------------------------------------------------
        * Loop over domains inside state/draw
        *----------------------------------------------------
		 * Mata objects can pile up — clear every state iteration 
		 
		mata: mata clear
		clear
		tempfile master
		save `master', emptyok
		
       foreach d of local domains {
			*local d="MA_08_fh_STR"
			*di "`d'"
            use `trips', clear
            keep if my_dom_id_string == "`d'"

            gen merge_id = _n
            levelsof month, local(mnth)
            levelsof mode, local(md)
            levelsof wave, local(wv)

            tempfile trips2
            save `trips2', replace
            local n = _N                   
            clear                           

            *  Catch draw rows for the same domain
            use `base', clear
            keep if my_dom_id_string == "`d'"
				
			count
			local n_obs=`r(N)'
			di `n_obs'
            if `n_obs' == 0 {                     // domain absent in catch draws
                set obs `n' 
                foreach v in str_keep_sim str_rel_sim str_cat blu_keep_sim blu_rel_sim blu_cat {
                    replace `v' = 0
                }
                gen merge_id = _n
                replace my_dom_id_string = "`d'"
            }
			
            else {
                local expand = ceil(`n' / `n_simulated_draw')
				di `expand'
                expand `expand'
                sample `n', count
                gen merge_id = _n
            }

            merge 1:1 merge_id using `trips2', keep(3) nogen
            drop merge_id                     

            tostring tripid, gen(tripid2)
            gen id_code = tripid2 + "_" + date
            egen group_id = group(id_code)
			distinct id_code
			return list
			local n_dems = `r(ndistinct)' // number of trips/anglers requiring demographics/trip costs that are constant across catch draws
            drop tripid2 id_code             
			
			/*
            *  (iii)  Angler demographics
            preserve
                use "$input_data_cd\angler_dems.dta", clear
                keep if wave == `wv'
				count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile dems
                save `dems', replace
			restore
				
			merge m:1 group_id using `dems', keep(3) nogen
			*/
            *  (iv)  Trip costs
            preserve
                use "$input_data_cd\trip_costs.dta", clear
				keep if state=="`s'"
                keep if mode == `md'
                count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile costs
                save `costs', replace
            restore
			
            merge m:1 group_id using `costs', keep(3) nogen
			
			/*
			*  (v)  Angler preferences
            preserve
                use "$iterative_input_data_cd\preference_params.dta", clear
				keep if draw==`i'
				drop draw
                count
				return list
				
				if `r(N)'<`n_dems'{
				local expand=round(`n_dems'/`r(N)')+1
				expand `expand'
					}
					
				sample `n_dems', count
                gen group_id = _n
                tempfile costs
                save `costs', replace
            restore
			
            merge m:1 group_id using `costs', keep(3) nogen
			*/

            keep my_dom_id_string draw ///
                 str_keep_sim str_cat str_rel_sim ///
                 blu_keep_sim blu_rel_sim blu_cat ///
                 mode date dtrip  ///
                 tripid catch_draw 
				 *age total_trips_12 day cost beta*

            order draw my_dom_id_string mode  date tripid catch_draw dtrip
			gen date_num = date(date, "DMY")
			gen month = month(date_num)
			gen day = day(date_num)	
			
			drop date_num date
			
            sort month day tripid catch_draw
            compress                         

        // Append to growing master dataset
        append using `master'
        save `master', replace
        clear                            
        }

        *----------------------------------------------------
        * combine domains and write output for draw i
        *----------------------------------------------------
		use `master', clear
		compress                             

        save "$iterative_input_data_cd\calib_catch_draws_`s'`i'.dta", replace
        clear                               
    }



