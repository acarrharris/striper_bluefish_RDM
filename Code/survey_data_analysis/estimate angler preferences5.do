

set seed 03211990

u "$input_data_cd\striper_CE_data.dta", clear 

gen log_striper_length=log(striper_length) if inlist(alt, 1,2)
mvencode log_striper_length, mv(0)
gen sqrt_striper_length=sqrt(striper_length)

gen log_striper_keep=log(striper_keep) if inlist(alt, 1,2)
gen log_striper_rel=log(striper_rel) if inlist(alt, 1,2)
gen log_bluefish_keep=log(bluefish_keep) if inlist(alt, 1,2)
gen log_bluefish_rel=log(bluefish_rel) if inlist(alt, 1,2)

mvencode log_striper_keep log_striper_rel log_bluefish_keep log_bluefish_rel, mv(0) override

gen striper_catch=striper_keep+striper_rel
gen sqrt_striper_catch=sqrt(striper_catch)

gen sqrt_striper_keep_log_length=log_striper_length*sqrt_striper_keep
gen sqrt_striper_rel_log_length=log_striper_length*sqrt_striper_rel

gen sqrt_striper_keep_sqrt_length=sqrt_striper_length*sqrt_striper_keep
gen sqrt_striper_rel_sqrt_length=sqrt_striper_length*sqrt_striper_rel

gen sqrt_striper_catch_log_length=log_striper_length*sqrt_striper_catch
gen sqrt_striper_catch_sqrt_length=sqrt_striper_length*sqrt_striper_catch

gen sqrt_striper_catch_length=striper_length*sqrt_striper_catch
gen sqrt_striper_catch_length2=striper_length^2*sqrt_striper_catch


clogit choice cost  ///
		sqrt_striper_catch ///
		sqrt_striper_catch_length sqrt_striper_catch_length2 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if no_choice_protest!=1 & alt3_choice_protest!=1 &  alt1_choice_protest!=1 & alt2_choice_protest!=1 , group(gid)
			
clogit choice cost  ///
		sqrt_striper_catch ///
		sqrt_striper_catch_length sqrt_striper_catch_length2 ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)
			
clogit choice cost  ///
		sqrt_striper_catch ///
		sqrt_striper_catch_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)			

clogit choice cost  ///
		sqrt_striper_catch ///
		sqrt_striper_catch_sqrt_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)				
			
local minL = 18
local maxL = 43			
tempfile wtp_fish
postfile F double striper_length cL basecatch ///
    wtp_keep1 se_keep1 lb_keep1 ub_keep1 ///
    wtp_rel1  se_rel1  lb_rel1  ub_rel1 ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {
        local z = `L'

quietly nlcom ///
        (wtp_keep: -(_b[sqrt_striper_catch] ///
                     + _b[sqrt_striper_catch_log_length]*log(`z'))  ///
                     / (2*sqrt(`K')*_b[cost])) 

        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar wr  = b[1,2]
        scalar sek = sqrt(V[1,1])
        scalar ser = sqrt(V[2,2])

        post F (`L') (`z') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `wtp_fish', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep1 ub_keep1 striper_length if basecatch==1, sort  ///
        color(navy%30) lcolor(navy%40)) ///
    (line  wtp_keep1 striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP catch, catch*log(length)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))




set seed 03211990

u "$input_data_cd\striper_CE_data.dta", clear 

gen log_striper_length=log(striper_length) if inlist(alt, 1,2)
mvencode log_striper_length, mv(0)
gen sqrt_striper_length=sqrt(striper_length)

gen log_striper_keep=log(striper_keep) if inlist(alt, 1,2)
gen log_striper_rel=log(striper_rel) if inlist(alt, 1,2)
gen log_bluefish_keep=log(bluefish_keep) if inlist(alt, 1,2)
gen log_bluefish_rel=log(bluefish_rel) if inlist(alt, 1,2)

mvencode log_striper_keep log_striper_rel log_bluefish_keep log_bluefish_rel, mv(0) override

gen striper_catch=striper_keep+striper_rel
gen sqrt_striper_catch=sqrt(striper_catch)

gen sqrt_striper_keep_log_length=log_striper_length*sqrt_striper_keep
gen sqrt_striper_rel_log_length=log_striper_length*sqrt_striper_rel

gen sqrt_striper_keep_sqrt_length=sqrt_striper_length*sqrt_striper_keep
gen sqrt_striper_rel_sqrt_length=sqrt_striper_length*sqrt_striper_rel

gen sqrt_striper_catch_log_length=log_striper_length*sqrt_striper_catch
gen sqrt_striper_catch_sqrt_length=sqrt_striper_length*sqrt_striper_catch

gen sqrt_striper_catch_length=striper_length*sqrt_striper_catch
gen sqrt_striper_catch_length2=striper_length^2*sqrt_striper_catch

gen striper_keep1=1 if striper_keep==1 & inlist(alt, 1, 2)
gen striper_keep2=1 if striper_keep==2 & inlist(alt, 1, 2)
gen striper_keep3=1 if striper_keep==3 & inlist(alt, 1, 2)
mvencode striper_keep*, mv(0) override 

split version, parse(" ")
rename version1 svy_version
drop version2 version3 version4

gen opt_other_bag	= 	opt_other*striper_bag
gen opt_other_min	= 	opt_other*striper_min
gen opt_other_min_max	= 	opt_other*striper_min*max_dummy
gen opt_other_min_max_slot	= 	opt_other*striper_min*max_dummy*slot_length
					
gen opt_out_bag	= 	opt_out*striper_bag
gen opt_out_min	= 	opt_out*striper_min
gen opt_out_min_max	= 	opt_out*striper_min*max_dummy
gen opt_out_min_max_slot	= 	opt_out*striper_min*max_dummy*slot_length


gen striper_rel_18_23=striper_rel if inlist(striper_length, 18, 23) & striper_rel>0
gen striper_rel_28_33=striper_rel if inlist(striper_length, 28, 33) & striper_rel>0
gen striper_rel_38_43=striper_rel if inlist(striper_length, 38, 43) & striper_rel>0

gen striper_keep_18_23=striper_keep if inlist(striper_length, 18, 23) & striper_keep>0
gen striper_keep_28_33=striper_keep if inlist(striper_length, 28, 33) & striper_keep>0
gen striper_keep_38_43=striper_keep if inlist(striper_length, 38, 43) & striper_keep>0
gen striper_keep_18_33=striper_keep if inlist(striper_length, 18, 23, 28, 33) & striper_keep>0


gen striper_keep_18=striper_keep if inlist(striper_length, 18) & striper_keep>0
gen striper_keep_28=striper_keep if inlist(striper_length, 28) & striper_keep>0
gen striper_keep_38=striper_keep if inlist(striper_length, 38) & striper_keep>0
gen striper_keep_23=striper_keep if inlist(striper_length, 23) & striper_keep>0
gen striper_keep_33=striper_keep if inlist(striper_length, 33) & striper_keep>0
gen striper_keep_43=striper_keep if inlist(striper_length, 43) & striper_keep>0

gen striper_rel_18=striper_rel if inlist(striper_length, 18) & striper_rel>0
gen striper_rel_28=striper_rel if inlist(striper_length, 28) & striper_rel>0
gen striper_rel_38=striper_rel if inlist(striper_length, 38) & striper_rel>0
gen striper_rel_23=striper_rel if inlist(striper_length, 23) & striper_rel>0
gen striper_rel_33=striper_rel if inlist(striper_length, 33) & striper_rel>0
gen striper_rel_43=striper_rel if inlist(striper_length, 43) & striper_rel>0


gen striper_catch_18=striper_catch if inlist(striper_length, 18) & striper_catch>0
gen striper_catch_28=striper_catch if inlist(striper_length, 28) & striper_catch>0
gen striper_catch_38=striper_catch if inlist(striper_length, 38) & striper_catch>0
gen striper_catch_23=striper_catch if inlist(striper_length, 23) & striper_catch>0
gen striper_catch_33=striper_catch if inlist(striper_length, 33) & striper_catch>0
gen striper_catch_43=striper_catch if inlist(striper_length, 43) & striper_catch>0


local vars striper_keep_18_23 striper_keep_28_33 striper_keep_38_43 striper_rel_18_23 striper_rel_28_33 striper_rel_38_43 striper_keep_18_33 ///
striper_keep_18  striper_keep_28 striper_keep_38 striper_keep_23 striper_keep_33 striper_keep_43 ///
striper_rel_18  striper_rel_28 striper_rel_38 striper_rel_23 striper_rel_33 striper_rel_43 ///
striper_catch_18  striper_catch_28 striper_catch_38 striper_catch_23 striper_catch_33 striper_catch_43

foreach v of local vars{
	gen sqrt_`v'= sqrt(`v')
}
mvencode striper_rel_* striper_keep_* striper_catch* sqrt_striper_rel_* sqrt_striper_keep_* sqrt_striper_catch_* , mv(0) override

gen striper_keep_18_33_log_length=striper_keep_18_33*log_striper_length
mvencode striper_keep_18_33_log_length, mv(0) override 
	
gen tab=1 if striper_keep==striper_rel & inlist(alt,1, 2)	
egen sumtab=sum(tab), by(gid)

local vars sqrt_striper_keep sqrt_striper_keep_lengh sqrt_striper_keep_18  sqrt_striper_keep_23 sqrt_striper_keep_28 sqrt_striper_keep_33 sqrt_striper_keep_38   sqrt_striper_keep_43
foreach v of local vars{
	gen `v'_adv=`v'*fishing_skill_3_4
}

gen striper_keep_more1=1 if striper_keep>1 & inlist(alt, 1, 2)
egen sumstriper_keep_more1=sum(striper_keep_more1), by(gid)
gen sqrt_striper_keep_log_length_adv=sqrt_striper_keep_log_length*sqrt_striper_keep_log_length

clogit choice cost  ///
	sqrt_striper_keep_18  sqrt_striper_keep_23 sqrt_striper_keep_28 sqrt_striper_keep_33 sqrt_striper_keep_38   sqrt_striper_keep_43 ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1 & sumstriper_keep_more1!=1, group(gid)

clogit choice cost  ///
	sqrt_striper_keep_18  sqrt_striper_keep_18_ad ///
	sqrt_striper_keep_23 sqrt_striper_keep_23_ad /// 
	sqrt_striper_keep_28 sqrt_striper_keep_28_ad ///
	sqrt_striper_keep_33 sqrt_striper_keep_33_ad ///
	sqrt_striper_keep_38   sqrt_striper_keep_38_ad ///
	sqrt_striper_keep_43 sqrt_striper_keep_43_ad ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1 & sumtab!=1, group(gid)

clogit choice cost  ///
	sqrt_striper_keep_18  sqrt_striper_keep_18_ad ///
	sqrt_striper_keep_23 sqrt_striper_keep_23_ad /// 
	sqrt_striper_keep_28 sqrt_striper_keep_28_ad ///
	sqrt_striper_keep_33 sqrt_striper_keep_33_ad ///
	sqrt_striper_keep_38   sqrt_striper_keep_38_ad ///
	sqrt_striper_keep_43 sqrt_striper_keep_43_ad ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1 & sumtab!=1, group(gid)			

egen sum_first=sum(choice), by(qtid scenario)
egen sum_2nd=sum(choice2), by(qtid scenario)

preserve
drop if sum_2nd==0
drop if choice==1
replace choice=choice2
drop choice2
replace scenario =scenario+.5
tempfile second
save `second', replace 
restore 
append using `second'
sort qtid scenario alt
egen scenario2=group(scenario)
order scenario2
egen gid2=group(qtid scenario2)



tab striper_keep striper_rel
gen tab2=1 if striper_keep==striper_rel & inlist(alt, 1, 2)
egen sumtab2=sum(tab2), by(gid)
clogit choice cost  ///
	sqrt_striper_keep_18   ///
	sqrt_striper_keep_23  /// 
	sqrt_striper_keep_28  ///
	sqrt_striper_keep_33  ///
	sqrt_striper_keep_38    ///
	sqrt_striper_keep_43  ///
	sqrt_striper_rel_18   ///
	sqrt_striper_rel_23  /// 
	sqrt_striper_rel_28  ///
	sqrt_striper_rel_33  ///
	sqrt_striper_rel_38    ///
	sqrt_striper_rel_43  ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1 & sumtab2==0 & svy_version=="SHORE", group(gid)
			
	clogit choice cost  ///
	sqrt_striper_keep_18_23   ///
	sqrt_striper_keep_28_33  ///
	sqrt_striper_keep_38_43    ///
	sqrt_striper_rel_18_23   ///
	sqrt_striper_rel_28_33  ///
	sqrt_striper_rel_38_43    ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1 , group(gid)
					
			
clogit choice cost  ///
		sqrt_striper_keep sqrt_striper_keep_length  sqrt_striper_keep_length2 ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1 , group(gid)	
			
clogit choice cost  ///
		sqrt_striper_keep sqrt_striper_keep_log_length ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  opt_other_min opt_other_min_max opt_out_min_max_slot ///
			if second_choice_protest!=1  , group(gid)	
			
clogit choice cost  ///
		sqrt_striper_keep sqrt_striper_keep_sqrt_length ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)			

gen striper_keep1_length=striper_keep1*striper_length
gen striper_keep2_length=striper_keep2*striper_length
gen striper_keep3_length=striper_keep3*striper_length

clogit choice cost  ///
		striper_keep1 striper_keep1_length ///
		striper_keep2 striper_keep2_length /// 
		striper_keep3 striper_keep3_length ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)					


*model 10 threshold
gen keep_len_base32 = sqrt_striper_keep * striper_length
gen keep_len_above32 = sqrt_striper_keep * cond(striper_length > 32, striper_length - 32, 0)			

gen keep_len_base32_log = sqrt_striper_keep * log_striper_length
gen keep_len_above32_log = sqrt_striper_keep * cond(striper_length > 32, log_striper_length - log(32), 0)			

gen striper_keep_rel_interact=sqrt_striper_keep*sqrt_striper_rel
mvencode striper_keep_rel_interact, mv(0) override


					
gen striper_keep_new=striper_keep if striper_c_and_r_only==0
gen striper_rel_new=striper_rel if striper_c_and_r_only==0
replace striper_keep_new= 0 if striper_c_and_r_only==1
replace striper_rel_new=striper_keep + striper_rel if striper_c_and_r_only==1

gen sqrt_striper_keep_new=sqrt(striper_keep_new)
gen  sqrt_striper_rel_new=sqrt(striper_rel_new)
gen sqrt_striper_keep_new_log_length=sqrt_striper_keep_new*log_striper_length
gen sqrt_striper_rel_new_log_length=sqrt_striper_rel_new*log_striper_length

clogit choice cost  ///
		sqrt_striper_keep_new  sqrt_striper_keep_new_log_length ///
		sqrt_striper_rel_new sqrt_striper_rel_new_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)		
			
			
clogit choice cost  ///
		sqrt_striper_keep ///
		keep_len_base32 /// 
		keep_len_above32 ///
		striper_keep_rel_interact ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)		
			
			
clogit choice cost  ///
		sqrt_striper_keep ///
		keep_len_base32 /// 
		keep_len_above32 ///
		striper_keep_rel_interact ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)					

clogit choice cost  ///
		sqrt_striper_keep ///
		keep_len_base32_log /// 
		keep_len_above32_log ///
		sqrt_striper_rel sqrt_striper_rel_log_length ///
		sqrt_bluefish_keep sqrt_bluefish_rel opt_other opt_out ///
		opt_out_male opt_out_age ///
		opt_out_fishing_skill_3_4  ///
		opt_other_likely opt_other_days_str_blue12 opt_other_days_non_str_blue12  ///
			if second_choice_protest!=1, group(gid)				
			
			
local minL = 18
local maxL = 43			
tempfile wtp_fish
postfile F double striper_length cL basecatch ///
    wtp_keep1 se_keep1 lb_keep1 ub_keep1 ///
    wtp_rel1  se_rel1  lb_rel1  ub_rel1 ///
    using `wtp_fish', replace

foreach K in 1 2 {
    forvalues L = `minL'/`maxL' {
        local z = `L'

quietly nlcom ///
		        (wtp_keep:  -(_b[sqrt_striper_keep] ///
                     + _b[sqrt_striper_keep_sqrt_length]*sqrt(`z')) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[sqrt_striper_rel] ///
                     + _b[sqrt_striper_rel_log_length]*log(`z')) ///
                     / (2*sqrt(`K')*_b[cost])) 
					 
					         /*(wtp_keep:  -(_b[sqrt_striper_keep] ///
                     + _b[sqrt_striper_keep_length]*(`z') ///
                     + _b[sqrt_striper_keep_length2]*(`z'^2)) ///
                     / (2*sqrt(`K')*_b[cost])) ///
        (wtp_rel:  -(_b[sqrt_striper_rel] ///
                     + _b[sqrt_striper_rel_log_length]*log(`z')) ///
                     / (2*sqrt(`K')*_b[cost])) */
					 
        matrix b = r(b)
        matrix V = r(V)

        scalar wk  = b[1,1]
        scalar wr  = b[1,2]
        scalar sek = sqrt(V[1,1])
        scalar ser = sqrt(V[2,2])

        post F (`L') (`z') (`K') ///
            (wk) (sek) (wk-1.96*sek) (wk+1.96*sek) ///
            (wr) (ser) (wr-1.96*ser) (wr+1.96*ser)
    }
}
postclose F

use `wtp_fish', clear
label define basecatchlbl 1 "Base catch = 1 fish" 2 "Base catch = 2 fish"
label values basecatch basecatchlbl
twoway ///
    (rarea lb_keep1 ub_keep1 striper_length if basecatch==1, sort  ///
        color(navy%30) lcolor(navy%40)) ///
    (line  wtp_keep1 striper_length if basecatch==1, sort ///
        lcolor(navy) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP keep, keep*sqrt(length)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_keep, replace))

	
twoway ///
    (rarea lb_rel1 ub_rel1 striper_length if basecatch==1, sort  ///
        color(maroon%30) lcolor(maroon%40)) ///
    (line  wtp_rel1 striper_length if basecatch==1, sort ///
        lcolor(maroon) lwidth(medthick) ///
    xtitle("Length (inches)") ///
	legend(off) ///
    ytitle("$") ///
    title("MWTP rel, rel*log(length)", size(medium)) ///
    xlabel(18 23 28 33 38 43) ylab(#10) xlab(#15) name(wtp_rel, replace))
gr combine wtp_keep wtp_rel, cols(2) ycommon		


	
