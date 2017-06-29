global ROOT "\\stu05-fsrv.ad.syr.edu\ykbagir$\Downloads"

*GROUP DUMMIES

use "${ROOT}\maplight_bill_position.dta", clear
keep if legislative_session==112
drop if strpos( bill_number , "S")!=0
drop if strpos( motion , "Passed Senate")!=0
drop if strpos( motion , "On Passage of the Bill")!=0
drop if strpos( motion , "On the Conference Report H.R.")!=0
drop if strpos( motion , "Received in the Senate")!=0
*Generate dummies for each catcode
gen manuf=0
replace manuf=1 if OS_catcode=="LM100"
gen dairy=0
replace dairy=1 if OS_catcode=="A2000"
gen computer=0
replace computer=1 if OS_catcode=="C5120"
gen oil=0
replace oil=1 if OS_catcode=="E1100"
gen com_banks=0
replace com_banks=1 if OS_catcode=="F1100"
gen chem=0
replace chem=1 if OS_catcode=="M1000"
gen agr_chem=0
replace agr_chem=1 if OS_catcode=="A4100"
gen ind_eq=0
replace ind_eq=1 if OS_catcode=="M2300"
gen stone=0
replace stone=1 if OS_catcode=="B5100"
gen cons_eq=0
replace cons_eq=1 if OS_catcode=="B6000"
order action_id OS_catcode
sort action_id OS_catcode
duplicates drop (action_id OS_catcode), force
sort OS_catcode action_id
*to have a relevant join across two data sets I drop some unnecessary variables 
drop bill_topic bill_description motion vote_roll date sector industry maplight_url
drop legislative_session
drop org_name bill_number
sort action_id OS_catcode
*Use joinby command to combine two data sets
joinby using "${ROOT}\kristy\raw_votes.dta"
sort action_id politician_id OS_catcode
order action_id politician_id OS_catcode
*Joinby command adds everything all together. This is good because we do not loose any observation 
*however it generates too many unrelevant duplicated rows. Here, I delete duplicated rows
*across interest action_id politician_id manuf banks dairy computer estate oil
duplicates drop ( action_id politician_id manuf dairy computer oil chem agr_chem stone cons_eq ind_eq com_banks), force
*I still have duplicated rows for those votes at least two interest groups take position on.
*I assign the max value of a dummy (which is 1 if a gorup takes position) to all duplicated 
*rows in order not to loose information
bys action_id : egen group2 = max( dairy )
bys action_id : egen group3 = max( computer )
bys action_id : egen group4 = max( oil )
bys action_id : egen group5 = max( manuf )
bys action_id : egen group6 = max( chem )
bys action_id : egen group7 = max( agr_chem )
bys action_id : egen group8 = max( stone )
bys action_id : egen group9 = max( cons_eq )
bys action_id : egen group10 = max( ind_eq )
bys action_id : egen group11 = max( com_banks )


*Now, I drop all these duplicated rows to get the final results for group dummies
duplicates drop ( action_id politician_id ), force
*I generate a new dummy variable which takes the value of one across all observations.
gen group1=1
order group1
gen i= _n
order i
reshape long group, i(i) j(j)
keep j group
sort j
*save "${ROOT}\group_trial.dta"
export delimited using "${ROOT}\groups.csv", replace

**************** OUTPUT FROM R *******************************************
*Output from R includes all parameters together. I seperate out parameters alpha and gamma  from the overall output
sort Parameter 
egen newid = group( Parameter )
*drop those irrelevant parameteters ((159+159+445)*11=8393+4=8397)
drop if newid>8397
save "${ROOT}\output_8_13_without_total.dta", replace
clear
*now keep only alpha for the final analyses (445*11=4895)
use "${ROOT}\output_8_13_without_total.dta"
drop if newid>4895
save "${ROOT}\output_8_13_alpha.dta"
clear 
*Now we can generate summary statistics for the parameters of interest using below command
by Parameter: egen mean=mean( value )
by Parameter: egen sd=sd( value )
duplicates drop ( mean sd), force
save "${ROOT}\stat_5000_8_13.dta"
clear
**GRAPH
use "${ROOT}\stat_5000_8_13.dta"
gen alpha_group1=
twoway (scatter sd mean in 1/445, sort)
graph save Graph "${ROOT}\alpha_1_8_13.gph"
twoway (scatter sd mean in 446/890, sort)
graph save Graph "${ROOT}\alpha_2_8_13.gph"
twoway (scatter sd mean in 891/1335, sort)
graph save Graph "${ROOT}\alpha_3_8_13.gph"
twoway (scatter sd mean in 1336/1780, sort)
graph save Graph "${ROOT}\alpha_4_8_13.gph"
twoway (scatter sd mean in 1781/2225, sort)
graph save Graph "${ROOT}\alpha_5_8_13.gph"
twoway (scatter sd mean in 2226/2670, sort)
graph save Graph "${ROOT}\alpha_6_8_13.gph"
twoway (scatter sd mean in 2671/3115, sort)
graph save Graph "${ROOT}\alpha_7_8_13.gph"
twoway (scatter sd mean in 3116/3560, sort)
graph save Graph "${ROOT}\alpha_8_8_13.gph"
twoway (scatter sd mean in 3561/4005, sort)
graph save Graph "${ROOT}\alpha_9_8_13.gph"
twoway (scatter sd mean in 4006/4450, sort)
graph save Graph "${ROOT}\alpha_10_8_13.gph"
twoway (scatter sd mean in 4451/4890, sort)
graph save Graph "${ROOT}\alpha_11_8_13.gph"
clear
***COMBINED GRAPH
use "${ROOT}\stat_5000_8_13.dta"
* drop the one outlier from manufacturing
drop if sd>2.5 in 1781/2225
*because the order numbers have changed by "1" due to drop command, I reorganized the numbers
twoway (scatter sd mean in 2225/2669, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.5) titlegap(1)) xtitle(Legislator's Ideal Point) title(Chemicals ) name(chemicals,replace)
twoway (scatter sd mean in 1781/2224, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.5) titlegap(1)) xtitle(Legislator's Ideal Point) title(Manufacturing Un ) name(manufacturing,replace)
twoway (scatter sd mean in 4450/4889, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.5) titlegap(1)) xtitle(Legislator's Ideal Point) title(Commercial Banks) name(com_bank,replace)
graph combine chemicals manufacturing com_bank, rows(1) 
graph export "${ROOT}\combined_graph.pdf", as(pdf) replace
clear

****I COULDN'T FIND A WAY TO STORE THESE RESULTS AS NEW VARIABLES SO THAT WE CAN PRODUCE THE GRAPHICAL ILLUSTRATION.
