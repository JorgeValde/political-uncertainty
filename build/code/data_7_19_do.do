***********************     CLEARING THE DATA
global ROOT "\\stu05-fsrv.ad.syr.edu\ykbagir$\Downloads"
use  "${ROOT}\maplight_bill_position.dta"
*I want to get rid of the observations for other sessions right away--it only
*eats up computation time to carry them around
keep if legislative_session=112

*This code works for the 112th Congress, but will need an extra line to get rid of
*""On the Conference Report - Senate" for whole sample
drop if strpos( bill_number , "S")!=0
drop if strpos( motion , "Passed Senate")!=0
drop if strpos( motion , "On Passage of the Bill")!=0
drop if strpos( motion , "On the Conference Report H.R.")!=0
drop if strpos( motion , "Received in the Senate")!=0

*I'm not familiar with using joinby and it's very computationally intensive, so
*I tried running this with a merge and verified that the two approaches produce
*the same number of observations.
sort action_id
*This "quietly" statement generates a count variable "dup" that numbers observations that are
*uniquely identified by action_id as zero, the first observation in an action_id
*by group as 1, the second as 2, etc.
quietly by action_id:  gen dup = cond(_N==1,0,_n)
*Then we drop anything that is a repeat, and drop the dup variable--no need to carry it around
drop if dup > 1
drop dup
*An added benefit of this approach: a quick describe gives us the total # of actions (159)
d,s

*Now we have a 1-to-many merge on action_id and won't need to drop duplicates
merge 1:m action_id using "${ROOT}\maplight_votes.dta"
*We DO need to drop all of the observations that are for Senate votes or House
*votes that took place outside of the 112th Congress; those will not have matched
*with an observation in the base data set, so we only keep those observations
*that were in both datasets.
keep if _merge == 3
*rename votes as 0 for NO and 1 for YES
replace vote=. if vote==9
replace vote=0 if vote==6
*Reorder politician and action id numbers starting from 1
*because politician_id and action_id are not numeric, here I am generating new string variables for them.
sort politician_id action_id
by politician_id : gen politician_id_numeric = 1 if _n==1
replace politician_id_numeric = sum( politician_id_numeric )
replace politician_id_numeric = . if missing( politician_id )
egen action_id_numeric = group( action_id )
replace action_id_numeric = . if missing( action_id )
*Generating the CSV data file for R.
save "${ROOT}\data_7_19.dta", replace
export excel using "${ROOT}\data_7_19.csv", firstrow(variables) replace
clear
