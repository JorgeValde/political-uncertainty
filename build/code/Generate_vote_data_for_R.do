/********************************************************************************
PURPOSE OF THIS FILE:
Create vote data file to be used in Rstan model 
****************************************************************************************/
/* A few initial settings of Stata */
version 15
clear all
set more off

/* Define the working directory 
(Get current directory and use upper level dir as working dir, then use relative path in the following code;
then run the code from their location in the directory) */
local curr_dir `c(pwd)'
local work_dir "`curr_dir'\.."
cd `work_dir'

/****************************************************************************************************
Create local macro names for input and output directories and files
*****************************************************************************************************/
/* Directory for inputs */
local inputdir "`work_dir'\input"
/* Use maplight data as input */
local input_data "maplight_bill_position.dta"
local vote_data "maplight_votes.dta"

/* Directory for output */
local outputdir "`work_dir'\output"
/* Output file */
local output "vote_data_rstan" 
local output_log "`output'.log"

/* Intermediate file */
local tempdir "`work_dir'\temp"

/****************************************************************************************************
Clear the data
*****************************************************************************************************/

/* Open log file */
capture log close
cd `tempdir'
log using `output_log', replace

/* Read the output results of alpha parameter from mcmc simulation */
cd `inputdir'
use `input_data', clear

/*Get rid of the observations for other sessions right away--it only
eats up computation time to carry them around */
keep if legislative_session==112

/*This code works for the 112th Congress, but will need an extra line to get rid of
""On the Conference Report - Senate" for whole sample */
drop if strpos( bill_number , "S")!=0
drop if strpos( motion , "Passed Senate")!=0
drop if strpos( motion , "On Passage of the Bill")!=0
drop if strpos( motion , "On the Conference Report H.R.")!=0
drop if strpos( motion , "Received in the Senate")!=0

/*I'm not familiar with using joinby and it's very computationally intensive, so
I tried running this with a merge and verified that the two approaches produce
the same number of observations. */
sort action_id

/*This "quietly" statement generates a count variable "dup" that numbers observations that are
uniquely identified by action_id as zero, the first observation in an action_id
by group as 1, the second as 2, etc. */
quietly by action_id:  gen dup = cond(_N==1,0,_n)

/*Then we drop anything that is a repeat, and drop the dup variable--no need to carry it around */
drop if dup > 1
drop dup

/*An added benefit of this approach: a quick describe gives us the total # of actions (159) */
d,s

/*Now we have a 1-to-many merge on action_id and won't need to drop duplicates */
merge 1:m action_id using `vote_data' 

/*We DO need to drop all of the observations that are for Senate votes or House
votes that took place outside of the 112th Congress; those will not have matched
with an observation in the base data set, so we only keep those observations
that were in both datasets. */
keep if _merge == 3
/*no need to keep this variable */
drop _merge

/*rename votes as 0 for NO and 1 for YES */
replace vote=. if vote==9
replace vote=0 if vote==6

/*drop observations missing vote, also notice dropping before create numeric id for politician and action */
drop if missing(vote)

/*Reorder politician and action id numbers starting from 1, because politician_id and action_id are not numeric, so generating new numeric variables for them. */
/* those are two methods of generating group identifier for variables */
sort politician_id action_id
/*set politician_id_numeric = 1 in the first observation of of each politician */
by politician_id : gen politician_id_numeric = 1 if _n==1
/* replace politician_id_numeric by its cumulative sum */
replace politician_id_numeric = sum( politician_id_numeric )
replace politician_id_numeric = . if missing( politician_id )
/*create action_id_numeric directory using egen */
egen action_id_numeric = group( action_id )
replace action_id_numeric = . if missing( action_id )

/* put those three important variables in the first three columns */
order politician_id_numeric action_id_numeric vote, before(legislative_session)

/* save file for generating dummies later */
cd `tempdir'
save "`output'.dta", replace

/*Generating the CSV data file for R. Since we add dummies into it later, only keep for comparision */
export delimited using "`output'.csv", replace

/* Close the log file */
log close
clear
