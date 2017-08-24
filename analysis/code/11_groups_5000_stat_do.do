/********************************************************************************
PURPOSE OF THIS FILE:
Get parameter from Bayesian result(rstan)
****************************************************************************************/
/* A few initial settings of Stata */
version 14
clear all
set more off

/* Define the working directory 
(Get current directory and use upper level dir as working dir, then use relative path in the following code;
so run the script from their location in the directory) */
local curr_dir `c(pwd)'
local work_dir "`curr_dir'\.."
cd `work_dir'

/****************************************************************************************************
Create local macro names for input and output directories and files
*****************************************************************************************************/
/* Directory for inputs */
local inputdir "`work_dir'\temp"
/* Use output data from R as input */
local input_data "mcmc_output.dta"

/* Directory for output */
local outputdir "`work_dir'\output"
/* Output file */
local output "11_groups_5000_stat" 
local output_log "`output'.log"
local output_data "`output'.dta"

/* Intermediate file */
local tempdir "`work_dir'\temp"

/*******************************************************************************************
Output from R includes all parameters together. I seperate out parameters alpha and gamma  from the overall output
********************************************************************************************/

/* Open log file */
capture log close
cd `tempdir'
log using `output_log', replace

/* Read the output results from mcmc simulation */
cd `inputdir'
use `input_data', clear

sort Parameter 
egen newid = group( Parameter )
/*drop those irrelevant parameteters ((159+159+445)*11=8393+4=8397)*/
drop if newid>8397

/*now keep only alpha for the final analyses (445*11=4895)*/
drop if newid>4895

/*Now we can generate summary statistics for the parameters of interest using below command*/
by Parameter: egen mean=mean( value )
by Parameter: egen sd=sd( value )
duplicates drop ( mean sd), force

/* Save cleaned dataset */
cd `outputdir'
save `output_data', replace

/* Close the log file */
log close
