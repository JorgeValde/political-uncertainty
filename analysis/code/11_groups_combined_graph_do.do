/********************************************************************************
PURPOSE OF THIS FILE:
Create graph for firstary analysis
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
local inputdir "`work_dir'\output"
/* Use output data of alpha parameter as input */
local input_data "11_groups_5000_stat.dta"

/* Directory for output */
local outputdir "`work_dir'\output"
/* Output file */
local output "11_groups_combined_graph" 
local output_log "`output'.log"

/* Intermediate file */
local tempdir "`work_dir'\temp"

/*******************************************************************************************
COMBINED GRAPH
*******************************************************************************************/

/* Open log file */
capture log close
cd `tempdir'
log using `output_log', replace

/* Read the output results of alpha parameter from mcmc simulation */
cd `inputdir'
use `input_data', clear

twoway (scatter sd mean in 2226/2670 if sd<=2.5, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-3 3)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(Chemicals) name(chemicals,replace)
twoway (scatter sd mean in 1781/2225 if sd<=2.5, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-3 3)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(Manufacturing Unions) name(manufacturing,replace)
twoway (scatter sd mean in 4451/4890 if sd<=2.5, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-3 3)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(Commercial Banks) name(com_bank,replace)
graph combine chemicals manufacturing com_bank, rows(1) 

cd `outputdir'
graph export "`output'_ch1.pdf", as(pdf) replace
clear

/* Close the log file */
log close
