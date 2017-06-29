global ROOT "\\stu05-fsrv.ad.syr.edu\ykbagir$\Downloads"

**************** OUTPUT FROM R *******************************************
*Output from R includes all parameters together. I seperate out parameters alpha and gamma  from the overall output
sort Parameter 
egen newid = group( Parameter )
*drop those irrelevant parameteters ((159+159+445)*11=8393+4=8397)
drop if newid>8397
*now keep only alpha for the final analyses (445*11=4895)
drop if newid>4895
*Now we can generate summary statistics for the parameters of interest using below command
by Parameter: egen mean=mean( value )
by Parameter: egen sd=sd( value )
duplicates drop ( mean sd), force
save "${ROOT}\11_groups_stat_5000_8_13.dta"
clear
