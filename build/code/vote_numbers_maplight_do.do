global ROOT "G:\MAX-Filer\Collab\SOP-kbuzard-S13\Admin\Combined files"

****************************** COMPARING MAPLIGHT VOTES TO 
use "${ROOT}\maplight_bill_position.dta"
/*I want to get rid of the observations for other sessions right away--it only
eats up computation time to carry them around */
drop if legislative_session<112
drop if legislative_session>112

/*This code works for the 112th Congress, but will need an extra line to get rid of
""On the Conference Report - Senate" for whole sample */

drop if strpos( bill_number , "S")!=0
drop if strpos( motion , "Passed Senate")!=0
drop if strpos( motion , "On Passage of the Bill")!=0
drop if strpos( motion , "On the Conference Report H.R.")!=0
drop if strpos( motion , "Received in the Senate")!=0
drop if OS_catcode == ""
sort bill_number OS_catcode
by bill_number OS_catcode : drop if _n > 1

/*FIRST, I destring bill numbers to have an ordered and comparible bill number across two data sets */
sort bill_number
destring bill_number , ignore("H ", "HC ", "HJ ", "HR ") replace
save "${ROOT}\112_bill_positions_destring.dta"

/* Destring bill numbers in other */
clear
use "${ROOT}\hou112kh_merged.dta"
sort bill
destring bill, ignore("S CON RES ", "S ", "MOTION", "JOURNAL", "H RES ", "H R ", "H J RES ", "H CON RES ", "ADJOURN") replace
sort bill
rename bill bill_number
keep bill_number number session
sort bill_number
save "${ROOT}\hou112kh_merged_destring.dta", replace
clear
/* JOINBY two data sets. keeps the data set with higher number of bills and assigns the each catcode for those bill numbers. 
We assume that whenever an organization takes position on a bill, it takes position on all of the related actions as well. 
This assumption is somewhat problematic and leads to exagerated numbers of votes for each ineterst group.
As an example, If we look at "112_bill_positions_destring.dta" data, there are two actions on bill number 2.
There exists a large difference between the total number of interests groups that took positions on these actions. */

use "${ROOT}\112_bill_positions_destring.dta"
drop legislative_session bill_topic bill_description motion action_id vote_roll date position org_name sector industry business maplight_url
joinby using "${ROOT}\hou112kh_merged_destring.dta"
sort bill_number
duplicates drop (bill_number OS_catcode number session ), force
sort OS_catcode
save "${ROOT}\joined_112_bills.dta", replace
clear


******************************CALCULATING THE NUMBER OF VOTES FOR EACH CATCODE IN MAPLIGHT
clear
use "${ROOT}\112_bill_positions_destring.dta"
sort OS_catcode action_id
duplicates drop (
quietly by OS_catcode action_id:  gen dup = cond(_N==1,0,_n)
drop if dup>1
tab OS_catcode , matcell(x)
matrix list x
svmat x
drop if x1==.
export excel x1 using "E:\work order\vote_numbers_maplight.xlsx", sheet("sheet1") sheetmodify cell(C1) firstrow(variables)
/*Labeling cat-codes */

clear
use "${ROOT}\112_bill_positions_destring.dta"
duplicates drop (OS_catcode), force
keep OS_catcode business
sort OS_catcode
export excel OS_catcode using "E:\work order\vote_numbers_maplight.xlsx", sheet("sheet1") sheetmodify cell(A1) firstrow(variables)
export excel business using "E:\work order\vote_numbers_maplight.xlsx", sheet("sheet1") sheetmodify cell(B1) firstrow(variables)
clear
********Calculating the total number of votes for each interest group in COMBINED data*************
clear
use "${ROOT}\joined_112_bills.dta"
sort OS_catcode number
quietly by OS_catcode number:  gen dup = cond(_N==1,0,_n)
drop if dup>1
tab OS_catcode , matcell(x)
matrix list x
svmat x
drop if x1==.
export excel x1 using "${ROOT}\vote_numbers_maplight.xlsx", sheet("sheet2") sheetmodify cell(B1) firstrow(variables)

/*Labeling cat-codes */
clear
use "${ROOT}\joined_112_bills.dta"
duplicates drop (OS_catcode), force
keep OS_catcode 
sort OS_catcode
export excel OS_catcode using "${ROOT}\vote_numbers_maplight.xlsx", sheet("sheet2") sheetmodify cell(A1) firstrow(variables)
