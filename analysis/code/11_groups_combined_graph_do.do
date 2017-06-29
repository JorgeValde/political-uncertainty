global ROOT "\\stu05-fsrv.ad.syr.edu\ykbagir$\Downloads"

***COMBINED GRAPH
use "${ROOT}\11_groups_5000_stat.dta"
twoway (scatter sd mean in 2226/2670 if sd<=2.5, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(Chemicals) name(chemicals,replace)
twoway (scatter sd mean in 1781/2225 if sd<=2.5, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(Manufacturing Unions) name(manufacturing,replace)
twoway (scatter sd mean in 4451/4890 if sd<=2.5, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(Commercial Banks) name(com_bank,replace)
graph combine chemicals manufacturing com_bank, rows(1) 
graph export "${ROOT}\11_groups_combined_graph_ch1.pdf", as(pdf) replace
clear
