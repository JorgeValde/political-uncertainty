global ROOT "G:\MAX-Filer\Collab\SOP-kbuzard-S13\Admin\Combined files"

***COMBINED GRAPH
use "${ROOT}\7_groups_output_alpha_comparison_by_group_stata.dta"
twoway (scatter sd_entire_1 mean_entire_1, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(entire) name(entire,replace)
twoway (scatter sd_dairy_1 mean_dairy_1, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(dairy) name(dairy,replace)
twoway (scatter sd_manuf_1 mean_manuf_1, sort), ytitle(Uncertainty (Std. Dev.)) xscale(range(-5 5)) yscale(range(0 2.25)) ylabel(0.5 1 1.5 2 ) xtitle(Legislator's Ideal Point) title(manufacturing) name(manufacturing,replace)
graph combine entire dairy manufacturing , rows(1) 
graph export "${ROOT}\7_groups_combined_graph_ch1.pdf", as(pdf) replace
clear
