* ------------------------------------------------------------------------------
* 1. Setup
* ------------------------------------------------------------------------------

clear all
set more off

global figure_event "event_study_cs.png"

log using "TP6_resultados.log", replace text

* ------------------------------------------------------------------------------
* 2. Data
* ------------------------------------------------------------------------------

use "sw_divorcio.dta", clear

* Mean female suicide rate
summarize asmrs

* ------------------------------------------------------------------------------
* 3. Question 4
* ------------------------------------------------------------------------------

* Regressions changing the variance estimator

reg asmrs post i.stfips i.year

reg asmrs post i.stfips i.year, vce(robust)

reg asmrs post i.stfips i.year, vce(cluster stfips)

* ------------------------------------------------------------------------------
* 4. Question 5
* ------------------------------------------------------------------------------

* Specification with bacondecomp and decomposition

bacondecomp asmrs post

quietly bacondecomp asmrs post, ddetail
matrix bacon_parts = e(sumdd)
local part_labels : rownames bacon_parts

foreach type in Early_v_Late Late_v_Early Always_v_timing Never_v_timing {
    scalar weight_`type' = 0
    scalar wbeta_`type'  = 0
}

forvalues i = 1/`=rowsof(bacon_parts)' {
    local this_type : word `i' of `part_labels'
    scalar weight_`this_type' = weight_`this_type' + bacon_parts[`i', 2]
    scalar wbeta_`this_type'  = wbeta_`this_type'  + bacon_parts[`i', 1] * bacon_parts[`i', 2]
}

display _newline "=== Question 5. Decomposition ==="
display "Comparison type" _col(24) "Weight" _col(38) "Average DD"
scalar total_weight = 0
scalar total_beta   = 0
foreach type in Early_v_Late Late_v_Early Always_v_timing Never_v_timing {
    display "`type'" _col(24) %6.4f weight_`type' _col(38) %8.4f wbeta_`type'/weight_`type'
    scalar total_weight = total_weight + weight_`type'
    scalar total_beta   = total_beta   + wbeta_`type'
}
display "TOTAL" _col(24) %6.4f total_weight _col(38) %8.4f total_beta

* Weight of the forbidden comparison. I drop it and renormalize the weights to 1
scalar clean_weight = total_weight - weight_Late_v_Early
scalar clean_beta   = (total_beta - wbeta_Late_v_Early) / clean_weight
display _newline "Average after removing the forbidden comparison = " %8.4f clean_beta
display "The forbidden comparison biases the coefficient toward zero by " ///
        %6.4f total_beta - clean_beta " points"

* ------------------------------------------------------------------------------
* 5. Question 6
* ------------------------------------------------------------------------------
* Callaway - Sant'Anna ATT
csdid asmrs, ivar(stfips) time(year) gvar(gvar) method(dripw) notyet long2

* Simple
estat simple
matrix simple_att = r(table)

* Event study
estat event, window(-10 20)

* Figure
csdid_plot, title("Event study, Callaway and Sant'Anna") ///
            xtitle("Years since the reform") ///
            ytitle("Effect on the female suicide rate")
graph export "$figure_event", replace width(2000)

log close
