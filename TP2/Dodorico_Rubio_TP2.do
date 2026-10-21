*==============================================================================*
* PS 2  -  Karlan, Raswan & Udry (2026) on Balboni et al. (2022)
*==============================================================================*

clear all
set more off

* The dataset lives in the TP1 folder (a single shared copy).
* Run this .do from the TP2 folder so that the relative path resolves.
use "../TP1/PovertyTraps_analysis.dta", clear

*------------------------------------------------------------------------------*
* Sample
*------------------------------------------------------------------------------*

* Asset threshold (2007 log assets)
gen aboveT = (Lk1 >= 2.333) if !missing(Lk1)

* "Expensive cow" branches, based on the median cow price
summarize Pcows_no, detail
gen highcow = (Pcows_no > r(p50)) if !missing(Pcows_no)

*==============================================================================*
* QUESTION 2. The threshold and asset growth
*==============================================================================*

*------------------------------------------------------------------------------*
* QUESTION 2 (a)
* We regress asset growth 2007->2011 (deltaLk3) on aboveT
* with robust standard errors
*------------------------------------------------------------------------------*
reg deltaLk3 aboveT if treat==1 & stup==1 & survey_wave==3 & Lk1<3, vce(robust)

*------------------------------------------------------------------------------*
* QUESTION 2 (c)
* We add highcow as a control to the previous regression
*------------------------------------------------------------------------------*
reg deltaLk3 aboveT highcow if treat==1 & stup==1 & survey_wave==3 & Lk1<3, vce(robust)

* Auxiliary regression for the omitted variable bias
reg highcow aboveT if treat==1 & stup==1 & survey_wave==3 & Lk1<3, vce(robust)

*==============================================================================*
* QUESTION 3. Work hours as a control
*==============================================================================*

*------------------------------------------------------------------------------*
* QUESTION 3 (a)
* Mean hours by wave, among the treated ultra-poor
*------------------------------------------------------------------------------*
tabstat wage_hr_tot selfemp_hr_tot husbandry_hours_tot ///
    if treat==1 & stup==1, by(survey_wave) statistics(mean) columns(variables)

*------------------------------------------------------------------------------*
* QUESTION 3 (b)
* We add livestock rearing and self-employment hours to the regression in 2(a)
*------------------------------------------------------------------------------*
reg deltaLk3 aboveT husbandry_hours_tot selfemp_hr_tot ///
    if treat==1 & stup==1 & survey_wave==3 & Lk1<3, vce(robust)

*==============================================================================*
* QUESTION 4. The transition function
*==============================================================================*

preserve
keep if treat==1 & stup==1 & survey_wave==3   // one household per row (Lk1, Lk3 fixed)
keep if !missing(Lk1, Lk3)

*------------------------------------------------------------------------------*
* QUESTION 4 (a)
* Transition function in logs with D=1000
*------------------------------------------------------------------------------*
twoway (lpolyci Lk3 Lk1 if inrange(Lk1,2,3.5), bw(0.062)) ///
       (function y=x, range(2 3.5) lpattern(dash)), ///
       aspect(1) legend(off) ///
       xscale(range(2 3.5)) yscale(range(2 3.5)) ///
       xlabel(2(0.5)3.5) ylabel(2(0.5)3.5) ///
       xtitle("log assets 2007") ytitle("log assets 2011")
graph export "fig4a_logs.png", replace width(1600)

*------------------------------------------------------------------------------*
* QUESTION 4 (b)
* Transition function in levels
*------------------------------------------------------------------------------*
gen klev1 = exp(Lk1) - 1
gen klev3 = exp(Lk3) - 1
_pctile klev1, p(99)
scalar c1 = r(r1)
_pctile klev3, p(99)
scalar c3 = r(r1)
twoway (lpolyci klev3 klev1 if klev1<=c1 & klev3<=c3) ///
       (function y=x, range(0 60) lpattern(dash)), ///
       aspect(1) legend(off) ///
       xtitle("assets 2007 (thousand BDT)") ytitle("assets 2011 (thousand BDT)")
graph export "fig4b_niveles.png", replace width(1600)

*------------------------------------------------------------------------------*
* QUESTION 4 (c)
* Transition function in logs with D=200 and D=1800
*------------------------------------------------------------------------------*
_pctile klev1, p(93)
scalar p93 = r(r1)
keep if klev1<=p93 & klev3<=c3

gen k1_bdt = klev1*1000
gen k3_bdt = klev3*1000

foreach D in 200 1000 1800 {
    gen tx`D' = ln(k1_bdt/`D' + 1)
    gen ty`D' = ln(k3_bdt/`D' + 1)
    lpoly ty`D' tx`D', nograph n(100) gen(gx`D' gy`D')
    gen xlev`D' = (exp(gx`D')-1)*`D'/1000     // back to levels (thousand BDT)
    gen ylev`D' = (exp(gy`D')-1)*`D'/1000
}

twoway (line ylev200  xlev200,  sort lwidth(medthick)) ///
       (line ylev1000 xlev1000, sort lwidth(medthick)) ///
       (line ylev1800 xlev1800, sort lwidth(medthick)) ///
       (function y=x, range(4 20) lpattern(dash)), ///
       aspect(1) xscale(log range(4 20)) yscale(log range(4 20)) ///
       xlabel(4 6 9.3 13 20) ylabel(4 6 9.3 13 20) ///
       xline(9.309, lpattern(dot) lcolor(gs9)) ///
       legend(order(1 "D=200" 2 "D=1,000" 3 "D=1,800" 4 "45 degrees") pos(6) rows(1)) ///
       xtitle("assets 2007 (thousand BDT)") ytitle("assets 2011 (thousand BDT)")
graph export "fig4c_D.png", replace width(1600)

restore
