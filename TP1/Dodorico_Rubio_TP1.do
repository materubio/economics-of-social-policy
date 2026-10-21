*==============================================================================*
* PS 1  -  Balboni et al. (2022) "Why Do People Stay Poor?"
*==============================================================================*

clear all
set more off

use "PovertyTraps_analysis.dta", clear

*==============================================================================*
* QUESTION 2. Balance
*==============================================================================*

*------------------------------------------------------------------------------*
* We keep the sample required throughout Question 2:
* the BASELINE (survey_wave==1) of the ELIGIBLE ULTRA-POOR (stup==1).
*------------------------------------------------------------------------------*
keep if survey_wave==1 & stup==1

* We check how many households remain and from how many branches.
count
codebook branchid, compact


*==============================================================================*
* QUESTION 2 (a)
* Compare the productive assets of treated vs. control households using the
* asset variable AS IT APPEARS IN THE BASELINE SURVEY (k1, post-transfer).
*==============================================================================*

ttest k1, by(treat)

*==============================================================================*
* QUESTION 2 (b)
* The baseline survey was fielded AFTER the transfer was delivered, so k1
* already includes the cow. The variable that reflects assets PRIOR to the
* transfer is k0. We redo the balance test with k0.
*==============================================================================*
ttest k0, by(treat)

*--- Show that the difference in (a) is the first stage (the asset value) --*
* The coefficient on treat is exactly
* the difference in means between treated and control households, and the
* constant is the control group mean. Hence, the difference in means in "reg k1
* treat" reflects the jump in assets caused by the transfer.
reg k1 treat

* We compare that jump with the value of the transfer. Pcows_no is the median
* price of a cow in the branch. The coefficient on treat should be a number
* close to the median of Pcows_no, since the difference in (a) is the
* asset.

summarize Pcows_no, detail


*==============================================================================*
* QUESTION 2 (c)
* Check balance on other baseline covariates, clustering the standard errors
* by branchid.
*
* Covariates:
*   resp_educ_years  = education
*   resp_literate    = literacy
*   bmi              = BMI
*   hhsize_adult_eq  = household size
*   pce_total        = consumption
*==============================================================================*

* We run the regressions for the 5 covariates
foreach v in resp_educ_years resp_literate bmi hhsize_adult_eq pce_total {
    display _newline "==================== Covariate: `v' ===================="
    reg `v' treat, vce(cluster branchid)
}

*==============================================================================*
* QUESTION 3.  The effect of the shock on consumption
*==============================================================================*
use "PovertyTraps_analysis.dta", clear

*------------------------------------------------------------------------------*
* QUESTION 3 (a)
* Effect of the program on household consumption (pce_total) in 2011
* (survey_wave==3), comparing treated vs. control among the eligible ultra-poor
* (stup==1), with standard errors clustered by branch.
*------------------------------------------------------------------------------*
reg pce_total treat if survey_wave==3 & stup==1, vce(cluster branchid)

scalar efecto = _b[treat]
summarize pce_total if survey_wave==3 & stup==1 & treat==0
scalar cmean  = r(mean)
display "Effect in levels (BDT)    = " efecto
display "Control mean              = " cmean
display "Effect as % of control    = " 100*efecto/cmean

*------------------------------------------------------------------------------*
* QUESTION 3 (b)
* Before trusting that simple difference, we check that consumption was
* balanced at baseline
*------------------------------------------------------------------------------*
reg pce_total treat if survey_wave==1 & stup==1, vce(cluster branchid)


*==============================================================================*
* QUESTION 4.  Persistence and conceptual wrap-up
*==============================================================================*

*------------------------------------------------------------------------------*
* QUESTION 4 (a)
* Does the effect persist while the control group remains untreated? We compare
* the effect on consumption in 2009 (survey_wave==2) and 2011 (survey_wave==3),
* the two waves in which the control group had not yet received the program.
*------------------------------------------------------------------------------*
foreach w in 2 3 {
    display _newline "===== Consumption, wave survey_wave==`w' ====="
    reg pce_total treat if survey_wave==`w' & stup==1, vce(cluster branchid)
    scalar efecto = _b[treat]
    summarize pce_total if survey_wave==`w' & stup==1 & treat==0
    scalar cmean  = r(mean)
    display "Effect (BDT) = " efecto "     % of control = " 100*efecto/cmean
}
