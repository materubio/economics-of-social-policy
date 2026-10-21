*==============================================================================*
* PS 3  -  The effect of the number of children (instrumental variables)
* Colombia 2005 Census (IPUMS)
*==============================================================================*

clear all
set more off

log using "TP3_resultados.log", replace text

use "colombia_clean.dta", clear

*==============================================================================*
* QUESTION 1. The sex composition instrument and the sample
*==============================================================================*

*------------------------------------------------------------------------------*
* Family identifier and original order of the data
*------------------------------------------------------------------------------*
gen long ord = _n                                   // household listing order
egen fam = group(sample serial famunit momloc)      // family id

*------------------------------------------------------------------------------*
* Diagnosis of the multiple first birth case
* If mb1==1, birth_order==1 identifies two (or three) children at once
*------------------------------------------------------------------------------*
bys fam: gen byte first = _n==1

count if first                                      // families in the data
count if first & nbirths>=2                         // families with 2+ births
count if first & nbirths>=2 & mb1==1                // families with a multiple first birth
tab n1child if first & nbirths>=2 & mb1==1

*------------------------------------------------------------------------------*
* QUESTION 1 (a)
* Sex of the first and second births, propagated to the whole family,
* and construction of samesex.
* Treatment of multiple first births: those families are excluded
*------------------------------------------------------------------------------*
gen byte boy = (sex==1)                             // 1 = boy

drop if mb1==1

* Within each family, we sort by birth and, within each birth, by listing
* order. Without multiple first births, row 1 is the first birth and row 2 is
* the first of the children from the second birth
bysort fam (birth_order ord): gen byte b1 = boy[1]  // boy at the first birth
bysort fam (birth_order ord): gen byte b2 = boy[2]  // boy at the second birth

gen byte samesex  = (b1==b2)            if !missing(b1,b2)
gen byte twoboys  = (b1==1 & b2==1)     if !missing(b1,b2)
gen byte twogirls = (b1==0 & b2==0)     if !missing(b1,b2)

label var samesex  "The first two children are of the same sex"
label var twoboys  "Two boys"
label var twogirls "Two girls"

*------------------------------------------------------------------------------*
* QUESTION 1 (c)
* 2+ sample: families with two or more births, oldest child
*------------------------------------------------------------------------------*
keep if nbirths>=2 & birth_order==1

count
duplicates report fam                               // one row per family

*------------------------------------------------------------------------------*
* Family-level frequencies of the two instruments
*------------------------------------------------------------------------------*
tab samesex
tab twoboys
tab twogirls
tab mb2

sum b1 b2 samesex twoboys twogirls mb2 n1child nbirths

* Independence between the two instruments
tab samesex mb2, row chi2

* Raw fertility difference by each instrument
tabstat n1child, by(samesex) stat(mean n)
tabstat n1child, by(mb2)     stat(mean n)

*------------------------------------------------------------------------------*
* Controls used throughout the problem set
*------------------------------------------------------------------------------*
global X i.age_mom i.age i.myears_education rural

*==============================================================================*
* QUESTION 2. OLS and the sign of the bias
*==============================================================================*

*------------------------------------------------------------------------------*
* QUESTION 2 (a)
* Years of education of the oldest child on the number of children
*------------------------------------------------------------------------------*
reg years_education n1child $X, vce(robust)
estimates store ols_educ
sum years_education if e(sample)

*------------------------------------------------------------------------------*
* QUESTION 2 (b)
* Omitted variable bias. The omitted variable we propose, household resources,
* is not observed in the census. We illustrate the sign of gamma*delta with the
* mother's education, which belongs to the same family of variables and is
* observed. We drop the mother's education from the set of controls so that it
* plays the role of the omitted variable, and we verify the identity
* short = long + gamma*delta
*------------------------------------------------------------------------------*
global X0 i.age_mom i.age rural

* Long regression (includes the omitted variable): gives the unbiased
* coefficient and gamma
reg years_education n1child myears_education $X0, vce(robust)
gen byte esamp = e(sample)
scalar blarga = _b[n1child]
scalar gamma  = _b[myears_education]

* Short regression (omits the variable)
reg years_education n1child $X0 if esamp, vce(robust)
scalar bcorta = _b[n1child]

* Auxiliary regression: the omitted variable on the regressor of interest (delta)
reg myears_education n1child $X0 if esamp, vce(robust)
scalar delta = _b[n1child]

di "short = "        %8.4f bcorta
di "long = "         %8.4f blarga
di "gamma = "        %8.4f gamma
di "delta = "        %8.4f delta
di "gamma*delta = "  %8.4f gamma*delta
di "long + g*d = "   %8.4f blarga + gamma*delta

*------------------------------------------------------------------------------*
* QUESTION 2 (c)
* The other three outcomes by OLS
*------------------------------------------------------------------------------*
foreach y in employed_mom divorce_mom going_schl {
    reg `y' n1child $X, vce(robust)
    estimates store ols_`y'
    sum `y' if e(sample)
}

*==============================================================================*
* QUESTION 3. First stage and 2SLS with twins
*==============================================================================*

*------------------------------------------------------------------------------*
* QUESTION 3 (a)
* First stage of the twins instrument
*------------------------------------------------------------------------------*
reg n1child mb2 $X, vce(robust)
test mb2
scalar F_mb2 = r(F)
di "First-stage F (twins) = " %8.2f F_mb2

* Later fertility: total births by the instrument
tabstat nbirths n1child, by(mb2) stat(mean n)

*------------------------------------------------------------------------------*
* QUESTION 3 (b)
* 2SLS of years of education with twins
*------------------------------------------------------------------------------*
ivregress 2sls years_education (n1child = mb2) $X, vce(robust)
estimates store iv_educ
estat firststage

* Reduced form, to read the 2SLS estimate as a ratio
reg years_education mb2 $X, vce(robust)

*------------------------------------------------------------------------------*
* QUESTION 3 (c)
* 2SLS with twins for the other three outcomes
*------------------------------------------------------------------------------*
foreach y in employed_mom divorce_mom going_schl {
    ivregress 2sls `y' (n1child = mb2) $X, vce(robust)
    estimates store iv_`y'
    estat firststage
    reg `y' mb2 $X, vce(robust)
}

* Summary table, OLS against 2SLS
estimates table ols_educ iv_educ ols_employed_mom iv_employed_mom, ///
    keep(n1child) b(%9.4f) se(%9.4f) stats(N)
estimates table ols_divorce_mom iv_divorce_mom ols_going_schl iv_going_schl, ///
    keep(n1child) b(%9.4f) se(%9.4f) stats(N)

* Standard deviations of the two quality outcomes, to compare magnitudes
sum years_education going_schl

*------------------------------------------------------------------------------*
* Size of the OLS - 2SLS gap, for the comparison in 3(c)(ii)
* Note: scalars cannot be named with an abbreviation of a variable in the
* data, because Stata resolves the name to the variable and not to the scalar
*------------------------------------------------------------------------------*
foreach v in educ employed_mom divorce_mom going_schl {
    estimates restore ols_`v'
    scalar bols = _b[n1child]
    estimates restore iv_`v'
    scalar biv  = _b[n1child]
    scalar seiv = _se[n1child]
    scalar gap  = bols - biv
    di "`v': gap = " %8.4f gap "  % of OLS = " %6.1f 100*gap/bols ///
       "  gap in 2SLS standard errors = " %6.2f gap/seiv
}

*==============================================================================*
* QUESTION 4. Two instruments, two LATEs
*==============================================================================*

*------------------------------------------------------------------------------*
* QUESTION 4 (a)
* First stage of the sex composition instrument
*------------------------------------------------------------------------------*
reg n1child samesex $X, vce(robust)
test samesex
scalar F_ss = r(F)
di "First-stage F (samesex) = " %8.2f F_ss "   vs. twins = " %8.2f F_mb2

* Decomposition into two boys and two girls
reg n1child twoboys twogirls $X, vce(robust)
test twoboys twogirls

*------------------------------------------------------------------------------*
* QUESTION 4 (b)
* 2SLS of years of education with samesex
*------------------------------------------------------------------------------*
ivregress 2sls years_education (n1child = samesex) $X, vce(robust)
estimates store iv_educ_ss
estat firststage

* Reduced form and first stage on the sample of the education regression,
* so that the Wald ratio matches exactly
reg years_education samesex $X, vce(robust)
reg n1child samesex $X if e(sample), vce(robust)
reg n1child mb2     $X if e(sample), vce(robust)

*------------------------------------------------------------------------------*
* QUESTION 4 (c)
* 2SLS with both instruments together and overidentification test
*------------------------------------------------------------------------------*
ivregress 2sls years_education (n1child = mb2 samesex) $X, vce(robust)
estimates store iv_educ_both
estat firststage
estat overid

* Summary table of the three 2SLS estimates
estimates table ols_educ iv_educ iv_educ_ss iv_educ_both, ///
    keep(n1child) b(%9.4f) se(%9.4f) stats(N)

*==============================================================================*
* QUESTION 5. Rural / urban heterogeneity
*==============================================================================*

* Controls without the rural dummy, which is collinear within each subsample
global XS i.age_mom i.age i.myears_education

*------------------------------------------------------------------------------*
* Descriptive statistics by area
*------------------------------------------------------------------------------*
tabstat years_education n1child mb2, by(rural) stat(mean n)
tab rural mb2

*------------------------------------------------------------------------------*
* QUESTION 5 (a)
* 2SLS with twins separately for urban and rural areas
*------------------------------------------------------------------------------*
foreach s in 0 1 {
    di _n "=================== rural==`s' ==================="
    reg n1child mb2 $XS if rural==`s', vce(robust)
    test mb2
    ivregress 2sls years_education (n1child = mb2) $XS if rural==`s', vce(robust)
    estimates store iv_educ_r`s'
    estat firststage
    reg years_education mb2 $XS if rural==`s', vce(robust)
}

estimates table iv_educ_r0 iv_educ_r1, keep(n1child) b(%9.4f) se(%9.4f) stats(N)

*------------------------------------------------------------------------------*
* QUESTION 5 (c)
* Are they the same parameter with noise, or different effects?
* The question is not answered by eyeballing the two coefficients, but by
* estimating the difference and looking at its standard error
*------------------------------------------------------------------------------*

* Simple version: the two subsamples are independent, so the variance of the
* difference is the sum of the variances
estimates restore iv_educ_r0
scalar burb = _b[n1child]
scalar seurb = _se[n1child]
estimates restore iv_educ_r1
scalar brur = _b[n1child]
scalar serur = _se[n1child]
scalar dif   = brur - burb
scalar sedif = sqrt(serur^2 + seurb^2)
di "difference rural - urban = " %8.4f dif "  se = " %8.4f sedif ///
   "  t = " %6.2f dif/sedif ///
   "  95% CI = [" %8.4f dif-1.96*sedif ", " %8.4f dif+1.96*sedif "]"

* Formal version: model with an interaction, instrumenting both n1child and its
* interaction with rural
gen n1child_r = n1child*rural
gen mb2_r     = mb2*rural

ivregress 2sls years_education (n1child n1child_r = mb2 mb2_r) $X, vce(robust)
test n1child_r
lincom n1child_r

log close
