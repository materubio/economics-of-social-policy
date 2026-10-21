*=====================================================================
* Problem Set 4: The reality show 16 and Pregnant and teen births
* Economics of Social Policy - UCEMA
* Tiziana Dodorico and Mateo Rubio Mazza
* Part B: the mechanism (tweets about birth control and abortion)
*=====================================================================

clear all
set more off

log using "TP4_resultados.log", replace text

use "tweets_16p.dta", clear

describe
summarize lntwtrate16p lntwtratebc lntwtrateabort totaltweets r_inseason pre16p trend trend2
count if r_inseason == 1
count if pre16p == 0

*---------------------------------------------------------------------
* Question 6: Kearney & Levine baseline specification,
* weighted by totaltweets, in-season days only, robust standard errors
*---------------------------------------------------------------------

display _newline "=== Q6: birth control (baseline) ==="
reg lntwtratebc lntwtrate16p trend trend2 if r_inseason == 1 [aw = totaltweets], vce(robust)
display "b = " %9.4f _b[lntwtrate16p] "  se = " %9.4f _se[lntwtrate16p] ///
        "  t = " %9.3f _b[lntwtrate16p]/_se[lntwtrate16p] ///
        "  p = " %9.4f 2*ttail(e(df_r), abs(_b[lntwtrate16p]/_se[lntwtrate16p])) ///
        "  N = " e(N)

display _newline "=== Q6: abortion (baseline) ==="
reg lntwtrateabort lntwtrate16p trend trend2 if r_inseason == 1 [aw = totaltweets], vce(robust)
display "b = " %9.4f _b[lntwtrate16p] "  se = " %9.4f _se[lntwtrate16p] ///
        "  t = " %9.3f _b[lntwtrate16p]/_se[lntwtrate16p] ///
        "  p = " %9.4f 2*ttail(e(df_r), abs(_b[lntwtrate16p]/_se[lntwtrate16p])) ///
        "  N = " e(N)

*---------------------------------------------------------------------
* Question 7: robustness to the two modeling choices
*   (a) weighted, in season        (baseline)
*   (b) unweighted, in season
*   (c) weighted, all dates (pre16p == 0)
*   (d) unweighted, all dates
*---------------------------------------------------------------------

foreach y in lntwtratebc lntwtrateabort {

    display _newline "########## OUTCOME: `y' ##########"

    display _newline "--- (a) weighted, in season ---"
    reg `y' lntwtrate16p trend trend2 if r_inseason == 1 [aw = totaltweets], vce(robust)
    display "b = " %9.4f _b[lntwtrate16p] "  se = " %9.4f _se[lntwtrate16p] ///
            "  t = " %9.3f _b[lntwtrate16p]/_se[lntwtrate16p] ///
            "  p = " %9.4f 2*ttail(e(df_r), abs(_b[lntwtrate16p]/_se[lntwtrate16p])) ///
            "  N = " e(N)

    display _newline "--- (b) unweighted, in season ---"
    reg `y' lntwtrate16p trend trend2 if r_inseason == 1, vce(robust)
    display "b = " %9.4f _b[lntwtrate16p] "  se = " %9.4f _se[lntwtrate16p] ///
            "  t = " %9.3f _b[lntwtrate16p]/_se[lntwtrate16p] ///
            "  p = " %9.4f 2*ttail(e(df_r), abs(_b[lntwtrate16p]/_se[lntwtrate16p])) ///
            "  N = " e(N)

    display _newline "--- (c) weighted, all dates ---"
    reg `y' lntwtrate16p trend trend2 if pre16p == 0 [aw = totaltweets], vce(robust)
    display "b = " %9.4f _b[lntwtrate16p] "  se = " %9.4f _se[lntwtrate16p] ///
            "  t = " %9.3f _b[lntwtrate16p]/_se[lntwtrate16p] ///
            "  p = " %9.4f 2*ttail(e(df_r), abs(_b[lntwtrate16p]/_se[lntwtrate16p])) ///
            "  N = " e(N)

    display _newline "--- (d) unweighted, all dates ---"
    reg `y' lntwtrate16p trend trend2 if pre16p == 0, vce(robust)
    display "b = " %9.4f _b[lntwtrate16p] "  se = " %9.4f _se[lntwtrate16p] ///
            "  t = " %9.3f _b[lntwtrate16p]/_se[lntwtrate16p] ///
            "  p = " %9.4f 2*ttail(e(df_r), abs(_b[lntwtrate16p]/_se[lntwtrate16p])) ///
            "  N = " e(N)
}

log close
