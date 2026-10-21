*=====================================================================
* Problem Set 5: Alcohol, violence and mortality. South Africa's 2020 ban
* Economics of Social Policy - UCEMA
* Tiziana Dodorico and Mateo Rubio Mazza
* Part B: replication, June placebo, sex and control window
*=====================================================================

clear all
set more off

log using "TP5_resultados.log", replace text

use "barron_mortality.dta", clear

* lambda vector from column 1c (calendar fixed effects + weekend controls)
global lambda i.dayofweek_new i.dayofmonth i.year Days_Sat_Sun FirstWeekendMonth ///
              LastWeekendMonth Days_Sat_Sun2020 FirstWkndMonth2020 LastWkndMonth2020

* weekend controls from column 1b (without year fixed effects)
global finde Days_Sat_Sun FirstWeekendMonth LastWeekendMonth ///
             Days_Sat_Sun2020 FirstWkndMonth2020 LastWkndMonth2020

* summary of the two coefficients of interest; the argument is the alpha3 regressor
capture program drop coefs
program define coefs
    args a3
    display "beta   = " %8.3f _b[July_AlcBan_Period_2020] "  se = " %7.3f _se[July_AlcBan_Period_2020] ///
            "  p = " %6.3f 2*ttail(e(df_r), abs(_b[July_AlcBan_Period_2020]/_se[July_AlcBan_Period_2020]))
    display "alpha3 = " %8.3f _b[`a3'] "  se = " %7.3f _se[`a3'] ///
            "  p = " %6.3f 2*ttail(e(df_r), abs(_b[`a3']/_se[`a3']))
end

*---------------------------------------------------------------------
* Question 3: effect of the ban on daily mortality
*---------------------------------------------------------------------

display _newline "=== Q3(a): column 1a, no calendar controls ==="
reg Mort_Daily_Pooled Level_3_Period July_AlcBan_Period Level_3_Period_2020 ///
    July_AlcBan_Period_2020, vce(robust)
coefs Level_3_Period_2020

display _newline "=== Q3(a): column 1c, full lambda vector ==="
reg Mort_Daily_Pooled Level_3_Period July_AlcBan_Period Level_3_Period_2020 ///
    July_AlcBan_Period_2020 $lambda, vce(robust)
coefs Level_3_Period_2020
scalar beta_pool = _b[July_AlcBan_Period_2020]

display _newline "=== Q3(b): counterfactual, ban period in 2017-2019 ==="
sum Mort_Daily_Pooled if July_AlcBan_Period == 1 & year < 2020
display "beta as % of the counterfactual = " %6.1f 100*beta_pool/r(mean)

*---------------------------------------------------------------------
* Question 4: the June placebo
*   (a) alpha3 from columns 1a and 1c, already reported in Question 3
*---------------------------------------------------------------------

display _newline "=== Q4(b): column 1a plus the 2020 dummy ==="
reg Mort_Daily_Pooled Level_3_Period July_AlcBan_Period Level_3_Period_2020 ///
    July_AlcBan_Period_2020 year_2020, vce(robust)
coefs Level_3_Period_2020
display "2020 dummy = " %8.3f _b[year_2020] ///
        "   alpha3 + 2020 dummy = " %8.3f _b[Level_3_Period_2020] + _b[year_2020]

display _newline "=== Q4(b): 2020 level gap outside the level 3 period ==="
sum Mort_Daily_Pooled if Level_3_Period == 0 & year < 2020
scalar media_fuera = r(mean)
sum Mort_Daily_Pooled if Level_3_Period == 0 & year == 2020
display "gap 2020 vs 2017-2019 = " %8.3f r(mean) - media_fuera

display _newline "=== Q4(c): column 1b, weekend controls without year effects ==="
reg Mort_Daily_Pooled Level_3_Period July_AlcBan_Period Level_3_Period_2020 ///
    July_AlcBan_Period_2020 $finde, vce(robust)
coefs Level_3_Period_2020

* how much of the 2020 level gap is absorbed by the three 2020-specific controls
gen ajuste2020 = _b[Days_Sat_Sun2020]*Days_Sat_Sun2020 + ///
                 _b[FirstWkndMonth2020]*FirstWkndMonth2020 + ///
                 _b[LastWkndMonth2020]*LastWkndMonth2020
sum Days_Sat_Sun FirstWeekendMonth LastWeekendMonth ajuste2020 if year == 2020 & Level_3_Period == 0

*---------------------------------------------------------------------
* Question 5: column 1c by sex
*---------------------------------------------------------------------

foreach v in Mort_men Mort_women {
    display _newline "=== Q5: `v', column 1c ==="
    sum `v' if July_AlcBan_Period == 1 & year < 2020
    scalar media_sexo = r(mean)
    reg `v' Level_3_Period July_AlcBan_Period Level_3_Period_2020 ///
        July_AlcBan_Period_2020 $lambda, vce(robust)
    coefs Level_3_Period_2020
    display "counterfactual = " %7.2f media_sexo ///
            "   beta as % of the counterfactual = " %6.1f 100*_b[July_AlcBan_Period_2020]/media_sexo
}

*---------------------------------------------------------------------
* Question 6: the control window
*---------------------------------------------------------------------

display _newline "=== Q6(a): column 1c without the first week of June in the control ==="
reg Mort_Daily_Pooled Level_3_Prd_Excl_FirstWeek July_AlcBan_Period ///
    Level_3_Prd_Excl_FirstWeek_2020 July_AlcBan_Period_2020 $lambda, vce(robust)
coefs Level_3_Prd_Excl_FirstWeek_2020

display _newline "=== Q6(b): the first week of June, in levels and in deviations ==="
gen primera_sem_junio = Level_3_Period - Level_3_Prd_Excl_FirstWeek
egen nivel_anual = mean(cond(Level_3_Period == 0, Mort_Daily_Pooled, .)), by(year)
gen desvio = Mort_Daily_Pooled - nivel_anual
tabstat Mort_Daily_Pooled desvio if primera_sem_junio == 1, by(year) stat(mean n)
tabstat nivel_anual, by(year) stat(mean)

*---------------------------------------------------------------------
* Question 7: the rebound when the ban is lifted, with the curfew still in place
*---------------------------------------------------------------------

display _newline "=== Q7(c): ban period and the two following weeks, by year ==="
gen post_ban = (month == 8 & dayofmonth >= 18)
tabstat Mort_Daily_Pooled if July_AlcBan_Period == 1, by(year) stat(mean n)
tabstat Mort_Daily_Pooled if post_ban == 1, by(year) stat(mean n)

log close
