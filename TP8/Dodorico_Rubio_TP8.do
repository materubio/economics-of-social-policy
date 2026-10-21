* ==============================================================================
* Dodorico_Rubio_TP8.do
*
* Purpose:
*   Replicates the Maimonides' rule discontinuity design of Angrist and Lavy
*   (1999) on the original data for fifth grade in Israel in 1991, and runs the
*   diagnostics that Angrist et al. (2019) apply to it: the manipulation test
*   on the enrollment density, the balance of predetermined covariates, the
*   donut estimates and the local estimates around the first cutoff.
*
* Inputs:
*   maimonides.dta                  One observation per fifth grade class,
*                                   2,019 classes in 1,002 schools, Israel 1991
*
* Outputs:
*   maimonides_schools.dta          One observation per school, built with
*                                   collapse from maimonides.dta
*   TP8_resultados.log              Full log of the results reported in the .tex
*   fig_maimonides_rule.png         Predicted and observed class size against
*                                   fifth grade enrollment
*   fig_histograma_matricula.png    Histogram of enrollment between 20 and 60
*                                   students, one bar per integer value
*
* Notes:
*   Run from inside the TP8 folder, all paths are relative.
*
*   Packages. rdrobust, rddensity and lpdensity are user-written and need to be
*   installed only once. The assignment asks to install them from the authors'
*   GitHub repositories, because the version of rddensity on SSC is
*   incomplete:
*
*       net install rdrobust, replace ///
*           from("https://raw.githubusercontent.com/rdpackages/rdrobust/main/stata")
*       net install rddensity, replace ///
*           from("https://raw.githubusercontent.com/rdpackages/rddensity/main/stata")
*       net install lpdensity, replace ///
*           from("https://raw.githubusercontent.com/nppackages/lpdensity/main/stata")
*
*   Those three lines do not run on this machine, for two different reasons, so
*   the packages were installed by hand. First, Stata 14 cannot validate
*   GitHub's certificate and net install fails with a PKIX error, so the files
*   were downloaded outside Stata and installed from a local directory. Second,
*   the current versions of the three packages require Stata 16, so we
*   installed the latest versions that still run under Stata 14, namely
*   rdrobust 10.0.0, rddensity 2.5 and lpdensity 2.5. Their compiled Mata files
*   were rebuilt from the .do files that ship with each package, because the
*   distributed .mo files are compiled with a later version of Stata and this
*   one cannot read them.
* ==============================================================================

* ------------------------------------------------------------------------------
* 1. Setup and paths
* ------------------------------------------------------------------------------

clear all
set more off

* All the work is done inside the TP8 folder
global data_cursos   "maimonides.dta"
global data_escuelas "maimonides_schools.dta"
global figura_regla  "fig_maimonides_rule.png"
global figura_hist   "fig_histograma_matricula.png"

* The first cutoff of the rule falls between 40 and 41 enrolled students
global corte = 40.5

* The log of a run with /e takes the name of the do file, so the results go
* to a log with a different name in order to keep both
log using "TP8_resultados.log", replace text

* ------------------------------------------------------------------------------
* 2. Data
* ------------------------------------------------------------------------------

use "$data_cursos", clear

describe
summarize

* ------------------------------------------------------------------------------
* 3. School-level data
* ------------------------------------------------------------------------------

* The rule operates on grade enrollment, which is a school variable, but the
* data come with one observation per class. Several questions need schools
* rather than classes, so the collapsed data are saved once and read again
* every time they are needed.
preserve

    * classize and avgverb are the two variables that vary across classes
    * within a school, so the average actually changes them. The other four
    * are school characteristics and are constant within the school.
    * n_cursos is not required by the assignment; we add it because the number
    * of classes the school actually opened is what tells whether it followed
    * the rule or not.
    collapse (mean) classize avgverb c_size tipuach pgirls relig             ///
             (count) n_cursos = classid, by(schlcode)

    label variable classize "Average class size in the school"
    label variable avgverb  "School average verbal score"
    label variable n_cursos "Fifth grade classes opened by the school"

    save "$data_escuelas", replace

restore

* ------------------------------------------------------------------------------
* 4. Question 1(a). The rule and observed class size
* ------------------------------------------------------------------------------

* Maimonides' rule splits the cohort into the smallest number of classes that
* keeps every class at forty students or fewer, and all classes in a school
* end up the same size. With enrollment e the predicted size is
*
*       pred_size = e / ( int((e-1)/40) + 1 )
*
* int() truncates toward zero, so int(39/40) = 0 and int(40/40) = 1: the number
* of classes goes up by one exactly at 41, 81, 121 and so on. It is func1 in
* Angrist and Lavy's replication code, and f_jt in the paper.

use "$data_escuelas", clear

generate pred_size = c_size / (int((c_size - 1) / 40) + 1)
label variable pred_size "Class size predicted by the rule"

summarize pred_size classize c_size

* How far observed size is from the rule, overall and by segment. Each segment
* runs between two cutoffs, which is where the rule predicts a different
* number of classes.
generate brecha = classize - pred_size
label variable brecha "Observed minus predicted"

generate cursos_predichos = int((c_size - 1) / 40) + 1
label variable cursos_predichos "Number of classes predicted by the rule"

summarize brecha
correlate classize pred_size

table cursos_predichos, contents(n brecha mean brecha sd brecha mean classize mean pred_size)

* The rule holds exactly when the school opens the number of classes the rule
* predicts and splits the cohort into equal parts, which is what makes observed
* size equal to predicted size
generate byte cumple_regla = (abs(brecha) < 0.01)
tabulate cursos_predichos cumple_regla, row

* Where the deviations come from: schools that open more classes than the rule
* requires fall below the sawtooth, those that open fewer fall above it
tabulate cursos_predichos n_cursos

* The size of the drop the rule produces at each cutoff, against the drop
* observed in the data. Five enrollment values on each side of the cutoff.
foreach k in 40 80 120 160 {
    display _newline "=== Cutoff at `k' ==="
    tabulate cursos_predichos if inrange(c_size, `k' - 4, `k')
    summarize classize pred_size if inrange(c_size, `k' - 4, `k')
    summarize classize pred_size if inrange(c_size, `k' + 1, `k' + 5)
}

* Figure. Predicted size is a deterministic function of enrollment, so it is
* plotted as is. Observed size is averaged across schools that share the same
* enrollment, which is the comparison Angrist and Lavy plot.
preserve

    collapse (mean) classize pred_size, by(c_size)

    twoway (line pred_size c_size, lcolor(navy) lwidth(medthick))                ///
           (line classize  c_size, lcolor(cranberry) lpattern(dash) lwidth(medthick)), ///
           xline(40.5 80.5 120.5 160.5 200.5, lcolor(gs10) lpattern(shortdash))  ///
           xlabel(0(20)220) ylabel(0(10)50)                                      ///
           xtitle("Fifth grade enrollment") ytitle("Students per class")         ///
           legend(order(1 "Class size predicted by the rule"                     ///
                        2 "Observed class size, average by enrollment")          ///
                  rows(2) position(6))                                           ///
           graphregion(color(white))
    graph export "$figura_regla", replace width(2000)

restore

* ------------------------------------------------------------------------------
* 5. Question 1(b). The first stage
* ------------------------------------------------------------------------------

* The first stage is at the class level, so standard errors are clustered by
* school: pred_size only varies across schools, so the 2,019 classes carry
* 1,002 independent observations rather than 2,019.
use "$data_cursos", clear

generate pred_size = c_size / (int((c_size - 1) / 40) + 1)
label variable pred_size "Class size predicted by the rule"

* Bivariate first stage, which is the one the assignment asks for. With
* literal compliance every class has exactly the predicted size and the slope
* is 1.
regress classize pred_size, vce(cluster schlcode)
test pred_size = 1

* The same regression with the controls of the equation in 1(c), to check that
* the instrument still moves class size once enrollment enters linearly. This
* is the first stage that the 2SLS actually uses.
regress classize pred_size tipuach c_size, vce(cluster schlcode)
test pred_size = 1

* ------------------------------------------------------------------------------
* 6. Question 1(c). The effect of class size on the verbal score
* ------------------------------------------------------------------------------

* The equation is Angrist and Lavy's, at the class level,
*
*       avgverb = mu + beta*tipuach + f(c_size) + alpha*classize + e
*
* with f() approximated by a linear term in enrollment. alpha is the parameter
* of interest and is estimated first by OLS and then by 2SLS with pred_size as
* the instrument for classize.

* We store the standard deviation of the score to express the effect of
* removing ten students from the class in standard deviations
summarize avgverb
scalar desvio_verb = r(sd)

* OLS, which compares classes of different sizes without accounting for the
* fact that class size responds to things that also move the score
regress avgverb classize tipuach c_size, vce(cluster schlcode)
scalar alpha_mco = _b[classize]

* 2SLS, which keeps only the variation in class size induced by the rule
ivregress 2sls avgverb tipuach c_size (classize = pred_size), vce(cluster schlcode) first
scalar alpha_iv = _b[classize]

* Effect of removing ten students from the class, in standard deviations of
* the score. The sign flips because alpha is the effect of one more student.
display _newline "=== Ten fewer students per class ==="
display "Standard deviation of avgverb: " desvio_verb
display "OLS:  " -10 * alpha_mco " points = " -10 * alpha_mco / desvio_verb " SDs"
display "2SLS: " -10 * alpha_iv  " points = " -10 * alpha_iv  / desvio_verb " SDs"

* ------------------------------------------------------------------------------
* 7. Question 2(a). The enrollment density around the first cutoff
* ------------------------------------------------------------------------------

* A school that wants to end up on a given side of the cutoff only has to move
* one student, because the rule changes between 40 and 41. If several do so,
* the enrollment distribution shows a gap on one side and an excess on the
* other. This uses the school-level data, one observation per school.
use "$data_escuelas", clear

* discrete asks for one bar per integer value instead of grouping into
* intervals, and freq puts the number of schools on the vertical axis
histogram c_size if inrange(c_size, 20, 60), discrete freq xline(40.5)         ///
    xlabel(20(5)60) xtitle("Fifth grade enrollment")                           ///
    ytitle("Schools") graphregion(color(white))
graph export "$figura_hist", replace width(2000)

* The values right next to the cutoff, which is where schools that sorted
* themselves would show up
tabulate c_size if inrange(c_size, 35, 46)

* Counts on each side of the cutoff, over the same number of integer values
count if inrange(c_size, 35, 40)
count if inrange(c_size, 41, 46)

* The same range away from the cutoff, where the rule changes nothing and the
* histogram should look like whatever the underlying distribution looks like
tabulate c_size if inrange(c_size, 20, 34)

* ------------------------------------------------------------------------------
* 8. Question 2(b). The same tabulation on the class-level data
* ------------------------------------------------------------------------------

* In the class-level data a school contributes as many rows as it has classes,
* so a school with 41 enrolled students contributes two rows and one with 40
* contributes one. The comparison with the previous table is the point of the
* question.
use "$data_cursos", clear

tabulate c_size if inrange(c_size, 35, 46)

count if inrange(c_size, 35, 40)
count if inrange(c_size, 41, 46)

* Average number of classes per school on each side of the cutoff, which is
* what generates the difference between the two tables
preserve
    collapse (count) n_cursos = classid, by(schlcode c_size)
    summarize n_cursos if inrange(c_size, 35, 40)
    summarize n_cursos if inrange(c_size, 41, 46)
restore

* ------------------------------------------------------------------------------
* 9. Question 2(c). The manipulation test
* ------------------------------------------------------------------------------

* rddensity reports two blocks. The top one is the McCrary test, which fits a
* local polynomial to the density on each side of the cutoff and compares the
* two fits at the threshold. The bottom one contains binomial tests, which take
* a symmetric window around the cutoff and count observations on each side.
*
* p(1) fits a line on each side, which is what the McCrary test in the Redux
* does. The command's default is p(2), a parabola.

* Test on the school-level data, which is the appropriate one
use "$data_escuelas", clear
rddensity c_size, c($corte) p(1)

* The same test on the class-level data, to see what the mechanical
* duplication of rows does to the diagnostic
use "$data_cursos", clear
rddensity c_size, c($corte) p(1)

* Placebo. At 60.5 the rule changes nothing, so a test that rejects there
* would signal that the test is picking up something other than manipulation
use "$data_escuelas", clear
rddensity c_size, c(60.5) p(1)

* ------------------------------------------------------------------------------
* 10. Question 2(d). The same test with the command's defaults
* ------------------------------------------------------------------------------

* Without p(1) the command fits a parabola on each side instead of a line.
* Everything else stays the same, so the comparison isolates what the order of
* the polynomial does to the conclusion.
use "$data_escuelas", clear
rddensity c_size, c($corte)

* ------------------------------------------------------------------------------
* 11. Question 3(a). Balance of the predetermined covariates
* ------------------------------------------------------------------------------

* The balance test runs the design in 1(c) with a predetermined characteristic
* as the dependent variable, expecting zero. It uses the school-level data,
* because both covariates are school characteristics and that is the unit the
* Redux uses for this test. Standard errors are robust, because with one
* observation per school there is nothing left to cluster.
use "$data_escuelas", clear

generate pred_size = c_size / (int((c_size - 1) / 40) + 1)
label variable pred_size "Class size predicted by the rule"

* Means of the two covariates over schools, which are not the means over classes
summarize tipuach pgirls

* Both covariates go through the same specification, the covariate on class
* size instrumented by the rule, with enrollment entering linearly. tipuach
* leaves the right-hand side because it is the dependent variable in the first.
ivregress 2sls tipuach c_size (classize = pred_size), vce(robust)
ivregress 2sls pgirls  c_size (classize = pred_size), vce(robust)

* The same test for pgirls keeping tipuach as a control, which is the literal
* reading of the equation in 1(c) changing only the dependent variable
ivregress 2sls pgirls tipuach c_size (classize = pred_size), vce(robust)

* ------------------------------------------------------------------------------
* 12. Question 3(c). Where the estimate in 1(c) comes from
* ------------------------------------------------------------------------------

* Dropping tipuach from the equation in 1(c) moves the coefficient a lot, even
* though 3(a) showed that tipuach does not jump at the cutoff. The point of the
* question is that the two facts refer to different sets of schools, so both
* are estimated to make the comparison explicit.
use "$data_cursos", clear

generate pred_size = c_size / (int((c_size - 1) / 40) + 1)

* Full sample, which is the one used in 1(c): enrollments from 8 to 226 students
ivregress 2sls avgverb tipuach c_size (classize = pred_size), vce(cluster schlcode)
ivregress 2sls avgverb         c_size (classize = pred_size), vce(cluster schlcode)

* The reason the control matters over the full range is that the PD index is
* strongly related to enrollment, and the instrument is a function of
* enrollment
correlate tipuach c_size pred_size

* The same two regressions on schools within five students of the first
* cutoff, which is the comparison the balance test speaks to
preserve
    keep if inrange(c_size, 36, 45)
    count
    egen tag_escuela = tag(schlcode)
    count if tag_escuela == 1
    ivregress 2sls avgverb tipuach c_size (classize = pred_size), vce(cluster schlcode)
    ivregress 2sls avgverb         c_size (classize = pred_size), vce(cluster schlcode)
restore

* ------------------------------------------------------------------------------
* 13. Question 4(a). The donut
* ------------------------------------------------------------------------------

* Schools that manipulated their enrollment sit right next to the cutoff, so
* dropping a window around the threshold should remove them. The estimate is
* the same equation as in 1(c) on what remains, extrapolating up to the cutoff.
use "$data_cursos", clear

generate pred_size = c_size / (int((c_size - 1) / 40) + 1)

* The first column repeats the full estimate, so that the three numbers are
* comparable
egen tag_escuela = tag(schlcode)
count if tag_escuela == 1
ivregress 2sls avgverb tipuach c_size (classize = pred_size), vce(cluster schlcode)

* Narrow donut, dropping schools with enrollment of 39, 40 or 41
preserve
    drop if inrange(c_size, 39, 41)
    count
    count if tag_escuela == 1
    ivregress 2sls avgverb tipuach c_size (classize = pred_size), vce(cluster schlcode)
restore

* Wider donut, dropping schools with enrollment between 38 and 42
preserve
    drop if inrange(c_size, 38, 42)
    count
    count if tag_escuela == 1
    ivregress 2sls avgverb tipuach c_size (classize = pred_size), vce(cluster schlcode)
restore

* ------------------------------------------------------------------------------
* 14. Question 5. Letting the command choose the bandwidth
* ------------------------------------------------------------------------------

* rdrobust fits a line on each side of the cutoff using the schools that fall
* within a bandwidth h, giving more weight to those close to the threshold, and
* returns the jump between the two lines. It chooses h by minimizing the mean
* squared error of that jump. The fuzzy() option indicates the treatment
* variable, so what it returns is the effect of class size and not that of
* crossing the cutoff.
*
* Everything uses the school-level data and no covariates, unlike the equation
* in 1(c), which controls for the PD index.
use "$data_escuelas", clear

* The running variable is centered at the cutoff, so c(0) is the first cutoff
generate x = c_size - 40.5
label variable x "Enrollment centered at the first cutoff"

* First run, with the bandwidth chosen by the command
rdrobust avgverb x, c(0) fuzzy(classize)
local h_elegido = e(h_l)
display _newline "Bandwidth chosen by the command: " `h_elegido'

* The fuzzy output already reports the first stage, so this run is only a
* cross-check. It gives the same point estimate with a different z, because
* when h is fixed by hand rdrobust uses b = h, and that inflates the
* bias-corrected variance.
rdrobust classize x, c(0) h(`h_elegido' `h_elegido')

* Second run, with the bandwidth fixed at 5, which is Angrist and Lavy's
* window. The only thing that sets it apart from the first run is the bandwidth.
rdrobust avgverb x, c(0) fuzzy(classize) h(5 5)

* And its first stage
rdrobust classize x, c(0) h(5 5)

log close
