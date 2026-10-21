# Economics of Social Policy: problem sets

Stata code for the problem sets of Economics of Social Policy (UCEMA, Professor Eugenio Giolito), written by Tiziana Dodorico and Mateo Rubio Mazza. Each folder holds one problem set ("TP", for *trabajo práctico*) with its do file and the data it uses. The comments and output were translated into English for this repository; the code is the one we submitted.

| Folder | Topic | Data |
|---|---|---|
| `TP1` | Poverty traps: balance and treatment effects in Balboni et al. (2022) | `PovertyTraps_analysis.dta` |
| `TP2` | The asset threshold in Balboni et al. (2022), following Karlan, Raswan and Udry (2026) | Uses the TP1 data |
| `TP3` | The effect of the number of children with instrumental variables (twins and sex composition), Colombia 2005 Census | `colombia_clean.zip` |
| `TP4` | *16 and Pregnant* and teen births: the Twitter mechanism in Kearney and Levine (2015) | `tweets_16p.dta` |
| `TP5` | South Africa's 2020 alcohol ban and mortality, following Barron et al. | `barron_mortality.dta` |
| `TP6` | Unilateral divorce and female suicide with staggered difference in differences (Stevenson and Wolfers, Goodman-Bacon, Callaway and Sant'Anna) | `sw_divorcio.dta` |
| `TP8` | Maimonides' rule: regression discontinuity, manipulation test, balance and donut estimates (Angrist and Lavy, 1999; Angrist et al., 2019) | `maimonides.dta` |

## How to run

Each do file uses relative paths and is meant to be run from inside its own folder. The code was written for Stata 14.

- `TP2` reads the data from `../TP1/PovertyTraps_analysis.dta`, so both folders need to be kept side by side.
- `TP3`: the data file is larger than GitHub's 100 MB limit, so it is stored compressed. Unzip `colombia_clean.zip` in the same folder before running the do file.
- `TP6` uses the user-written packages `bacondecomp`, `csdid` and `drdid`.
- `TP8` uses `rdrobust`, `rddensity` and `lpdensity`. The header of the do file explains which versions run under Stata 14. `maimonides_schools.dta` is created by the do file itself from `maimonides.dta`.
