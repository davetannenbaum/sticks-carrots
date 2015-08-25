** =============================================================================
** This file: study1b.do
** Format: Stata 12.1 do-file
** Author: David Tannenbaum <davetannenbaum@gmail.com>
** Purpose: Analyses of findings in Study 1b of Tannenbaum et al., 2013 
** "Worksite Wellness Programs: Stick (Not Carrots) Send Stigmatizing Signals"
** =============================================================================

** IMPORTANT: change the working directory to wherever you have placed the files
** =============================================================================
version 12.1
cd "~/GitHub/stick-carrot/study1b"
import delimited study1b.csv, clear

** Dropping one subject who was below age 18
** =============================================================================
drop if age < 18

** Dropping subjects who fail any of the comprehension check items
** =============================================================================
gen cc_pass = 1
replace cc_pass = 0 if cc1 != 3
replace cc_pass = 0 if cc2 != 2 & cond1==1
replace cc_pass = 0 if cc2 != 1 & cond1==2
drop if cc1 != 3
drop if cc2 != 2 & cond1==1
drop if cc2 != 1 & cond1==2

** Sample characteristics
** =============================================================================
tab gender
sum age

** Manipulation check
** =============================================================================
// combined stick vs carrot
ttest manipcheck, by(cond1)

// pairwise comparisons of each condition
pwmean manipcheck, over(cond2) pveffects 

** Comparing General Informativeness across conditions
** =============================================================================
// test are conducted against midpoint of 0 (conducted two ways, using slightly different specifications)
bysort cond2: ttest leakiness = 0
regress leakiness i.cond2
margins cond2

** Information Absorption: Healthy-weight Employees
** =============================================================================
// combined stick vs carrot
ttest inference2, by(cond1)

// testing each condition against midpoint of 0 (conducted two ways, using slightly different specifications)
bysort cond2: ttest inference2 = 0
regress inference2 i.cond2
margins cond2

** Information Absorption: Overweight Employees
** =============================================================================
// combined stick vs carrot 
ttest inference1, by(cond1)

// testing each condition against midpoint of 0 (conducted two ways, using slightly different specifications)
bysort cond2: ttest inference1 = 0
regress inference1 i.cond2
margins cond2

** Information Absorption: Variance across conditions
** =============================================================================
// note: this analysis was removed from the final manuscript due to space constraints
table cond2, c(sd leakiness)
sdtest leakiness, by(cond1)
robvar leakiness, by(cond1)

** Participant BMI scores
** =============================================================================
gen bmi = (lbs/(height^2))*703.06957964
recode bmi (0/15 = 1 "very underweight") (15.01/18.4 = 2 "underweight") (18.5/24.99 = 3 "normal") (25/29.999 = 4 "overweight") (30/34.999 = 5 "obese") (35/60 = 6 "very obese"), gen(bmi2)
sum bmi, detail
replace bmi = . if bmi < 2 // omitting observations with suspicious BMI scores

** Creating Stigma and Job Dissatisfaction indices
** =============================================================================
alpha stigma*, item gen(stigma)
alpha dis1-dis8, item gen(diss)
replace diss = diss + 4

** Anticipated Feelings of Stigmatization by Employer
** =============================================================================  
regress stigma i.cond1##c.bmi, robust
margins cond1, dydx(bmi)
regress stigma i.cond2##c.bmi, robust
margins cond2, dydx(bmi)

** Expected Job Dissatisfaction
** =============================================================================  
regress diss i.cond1##c.bmi, robust
margins cond1, dydx(bmi)
regress diss i.cond2##c.bmi, robust
margins cond2, dydx(bmi)

** Do high and low BMI participants interpret the policy differently?
** =============================================================================  
regress leakiness i.cond1##c.bmi, robust
regress inference2 i.cond1##c.bmi, robust
regress inference1 i.cond1##c.bmi, robust

** Mediation Analysis
** =============================================================================  
// DV: Anticipated Feelings of Stigma
sureg (leakiness cond1) (stigma leakiness cond1)
capture program drop bootm
program bootm, rclass
  sureg (leakiness cond1)(stigma leakiness cond1)
  return scalar indirect = [leakiness]_b[cond1]*[stigma]_b[leakiness]
  return scalar direct = [leakiness]_b[cond1]
end
bootstrap r(indirect) r(direct), reps(5000) nodots: bootm
estat boot, bc percentile

// DV: Expected Job Dissatisfaction
sureg (leakiness cond1) (diss leakiness cond1)
capture program drop bootm
program bootm, rclass
  sureg (leakiness cond1) (diss leakiness cond1)
  return scalar indirect = [leakiness]_b[cond1]*[diss]_b[leakiness]
  return scalar direct = [diss]_b[cond1]
end
bootstrap r(indirect) r(direct), reps(5000) nodots: bootm
estat boot, bc percentile

** Moderated Mediation Analysis
** =============================================================================  
rename leakiness leak
summarize bmi
global m = r(mean)
global s = r(sd)
gen mw = leak * bmi // mediator by moderator interaction

// DV: Anticipated Feelings of Stigma
capture program drop bootm3
program bootm3, rclass
  sem (cond1 -> leak) (leak bmi mw -> stigma) (cond1 -> stigma), vce(robust)
  return scalar cielow = [leak]_b[cond1]*([stigma]_b[leak]+($m-$s)*[stigma]_b[mw])
  return scalar ciemean = [leak]_b[cond1]*([stigma]_b[leak]+($m)*[stigma]_b[mw])
  return scalar ciehigh = [leak]_b[cond1]*([stigma]_b[leak]+($m+$s)*[stigma]_b[mw])
end
bootstrap r(cielow) r(ciemean) r(ciehigh), reps(5000) nodots: bootm3
estat boot, bc percentile

// DV: Expected Job Dissatisfaction
capture program drop bootm3
program bootm3, rclass
  sem (cond1 -> leak) (leak bmi mw -> diss) (cond1 -> diss), vce(robust) 
  return scalar cielow = [leak]_b[cond1]*([diss]_b[leak]+($m-$s)*[diss]_b[mw])
  return scalar ciemean = [leak]_b[cond1]*([diss]_b[leak]+($m)*[diss]_b[mw])
  return scalar ciehigh = [leak]_b[cond1]*([diss]_b[leak]+($m+$s)*[diss]_b[mw])
end
bootstrap r(cielow) r(ciemean) r(ciehigh), reps(5000) nodots: bootm3
estat boot, bc percentile

** Table S2: Orthogonal Contrasts
** =============================================================================
foreach var of varlist stigma diss leakiness inference2 inference1 {
	di "Orthogonal Contrast: `var'"
	anova `var' cond2
	contrast {cond2 3 -1 -1 -1} {cond2 1 1 -1 -1} {cond2 -1 -1 3 -1} {cond2 -2 -2 3 1}, effects
	matrix `var' = r(F)
	di "Carrot vs Stick: r = " sqrt(`var'[1,1]/(e(df_r)+e(F)*e(df_m)))
	di "Costs to Overweight Employees: r = " sqrt(`var'[1,2]/(e(df_r)+e(F)*e(df_m)))
	di "Costs to Healthy-weight Employees: r = " sqrt(`var'[1,3]/(e(df_r)+e(F)*e(df_m)))
	di "Costs to Employers: r = " sqrt(`var'[1,4]/(e(df_r)+e(F)*e(df_m)))
}

** Table S2: Pairwise Comparisons
** Note: this analysis requires the add-on package 'cohend'
** =============================================================================
foreach var of varlist stigma diss leakiness inference2 inference1 {
	pwmean `var', over(cond2) effects
	cohend `var' cond2 if cond2 == 1 | cond2 == 2 // Carrot vs Stick
	cohend `var' cond2 if cond2 == 1 | cond2 == 3 // Carrot vs Low-baseline Stick
	cohend `var' cond2 if cond2 == 1 | cond2 == 4 // Carrot vs Low-premium Stick
	cohend `var' cond2 if cond2 == 2 | cond2 == 3 // Stick vs Low-baseline Stick
	cohend `var' cond2 if cond2 == 2 | cond2 == 4 // Stick vs Low-premium Stick
	cohend `var' cond2 if cond2 == 3 | cond2 == 4 // Low-baseline Stick vs Low-premium Stick
}
