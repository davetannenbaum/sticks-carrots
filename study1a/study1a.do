** =============================================================================
** This file: study1a.do
** Format: Stata 12.1 do-file
** Author: David Tannenbaum <davetannenbaum@gmail.com>
** Purpose: Analyses of findings in Study 1a of Tannenbaum et al., 2013
** "Worksite Wellness Programs: Stick (Not Carrots) Send Stigmatizing Signals"
** =============================================================================

** IMPORTANT: Need to set working directory to call on data files
** =============================================================================
version 12.1
cd "~/GitHub/stick-carrot/study1a"
import delimited study1a.csv, clear

** Dropping subjects who fail any comprehension check items
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

** Comparing Policy evaluations across conditions
** =============================================================================
// constructing index of policy support
pwcorr support fair, sig
alpha support fair, gen (eval)

// combined stick vs carrot
ttest eval, by(cond1)

// pairwise comparisons of each condition
pwmean eval, over(cond2) pveffects

** Comparing General Informativeness across conditions
** =============================================================================
// test are conducted against midpoint of 0 (conducted two ways, using slightly different specifications)
bysort cond2: ttest leakiness = 0
regress leakiness i.cond2
margins cond2

** Information Absorption: Healthy-weight Employees
** =============================================================================
// combined stick vs carrot
ttestinference2, by(cond1)

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


** Table 3: Orthogonal Contrasts
** =============================================================================
foreach var of varlist eval leakiness inference2 inference1 {
	display "Orthogonal Contrast: `var'"
	anova `var' cond2
	contrast {cond2 3 -1 -1 -1} {cond2 1 1 -1 -1} {cond2 -1 -1 3 -1} {cond2 -2 -2 3 1}, effects
	matrix `var' = r(F)
	display "Carrot vs Stick: r = " sqrt(`var'[1,1]/(e(df_r)+e(F)*e(df_m)))
	display "Costs to Overweight Employees: r = " sqrt(`var'[1,2]/(e(df_r)+e(F)*e(df_m)))
	display "Costs to Healthy-weight Employees: r = " sqrt(`var'[1,3]/(e(df_r)+e(F)*e(df_m)))
	display "Costs to Employers: r = " sqrt(`var'[1,4]/(e(df_r)+e(F)*e(df_m)))
}

** Table 3: Pairwise Comparisons
** Note: this analysis requires the add-on module 'cohend'
** =============================================================================
foreach var of varlist eval leakiness inference2 inference1 {
	pwmean `var', over(cond2) effects
	cohend `var' cond2 if cond2 == 1 | cond2 == 2 // Carrot vs Stick
	cohend `var' cond2 if cond2 == 1 | cond2 == 3 // Carrot vs Low-baseline Stick
	cohend `var' cond2 if cond2 == 1 | cond2 == 4 // Carrot vs Low-premium Stick
	cohend `var' cond2 if cond2 == 2 | cond2 == 3 // Stick vs Low-baseline Stick
	cohend `var' cond2 if cond2 == 2 | cond2 == 4 // Stick vs Low-premium Stick
	cohend `var' cond2 if cond2 == 3 | cond2 == 4 // Low-baseline Stick vs Low-premium Stick
}

** Mediation analysis
** =============================================================================
sureg (leakiness cond1) (eval leakiness cond1)
capture program drop bootm
program bootm, rclass
	sureg (leakiness cond1) (eval leakiness cond1)
	return scalar indirect = [leakiness]_b[cond1]*[eval]_b[leakiness]
	return scalar direct = [eval]_b[cond1]
end
bootstrap r(indirect) r(direct), reps(5000) nodots: bootm
estat boot, bc percentile

** Supplemental Mediation analysis
** =============================================================================
// Using inferences about overweight employees as the mediator
sureg (inference1 cond1) (eval inference1 cond1)
capture program drop bootm
program bootm, rclass
	sureg (inference1 cond1) (eval inference1 cond1)
	return scalar indirect = [inference1]_b[cond1]*[eval]_b[inference1]
	return scalar direct = [eval]_b[cond1]
end
bootstrap r(indirect) r(direct), reps(5000) nodots: bootm
estat boot, bc percentile

// Using inferences about healthy-weight employees as the mediator
sureg(inference2 cond1)(eval inference2 cond1)
capture program drop bootm
program bootm, rclass
	sureg (inference2 cond1) (eval inference2 cond1)
	return scalar indirect = [inference2]_b[cond1]*[eval]_b[inference2]
	return scalar direct = [eval]_b[cond1]
end
bootstrap r(indirect) r(direct), reps(5000) nodots: bootm
estat boot, bc percentile

// Using difference score between the two inference items as the mediator
gen diff = inference1 - inference2
sureg (diff cond1)(eval diff cond1)
capture program drop bootm
program bootm, rclass
	sureg (diff cond1) (eval diff cond1)
	return scalar indirect  = [diff]_b[cond1]*[eval]_b[diff]
	return scalar direct  = [eval]_b[cond1]
end
bootstrap r(indirect) r(direct), reps(5000) nodots: bootm
estat boot, bc percentile