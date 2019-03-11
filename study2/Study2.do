** =============================================================================
** This file: study2.do
** Format: Stata 12.1 do-file
** Author: David Tannenbaum <davetannenbaum@gmail.com>
** =============================================================================

** Calling data
** =============================================================================
version 12.1
import delimited "https://raw.githubusercontent.com/davetannenbaum/sticks-carrots/master/study2/study2.csv", clear

** Sample characteristics
** =============================================================================
tabulate sex
summarize age

** Manipulation check
** =============================================================================
ttest choiceq5 = choiceq6
bysort cond: ttest choiceq5 = choiceq6

** Testing the Rarity Assumption (Do `policymakers' choose sticks infrequently?)
** =============================================================================
tabulate choice, gen(choice)
prtest choice1 = .5
bysort cond: prtest choice1 = .5

** Predictors of Policy Decisions
** =============================================================================
// implicit anti-fat bias
logit choice1 implicit, robust
margins, dydx(implicit)
summarize implicit
local ilow = r(mean) - r(sd)
local ihigh = r(mean) + r(sd)
margins, at(implicit = (`ilow' `ihigh'))

// explicit anti-fat bias
logit choice1 explicit, robust
margins, dydx(explicit)
summarize explicit
local elow = r(mean) - r(sd)
local ehigh = r(mean) + r(sd)
margins, at(explicit = (`elow' `ehigh'))

// implicit and explicit anti-fat bias
logit choice1 implicit explicit, robust
margins, dydx(implicit explicit)

** Policy Rationales
** =============================================================================
ttest choiceq1, by(choice)
pwcorr implicit choiceq1 choiceq2, star(.05)
spearman implicit choiceq1 choiceq2, star(.05)
pwcorr explicit choiceq1 choiceq2, star(.05)
spearman explicit choiceq1 choiceq2, star(.05)

** Sensitivity to Costs
** =============================================================================
tab cond choice, row chi2
logit choice1 i.cond##c.implicit, nolog
margins cond, dydx(implicit)
margins cond, dydx(implicit) pwcompare(effects) mcompare(bonferroni)
margins cond, pwcompare(effects) mcompare(bonferroni)
quietly margins cond, at(implicit = (-1.5(.1)1.25))
marginsplot, recast(line) noci