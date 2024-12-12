cd "/Users/adrienfoutelet/Dropbox/Adrien/8_Brown/ECON2390 Fall24 Applied Econometrics I/ECON2390-PSET2/"


// 1. Regression Discontinuity Design

// 1.a.

use "data/Dell.dta", clear

reg  Y PANwin

est store model1

esttab model1 using "outputs/model1_table.tex", replace label booktabs ///
    title("Simple OLS regression model\label{tab:model1}") ///
    style(tex) nonumber

// 1.b.

// 1.c.

ssc install rddensity
ssc install lpdensity

rddensity margin_victory, c(0) all
scalar log_density_ratio = e(T_q)
scalar se = e(se_q)
scalar p_value = e(pv_q)
scalar bandwidth_left = e(h_l)
scalar bandwidth_right = e(h_r)
estadd scalar LogDensityRatio = log_density_ratio
estadd scalar StdError = se
estadd scalar PValue = p_value
estadd scalar Bandwidth_left = bandwidth_left	
estadd scalar Bandwidth_right = bandwidth_right	
esttab . using "outputs/mccrary_table.tex", replace stats(LogDensityRatio StdError PValue Bandwidth_left Bandwidth_right) title("McCrary density test\label{tab:McCrary}")  mtitles("margin_victory") style(tex) nonumber not nostar
rddensity margin_victory, c(0) plot graph_opt(legend(off))
graph export "outputs/mccrary_plot.pdf", replace

// 1.d.

ssc install rdrobust

rdplot Y margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(position(6)))
graph export "outputs/binned_scatter_esmv_linear_plot.pdf", replace

rdplot Y margin_victory, c(0) binselect(esmv) p(2) graph_options(title() legend(position(6)))
graph export "outputs/binned_scatter_esmv_quadratic_plot.pdf", replace

rdplot Y margin_victory, c(0) binselect(esmv) p(3) graph_options(title() legend(position(6)))
graph export "outputs/binned_scatter_esmv_cubic_plot.pdf", replace

rdplot Y margin_victory, c(0) binselect(es) p(2) graph_options(title() ylabel(0(50)400) legend(position(6)))
graph export "outputs/binned_scatter_es_cubic_plot.pdf", replace

rdplot Y margin_victory, c(0) binselect(qs) p(2) graph_options(title() ylabel(0(50)400) legend(position(6)))
graph export "outputs/binned_scatter_qs_cubic_plot.pdf", replace

rdplot Y margin_victory, c(0) nbins(10) p(2) graph_options(title() ylabel(0(50)400) legend(position(6)))
graph export "outputs/binned_scatter_10bins_cubic_plot.pdf", replace

rdplot Y margin_victory, c(0) nbins(20) p(2) graph_options(title() ylabel(0(50)400) legend(position(6)))
graph export "outputs/binned_scatter_20bins_cubic_plot.pdf", replace

// 1.e.

rdrobust Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store non_para_trian_lin

rdrobust Y margin_victory, c(0) p(2) kernel(triangular) bwselect(mserd)
est store non_para_trian_quad

rdbwselect Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
local optimal_bandwidth = e(h_mserd)
regress Y margin_victory PANwin c.margin_victory##PANwin ///
if abs(margin_victory) <= `optimal_bandwidth'
est store para_trian_lin

rdbwselect Y margin_victory, c(0) p(2) kernel(triangular) bwselect(mserd)
local optimal_bandwidth = e(h_mserd)
gen margin_victory2 = margin_victory^2
label variable margin_victory2 "(mean) spread squared"
regress Y margin_victory margin_victory2 PANwin c.margin_victory#PANwin c.margin_victory2#PANwin ///
if abs(margin_victory) <= `optimal_bandwidth'
est store para_trian_quad

esttab non_para_trian_lin non_para_trian_quad para_trian_lin para_trian_quad ///
using "outputs/model2_table.tex", replace ///
label booktabs ///
title("Non-parametric and parametric RDD models\label{tab:model2}") ///
mtitles("Non-para tri lin" "Non-para tri quad" "Para lin" "Para quad") ///
style(tex) nonumber noomitted nobaselevels 

est restore non_para_trian_lin
local bdth_l = e(b_l)
local bdth_r = e(b_r)
rdplot Y margin_victory, p(1) kernel(tri) masspoints(adjust) graph_options(title() legend(position(6))) h(`bdth_l' `bdth_r') 
graph export "outputs/binned_scatter_esmv_linear_plot_corrected.pdf", replace

// 1.f.

rdrobust Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store non_para_trian_lin

rdbwselect Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
local optimal_bandwidth = e(h_mserd)

preserve
drop if abs(margin_victory) <= (5/100)*`optimal_bandwidth'
rdrobust Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store non_para_trian_lin_5per
restore

preserve
drop if abs(margin_victory) <= (10/100)*`optimal_bandwidth'
rdrobust Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store non_para_trian_lin_10per
restore

preserve
drop if abs(margin_victory) <= (20/100)*`optimal_bandwidth'
rdrobust Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store non_para_trian_lin_20per
restore

esttab non_para_trian_lin non_para_trian_lin_5per non_para_trian_lin_10per non_para_trian_lin_20per ///
using "outputs/model2_robustness_table.tex", replace ///
label booktabs ///
title("RDD model robustness\label{tab:model2-robustness}") ///
mtitles("Para tri lin" "Para tri lin 5\%" "Para tri lin 10\%" "Para tri lin 20\%") ///
style(tex) nonumber noomitted nobaselevels 

rdbwselect Y margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
local optimal_bandwidth = e(h_mserd)
local i = 0.50
while `i'<=1.5 {
	display "`i'"
	rdrobust Y margin_victory, c(0) p(1) kernel(triangular) h(`i'*`optimal_bandwidth')
	local 100i = floor(100 * `i')
	display "`100i'"
	est store non_para_trian_lin_`100i'
	local cc `"`cc' (non_para_trian_lin_`100i', label(`100i'/100 * optimal bandwidth)) "'
	di `"`cc'"'
    coefplot `cc', yline(0) vertical  legend(off) mcolor(black) ciopts(lcol(dknavy)) ///
	xtitle(Optimal bandwidth multiplicator (from .5 to 1.5 in increments of .001)) ytitle(Estimate) ///
	xlabel(, nolabels)
	local i = `i' + 0.01
}
graph export "outputs/model2_robustness_plot.pdf", replace

// 1.g.

// 1.h.

merge 1:1 id_municipio using "data/Candidates.dta"

ssc install rdrobust

rdplot prior_experience          margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(prior_experience))
graph save temp/characteristic1, replace
rdplot criminal_record           margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(criminal_record))
graph save temp/characteristic2, replace
rdplot business_background       margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(business_background))
graph save temp/characteristic3, replace
rdplot social_leader             margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(social_leader))
graph save temp/characteristic4, replace
rdplot years_education           margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(years_education))
graph save temp/characteristic5, replace
rdplot age_candidate             margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(age_candidate))
graph save temp/characteristic6, replace
rdplot campaign_budget           margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(campaign_budget))
graph save temp/characteristic7, replace
rdplot years_in_local_government margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(years_in_local_government))
graph save temp/characteristic8, replace

graph combine temp/characteristic1.gph temp/characteristic2.gph temp/characteristic3.gph temp/characteristic4.gph temp/characteristic5.gph temp/characteristic6.gph temp/characteristic7.gph temp/characteristic8.gph, col(2) ysize(20) xsize(15)
graph export "outputs/characteristics_binned_scatter_esmv_linear_plot.pdf", replace

foreach var in prior_experience criminal_record business_background social_leader years_education age_candidate campaign_budget years_in_local_government {
    summarize `var'
    gen `var'_z = (`var' - r(mean)) / r(sd)
}

pca prior_experience_z criminal_record_z business_background_z social_leader_z years_education_z age_candidate_z campaign_budget_z years_in_local_government_z

predict characteristics_pca_index, score

rdplot characteristics_pca_index          margin_victory, c(0) binselect(esmv) p(1) graph_options(title() legend(off) xtitle(margin_victory) ytitle(characteristics_pca_index))
graph export "outputs/characteristics_pca_index_binned_scatter_esmv_linear_plot.pdf", replace

rdrobust characteristics_pca_index margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store pca_index
rdrobust prior_experience          margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store prior_experience
scalar betaD_prior_experience = e(tau_cl)
rdrobust criminal_record           margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store criminal_record
scalar betaD_criminal_record = e(tau_cl)
rdrobust business_background       margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store business_background
scalar betaD_business_background = e(tau_cl)
rdrobust social_leader             margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store social_leader
scalar betaD_social_leader = e(tau_cl)
rdrobust years_education           margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store years_education
scalar betaD_years_education = e(tau_cl)
rdrobust age_candidate             margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store age_candidate
scalar betaD_age_candidate = e(tau_cl)
rdrobust campaign_budget           margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store campaign_budget
scalar betaD_campaign_budget = e(tau_cl)
rdrobust years_in_local_government margin_victory, c(0) p(1) kernel(triangular) bwselect(mserd)
est store years_in_local_government
scalar betaD_years_in_local_government = e(tau_cl)

estwide pca_index prior_experience criminal_record business_background social_leader years_education age_candidate campaign_budget years_in_local_government ///
using "outputs/characteristics_table.tex", replace ///
label booktabs ///
title("Candidate characteristic RDD analysis\label{tab:characteristics}") ///
mtitles("pca_index" "prior_experience" "criminal_record" "business_background" "social_leader" "years_education" "age_candidate" "campaign_budget" "years_in_local_government") ///
style(tex) nonumber noomitted nobaselevels 

save "data/temp/intermediary_0.dta", replace

// 1.i.

use "data/temp/intermediary_0.dta", clear

foreach var in prior_experience criminal_record business_background social_leader years_education age_candidate campaign_budget years_in_local_government {
    regress Y `var'
    scalar beta_`var' = _b[`var']
}

foreach var in prior_experience criminal_record business_background social_leader years_education age_candidate campaign_budget years_in_local_government {
	rdrobust `var' margin_victory, c(0) p(1) kernel(triangular)
	scalar delta_`var' = e(tau_cl)
}

rdrobust Y margin_victory, c(0) p(1) kernel(triangular)
gen tau_rdd = e(tau_cl)

save "data/temp/intermediary_00.dta", replace

clear
set obs 1
gen wgt_prior_experience = .
gen wgt_criminal_record = .
gen wgt_business_background = .
gen wgt_social_leader = .
gen wgt_years_education = .
gen wgt_age_candidate = .
gen wgt_campaign_budget = .
gen wgt_years_in_local_government = .
foreach p1 in 0.5 1 1.5 {
foreach p2 in 0.5 1 1.5 {
foreach p3 in 0.5 1 1.5 {
foreach p4 in 0.5 1 1.5 {
foreach p5 in 0.5 1 1.5 {
foreach p6 in 0.5 1 1.5 {
foreach p7 in 0.5 1 1.5 {
foreach p8 in 0.5 1 1.5 {
expand 2 if _n == _N 
replace wgt_prior_experience = beta_prior_experience * `p1' if _n == _N
replace wgt_criminal_record = beta_criminal_record * `p2' if _n == _N
replace wgt_business_background = beta_business_background * `p3' if _n == _N
replace wgt_social_leader = beta_social_leader * `p4' if _n == _N
replace wgt_years_education = beta_years_education * `p5' if _n == _N
replace wgt_age_candidate = beta_age_candidate * `p6' if _n == _N
replace wgt_campaign_budget = beta_campaign_budget * `p7' if _n == _N
replace wgt_years_in_local_government = beta_years_in_local_government * `p8' ///
if _n == _N
}
}
}
}
}
}
}
}
drop if _n == 1

append using "data/temp/intermediary_00.dta"

foreach var in prior_experience criminal_record business_background social_leader years_education age_candidate campaign_budget years_in_local_government {
	gen adj_`var' = delta_`var' * wgt_`var'
}

gen correction = adj_prior_experience + adj_criminal_record ///
+ adj_business_background + adj_social_leader + adj_years_education ///
+ adj_age_candidate + adj_campaign_budget + adj_years_in_local_government

summarize correction
local correction_mean = r(mean)
histogram correction, bin(50) normal xline(`correction_mean')
graph export "outputs/correction_term_distribution_plot.pdf", replace

// 2. Instrumental variables with controls

clear

// 2.a.

set seed 20241130

set obs 10000

gen x1 = 0
gen x2 = 0
gen covariate_type = 1

replace x1 = 1 if _n >= 2501 & _n <= 5000
replace x1 = 1 if _n >= 7501 & _n <= 10000
replace x2 = 1 if _n >= 5001 & _n <= 7500
replace x2 = 1 if _n >= 7501 & _n <= 10000
replace covariate_type = 2 if _n >= 2501 & _n <= 5000
replace covariate_type = 3 if _n >= 5001 & _n <= 7500
replace covariate_type = 4 if _n >= 7501 & _n <= 10000

gen Z = 0
replace Z = 1 if _n >= 1 & _n <= 1000
replace Z = 1 if _n >= 2501 & _n <= 3000
replace Z = 1 if _n >= 5001 & _n <= 6500
replace Z = 1 if _n >= 7501 & _n <= 9500

gen x1x2 = x1*x2

gen shuffle = runiform()
sort covariate_type shuffle
gen G = "AT"
replace G = "NT" if _n >= 1 & _n <= 833
replace G = "CP" if _n >= 834 & _n <= 1250
replace G = "NT" if _n >= 2501 & _n <= 3333
replace G = "CP" if _n >= 3334 & _n <= 4166
replace G = "NT" if _n >= 5001 & _n <= 5833
replace G = "CP" if _n >= 5834 & _n <= 6250
replace G = "NT" if _n >= 7501 & _n <= 8334
replace G = "CP" if _n >= 8335 & _n <= 9167
drop shuffle

gen epsilon = rnormal()
gen nu = rnormal()

gen Y_0 = epsilon

gen Y_1 = 0
replace Y_1 = 2 + 0.5*x1 + 1.5*x2 + nu if G == "NT"
replace Y_1 = 6 + 2*x1 + x2 + nu       if G == "CP"
replace Y_1 = 10 + x1 + nu             if G == "AT"

gen D = 0
replace D = 1 if G == "AT"
replace D = Z if G == "CP"

gen Y = D * Y_1 + (1-D) * Y_0

ivregress 2sls Y (D = Z) x1 x2 x1x2
est store IV_2SLS

regress Z x1 x2 c.x1#c.x2
predict Z_res, residuals

ivregress 2sls Y (D = Z_res)
est store IV_Wald

esttab IV_2SLS IV_Wald ///
using "outputs/IV_2SLS_IV_Wald_table.tex", replace ///
label booktabs ///
title("IV estimation\label{tab:IV-2SLS-IV-Wald}") ///
mtitles("2SLS" "Wald") ///
style(tex) nonumber noomitted nobaselevels

// 2.b.
ivregress 2sls Y (D = Z) x1 x2 x1x2 if covariate_type == 1
est store CP_1_Est
regress Y D x1 x2 x1x2 if G == "CP" & covariate_type == 1
est store CP_1_True
scalar beta_CP_1 = _b[D]
ivregress 2sls Y (D = Z) x1 x2 x1x2 if covariate_type == 2
est store CP_2_Est
regress Y D x1 x2 x1x2 if G == "CP" & covariate_type == 2
est store CP_2_True
scalar beta_CP_2 = _b[D]
ivregress 2sls Y (D = Z) x1 x2 x1x2 if covariate_type == 3
est store CP_3_Est
regress Y D x1 x2 x1x2 if G == "CP" & covariate_type == 3
est store CP_3_True
scalar beta_CP_3 = _b[D]
ivregress 2sls Y (D = Z) x1 x2 x1x2 if covariate_type == 4
est store CP_4_Est
regress Y D x1 x2 x1x2 if G == "CP" & covariate_type == 4
est store CP_4_True
scalar beta_CP_4 = _b[D]

estwide CP_1_Est CP_1_True CP_2_Est CP_2_True CP_3_Est CP_3_True CP_4_Est CP_4_True ///
using "outputs/IV_2SLS_IV_Wald_CP_table.tex", replace ///
label booktabs ///
title("Estimated and true conditional LATEs \label{tab:IV-2SLS-IV-Wald-CP}") ///
mtitles("2SLS X=(0,0)" "LATE X=(0,0)" "2SLS X=(1,0)" "LATE X=(1,0)" "2SLS X=(0,1)" "LATE X=(0,1)" "2SLS X=(1,1)" "LATE X=(1,1)") ///
style(tex) nonumber noomitted nobaselevels

save "data/temp/intermediary_1.dta", replace

// 2.c.

use "data/temp/intermediary_1.dta", clear

collapse (mean) D (count) D_new = D, by(covariate_type Z)
rename D mean_D
preserve
collapse (mean) mean_D, by(covariate_type)
rename mean_D group_mean_D
save temp_group_means, replace
restore
merge m:1 covariate_type using temp_group_means
drop _merge
gen diff = mean_D - group_mean_D
gen diff_squared = diff^2
collapse (mean) diff_squared, by(covariate_type)
rename diff_squared var_D
summarize var_D
gen weight = var_D / r(mean)
levelsof covariate_type, local(covariate_type_values)
forval i = 1/4 {
    scalar weight_X`i' = weight[`i']
	di weight_X`i'
}

scalar beta_2SLS = (weight_X1 * beta_CP_1 + weight_X2 * beta_CP_2 ///
+ weight_X3 * beta_CP_3 + weight_X4 * beta_CP_4) /4

est restore IV_2SLS
estadd scalar Weight00 = weight_X1, replace
estadd scalar Weight10 = weight_X2, replace
estadd scalar Weight01 = weight_X3, replace
estadd scalar Weight11 = weight_X4, replace
estadd scalar LATEsCvxWghtedAvg = beta_2SLS, replace
esttab . using "outputs/angrist_table2.tex", keep(D) varlabels(D "Beta 2SLS")replace stats(Weight00 Weight10 Weight01 Weight11 LATEsCvxWghtedAvg) title("Weights and convex weighted average of the conditional LATEs\label{tab:Angrist2}")  mtitles("") style(tex) nonumber not nostar

// 2.d.

// 2.e.

use "data/temp/intermediary_1.dta", clear

ivregress 2sls Y (D = Z)
est store CP_Est
regress Y D if G == "CP"
est store CP_True

esttab CP_Est CP_True ///
using "outputs/CP_table_unconditional.tex", replace ///
label booktabs ///
title("Estimated and true unconditional LATEs \label{tab:CPunconditional}") ///
mtitles("2SLS" "LATE") ///
style(tex) nonumber noomitted nobaselevels
