********************************************************************************
* GROWTH-CURVE MODELS - CHARITY DENSITY
********************************************************************************

/** Preliminaries **/
/*
set autotabgraphs on, perm
set cformat %9.3f
set scheme white_tableau
capture graph drop * 

cd "C:\Users\diarm\Dropbox\charity-density-project\" // PC
*/


********************************************************************************
** DATA STRUCTURE AND DESCRIPTIVES [A001]
********************************************************************************

use data\clean\la-stats-1971-2021-analysis.dta, clear
	
/** Sample description **/

xtdes
tabstat charpop town qual nonbw cunum popest, s(n p50 mean sd min max)
tab1 region pph_cat

sort la_id year
tabstat charpop, by(year) s(n p50 min max p10 p90)

** Variation between local authorities **

bys la_id: egen mncharpop = mean(charpop)
qui sum charpop
local mn = round(`r(mean)', .01)
di "`mn'"
scatter mncharpop la_id, yline(`mn') title("Mean Charity Density (1971-2021)") subtitle("By Local Authority") ytitle("Number of charities per 1000 residents") ///
	note("Dashed line represents overall mean density (`mn')", size(small)) ///
	msize(large) xtitle(" ") xlab(, nolab)
graph export figures\fig1a-la-mean-density.png, replace width(4096)

** Variation within local authorities **

sort la_id period
twoway connect charpop period, connect(ascending) ///
	msize(large) mcolor(%10) lcolor(%50) connect(l) title("Charity Density") subtitle("By Local Authority") ///
	xtitle("Census year") xlab(0 "1971" 1 "1981" 2 "1991" 3 "2001" 4 "2011" 5 "2021")
graph export figures\fig1b-la-density-over-time.png, replace width(4096)

	** Use random sample of 50 LAs for clearer graph
	
	egen pickone = tag(la_id)
	set seed 20230906
	randomtag if pickone, count(50) gen(samp50)
	sort la_id samp50
	bys la_id: replace samp50 = samp50[6]
	
	sort la_id period
	twoway connect charpop period if samp50, connect(ascending) ///
		msize(large) mcolor(%50) lcolor(%70) connect(l) title("Charity Density") subtitle("By Local Authority") ///
		xtitle("Census year") xlab(0 "1971" 1 "1981" 2 "1991" 3 "2001" 4 "2011" 5 "2021")
	graph export figures\fig1b-la-density-over-time-sample50.png, replace width(4096)
	

	** Plot the growth curves overlaid on top of one another
	
	twoway connect charpop period, connect(ascending)
	/*
		Looks quadratic: intitial rapid growth that tapers off.
		
		Possible fanning out over time as well: differences between LAs
		are greater in recent years. Apply variance function during modelling.
	*/
	
	
	
********************************************************************************
** Statistical Models [A002]
********************************************************************************
/*
	* Is there LA variation in deprivation effect?
	* Is there an interaction between deprivation and time trend?
*/

/** Centre continuous predictors **/

foreach var in qual nonbw {
	qui sum `var'
	gen `var'_c = `var' - `r(mean)'
}

collect clear

/** Model 1 - Null **/

mixed charpop || la_id:
display -2*e(ll)
scalar m1sigma2u = exp([lns1_1_1]_cons)^2
scalar m1sigma2e = exp(_b[lnsig_e:_cons])^2
est store m1
estat icc
estat ic
qui etable
/*
	Strong evidence that most of the variation in the outcome is at
	the local authority-level.
*/

/** Model 2 - GCM **/
	
mixed charpop period period2 || la_id: period, covariance(unstructured)
display %9.0f -2*e(ll)
scalar m2sigma2u = exp([lns1_1_2]_cons)^2
scalar m2sigma2e = exp(_b[lnsig_e:_cons])^2
display %9.2f (m1sigma2u - m2sigma2u)/m1sigma2u
display %9.2f (m1sigma2e - m2sigma2e)/m1sigma2e
estat recovariance, correlation format(%9.3f)
estat icc
estat ic
est store m2
lrtest m1 m2
qui etable, append

/** Model 3 - GCM with Deprivation **/

mixed charpop period period2 town town2 || la_id: period, covariance(unstructured)
display %9.0f -2*e(ll)
scalar m3sigma2u = exp([lns1_1_2]_cons)^2
scalar m3sigma2e = exp(_b[lnsig_e:_cons])^2
display %9.2f (m2sigma2u - m3sigma2u)/m2sigma2u
display %9.2f (m2sigma2e - m3sigma2e)/m2sigma2e
estat recovariance, correlation format(%9.3f)
estat icc
estat ic
est store m3
lrtest m2 m3
qui etable, append

/** Model 4 - GCM with Deprivation, Ethnicity and Quals **/

mixed charpop period period2 town town2 qual_c nonbw_c || la_id: period, covariance(unstructured)
display %9.0f -2*e(ll)
scalar m4sigma2u = exp([lns1_1_2]_cons)^2
scalar m4sigma2e = exp(_b[lnsig_e:_cons])^2
display %9.2f (m3sigma2u - m4sigma2u)/m3sigma2u
display %9.2f (m3sigma2e - m4sigma2e)/m3sigma2e
estat recovariance, correlation format(%9.3f)
estat icc
estat ic
est store m4
lrtest m3 m4
qui etable, append

/** Model 5 - GCM with all time-varying covariates (TVCs) and time-invariant covariates (TICs) **/

mixed charpop period period2 town town2 qual_c nonbw_c ib7.region ib1.pph_cat || la_id: period, covariance(unstructured)
display %9.0f -2*e(ll)
mixed, estmetric
scalar m5sigma2u = exp([lns1_1_2]_cons)^2
scalar m5sigma2e = exp(_b[lnsig_e:_cons])^2
display %9.2f (m4sigma2u - m5sigma2u)/m4sigma2u
display %9.2f (m4sigma2e - m5sigma2e)/m4sigma2e
estat recovariance, correlation format(%9.3f)
estat icc
estat ic
est store m5
lrtest m4 m5
qui etable, append

	
	** Diagnostics **
	
	est restore m5
	
	predict xb
	predict xbu, fitted
	bys period: egen mnxbu = mean(xbu)
	predict u1 u0, reffects
	predict u1se u0se, reses
	predict res, residuals
	predict res_std, rstandard
	* DFBETAs for covariates
	
	** Model fit
	
	scatter charpop xbu
	scatter charpop xb
		
	** Linear correlation between random effects
	
	twoway (scatter u1 u0) (lfit u1 u0)
	corr u1 u0
	/*
		Linear with minor fanning out at high values of intercept
	*/

	** Normality of L2 and L1 residuals
	
	hist u0, percent norm
	hist res, percent norm
	hist res_std, percent norm
	/*
		L1 residuals are normal, L2 are positively skewed. Suggests the random effects are picking up omitted variables.
	*/
	
	scatter res_std la_id, yline(0) mlab(la_id)
	scatter u0 la_id, yline(0) mlab(la_id)
	scatter u1 la_id, yline(0) mlab(la_id) // identify extreme local authorities
	
	qui sum u0
	gen u0_std = u0 / `r(sd)'
	hist u0_std, percent norm
	scatter u0_std la_id, yline(0) mlab(la_id)
		
	** Heteroscedasticity of random effects and L1 residuals
	
	scatter res period
	scatter res town
	scatter res qual
	scatter res nonbw
	scatter res pph_cat
	/*
		Overall fairly homoscedastic, with the noticeable exception of high deprivation (these are areas
		with large random effects)
	*/

	** Caterpillar plots
	
	scatter u0 region
	graph box u0, ascategory by(region)
	graph box u1, ascategory by(region)
	
	egen tag = tag(la_id)
	egen u0rank = rank(u0) if tag==1
	serrbar u0 u0se u0rank if tag==1, scale(1.96) yline(0) ytitle("Deviation from average level") title("Hotspots and Coldspots") xtitle("Local Authority id")
	
	egen u1rank = rank(u1) if tag==1
	serrbar u1 u1se u1rank if tag==1, scale(1.96) yline(0) ytitle("Deviation from average linear time trend") title("Above and Below Trend") xtitle("Local Authority id")

	** Postestimation analyses **
	
	** Predicted growth curves
	
	sort la_id period
	twoway (line xbu period, connect(ascending)) (line mnxbu period, connect(ascending) lwidth(thick)) ///
		, legend(pos(6) size(small) rows(1) label(1 "LA Growth Trajectories") label(2 "Typical Growth Trajectory")) title("Predicted Growth Trajectories") ytitle("Number of charities per 1000 residents") xtitle("Census year") xlab(0 "1971" 1 "1981" 2 "1991" 3 "2001" 4 "2011" 5 "2021")
	graph export figures\fig2-predicted-trajectories.png, replace width(4096)
		
	// By region
	
	drop mnxbu
	bys period region: egen mnxbu = mean(xbu)
	
	twoway (line mnxbu period, connect(ascending) by(region, noiy title("Predicted Growth Trajectories") subtitle("By UK region") note("")) ///
		xlab(0 "71" 1 "81" 2 "91" 3 "01" 4 "11" 5 "21")) ///
		, xtitle("Census year") ytitle("Number of charities per 1000 residents")
	graph export figures\fig3-predicted-trajectories-region.png, replace width(4096)
	
	// By Urban / Rural

	drop mnxbu
	bys period pph_cat: egen mnxbu = mean(xbu)
	
	twoway (line mnxbu period, connect(ascending) by(pph_cat, noiy title("Predicted Growth Trajectories") subtitle("By rurality") note("")) ///
		xlab(0 "71" 1 "81" 2 "91" 3 "01" 4 "11" 5 "21")) ///
		, xtitle("Census year") ytitle("Number of charities per 1000 residents")
	graph export figures\fig4-predicted-trajectories-pphcat.png, replace width(4096)

exit
