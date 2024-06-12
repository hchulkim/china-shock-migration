

*============================================================================================================================================================================================================
* This code plots all necessary figures used in our paper using stata.
*
*============================================================================================================================================================================================================



* this computes rough regression (ols, 2sls) results and plots scatter plot for first stage

 clear all
 set more off

 
 *set your own path
 glo user "hckim"
 if "${user}" =="hckim" glo cdir "/Users/hchulkim/Dropbox/00_china_labor"

 
 glo datapath = "$cdir/data"

 
 *read data (2001-2010) ===================================================

   clear all
   use "$datapath/final/shock/shock_main_data2001_2010.dta"

ivreghdfe migrate (imp_shock_b= imp_shock_iv_b) [aw=pop2001_o], cluster(cz_o)

* 2SLS 1st Stage Regression (import) scatter plot
reg imp_shock_b imp_shock_iv_b [aw=pop2001_d]

#delimit;
set scheme s2color;
avplot imp_shock_iv_b,
t1("First Stage Regression (import), 2001-2010")
xtitle("Change in Predicted Import Exposure per Worker (in kUSD)")
ytitle("Change in Import Exposure per Worker (in kUSD)")
msymbol(circle)
ylabel(-10(10)10)
saving($cdir/results/figures/iv_imp2010_weight.gph, replace);
#delimit cr



* 2SLS 1st Stage Regression (export) scatter plot
reg exp_shock_b exp_shock_iv_b [aw=pop2001_d]

#delimit;
set scheme s2color;
avplot exp_shock_iv_b,
t1("First Stage Regression (export), 2001-2010")
xtitle("Change in Predicted Export Exposure per Worker (in kUSD)")
ytitle("Change in Export Exposure per Worker (in kUSD)")
msymbol(circle)
ylabel(-20(10)30)
saving($cdir/results/figures/iv_exp2010_weight.gph, replace);
#delimit cr




* ====================================================================================================



 *read data (2010-2019) ===================================================

   clear all
   use "$datapath/final/shock/shock_main_data2010_2019.dta"



* 2SLS 1st Stage Regression (import) scatter plot
reg imp_shock_b imp_shock_iv_b [aw=pop2010_d]

#delimit;
set scheme s2color;
avplot imp_shock_iv_b,
t1("First Stage Regression (import), 2010-2019")
xtitle("Change in Predicted Import Exposure per Worker (in kUSD)")
ytitle("Change in Import Exposure per Worker (in kUSD)")
msymbol(circle)
ylabel(-20(10)30)
saving($cdir/results/figures/iv_imp2019_weight.gph, replace);
#delimit cr



* 2SLS 1st Stage Regression (export) scatter plot
reg exp_shock_b exp_shock_iv_b [aw=pop2010_d]

#delimit;
set scheme s2color;
avplot exp_shock_iv_b,
t1("First Stage Regression (export), 2010-2019")
xtitle("Change in Predicted Export Exposure per Worker (in kUSD)")
ytitle("Change in Export Exposure per Worker (in kUSD)")
msymbol(circle)
ylabel(-20(10)30)
saving($cdir/results/figures/iv_exp2019_weight.gph, replace);
#delimit cr


* ====================================================================================================



 *read data (2001-2019) ===================================================

   clear all
   use "$datapath/final/shock/shock_main_data2001_2019.dta"



* 2SLS 1st Stage Regression (import) scatter plot
reg imp_shock_b imp_shock_iv_b [aw=pop2001_d]

#delimit;
set scheme s2color;
avplot imp_shock_iv_b,
t1("First Stage Regression (import), 2001-2019")
xtitle("Change in Predicted Import Exposure per Worker (in kUSD)")
ytitle("Change in Import Exposure per Worker (in kUSD)")
msymbol(circle)
ylabel(-20(10)30)
saving($cdir/results/figures/iv_imp_weight.gph, replace);
#delimit cr



* 2SLS 1st Stage Regression (export) scatter plot
reg exp_shock_b exp_shock_iv_b [aw=pop2001_d]

#delimit;
set scheme s2color;
avplot exp_shock_iv_b,
t1("First Stage Regression (export), 2001-2019")
xtitle("Change in Predicted Export Exposure per Worker (in kUSD)")
ytitle("Change in Export Exposure per Worker (in kUSD)")
msymbol(circle)
ylabel(-20(10)30)
saving($cdir/results/figures/iv_exp_weight.gph, replace);
#delimit cr



* ====================================================================================================




* 2SLS Regression (2001-2010)

   clear all
   use "$datapath/final/shock/shock_main_data2001_2010.dta"



eststo clear
eststo: ivregress 2sls migrate (imp_shock_b exp_shock_b = imp_shock_iv_b exp_shock_iv_b) [aw=pop2001_d], first 
eststo: ivregress 2sls migrate (imp_shock_b exp_shock_b = imp_shock_iv_b exp_shock_iv_b) i.cz_o [aw=pop2001_d], first 
eststo: ivregress 2sls migrate (imp_shock_b exp_shock_b = imp_shock_iv_b exp_shock_iv_b) i.cz_d [aw=pop2001_d], first 
eststo: ivregress 2sls migrate (imp_shock_b exp_shock_b = imp_shock_iv_b exp_shock_iv_b) i.cz_o i.cz_d [aw=pop2001_d], first 
esttab using $cdir/results/tables/2sls2010_4.tex, b(%9.3f) se(%9.3f) r2 replace


	
	
	
* 2SLS Regression (2010-2019)


   clear all
   use "$datapath/final/shock/shock_main_data2010_2019.dta"
	
	
eststo clear
eststo: ivregress 2sls migrate (imp_shock_b exp_shock_b = imp_shock_iv_b exp_shock_iv_b) [aw=pop2010_d], first
esttab using $cdir/results/tables/2sls2019.tex, b(%9.3f) se(%9.3f) r2 replace



* 2SLS Regression (2001-2019)


   clear all
   use "$datapath/final/shock/shock_main_data2001_2019.dta"
	
	
eststo clear
eststo: ivregress 2sls migrate (imp_shock_b exp_shock_b = imp_shock_iv_b exp_shock_iv_b) [aw=pop2001_d], first
esttab using $cdir/results/tables/2sls.tex, b(%9.3f) se(%9.3f) r2 replace




* ====================================================================================================

*ols regression  (2001-2019)


   clear all
   use "$datapath/final/shock/shock_main_data2001_2019.dta"

eststo clear
eststo: reg migrate imp_shock_b exp_shock_b [aw=pop2001_d]
esttab using $cdir/results/tables/ols.tex, b(%9.3f) se(%9.3f) r2 replace




	
	
*ols regression (2001-2010)

   clear all
   use "$datapath/final/shock/shock_main_data2001_2010.dta"


eststo clear
eststo: reg migrate imp_shock_b exp_shock_b [aw=pop2001_d]
esttab using $cdir/results/tables/ols2010.tex, b(%9.3f) se(%9.3f) r2 replace



*ols regression  (2010-2019)


   clear all
   use "$datapath/final/shock/shock_main_data2010_2019.dta"

eststo clear
eststo: reg migrate imp_shock_b exp_shock_b [aw=pop2010_d]
esttab using $cdir/results/tables/ols2019.tex, b(%9.3f) se(%9.3f) r2 replace

