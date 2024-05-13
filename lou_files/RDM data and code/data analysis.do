clear
mata: mata clear
clear matrix 
set matsize 10000
set maxvar  120000



*cd "\\net.nefsc.noaa.gov\aharris\2022 fluke survey data\"
cd "Z:\choice experiment data\2022 fluke survey data"
use "surveydata.dta", clear 
keep qtid a4
duplicates drop 
tempfile boat
save `boat', replace

use "formattedsurveydata.dta", clear 
merge m:1 qtid using `boat', keep(3) nogen


/* //////////////////////////////////////////////////////////////////////////////////////////////////
Creating additional variables for running the logit models/////////////////////////////////// */
gen constant=0
replace constant=1 if trip=="C"
gen avidity=(a3_party+a3_chart+a3_shore+a3_priva)
gen male=d1
replace male=0 if d1==2
gen birthday=date(registra,"DMY")
gen age=age(birthday, td(25jul2022))
drop birthday
gen likely=1 if inlist(a5, 1,2)
replace likely=0 if inlist(a5, 3, 4)
gen ownboat=1 if inlist(a4, 1)
replace ownboat=0 if inlist(a4, 0)


gen income_low=1 if inlist(d4, 1, 2, 3) /*Creating the dummies for Income. The excluded category if Income_low, which includes income levels of 1,2 and 3*/

gen income_medium=0  /*Creating the dummies for Income. The excluded category if Income_low, which includes income levels of 1,2 and 3*/
replace income_medium=1 if (d4>3 & d4<7)
replace income_medium=. if mi(d4)
gen income_high=0
replace income_high=1 if d4>6
replace income_high=. if mi(d4)
gen education_basic =1 if inlist(d3, 1, 2)
gen education_college=0  /*Creating the dummies for Education. The excluded category if Education_basic, which includes education levels of 1,2 and 3*/
replace education_college=1 if (d3>2 & d3<6)
replace education_college=. if mi(d3)
gen education_graduate=0
replace education_graduate=1 if d3>5
replace education_graduate=. if mi(d3)

replace avidity=constant*avidity
replace age=constant*age
replace income_medium=constant*income_medium
replace income_high=constant*income_high
replace education_college=constant*education_college
replace education_graduate=constant*education_graduate
replace male=constant*male
replace likely=constant*likely
replace ownboat=constant*ownboat

gen SFkept=(keep_fluke_max+keep_fluke_min)/2
replace SFkept=(keep_fluke_0*p_keep_fluke_0+keep_fluke_1*p_keep_fluke_1+keep_fluke_2*p_keep_fluke_2+keep_fluke_3*p_keep_fluke_3+keep_fluke_4*p_keep_fluke_4+keep_fluke_5*p_keep_fluke_5+keep_fluke_6*p_keep_fluke_6+keep_fluke_7*p_keep_fluke_7+keep_fluke_8*p_keep_fluke_8) if keep_fluke_distribution=="Yes"
gen SFrelease=(catch_fluke_max+catch_fluke_min)/2-SFkept


gen BSBkept=keep_bsb
gen BSBrelease=catch_bsb-keep_bsb
*gen catch_bsb_check=BSBrelease+BSBkept

gen SCUPkept=keep_scup
gen SCUPrelease=catch_scup-keep_scup
*gen catch_scup_check=SCUPkept+SCUPrelease

gen KeepOther=(keep_bsb+keep_scup)
gen ReleaseOther=(catch_bsb+catch_scup)-KeepOther
*gen release_bsb=catch_bsb-keep_bsb
*gen release_scup=catch_scup-keep_scup

/*The opt-out option*/
replace SFkept=0 if trip=="C"
replace SFrelease=0 if trip=="C"
replace ReleaseOther=0 if trip=="C"
replace KeepOther=0 if trip=="C"
replace BSBkept=0 if trip=="C"
replace BSBrelease=0 if trip=="C"
replace SCUPkept=0 if trip=="C"
replace SCUPrelease=0 if trip=="C"
replace cost=cost
replace cost=0 if trip=="C"
replace catch_scup=0 if trip=="C"

/* square roots of catch*/
local vars SFkept SFrelease BSBkept BSBrelease SCUPkept SCUPrelease
foreach v of local vars{
	gen sqrt_`v'=sqrt(`v')
	
}

gen sqrt_scup_catch= sqrt(catch_scup)
gen sqrt_SF_catch= sqrt(SFkept+SFrelease)
gen sqrt_BSB_catch= sqrt(BSBkept+BSBrelease)

replace sqrt_scup_catch=0 if trip=="C"
replace sqrt_SF_catch=0 if trip=="C"
replace sqrt_BSB_catch=0 if trip=="C"


*Check for protest responses: chose opt-out for every question answered
gen check=1 if chosen==1 & trip=="C"
bysort qtid: egen sumoptout=sum(check)

bysort qtid id: gen nset=1 if _n==1
egen nsets=sum(nset), by(qtid)

gen protest=1 if sumoptout==nsets
drop check sumoptout nset /*nsets*/


/*
*Demographics
keep avidity age income_low income_medium income_high education_college education_graduate male a1_none protest qtid nsets trip education_basic 
drop trip
duplicates drop 

gen drop1=1 if a1_none!=0
drop if drop1==1
gen drop2=1 if protest==1 
tab drop2 , missing
drop if drop2==1

tab male, missing
su age 
return list

tab education_basic, missing
tab education_college, missing
tab education_graduate, missing


tab income_low, missing
tab income_medium, missing
tab income_high, missing

su avidity
return list
*/
/*Mixed logit*/
/*
mixlogit chosen cost if a1_none==0,  group(identifier) id(qtid) rand(SFrelease SFkept ReleaseOther KeepOther constant)
estat ic

mixlogit chosen cost $interactions if a1_none==0,  group(identifier) id(qtid) rand(SFrelease SFkept ReleaseOther KeepOther constant)
estat ic
*/

*mixlogit chosen cost if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(SFkept SFrelease BSBkept BSBrelease SCUPkept SCUPrelease constant)
*mixlogit chosen cost $interactions if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(SFkept SFrelease BSBkept BSBrelease SCUPkept SCUPrelease constant)

*mixlogit chosen cost if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SCUPkept sqrt_SCUPrelease constant)

*mixlogit chosen cost $interactions2 if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SCUPkept sqrt_SCUPrelease constant)

*model 1: total scup catch, no demographics
/*
eststo m1: mixlogit chosen cost  if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease  sqrt_scup_catch constant)

eststo m2: mixlogit chosen cost $interactions if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease  sqrt_scup_catch constant)



mixlogit choice  cost if protest!=1,  group(gid) id(id) rand(`catchvars' `catchsizes' optout $controls striper_blue) nreps(50)


gen one=1
eststo clear	
clogit chosen one if  a1_none==0 & protest!=1 ,  group(identifier) 
di 1--7720.918/-9894.102

eststo m1: mixlogit chosen cost if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease  sqrt_scup_catch constant)

estat ic
matrix s = r(S)
estadd scalar AIC = s[1,5]
estadd scalar AIC_n = s[1,5]/s[1,1]
estadd scalar BIC_n = s[1,6]/s[1,1]
estadd scalar r2_p = .21964439

distinct qtid if a1_none==1 
distinct qtid if protest==1
distinct qtid if a1_none==0 & protest!=1
distinct qtid if e(sample)
distinct identifier if e(sample)

eststo w1: nlcom (sfkeep: -_b[sqrt_SFkept]/(2*_b[cost])) (sfrelease: -_b[sqrt_SFrelease]/(2*_b[cost])) ///
		  (bsbkeep: -_b[sqrt_BSBkept]/(2*_b[cost])) (bsbrelease: -_b[sqrt_BSBrelease]/(2*_b[cost])) ///
		  (otherkeep: -_b[sqrt_scup_catch]/(2*_b[cost])) , post 

esttab m1  using "regression_results.tex", replace nobase noomitted  /// 
				stats(N r2_p ll ll_0 aic, fmt(0 3 3 3))  se b(%9.3f) label nogaps    starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease  sqrt_scup_catch cost constant) 
				
esttab  w1  using "wtp_results.csv", replace nobase noomitted  /// 
				stats(N , fmt(0 ))  cells(`"b(fmt(a2) star) ci( par("(" " " ")"))"') label nogaps    starlevels( * 0.10 ** 0.05 *** 0.010)
								
	*/			


*gen sqrt_SF_BSB_keep = sqrt(SFkept+BSBkept)



global interactions avidity age income_medium income_high education_college education_graduate male likely ownboat
global interactions2 avidity 

***Final model (6/27/23)
*model with interactions between keep of sf and bsb
gen sqrt_SF_BSB_keep = sqrt_SFkept*sqrt_BSBkept
gen SF_BSB_keep = SFkept*BSBkept

gen one=1
egen group_q = group(qtid question)

egen all_rowmiss=rowmiss(avidity age income_medium income_high education_college education_graduate male likely ownboat)

eststo clear	
clogit chosen one if all_rowmiss==0 & a1_none==0 & protest!=1 ,  group(identifier) 
local l0 = e(ll) //LL with no paramaters

*full set of interactions
eststo m1: mixlogit chosen cost $interactions if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 

local ll = e(ll) 
di 1-(-6545.065/-8473.5966)
*estat ic
estadd scalar r2_p = 1-`ll'/`l0'

distinct qtid if e(sample)
local n_anglers1  =`r(ndistinct)'
distinct group_q if e(sample)
local n_choices1 = `r(ndistinct)'

estadd local n_anglers 
estadd local n_choices

*only signinfnact interactions
egen rowmiss2=rowmiss(age avidity ownboat)
clogit chosen one if rowmiss2==0 & a1_none==0 & protest!=1 ,  group(identifier) 
local l0 = e(ll) //LL with no paramaters

eststo m2: mixlogit chosen cost age avidity ownboat if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 
di 1-(-7140.780/-9161.3279)

local ll = e(ll) 

*estat ic
estadd scalar r2_p = 1-`ll'/`l0'

distinct qtid if e(sample)
local n_anglers1  =`r(ndistinct)'
distinct group_q if e(sample)
local n_choices1 = `r(ndistinct)'

estadd local n_anglers 
estadd local n_choices

*only signinfnact interactions 2
egen rowmiss3=rowmiss(age avidity)
clogit chosen one if rowmiss3==0 & a1_none==0 & protest!=1 ,  group(identifier) 
local l0 = e(ll) 
di `l0'

eststo m3: mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 
local ll = e(ll) 
di `ll'

di 1-`ll'/`l0'



*Test difference when enetering scup keep and release 
egen rowmiss3=rowmiss(age avidity)
clogit chosen one if rowmiss3==0 & a1_none==0 & protest!=1 ,  group(identifier) 
local l0 = e(ll) 
di `l0'

eststo m3: mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep  sqrt_SCUPkept sqrt_SCUPrelease constant) 
local ll = e(ll) 
di `ll'

di 1-`ll'/`l0'


*** using cmxtmixlogit
encode trip, gen(trip2)
cmset qtid question trip2

bysort qtid: egen sum_q=sum(chosen)
bysort  qtid question: egen sum_a=sum(chosen)


drop if sum_a==0 /*drop if no trip chosen*/
drop if sum_q==1 /*drop if only one question answered */


drop if a1_none==1 | protest==1 /*drop did not fish for target species or chose the opt-out every time*/
drop if age==. | avidity==. /*drop if did not provde age or avidity (which were signifcant interactions) */

sort qtid question 

sort qtid  trip2

bysort qtid trip (question): gen question2=_n
order question2
browse if question!=question2

sort qtid  question2  trip2
cmset qtid question2 trip2
cmxtmixlogit chosen cost age avidity , random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant)  intpoints(50)  technique(nr) noconstant  


*estimate the mix logit to get starting points
mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 

mat b= e(b)
cmxtmixlogit chosen cost age avidity if a1_none==0 & protest!=1, random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant)  intmethod(halton) intpoints(50)  technique(bhhh) noconstant
mat b= e(b)
cmxtmixlogit chosen cost age avidity if a1_none==0 & protest!=1, random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant)  intmethod(halton) intpoints(500)  technique(bhhh) noconstant from(b, skip)

**cmxtmixlogit is not converging. Follow Jorge and try to rescale the variables so they are all on the same order of magnite 
su cost /*mean 112.7395*/
su age /*mean 17.79278*/
su avidity /*mean 4.014733*/
su sqrt_SFkept /*mean .9962896*/
su sqrt_SFrelease /*mean 1.150575*/
su sqrt_BSBkept /*mean 1.013365*/
su sqrt_BSBrelease /*mean 1.348114*/
su sqrt_scup_catch /*mean 1.348114*/
su constant /*mean 1.348114*/

gen cost1000=cost/1000 /*mean .1127395*/
gen age100=age/100  /*mean .1779278*/
gen avidity100=avidity/100 /*mean .0401473*/

local vars SFkept SFrelease BSBkept BSBrelease catch_scup
foreach v of local vars{
	gen `v'100= `v'/100
	gen sqrt_`v'100=sqrt(`v'100)
	
}

gen sqrt_scup_catch= sqrt(catch_scup)
gen sqrt_SF_catch= sqrt(SFkept+SFrelease)
gen sqrt_BSB_catch= sqrt(BSBkept+BSBrelease)

gen sqrt_SF_BSB_keep100=sqrt_SFkept100*sqrt_BSBkept100

mixlogit chosen cost1000 age100 avidity100 if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept100 sqrt_SFrelease100 sqrt_BSBkept100 sqrt_BSBrelease100 sqrt_SF_BSB_keep100 sqrt_catch_scup100 constant) 

mat b= e(b)

cmxtmixlogit chosen cost1000 age100 avidity100 if a1_none==0 & protest!=1, random(sqrt_SFkept100 sqrt_SFrelease100 sqrt_BSBkept100 sqrt_BSBrelease100 sqrt_SF_BSB_keep100 sqrt_catch_scup100 constant)  intmethod(halton, antithetics) intpoints(100)  technique(bhhh) noconstant 

order _caseid _pane
mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 
mat b= e(b)

cmxtmixlogit chosen cost age avidity if a1_none==0 & protest!=1, random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant)  intmethod(halton, antithetics) intpoints(500)  technique(bfgs 10 nr 5) noconstant from(b)


mat b= e(b)


mixlogit chosen cost age avidity , group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 
mat b= e(b)

sort qtid identifier question trip2
cmxtmixlogit chosen cost age avidity , random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant)  intmethod(halton) intpoints(1000)  noconstant  technique(bhhh)  

cmxtmixlogit chosen cost age avidity , random(SFkept SFrelease BSBkept BSBrelease SF_BSB_keep catch_scup constant)  intmethod(halton) intpoints(100)  noconstant  technique(bhhh)  



cmxtmixlogit chosen cost  age avidity , random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep  sqrt_scup_catch constant)  intmethod(halton) intpoints(100)  noconstant  technique(bhhh)  


mixlogit chosen cost1000 age100 avidity100 , group(identifier) id(qtid) rand(sqrt_SFkept100 sqrt_SFrelease100 sqrt_BSBkept100 sqrt_BSBrelease100 sqrt_SF_BSB_keep100 sqrt_catch_scup100 constant) 

mat b= e(b)

cmxtmixlogit chosen cost1000 age100 avidity100, random(sqrt_SFkept100 sqrt_SFrelease100 sqrt_BSBkept100 sqrt_BSBrelease100 sqrt_SF_BSB_keep100 sqrt_catch_scup100 constant)  intmethod(halton, antithetics) intpoints(100)  technique(bhhh) noconstant  from(b, copy)

mat input c -11.6278 1.046345 -1.043399 8.274556 .6496854 3.525158 .7378899 -5.615605 .1795179 -2.055716 12.67089 ///
 3.2506 1.288044 .5463283 19.62749 .2447442 1.977476 

matrix input A = (-11.6278 ,1.046345, -1.043399 ,8.274556 ,.6496854, 3.525158, .7378899 ,-5.615605 ,.1795179, -2.055716 ,12.67089 , 3.2506 ,1.288044 ,.5463283 ,19.62749 ,.2447442 ,1.977476 )


cmxtmixlogit chosen cost1000 age100 avidity100, random(sqrt_SFkept100 sqrt_SFrelease100 sqrt_BSBkept100 sqrt_BSBrelease100 sqrt_SF_BSB_keep100 sqrt_catch_scup100 constant)  intmethod(halton, antithetics) intpoints(100)  technique(nr) noconstant  from(A, copy) gradient  ltol(0) tol(1e-7) difficult

matrix input A0 = (0,0, 0 ,0 ,0, 0, 0 ,0 ,0, 0 ,0 , 0 ,0 ,0 ,0,0 ,0 )


cmxtmixlogit chosen cost1000 age100 avidity100, random(sqrt_SFkept100 sqrt_SFrelease100 sqrt_BSBkept100 sqrt_BSBrelease100 sqrt_SF_BSB_keep100 sqrt_catch_scup100 constant)  intmethod(halton, antithetics) intpoints(50)  technique(nr) noconstant  from(A0, copy) gradient 


mixlogit chosen cost age avidity , group(identifier) id(qtid) rand(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant) 


drop if sum_q==1
cmxtmixlogit chosen cost age avidity , random(sqrt_SFkept sqrt_SFrelease sqrt_BSBkept sqrt_BSBrelease sqrt_SF_BSB_keep sqrt_scup_catch constant)  intpoints(50)  technique(nr) noconstant  


from(b)

*linear in catch 
eststo m4: mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(SFkept SFrelease BSBkept BSBrelease SF_BSB_keep catch_scup constant) 
eststo m4: mixlogit chosen cost age avidity if a1_none==0 & protest!=1, group(identifier) id(qtid) rand(SFkept SFrelease BSBkept BSBrelease SF_BSB_keep  SCUPkept SCUPrelease constant) 

local ll = e(ll) 

*estat ic
estadd scalar r2_p = 1-`ll'/`l0'
di 1-(-7297.272/-9362.3739 )

distinct qtid if e(sample)
local n_anglers1  =`r(ndistinct)'
distinct group_q if e(sample)
local n_choices1 = `r(ndistinct)'

estadd local n_anglers 
estadd local n_choices

esttab m1 m2 m3 using "fluke_parameters.csv", replace nobase noomitted  /// 
				stats(n_anglers  n_choices ll r2_p aic , fmt(0 0 3 3 3))  se b(%9.3f) label nogaps    starlevels( * 0.10 ** 0.05 *** 0.010)  ///
				order(sqrt_SFkept sqrt_BSBkept sqrt_SF_BSB_keep sqrt_SFrelease  sqrt_BSBrelease  sqrt_scup_catch cost constant)

				
mixlpred pred

*k and r for increase in the substitue species 
* krinksy and robb procedure 
* Generate random b
global params
forv x=1/10000{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }
    
        * Generate draws from vector beta
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		

		* Now account for estimated preference heterogeneity by drawing 500 params from the simulated distributions
		*clear 
		*set obs 2000
		*gen mean_beta_sf_keep =  beta_draw[2,1]+ sqrt(abs(beta_draw[9,1]))*rnormal()
		*gen mean_beta_bsb_keep =  beta_draw[4,1] +sqrt(abs(beta_draw[11,1]))*rnormal()
		*gen mean_beta_sf_bsb_keep =  beta_draw[6,1]+ sqrt(abs(beta_draw[13,1]))*rnormal()
		*gen mean_beta_cost =  beta_draw[1,1]
		
		*Ignore preference heterogeneity
		clear 
		set obs 1
		gen mean_beta_sf_keep =  beta_draw[4,1]
		gen mean_beta_bsb_keep =  beta_draw[6,1] 
		gen mean_beta_sf_bsb_keep =  beta_draw[8,1]
		gen mean_beta_cost =  beta_draw[1,1]
		
		*forv i =0/26{
		forv i =0/15{

		gen wtp_sf_keep_bsb`i'=-(mean_beta_sf_keep+mean_beta_sf_bsb_keep*sqrt(`i'))/(2*mean_beta_cost*1)
		gen wtp_bsb_keep_sf`i'=-(mean_beta_bsb_keep+mean_beta_sf_bsb_keep*sqrt(`i'))/(2*mean_beta_cost*1)

		}
		
		
		/*
		gen wtp_sf_keep_bsb0 = -(beta_draw[2,1]+sqrt(abs(beta_draw[9,1]))*rnormal())/(2*beta_draw[1,1]*1)
		
		gen wtp_sf_keep_bsb`i'=-((beta_draw[2,1]+sqrt(abs(beta_draw[9,1]))*rnormal()))+((beta_draw[6,1]+sqrt(abs(beta_draw[13,1]))*rnormal())*sqrt(`i'))
		
		rnormal(0,abs(beta_draw[13,1])))*sqrt(`i')))/(2*beta_draw[1,1]*1)

		}
		
		rnormal(0,abs(beta_draw[9,1])))/(2*beta_draw[1,1]*1)

		gen wtp_sf_keep_bsb0 = -(beta_draw[2,1]+rnormal(0,abs(beta_draw[9,1])))/(2*beta_draw[1,1]*1)
		
		
		gen wtp_sf_keep_bsb`i'=-((beta_draw[2,1]+rnormal(0,abs(beta_draw[9,1])))+((beta_draw[6,1]+rnormal(0,abs(beta_draw[13,1])))*sqrt(`i')))/(2*beta_draw[1,1]*1)
		}
		
		gen mean_beta_sf_keep =  rnormal(beta_draw[2,1], abs(beta_draw[9,1]))
		gen mean_beta_bsb_keep =  rnormal(beta_draw[4,1], abs(beta_draw[11,1]))
		gen mean_beta_sf_bsb_keep =  rnormal(beta_draw[6,1], abs(beta_draw[13,1]))
		gen mean_beta_cost =  beta_draw[1,1]
		
		* generate wtp values based on the random params
		* wtp for first fish, no other keep 
		*gen wtp_sf_keep_bsb0 = -mean_beta_sf_keep/(2*mean_beta_cost*1)
		*gen wtp_bsb_keep = -mean_beta_bsb_keep/(2*mean_beta_cost*1)

		*wtp for first fish, increasing keep of other species 
		*forv i =1/20{
		*	gen wtp_sf_keep_bsb`i'=-(mean_beta_sf_keep+mean_beta_sf_bsb_keep*sqrt(`i'))/(2*mean_beta_cost*1)
		*}
		
		*/
		
	drop mean*	
	gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params


tempfile new
save `new', replace  

*WTP SF
/*
global wtp

*forv i=0/26{
	forv i=0/15{

	u `new', clear 
	keep wtp_sf_keep_bsb`i'
	
	su wtp_sf_keep_bsb`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort wtp_sf_keep_bsb`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su wtp_sf_keep_bsb`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen wtp_n_secondary = `i'

	tempfile wtp`i'
	save `wtp`i'', replace
	global wtp "$wtp "`wtp`i''" " 

}	

clear
dsconcat $wtp


gen spread=upper-lower

global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway rcap upper lower wtp_n_secondary || scatter wtp_med wtp_n_secondary, $graphoptions
gr play wtp_fluke
gr play wtp_fluke2
*/

*WTP BSB

global wtp

*forv i=0/26{
	forv i=0/15{

	u `new', clear 
	keep wtp_bsb_keep_sf`i'
	
	su wtp_bsb_keep_sf`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort wtp_bsb_keep_sf`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su wtp_bsb_keep_sf`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen wtp_n_secondary = `i'

	tempfile wtp`i'
	save `wtp`i'', replace
	global wtp "$wtp "`wtp`i''" " 

}	

clear
dsconcat $wtp


gen spread=upper-lower

global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
*twoway rcap upper lower wtp_n_secondary if wtp_n_secondary<=14 || scatter wtp_med wtp_n_secondary if wtp_n_secondary<=14, $graphoptions
twoway rcap upper lower wtp_n_secondary if wtp_n_secondary<=15 || scatter wtp_med wtp_n_secondary if wtp_n_secondary<=15, $graphoptions

gr play wtp_fluke
gr play wtp_fluke2
gr play wtp_bsb1
gr play wtp_bsb2

*/


*k and r for increase in the primary species 
* krinksy and robb procedure 
* Generate random b
global params
forv x=1/2000{
local K=e(k) //-e(krnd)
mat bfull=e(b)
mat b=bfull[1,1..`K']
mat Vfull=e(V)
mat Ve=Vfull[1..`K',1..`K']
mat cholV=cholesky(Ve)

mat iid_err=J(`K',1,0)
        
        forvalues i=1/`K' {
            mat iid_err[`i',1]=rnormal()
        }
    
        * Generate draws from vector beta
        mat beta_draw=b' + cholV * iid_err
		mat  list beta_draw
		

		* Now account for estimated preference heterogeneity by drawing 500 params from the simulated distributions
		*clear 
		*set obs 2000
		*gen mean_beta_sf_keep =  beta_draw[2,1]+ sqrt(abs(beta_draw[9,1]))*rnormal()
		*gen mean_beta_bsb_keep =  beta_draw[4,1] +sqrt(abs(beta_draw[11,1]))*rnormal()
		*gen mean_beta_sf_bsb_keep =  beta_draw[6,1]+ sqrt(abs(beta_draw[13,1]))*rnormal()
		*gen mean_beta_cost =  beta_draw[1,1]
		
		*Ignore preference heterogeneity
		clear 
		set obs 1
		gen mean_beta_sf_keep =  beta_draw[4,1]
		gen mean_beta_bsb_keep =  beta_draw[6,1] 
		gen mean_beta_sf_bsb_keep =  beta_draw[8,1]
		gen mean_beta_cost =  beta_draw[1,1]
		
		forv i =1/15{
		gen wtp_sf_keep_`i'=-(mean_beta_sf_keep/(2*mean_beta_cost*sqrt(`i')))
		gen wtp_bsb_keep_`i'=-(mean_beta_bsb_keep/(2*mean_beta_cost*sqrt(`i')))

		}
		
		gen sum_wtp_sf_keep_1=wtp_sf_keep_1
		gen sum_wtp_bsb_keep_1=wtp_bsb_keep_1

		order wtp_sf_keep_*  wtp_bsb_keep_*
		forv i =2/15{
			egen sum_wtp_sf_keep_`i'=rowtotal(wtp_sf_keep_1-wtp_sf_keep_`i')
			egen sum_wtp_bsb_keep_`i'=rowtotal(wtp_bsb_keep_1-wtp_bsb_keep_`i')

		}
		
		/*
		gen wtp_sf_keep_bsb0 = -(beta_draw[2,1]+sqrt(abs(beta_draw[9,1]))*rnormal())/(2*beta_draw[1,1]*1)
		
		gen wtp_sf_keep_bsb`i'=-((beta_draw[2,1]+sqrt(abs(beta_draw[9,1]))*rnormal()))+((beta_draw[6,1]+sqrt(abs(beta_draw[13,1]))*rnormal())*sqrt(`i'))
		
		rnormal(0,abs(beta_draw[13,1])))*sqrt(`i')))/(2*beta_draw[1,1]*1)

		}
		
		rnormal(0,abs(beta_draw[9,1])))/(2*beta_draw[1,1]*1)

		gen wtp_sf_keep_bsb0 = -(beta_draw[2,1]+rnormal(0,abs(beta_draw[9,1])))/(2*beta_draw[1,1]*1)
		
		
		gen wtp_sf_keep_bsb`i'=-((beta_draw[2,1]+rnormal(0,abs(beta_draw[9,1])))+((beta_draw[6,1]+rnormal(0,abs(beta_draw[13,1])))*sqrt(`i')))/(2*beta_draw[1,1]*1)
		}
		
		gen mean_beta_sf_keep =  rnormal(beta_draw[2,1], abs(beta_draw[9,1]))
		gen mean_beta_bsb_keep =  rnormal(beta_draw[4,1], abs(beta_draw[11,1]))
		gen mean_beta_sf_bsb_keep =  rnormal(beta_draw[6,1], abs(beta_draw[13,1]))
		gen mean_beta_cost =  beta_draw[1,1]
		
		* generate wtp values based on the random params
		* wtp for first fish, no other keep 
		*gen wtp_sf_keep_bsb0 = -mean_beta_sf_keep/(2*mean_beta_cost*1)
		*gen wtp_bsb_keep = -mean_beta_bsb_keep/(2*mean_beta_cost*1)

		*wtp for first fish, increasing keep of other species 
		*forv i =1/20{
		*	gen wtp_sf_keep_bsb`i'=-(mean_beta_sf_keep+mean_beta_sf_bsb_keep*sqrt(`i'))/(2*mean_beta_cost*1)
		*}
		
		*/
		
	drop mean*	
	gen draw=`x'
		
	tempfile params`x'
	save `params`x'', replace
	global params "$params "`params`x''" " 

}	

clear
dsconcat $params


tempfile new
save `new', replace  

*WTP SF
/*
global wtp

forv i=1/15{
	u `new', clear 
	keep sum_wtp_sf_keep_`i'
	
	su sum_wtp_sf_keep_`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort sum_wtp_sf_keep_`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su sum_wtp_sf_keep_`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen wtp_n_secondary = `i'

	tempfile wtp`i'
	save `wtp`i'', replace
	global wtp "$wtp "`wtp`i''" " 

}	

clear
dsconcat $wtp


gen spread=upper-lower

global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway rcap upper lower wtp_n_secondary || scatter wtp_med wtp_n_secondary, $graphoptions
gr play wtp_fluke
*/

*WTP BSB
global wtp

forv i=1/15{
	u `new', clear 
	keep sum_wtp_bsb_keep_`i'
	
	su sum_wtp_bsb_keep_`i', d
	local mean=`r(mean)'
	local med=`r(p50)'
	local n = r(N)
	local c975=`n'*.975
	local c025=`n'*.025

	sort sum_wtp_bsb_keep_`i'
	
	*sort the WTPs and drop the lower and upper 2.5% to get the 95% CIs
	gen id=_n
	drop if id>`c975' | id<`c025'
	
	su sum_wtp_bsb_keep_`i'
	local max = `r(max)'
	local min=`r(min)'
	
	clear
	set obs 1
	
	gen wtp_mean= `mean'
	gen wtp_med= `med'
	gen upper = `max'
	gen lower = `min'
	gen wtp_n_secondary = `i'

	tempfile wtp`i'
	save `wtp`i'', replace
	global wtp "$wtp "`wtp`i''" " 

}	

clear
dsconcat $wtp


gen spread=upper-lower

global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)
twoway rcap upper lower wtp_n_secondary || scatter wtp_med wtp_n_secondary, $graphoptions
gr play wtp_fluke