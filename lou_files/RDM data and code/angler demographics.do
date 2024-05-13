
*demographics from 2019 durables
*Received from Sarbina Lovell 4/5/22
/*
The attached pdf shows some of the details and frequencies.
I did not put the income and education into the excel spreadsheet, but you can see them on the pdf.
The order of the results is first Region 
1. Me to NY
2. NJ
3. DE/MD
4. VA/NC

*/
global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
import excel using "population ages.xlsx", clear first
gen round_freq=round(wtd_freq)
expand round_freq

levelsof region, local(regs)
foreach r of local regs{
	su age if region=="`r'"
	local mean=round(`r(mean)', .01) 
	
	hist age if region=="`r'", $graphoptions title(`r', size(small)) xtitle("Age", yoffset(-2) size(small)) ytitle(, xoffset(-2)) ylab(, angle(horizontal) labsize(small)) xlab(, angle(horizontal) labsize(small)) name(gr`r', replace) note("average age = `mean'", yoffset(-2)) bin(50)

}
gr combine grMENY grNJ grDEMD grVANC, xcommon ycommon 
gr combine grMENY , xcommon ycommon 

*/

drop freq wtd se* perc round

expand 2 if region=="MENY", gen(dup)
gen state="MA" if region=="MENY" & dup==0
replace state="RI" if region=="MENY" & dup==1
drop dup

expand 2 if state=="MA", gen(dup)
replace state="CT" if state=="MA" & dup==1
drop dup

expand 2 if state=="CT", gen(dup)
replace state="NY" if state=="CT" & dup==1
drop dup

expand 2 if region=="DEMD", gen(dup)
replace state="DE" if region=="DEMD" & dup==0
replace state="MD" if region=="DEMD" & dup==1
drop dup

expand 2 if region=="VANC", gen(dup)
replace state="VA" if region=="VANC" & dup==0
replace state="NC" if region=="VANC" & dup==1
drop dup

replace state="NJ" if region=="NJ"
drop region 
order state age
sort state age 

save "age_distribution_by_state.dta", replace 

export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\age_distribution_by_state.csv", replace


cd "C:\Users\andrew.carr-harris\Dropbox\NMFS\HCR user interface"
import excel using "population avidity.xlsx", clear first
gen round_freq=round(wtd_freq)
expand round_freq


levelsof region, local(regs)
foreach r of local regs{
	su days if region=="`r'"
	local mean=round(`r(mean)', .01) 
	
	hist days if region=="`r'", $graphoptions title(`r', size(small)) xtitle("Days fished past 12 months", yoffset(-2) size(small)) ytitle(, xoffset(-2)) ylab(, angle(horizontal) labsize(small)) xlab(, angle(horizontal) labsize(small)) name(gr`r', replace) note("average days = `mean'", yoffset(-2)) bin(50)

}
gr combine grMENY grNJ grDEMD grVANC, xcommon ycommon 
*/

drop freq wtd se*  round

expand 2 if region=="MENY", gen(dup)
gen state="MA" if region=="MENY" & dup==0
replace state="RI" if region=="MENY" & dup==1
drop dup

expand 2 if state=="MA", gen(dup)
replace state="CT" if state=="MA" & dup==1
drop dup

expand 2 if state=="CT", gen(dup)
replace state="NY" if state=="CT" & dup==1
drop dup

expand 2 if region=="DEMD", gen(dup)
replace state="DE" if region=="DEMD" & dup==0
replace state="MD" if region=="DEMD" & dup==1
drop dup

expand 2 if region=="VANC", gen(dup)
replace state="VA" if region=="VANC" & dup==0
replace state="NC" if region=="VANC" & dup==1
drop dup

replace state="NJ" if region=="NJ"
drop region 
order state days
sort state days
*hist days

save "avidity_distribution_by_state.dta", replace 
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\avidity_distribution_by_state.csv"
