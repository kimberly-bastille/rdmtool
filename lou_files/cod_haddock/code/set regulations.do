


****Set regulations for the calibration period and the projection period****
*These need to be changed every year 



/*FY 2024 model
*************************
*Create the baseline regulations for the calibration period here
gen cod_bag=0 
gen cod_min=100

gen hadd_bag=0
gen hadd_min=100


*Hadd regs - calibration period, which covers "(year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))" 
*FY 2022
replace hadd_bag=20 if  day>=td(01may2022) & day<=td(28feb2023)
replace hadd_min=17 if  day>=td(01may2022)  & day<=td(28feb2023)

replace hadd_bag=20 if  day>=td(01apr2023) & day<=td(30apr2023)
replace hadd_min=17 if  day>=td(01apr2023) & day<=td(30apr2023)

*FY 2023: The Council proposed an 18-inch minimum size and 15 fish limit for both the for-hire and private angler sector. NMFS implemented split measures out of concerns that an 18-inch minimum would unnecessarily constrain catch and increase dead discards in the private angler sector. This rule became effective August 14, 2023. Split measures- For hire: 18", 15 bag; private 17", 10 bag

*original measures
replace hadd_bag=15 if  day>=td(01may2023) & day<=td(02feb2024) 
replace hadd_min=18 if  day>=td(01may2023) & day<=td(02feb2024)  

*split measures effective August 14, 2023
replace hadd_bag=15 if  day>=td(14aug2023) & day<=td(02feb2024) & inlist(mode, "fh")
replace hadd_min=18 if  day>=td(14aug2023) & day<=td(02feb2024)  & inlist(mode, "fh")

replace hadd_bag=10 if  day>=td(14aug2023) & day<=td(02feb2024) & inlist(mode, "pr", "sh")
replace hadd_min=17 if  day>=td(14aug2023) & day<=td(02feb2024)  & inlist(mode, "pr", "sh")


*Cod regs FY2022 and FY 2023
replace cod_bag=1 if  day>=td(01sep2022) & day<=td(07oct2022)
replace cod_min=22 if  day>=td(01sep2022) & day<=td(07oct2022)

replace cod_bag=1 if  day>=td(01apr2023) & day<=td(14apr2023)
replace cod_min=22 if  day>=td(01apr2023) & day<=td(14apr2023)

replace cod_bag=1 if  day>=td(01sep2023) & day<=td(31oct2023)
replace cod_min=22 if  day>=td(01sep2023) & day<=td(31oct2023)

*************************


tempfile regulations
save `regulations', replace 


*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full

gen day1=day(day_y2)
gen month1=month(day_y2)
gen year_y2=year(day_y2)
drop if day_y2==td(29feb2024)
gen dow_y2 = dow(day_y2)  

gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		
replace kod_y2="we" if $fed_holidays_y2

gen month2_y2= string(month1,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup


merge 1:m  mode day1 month1 using `regulations'
drop _merge 
order year mode month kod dow day  draw cod_bag cod_min hadd_bag hadd_min day_y2 dow_y2 kod_y2 month_y2
sort  mode day draw





*************************
*Create status-quo regualtions for projection period here: td(01may2024)- td(30apr2025)
gen cod_bag_y2=0 
gen cod_min_y2=100

gen hadd_bag_y2=0
gen hadd_min_y2=100

*Haddock
replace hadd_bag_y2=15 if  day_y2>=td(01may2024) & day_y2<=td(02feb2025) & inlist(mode, "fh")
replace hadd_min_y2=18 if  day_y2>=td(01may2024) & day_y2<=td(02feb2025)  & inlist(mode, "fh")

replace hadd_bag_y2=10 if  day_y2>=td(01may2024) & day_y2<=td(02feb2025) & inlist(mode, "pr", "sh")
replace hadd_min_y2=17 if  day_y2>=td(01may2024) & day_y2<=td(02feb2025)  & inlist(mode, "pr", "sh")

*Cod 
replace cod_bag_y2=1 if  day_y2>=td(01sep2024) & day_y2<=td(07oct2024)
replace cod_min_y2=22 if  day_y2>=td(01sep2024) & day_y2<=td(07oct2024)

replace cod_bag_y2=1 if  day_y2>=td(01apr2025) & day_y2<=td(14apr2025)
replace cod_min_y2=22 if  day_y2>=td(01apr2025) & day_y2<=td(14apr2025)

*************************
*/


*FY 2025 model
*************************
*Create the baseline regulations for the calibration period, 
*which covers (year==2024 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2023 & inlist(wave, 6)). So FY 2023 regs until May 1, 2024, FY2024 regs after May 1, 2024
gen cod_bag=0 
gen cod_min=100

gen hadd_bag=0
gen hadd_min=100


*Hadd regs 
*FY 2023: The Council proposed an 18-inch minimum size and 15 fish limit for both the for-hire and private angler sector. NMFS implemented split measures out of concerns that an 18-inch minimum would unnecessarily constrain catch and increase dead discards in the private angler sector. This rule became effective August 14, 2023. Split measures- For hire: 18", 15 bag; private 17", 10 bag
*split measures effective August 14, 2023
replace hadd_bag=15 if  day>=td(14aug2023) & day<=td(28feb2024) & inlist(mode, "fh")
replace hadd_min=18 if  day>=td(14aug2023) & day<=td(28feb2024)  & inlist(mode, "fh")

replace hadd_bag=10 if  day>=td(14aug2023) & day<=td(28feb2024) & inlist(mode, "pr", "sh")
replace hadd_min=17 if  day>=td(14aug2023) & day<=td(28feb2024)  & inlist(mode, "pr", "sh")

replace hadd_bag=15 if  day>=td(01apr2024) & day<=td(30apr2024) & inlist(mode, "fh")
replace hadd_min=18 if  day>=td(01apr2024) & day<=td(30apr2024)  & inlist(mode, "fh")

replace hadd_bag=10 if  day>=td(01apr2024) & day<=td(30apr2024) & inlist(mode, "pr", "sh")
replace hadd_min=17 if  day>=td(01apr2024) & day<=td(30apr2024)  & inlist(mode, "pr", "sh")

replace hadd_bag=15 if  day>=td(01may2024) & day<=td(28feb2025) 
replace hadd_min=18 if  day>=td(01may2024) & day<=td(28feb2025)  


*Cod regs 
replace cod_bag=1 if  day>=td(01sep2023) & day<=td(31oct2023)
replace cod_min=22 if  day>=td(01sep2023) & day<=td(31oct2023)

replace cod_bag=1 if  day>=td(01sep2024) & day<=td(31oct2024)
replace cod_min=23 if  day>=td(01sep2024) & day<=td(31oct2024)

*************************

tempfile regulations
save `regulations', replace 

*now merge to this file the calender for y+1 (_y2)
clear 
set obs 2
gen day_y2=$projection_date_start if _n==1
replace day_y2=$projection_date_end if _n==2
format day_y2 %td
tsset day_y2
tsfill, full

gen day1=day(day_y2)
gen month1=month(day_y2)
gen year_y2=year(day_y2)
drop if day_y2==$leap_yr_days
gen dow_y2 = dow(day_y2)  

gen kod_y2="we" if inlist(dow, 5, 6, 0)
replace kod_y2="wd" if inlist(dow, 1, 2, 3, 4)		
replace kod_y2="we" if $fed_holidays_y2

gen month2_y2= string(month1,"%02.0f")
rename month2_y2 month_y2
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup


merge 1:m  mode day1 month1 using `regulations'
drop if day==$leap_yr_days
drop _merge 
order year mode month kod dow day  draw cod_bag cod_min hadd_bag hadd_min day_y2 dow_y2 kod_y2 month_y2
sort  mode day draw





*************************
*Create status-quo regualtions for projection period here: 01may2025  -  td(30apr2026)
gen cod_bag_y2=0 
gen cod_min_y2=100

gen hadd_bag_y2=0
gen hadd_min_y2=100

*Haddock
replace hadd_bag_y2=15 if day_y2>=td(01may2025) & day_y2<=td(28feb2026) 
replace hadd_min_y2=18 if day_y2>=td(01may2025) & day_y2<=td(28feb2026) 

replace hadd_bag_y2=15 if day_y2>=td(01apr2026) & day_y2<=td(30apr2026) 
replace hadd_min_y2=18 if day_y2>=td(01apr2026) & day_y2<=td(30apr2026) 

*Cod 
replace cod_bag_y2=1 if  day_y2>=td(01sep2025) & day_y2<=td(31oct2025)
replace cod_min_y2=23 if  day_y2>=td(01sep2025) & day_y2<=td(31oct2025)

*************************


*translate to inches
replace cod_min = cod_min*2.54
replace hadd_min = hadd_min*2.54
replace cod_min_y2 = cod_min_y2*2.54
replace hadd_min_y2 = hadd_min_y2*2.54


