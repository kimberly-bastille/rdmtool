/* scott stores his MRIP data in "/win/net/data4/ssteinba/MRIP" */
/* all mrip files are also sotred and updated on the network \\net\mrfss\products\mrip_estim\Public_data_cal2018 */
*format MRIP stata files 
*cd "C:\Users\andrew.carr-harris\Desktop\Fluke_MSE\MRIP_data"

*cd "C:\Users\Lou\Desktop\MRIP_data"

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"


/*foreach wave in 20171 20172 20173 20174 20175 20176 ///
						20181 20182 20183 20184 20185 20186 ///
						20191 20192 20193 20194 20195 20196 ///
						20201 20202 20203 20204 20205 20206 ///
						20211 20212 20213 20214 20215 20216 ///
						20221 20222 20223 20224 20225 20226  /// */
	foreach wave in	20231 20232 20233 20234 {	///
/*	  19822 19823 19824 19825 19826 ///
						19831 19832 19833 19834 19835 19836 ///
						19841 19842 19843 19844 19845 19846 ///
						19851 19852 19853 19854 19855 19856 ///
						19861 19862 19863 19864 19865 19866 ///
						19871 19872 19873 19874 19875 19876 ///					
						19881 19882 19883 19884 19885 19886 ///						
						19891 19892 19893 19894 19895 19896 ///						
						19901 19902 19903 19904 19905 19906 ///						
						19911 19912 19913 19914 19915 19916 ///						
						19921 19922 19923 19924 19925 19926 ///
						19931 19932 19933 19934 19935 19936 ///						
						19941 19942 19943 19944 19945 19946 ///						
						19951 19952 19953 19954 19955 19956 ///
						19961 19962 19963 19964 19965 19966 ///						
						19971 19972 19973 19974 19975 19976 ///						
						19981 19982 19983 19984 19985 19986 ///
						19991 19992 19993 19994 19995 19996 ///						
						20001 20002 20003 20004 20005 20006 ///
						20011 20012 20013 20014 20015 20016 ///
						20021 20022 20023 20024 20025 20026 ///
						20031 20032 20033 20034 20035 20036 /// 
						20041 20042 20043 20044 20045 20046 ///
						20051 20052 20053 20054 20055 20056 ///
						20061 20062 20063 20064 20065 20066 /// 
						20071 20072 20073 20074 20075 20076 ///
						20081 20082 20083 20084 20085 20086 ///
						20091 20092 20093 20094 20095 20096 ///
						20101 20102 20103 20104 20105 20106 ///
						20111 20112 20113 20114 20115 20116 ///
						20121 20122 20123 20124 20125 20126 /// 
						20131 20132 20133 20134 20135 20136 ///
						20141 20142 20143 20144 20145 20146 ///
						20151 20152 20153 20154 20155 20156 /// 
						20161 20162 20163 20164 20165 20166 ///
						20171 20172 20173 20174 20175 20176 ///
						20181 20182 20183 20184 20185 20186 /// 
						20191 20192 20193 20194 20195 20196 ///
						20201 20202 20203 20204 20205 20206 ///
						*/
						*20211 20212 20213 20214 20215 20216{
						

capture confirm file  "trip_`wave'.dta"
if _rc==0{
	use trip_`wave'.dta, clear
	renvarlab, lower
	save trip_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_b2_`wave'.dta"
if _rc==0{
	use size_b2_`wave'.dta, clear
	renvarlab, lower
	save size_b2_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_`wave'.dta"
if _rc==0{
	use size_`wave'.dta, clear
	renvarlab, lower
	save size_`wave'.dta, replace
}

else{
	
}

capture confirm file "catch_`wave'.dta"
if _rc==0{
	use catch_`wave'.dta, clear
	renvarlab, lower
	save catch_`wave'.dta, replace
}

else{
	
}

}


cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

set matsize 10000
/* Use these to control the years for which the MRIP data is polled/queried*/

global yearlist  1982 1983 1984 1985 1986 1987 1988 1989 ///
					   1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 ///
					   2000 2001 2002 2003 2004 2005 2006 2007 2008 2009  ///
					   2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 
global yearlist  2017 2018 2019 2020 

global yearlist  2021 2022 2023 
global wavelist 1 2 3 4 5 6


/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}



/*SIZE_LIST */
global sizelist
foreach year of global yearlist{
	foreach wave of global wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta" " 
		}
		else{
		}
	}
	else{
	}
	
}
}








/*

/* catch totals  -- these are done for all 3 years at once*/


do "/home/mlee/Documents/Workspace/MRIP_working/domain_cod_catch_totals.do"


use "/home/mlee/Documents/Workspace/MRIP_working/atlanticcod_catch.dta", clear
keep year wave tot_cat claim harvest release
rename claim a
rename harvest b1
rename release b2
rename tot_cat tot_catch
gen landings=a+b1
save "/home/mlee/Documents/Workspace/MRIP_working/cod_landings_`yr1'_`yre'.dta", replace



do "/home/mlee/Documents/Workspace/MRIP_working/domain_haddock_catch_totals.do"





use "/home/mlee/Documents/Workspace/MRIP_working/haddock_catch.dta", clear
keep year wave tot_cat claim harvest release
rename claim a
rename harvest b1
rename release b2
rename tot_cat tot_catch
gen landings=a+b1
save "/home/mlee/Documents/Workspace/MRIP_working/haddock_landings_`yr1'_`yre'.dta", replace




/* catch frequencies per trip*/

global cod_wave_files

foreach year of global yearlist{
	global myyear=`year'
	global my_common "atlanticcod"

	do "/home/mlee/Documents/Workspace/MRIP_working/domain_catch_frequencies_gom_cod_wave.do"


}


global haddock_wave_files
foreach year of global yearlist{
	global myyear=`year'
	global my_common "haddock"
	do "/home/mlee/Documents/Workspace/MRIP_working/domain_catch_frequencies_gom_haddock_wave.do"
}






/*stack these into a single dataset */

clear
dsconcat $cod_wave_files 
rename tot_cat num_fish
rename count count_trips
sort year wave num_fish
save "/home/mlee/Documents/Workspace/MRIP_working/cod_catch_class_`yr1'_`yre'.dta", replace


clear
dsconcat $haddock_wave_files 
rename tot_cat num_fish
rename count count_trips
sort year wave num_fish
save "/home/mlee/Documents/Workspace/MRIP_working/haddock_catch_class_`yr1'_`yre'.dta", replace




clear

/* length frequencies */


global cod_wave_lengths

foreach year of global yearlist{
	global myyear=`year'
	global my_common "atlanticcod"
	do "/home/mlee/Documents/Workspace/MRIP_working/cod_length_freqs_by_wave_gom.do"
}



clear
dsconcat $cod_wave_lengths
save "/home/mlee/Documents/Workspace/MRIP_working/cod_ab1.dta", replace


global haddock_wave_lengths

foreach year of global yearlist{
	global myyear=`year'
	global my_common "haddock"

	do "/home/mlee/Documents/Workspace/MRIP_working/haddock_length_freqs_by_wave_gom.do"


}




clear
dsconcat $haddock_wave_lengths
save "/home/mlee/Documents/Workspace/MRIP_working/haddock_ab1.dta", replace

/* B2 length frequencies per wave*/
global cod_wave_b2

foreach year of global yearlist{
	global myyear=`year'
	global my_common "atlanticcod"
	do "/home/mlee/Documents/Workspace/MRIP_working/b2cod_length_freqs_by_wave_gom.do"
}




/*stack these into a single dataset */
clear

dsconcat $cod_wave_b2
save "/home/mlee/Documents/Workspace/MRIP_working/cod_b2.dta", replace

/* B2 length frequencies per wave for haddock*/


global haddock_wave_b2

foreach year of global yearlist{
	global myyear=`year'
	global my_common "haddock"
	do "/home/mlee/Documents/Workspace/MRIP_working/b2haddock_length_freqs_by_wave_gom.do"
}


dsconcat $haddock_wave_b2
save "/home/mlee/Documents/Workspace/MRIP_working/haddock_b2.dta", replace


/* join the b2 length data with the total number of released to get the length distribution for the number of fish released */
do "/home/mlee/Documents/Workspace/MRIP_working/process_b2_cod.do"
do "/home/mlee/Documents/Workspace/MRIP_working/process_b2_haddock.do"



/* I NEED TO EDIT THIS, read scotts email */
/* caught/targeted haddock or caught cod by wave */
do "/home/mlee/Documents/Workspace/MRIP_working/haddock_plus_directed_trips_by_wave.do"



/* caught/targeted cod or haddock by wave */
do "/home/mlee/Documents/Workspace/MRIP_working/cod_haddock_directed_trips_by_wave.do"

/* caught/targeted cod by wave */
do "/home/mlee/Documents/Workspace/MRIP_working/cod_directed_trips_by_wave.do"


/* caught/targeted haddock by wave */
do "/home/mlee/Documents/Workspace/MRIP_working/haddock_directed_trips_by_wave.do"
/* END I NEED TO EDIT THIS, read scotts email */


/* START COMMENTED OUT


/*This grabs just the FY2013 data */
/* You have to manually copy these 4 files into the appropriate folder:

Copy over the FY2013 data:
*/
use "/home/mlee/Documents/Workspace/MRIP_working/cod_catch_class_`yr1'_`yre'.dta", clear
keep if year>=2013
drop if year==2013 & wave<=2
replace year=2013 if year==2014 & wave<=2
keep if  year==2013
sort wave num_fish
drop year
save "/home/mlee/Documents/Workspace/MRIP_working/cod_catch_class_fy2013.dta", replace
/*
save "/home/mlee/Documents/Workspace/recreational_simulations/cod_and_haddock_GARFO/source_data/cod_catch_class_fy2013.dta", replace
*/



use "/home/mlee/Documents/Workspace/MRIP_working/haddock_catch_class_`yr1'_`yre'.dta", clear
keep if year>=2013
drop if year==2013 & wave<=2
replace year=2013 if year==2014 & wave<=2
keep if  year==2013
sort wave num_fish

drop year
save "/home/mlee/Documents/Workspace/MRIP_working/haddock_catch_class_fy2013.dta", replace
/*
save "/home/mlee/Documents/Workspace/recreational_simulations/cod_and_haddock_GARFO/source_data/haddock_catch_class_fy2013.dta", replace
*/


use "/home/mlee/Documents/Workspace/MRIP_working/cod_size_class_`yr1'_`yre'.dta", clear

replace count=330 if year==2013 & wave==3 & lngcat==30 & count>=5000

keep if year>=2013
drop if year==2013 & wave<=2
replace year=2013 if year==2014 & wave<=2
keep if  year==2013
sort wave lngcat
drop if lngcat==0
drop year
save "/home/mlee/Documents/Workspace/MRIP_working/cod_size_class_fy2013.dta", replace
/*
save "/home/mlee/Documents/Workspace/recreational_simulations/cod_and_haddock_GARFO/source_data/cod_size_class_fy2013.dta", replace*/


use "/home/mlee/Documents/Workspace/MRIP_working/haddock_size_class_`yr1'_`yre'.dta", clear
keep if year>=2013
drop if year==2013 & wave<=2
replace year=2013 if year==2014 & wave<=2
keep if  year==2013
sort wave lngcat
drop if lngcat==0

drop year
save "/home/mlee/Documents/Workspace/MRIP_working/haddock_size_class_fy2013.dta", replace
*/
