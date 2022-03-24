********************************
*Scholes and Bann.
*Educational-related disparities in reported physical activity during leisure-time.
*active transportation and work among US adults: repeated cross-sectional analysis from the 
*National Health and Nutrition Examination Surveys, 2007 to 2016.
*BMC Public Health: 2018: 18:926.
********************************

clear
cd "C:\Learning\Papers\Scholes and Bann 2018\Datasets"

*Demographics(2007-08).
import sasxport5 "DEMO_E.XPT", clear
label variable seqn "Respondent sequence number"
sort seqn
count
svyset [pweight=wtint2yr]
keep if (ridageyr>=20)
rename riagendr sex
rename ridageyr age
keep if inrange(ridstatr,1,2)
keep if (age>=20)
generate age3=0
replace age3=1 if inrange(age,20,39)
replace age3=2 if inrange(age,40,59)
replace age3=3 if inrange(age,60,89)
label define dmdeduc2lbl 1 "Less than 9th grade" ///
2 "9-11th grade" 3 "High school graduate" 4 "Some college or AA degree" 5 "College graduate" 7 "Refused" 9 "DK"
label values dmdeduc2  dmdeduc2lbl 
generate year=1
keep seqn year sex age age3 wtint2yr wtmec2yr sdmvpsu sdmvstra ridexprg dmdeduc2 ridreth1
sort seqn
save "Temp1.dta", replace

* PA(2007-2008).
import sasxport5 "PAQ_E.XPT", clear
sort seqn
merge 1:1 seqn using "Temp1.dta"
keep if _merge==3
drop _merge
sort seqn
*Valid SEP (N=5928).
*keep if inrange(dmdeduc2,1,5)
keep seqn year sex age ridreth1 wtint2yr sdmvpsu sdmvstra ///
paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650 paq655 pad660 paq665 paq670 pad675 dmdeduc2
save "TempPA_E.XPT.dta", replace

*2009-2010.
import sasxport5 "DEMO_F.xpt", clear
label variable seqn "Respondent sequence number"
sort seqn
svyset [pweight=wtint2yr]
keep if (ridageyr>=20)
rename riagendr sex
rename ridageyr age
keep if inrange(ridstatr,1,2)
keep if (age>=20)
label define dmdeduc2lbl 1 "Less than 9th grade" ///
2 "9-11th grade" 3 "High school graduate" 4 "Some college or AA degree" 5 "College graduate" 7 "Refused" 9 "DK"
label values dmdeduc2  dmdeduc2lbl 
generate year=2
keep seqn year sex age ridreth1 wtint2yr wtmec2yr sdmvpsu sdmvstra ridexprg dmdeduc2
sort seqn
save "Temp1.dta", replace

* PA(2009-2010).
import sasxport5 "PAQ_F.XPT", clear
sort seqn
merge 1:1 seqn using "Temp1.dta"
keep if _merge==3
drop _merge
sort seqn
*Valid SEP (N=6203).
*keep if inrange(dmdeduc2,1,5)
count
keep seqn year sex age ridreth1 wtint2yr sdmvpsu sdmvstra ///
paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650 paq655 pad660 paq665 paq670 pad675 dmdeduc2
save "TempPA_F.XPT.dta", replace

*2011-2012.
import sasxport5 "DEMO_G.xpt", clear
label variable seqn "Respondent sequence number"
sort seqn
svyset [pweight=wtint2yr]
keep if (ridageyr>=20)
rename ridageyr age
rename riagendr sex
keep if inrange(ridstatr,1,2)
keep if (age>=20)
label define dmdeduc2lbl 1 "Less than 9th grade" ///
2 "9-11th grade" 3 "High school graduate" 4 "Some college or AA degree" 5 "College graduate" 7 "Refused" 9 "DK"
label values dmdeduc2  dmdeduc2lbl 
generate year=3
keep seqn year sex age ridreth1 wtint2yr wtmec2yr sdmvpsu sdmvstra ridexprg dmdeduc2
sort seqn
save "Temp1.dta", replace

* PA(2011-2012).
import sasxport5 "PAQ_G.XPT", clear
sort seqn
merge 1:1 seqn using "Temp1.dta"
keep if _merge==3
drop _merge
sort seqn
* Valid SEP (N=5555).
*keep if inrange(dmdeduc2,1,5)
keep seqn year sex age ridreth1 wtint2yr sdmvpsu sdmvstra ///
paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650 paq655 pad660 paq665 paq670 pad675 dmdeduc2
save "TempPA_G.XPT.dta", replace


*2013-2014.
import sasxport5 "DEMO_H.xpt", clear
label variable seqn "Respondent sequence number"
sort seqn
svyset [pweight=wtint2yr]
keep if (ridageyr>=20)
rename riagendr sex
rename ridageyr age
keep if inrange(ridstatr,1,2)
keep if (age>=20)
label define dmdeduc2lbl 1 "Less than 9th grade" ///
2 "9-11th grade" 3 "High school graduate" 4 "Some college or AA degree" 5 "College graduate" 7 "Refused" 9 "DK"
label values dmdeduc2  dmdeduc2lbl 
generate year=4
keep seqn year sex age ridreth1 wtint2yr wtmec2yr sdmvpsu sdmvstra ridexprg dmdeduc2
sort seqn
save "Temp1.dta", replace

*PA(2013-2014).
import sasxport5 "PAQ_H.XPT", clear
sort seqn
merge 1:1 seqn using "Temp1.dta"
keep if _merge==3
drop _merge
sort seqn
*Valid SEP (N=5762).
*keep if inrange(dmdeduc2,1,5)
keep seqn year sex age ridreth1 wtint2yr sdmvpsu sdmvstra ///
paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650 paq655 pad660 paq665 paq670 pad675 dmdeduc2
save "TempPA_H.XPT.dta", replace


*2015-16.
import sasxport5 "DEMO_I.xpt", clear
label variable seqn "Respondent sequence number"
sort seqn
svyset [pweight=wtint2yr]
keep if (ridageyr>=20)
rename riagendr sex
rename ridageyr age
keep if inrange(ridstatr,1,2)
keep if (age>=20)
label define dmdeduc2lbl 1 "Less than 9th grade" ///
2 "9-11th grade" 3 "High school graduate" 4 "Some college or AA degree" 5 "College graduate" 7 "Refused" 9 "DK"
label values dmdeduc2  dmdeduc2lbl 
generate year=5
keep seqn year sex age ridreth1 wtint2yr wtmec2yr sdmvpsu sdmvstra ridexprg dmdeduc2
sort seqn
save "Temp1.dta", replace

* PA(2015-2016).
import sasxport5 "PAQ_I.XPT", clear
sort seqn
merge 1:1 seqn using "Temp1.dta"
keep if _merge==3
drop _merge
sort seqn
* Valid SEP (N=5555).
*keep if inrange(dmdeduc2,1,5)
keep seqn year sex age ridreth1 wtint2yr sdmvpsu sdmvstra ///
paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650 paq655 pad660 paq665 paq670 pad675 dmdeduc2
save "TempPA_I.XPT.dta", replace

*Put the datasets together.

use "TempPA_E.XPT.dta", clear
append using "TempPA_F.XPT.dta"
append using "TempPA_G.XPT.dta"
append using "TempPA_H.XPT.dta"
append using "TempPA_I.XPT.dta"

generate age3=0
replace age3=1 if inrange(age,20,39)
replace age3=2 if inrange(age,40,59)
replace age3=3 if inrange(age,60,89)

generate SES = -1
replace SES = 1 if inrange(dmdeduc2,1,2)
replace SES = 2 if dmdeduc2==3
replace SES = 3 if dmdeduc2==4
replace SES = 4 if dmdeduc2==5
label define SESlbl 1 "lowest" 2 "2" 3 "3" 4 "highest"
label values SES SESlbl

keep if inrange(SES,1,4)

label define ethniclbl 1 "Mexican American" 2 "Other Hispanic" 3 "Non-Hispanic White" ///
4 "Non-Hispanic Black" 5 "Other race"
label values ridreth1 ethniclbl

generate ETHNIC = 0
replace ETHNIC = 1 if inrange(ridreth1,1,2)
replace ETHNIC = 2 if ridreth1==3
replace ETHNIC = 3 if ridreth1==4
replace ETHNIC = 4 if ridreth1==5

label define ethnic2lbl 1 "Mexican and other Hispanic" 2 "Non-Hispanic White"  ///
3 "Non-Hispanic Black" 4 "Other"
label values ETHNIC ethnic2lbl
tab1 ETHNIC
recode ETHNIC (2=1) (1=2) (3=3) (4=4)

label define ethnic2lbl 2 "Mexican and other Hispanic" 1 "White"  ///
3 "Non-Hispanic Black" 4 "Other", replace
label values ETHNIC ethnic2lbl
tab1 ETHNIC


*summ paq605 paq610 pad615 paq620 paq625 pad630 paq635 paq640 pad645 paq650 paq655 pad660 paq665 paq670 pad675

********************************************
*Vigorous (occupational): (days * minutes).
********************************************

mvdecode paq605,mv(7,9)      /* binary */
mvdecode paq610,mv(77,99)    /* days */
mvdecode pad615,mv(7777,9999)  /* minutes */

generate VigWorkTime=-1
replace VigWorkTime=0 if (paq605==2)
replace VigWorkTime=(paq610 * pad615) if (paq605==1) & inrange(paq610,1,7) & inrange(pad615,10,1080)
label variable VigWorkTime "Vigorous work (Min/week)"
mvdecode VigWorkTime,mv(-1)

**********************************************
* Moderate (occupational): (days * minutes).
**********************************************

mvdecode paq620,mv(7,9)
mvdecode paq625,mv(77,99)
mvdecode pad630,mv(7777,9999)

generate ModWorkTime=-1
replace ModWorkTime=0 if (paq620==2)
replace ModWorkTime=(paq625 * pad630) if (paq620==1) & inrange(paq625,1,7) & inrange(pad630,10,1440)
label variable ModWorkTime "Moderate work (Min/week)"
mvdecode ModWorkTime,mv(-1)

**********************************************
* Transport.
**********************************************

mvdecode paq635,mv(7,9)
mvdecode paq640,mv(77,99)
mvdecode pad645,mv(7777,9999)

generate TravelTime=-1
replace TravelTime=0 if (paq635==2)
replace TravelTime=(paq640 * pad645) if (paq635==1) & inrange(paq640,1,7) & inrange(pad645,10,960)
label variable TravelTime "Walking/bicycling (Min/week)"
mvdecode TravelTime,mv(-1)

**********************************************
**Vigorous recreational.
**********************************************

mvdecode paq650,mv(7,9)
mvdecode paq655,mv(77,99)
mvdecode pad660,mv(7777,9999)

generate VigSportsTime=-1
replace VigSportsTime=0 if (paq650==2)
replace VigSportsTime=(paq655 * pad660) if (paq650==1) & inrange(paq655,1,7) & inrange(pad660,10,990)
label variable VigSportsTime "Vigorous recreational (Min/week)"

mvdecode VigSportsTime,mv(-1)

**********************************************
**Moderate (recreational).
**********************************************

mvdecode paq665,mv(7,9)
mvdecode paq670,mv(77,99)
mvdecode pad675,mv(7777,9999)

generate ModSportsTime=-1
replace ModSportsTime=0 if (paq665==2)
replace ModSportsTime=(paq670 * pad675) if (paq665==1) & inrange(paq670,1,7) & inrange(pad675,10,900)
label variable ModSportsTime "Moderate recreational (Min/week)"
mvdecode ModSportsTime,mv(-1)


****************
*Summary.
****************

generate base=0
replace base=1 if (VigWorkTime!=.) & (ModWorkTime!=.) & (TravelTime!=.) & (VigSportsTime!=.) & (ModSportsTime!=.)
tab1 base

summ VigWorkTime ModWorkTime TravelTime VigSportsTime ModSportsTime if base==1

*600METs as threshold
generate Ptotal1 = (VigWorkTime * 8) + (ModWorkTime * 4) + (TravelTime * 4) ///
+ (VigSportsTime * 8) + (ModSportsTime * 4)  if base==1
generate a1 = (VigWorkTime * 8) + (ModWorkTime * 4) if base==1
generate b1 = (TravelTime * 4) if base==1 
generate c1 = (VigSportsTime * 8) + (ModSportsTime * 4)  if base==1
generate work150a=0
replace work150a=1 if a1>=600
generate transa=0
replace transa=1 if b1>=600
generate recsa=0
replace recsa=1 if c1>=600
generate MeetsRecs1=0
replace MeetsRecs1=1 if Ptotal1>=600


*********************************
*Multiply vigorous by two
*150m/week as threshold
********************************

generate Ptotal2 = (VigWorkTime * 2) + (ModWorkTime) + (TravelTime) + (VigSportsTime * 2) + (ModSportsTime)  if base==1
generate a2 = (VigWorkTime * 2) + (ModWorkTime) if base==1 
generate b2 = (TravelTime) if base==1 
generate c2 = (VigSportsTime * 2) + (ModSportsTime)  if base==1

**********************************************
*Binary variables for sensitivity analysis.
**********************************************

generate work150b=0
replace work150b=1 if a2>=150
generate transb=0
replace transb=1 if b2>=150
generate recsb=0
replace recsb=1 if c2>=150
keep if (base==1)
generate MeetsRecs2=0
replace MeetsRecs2=1 if Ptotal2>=150
generate MeetsRecs2a=0
replace MeetsRecs2a=1 if Ptotal2>=60
generate MeetsRecs2b=0
replace MeetsRecs2b=1 if Ptotal2>=90
generate MeetsRecs2c=0
replace MeetsRecs2c=1 if Ptotal2>=120
generate MeetsRecs2d=0
replace MeetsRecs2d=1 if Ptotal2>=150

svyset [pweight=wtint2yr],psu(sdmvpsu) strata(sdmvstra)

**************
*Analysis.
**************

count /* N=29039 */

***************************************************
* Additional File 1.
* Educational attainment by demographic sub-group.
***************************************************

tab1 SES
tab2 age3 SES
tab2 sex SES
tab2 ETHNIC SES

svy:tab SES 
svy:tab age3 SES, row
svy:tab sex SES, row
svy:tab ETHNIC SES, row


***************************************************
* Additional File 2.
* Bivariate analysis: subgroups and active levels.
***************************************************

*Overall MVPA.
preserve
replace MeetsRecs1=MeetsRecs1*100 
svy:mean MeetsRecs1
svy:mean MeetsRecs1,over(age3)
svy:mean MeetsRecs1,over(sex)
svy:mean MeetsRecs1,over(ETHNIC)
*svy:mean MeetsRecs1,over(SES)
svy:tab age3 MeetsRecs1, row
svy:tab sex MeetsRecs1, row
svy:tab ETHNIC MeetsRecs1, row
*svy:tab SES MeetsRecs1, row
restore

*Leisure-Time.
*How many zeros?
preserve
summ c2 
summ c2 if c2==0
summ recsa if c2>=150
replace recsa=recsa*100 /*Leisure-Time */
svy:mean recsa
svy:mean recsa,over(age3)
svy:mean recsa,over(sex)
svy:mean recsa,over(ETHNIC)
*svy:mean recsa,over(SES)
svy:tab age3 recsa, row
svy:tab sex recsa, row
svy:tab ETHNIC recsa, row
*svy:tab SES recsa, row
restore
*di 15518/29039   /*53% did no Leisure-Time */
 
*Transport.
preserve
summ b2 
summ b2 if b2==0
summ transa if b2>=150
replace transa=transa*100 /*Transportation */
svy:mean transa
svy:mean transa,over(age3)
svy:mean transa,over(sex)
svy:mean transa,over(ETHNIC)
svy:mean transa,over(SES)
svy:tab age3 transa, row
svy:tab sex transa, row
svy:tab ETHNIC transa, row
svy:tab SES transa, row
restore
*di 21640/29039 /*75% did no active trans */

*Work.
preserve
summ a2 
summ a2 if a2==0
summ work150a if a2>=150
replace work150a=work150a*100 
svy:mean work150a
svy:mean work150a,over(age3)
svy:mean work150a,over(sex)
svy:mean work150a,over(ETHNIC)
*svy:mean work150a,over(SES)
svy:tab age3 work150a, row
svy:tab sex work150a, row
svy:tab ETHNIC work150a, row
*svy:tab SES work150a, row
restore
di (17858/29039) /*Work */

**************************
*Leisure-time (Table 1).
*Two-way interaction.
***************************

svy:logit recsa i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#age3
testparm SES#sex
testparm SES#ETHNIC
*excludes other ethnic group
test [recsa]2.SES#2.ETHNIC [recsa]2.SES#3.ETHNIC [recsa]3.SES#2.ETHNIC  [recsa]3.SES#3.ETHNIC [recsa]4.SES#2.ETHNIC [recsa]4.SES#3.ETHNIC

margins,vce(unconditional)
margins,at(SES=(1 2 3 4)) vce(unconditional)
margins,at(SES=(1 2 3 4) age3=(1 2 3)) vce(unconditional)
margins,at(SES=(1 2 3 4) sex=(1 2)) vce(unconditional)
margins,at(SES=(1 2 3 4) ETHNIC=(1 2 3)) vce(unconditional)
margins age3, dydx(SES) 
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES) 
*di (.3809915) - (.2386755)   /* Text */
*di (.3809915) / (.2386755)  /* Text */

*2-way interaction (SES*year).
qui:svy:logit recsa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm i.SES#c.year

*3-way interactions.
qui:svy:logit recsa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.age3##c.year,or
testparm i.SES#i.age3#c.year

qui:svy:logit recsa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.sex##c.year,or
testparm i.SES#i.sex#c.year	

qui:svy:logit recsa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.ETHNIC##c.year,or
testparm i.SES#i.ETHNIC#c.year
test [recsa]2.SES#2.ETHNIC#c.year [recsa]2.SES#3.ETHNIC#c.year ///
[recsa]3.SES#2.ETHNIC#c.year [recsa]3.SES#3.ETHNIC#c.year ///
[recsa]4.SES#2.ETHNIC#c.year [recsa]4.SES#3.ETHNIC#c.year

****************************
** Active Transportation.
****************************

svy:logit transa i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#age3
testparm SES#sex
testparm SES#ETHNIC
test [transa]2.SES#2.ETHNIC [transa]2.SES#3.ETHNIC [transa]3.SES#2.ETHNIC  ///
[transa]3.SES#3.ETHNIC [transa]4.SES#2.ETHNIC [transa]4.SES#3.ETHNIC

margins,vce(unconditional)
margins,at(SES=(1 2 3 4)) vce(unconditional)
margins,at(SES=(1 2 3 4) age3=(1 2 3)) vce(unconditional)
margins,at(SES=(1 2 3 4) sex=(1 2)) vce(unconditional)
margins,at(SES=(1 2 3 4) ETHNIC=(1 2 3)) vce(unconditional)
margins age3, dydx(SES) 
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES) 

*2-way interaction.
qui:svy:logit transa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm i.SES#c.year

*3-way interaction.
qui:svy:logit transa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.age3##c.year,or
testparm i.SES#i.age3#c.year

qui:svy:logit transa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.sex##c.year,or
testparm i.SES#i.sex#c.year	

qui:svy:logit transa i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.ETHNIC##c.year,or
testparm i.SES#i.ETHNIC#c.year
test [transa]2.SES#2.ETHNIC#c.year [transa]2.SES#3.ETHNIC#c.year ///
[transa]3.SES#2.ETHNIC#c.year [transa]3.SES#3.ETHNIC#c.year ///
[transa]4.SES#2.ETHNIC#c.year [transa]4.SES#3.ETHNIC#c.year

************************
**Work.
*Two-way interaction.
************************

svy:logit work150a i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#age3
testparm SES#sex
testparm SES#ETHNIC
test [work150a]2.SES#2.ETHNIC [work150a]2.SES#3.ETHNIC [work150a]3.SES#2.ETHNIC  ///
[work150a]3.SES#3.ETHNIC [work150a]4.SES#2.ETHNIC [work150a]4.SES#3.ETHNIC
margins,vce(unconditional)
margins,at(SES=(1 2 3 4)) vce(unconditional)
margins,at(SES=(1 2 3 4) age3=(1 2 3)) vce(unconditional)
margins,at(SES=(1 2 3 4) sex=(1 2)) vce(unconditional)
margins,at(SES=(1 2 3 4) ETHNIC=(1 2 3)) vce(unconditional)
margins age3, dydx(SES) 
margins sex, dydx(SES) 
*di (-.1796726) - (-.0325408)
*di (-.1796726) / (-.0325408)
margins ETHNIC, dydx(SES) 

*2-way interaction.
qui:svy:logit work150a i.SES i.age3 i.ETHNIC i.sex c.year i.SES#c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm i.SES#c.year

*3-way interaction.
qui:svy:logit work150a i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.age3##c.year,or
testparm i.SES#i.age3#c.year

qui:svy:logit work150a i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.sex##c.year,or
testparm i.SES#i.sex#c.year	

qui:svy:logit work150a i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.ETHNIC##c.year,or
testparm i.SES#i.ETHNIC#c.year

test [work150a]2.SES#2.ETHNIC#c.year [work150a]2.SES#3.ETHNIC#c.year [work150a]3.SES#2.ETHNIC#c.year ///
[work150a]3.SES#3.ETHNIC#c.year [work150a]4.SES#2.ETHNIC#c.year  [work150a]4.SES#3.ETHNIC#c.year

*********************************
*Composite.
*Two-way interaction.
*********************************

svy:logit MeetsRecs1 i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#age3
testparm SES#sex
testparm SES#ETHNIC
*exclude other ethnic
test [MeetsRecs1]2.SES#2.ETHNIC [MeetsRecs1]2.SES#3.ETHNIC [MeetsRecs1]3.SES#2.ETHNIC  ///
[MeetsRecs1]3.SES#3.ETHNIC [MeetsRecs1]4.SES#2.ETHNIC [MeetsRecs1]4.SES#3.ETHNIC
margins,vce(unconditional) 
margins,at(SES=(1 2 3 4)) vce(unconditional) 
margins,at(SES=(1 2 3 4) age3=(1 2 3)) vce(unconditional)
margins,at(SES=(1 2 3 4) sex=(1 2)) vce(unconditional)
margins,at(SES=(1 2 3 4) ETHNIC=(1 2 3)) vce(unconditional)
margins age3, dydx(SES) 
*di (.2313205 )-(.1083024 )
*di (.2313205 )/(.1083024 )
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES)

*2-way interaction.
qui:svy:logit MeetsRecs1 i.SES i.age3 i.ETHNIC i.sex c.year i.SES#c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm i.SES#c.year

*3-way interaction.
qui:svy:logit MeetsRecs1 i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.age3##c.year,or
testparm i.SES#i.age3#c.year
qui:svy:logit MeetsRecs1 i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.sex##c.year,or
testparm i.SES#i.sex#c.year	
qui:svy:logit MeetsRecs1 i.SES i.age3 i.ETHNIC i.sex c.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC i.SES##i.ETHNIC##c.year,or
testparm i.SES#i.ETHNIC#c.year
test [MeetsRecs1]2.SES#2.ETHNIC#c.year [MeetsRecs1]2.SES#3.ETHNIC#c.year [MeetsRecs1]3.SES#2.ETHNIC#c.year ///
[MeetsRecs1]3.SES#3.ETHNIC#c.year [MeetsRecs1]4.SES#2.ETHNIC#c.year  [MeetsRecs1]4.SES#3.ETHNIC#c.year



*********************************
* SII.
* Additional file 3.
*********************************

wridit SES [pwei=wtint2yr], generate(ridit) by(year)
svy:tab ridit year, col

*_______________________
*SII: Leisure-Time.

svy:regress recsa c.ridit i.age3 i.ETHNIC i.sex i.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC
testparm c.ridit#sex
testparm c.ridit#age3
testparm c.ridit#ETHNIC
estimates store m1
margins,at(ridit=(0 1)) post vce(unconditional) /* overall */
lincom (2._at - 1._at) /* SII */

estimates restore m1
margins,at(ridit=(0 1) age3=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII at each age */
lincom (5._at - 2._at) /* SII at each age */
lincom (6._at - 3._at) /* SII at each age */

estimates restore m1
margins,at(ridit=(0 1) sex=(1 2)) vce(unconditional) post /* SII at each gender */
lincom (3._at - 1._at) /* SII (male) */
lincom (4._at - 2._at) /* SII (female) */

estimates restore m1
margins,at(ridit=(0 1) ETHNIC=(1 2 3)) vce(unconditional) post /* SII at each ethnic */
lincom (4._at - 1._at) /* SII (white) */
lincom (5._at - 2._at) /* SII (Hispanic) */
lincom (6._at - 3._at) /* SII (Black) */

*3-way interactions.
qui:svy:regress recsa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.age3##c.year
testparm c.ridit#i.age3#c.year

qui:svy:regress recsa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.sex##c.year
testparm c.ridit#i.sex#c.year	

qui:svy:regress recsa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.ETHNIC##c.year
testparm c.ridit#i.ETHNIC#c.year

*two-way (year)
qui:svy:logit recsa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC,or
testparm c.ridit#c.year

************************
*SII: Transportation.
*************************
svy:regress transa c.ridit i.age3 i.ETHNIC i.sex i.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC
testparm c.ridit#sex
testparm c.ridit#age3
testparm c.ridit#ETHNIC
estimates store m1
margins,at(ridit=(0 1)) post vce(unconditional) /* overall */
lincom (2._at - 1._at) /* SII */

estimates restore m1
margins,at(ridit=(0 1) age3=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII at each age */
lincom (5._at - 2._at) /* SII at each age */
lincom (6._at - 3._at) /* SII at each age */

estimates restore m1
margins,at(ridit=(0 1) sex=(1 2)) vce(unconditional) post /* SII at each age */
lincom (3._at - 1._at) /* SII (male) */
lincom (4._at - 2._at) /* SII (female) */

estimates restore m1
margins,at(ridit=(0 1) ETHNIC=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII (white) */
lincom (5._at - 2._at) /* SII (Hispanic) */
lincom (6._at - 3._at) /* SII (Black) */

*3-way interactions.
qui:svy:regress transa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.age3##c.year
testparm c.ridit#i.age3#c.year

qui:svy:regress transa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.sex##c.year
testparm c.ridit#i.sex#c.year	

qui:svy:regress transa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.ETHNIC##c.year
testparm c.ridit#i.ETHNIC#c.year

*two-way (year)
qui:svy:logit transa c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC,or
testparm c.ridit#c.year


************************
*SII: Work.
************************
svy:regress work150a c.ridit i.age3 i.ETHNIC i.sex i.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC
testparm c.ridit#sex
testparm c.ridit#age3
testparm c.ridit#ETHNIC
estimates store m1
margins,at(ridit=(0 1)) post vce(unconditional) /* overall */
lincom (2._at - 1._at) /* SII */

estimates restore m1
margins,at(ridit=(0 1) age3=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII at each age */
lincom (5._at - 2._at) /* SII at each age */
lincom (6._at - 3._at) /* SII at each age */

estimates restore m1
margins,at(ridit=(0 1) sex=(1 2)) vce(unconditional) post /* SII at each age */
lincom (3._at - 1._at) /* SII (male) */
lincom (4._at - 2._at) /* SII (female) */

estimates restore m1
margins,at(ridit=(0 1) ETHNIC=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII (white) */
lincom (5._at - 2._at) /* SII (Hispanic) */
lincom (6._at - 3._at) /* SII (Black) */

*3-way interactions.
qui:svy:regress work150a c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.age3##c.year
testparm c.ridit#i.age3#c.year

qui:svy:regress work150a c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.sex##c.year
testparm c.ridit#i.sex#c.year	

qui:svy:regress work150a c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.ETHNIC##c.year
testparm c.ridit#i.ETHNIC#c.year

*two-way (year)
qui:svy:logit work150a c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC,or
testparm c.ridit#c.year


*********************
*SII: Overall.
*********************
svy:regress MeetsRecs1 c.ridit i.age3 i.ETHNIC i.sex i.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC
testparm c.ridit#sex
testparm c.ridit#age3
testparm c.ridit#ETHNIC
estimates store m1
margins,at(ridit=(0 1)) post vce(unconditional) /* overall */
lincom (2._at - 1._at) /* SII */

estimates restore m1
margins,at(ridit=(0 1) age3=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII at each age */
lincom (5._at - 2._at) /* SII at each age */
lincom (6._at - 3._at) /* SII at each age */

estimates restore m1
margins,at(ridit=(0 1) sex=(1 2)) vce(unconditional) post /* SII at each age */
lincom (3._at - 1._at) /* SII (male) */
lincom (4._at - 2._at) /* SII (female) */

estimates restore m1
margins,at(ridit=(0 1) ETHNIC=(1 2 3)) vce(unconditional) post /* SII at each age */
lincom (4._at - 1._at) /* SII (white) */
lincom (5._at - 2._at) /* SII (Hispanic) */
lincom (6._at - 3._at) /* SII (Black) */

*3-way interactions.
qui:svy:regress MeetsRecs1 c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.age3##c.year
testparm c.ridit#i.age3#c.year

qui:svy:regress MeetsRecs1 c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.sex##c.year
testparm c.ridit#i.sex#c.year	

qui:svy:regress MeetsRecs1 c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC c.ridit##i.ETHNIC##c.year
testparm c.ridit#i.ETHNIC#c.year

*two-way (year)
qui:svy:logit MeetsRecs1 c.ridit i.age3 i.ETHNIC i.sex c.year c.ridit#c.year c.ridit#i.sex c.ridit#i.age3 c.ridit#i.ETHNIC,or
testparm c.ridit#c.year

************************
*Additional File 5
************************

*********************************
* Composite (MVPA: <60 minutes).
*********************************
svy:logit MeetsRecs2a i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#sex
testparm SES#age3
testparm SES#ETHNIC
margins age3, dydx(SES) 
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES) 		 

*********************************
* Composite (MVPA: <90 minutes).
*********************************
svy:logit MeetsRecs2b i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#sex
testparm SES#age3
testparm SES#ETHNIC
margins age3, dydx(SES) 
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES) 			 
			 
*********************************
* Composite (MVPA: <120 minutes).
*********************************
svy:logit MeetsRecs2c i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#sex
testparm SES#age3
testparm SES#ETHNIC
margins age3, dydx(SES) 
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES) 			 
			 
*********************************
* Composite (MVPA: <150 minutes).
*********************************
svy:logit MeetsRecs2d i.SES i.age3 i.ETHNIC i.sex i.year i.SES#i.sex i.SES#i.age3 i.SES#i.ETHNIC,or
testparm SES#sex
testparm SES#age3
testparm SES#ETHNIC
margins age3, dydx(SES) 
margins sex, dydx(SES) 
margins ETHNIC, dydx(SES) 	













 













