
****************************************************************
*** This program makes Tables 1, 2, 3 and Appendix Table A2. ***
****************************************************************

clear all 
set maxvar 32000
set matsize 3000
set more off, perm

clear
global PATH "E:\Dropbox\Dropbox\"

cd "$PATH\Data"

use ipeds_cleaned_final.dta, replace

*** Set sample ***

*Keep only the years with reported grad rate data
drop if year<1991 | year>2010
drop if year==1994

*Keep only sample that has non-missing covariates 
drop if faculty==. | instat==. | revenue==. | cost==. 

*Keep only schools that report grad rates (4- and 6-year) in each year
fillin unitid year
gen tag=0
replace tag=1 if missing(gradrate4yr)
bysort unitid: egen sumtag=sum(tag)
drop if sumtag>0

drop if gradrate4yr==.
drop if gradrate6yr==.

*** A bit more cleaning before starting the analysis ***

*Generate covariates 
gen per_white=(white_cohortsize/totcohortsize)
gen per_urm=(urm_cohortsize/totcohortsize)
gen per_male=(m_cohortsize/totcohortsize)
gen per_fem=(w_cohortsize/totcohortsize)

*egen     timing_group=group(yearofsem)
*replace  timing_group=99 if yearofsem==.

*Flag states that had a state-level mandate for all public 4-year schools to switch from quarters to semesters 
gen mandate=0
replace mandate=1 if fips==13 | fips==27 | fips==38 | fips==39 | fips==48 | fips==49 

*Flag institutions that switch from quarters to semesters
gen changers=0
replace changers=1 if yearstosem!=.

*Generate year dummies
tab year, generate(y)

*Define controls 
global timevar "instatetuition faculty  costs per_urm per_white  per_fem"

log using "$PATH\Output\table_1.txt", replace text
***Table 1: Institution-Level Summary Statistics 
*Column 1: All Observations
sutex semester2, dig(2)
sutex gradrate4yr womengradrate4yr mengradrate4yr urmgradrate4yr nonurmgradrate4yr [aw= meansize] , dig(2)
sutex gradrate6yr womengradrate6yr mengradrate6yr urmgradrate6yr nonurmgradrate6yr [aw= meansize] , dig(2)
sutex public faculty totcohortsize instatetuition  costs  , dig(2)
*sutex gradrate4yr gradrate6yr if public==1 [aw= meansize] , dig(2)

*Column 2: Never Changes Only
sutex semester2 if changers==0, dig(2)
sutex gradrate4yr womengradrate4yr mengradrate4yr urmgradrate4yr nonurmgradrate4yr [aw= meansize] if changers==0 , dig(2)
sutex gradrate6yr womengradrate6yr mengradrate6yr urmgradrate6yr nonurmgradrate6yr [aw= meansize] if changers==0, dig(2)
sutex public faculty totcohortsize instatetuition  costs if changers==0 , dig(2)

*Column 3: Changers Only
sutex semester2 if changers==1, dig(2)
sutex gradrate4yr womengradrate4yr mengradrate4yr urmgradrate4yr nonurmgradrate4yr [aw= meansize] if changers==1 , dig(2)
sutex gradrate6yr womengradrate6yr mengradrate6yr urmgradrate6yr nonurmgradrate6yr [aw= meansize] if changers==1, dig(2)
sutex public faculty totcohortsize instatetuition  costs if changers==1 , dig(2)

log close

***Table 2: Effect of Switching to Semesters on Institution and Student Characteristics (Balance Test)
*Institutional Characterisitics 
*Column 1
areg faculty                 semester2         i.year c.year#i.unitid     , absorb(unitid) cluster(unitid) 
outreg2 using "$PATH\Output\table_2", replace tex dec(3)  keep(semester2) 
*Column 2
areg costs                    semester2         i.year c.year#i.unitid       , absorb(unitid)  cluster(unitid) 
outreg2 using "$PATH\Output\table_2", append tex dec(3)  keep(semester2) 
*Column 3
areg instatetuition           semester2         i.year c.year#i.unitid      , absorb(unitid) cluster(unitid) 
outreg2 using "$PATH\Output\table_2", append tex dec(3)  keep(semester2) 
*Column 4
areg totcohortsize           semester2         i.year c.year#i.unitid      , absorb(unitid) cluster(unitid) 
outreg2 using "$PATH\Output\table_2", append tex dec(3)  keep(semester2) 

*Student Characteristics 
*Column 5
areg per_urm                   semester2         i.year c.year#i.unitid     , absorb(unitid) cluster(unitid)  
outreg2 using "$PATH\Output\table_2", append tex dec(3)  keep(semester2) 
*Column 6
areg per_white                 semester2         i.year c.year#i.unitid      , absorb(unitid) cluster(unitid) 
outreg2 using "$PATH\Output\table_2", append tex dec(3)  keep(semester2) 
*Column 7
areg per_fem                   semester2        i.year c.year#i.unitid     , absorb(unitid) cluster(unitid) 
outreg2 using "$PATH\Output\table_2", append tex dec(3)  keep(semester2) 

log using "$PATH\Output\table_2_means.txt", replace text
sum  faculty costs instatetuition totcohortsize per_urm per_white per_fem
log close

***Table 3: Effect of Switching to Semesters on Graduation Rates (main table of results) 
*Panel A
*generate the 2 post-period indicators for four-year grad outcome 
gen block1=0
replace block1=1 if yearstosem>=-3 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

*Column 1
areg gradrate4yr  block1 block2     i.year [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", replace tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, No)
*Column 2
areg gradrate4yr  block1 block2  $timevar i.year      [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, Yes, Time Trends, No)
*Column 3
areg gradrate4yr  block1 block2     i.year c.year#i.unitid  [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 4
areg gradrate4yr  block1 block2     i.year c.year#i.unitid $timevar   [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, Yes, Time Trends, Yes)
*Column 5
areg womengradrate4yr  block1 block2 $timevar   i.year c.year#i.unitid   [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 6
areg mengradrate4yr  block1 block2 $timevar    i.year c.year#i.unitid   [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 7
areg urmgradrate4yr  block1 block2 $timevar   i.year c.year#i.unitid   [aw=meansize]   , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 8
areg nonurmgradrate4yr  block1 block2 $timevar    i.year c.year#i.unitid  [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 9
areg gradrate4yr  block1 block2 $timevar   i.year c.year#i.unitid   [aw=meansize]  if sector==1  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 10
areg gradrate4yr  block1 block2  $timevar  i.year c.year#i.unitid   [aw=meansize]  if sector==2 , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)

*Panel B
*generate the 2 post-period indicators for six-year grad outcome 
drop block1 block2
gen block1=0
replace block1=1 if yearstosem>=-5 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

*Column 1
areg gradrate6yr  block1 block2     i.year [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", replace tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, No)
*Column 2
areg gradrate6yr  block1 block2  $timevar i.year      [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, Yes, Time Trends, No)
*Column 3
areg gradrate6yr  block1 block2     i.year c.year#i.unitid  [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 4
areg gradrate6yr  block1 block2     i.year c.year#i.unitid $timevar   [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, Yes, Time Trends, Yes)
*Column 5
areg womengradrate6yr  block1 block2 $timevar   i.year c.year#i.unitid   [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 6
areg mengradrate6yr  block1 block2 $timevar    i.year c.year#i.unitid   [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 7
areg urmgradrate6yr  block1 block2 $timevar   i.year c.year#i.unitid   [aw=meansize]   , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 8
areg nonurmgradrate6yr  block1 block2 $timevar    i.year c.year#i.unitid  [aw=meansize]  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 9
areg gradrate6yr  block1 block2 $timevar   i.year c.year#i.unitid   [aw=meansize]  if sector==1  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 10
areg gradrate6yr  block1 block2  $timevar  i.year c.year#i.unitid   [aw=meansize]  if sector==2 , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)

*** Appendix Tables *****

*** Table A1: Effect of Switching to Semesters on Graduation Rates, Unbalanced Panel 
*See following program to generate this table: Table_A1.do

*** Table A2: Effect of Switching to Semesters on Graduation Rates, Unweighted
*Panel A
*generate the 2 post-period indicators for four-year grad outcome 
drop block1 block2
gen block1=0
replace block1=1 if yearstosem>=-3 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

*Column 1
areg gradrate4yr  block1 block2     i.year c.year#i.unitid $timevar     , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", replace tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, Yes, Time Trends, Yes)
*Column 2
areg womengradrate4yr  block1 block2 $timevar   i.year c.year#i.unitid     , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 3
areg mengradrate4yr  block1 block2 $timevar    i.year c.year#i.unitid     , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 4
areg urmgradrate4yr  block1 block2 $timevar   i.year c.year#i.unitid      , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 5
areg nonurmgradrate4yr  block1 block2 $timevar    i.year c.year#i.unitid    , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 6
areg gradrate4yr  block1 block2 $timevar   i.year c.year#i.unitid     if sector==1  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 7
areg gradrate4yr  block1 block2  $timevar  i.year c.year#i.unitid     if sector==2 , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3a_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)

*Panel B
*generate the 2 post-period indicators for six-year grad outcome 
drop block1 block2
gen block1=0
replace block1=1 if yearstosem>=-5 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

*Column 1
areg gradrate6yr  block1 block2     i.year c.year#i.unitid $timevar     , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, Yes, Time Trends, Yes)
*Column 2
areg womengradrate6yr  block1 block2 $timevar   i.year c.year#i.unitid     , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 3
areg mengradrate6yr  block1 block2 $timevar    i.year c.year#i.unitid     , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 4
areg urmgradrate6yr  block1 block2 $timevar   i.year c.year#i.unitid      , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 5
areg nonurmgradrate6yr  block1 block2 $timevar    i.year c.year#i.unitid    , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 6
areg gradrate6yr  block1 block2 $timevar   i.year c.year#i.unitid     if sector==1  , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)
*Column 7
areg gradrate6yr  block1 block2  $timevar  i.year c.year#i.unitid     if sector==2 , cluster(unitid) abs(unitid)
outreg2 using "$PATH\Output\table_3b_appa2", append tex dec(3)  keep(block1 block2   ) addtext(School FE, Yes, Year FE, Yes,Controls, No, Time Trends, Yes)


