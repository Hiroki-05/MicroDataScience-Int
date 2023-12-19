
****************************************************************
*** This program makes Figures 1, 2, A1, A2 & A3  **************
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

********************************************************
** Figure 1: Share On Semesters and 4-year Grad Rates **
********************************************************
preserve

collapse (mean)  gradrate4yr semester2 , by(year)

*variation in treatment Graph for data section
*collapse (mean) semester2 (sum) totco tot4yrgrads, by(year)
label var gradrate4yr "4-Year Graduation Rate"
tsset year

twoway (tsline semester2, lcolor(black)) (tsline gradrate4yr, yaxis(2) lcolor(black) lpattern(dash)), ytitle(Fraction of Schools on Semesters) yscale(range(.8 1)) ///
ylabel(0.8(0.05)1, valuelabel nogrid) ytitle(4-Year Graduation Rate, axis(2)) ylabel(0.25(0.05)0.45, axis(2)) ttitle(Year)  graphregion(fcolor(white)) xsize(6.5) 
graph export "$PATH\Output\figure_1.pdf", replace 
restore

************************************************
** Figure 2a and 2b: event study ***************
************************************************

*Figure 2a (4-year grad outcome)
xtset unitid
xtreg gradrate4yr yearsbf10minus yearsbf9-yearsbf5 yearsbf3-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus $timevar i.year c.year#i.unitid [aw= meansize], fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = _b[yearsbf6], _b[yearsbf6] - (2*_se[yearsbf6]), _b[yearsbf6] + (2*_se[yearsbf6])
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = 0,0,0
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(7, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash) ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") 

graph export "$PATH\Output\figure_2a.pdf", replace 

*Figure 2b (6-year grad outcome)
xtreg gradrate6yr yearsbf10minus yearsbf9-yearsbf7 yearsbf5-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus  $timevar i.year c.year#i.unitid [aw= meansize], fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = 0,0,0
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = _b[yearsbf4], _b[yearsbf4] - (2*_se[yearsbf4]), _b[yearsbf4] + (2*_se[yearsbf4])
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(5, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash)   ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") ylabel(-0.1 (0.05) 0.05)

graph export "$PATH\Output\figure_2b.pdf", replace 


*************************************************
** Figure A1: event study robustness check ******
** Subsample of institutions that were mandated**
** to swtich to semesters by the state    *******
*************************************************

*Figure A1 (4-year grad outcome)
xtset unitid
xtreg gradrate4yr yearsbf10minus yearsbf9-yearsbf5 yearsbf3-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus $timevar i.year c.year#i.unitid [aw= meansize] if mandate==1, fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = _b[yearsbf6], _b[yearsbf6] - (2*_se[yearsbf6]), _b[yearsbf6] + (2*_se[yearsbf6])
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = 0,0,0
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(7, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash) ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") 

graph export "$PATH\Output\figure_A1.pdf", replace 

*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts for the subset of institutions that 
*experienced state-level mandates
gen block1=0
replace block1=1 if yearstosem>=-3 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

log using "$PATH\Output\figure_A1_DnD.txt", replace text
areg gradrate4yr  block1 block2     i.year c.year#i.unitid $timevar   [aw=meansize] if mandate==1 , cluster(unitid) abs(unitid)
log close 

*************************************************
** Figure A2: event study robustness checks *****
*************************************************

*Figure A2a: Excluding First 10 Years, 4-year graduation outcome 
preserve
* Drop first ten years 
drop if yearstosem<=-14
xtset unitid
xtreg gradrate4yr yearsbf10minus yearsbf9-yearsbf5 yearsbf3-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus $timevar i.year c.year#i.unitid [aw= meansize], fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = _b[yearsbf6], _b[yearsbf6] - (2*_se[yearsbf6]), _b[yearsbf6] + (2*_se[yearsbf6])
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = 0,0,0
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(7, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash) ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") 

graph export "$PATH\Output\figure_A2a.pdf", replace 

*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts 
drop block*
gen block1=0
replace block1=1 if yearstosem>=-3 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

log using "$PATH\Output\figure_A2a_DnD.txt", replace text
areg gradrate4yr  block1 block2     i.year c.year#i.unitid $timevar   [aw=meansize] , cluster(unitid) abs(unitid)
log close 
restore

*Figure A2b: No Linear Time Trends, 4-year graduation outcome  
xtreg gradrate4yr yearsbf10minus yearsbf9-yearsbf5 yearsbf3-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus i.year  $timevar [aw= meansize] , fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = _b[yearsbf6], _b[yearsbf6] - (2*_se[yearsbf6]), _b[yearsbf6] + (2*_se[yearsbf6])
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = 0,0,0
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(7, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash) ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") 

graph export "$PATH\Output\figure_A2b.pdf", replace 


*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts 
drop block*
gen block1=0
replace block1=1 if yearstosem>=-3 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

log using "$PATH\Output\figure_A2b_DnD.txt", replace text
areg gradrate4yr  block1 block2     i.year  $timevar   [aw=meansize] , cluster(unitid) abs(unitid)
log close 

*Figure A2c: Changers Only, No Linear Time Trends, 4-year graduation outcome  
xtreg gradrate4yr yearsbf10minus yearsbf9-yearsbf5 yearsbf3-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus i.year  $timevar [aw= meansize] if changer==1, fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = _b[yearsbf6], _b[yearsbf6] - (2*_se[yearsbf6]), _b[yearsbf6] + (2*_se[yearsbf6])
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = 0,0,0
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(7, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash) ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") 

graph export "$PATH\Output\figure_A2c.pdf", replace 

log using "$PATH\Output\figure_A2c_DnD.txt", replace text
*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts 
areg gradrate4yr  block1 block2     i.year  $timevar   [aw=meansize] if changer==1 , cluster(unitid) abs(unitid)
log close

*Figure A2d: Excluding First 10 Years, 6-year graduation outcome 
preserve
drop if yearstosem<=-14
xtset unitid
xtreg gradrate6yr yearsbf10minus yearsbf9-yearsbf7 yearsbf5-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus i.year c.year#i.unitid  $timevar  [aw= meansize], fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = 0,0,0
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = _b[yearsbf4], _b[yearsbf4] - (2*_se[yearsbf4]), _b[yearsbf4] + (2*_se[yearsbf4])
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(5, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash)   ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") ylabel(-0.1 (0.05) 0.05)

graph export "$PATH\Output\figure_A2d.pdf", replace 


*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts 
drop block1 block2
gen block1=0
replace block1=1 if yearstosem>=-5 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

log using "$PATH\Output\figure_A2d_DnD.txt", replace text
areg gradrate6yr  block1 block2     i.year c.year#i.unitid  $timevar   [aw=meansize] , cluster(unitid) abs(unitid)
log close 
restore 

*Figure A2e: No Linear Time Trends, 6-year graduation outcome  
xtreg gradrate6yr yearsbf10minus yearsbf9-yearsbf7 yearsbf5-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus i.year $timevar  [aw= meansize], fe cluster(unitid) 
matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = 0,0,0
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = _b[yearsbf4], _b[yearsbf4] - (2*_se[yearsbf4]), _b[yearsbf4] + (2*_se[yearsbf4])
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(5, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash)   ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") ylabel(-0.1 (0.05) 0.05)

graph export "$PATH\Output\figure_A2e.pdf", replace 


*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts 
drop block1 block2
gen block1=0
replace block1=1 if yearstosem>=-5 & yearstosem<0 
gen block2=0
replace block2=1 if yearstosem>=0 & !missing(yearstosem)

log using "$PATH\Output\figure_A2e_DnD.txt", replace text
areg gradrate6yr  block1 block2     i.year  $timevar   [aw=meansize] , cluster(unitid) abs(unitid)
log close 

*Figure A2f: Changers Only, No Linear Time Trends, 6-year graduation outcome  
xtreg gradrate6yr yearsbf10minus yearsbf9-yearsbf7 yearsbf5-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus i.year $timevar  [aw= meansize] if changer==1, fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = 0,0,0
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = _b[yearsbf4], _b[yearsbf4] - (2*_se[yearsbf4]), _b[yearsbf4] + (2*_se[yearsbf4])
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9"  ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(5, lcolor(blue) lwidth(thin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash)   ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") ylabel(-0.1 (0.05) 0.05)

graph export "$PATH\Output\figure_A2f.pdf", replace 

log using "$PATH\Output\figure_A2f_DnD.txt", replace text
*This code provides the D-in-D point estimates that are included on the figure for the partially treated and fully treated cohorts 
areg gradrate6yr  block1 block2     i.year  $timevar   [aw=meansize] if changer==1 , cluster(unitid) abs(unitid)
log close

*************************************************
** Figure A3: event study for in-state tuition **
*************************************************
xtset unitid
xtreg instatetuition yearsbf10minus yearsbf9-yearsbf5 yearsbf3-yearsbf1 yearof yearsaf1-yearsaf9 yearsaf10plus i.year c.year#i.unitid   [aw= meansize], fe cluster(unitid) 

matrix D = J(19,3,.) 

matrix D[1,1] = _b[yearsbf9], _b[yearsbf9] - (2*_se[yearsbf9]),_b[yearsbf9] + (2*_se[yearsbf9])
matrix D[2,1] = _b[yearsbf8], _b[yearsbf8] - (2*_se[yearsbf8]), _b[yearsbf8] +(2*_se[yearsbf8])
matrix D[3,1] = _b[yearsbf7], _b[yearsbf7] - (2*_se[yearsbf7]), _b[yearsbf7] +(2*_se[yearsbf7])
matrix D[4,1] = _b[yearsbf6], _b[yearsbf6] - (2*_se[yearsbf6]), _b[yearsbf6] + (2*_se[yearsbf6])
matrix D[5,1] = _b[yearsbf5], _b[yearsbf5] - (2*_se[yearsbf5]), _b[yearsbf5] + (2*_se[yearsbf5])
matrix D[6,1] = 0,0,0
matrix D[7,1] = _b[yearsbf3], _b[yearsbf3] - (2*_se[yearsbf3]), _b[yearsbf3] + (2*_se[yearsbf3])
matrix D[8,1] = _b[yearsbf2], _b[yearsbf2] - (2*_se[yearsbf2]), _b[yearsbf2] + (2*_se[yearsbf2])
matrix D[9,1] = _b[yearsbf1], _b[yearsbf1] - (2*_se[yearsbf1]), _b[yearsbf1] + (2*_se[yearsbf1])
matrix D[10,1] = _b[yearof], _b[yearof] - (2*_se[yearof]), _b[yearof] + (2*_se[yearof])
matrix D[11,1] = _b[yearsaf1], _b[yearsaf1] - (2*_se[yearsaf1]),_b[yearsaf1] + (2*_se[yearsaf1])
matrix D[12,1] = _b[yearsaf2], _b[yearsaf2] - (2*_se[yearsaf2]),_b[yearsaf2] + (2*_se[yearsaf2])
matrix D[13,1] = _b[yearsaf3], _b[yearsaf3] - (2*_se[yearsaf3]),_b[yearsaf3] + (2*_se[yearsaf3])
matrix D[14,1] = _b[yearsaf4], _b[yearsaf4] - (2*_se[yearsaf4]),_b[yearsaf4] + (2*_se[yearsaf4])
matrix D[15,1] = _b[yearsaf5], _b[yearsaf5] - (2*_se[yearsaf5]),_b[yearsaf5] + (2*_se[yearsaf5])
matrix D[16,1] = _b[yearsaf6], _b[yearsaf6] - (2*_se[yearsaf6]),_b[yearsaf6] + (2*_se[yearsaf6])
matrix D[17,1] = _b[yearsaf7], _b[yearsaf7] - (2*_se[yearsaf7]),_b[yearsaf7] + (2*_se[yearsaf7])
matrix D[18,1] = _b[yearsaf8], _b[yearsaf8] - (2*_se[yearsaf8]),_b[yearsaf8] + (2*_se[yearsaf8])
matrix D[19,1] = _b[yearsaf9], _b[yearsaf9] - (2*_se[yearsaf9]),_b[yearsaf9] + (2*_se[yearsaf9])

coefplot matrix(D[,1]), xlabel( 1 "-9" 2 "-8" 3 "-7" 4 "-6" 5 "-5" 6 "-4" 7 "-3" 8 "-2" 9 "-1" 10 "0" 11 "1"  12 "2" 13 "3" 14 "4" 15 "5" 16 "6" 17 "7" 18 "8" 19 "9" ) ///
vertical yline(0, lcolor(black) lwidth(medthin)) xline(10, lcolor(blue) lwidth(thin)) ci((D[,2] D[,3])) lcolor(black) msymbol(Oh) /// 
mcolor(black) mlwidth(medium) connect(direct) graphregion(color(white)) ciopts(lpattern(shortdash) ///
ylab(,nogrid) recast(rline) lcolor(red)) xtitle("Years Relative to Policy") ytitle("Coefficient") 

graph export "$PATH\Output\figure_A3.pdf", replace 
