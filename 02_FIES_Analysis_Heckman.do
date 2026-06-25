clear

*Importing dataset
use "C:\Users\Angela\OneDrive - University of the Philippines\ECON 299\1. DO FILE\FIESV2.dta"

do "ADiana. do file. preliminaries.do"

*Start logging
cap log on 
log using "ADiana. Econ 299 Initial Runs[LOG file]_120923.smcl", replace

*Description of the variables	
asdoc tab privatepension, save(summary statistics.doc) title(Table 1: Descriptive Statistics of Relevant Indicator Variables) replace
asdoc tab occup2 privatepension, save(summary statistics.doc) append
asdoc tab kb2 privatepension, save(summary statistics.doc) append

**Getting the log of continuous variables
gen logppension = log(pensionp_qty)
replace logppension = 0 if logppension == .

gen logwages = log(wages)
replace logwages = 0 if logwages == .

gen wages2 = wages*wages
gen logwages2 = log(wages2)

gen logtoinc = log(toinc)
replace logtoinc = 0 if logtoinc == .

gen toinc2 = toinc*toinc
gen logtoinc2 = log(toinc2)


*Using Heckman Selection Model
*Using toinc
heckman logppension logtoinc i.kb2 hhsex age college, select (privatepension = logtoinc i.occup2 i.hhtype spousejob hhsex age college) twostep
outreg2 using Heckman_120923.doc, append dec(4) ctitle(Model 3) title(Results: Heckman Selection Model) label

heckman logppension logtoinc i.occup2 i.kb2 i.occup2#kb2 hhsex age college, select (privatepension = logtoinc i.occup2 i.hhtype spousejob hhsex age college) twostep
outreg2 using Heckman_120923.doc, append dec(4) ctitle(Model 4) title(Results: Heckman Selection Model) label

log close
