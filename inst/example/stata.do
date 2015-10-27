log using "test.log", replace
webuse auto.dta

di "tab2r tab.RData"
tab make foreign
di "tab2r tabsum.RData"
tab foreign, sum(mpg)
di "tabstat2r tabstat.RData"
tabstat mpg, by(foreign) stats(mean sd p25 p50 p75)
di "list2r list.RData"
list price mpg

log close

