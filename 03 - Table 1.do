********************************************************************************
* TABLE 1. SAMPLE CHARACTERISTICS BY TRANSITION-TO-RETIREMENT CLUSTER
********************************************************************************

use "analytic_dataset.dta", clear

* Keep one observation per participant for baseline/sample characteristics
bysort key (wave): keep if _n==1

* Confirm one record per participant
isid key

* Check final analytic sample size
count
// Expected N = 13,966

gen regression_sample = ///
    !missing(final_clusters, age, female, race, bplace, ///
             schlyrs, medu, fedu, childhood_health)

tab regression_sample

count if regression_sample==1
// Expected N = 13,879

********************************************************************************
* PREPARE TABLE
********************************************************************************

tempfile table1
tempname memhold

postfile `memhold' ///
    str60 characteristic ///
    str25 cluster1 ///
    str25 cluster2 ///
    str25 cluster3 ///
    str25 cluster4 ///
    str25 cluster5 ///
    str25 cluster6 ///
    str25 cluster7 ///
    str25 cluster8 ///
    str25 cluster9 ///
    str25 cluster10 ///
    using `table1', replace


********************************************************************************
* SAMPLE SIZE
********************************************************************************

local row `" "'

forvalues c = 1/10 {
    quietly count if final_clusters==`c'
    local c`c' = string(r(N), "%9.0fc")
}

post `memhold' ///
    ("No. of participants") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* AGE, MEAN (SD)
********************************************************************************

forvalues c = 1/10 {
    quietly summarize base_age if final_clusters==`c'
    local c`c' = string(r(mean), "%4.1f") + " (" + ///
        string(r(sd), "%4.1f") + ")"
}

post `memhold' ///
    ("Age, years, mean (SD)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* FEMALE, N (%)
********************************************************************************

forvalues c = 1/10 {
    quietly count if final_clusters==`c'
    local denom = r(N)

    quietly count if final_clusters==`c' & female==1
    local pct = 100*r(N)/`denom'

    local c`c' = string(r(N), "%9.0fc") + " (" + ///
        string(`pct', "%4.1f") + ")"
}

post `memhold' ///
    ("Female, No. (%)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* RACE/ETHNICITY, N (%)
********************************************************************************

foreach r in 0 1 2 3 {

    if `r'==0 local rowname "NH White"
    if `r'==1 local rowname "NH Black"
    if `r'==2 local rowname "Hispanic"
    if `r'==3 local rowname "NH other"

    forvalues c = 1/10 {
        quietly count if final_clusters==`c' & !missing(race)
        local denom = r(N)

        quietly count if final_clusters==`c' & race==`r'
        local pct = 100*r(N)/`denom'

        local c`c' = string(r(N), "%9.0fc") + " (" + ///
            string(`pct', "%4.1f") + ")"
    }

    post `memhold' ///
        ("    `rowname'") ///
        ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
        ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")
}


********************************************************************************
* BIRTHPLACE, N (%)
********************************************************************************

foreach b in 0 1 2 3 {

    if `b'==0 local rowname "U.S., not South"
    if `b'==1 local rowname "Southern U.S."
    if `b'==2 local rowname "Born outside U.S."
    if `b'==3 local rowname "U.S., region unspecified"

    forvalues c = 1/10 {
        quietly count if final_clusters==`c' & !missing(bplace)
        local denom = r(N)

        quietly count if final_clusters==`c' & bplace==`b'
        local pct = 100*r(N)/`denom'

        local c`c' = string(r(N), "%9.0fc") + " (" + ///
            string(`pct', "%4.1f") + ")"
    }

    post `memhold' ///
        ("    `rowname'") ///
        ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
        ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")
}


********************************************************************************
* MOTHER'S EDUCATION, MEAN (SD)
********************************************************************************

forvalues c = 1/10 {
    quietly summarize medu if final_clusters==`c'
    local c`c' = string(r(mean), "%4.1f") + " (" + ///
        string(r(sd), "%4.1f") + ")"
}

post `memhold' ///
    ("Mother's education, years, mean (SD)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* MISSING MOTHER'S EDUCATION, N (%)
********************************************************************************

forvalues c = 1/10 {
    quietly count if final_clusters==`c'
    local denom = r(N)

    quietly count if final_clusters==`c' & missing_medu==1
    local pct = 100*r(N)/`denom'

    local c`c' = string(r(N), "%9.0fc") + " (" + ///
        string(`pct', "%4.1f") + ")"
}

post `memhold' ///
    ("    Missing, No. (%)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* FATHER'S EDUCATION, MEAN (SD)
********************************************************************************

forvalues c = 1/10 {
    quietly summarize fedu if final_clusters==`c'
    local c`c' = string(r(mean), "%4.1f") + " (" + ///
        string(r(sd), "%4.1f") + ")"
}

post `memhold' ///
    ("Father's education, years, mean (SD)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* MISSING FATHER'S EDUCATION, N (%)
********************************************************************************

forvalues c = 1/10 {
    quietly count if final_clusters==`c'
    local denom = r(N)

    quietly count if final_clusters==`c' & missing_fedu==1
    local pct = 100*r(N)/`denom'

    local c`c' = string(r(N), "%9.0fc") + " (" + ///
        string(`pct', "%4.1f") + ")"
}

post `memhold' ///
    ("    Missing, No. (%)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* PARTICIPANT EDUCATION, MEAN (SD)
********************************************************************************

forvalues c = 1/10 {
    quietly summarize schlyrs if final_clusters==`c'
    local c`c' = string(r(mean), "%4.1f") + " (" + ///
        string(r(sd), "%4.1f") + ")"
}

post `memhold' ///
    ("Participant education, years, mean (SD)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* CHILDHOOD HEALTH, MEAN (SD)
********************************************************************************

forvalues c = 1/10 {
    quietly summarize childhood_health if final_clusters==`c'
    local c`c' = string(r(mean), "%4.1f") + " (" + ///
        string(r(sd), "%4.1f") + ")"
}

post `memhold' ///
    ("Childhood health, mean (SD)") ///
    ("`c1'") ("`c2'") ("`c3'") ("`c4'") ("`c5'") ///
    ("`c6'") ("`c7'") ("`c8'") ("`c9'") ("`c10'")


********************************************************************************
* CLOSE AND EXPORT TABLE
********************************************************************************

postclose `memhold'

use `table1', clear

label variable characteristic "Characteristic"
label variable cluster1  "1"
label variable cluster2  "2"
label variable cluster3  "3"
label variable cluster4  "4"
label variable cluster5  "5"
label variable cluster6  "6"
label variable cluster7  "7"
label variable cluster8  "8"
label variable cluster9  "9"
label variable cluster10 "10"

export excel using "$figures/Table1_transition_retirement.xlsx", ///
    firstrow(varlabels) replace
