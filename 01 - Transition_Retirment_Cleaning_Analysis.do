*****THE TRANSITION TO RETIREMENT AND DEMENTIA RISK: DATA CLEANING AND ANALYSIS
*****First author: Lucia Pacca
*****Code Reviewer: Jilly Hebert

********************************************************************************
* CODE INDEX
*
* 1.  Define project directory ........................................ line 39
* 2.  Prepare HRS Tracker file ........................................ line 52
* 3.  Define sequence-analysis sample ................................. line 68
* 4.  Define sequence-analysis states ................................. line 126
* 5.  Reshape sequence data for analysis in R ......................... line 169
* 6.  Import and label main Optimal Matching cluster solution ......... line 183
* 7.  Import and label Hamming sensitivity cluster solution ........... line 235
* 8.  Merge cluster assignments and verify cluster ordering ........... line 289
* 9.  Prepare RAND interview-status data .............................. line 324
* 10. Merge dementia outcomes and construct longitudinal follow-up ..... line 379
* 11. Prepare and merge analytic covariates ............................ line 528
*       a. Parental education .......................................... line 532
*       b. Childhood health ............................................ line 598
*       c. Alternative dementia classification ......................... line 645
* 12. Define analytic covariates and check final sample ................ line 674
* 13. Main analysis: RCC-IPW weighted GEE .............................. line 850
* 14. Export and plot main results ..................................... line 937
* 15. Sensitivity analyses ............................................. line 980
*       a. Hamming distance ............................................ line 984
*       b. Alternative dementia probability score ...................... line 1008
********************************************************************************

//We are using the cleaned employment status trajectories from the employment/non-employment trajectories paper [Core Survey Only]. Data cleaning for these trajectories has already been code reviewed for the employment/non-employment paper. 
//Differences with the prior analysis: (1)This analysis is restricted to the transition-to-retirement period (age 51-75); (2) This analysis only uses information from Core Surveys 1992-2020. We are not including LHMS, since merging prospective and retrospective data can be problematic.

//For each participant, we kept employment states from ages 51–75, corresponding to the exposure period. We used two sources of employment information: current employment status reported in each core survey wave, and retrospective reports of previous/past jobs, which are used to fill remaining gaps.

//For retrospective past-job states, when the same age was reported in more than one survey wave, we prioritized the first report, because it is closer in time to the relevant age and therefore likely less affected by recall error. An exception is made for full-time versus part-time work: because part-time status is not consistently reported for every job, the code used the modal value across reports when distinguishing full-time work from part-time work.

//After creating one unique current-state and one unique retrospective-state variable for each age, the data are reshaped into long format with one row per participant-age. Small gaps in current employment status were filled when the preceding and following ages had the same state, with additional edge rules for ages 51 and 75. Finally, the code created the merged final state variable by using the cleaned current employment state when available and filling remaining missing values with retrospective past-job information. We will start from this cleaned trajectories dataset, "final_states.dta", which is already restricted to the sample that we weill be using for the sequence analysis (13,966 participants).

********************************************************************************
* 1. DEFINE PROJECT DIRECTORY
*
* Set the project directory once. All input and output datasets used in this
* do-file are stored in the data subfolder.
********************************************************************************

global project "/Users/lpacca/Research Projects/02 - Transition to Retirement/Code Review for Jilly" //Jilly, change this directory if needed
global data "$project/data"
global figures "$project/figures"

cd "$data"

********************************************************************************
* 2. PREPARE HRS TRACKER FILE FOR LATER MERGES
********************************************************************************

use "$data/trk2022tr_r.dta", clear

* Create participant identifier used throughout the analysis
capture drop key
gen key = HHID + PN

sort key

* Save prepared Tracker file in data directory
save "$data/trk2022tr_r_key.dta", replace


********************************************************************************
* 3. DEFINE SEQUENCE ANALYSIS SAMPLE
*
* Sequence analysis sample = participants with at least one non-missing
* dementia probability score at age >=76.
*
* Birth year is merged with the Gianattasio dementia probability dataset to
* calculate age at each assessment. Participants are retained if they have at
* least one dementia probability score after age 75.
*
* No exclusions are made based on exposure data. Missing employment/retirement
* information during the exposure period is retained as an "unreported" state
* in the trajectories.
********************************************************************************

*******Keep birth year variable 
use "/Users/lpacca/Library/CloudStorage/Box-Box/02 - Transition to Retirement/final_states.dta", clear
replace key=HHID+PN
keep key BIRTHYR
sort key
bysort key:gen dup_id= cond(_N==1,0,_n)
drop if dup_id>1
save "/Users/lpacca/Research Projects/02 - Transition to Retirement/Code Review for Jilly/data/birthyear.dta", replace

********Define sequence analysis sample
use "Gianattasio_demprob.dta", clear
sort key

egen tag = tag(key)
count if tag==1 //18,591 participants had dementia probability scores starting at age 70. 

capture drop _merge
merge m:1 key using "birthyear.dta" //merge Gianattasio dataset with birthyear, which we will use
drop if _merge==2 //Drop participants in birthyear file who are not present in Gianattasio data
destring HRS_year, replace
gen age=HRS_year-BIRTHYR

* Identify observations with a non-missing dementia probability score at age >=76
gen age_75plus=1 if age>75 & expert_p!=.

* Identify participants with at least assessment at age >=76
sort key HRS_year
by key: egen mean_age75plus=mean(age_75plus) 
gen hhidpn=key
sort hhidpn
save "Gianattasio_demprob_withage.dta", replace

* Retain one observation per participant to create ID-level sample file
capture drop dup_id
bysort hhidpn:gen dup_id= cond(_N==1,0,_n)
drop if dup_id>1
save "Gianattasio_after75_nodup.dta", replace //removing duplicates to have list of ids
drop if mean_age75plus==.
drop _merge
sort key
count //13,966 participants with a dementia probability score after age 75 --> sequence analysis sample
 

 *******************************************************************************
*4. DEFINE SEQUENCE ANALYSIS STATES
* The original employment trajectory data distinguish several reasons for being
* out of the labor force. Because this analysis focuses on employment type and
* retirement timing, these reasons are combined into a single "not working"
* state.
 *******************************************************************************

use "final_states.dta", clear
sort key
merge m:1 key using "Gianattasio_after75_nodup.dta"
tab _merge //all merges should be equal to 3

* Confirm that participants in the employment trajectory file are included
* in the dementia probability sample. final_states.dta is already restricted
* to the 13,966 participants used for sequence analysis.

* Recode original employment/non-employment states into the six states used
* in the transition-to-retirement sequence analysis
capture drop state
gen state = 1 if state_final_merged==1                    // Full-time work
replace state = 2 if state_final_merged==7                // Part-time work
replace state = 3 if state_final_merged==4                // Retired
replace state = 4 if state_final_merged==5                // Disabled
replace state = 5 if inlist(state_final_merged, 2, 3, 6) // Not working
replace state = 6 if state==.                             // Unreported

* Assign descriptive labels to sequence states
label define state_lbl ///
    1 "Full-time work" ///
    2 "Part-time work" ///
    3 "Retired" ///
    4 "Disabled" ///
    5 "Not working" ///
    6 "Unreported"

label values state state_lbl

* Declare sequence data in long format
sqset state key age
capture drop _merge
save, replace

********************************************************************************
* 5. RESHAPE SEQUENCE DATA TO WIDE FORMAT FOR ANALYSIS IN R
********************************************************************************
keep state key age
sort key age
reshape wide state, i(key) j(age)
count //13,966 participants
save "final_states_wide.dta", replace 

********************************************************************************
* NOTE FOR JILLY
*Please review the R sequence analysis code before moving to the next section.
********************************************************************************

********************************************************************************
* 6. IMPORT, REORDER, AND LABEL MAIN 10-CLUSTER OM SOLUTION
*
* Sequence and cluster analyses are conducted in R. The main analysis uses
* Optimal Matching dissimilarities and Ward's hierarchical agglomerative
* clustering. The selected solution contains 10 clusters.
*
* Cluster numbers generated in R are arbitrary. Clusters are reordered here
* for presentation and interpretation; cluster membership is unchanged.
********************************************************************************

import delimited "clusters_10_om.csv", encoding(UTF-8) clear

* Check original cluster assignments and sizes
tab cluster

* Reorder clusters for presentation
gen clus_10 = 1 if cluster==1
replace clus_10 = 2 if cluster==3
replace clus_10 = 3 if cluster==5
replace clus_10 = 4 if cluster==8
replace clus_10 = 5 if cluster==2
replace clus_10 = 6 if cluster==10
replace clus_10 = 7 if cluster==6
replace clus_10 = 8 if cluster==9
replace clus_10 = 9 if cluster==4
replace clus_10 = 10 if cluster==7

* Assign descriptive cluster labels
capture label drop clus_10_lbl
label define clus_10_lbl ///
    1  "Retired after full-time work at ages 62-65" ///
    2  "Retired before age 60" ///
    3  "Retired after full-time work after age 70" ///
    4  "Full-time to part-time to retired after age 70" ///
    5  "Retired after part-time work at ages 62-65" ///
    6  "Continuous part-time work" ///
    7  "Mostly not working" ///
    8  "Mostly disability" ///
    9  "Unreported, then retired at ages 62-65" ///
    10 "Mostly unreported"

label values clus_10 clus_10_lbl

* Check reordered and labeled solution
tab clus_10

sort id
rename id key
tostring key, replace format(%09.0f)
save "clusters_10.dta", replace

********************************************************************************
* 7. IMPORT, REORDER, AND LABEL 11-CLUSTER HAMMING SENSITIVITY SOLUTION
*
* As a sensitivity analysis, sequence dissimilarities are calculated using
* Hamming distance instead of Optimal Matching. Ward's hierarchical
* agglomerative clustering is then used to obtain an 11-cluster solution.
*
* Clusters are reordered to facilitate comparison with the main analysis;
* cluster membership is unchanged.
********************************************************************************

import delimited "clusters_11_ham.csv", encoding(UTF-8) clear

* Check original cluster assignments and sizes
tab cluster

* Reorder clusters for presentation
gen clus_11 = 1 if cluster==1
replace clus_11 = 2 if cluster==4
replace clus_11 = 3 if cluster==6
replace clus_11 = 4 if cluster==5
replace clus_11 = 5 if cluster==2
replace clus_11 = 6 if cluster==11
replace clus_11 = 7 if cluster==3
replace clus_11 = 8 if cluster==9
replace clus_11 = 9 if cluster==10
replace clus_11 = 10 if cluster==7
replace clus_11 = 11 if cluster==8

* Assign descriptive cluster labels
capture label drop clus_11_lbl
label define clus_11_lbl ///
    1  "Retired after full-time work at ages 62-65" ///
    2  "Retired before age 60" ///
    3  "Retired after full-time work after age 70" ///
    4  "Full-time to part-time to retired after age 70" ///
    5  "Retired after part-time work at ages 62-65" ///
    6  "Continuous part-time work" ///
    7  "Mostly not working" ///
    8  "Mostly disability" ///
    9  "Unreported to full-time work, then retired after age 70" ///
    10 "Unreported, then retired at ages 62-65" ///
    11 "Mostly unreported"

label values clus_11 clus_11_lbl

* Check reordered and labeled solution
tab clus_11

sort id
rename id key
tostring key, replace format(%09.0f)
save "clusters_11_ham.dta", replace

********************************************************************************
* 8. MERGE CLUSTER ASSIGNMENTS WITH SEQUENCE DATA AND VERIFY CLUSTER ORDERING
********************************************************************************

use "final_states_wide.dta", clear

* Merge main 10-cluster OM solution
merge 1:1 key using "clusters_10.dta"
tab _merge
assert _merge==3
drop _merge

* Merge 11-cluster Hamming sensitivity solution
merge 1:1 key using "clusters_11_ham.dta"
tab _merge
assert _merge==3
drop _merge


********************************************************************************
* CHECK REORDERED MAIN CLUSTER SOLUTION
********************************************************************************

* Reshape to long format for Stata sequence commands
reshape long state, i(key) j(age)

* Visually confirm that reordered cluster assignments correspond to the
* expected sequence patterns
sqindexplot, color(ebblue eltblue green purple gold gray) by(clus_10)

* Return to participant-level wide format
reshape wide state, i(key) j(age)

save "clusters.dta", replace

********************************************************************************
* 9. PREPARE RAND INTERVIEW STATUS DATA FOR INVERSE PROBABILITY WEIGHTS
*
* RAND interview-status variables are used to identify whether respondents
* were interviewed, had died, or were otherwise not interviewed at each wave.
* These variables are subsequently used to construct inverse probability
* weights accounting for differential follow-up due to death or dropout.
********************************************************************************

clear matrix
clear mata
clear
set maxvar 30000

use "randhrs1992_2022v1.dta", clear

* Retain participant ID, wave-specific interview status, date of death,
* and birth year
keep rahhidpn r*iwstat raddate rabyear

* Rename wave-specific interview-status variables to facilitate reshaping
rename r1iwstat  iwstat1
rename r2iwstat  iwstat2
rename r3iwstat  iwstat3
rename r4iwstat  iwstat4
rename r5iwstat  iwstat5
rename r6iwstat  iwstat6
rename r7iwstat  iwstat7
rename r8iwstat  iwstat8
rename r9iwstat  iwstat9
rename r10iwstat iwstat10
rename r11iwstat iwstat11
rename r12iwstat iwstat12
rename r13iwstat iwstat13
rename r14iwstat iwstat14
rename r15iwstat iwstat15
rename r16iwstat iwstat16

* Reshape from one row per participant to one row per participant-wave
egen id = group(rahhidpn)
reshape long iwstat, i(id) j(wave)

rename rahhidpn hhidpn

* Derive calendar year of death
format raddate %d
gen yrdeath = year(raddate)

sort hhidpn wave

* Create string version of wave for subsequent merges if needed
tostring wave, gen(wave_s)

save "randhrs1992_2022v1_IWSTAT.dta", replace

********************************************************************************
* 10. MERGE OUTCOME DATA WITH CLUSTER ASSIGNMENTS
*
* Merge dementia probability outcomes with the transition-to-retirement cluster
* assignments. Outcome follow-up begins at age 76.
********************************************************************************

use "Gianattasio_demprob.dta", clear

* Calculate age at each outcome assessment
gen age = HRS_year - BIRTHYR

* Merge cluster assignments for participants in the sequence analysis sample
sort key
merge m:1 key using "clusters.dta"

keep if _merge==3
drop _merge

* Restrict outcome observations to age 76 and older
drop if age < 76

* Check number of unique participants retained (should be 13,966)
egen tag_participant = tag(key)
count if tag_participant==1
drop tag_participant


********************************************************************************
* MERGE WAVE-SPECIFIC INTERVIEW STATUS
*
* Add RAND interview-status records so that subsequent weighting models can
* identify interview, dropout, and death status over follow-up.
********************************************************************************

* Create HRS wave number corresponding to interview year
gen wave = (HRS_year - 1990)/2 if inrange(HRS_year, 1992, 2022)

tostring wave, gen(wave_s)

gen hhidpn=key

sort hhidpn wave_s

merge 1:1 hhidpn wave_s using "randhrs1992_2022v1_IWSTAT.dta"

* Retain participants who matched the RAND interview-status data in at least
* one wave. Wave records corresponding to death or non-interview are retained.
gen matched_rand = (_merge==3)
bysort hhidpn: egen ever_matched_rand = max(matched_rand)

keep if ever_matched_rand==1

drop matched_rand ever_matched_rand _merge

********************************************************************************
* COMPLETE AGE AND YEAR INFORMATION FOR RAND-ONLY WAVE RECORDS
*
* RAND contributes additional person-wave records for waves in which outcome
* data are absent. Reconstruct interview year and age for these records so that
* follow-up can be restricted consistently to age 76 and older.
********************************************************************************

* Recover calendar year from HRS wave number where outcome year is missing
replace HRS_year = 1990 + 2*wave if missing(HRS_year)

sort hhidpn HRS_year

* Forward-fill age using elapsed calendar time
by hhidpn: replace age = age[_n-1] + ///
    (HRS_year - HRS_year[_n-1]) ///
    if missing(age) & !missing(age[_n-1])

* Backward-fill age where needed
gsort hhidpn -HRS_year
by hhidpn: replace age = age[_n-1] - ///
    (HRS_year[_n-1] - HRS_year) ///
    if missing(age) & !missing(age[_n-1])

sort hhidpn HRS_year

* Outcome follow-up begins at age 76
drop if age < 76


********************************************************************************
* CLEAN VARIABLES AND STANDARDIZE PARTICIPANT IDENTIFIER
********************************************************************************

capture drop BIRTHYR DEGREE GENDER RACE SCHLYRS HHID PN BIRTHMO ///
    EXDEATHMO EXDEATHYR FIRSTIW HISPANIC IMMGYEAR USBORN ///
    AAGE BAGE CAGE DAGE EAGE FAGE GAGE HAGE KAGE MAGE NAGE ///
    OAGE PAGE QAGE

capture drop key
gen key = hhidpn

sort key

********************************************************************************
* ADD COVARIATES AND SAVE ANALYTIC DATASET
*
* Merge participant-level sociodemographic covariates from the HRS Tracker
* file with the outcome and cluster data.
********************************************************************************

********************************************************************************
* ADD COVARIATES
********************************************************************************

merge m:1 key using "trk2022tr_r_key.dta"

* Retain observations from participants in the analytic sample
drop if _merge==2
drop _merge

* Recode missing years of education
tab SCHLYRS, missing
replace SCHLYRS = . if SCHLYRS==99

********************************************************************************
* SAVE FINAL ANALYTIC DATASET
********************************************************************************

sort key wave
save "analytic_dataset.dta", replace

********************************************************************************
* CHECK FINAL ANALYTIC SAMPLE
********************************************************************************

* Count unique participants in final analytic dataset
egen tag_participant = tag(key)
count if tag_participant==1
// Expected N = 13,966 participants

drop tag_participant

* Check number of observations contributed by each participant
bysort key: gen n_obs = _N
tab n_obs
drop n_obs

* Check distribution of observations across HRS waves
tab wave, missing

sort key wave
save "analytic_dataset.dta", replace

********************************************************************************
* 11. PREPARE AND MERGE ANALYTIC COVARIATES
********************************************************************************

********************************************************************************
* 11a. PREPARE RAND PARENTAL EDUCATION COVARIATES
*
* Obtain mother's and father's education from RAND. Missing parental education
* values are replaced with the sample mean, and missingness indicators are
* created for use in regression models.
********************************************************************************

use "randhrs1992_2022v1.dta", clear

keep rameduc rafeduc hhidpn raracem rahispan ragender rabplace raedyrs

* Create participant identifier consistent with the analytic dataset
gen key = string(hhidpn, "%09.0f")
sort key

* Recode RAND extended missing values to standard missing
foreach var of varlist rameduc rafeduc {
    replace `var' = . if inlist(`var', .a, .d, .e, .l, .m, .n, .r, .s, .w, .x)
}

* Calculate sample means for parental education
egen rameduc_mean = mean(rameduc)
egen rafeduc_mean = mean(rafeduc)

* Create mean-imputed parental education variables
gen medu = rameduc
replace medu = rameduc_mean if missing(medu)

gen fedu = rafeduc
replace fedu = rafeduc_mean if missing(fedu)

* Create indicators for originally missing parental education
gen missing_medu = missing(rameduc)
gen missing_fedu = missing(rafeduc)

save "rand_covariates.dta", replace

********************************************************************************
* INCORPORATE AHEAD-SPECIFIC PARENTAL EDUCATION VALUES
*
* For respondents in the AHEAD cohort, parental education may be coded as
* 7.5 or 8.5 years in RAND. Replace these values with the previously imputed
* parental education measures available in cses_measures.dta.
********************************************************************************

use "rand_covariates.dta", clear

* Convert participant identifier to 9-character string
tostring key, replace format(%09.0f)

merge 1:1 key using "cses_measures.dta"
tab _merge

* Retain RAND participants; cses-only observations are not needed
drop if _merge==2
drop _merge

* Replace AHEAD grouped parental education values with imputed years
replace medu = myrs if inlist(medu, 7.5, 8.5)
replace fedu = fyrs if inlist(fedu, 7.5, 8.5)

sort key
drop hhidpn
save "rand_covariates.dta", replace

********************************************************************************
* 11b. PREPARE CHILDHOOD HEALTH COVARIATES
*
* Import the HRS Childhood Health and Family data and create the participant
* identifier used throughout the analysis.
*
* NOTE: The first line of AGGCHLDFH2016A_R.dct should point to:
* "ChildhoodHealthAndFamily/AGGCHLDFH2016A_R.da"
********************************************************************************

clear

* Import childhood health and family data
infile using "ChildhoodHealthAndFamily/AGGCHLDFH2016A_R.dct"

* Confirm that observations were successfully imported
count

* Create participant identifier consistent with the analytic dataset
gen key = HHID + PN

sort key
save "childhood_health.dta", replace


********************************************************************************
* ADD PARENTAL EDUCATION AND CHILDHOOD HEALTH TO ANALYTIC DATASET
********************************************************************************

use "analytic_dataset.dta", clear

* Merge RAND parental education and demographic covariates
merge m:1 key using "rand_covariates.dta"
tab _merge

* Retain observations from participants in the analytic dataset
drop if _merge==2
drop _merge

* Merge childhood health and family covariates
merge m:1 key using "childhood_health.dta"
tab _merge

* Retain observations from participants in the analytic dataset
drop if _merge==2
drop _merge

*******************************************************************************
* 11c. ADD HUDOMIET DEMENTIA CLASSIFICATION
*
* Merge wave-specific dementia classification for HRS waves available in the
* Hudomiet dementia dataset.
********************************************************************************

* Create participant-wave identifier for merge
gen hhidpn_wave = hhidpn + wave_s
sort hhidpn_wave

merge 1:1 hhidpn_wave using "Hudomiet_Dementia_HRS_2000-2016.dta"
tab _merge

* Retain observations from participants in the analytic dataset
drop if _merge==2
drop _merge

********************************************************************************
* CHECK FOR DUPLICATE PARTICIPANT-WAVE OBSERVATIONS
********************************************************************************

* Confirm that there is no more than one observation per participant and wave
capture drop dup_id_wave
bysort key wave: gen dup_id_wave = cond(_N==1, 0, _n)

tab dup_id_wave


********************************************************************************
* 12. DEFINE ANALYTIC COVARIATES
********************************************************************************

********************************************************************************
* GENDER
* Female is the reference category in subsequent regression models.
********************************************************************************

capture drop female

tab ragender, missing

gen female = 0 if ragender==1
replace female = 1 if ragender==2

label define female_lbl 0 "Male" 1 "Female", replace
label values female female_lbl

tab female, missing


********************************************************************************
* RACE/ETHNICITY
* Non-Hispanic White is the reference category in subsequent regression models.
********************************************************************************

capture drop race

gen race = 0 if raracem==1 & rahispan==0       // Non-Hispanic White
replace race = 1 if raracem==2 & rahispan==0  // Non-Hispanic Black
replace race = 2 if rahispan==1                // Hispanic
replace race = 3 if raracem==3 & rahispan==0  // Non-Hispanic other

capture label drop race_lbl
label define race_lbl ///
    0 "NH White" ///
    1 "NH Black" ///
    2 "Hispanic" ///
    3 "NH other"

label values race race_lbl

tab race, missing


********************************************************************************
* YEARS OF EDUCATION
********************************************************************************

tab raedyrs, missing

capture drop schlyrs
gen schlyrs = raedyrs

* RAND groups respondents with 0-5 years of education; assign 5 years
* to this category for analysis
replace schlyrs = 5 if raedyrs<=5 & !missing(raedyrs)


********************************************************************************
* BASELINE AGE
********************************************************************************

capture drop base_age

* Age at first observation in the analytic follow-up period
bysort key: egen base_age = min(age)


********************************************************************************
* BIRTHPLACE
********************************************************************************

tab rabplace, missing

* Recode RAND extended missing value
replace rabplace = . if rabplace==.m

capture drop bplace

gen bplace = 0 if !inrange(rabplace, 5, 7) & ///
    rabplace!=10 & rabplace!=11 & !missing(rabplace)   // U.S., not South

replace bplace = 1 if inlist(rabplace, 5, 6, 7)       // Southern U.S.
replace bplace = 2 if rabplace==11                    // Born outside U.S.
replace bplace = 3 if rabplace==10                    // U.S. region unspecified

capture label drop bplace_lbl
label define bplace_lbl ///
    0 "U.S., not South" ///
    1 "Southern U.S." ///
    2 "Born outside U.S." ///
    3 "U.S., region unspecified"

label values bplace bplace_lbl

tab rabplace bplace, missing


********************************************************************************
* CHILDHOOD HEALTH
********************************************************************************

capture drop childhood_health

gen childhood_health = RTHLTHCH

* Recode "don't know/not ascertained" and "refused" responses as missing
replace childhood_health = . if inlist(childhood_health, 8, 9)

capture label drop childhood_health_lbl
label define childhood_health_lbl ///
    1 "Excellent" ///
    2 "Very good" ///
    3 "Good" ///
    4 "Fair" ///
    5 "Poor"

label values childhood_health childhood_health_lbl

tab childhood_health, missing


********************************************************************************
* RESTRICT OUTCOME FOLLOW-UP PERIOD
********************************************************************************

* Dementia probability outcomes are available through 2020; RAND 2022 records
* were retained previously to characterize interview status but are not included
* as outcome observations.
drop if wave==16


********************************************************************************
* CREATE PARTICIPANT-LEVEL CLUSTER VARIABLES
********************************************************************************

* Cluster assignment is constant within participant. Carry the main OM and
* Hamming sensitivity cluster assignments across all participant-wave records.
capture drop final_clusters final_clusters_11

bysort key: egen final_clusters = mean(clus_10)
bysort key: egen final_clusters_11 = mean(clus_11)

* Apply descriptive labels defined previously
label values final_clusters clus_10_lbl
label values final_clusters_11 clus_11_lbl

* Verify that cluster assignment is constant within participant
bysort key: assert final_clusters == final_clusters[1]
bysort key: assert final_clusters_11 == final_clusters_11[1]

********************************************************************************
* FINAL SAMPLE CHECKS
********************************************************************************

* Count unique participants in the final analytic dataset
capture drop tag_participant
egen tag_participant = tag(key)
count if tag_participant==1
// Expected N = 13,966

drop tag_participant

* Verify that each participant has only one observation per wave
isid key wave
//No error should be detected. If Stata detects an error, it means that there are duplicates.

********************************************************************************
* SAVE ANALYTIC DATASET
********************************************************************************

sort key wave
save "analytic_dataset.dta", replace

********************************************************************************
* 13. REGRESSION: DEMENTIA PROBABILITY
********************************************************************************

* Create numeric participant ID
capture drop id
egen id = group(hhidpn)

* Identify first wave with non-missing dementia probability
sort id HRS_year

by id: gen first_nonmiss = !missing(expert_p) & ///
    sum(!missing(expert_p)) == 1

gen year_first_nonmiss = HRS_year if first_nonmiss == 1

by id: egen mean_firstnmiss_year = mean(year_first_nonmiss)

* Remove observations occurring before the first observed dementia outcome
drop if missing(expert_p) & HRS_year < mean_firstnmiss_year


********************************************************************************
* CREATE TIME INDEX
********************************************************************************

sort id HRS_year

capture drop timeidxvar
bysort id (HRS_year): gen timeidxvar = _n


********************************************************************************
* DECLARE PANEL DATA
********************************************************************************

xtset id HRS_year

* Check panel structure
isid id HRS_year

* Check time index
bysort id (HRS_year): assert timeidxvar == _n

* Number of participants entering regression dataset
egen tag_id = tag(id)
count if tag_id
drop tag_id

* Outcome availability
count
count if !missing(expert_p)
count if missing(expert_p)

********************************************************************************
* MAIN ANALYSIS: RUN RCC-IPW WEIGHTED GEE
********************************************************************************

capture drop _merge

xtrccipw expert_p, ///
    idvar(id) ///
    timevar(HRS_year) ///
    timeidxvar(timeidxvar) ///
    generate(ipw) ///
    trtimevar(raddate) ///
    linkfxn(logit) ///
    glmvars( ///
        age ///
        i.wave ///
        i.final_clusters ///
        female ///
        i.race ///
        schlyrs ///
        medu ///
        fedu ///
        i.bplace ///
        missing_medu ///
        missing_fedu ///
        childhood_health ///
    ) ///
    glmfamily(binomial) ///
    glmlink(logit)

estimate store expert_p


********************************************************************************
* 14. EXPORT RESULTS
********************************************************************************

outreg2 using expert_p.doc, ///
    dec(2) ///
    eform ///
    cti(odds ratio) ///
    stats(coef ci pval) ///
    sideway ///
    alpha(0.001, 0.01, 0.05) ///
    label ///
    replace
	
coefplot expert_p, ///
    eform ///
    keep(2.final_clusters 3.final_clusters 4.final_clusters ///
         5.final_clusters 6.final_clusters 7.final_clusters ///
         8.final_clusters 9.final_clusters 10.final_clusters) ///
    coeflabels( ///
        2.final_clusters = "Retired before age 60" ///
        3.final_clusters = "Retired after FT work after 70" ///
        4.final_clusters = "FT → PT → retired after 70" ///
        5.final_clusters = "Retired after PT work, ages 62–65" ///
        6.final_clusters = "Continuous PT work" ///
        7.final_clusters = "Mostly not working" ///
        8.final_clusters = "Mostly disability" ///
        9.final_clusters = "Unreported → retired, ages 62–65" ///
        10.final_clusters = "Mostly unreported") ///
    xline(1, lpattern(dash)) ///
    xtitle("Odds Ratio for Dementia") ///
    ytitle("") ///
    ylabel(, labsize(small)) ///
    ciopts(recast(rcap)) ///
    legend(off)

graph export "expert_p_clusters.png", replace width(2400)
graph export "expert_p_clusters.pdf", replace

* Export plot
graph export "expert_p_clusters.png", replace width(2400)
graph export "expert_p_clusters.pdf", replace

********************************************************************************
* 15. SENSITIVITY ANALYSES
********************************************************************************

********************************************************************************
* 15a. SENSITIVITY ANALYSIS 1: HAMMING DISTANCE
*
* Repeat the main analysis using the 11-cluster solution obtained from sequence
* analysis using Hamming distance rather than Optimal Matching.
********************************************************************************

capture drop ipw_ham

xtrccipw expert_p, ///
    idvar(id) ///
    timevar(HRS_year) ///
    timeidxvar(timeidxvar) ///
    generate(ipw_ham) ///
    trtimevar(raddate) ///
    linkfxn(logit) ///
    glmvars(age i.wave i.final_clusters_11 female i.race schlyrs ///
            medu fedu i.bplace missing_medu missing_fedu childhood_health) ///
    glmfamily(binomial) ///
    glmlink(logit)

estimates store hamming


********************************************************************************
* 15b. SENSITIVITY ANALYSIS 2: ALTERNATIVE DEMENTIA PROBABILITY SCORE
*
* Repeat the main analysis using the Hurd et al. dementia probability score
* (PrDem) rather than the Gianattasio expert-model dementia probability score.
********************************************************************************

capture drop ipw_hud

xtrccipw PrDem, ///
    idvar(id) ///
    timevar(HRS_year) ///
    timeidxvar(timeidxvar) ///
    generate(ipw_hud) ///
    trtimevar(raddate) ///
    linkfxn(logit) ///
    glmvars(age i.wave i.final_clusters female i.race schlyrs ///
            medu fedu i.bplace missing_medu missing_fedu childhood_health) ///
    glmfamily(binomial) ///
    glmlink(logit)

estimates store hurd


********************************************************************************
* PLOT SENSITIVITY ANALYSIS RESULTS
********************************************************************************

* Sensitivity analysis 1: Hamming distance
coefplot hamming, ///
    keep(*.final_clusters_11) ///
    xline(1, lcolor(black) lwidth(thin) lpattern(dash)) ///
    ytitle("") ///
    ylabel(, labsize(small)) ///
	xtitle("Odds Ratio for Dementia") ///
    baselevels ///
    eform

* Sensitivity analysis 2: Alternative dementia probability score
coefplot hurd, ///
    keep(*.final_clusters) ///
	xline(1, lcolor(black) lwidth(thin) lpattern(dash)) ///
    ytitle("") ///
    ylabel(, labsize(small)) ///
	xtitle("Odds Ratio for Dementia") ///
    baselevels ///
    eform
