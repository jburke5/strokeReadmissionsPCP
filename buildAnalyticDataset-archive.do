* load medpar.
cd "/Volumes/Arterial_Thrombploysis_DUA23127/New_Data/SAS_Files/"
use  BENE_ID MEDPAR_ID BENE_AGE_CNT-BENE_DSCHRG_STUS_CD PRVDR_NUM-SS_LS_SNF_IND_CD SRC_IP_ADMSN_CD-DSCHRG_DSTNTN_CD BENE_DEATH_DT-BENE_DEATH_DT_VRFY_CD ///
	LOS_DAY_CNT DRG_CD DGNS_1_CD-DGNS_25_CD SRGCL_PRCDR_1_CD-SRGCL_PRCDR_DT_CNT  using medpar2012.dta
	
* first get us down to the strokes...
gen primaryStroke = 0
local strokeDxList1 = `""43301", "43311", "43321", "43331", "43381", "43391", "43401", "43411", "43491""'
local strokeDxList2 = "436"
replace primaryStroke = 1 if inlist(DGNS_1_CD,`strokeDxList1') 
replace primaryStroke = 1 if  DGNS_1_CD == "`strokeDxList2'"

gen shortStay =   SS_LS_SNF_IND_CD == "S"	//keep short stays and get rid of SNF for now.

gen stortStayPrimaryStroke = shortStay & primaryStroke

bysort BENE_ID : egen hasAnyPrimaryStroke = max(stortStayPrimaryStroke)
keep if hasAnyPrimaryStroke== 1

save allHospitalizationsInPrimaryStrokePatients.dta, replace

preserve
keep if primaryStroke==1
save medpar2012PrimStrokes.dta, replace
restore

sort BENE_ID DSCHRG_DT
by BENE_ID  : gen strokeAdmitDateTemp = ADMSN_DT if stortStayPrimaryStroke==1
by BENE_ID  : gen strokeDischargeDateTemp = DSCHRG_DT if stortStayPrimaryStroke==1
by BENE_ID : egen strokeAdmitDate = min(strokeAdmitDateTemp)
by BENE_ID : egen strokeDischargeDate = min(strokeDischargeDateTemp)

gen priorToStroke = DSCHRG_DT < strokeDischargeDate
gen shortStayPriorToStroke = priorToStroke & shortStay

bysort BENE_ID : egen totalHospPriorStroke = sum (shortStayPriorToStroke)
drop if priorToStroke==1

gen IRF = 0
replace IRF = 1 if PRVDR_NUM >= "3025" & PRVDR_NUM <= "3099" 

gen SNF = 0 
replace SNF = 1 if PRVDR_NUM >=   "5000" & PRVDR_NUM<= "6499"

gen LTAC = 0
replace LTAC = 1 if  PRVDR_NUM >= "2000" & PRVDR_NUM <= "2299"

gen longTermHospital = IRF | SNF | LTAC | !shortStay


gen readmission = DSCHRG_DT > strokeDischargeDate 


sort BENE_ID DSCHRG_DT
by BENE_ID : gen possibleTransferOut =  (DSCHRG_DT[_n]  == ADMSN_DT[_n+1]) |  (DSCHRG_DT[_n] +1  == ADMSN_DT[_n+1] ) |  (DSCHRG_DT[_n] -1  == ADMSN_DT[_n+1] ) ///
	|  (DSCHRG_DT[_n] +2  == ADMSN_DT[_n+1] ) |  (DSCHRG_DT[_n] -2  == ADMSN_DT[_n+1] )
by BENE_ID : gen possibleTransferIn =  (DSCHRG_DT[_n-1]  == ADMSN_DT[_n]) |  (DSCHRG_DT[_n-1] +1  == ADMSN_DT[_n] )  |  (DSCHRG_DT[_n-1] -1  == ADMSN_DT[_n] ) ///
		|  (DSCHRG_DT[_n-1] +2  == ADMSN_DT[_n] ) |  (DSCHRG_DT[_n-1] -2  == ADMSN_DT[_n] )

by BENE_ID : gen shortStayBothOut = stortStayPrimaryStroke[_n] ==1 & !longTermHospital[_n+1]
by BENE_ID : gen shortStayBothIn = !longTermHospital[_n]  & !longTermHospital[_n-1] & primaryStroke[_n-1]

by BENE_ID : gen shortStayToShortStay = stortStay[_n] ==1 & shortStay[_n+1]==1


by BENE_ID : gen nextProviderType = shortStay[_n+1] if possibleTransferOut==1

gen probableTransferOut = possibleTransferOut & shortStayBothOut
gen probableTransferIn = possibleTransferIn & shortStayBothIn

by BENE_ID : gen transferOutHospital = PRVDR_NUM[_n-1] if probableTransferOut==1

gen shortStayReadmission = DSCHRG_DT > strokeDischargeDate & longTermHospital==0 & probableTransferIn==0


sort BENE_ID DSCHRG_DT
by BENE_ID : egen hasAnyReadmission = max(shortStayReadmission)
by BENE_ID : gen indexStroke = 1 if _n==1
by BENE_ID : gen readmissionIndex = _n if shortStayReadmission==1 & probableTransferIn ==0
by BENE_ID : egen lowestReadmission = min(readmissionIndex)
by BENE_ID : gen firstReadmission = 1 if _n==lowestReadmission
by BENE_ID : gen dateToFirstReadmitTemp = ADMSN_DT[_n] - DSCHRG_DT[1] if firstReadmission==1
by BENE_ID : egen dateToFirstReadmission = min(dateToFirstReadmitTemp)
by BENE_ID : gen firstReadmitDx = DGNS_1_CD[_n] if firstReadmission==1
gen readmit30 = dateToFirstReadmission < 30
gen readmit15 = dateToFirstReadmission < 15
gen readmit7 = dateToFirstReadmission < 7

forvalues i = 1/30	{
	by BENE_ID : replace firstReadmitDx = firstReadmitDx[_n+1]	if missing(firstReadmitDx)	//propogate them backwards...
}

replace indexStroke = 1 if probableTransferIn ==1	//we're going to call this the index event.
replace PRVDR_NUM = PRVDR_NUM if probableTransferIn == 1	//assign to the transfer out hospital, but will still mark as a transfer.

drop if probableTransferOut == 1 //we're going to drop the transfer outs...we'll adjust for trasnfer status and we'll assign hospitals to the initiating hopsital 
//for their randome effect
 
keep if DSCHRG_DSTNTN_CD == "01" | DSCHRG_DSTNTN_CD == "06"
gen homeHealth = DSCHRG_DSTNTN_CD == "06"

keep if indexStroke==1		//now we're down to short stay admissions that are discharged home...so we can pull out the readmisions next

drop hasAnyReadmission
gen hasAnyReadmission = 0
replace hasAnyReadmission = 1 if !missing(dateToFirstReadmission)


save baseHospitalizationsForAnalysis.dta, replace
sort BENE_ID DSCHRG_DT
by BENE_ID : gen indexForBene = _n
drop if indexForBene > 1


preserve
keep BENE_ID ADMSN_DT DSCHRG_DT
save strokeSampleBeneIDs.dta, replace
restore

clear 
use using ptbln12.dta
merge m:1 BENE_ID using strokeSampleBeneIDs, gen(hasMergedBeneID)

drop if hasMergedBeneID ==1 //only keepign clais that matched
* this drops the people that are in carrier and without a stroke (obvious)
save strokeCarrierLineFilesMerged.dta, replace

mrge 

drop if hasMergedBeneID ==2	//also drop the strokes where there aren't any carrier claims
* there "should" be carrier claims for the hospitalization itself. if theyr'e aren't
* i'm worried that we'd also miss teh outpatient claims even if they existed.



gen visitPriorToStroke = CLM_THRU_DT < ADMSN_DT
gen visitAfterStroke = CLM_THRU_DT > DSCHRG_DT
gen visitDuringHospitalization = CLM_THRU_DT >= ADMSN_DT & CLM_THRU_DT <= DSCHRG_DT

gen primaryCare = 0
* family medicine, internal medicine, geriatrics, "general practitioner"
replace primaryCare = 1 if inlist(PRVDR_SPCLTY, "01", "08", "11", "38")

gen cardiology = PRVDR_SPCLTY == "06"

gen neurology = PRVDR_SPCLTY == "13"

gen ed = PRVDR_SPCLTY == "93"

gen otherProvider = 0
replace otherProvider = 1 if primaryCare==0 & neurology==0 & cardiology==0


* drop radiology/IR, lab, ambulance, path, PT/OT, ambulartory surgical center
drop if inlist(PRVDR_SPCLTY, "30", "69", "59", "65", "22", "21", "47", "94")
drop if inlist(PRVDR_SPCLTY, "68", "49", "73", "67", "63", "74", "87", "60")
drop if inlist(PRVDR_SPCLTY, "75", "88", "45", "C0", "C1", "A5" )

bysort BENE_ID : egen visitsPriorToStroke = sum(visitPriorToStroke)
bysort BENE_ID : egen visitsDuringHospitalization = sum(visitDuringHospitalization)
bysort BENE_ID : egen visitsAfterStroke = sum(visitAfterStroke)

* drop outpatient claims
gen hospital = 0
replace hospital = 1  if inlist(LINE_PLACE_OF_SRVC_CD, "21", "22", "23", "31", "32", "51", "61", "62")

drop if hospital==1


gen primaryCarePrior = primaryCare & visitPriorToStroke
gen primaryCareAfter = primaryCare & visitAfterStroke

gen neurologyAfter = neurology & visitAfterStroke
gen cardiologyAfter = cardiology & visitAfterStroke
gen otherProviderAfter = otherProvider & visitAfterStroke

gen within60Days = visitAfterStroke & CLM_THRU_DT < DSCHRG_DT + 60
gen within30Days = visitAfterStroke & CLM_THRU_DT < DSCHRG_DT + 30
gen within15Days = visitAfterStroke & CLM_THRU_DT < DSCHRG_DT + 15
gen within7Days = visitAfterStroke & CLM_THRU_DT < DSCHRG_DT + 7

gen uniqueProviderBeforeTemp = 0
bysort BENE_ID PRF_PHYSN_NPI : replace uniqueProviderBeforeTemp = 1 if _n==1 & visitPriorToStroke==1
by BENE_ID : egen uniqueProvidersBefore = sum(uniqueProviderBeforeTemp)

gen uniqueProviderAfterTemp = 0
bysort BENE_ID PRF_PHYSN_NPI : replace uniqueProviderAfterTemp = 1 if _n==1 & visitAfterStroke==1
by BENE_ID : egen uniqueProvidersAfter = sum(uniqueProviderAfterTemp)

gen uniqueProviderAfter30Temp = 0
bysort BENE_ID PRF_PHYSN_NPI : replace uniqueProviderAfter30Temp = 1 if _n==1 & visitAfterStroke==1 & within30Days==1
by BENE_ID : egen uniqueProvidersAfter30 = sum(uniqueProviderAfter30Temp)

gen uniqueProviderAfter15Temp = 0
bysort BENE_ID PRF_PHYSN_NPI : replace uniqueProviderAfter15Temp = 1 if _n==1 & visitAfterStroke==1 & within30Days==1
by BENE_ID : egen uniqueProvidersAfter15 = sum(uniqueProviderAfter15Temp)

gen uniqueProviderAfter7Temp = 0
bysort BENE_ID PRF_PHYSN_NPI : replace uniqueProviderAfter7Temp = 1 if _n==1 & visitAfterStroke==1 & within30Days==1
by BENE_ID : egen uniqueProvidersAfter7 = sum(uniqueProviderAfter7Temp)

gen primaryCareAfter30Temp = 0
bysort BENE_ID primaryCare : replace primaryCareAfter30Temp = 1 if primaryCareAfter==1 & within30Days==1
by BENE_ID : egen totalPrimaryCareAfter30 = sum(primaryCareAfter30Temp)

gen primaryCareAfter15Temp = 0
bysort BENE_ID primaryCare : replace primaryCareAfter15Temp = 1 if primaryCareAfter==1 & within15Days==1
by BENE_ID : egen totalPrimaryCareAfter15 = sum(primaryCareAfter15Temp)

gen primaryCareAfter7Temp = 0
bysort BENE_ID primaryCare : replace primaryCareAfter7Temp = 1 if primaryCareAfter==1 & within7Days==1
by BENE_ID : egen totalPrimaryCareAfter7 = sum(primaryCareAfter7Temp)

gen neurologyAfter30Temp = 0
bysort BENE_ID neurology : replace neurologyAfter30Temp = 1 if neurologyAfter==1 & within30Days==1
by BENE_ID : egen neurologyAfter30 = sum(neurologyAfter30Temp)

gen neurologyAfter15Temp = 0
bysort BENE_ID neurology : replace neurologyAfter15Temp = 1 if neurologyAfter==1 & within15Days==1
by BENE_ID : egen neurologyAfter15 = sum(neurologyAfter15Temp)

gen neurologyAfter7Temp = 0
bysort BENE_ID neurology : replace neurologyAfter7Temp = 1 if neurologyAfter==1 & within7Days==1
by BENE_ID : egen neurologyAfter7 = sum(neurologyAfter7Temp)

gen cardiologyAfter30Temp = 0
bysort BENE_ID cardiology : replace cardiologyAfter30Temp = 1 if cardiologyAfter==1 & within30Days==1
by BENE_ID : egen cardiologyAfter30 = sum(cardiologyAfter30Temp)

gen cardiologyAfter15Temp = 0
bysort BENE_ID cardiology : replace cardiologyAfter15Temp = 1 if cardiologyAfter==1 & within15Days==1
by BENE_ID : egen cardiologyAfter15 = sum(cardiologyAfter15Temp)

gen cardiologyAfter7Temp = 0
bysort BENE_ID cardiology : replace cardiologyAfter7Temp = 1 if cardiologyAfter==1 & within7Days==1
by BENE_ID : egen cardiologyAfter7 = sum(cardiologyAfter7Temp)

gen otherAfter30Temp = 0
bysort BENE_ID otherProvider : replace otherAfter30Temp = 1 if otherProviderAfter==1 & within30Days==1
by BENE_ID : egen otherAfter30 = sum(otherAfter30Temp)

gen otherAfter15Temp = 0
bysort BENE_ID otherProvider : replace otherAfter15Temp = 1 if otherProviderAfter==1 & within15Days==1
by BENE_ID : egen otherAfter15 = sum(otherAfter15Temp)

gen otherAfter7Temp = 0
bysort BENE_ID otherProvider : replace otherAfter7Temp = 1 if otherProviderAfter==1 & within7Days==1
by BENE_ID : egen otherAfter7 = sum(otherAfter7Temp)


gen totalProviderVisPriorTemp= 0
bysort BENE_ID  PRF_PHYSN_NPI visitPriorToStroke: replace totalProviderVisPriorTemp = _N if visitPriorToStroke==1
by BENE_ID PRF_PHYSN_NPI : egen totalProviderVisPrior = max(totalProviderVisPriorTemp)

gen totalProviderDurPriorTemp= 0
bysort BENE_ID  PRF_PHYSN_NPI visitDuringHospitalization: replace totalProviderDurPriorTemp = _N if visitDuringHospitalization==1
by BENE_ID PRF_PHYSN_NPI : egen totalProviderVisDuring = max(totalProviderDurPriorTemp)

gen totalProviderAfterPriorTemp= 0
bysort BENE_ID  PRF_PHYSN_NPI visitAfterStroke: replace totalProviderAfterPriorTemp = _N if visitAfterStroke==1
by BENE_ID PRF_PHYSN_NPI : egen totalProviderVisAfter = max(totalProviderAfterPriorTemp)


gen priorPostContinuityTemp = 0
by BENE_ID PRF_PHYSN_NPI: replace priorPostContinuityTemp =  totalProviderVisPrior > 0 & totalProviderVisAfter > 0 
by BENE_ID : egen priorPriorContinuity = max(priorPostContinuityTemp)

gen duringPostContinuityTemp = 0
by BENE_ID PRF_PHYSN_NPI: replace duringPostContinuityTemp =  totalProviderVisDuring > 0 & totalProviderVisAfter > 0 
by BENE_ID : egen duringPostContinuity = max(duringPostContinuityTemp)

gen pcpPriorPostContinuityTemp = 0
by BENE_ID PRF_PHYSN_NPI: replace pcpPriorPostContinuityTemp =  totalProviderVisPrior > 0 & totalProviderVisAfter > 0 & primaryCare==1
by BENE_ID : egen pcpPriorPriorContinuity = max(pcpPriorPostContinuityTemp)

gen pcpDuringPostContinuityTemp = 0
by BENE_ID PRF_PHYSN_NPI: replace pcpDuringPostContinuityTemp =  totalProviderVisDuring > 0 & totalProviderVisAfter > 0 & primaryCare==1
by BENE_ID : egen pcpDuringPostContinuity = max(duringPostContinuityTemp)

gen daysFromStroke = CLM_THRU_DT - DSCHRG_DT

by BENE_ID : egen minDaysToPrimaryCareTemp = min(daysFromStroke) if primaryCare==1 & visitAfterStroke==1
by BENE_ID : egen minDaysToPrimaryCare = min(minDaysToPrimaryCareTemp) 

by BENE_ID : egen minDaysToPCPTemp = min(daysFromStroke) if (pcpPriorPostContinuityTemp==1 | pcpDuringPostContinuityTemp)  & visitAfterStroke==1
by BENE_ID : egen minDaysToPCP = min(minDaysToPCPTemp) 

by BENE_ID : egen minDaysToNeurologyTemp = min(daysFromStroke) if neurology ==1  & visitAfterStroke==1
by BENE_ID : egen minDaysToNeurology = min(minDaysToNeurologyTemp) 

by BENE_ID : egen minDaysToCardiologyTemp = min(daysFromStroke) if cardiology ==1  & visitAfterStroke==1
by BENE_ID : egen minDaysToCardiology = min(minDaysToCardiologyTemp) 

by BENE_ID : egen minDaysToOtherTemp = min(daysFromStroke) if otherProvider ==1  & visitAfterStroke==1
by BENE_ID : egen minDaysToOther = min(minDaysToOtherTemp) 


preserve
collapse (first) minDaysToPrimaryCare minDaysToPCP minDaysToNeurology minDaysToCardiology ///
	minDaysToOther priorPriorContinuity duringPostContinuity pcpPriorPriorContinuity pcpDuringPostContinuity ///
	totalProviderVisPrior totalProviderVisDuring totalProviderVisAfter ///
	otherAfter30 otherAfter15 otherAfter7 cardiologyAfter30 cardiologyAfter15 ///
	cardiologyAfter7 neurologyAfter30 neurologyAfter15 neurologyAfter7 ///
	totalPrimaryCareAfter30 totalPrimaryCareAfter15 totalPrimaryCareAfter7 ///
	uniqueProvidersAfter uniqueProvidersAfter30 uniqueProvidersAfter15 ///
	uniqueProvidersAfter7 uniqueProvidersBefore, by(BENE_ID)
	
save carrierProviderSummary.dta, replace

restore





clear 
use using ptbclms12.dta
merge m:1 BENE_ID using strokeSampleBeneIDs, gen(hasMergedBeneID)

drop if hasMergedBeneID ==1 //only keepign clais that matched
save strokeCarrierClaimFilesMerged.dta.


clear 
use using baseHospitalizationsForAnalysis.dta
merge m:1 BENE_ID using carrierProviderSummary, gen(carrierMerge)
drop if carrierMerge ==1	//people we can't match into carrier


rename BENE_MLG_CNTCT_ZIP_CD zip
cd "/Users/burke/Documents/research/readmissions"
destring zip, replace

merge m:1 zip using fipsToZipsUniqueForced.dta, gen(zipMerge)
drop if zipMerge==2

merge m:1 FIPS using rwjTotal.dta, gen(rwjMerge)
drop if rwjMerge==2

rename PRVDR_NUM providerid
destring providerid, replace

merge m:1 providerid using strokeProcessMeasures2013.dta, gen(processMerge)
drop if processMerge==2


forvalues i = 1/25{
	rename DGNS_`i'_CD dx`i'
	rename SRGCL_PRCDR_`i'_CD pr`i'
} 

rename BENE_AGE_CNT age


calcCharlson 1 1
setLST
drop if processMerge==2

bysort providerid : gen strokeVolume = _N

//mean imputation for scores...
foreach var of varlist score*	{
	sum `var', meanonly
	replace `var' = `r(mean)' if missing(`var')
}

gen tpa = 0
foreach var of varlist pr1-pr25	{
	replace tpa = 1 if `var' == "9910"
}
replace tpa =1 if DRG_CD=="61" | DRG_CD=="62" | DRG_CD=="63" | DRG_CD=="559"


destring BENE_SEX_CD, replace 
destring BENE_RACE_CD, replace 

gen averagePercentile = 0
gen countOfReportedMeasures = 0
foreach num of numlist 1 2 3 4 5 6 8 10	{
	replace countOfReportedMeasures = countOfReportedMeasures + 1 if !missing(numerator`num')
}

foreach num of numlist 1 2 3 4 5 6 8 10	{
	xtile ptile`num' = score`num', n(100)
	replace averagePercentile = averagePercentile + ptile`num' if !missing(numerator`num')
}
replace averagePercentile = averagePercentile / countOfReportedMeasures


* analysis starts here...
cd "/Users/burke/Documents/research/readmissions"
* drop discharges from teh last month of the year...
drop if DSCHRG_DT > date("12/1/2012","MDY") 



label variable age  "Age" 
label variable BENE_SEX_CD "Female"
label variable BENE_RACE_CD "Race/Ethnicity" 

*recode BENE_RACE_CD (3=0) (6=0), gen(raceShort)
label variable raceShort "Race/Ethnicity" 
 
label define raceLab 1 "White" 2 "Black" 3 "Other" 4 "Asian" 5 "Hispanic" 6 "North American Native" 0 "Unknown"
label values raceShort raceLab

label variable peg "PEG"
label variable trach "Trach"
label variable hemicraniectomy "Hemicraniectomy"
label variable intubation "Intubation"
label variable hemodialysis "Hemodialysis"
label variable strokeVolume "Stroke Volume" 
foreach num of numlist 1/6 8 10	{
	label variable score`num' "STK_`num'"
}
label variable LOS_DAY_CNT "LOS"
label variable tpa "tPA"
label variable homeHealth "Home Health"
label variable probableTransferIn  "Transfer In"
label variable PhysicallyInactive "Physical Inactivity"
label variable unemployed "Unemployed"
label variable GraduationRate "Graduation Rate"
label variable SomeCollege "Some College"
label variable IncomeRatio "Income Ratio"
label variable ViolentCrimeRate "Violent Crime Rate"
label variable HouseholdIncome "Household Income"
label variable InsufficientSleep "Insufficient Sleep"
label variable Segregationindex "Segregation Index"

*recode readmit30 (.=0), gen(readmit30Simple)
label variable readmit30Simple
label define binaryLabel 0 "No" 1 "Yes"
label values readmit30Simple binaryLabel

label variable readmit30Simple "Readmission 30"



log using table1.smcl, replace
printtable age BENE_SEX_CD raceShort charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis   score* ///
	strokeVolume LOS_DAY_CNT tpa homeHealth probableTransferIn ///
	PhysicallyInactive unemployed GraduationRate SomeCollege IncomeRatio ///
	ViolentCrimeRate HouseholdIncome InsufficientSleep Segregationindex, ///
	by(readmit30Simple) groupPoints(age charl_mi peg score1 strokeVolume PhysicallyInactive) labelNames("Demographics,Comorbidities,Life-Sustaining Treatment, Hospital Quality,Hospitalization Characteristics,Regional Factors") pvalue 

log close
  
gen anyPrimaryCare30 = 0
replace anyPrimaryCare30 = 1 if totalPrimaryCareAfter30 > 0

gen anyPrimaryCare15 = 0
replace anyPrimaryCare15 = 1 if totalPrimaryCareAfter15 > 0

gen anyPrimaryCare7 = 0
replace anyPrimaryCare7 = 1 if totalPrimaryCareAfter7 > 0


gen anyNeurology30 = 0
replace anyNeurology30 = 1 if neurologyAfter30 > 0

gen anyNeurology15 = 0
replace anyNeurology15 = 1 if neurologyAfter15 > 0

gen anyNeurology7 = 0
replace anyNeurology7 = 1 if neurologyAfter7 > 0


gen anyCardiology30 = 0
replace anyCardiology30 = 1 if cardiologyAfter30 > 0

gen anyCardiology15 = 0
replace anyCardiology15 = 1 if cardiologyAfter15 > 0

gen anyCardiology7 = 0
replace anyCardiology7 = 1 if cardiologyAfter7 > 0


gen anyOther30 = 0
replace anyOther30 = 1 if otherAfter30 > 0

gen anyOther15 = 0
replace anyOther15 = 1 if otherAfter15 > 0

gen anyOther7 = 0
replace anyOther7 = 1 if otherAfter7> 0

gen priorPCP30 = 0
replace priorPCP30 = 1 


log using outpatientVisitsByReadmisison.smcl, replace

tab anyPrimaryCare30  readmit30 , co
tab anyPrimaryCare15  readmit30 , co
tab anyPrimaryCare7  readmit30 , co

tab anyNeurology30  readmit30 , co
tab anyNeurology15  readmit30 , co
tab anyNeurology7  readmit30 , co

tab anyCardiology30  readmit30 , co
tab anyCardiology15  readmit30 , co
tab anyCardiology7  readmit30 , co

tab anyOther30  readmit30 , co
tab anyOther15  readmit30 , co
tab anyOther7  readmit30 , co

log close

save datasetForReadmissionAnalysis.dta, replace


log using baseModels.smcl, replace

* empty model for ICC
melogit readmit30 ||providerid:
estat icc


* all patient-level factors
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  || providerid: 
estat icc

* all patient + hospital factors (could add hospital-level practices here as well...but we've previously shown those are not terribly important)
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume score* ///
	|| providerid:
estat icc


* all patient + hospital + regional (FIPS) factors
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	|| providerid:
estat icc

log close

log using basicProviderModels.smcl, replace


* all patient + hospital + regional (FIPS) factors + anyPrimaryCare within 30 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyPrimaryCare30 ///
	|| providerid:
estat icc


* all patient + hospital + regional (FIPS) factors + any neuorology within 30 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyNeurology30 ///
	|| providerid:
estat icc


* all patient + hospital + regional (FIPS) factors + any cardiology within 30 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyCardiology30 ///
	|| providerid:
estat icc



* all patient + hospital + regional (FIPS) factors + any other provider within 30 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyOther30 ///
	|| providerid:
estat icc

/*
* all patient + hospital + regional (FIPS) factors + PCP 
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.priorPCP30 ///
	|| providerid:
estat icc

* all patient + hospital + regional (FIPS) factors + PCP that first saw patient during hospital
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.priorPCP30 ///
	|| duringPCP30:
estat icc
*/

log close


log using primaryCareAndNeurology.smcl, replace


* all patient + hospital + regional (FIPS) factors + any neuorology within 30 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyNeurology30 i.anyPrimaryCare30 ///
	|| providerid:
estat icc

log close



* it also drops the strokes that aren't in carrier — if your'e not in carrier at all
* in spite of receiving care in the hospital for your stroke...
* we probably shouldn't follow you...
	
* id first stroke hospitalizations and care in hospital - build a simple file that has only the first stroke hospitaliation and the readmission data
* get comorbidities and procedures only keep those that are discharged home from teh primary visit.
* calculate hospital stroke volume
* save readmission primary diagnosis reason so that we can classify as preventable

* load carrier for the ids in the sample only
* id generalist visits throughout the year. define PCP as individual with whom you hvae 2 or more visits within the year.
* id neurology visits throughout the year
* 

* merge the files up...will probably need to get provider IDS from teh part b file
* also going to need to get mortality from MBSF...probably also want to impose a continous enrolled criterion...
* load the 2013 hospital process measure data from teh medicare mortality project.
* load census data on SES
* do we need anything from the AHA file? maybe get bedsize, academic affilitation adn critical access status.

* there is clearly a disparities angle here to explore if there is a primary effect on readmission rates...
