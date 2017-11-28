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

forvalues i = 1/30	{
	by BENE_ID : replace firstReadmitDx = firstReadmitDx[_n+1]	if missing(firstReadmitDx)	//propogate them backwards...
}

replace indexStroke = 1 if probableTransferIn ==1	//we're going to call this the index event.
replace PRVDR_NUM = PRVDR_NUM if probableTransferIn == 1	//assign to the transfer out hospital, but will still mark as a transfer.

drop if probableTransferOut == 1 //we're going to drop the transfer outs...we'll adjust for trasnfer status and we'll assign hospitals to the initiating hopsital 
//for their randome effect

gen dischargeHome = DSCHRG_DSTNTN_CD == "01" | DSCHRG_DSTNTN_CD == "06"
 
keep if dischargeHome==1
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

gen anyPrimaryCare30 = 0
replace anyPrimaryCare30 = 1 if totalPrimaryCareAfter30 > 0
replace anyPrimaryCare30 = 0 if minDaysToPrimaryCare > dateToFirstReadmission	//we're not going to call these actual visits if they occur after the first readmission

gen priorPCP30 = 0
replace priorPCP30 = 1 if pcpPriorPriorContinuity ==1 & minDaysToPCP < 30
replace priorPCP30 = 0 if minDaysToPCP > dateToFirstReadmission	//we're not going to call these actual visits if they occur after the first readmission

gen anyNeurology30 = 0
replace anyNeurology30 = 1 if neurologyAfter30 > 0
replace anyNeurology30 = 0 if minDaysToNeurology > dateToFirstReadmission

gen anyCardiology30 = 0
replace anyCardiology30 = 1 if cardiologyAfter30 > 0
replace anyCardiology30 = 0 if minDaysToCardiology > dateToFirstReadmission

gen anyOther30 = 0
replace anyOther30 = 1 if otherAfter30 > 0
replace anyOther30 = 0 if minDaysToOther > dateToFirstReadmission


gen anyPrimaryCare7 = 0
replace anyPrimaryCare7 = 1 if totalPrimaryCareAfter7 > 0
replace anyPrimaryCare7 = 0 if minDaysToPrimaryCare > dateToFirstReadmission
	
gen anyPrimaryCare15 = 0
replace anyPrimaryCare15 = 1 if totalPrimaryCareAfter15 > 0
replace anyPrimaryCare15 = 0 if minDaysToPrimaryCare > dateToFirstReadmission


gen anyNeurology7 = 0
replace anyNeurology7 = 1 if neurologyAfter7 > 0
replace anyNeurology7 = 0 if  minDaysToNeurology > dateToFirstReadmission

	
gen anyNeurology15 = 0
replace anyNeurology15 = 1 if neurologyAfter15 > 0
replace anyNeurology15 = 0 if  minDaysToNeurology > dateToFirstReadmission


sort BENE_ID ADMSN_DT
by BENE_ID : gen admitNumber = _n
drop if admitNumber > 1


gen preventableReadmission = 0
* Prevention Quality Indicator 01 (PQI 01) Diabetes Short-Term Complications Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx, "25010", "25011", "25012","25013", "25020", "25021","25022", "25023")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "25030", "25031", "25032", "25033")

* Prevention Quality Indicator 03 (PQI 03) Diabetes Long-Term Complications Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx, "25040", "25041", "25042", "25043", "25050", "25051", "25052")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "25053", "25060", "25061", "25062", "25063", "25070", "25071")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "25072", "25073", "25080", "25081", "25082", "25083", "25090")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "25091", "25092", "25093")

* Prevention Quality Indicator 05 (PQI 05) Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx, "4910", "4911","49120","49121", "49122", "4918", "4919", "49300")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "49301", "49302", "49310", "49311", "49312", "49320", "4920")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "4928", "494", "4940", "4941", "496", "49321", "49322", "49381")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "49382", "49390", "49391", "49392")

* Prevention Quality Indicator 07 (PQI 07) Hypertension Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx, "4010", "4019", "40200","40210","40290", "40300", "40310")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "40390", "40400","40410","40490")

* Prevention Quality Indicator 08 (PQI 08) Heart Failure Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx,"39891", "40201", "40211", "40291","40401", "40403","40411","40413", "40491")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "40493", "4280", "4281", "42820", "42821", "42822", "42823", "42830", "42831")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "42832", "42833", "42840", "42841", "42842", "42843", "4289")

* Prevention Quality Indicator 10 (PQI 10) Dehydration Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx,"2765","27651", "27650", "27652", "2760", "00861", "00862", "00863", "00864")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "00865", "00866", "00867", "5845", "5846","5847", "5848", "00869", "0088")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "0090", "0091", "0092", "0093", "5589", "5849", "586", "9975", "40301")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "40413", "40311", "40492",  "40391","40493", "40402","5855", "40403")
replace preventableReadmission = 1 if inlist(firstReadmitDx, "5856", "40412") 

* Bacterial pneumonia diagnosis codes: (ACSBACD)
replace preventableReadmission = 1 if inlist(firstReadmitDx,"481","48242", "4822","48249", "48230", "4829", "48231","4830")
replace preventableReadmission = 1 if inlist(firstReadmitDx,"48232","4831", "48239","4838", "48240", "485", "48241", "486")

* Prevention Quality Indicator 12 (PQI 12) Urinary Tract Infection Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx,"59010", "59081", "59011", "5909", "5902", "5950", "5903", "5959")
replace preventableReadmission = 1 if inlist(firstReadmitDx,"59080", "5990")

* Prevention Quality Indicator 14 (PQI 14) Uncontrolled Diabetes Admission Rate
replace preventableReadmission = 1 if inlist(firstReadmitDx,"25002", "25003")

gen preventableReadmit30 = preventableReadmission == 1 & readmit30==1

save datasetForReadmissionAnalysis.dta, replace


* analysis starts here...
cd "/Users/burke/Documents/research/readmissions"
gen decemberDischarge = DSCHRG_DT > date("12/1/2012","MDY") 
gen daysFromDischargeToDeath = BENE_DEATH_DT - DSCHRG_DT

log using exploreDecemberEffect.smcl, replace
tab decemberDischarge, miss

tab readmit30 decemberDischarge , miss co ro

tab anyPrimaryCare30 decemberDischarge , miss co ro

bysort decemberDischarge : tab anyPrimaryCare30 readmit30, miss co ro
* unsurprisingly, it is much less common to have a readmission or to have a PCP visit in demember...
* the big difference is that those who don't have a PCP visit have MUCH lower radmission rates in december...
* so, its unsurprising that the effect changes a lot when you exclude december
log close

* drop discharges from teh last month of the year...
drop if decemberDischarge==1


log using twoByTwoTables.smcl, replace
tab anyPrimaryCare30 readmit30 , miss co
tab anyNeurology3 readmit30, miss co
log close


log using exploreKeyOutcomeExposureVariables.smcl, replace


* distribution for all readmissions
sum dateToFirstReadmission , det

sum dateToFirstReadmission if readmit30==1, det
sum minDaysToPrimaryCare if anyPrimaryCare30==1, det
sum minDaysToNeurology if anyNeurology30==1, det


hist dateToFirstReadmission if readmit30==1, graphregion(color(white))
graph export dateToreadmissionHist.eps, replace
hist minDaysToPrimaryCare if anyPrimaryCare30==1, graphregion(color(white))
graph export daysToPrimaryCare.eps, replace
hist minDaysToNeurology if anyNeurology30==1, graphregion(color(white))
graph export daysToNeurology.eps, replace

log close

log using describeTimeDistributions.smcl, replace
drop if dateToFirstReadmission < 0
preserve
replace minDaysToPrimaryCare = minDaysToPrimaryCare - 1

replace dateToFirstReadmission = dateToFirstReadmission  -1
sum dateToFirstReadmission, det
hist dateToFirstReadmission, graphregion(color(white))  xtitle("Days to First Readmission") ytitle("Percentage") discrete
graph export readmissionTimeHistogram.eps, replace

sum minDaysToPrimaryCare, det
hist minDaysToPrimaryCare, graphregion(color(white))   xtitle("Days to Primary Care")  ytitle("Percentage") discrete
graph export primaryCareHistogram.eps, replace

replace minDaysToNeurology = minDaysToNeurology - 1

sum minDaysToNeurology, det
hist minDaysToNeurology, graphregion(color(white))   xtitle("Days to Neurology")   ytitle("Percentage") discrete
graph export neurologyHisotogram.eps, replace
restore
log close




label variable age  "Age" 
label variable BENE_SEX_CD "Female"
label variable BENE_RACE_CD "Race/Ethnicity" 

recode BENE_RACE_CD (3=0) (6=0), gen(raceShort)
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

recode readmit30 (.=0), gen(readmit30Simple)
label variable readmit30Simple
label define binaryLabel 0 "No" 1 "Yes"
label values readmit30Simple binaryLabel

label variable readmit30Simple "Readmission 30"

gen missingFactors = 0
foreach var of varlist score*	{
	replace missingFactors = 1 if missing(`var')
}
foreach var of varlist  PhysicallyInactive unemployed GraduationRate SomeCollege IncomeRatio ///
	ViolentCrimeRate HouseholdIncome InsufficientSleep Segregationindex	{

	replace missingFactors = 2 if missing(`var')
}

log using whatAmIMissing.smcl, replace
tab missingFactors, miss
log close



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

log using predictPCPVisit.smcl, replace
melogit anyPrimaryCare30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
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

log using predictNeurologyVisit.smcl, replace
melogit anyNeurology30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
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

log using baseLogisticModels.smcl, replace

* empty model for ICC
melogit readmit30 ||providerid:
estat icc


* all patient-level factors
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	|| providerid:
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


* almost all missing data is on the county factors
misstable patterns readmit30 age BENE_SEX_CD BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  strokeVolume score* ///
	LOS_DAY_CNT tpa homeHealth probableTransferIn ///
	PhysicallyInactive unemployed GraduationRate SomeCollege IncomeRatio ///
	ViolentCrimeRate HouseholdIncome InsufficientSleep Segregationindex ///
	anyPrimaryCare30 
	
* so, rerun the model without the county factors to see what happens...
* not a lot...the logit coefficient barely changes, so it isn't likely that the missing data is influencing our analysis
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	i.anyPrimaryCare30 ///
	|| providerid:
estat icc
predict providerEffect, reffect

log close


/*
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



log close

log using timingModels.smcl, replace

* all patient + hospital + regional (FIPS) factors + anyPrimaryCare within 7 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyPrimaryCare7 ///
	|| providerid:

* all patient + hospital + regional (FIPS) factors + anyPrimaryCare within 15 days
	
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyPrimaryCare15 ///
	|| providerid:

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


	
* amongst those with 30 day visits, do earlier visits matter more?
preserve
keep if anyPrimaryCare30==1
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	c.minDaysToPrimaryCare ///
	|| providerid:
restore	
	
*  account for primary care visits as a continuous variable
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	c.totalPrimaryCareAfter30 ///
	|| providerid:
margins, at(totalPrimaryCareAfter30=(0 1 2 3 4 5 10))

* all patient + hospital + regional (FIPS) factors + any neurology within 7 days
	
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyNeurology7 ///
	|| providerid:

melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyNeurology15 ///
	|| providerid:
	
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



log close


log using neurologyAndPCPModels.smcl, replace

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
	

melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyNeurology15 i.anyPrimaryCare15 ///
	|| providerid:
	
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	i.anyNeurology7 i.anyPrimaryCare7 ///
	|| providerid:
log close

log using primaryCareSESInteraction.smcl, replace

* all patient + hospital + regional (FIPS) factors + anyPrimaryCare within 30 days
melogit readmit30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	i.anyPrimaryCare30##c.HouseholdIncome  i.anyPrimaryCare30##c.InsufficientSleep i.anyPrimaryCare30##c.Segregationindex  ///
	|| providerid:
estat icc

log close


log using survivalAnalysis.smcl, replace
preserve
drop if missing(ADMSN_DT)
expand 2, gen(finalDate)
gen time = 1 if finalDate == 0
replace time = dateToFirstReadmission if !missing(dateToFirstReadmission) & finalDate == 1
replace time = BENE_DEATH_DT - ADMSN_DT if !missing(BENE_DEATH_DT) & finalDate == 1
replace time =  date("Dec 31 2012", "MDY") - ADMSN_DT if finalDate==1 & missing(time)

gen readmissionFail = 0
replace readmissionFail = 1 if !missing(dateToFirstReadmission) & finalDate ==1

drop if time < 0		//one weird obesrvation
gen died = 0
replace died = 1 if !missing(BENE_DEATH_DT)


stset time, failure(readmissionFail) id(BENE_ID)
* effect on primary care visit within 30 days on readmisison at any time point
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex i.anyPrimaryCare30
	

* effect on neurology visit within 30 days on readmisison at any time point
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex i.anyNeurology30

* independent effects of neurology and any primary care visit
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex i.anyNeurology30 i.anyPrimaryCare30
*/

log using readmitDiagnoses.smcl, replace

rename dx1 temp
rename firstReadmitDx dx1
merge m:1 dx1 using "/Users/burke/Documents/research/dizziness outcomes/singleLevelCCS.dta", gen(ccsMerge)
rename dx1 firstReadmitDx
rename temp dx1
drop if ccsMerge==2

* all diagnosis categories for 30 day readmits
tab dxccsdescription if readmit30==1, sort miss

* all diagnosis categories for 30 day readmits seen by primary care
tab dxccsdescription if readmit30==1 & anyPrimaryCare30==1, sort miss

* all diagnosis categories for 30 day readmits seen by neurology
tab dxccsdescription if readmit30==1 & anyNeurology30==1, sort miss

log close


gen diedWithin30 = daysFromDischargeToDeath <= 30
gen  diedAfterPrimaryCareWithin30 = daysFromDischargeToDeath > minDaysToPrimaryCare & daysFromDischargeToDeath<= 30 & anyPrimaryCare30==1

log using exploreDaysToDeath.smcl, replace

* all deaths within 30 days, by readmission status
tab  diedWithin30 readmit30, miss co

* died after primary care...
tab  diedAfterPrimaryCareWithin30 readmit30, miss co


log close


log using exploreSurvivalBias.smcl, replace
* we're at risk for survival bias if the readmissions happen early...or if the PCP visits happen late. 

tab readmit30, miss 	//readmissions are relatively uncommon....which is good for minimizing bias...
tab anyPrimaryCare30, miss		//primary care visits are relatively common...also good for minimizing bias...

* median date is 14 - looks like they're pretty evenly distributed over the first 30 days.
sum dateToFirstReadmission if readmit30 ==1, det
hist dateToFirstReadmission if readmit30 ==1

* good news for bias here — most of the primary care visits that happen are within 7 days
sum minDaysToPrimaryCare if anyPrimaryCare30==1, det
hist minDaysToPrimaryCare if anyPrimaryCare30==1

*so, i think this is an argument that there shouldn't be a ton of bias from this...but, there could easily be some.

log close
/*
gen readmissionFail90 = 0
replace readmissionFail90 = 1 if readmissionFail == 1 & dateToFirstReadmission < 90  & finalDate ==1
replace time = 90 if time > 90
	
stset time, failure(readmissionFail90) id(BENE_ID)
* effect on primary care visit within 30 days on readmisison within 90 days
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex i.anyPrimaryCare30
	

* effect on neurology visit within 30 days on readmisison within 90 days
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex i.anyNeurology30

* independent effects of neurology and any primary care visit on readmission within 90 days
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex i.anyNeurology30 i.anyPrimaryCare30

	
	
restore
log close
*/
* have to stset separately for each exposure...
program setupTimeVaryingCovariates
	args followUpDays
	
	drop if missing(DSCHRG_DT )
	drop if dateToFirstReadmission <=0
	drop anyNeurology*
	
	replace minDaysToNeurology = . if minDaysToNeurology > `followUpDays'			//ignore primary care visits after follow-up
	replace minDaysToPrimaryCare = . if minDaysToPrimaryCare > `followUpDays'			//ignore primary care visits follow-up
	replace dateToFirstReadmission = . if dateToFirstReadmission > 30		//ignore readmissions after 30 days
	
	replace minDaysToNeurology = minDaysToNeurology + 1
	replace minDaysToPrimaryCare = minDaysToPrimaryCare + 1
	replace dateToFirstReadmission = dateToFirstReadmission + 1
	replace daysFromDischargeToDeath = daysFromDischargeToDeath + 1
	
	
	//an individual can have up to 5 "events" — a PCP visit, a neurology visit, a readmission, death + the start event + a 30 day cap on censoring
	gen time1 = .	//the date of entry...all of the covariates are set to zero
	gen time2 = .	
	gen time3 = .
	gen time4 = .
	gen time5 = .	
	gen time6 = .
	
	replace time1 = 1
	replace time2 = minDaysToNeurology 
	replace time3 = minDaysToPrimaryCare 
	replace time4 = dateToFirstReadmission 
	drop if daysFromDischargeToDeath <=0
	
	replace daysFromDischargeToDeath = . if daysFromDischargeToDeath >30
	replace time5 = daysFromDischargeToDeath
	replace time6 = 32
	
	
	reshape long time, i(BENE_ID) j(eventType)
	drop if missing(time)
	drop if minDaysToNeurology==minDaysToPrimaryCare & eventType==2	//if you have both on the same day...drop one of hte observations...
	drop if (minDaysToNeurology==dateToFirstReadmission | minDaysToNeurology==daysFromDischargeToDeath) & eventType==2	//if your'e admitted on the same day as you see neurology, drop the neurology time
	drop if (minDaysToPrimaryCare==dateToFirstReadmission | minDaysToPrimaryCare==daysFromDischargeToDeath) & eventType==3	//if your'e admitted on the same day as you see primary care drop the primary care time
	drop if dateToFirstReadmission == daysFromDischargeToDeath & eventType==4
	drop if minDaysToNeurology==daysFromDischargeToDeath & eventType==2
	
	
	sort BENE_ID time
	by BENE_ID : gen timeIndex = _n
	
	gen anyNeurology = 0
	gen anyPrimaryCare = 0
	gen readmissionFail = 0
	gen died = 0
	
	replace anyNeurology = 1 if time >= minDaysToNeurology
	replace anyPrimaryCare = 1 if time >= minDaysToPrimaryCare
	replace readmissionFail = 1 if time == dateToFirstReadmission
	drop if  time > dateToFirstReadmission		//you only need to fail once, just keep the first one...this will get rid of neurology, primary care visits and 32 day placeholder after the readmission
	replace died = 1 if time == daysFromDischargeToDeath
	drop if time > daysFromDischargeToDeath	& daysFromDischargeToDeath <= dateToFirstReadmission	//you also only die once. get rid of the events tha occur after that time.
	
	
	stset time, failure(readmissionFail) id(BENE_ID)
end program


log using serialModelBuidling.smcl, replace
preserve
setupTimeVaryingCovariates 30
* primary care + age only
stcox age , tvc(anyPrimaryCare) vce(cluster providerid)
* primary care + all patient factors
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	, tvc(anyPrimaryCare) vce(cluster providerid)
* primary care + all hospital factors (no missing data)
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	, tvc(anyPrimaryCare) vce(cluster providerid)
* primay care + all hospital + short SES
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.GraduationRate ///
	c.HouseholdIncome   c.Segregationindex ///
	, tvc(anyPrimaryCare) vce(cluster providerid)
	
	
	
* neurology + age only
stcox age , tvc(anyNeurology) vce(cluster providerid)
* neurology + all patient factors
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	, tvc(anyNeurology) vce(cluster providerid)
* neurology + all hospital factors (no missing data)
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	, tvc(anyNeurology) vce(cluster providerid)
* neurology + all hospital + short SES
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.GraduationRate ///
	c.HouseholdIncome   c.Segregationindex ///
	, tvc(anyNeurology) vce(cluster providerid)

restore
log close


log using repeatPrimaryModelsWithShorterSES.smcl, replace
preserve
setupTimeVaryingCovariates 30

*  add primary care
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.GraduationRate ///
	c.HouseholdIncome   c.Segregationindex ///
	, tvc(anyPrimaryCare) vce(cluster providerid)
	
*  add neurology
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.GraduationRate ///
	c.HouseholdIncome   c.Segregationindex ///
	, tvc(anyNeurology) vce(cluster providerid)

restore
log close

setupTimeVaryingCovariates 30




* setup TVC for prevetnable readmissions...copy/paste hack

preserve
drop if missing(DSCHRG_DT )
drop if dateToFirstReadmission <=0
drop anyNeurology*

replace minDaysToNeurology = . if minDaysToNeurology > 30			//ignore primary care visits after follow-up
replace minDaysToPrimaryCare = . if minDaysToPrimaryCare > 30			//ignore primary care visits follow-up
replace dateToFirstReadmission = . if dateToFirstReadmission > 30		//ignore readmissions after 30 days

replace minDaysToNeurology = minDaysToNeurology + 1
replace minDaysToPrimaryCare = minDaysToPrimaryCare + 1
replace dateToFirstReadmission = dateToFirstReadmission + 1
replace daysFromDischargeToDeath = daysFromDischargeToDeath + 1
	
	
//an individual can have up to 5 "events" — a PCP visit, a neurology visit, a readmission, death + the start event + a 30 day cap on censoring
gen time1 = .	//the date of entry...all of the covariates are set to zero
gen time2 = .	
gen time3 = .
gen time4 = .
gen time5 = .	
gen time6 = .

gen dateToFirstPrevReadmit = .
replace dateToFirstPrevReadmit = dateToFirstReadmission if preventableReadmit30  != 0
	
replace time1 = 1
replace time2 = minDaysToNeurology 
replace time3 = minDaysToPrimaryCare 
replace time4 = dateToFirstPrevReadmit 

drop if daysFromDischargeToDeath <=0
	
replace daysFromDischargeToDeath = . if daysFromDischargeToDeath >30
replace time5 = daysFromDischargeToDeath
replace time6 = 32

reshape long time, i(BENE_ID) j(eventType)
drop if missing(time)
drop if minDaysToNeurology==minDaysToPrimaryCare & eventType==2	//if you have both on the same day...drop one of hte observations...
drop if (minDaysToNeurology==dateToFirstPrevReadmit | minDaysToNeurology==daysFromDischargeToDeath) & eventType==2	//if your'e admitted on the same day as you see neurology, drop the neurology time
drop if (minDaysToPrimaryCare==dateToFirstPrevReadmit | minDaysToPrimaryCare==daysFromDischargeToDeath) & eventType==3	//if your'e admitted on the same day as you see primary care drop the primary care time
drop if dateToFirstPrevReadmit == daysFromDischargeToDeath & eventType==4
drop if minDaysToNeurology==daysFromDischargeToDeath & eventType==2


sort BENE_ID time
by BENE_ID : gen timeIndex = _n

gen anyNeurology = 0
gen anyPrimaryCare = 0
gen readmissionFail = 0
gen died = 0

replace anyNeurology = 1 if time >= minDaysToNeurology
replace anyPrimaryCare = 1 if time >= minDaysToPrimaryCare
replace readmissionFail = 1 if time == dateToFirstPrevReadmit
drop if  time > dateToFirstPrevReadmit		//you only need to fail once, just keep the first one...this will get rid of neurology, primary care visits and 32 day placeholder after the readmission
replace died = 1 if time == daysFromDischargeToDeath
drop if time > daysFromDischargeToDeath	& daysFromDischargeToDeath <= dateToFirstPrevReadmit	//you also only die once. get rid of the events tha occur after that time.


stset time, failure(readmissionFail) id(BENE_ID)

log using preventableReadmissions.smcl, replace
/*
tab preventableReadmit30 , miss
tab preventableReadmit30 anyPrimaryCare30, co miss
tab preventableReadmit30 anyNeurology30 , co miss
*/
* age cox model for PCP
stcox age , tvc(anyPrimaryCare) vce(cluster providerid) 

* patient characteristics for PCP
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis , tvc(anyPrimaryCare) vce(cluster providerid) 
	
* full model for PCP
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex ///
	, tvc(anyPrimaryCare) vce(cluster providerid)

	
* age cox model for neurology
stcox age , tvc(anyNeurology) vce(cluster providerid) 

* patient characteristics for PCP
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis , tvc(anyNeurology) vce(cluster providerid) 
	
* full model for PCP
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex ///
	, tvc(anyNeurology) vce(cluster providerid)

log close

restore

log using frequencyOfPreventableReadmissions.smcl, replace
tab readmit30 preventableReadmit30, miss co
log close 

preserve

setupTimeVaryingCovariates 30
log using timeVaryingAnyReadmissionAnyPCP.smcl, replace
* first a model just with the patient characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis , vce(cluster providerid) 

* then add hospitalizaiton charactericics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn , vce(cluster providerid)
	
* then add hospital characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* , vce(cluster providerid)

* then add community characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex , vce(cluster providerid)


* then add primary care
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex ///
	, tvc(anyPrimaryCare) vce(cluster providerid)

* then add the providereffect modeled from the logit model
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex ///
	c.providerEffect , tvc(anyPrimaryCare) vce(cluster providerid)
	
log close
restore

log using interactionsModel.smcl, replace
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex ///
	i.anyPrimaryCare30##c.HouseholdIncome  i.anyPrimaryCare30##c.InsufficientSleep i.anyPrimaryCare30##c.Segregationindex ///
	, tvc(anyPrimaryCare) vce(cluster providerid)

log close

log using timeVaryingMediation.smcl, replace
* first an empty model
stcox age,tvc(anyPrimaryCare)

* then a model just with the patient characteristics...
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis , tvc(anyPrimaryCare)

* then add hospitalizaiton charactericics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn, tvc(anyPrimaryCare)
	
* then add hospital characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* , tvc(anyPrimaryCare)

* then add community characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex , tvc(anyPrimaryCare) 


* then add the providereffect modeled from the logit model
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex ///
	c.providerEffect, tvc(anyPrimaryCare) 
log close
	
restore


* look at the impact of primary care visits with 7 days on 30 day radmissions
preserve 
setupTimeVaryingCovariates 7


log using timeVarying30DayReadmission7DayPCP.smcl, replace
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex,  tvc(anyPrimaryCare)
log close
restore



* look at the impact of primary care visits with 15 days on 30 day radmissions
preserve 
setupTimeVaryingCovariates 30
log using timeVarying30DayReadmission15DayPCP.smcl, replace
* first a model just with the patient characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex , tvc(anyPrimaryCare)
log close
restore


* look at the impact of primary care visits with 30 days on 30 day radmissions
preserve 
setupTimeVaryingCovariates 30
log using timeVarying30DayReadmission30DayPCPWithProviderEffect.smcl, replace
* add the provider effect
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex providerEffect, tvc(anyPrimaryCare)
log close

log using timeVarying30DayReadmission30DayPCP.smcl, replace
* first a model just with the patient characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex, tvc(anyPrimaryCare)
	

log close

restore


* now do for neurlogy....
* look at the impact of primary care visits with 30 days on 30 day radmissions
preserve 
setupTimeVaryingCovariates 30

log using neurology30Day.smcl, replace
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex , tvc(anyNeurology) vce(cluster providerid)
log close
restore




//now do th combined version for neurology + primary care.
preserve 
setupTimeVaryingCovariates 30


log using orimaryCareAndNeurologyTimeVarying.smcl, replace
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex , tvc(anyNeurology anyPrimaryCare)
log close
restore

preserve
setupTimeVaryingCovariates 30
sts graph, by(anyPrimaryCare)
* add the provider effect
log using proopritonalHazardsTest.smcl, replace
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex providerEffect anyPrimaryCare, nolog noshow schoenfeld(sch*) scaledsch(sca*)

estat phtest, log detail 
estat phtest, log plot(anyPrimaryCare) yline(0)
* violoation of proportional hazards for anyPrimaryCare (not a problem for us...its a TVC, as well as charl_mets and probable_transfer)
log close
restore

* look at the impact of primary care visits with 30 days on 30 day radmissions
preserve 
setupTimeVaryingCovariates 30

log using repeatBaseModelWithPropHazardsTVCProblems.smcl, replace
* first a model just with the patient characteristics

stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth  ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex, tvc(anyPrimaryCare i.charl_met i.probableTransferIn)
* no change in the diretion of any of the major effects...so a bit of a relief hee (as met and transfer in don't, themselves changes over time and can't be used as a TVC
log close
restore

preserve
setupTimeVaryingCovariates 30

* now a model that omits the covariates that violate the proporitonal hazardsassumption
log using repeatBaseModelDropingPropHazardsTVCProblems.smcl, replace
* first a model just with the patient characteristics
stcox age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver charl_met ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn  ///
	c.strokeVolume c.score* ///
	c.PhysicallyInactive   c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate   ///
	c.HouseholdIncome  c.InsufficientSleep c.Segregationindex, tvc(anyPrimaryCare )

log close
restore



log using propensityScore.smcl, replace

melogit anyPrimaryCare30 age i.BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex ///
	|| providerid:
	
predict propensity, mu

psmatch2 anyPrimaryCare30 , outcome(readmit30) neighbor(2) pscore(propensity) 
pstest age BENE_SEX_CD i.BENE_RACE_CD charl_mi charl_pvd charl_chf ///
	charl_dementia charl_copd charl_rheum charl_pud charl_mildLiver ///
	charl_uncomplicatedDiabetes charl_complicatedDiabetes charl_hemiplegia ///
	charl_renal charl_cancer charl_modSevereLiver charl_met charl_aids peg trach ///
	hemicraniectomy intubation hemodialysis  c.strokeVolume c.score* ///
	c.LOS_DAY_CNT i.tpa i.homeHealth i.probableTransferIn ///
	c.PhysicallyInactive c.unemployed c.GraduationRate c.SomeCollege c.IncomeRatio ///
	c.ViolentCrimeRate c.HouseholdIncome c.InsufficientSleep c.Segregationindex
psgraph
	

log close




log using unadjustedModel.smcl, replace
melogit readmit30 i.anyNeurology30 	|| providerid: , or

melogit readmit30 i.anyPrimaryCare30 	|| providerid: , or

melogit readmit30 i.anyNeurology30 i.anyPrimaryCare30 	|| providerid: , or

log close


	
	
* to dos...1...make sure that we "censor" our primary analysis at the time of the first visit to a specialist...if you see a specialist AFTEr your readmision date, that shouldn't count.
* 2. proopensity score

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
