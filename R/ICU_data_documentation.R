#' Demographics and comorbidities of 10,157 ICU patients
#'
#' An deidentified data set containing the demographics, comorbidities, DNR code
#' status, and surgical team assignment of 10,157 patients in the Stanford
#' University Hospital Intensive Care Unit (ICU).  This data was extracted from
#' the electronic record system, deidentified, and made publically available by
#' Chavez et al (2018) <doi:10.1371/journal.pone.0190569>.  It was reprocessed
#' for use in the \code{stratamatch} package as a sample data set. For more
#' details on the data extraction and inclusion criteria, see Chavez et al.
#'
#' License information for this data is as follows:
#'
#' Copyright (c) 2016, Stanford University
#'
#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#'
#' The above copyright notice and this permission notice shall be included in
#' all copies or substantial portions of the Software.
#'
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#'
#' @format A data frame with 10157 rows and 29 variables: \describe{
#'   \item{patid}{patient id, numeric} \item{Birth.preTimeDays}{age of patient
#'   at time of admission to the ICU in days, numeric} \item{Female.pre}{whether
#'   the patient was documented to be female prior to ICU visit, binary}
#'   \item{RaceAsian.pre}{whether the patient's race/ethnicity was documented as
#'   Asian prior to ICU visit, binary} \item{RaceUnknown.pre}{whether the
#'   patient's race/ethnicity was unknown prior to ICU visit, binary}
#'   \item{RaceOther.pre}{whether the patient's race/ethnicity was documented as
#'   Other" prior to ICU visit, binary} \item{RaceBlack.pre}{whether the
#'   patient's race/ethnicity was documented as Black/African American prior to
#'   ICU visit, binary} \item{RacePacificIslander.pre}{whether the patient's
#'   race/ethnicity was documented as PacificIslander prior to ICU visit,
#'   binary} \item{RaceNativeAmerican.pre}{whether the patient's race/ethnicity
#'   was documented as Native American prior to ICU visit, binary}
#'   \item{self_pay}{whether the patient was "self pay" (i.e. uninsured),
#'   binary} \item{all_latinos}{whether the patient was documented to be latino
#'   prior to ICU visit, binary} \item{DNR}{whether the patient had code status
#'   set to any DNR "Do not resuscitate" order at any point during their ICU
#'   stay, binary} \item{surgicalTeam}{whether the patient was assigned to a
#'   surgical team at any point during their ICU stay, binary} }
#' @source
#' \url{https://simtk.org/frs/download_confirm.php/latestzip/1969/ICUDNR-latest.zip?group_id=892}
#'
"ICU_data"
