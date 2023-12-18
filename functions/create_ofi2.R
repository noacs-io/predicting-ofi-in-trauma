#' Create Opportunities For Improvement Variable
#'
#' Create opportunities for improvement variable, which is either yes
#' or no
#'
#' Yes: The case was flagged and reviewed in a meeting and the
#' consensus was that there were opportunities for improvement
#' (variable Problemomrade_.FMP), or that the patient died and the
#' death was determined as preventable or potentially preventable
#' (variable Fr1.14 2 or 3).
#' 
#' No: The consensus was that there were no opportunities for
#' improvement, or the nurses in the initial review did not send the
#' case for review because everything was okay (variable VK_avslutad).
#'
#' NA: The case was never selected for review or the patient died but
#' whether the death was preventable is registered as unknown (Fr1.14
#' 999)
#' @param data A data.frame. The data needed to create the
#'     opportunities for improvements variable. Has to include columns
#'     with tne names specified in the arguments quality.review.done,
#'     problem.area, mortality.review.done, and preventable.death.  No
#'     default.
#' @param quality.review.done Character. The name of the quality
#'     review done variable, which indicates if the quality review has
#'     been completed. Defaults to "VK_avslutad".
#' @param problem.area Character. The name of the problem area
#'     variable, which indicates what, if any, problem area that were
#'     identified. Defaults to "Problemomrade_.FMP".
#' @param mortality.review.done Character. The name of the mortatlity
#'     review done variable, which indicates if the mortality review
#'     has been completed. Defaults to "tra_DodsfallsanalysGenomford".
#' @param preventable.death Character. The name of the preventable
#'     death variable, which indicates if a death was prevantable,
#'     potentially proventable, and not preventable. Defaults to
#'     "Fr1.14".
#' @export
create_ofi2 <- function(data,
                       quality.review.done = "VK_avslutad",
                       problem.area = "Problemomrade_.FMP",
                       mortality.review.done = "tra_DodsfallsanalysGenomford",
                       preventable.death = "Fr1.14") {
  ## Check arguments
  assertthat::assert_that(is.data.frame(data))
  variable.names <- c(quality.review.done = quality.review.done,
                      problem.area = problem.area,
                      mortality.review.done = mortality.review.done,
                      preventable.death = preventable.death)
  for (variable.name in variable.names) assertthat::assert_that(is.character(variable.name) & length(variable.name) == 1)
  assertthat::assert_that(all(variable.names %in% names(data)))
  
  ## Check for data shift, i.e. that input data has changed since
  ## this code was written
  ofi.data <- data[, variable.names]
  names(ofi.data) <- names(variable.names)
  ofi.data <- check_data_shift(ofi.data)
  
  ## Create ofi variable
  
  ## The starting point is that either the quality review or
  ## mortality review process is done
  ofi <- !with(ofi.data, quality.review.done == "ja" | mortality.review.done == 1)
  ## If neither of them are done, then ofi should be NA
  ofi[ofi] <- NA
  ## If the problem area is not labelled as okay then there is an
  ## opportunity for improvement
  
  ############
  # New code # Why & (and) for problem.area and | (or) for preventable death?
  ############
  ofi[with(ofi.data, problem.area != "ok" & problem.area != "föredömligt handlagd" 
           & problem.area != "inget problemområde" & problem.area != "nej")] <- TRUE
  ## If the death is preventable or potentially prevantable then
  ## there is an opportunity for improvement
  ofi[with(ofi.data, preventable.death == "2" | preventable.death == "3")] <- TRUE
  ## If the preventability is unknown then it is unknown if there is
  ## an opportunity for improvement, unless there is an opportunity
  ## for improvement according to the quality review
  ofi[ofi.data$preventable.death == 999 & ofi == FALSE] <- NA
  
  ## Make ofi character
  ofi <- ifelse(ofi, "Yes", "No")
  
  ## Return ofi vector
  return (ofi)
}

check_data_shift <- function(ofi.data) {
  ## Check quality review done variable
  ofi.data$quality.review.done <- tolower(as.character(ofi.data$quality.review.done))
  levels.quality.review.done <- unique(ofi.data$quality.review.done)
  original.levels.quality.review.done <- c("ja", NA, "nej")
  if (!all(levels.quality.review.done %in% original.levels.quality.review.done))
    stop ("Levels in the quality review done variable have changed.")
  ## Check problem area variable
  ofi.data$problem.area <- tolower(as.character(ofi.data$ problem.area))
  levels.problem.area <- unique(ofi.data$problem.area)
  ############
  # New code #
  ############
  
  original.levels.problem.area  <- c(NA,"ok","triage på akutmottagningen","vårdnivå","handläggning",
                                     "logistik/teknik","resurs","missad skada","lång tid till op",
                                     "kompetens brist","inget problemområde","föredömligt handlagd",
                                     "kommunikation","handläggning/logistik","traumakriterier/styrning",
                                     "lång tid till dt","triage på akm","tertiär survey","ok","nej",
                                     "dokumentation","bristande rutin","ok","neurokirurg","dokumetation",
                                     "handläggning\r\ndokumentation","annat", "vårdnivå+\r\nmissade skador",
                                     "kommunikation+missad skada", "handläggning prehosp")
  
  ## End new code    
  if (!all(levels.problem.area %in% original.levels.problem.area))
    stop ("Levels in the problem area variable have changed.")
  ## Check mortality review done variable
  ofi.data$mortality.review.done <- tolower(as.character(ofi.data$mortality.review.done))
  levels.mortality.review.done <- unique(ofi.data$mortality.review.done)
  original.levels.mortality.review.done <- c(NA, 2, 1)
  if (!all(levels.quality.review.done %in% original.levels.quality.review.done))
    stop ("Levels in the mortality review done variable have changed.")
  ## Check preventable death variable
  levels.preventable.death <- unique(ofi.data$preventable.death)
  original.levels.preventable.death <- c(NA, "1", "3", "2", "999")
  if (!all(levels.preventable.death %in% original.levels.preventable.death))
    stop ("Levels in the preventable death variable have changed.")
  ## Return data
  return (ofi.data)
}
