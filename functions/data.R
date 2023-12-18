library(rofi)
library(stringr)
library(themis)
source("functions/create_ofi2.R")

normalise.vector <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

create.dataset <-
  function(data.fraction = 1.0,
           include.doa = TRUE) {
    ## Import data
    datasets <- rofi::import_data()
    
    ## Merge data
    combined.dataset <- rofi::merge_data(datasets)
    
    #
    # CAVE: Temp fix!
    #
    # Dirty temp fix for duplicated columns
    tmp <-
      ifelse(
        is.na(combined.dataset$DateTime_FirstNormalBasecess),
        combined.dataset[duplicated(colnames(combined.dataset))][, 1],
        combined.dataset$DateTime_FirstNormalBasecess
      )
    combined.dataset <- combined.dataset[!duplicated(colnames(combined.dataset))]
    combined.dataset$DateTime_FirstNormalBasecess <- unlist(tmp)
    
    # Use a fraction of the dataset for debugging fast
    if (data.fraction < 1) {
      combined.dataset <-
        combined.dataset[sample(nrow(combined.dataset), floor(nrow(combined.dataset) * data.fraction)), ]
    }
    
    # Create DateTime_Case
    combined.dataset$DateTime_Case <-
      as.POSIXct(
        ifelse(
          is.na(combined.dataset$DateTime_Of_Trauma),
          combined.dataset$DateTime_ArrivalAtHospital,
          combined.dataset$DateTime_Of_Trauma
        )
      )
    combined.dataset$DateTime_Case_Year <- as.numeric(format(combined.dataset$DateTime_Case, format = "%Y"))
    
    
    #
    # CAVE: Temp fix!
    #
    ## Create OFI column
    #combined.dataset$ofi <- rofi::create_ofi(combined.dataset)
    combined.dataset$ofi <- create_ofi2(combined.dataset)
    
    # Remove documentation OFI
    combined.dataset$ofi <-
      ifelse(
        grepl(
          "Dokumetation|dokumentation|Dokumentation",
          combined.dataset$Problemomrade_.FMP
        ),
        "No",
        combined.dataset$ofi
      )
    
    combined.dataset <- clean.audit.filters(combined.dataset)
    
    ## Separate and store cases without known outcome
    missing.outcome <- is.na(combined.dataset$ofi)
    combined.dataset <- combined.dataset[!missing.outcome, ]
    
    combined.dataset <- combined.dataset[combined.dataset$pt_age_yrs > 14, ]
    
    ## Fix formating and remove wrong values like 999
    #combined.dataset <- clean_predictor(combined.dataset)
    combined.dataset <- clean.predictors(combined.dataset)
    
    # Add DOA column
    combined.dataset[is.na(combined.dataset$Fr1.12) | combined.dataset$Fr1.12 == 2 ,"DOA"] <- "No"
    combined.dataset[combined.dataset$Fr1.12 == 1 & is.na(combined.dataset$DOA) == TRUE,"DOA"] <- "Yes"
    
    if (!include.doa) {
      # remove doa
      combined.dataset <- combined.dataset[combined.dataset$DOA == "No", ]
    }
    
    ## Integrate RTS
    combined.dataset <- combine.rts(combined.dataset)
    
    return(combined.dataset)
  }

preprocess.data <- function(data,
                            columns.to.keep = c(
                              "ed_emerg_proc_other",
                              "AlarmRePrioritised",
                              "dt_ed_emerg_proc",
                              "hosp_dischg_dest",
                              "dt_ed_first_ct",
                              "TraumaAlarmAtHospital",
                              "pt_Gender",
                              "ISS",
                              "ed_rr_value",
                              "ed_gcs_sum",
                              "hosp_los_days",
                              "ed_emerg_proc",
                              "ed_sbp_value",
                              "res_survival",
                              "res_gos_dischg",
                              "pt_age_yrs",
                              "host_care_level",
                              "ofi"
                            ),
                            verbose = FALSE) {
  recipe <-
    recipe(ofi ~ ., data = data) %>%
    step_rm(all_predictors(), -all_of(columns.to.keep)) %>%
    step_indicate_na(all_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_nzv(all_predictors()) %>%
    step_adasyn(ofi)
  
  return (recipe)
}

#################################
# Clean audit-filters to Yes/No #
#################################

clean.audit.filters <- function(data) {
  
  
  audit.filter <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                    "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                    "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                    "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE","VK_avslutad","VK_annat")
  
  audit.filter2 <- c("VK_hlr_thorak","VK_sap_less90","VK_leverskada",
                     "VK_gcs_less9_ej_intubTE","VK_mjaltskada","VK_mer_30min_DT",
                     "VK_mass_transf","VK_mer_60min_interv","VK_iss_15_ej_iva",
                     "VK_ej_trombrof_TBI_72h","VK_iss_15_ej_TE","VK_annat")
  
  data[,audit.filter][data[,audit.filter] == "Ja"| 
                        data[,audit.filter] == "ja"] <- "Yes"
  
  data[,audit.filter][data[,audit.filter] == "Nej"| 
                        data[,audit.filter] == "nej" | 
                        data[,audit.filter] == "nj\r\nNej" | 
                        data[,audit.filter] == "nj"] <- "No"
  ##### Is nn = NA or No???
  
  data[,audit.filter][data[,audit.filter] == "nn"] <- NA
  
  ### Create reference vector to check for false inputs in the audit filters.  
  Levels.audit.filters <- unique(as.vector(as.matrix(data[,audit.filter])))
  Levels.audit.filters <- Levels.audit.filters[!is.na(Levels.audit.filters)]
  
  ##### 
  ##.   SIC (!!!) The safety check is not compatible with a bootsrap, replications probably lack one of the original values.
  ######
  #original.levels.audit.filters <- sort(c("Yes", NA, "No"))
  #if (!identical(Levels.audit.filters, original.levels.audit.filters))
  #  stop ("Levels in Audit filters have changed")
  
  #########
  #  Convert NA:s in VK rows to No if VK_avslutad = Yes (To be able to calc false neg)
  #########
  
  data[, audit.filter2] <- lapply(data[, audit.filter2], function(column) {
    column[is.na(column) & data$VK_avslutad == "Yes"] <- "No"
    return (column)
  })    
  
  return(data)
}


#######################
### Clean predictors ##
#######################
#
# TODO:something beside summary() to check correct formats?
# Fix dates?
# Integrate RTS?
# Not done columns?
#data <- dataset.clean.af
clean.predictors <- function(data) {
  
  data <- as.data.frame(data)
  data$intub <- with(data, ifelse(`pre_intubated` == 1 & is.na(data$pre_intubated) == FALSE, 3, `ed_intubated`))  
  
  ###cont
  cont.var<- c("pt_age_yrs","pre_gcs_sum","pre_gcs_motor","ed_gcs_sum","ed_gcs_motor",
               "pre_sbp_value","ed_sbp_value","pre_rr_value","ed_rr_value","ed_be_art",
               "ed_inr","hosp_vent_days","hosp_los_days","res_gos_dischg","ISS","NISS",
               "NumberOfActions","NumberOfInjuries","iva_dagar_n","iva_vardtillfallen_n")  
  
  ###cat 
  cat.var <- c("intub","Gender","pt_Gender","ed_be_art_NotDone","dt_ed_norm_be","hosp_dischg_dest","res_survival",           
               "inj_dominant","inj_mechanism","inj_intention","pt_asa_preinjury","pre_card_arrest",
               "pre_sbp_rtscat","ed_sbp_rtscat","pre_rr_rtscat","ed_rr_rtscat","ed_inr_NotDone",
               "host_vent_days_NotDone","pre_provided","pre_intubated","pre_intub_type",
               "ed_intubated","ed_intub_type","ed_tta","ed_emerg_proc_other","ed_emerg_proc","pre_transport",
               "Deceased","bedomn_primar_granskning","Riktlinje",
               "waran_beh_vid_ank","noak_vid_ankomst",
               "TraumaAlarmCriteria","TraumaAlarmAtHospital","AlarmRePrioritised",
               "FirstTraumaDT_NotDone","ISS_less_15_trauma_1_2","korrekt_triage",
               "tid_skadeplats_moore_20min","host_transfered", "host_care_level",
               "tra_DodsfallsanalysGenomford","ISS_moore_15_trauma_3","GCS_less13_DT_moore_120",
               "Kön","Tr_Nivå","tillavdelning_Direkt_eft_TE_AKM","IVA_efter_TE",
               "IVA_eft_avd","Flyttad_till_avd_eft_iva","Problemomrade_.FMP","ofi")
  
  vars <- c(cat.var,cont.var)
  
  #############################################
  # Change obvios values to NA (Like 999 etc) #
  #############################################
  
  
  ## Vars.99 = Vars that that should contain 99
  # "ed_emergency_proc" 99 == No action
  # "TraumaAlarmAtHospital" 99 == No trauma alarm
  # Note that ed_gcs_sum 99 == Intubated in a prehospital seting but we need to remove to calculate correct median/mean
  var.99 <- c("ed_emergency_pro","TraumaAlarmAtHospital","pt_age_yrs","pre_sbp_value","ed_sbp_value","hosp_vent_days","hosp_los_days","iva_dagar_n","iva_vardtillfallen_n")
  
  test.data <- data[,vars]
  test.data[test.data == 999] <- NA
  test.data[test.data == 9999] <- NA #### Consider removing since 9999 actually means not relevant?
  test.data[, -which(names(test.data) %in% var.99)][test.data[, -which(names(test.data) %in% var.99)] == 99 ] <- NA
  
  
  ##### Fix formating to factor/numeric for easy screening
  
  formated.data <- test.data
  formated.data[cat.var] <- lapply(formated.data[cat.var], factor)
  formated.data$ed_be_art = str_replace(formated.data$ed_be_art,",",".")
  formated.data$ed_inr = str_replace(formated.data$ed_inr,",",".")
  formated.data$ed_inr <- as.numeric(formated.data$ed_inr)
  formated.data$ed_be_art <- as.numeric(formated.data$ed_be_art)
  formated.data$NumberOfActions <- as.numeric(formated.data$NumberOfActions)
  formated.data$NumberOfInjuries <- as.numeric(formated.data$NumberOfInjuries)
  
  formated.data$dt_ed_norm_be <- as.numeric(formated.data$dt_ed_norm_be)
  ###### I Used summary to check each category, to my knowledge they follow the swetrau-manual
  ###### Need to add some better screening tool
  
  data[vars] <- formated.data
  
  return(data)
  
}

