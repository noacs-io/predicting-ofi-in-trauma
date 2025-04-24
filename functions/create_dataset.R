library(rofi)
library(stringr)
library(themis)

source("functions/combine_rts.R")


create_dataset <- function(){
  dataset <- rofi::import_data() %>%
    rofi::merge_data()
  
  # CAVE: Dirty temp fix for duplicated columns
  tmp <-
    ifelse(
      is.na(dataset$DateTime_FirstNormalBasecess),
      dataset[duplicated(colnames(dataset))][, 1],
      dataset$DateTime_FirstNormalBasecess
    )
  dataset <- dataset[!duplicated(colnames(dataset))]
  dataset$DateTime_FirstNormalBasecess <- unlist(tmp)
  
  dataset <- dataset %>% tibble()
  
  # Only keep adults
  dataset <- dataset %>% filter(pt_age_yrs >= 15)
  
  dataset <- dataset %>% mutate(
    DateTime_Case = ifelse(is.na(DateTime_Of_Trauma), DateTime_ArrivalAtHospital, DateTime_Of_Trauma) %>% as.POSIXct(),
    ofi = rofi::create_ofi(.),
  )
  
  # Only keep patients screened for OFI
  dataset <- dataset %>% filter(!is.na(ofi))
  
  # Remove documentation ofi
  dataset <- dataset %>% mutate(
    ofi = ifelse(grepl("Dokumetation|dokumentation|Dokumentation", Problemomrade_.FMP), "No", ofi)
  )
  
  # Add DOA column
  dataset <- dataset %>%
    mutate(
      DOA = case_when(
        is.na(Fr1.12) | Fr1.12 == 2 ~ "No",
        Fr1.12 == 1 ~ "Yes",
        TRUE ~ "No"
      )
    )
  
  # Clean audit filters (So VK_* are Yes/No)
  dataset <- clean_audit_filters(dataset)
  
  # Clean predictors
  dataset <- clean_predictors(dataset)
  
  # Replace missing values in come physiological parameters with the average RTS value 
  dataset <- combine_rts(dataset)
  
  return(dataset)
  
}

clean_audit_filters <- function(data) {
  
  # Define audit filter columns
  audit_filter <- c("VK_hlr_thorak", "VK_sap_less90", "VK_leverskada",
                    "VK_gcs_less9_ej_intubTE", "VK_mjaltskada", "VK_mer_30min_DT",
                    "VK_mass_transf", "VK_mer_60min_interv", "VK_iss_15_ej_iva",
                    "VK_ej_trombrof_TBI_72h", "VK_iss_15_ej_TE", "VK_avslutad", "VK_annat")
  

  # Standardize "Yes" values
  data <- data %>%
    mutate(across(all_of(audit_filter), 
                  ~case_when(
                    . %in% c("Ja", "ja") ~ "Yes",
                    . %in% c("Nej", "nej", "nj\r\nNej", "nj", "nn") ~ "No",
                    TRUE ~ .
                  )))
  
  
  
  # Convert NA values to "No" if VK_avslutad is "Yes"
  data <- data %>%
    mutate(across(all_of(audit_filter),
                  ~if_else(is.na(.) & VK_avslutad == "Yes", "No", .)))
  
  return(data)
}

clean_predictors <- function(data) {
  # Create intub variable
  data <- data %>%
    mutate(intub = if_else(!is.na(pre_intubated) & pre_intubated == 1, 3, ed_intubated))
  
  # Define continuous and categorical variables
  cont_var <- c("pt_age_yrs", "pre_gcs_sum", "pre_gcs_motor", "ed_gcs_sum", "ed_gcs_motor",
                "pre_sbp_value", "ed_sbp_value", "pre_rr_value", "ed_rr_value", "ed_be_art",
                "ed_inr", "hosp_vent_days", "hosp_los_days", "res_gos_dischg", "ISS", "NISS",
                "NumberOfActions", "NumberOfInjuries", "iva_dagar_n", "iva_vardtillfallen_n")
  
  cat_var <- c("intub", "Gender", "pt_Gender", "ed_be_art_NotDone", "dt_ed_norm_be", "hosp_dischg_dest", "res_survival",
               "inj_dominant", "inj_mechanism", "inj_intention", "pt_asa_preinjury", "pre_card_arrest",
               "pre_sbp_rtscat", "ed_sbp_rtscat", "pre_rr_rtscat", "ed_rr_rtscat", "ed_inr_NotDone",
               "host_vent_days_NotDone", "pre_provided", "pre_intubated", "pre_intub_type",
               "ed_intubated", "ed_intub_type", "ed_tta", "ed_emerg_proc_other", "ed_emerg_proc", "pre_transport",
               "Deceased", "bedomn_primar_granskning", "Riktlinje",
               "waran_beh_vid_ank", "noak_vid_ankomst",
               "TraumaAlarmCriteria", "TraumaAlarmAtHospital", "AlarmRePrioritised",
               "FirstTraumaDT_NotDone", "ISS_less_15_trauma_1_2", "korrekt_triage",
               "tid_skadeplats_moore_20min", "host_transfered", "host_care_level",
               "tra_DodsfallsanalysGenomford", "ISS_moore_15_trauma_3", "GCS_less13_DT_moore_120",
               "Kön", "Tr_Nivå", "tillavdelning_Direkt_eft_TE_AKM", "IVA_efter_TE",
               "IVA_eft_avd", "Flyttad_till_avd_eft_iva", "Problemomrade_.FMP", "ofi")
  
  vars <- c(cat_var, cont_var)
  
  # Variables that should contain 99
  var_99 <- c("ed_emergency_pro", "TraumaAlarmAtHospital", "pt_age_yrs", "pre_sbp_value", 
              "ed_sbp_value", "hosp_vent_days", "hosp_los_days", "iva_dagar_n", "iva_vardtillfallen_n")
  
  # Predictor specific cleaning
  data <- data %>%
    mutate(
      # If DT not done, set time to DT to 0
      dt_ed_first_ct = if_else(FirstTraumaDT_NotDone == 1, 0, dt_ed_first_ct),
      dt_ed_emerg_proc = if_else(ed_emerg_proc == 99, 0, dt_ed_emerg_proc),
      ed_emerg_proc_other = if_else(ed_emerg_proc == 99, 99, ed_emerg_proc_other),
      )
  
  # Process data
  data <- data %>%
    # Replace 999 with NA
    mutate(across(all_of(vars), ~{
      if(is.numeric(.)) na_if(., 999) else na_if(., "999")
    })) %>%
    # Convert categorical variables to factors
    mutate(across(all_of(cat_var), as.factor)) %>%
    # Fix decimal separator in specific columns
    mutate(
      ed_be_art = as.numeric(str_replace(ed_be_art, ",", ".")),
      ed_inr = as.numeric(str_replace(ed_inr, ",", ".")),
      NumberOfActions = as.numeric(NumberOfActions),
      NumberOfInjuries = as.numeric(NumberOfInjuries),
      dt_ed_norm_be = as.numeric(dt_ed_norm_be)
    )
  
  return(data)
}


