library(dplyr)

combine_rts <- function(data) {
  
  #################
  # Combine RTS #
  #################
  
  # Calculate mean values for SBP categories
  ed_sbp_means <- data %>%
    summarize(
      ed_sbp_4 = round(mean(ed_sbp_value[ed_sbp_value > 89], na.rm = TRUE)),
      ed_sbp_3 = round(mean(ed_sbp_value[ed_sbp_value <= 89 & ed_sbp_value > 75], na.rm = TRUE)),
      ed_sbp_2 = round(mean(ed_sbp_value[ed_sbp_value <= 75 & ed_sbp_value > 49], na.rm = TRUE)),
      ed_sbp_1 = round(mean(ed_sbp_value[ed_sbp_value <= 49 & ed_sbp_value > 1], na.rm = TRUE))
    )
  
  pre_sbp_means <- data %>%
    summarize(
      pre_sbp_4 = round(mean(pre_sbp_value[pre_sbp_value > 89], na.rm = TRUE)),
      pre_sbp_3 = round(mean(pre_sbp_value[pre_sbp_value <= 89 & pre_sbp_value > 75], na.rm = TRUE)),
      pre_sbp_2 = round(mean(pre_sbp_value[pre_sbp_value <= 75 & pre_sbp_value > 49], na.rm = TRUE)),
      pre_sbp_1 = round(mean(pre_sbp_value[pre_sbp_value <= 49 & pre_sbp_value > 1], na.rm = TRUE))
    )
  
  # Calculate mean values for RR categories
  ed_rr_means <- data %>%
    summarize(
      ed_rr_4 = round(mean(ed_rr_value[ed_rr_value > 9 & ed_rr_value < 30], na.rm = TRUE)),
      ed_rr_3 = round(mean(ed_rr_value[ed_rr_value >= 30], na.rm = TRUE)),
      ed_rr_2 = round(mean(ed_rr_value[ed_rr_value <= 9 & ed_rr_value > 5], na.rm = TRUE)),
      ed_rr_1 = round(mean(ed_rr_value[ed_rr_value <= 5 & ed_rr_value > 0], na.rm = TRUE))
    )
  
  pre_rr_means <- data %>%
    summarize(
      pre_rr_4 = round(mean(pre_rr_value[pre_rr_value > 9 & pre_rr_value < 30], na.rm = TRUE)),
      pre_rr_3 = round(mean(pre_rr_value[pre_rr_value >= 30], na.rm = TRUE)),
      pre_rr_2 = round(mean(pre_rr_value[pre_rr_value <= 9 & pre_rr_value > 5], na.rm = TRUE)),
      pre_rr_1 = round(mean(pre_rr_value[pre_rr_value <= 5 & pre_rr_value > 0], na.rm = TRUE))
    )
  
  # Update data with calculated means
  data <- data %>%
    mutate(
      # ED SBP
      ed_sbp_value = case_when(
        is.na(ed_sbp_value) & !is.na(ed_sbp_rtscat) & ed_sbp_rtscat == 4 ~ ed_sbp_means$ed_sbp_4,
        is.na(ed_sbp_value) & !is.na(ed_sbp_rtscat) & ed_sbp_rtscat == 3 ~ ed_sbp_means$ed_sbp_3,
        is.na(ed_sbp_value) & !is.na(ed_sbp_rtscat) & ed_sbp_rtscat == 2 ~ ed_sbp_means$ed_sbp_2,
        is.na(ed_sbp_value) & !is.na(ed_sbp_rtscat) & ed_sbp_rtscat == 1 ~ ed_sbp_means$ed_sbp_1,
        is.na(ed_sbp_value) & !is.na(ed_sbp_rtscat) & ed_sbp_rtscat == 0 ~ 0,
        TRUE ~ ed_sbp_value
      ),
      
      # Pre SBP
      pre_sbp_value = case_when(
        is.na(pre_sbp_value) & !is.na(pre_sbp_rtscat) & pre_sbp_rtscat == 4 ~ pre_sbp_means$pre_sbp_4,
        is.na(pre_sbp_value) & !is.na(pre_sbp_rtscat) & pre_sbp_rtscat == 3 ~ pre_sbp_means$pre_sbp_3,
        is.na(pre_sbp_value) & !is.na(pre_sbp_rtscat) & pre_sbp_rtscat == 2 ~ pre_sbp_means$pre_sbp_2,
        is.na(pre_sbp_value) & !is.na(pre_sbp_rtscat) & pre_sbp_rtscat == 1 ~ pre_sbp_means$pre_sbp_1,
        is.na(pre_sbp_value) & !is.na(pre_sbp_rtscat) & pre_sbp_rtscat == 0 ~ 0,
        TRUE ~ pre_sbp_value
      ),
      
      # ED RR
      ed_rr_value = case_when(
        is.na(ed_rr_value) & !is.na(ed_rr_rtscat) & ed_rr_rtscat == 4 ~ ed_rr_means$ed_rr_4,
        is.na(ed_rr_value) & !is.na(ed_rr_rtscat) & ed_rr_rtscat == 3 ~ ed_rr_means$ed_rr_3,
        is.na(ed_rr_value) & !is.na(ed_rr_rtscat) & ed_rr_rtscat == 2 ~ ed_rr_means$ed_rr_2,
        is.na(ed_rr_value) & !is.na(ed_rr_rtscat) & ed_rr_rtscat == 1 ~ ed_rr_means$ed_rr_1,
        is.na(ed_rr_value) & !is.na(ed_rr_rtscat) & ed_rr_rtscat == 0 ~ 0,
        TRUE ~ ed_rr_value
      ),
      
      # Pre RR
      pre_rr_value = case_when(
        is.na(pre_rr_value) & !is.na(pre_rr_rtscat) & pre_rr_rtscat == 4 ~ pre_rr_means$pre_rr_4,
        is.na(pre_rr_value) & !is.na(pre_rr_rtscat) & pre_rr_rtscat == 3 ~ pre_rr_means$pre_rr_3,
        is.na(pre_rr_value) & !is.na(pre_rr_rtscat) & pre_rr_rtscat == 2 ~ pre_rr_means$pre_rr_2,
        is.na(pre_rr_value) & !is.na(pre_rr_rtscat) & pre_rr_rtscat == 1 ~ pre_rr_means$pre_rr_1,
        is.na(pre_rr_value) & !is.na(pre_rr_rtscat) & pre_rr_rtscat == 0 ~ 0,
        TRUE ~ pre_rr_value
      )
    )
  
  return(data)
}
