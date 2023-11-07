preprocessing <- function(df, local_std = TRUE) {
  # Identifying the participants that need to be excluded can be done by
  # checking if the variable value is NULL.
  warnings <- c()
  # Warning:
  if (sum(!is.na(df$stroop_status)) > 0) {
    vtg::log$info("Cognitive test status available")
    # excluded <- unique(df$row[is.na(df$icam) | is.na(df$vwf) |
    #                            df$wlt_status != 1 |
    #                            df$stroop_status != 1 | df$ldst_status != 1 |
    #                            df$fluency_status != 1 | is.na(df$priority_executive_stroop_3_time) |
    #                            is.na(df$priority_memory_dr_ravlt)])
    included <- unique(c(df$row[!is.na(df$icam) & !is.na(df$vwf) & (df$wlt_status == 1 | df$stroop_status == 1 |
                                                                     df$ldst_status == 1 | df$fluency_status == 1 |
                                                                     !is.na(df$priority_executive_stroop_3_time) |
                                                                       !is.na(df$priority_memory_dr_ravlt))]))
    # include_biomarkers <- unique(c(df$row[!is.na(df$icam) & !is.na(df$vwf)]))
  } else {
    # !is.na(df$education_category_3) &
    included <- unique(c(df$row[!is.na(df$icam) & !is.na(df$vcam) & !is.na(df$e_selectin) & (
                               !is.na(df$attention_test_stroop_1_time) |
                               !is.na(df$attention_test_stroop_2_time) |
                               !is.na(df$priority_executive_stroop_3_time) |
                               !is.na(df$attention_test_ldst_60_correct) |
                               !is.na(df$priority_memory_im_pwlt) |
                               !is.na(df$priority_memory_de_pwlt) |
                               !is.na(df$priority_memory_im_15_word_list_correct) |
                               !is.na(df$priority_memory_dr_15_word_list_correct) |
                               !is.na(df$priority_language_animal_fluency_60_correct) |
                               !is.na(df$priority_language_category_fluency_60))]))
  }

  # Selected participants
  excluded <- unique(df$row[! df$row %in% included])
  vtg::log$info("Excluded '{length(excluded)}' participants")
  vtg::log$info("'{length(included)}' participants included in the analysis")
  df <- df[df$row %in% included,]

  if (nrow(df) == 0) {
    return(df)
  }

  # Mean-center age to avoid multicollinearity
  if (sum(is.na(df$date_attention)) > 0) {
    warnings <- c(warnings, "Date attention not available for all cases")
    print("Date attention not available for all cases")
    print(sum(is.na(df$date_attention)))
  }
  # Is age available?
  df$age <- ifelse(
    is.na(df$age),
    lubridate::time_length(difftime(df$date, paste(df$birth_year, "/01/01", sep="")), "years"),
    df$age
  )
  # If we want the age at the cognitive assessment
  #df$age <- ifelse(
  #  is.na(df$date_attention),
  #  df$age,
  #  lubridate::time_length(difftime(df$date_attention, paste(df$birth_year, "/01/01", sep="")), "years")
  #)

  df$Age_cent <- scale(
    df$age,
    scale = FALSE
    )
  df$Age2 <- df$Age_cent^2

  ## Sex
  df$sex_num <- as.numeric(df$sex)
  df$sex <- as.factor(df$sex)

  ## Education - had to change Cogeduc to N_Education_3cat
  df$Education <- as.factor(df$education_category_3)
  df$Education <- factor(df$Education, levels = c(0, 1, 2), labels = c("Low", "Medium", "High"))

  # Recode education for norm scores
  df$Education_low <- ifelse(df$Education == "Low", 1, 0)
  df$Education_high <- ifelse(df$Education == "High", 1, 0)

  ## Hypertension
  df$hypertension <- as.factor(df$hypertension)

  df$drugs_hypertension <- as.factor(df$drugs_hypertension)


  ## Hypercholesterolemia/Dyslipidemia
  df$hdl_ratio <- ifelse(is.na(df$hdl_ratio), (df$ldl + df$hdl) / df$hdl, df$hdl_ratio)

  df$drugs_hypercholesterolemia <- as.factor(df$drugs_hypercholesterolemia)

  ## Diabetes mellitus type 2
  df$dm_2 <- as.factor(df$dm_2)
  df$dm_type2 <- as.factor(ifelse(
    is.na(df$dm_2),
    df$dm,
    df$dm_2
  ))

  ## Smoking status
  df$smoking_behavior <- as.factor(df$smoking_behavior)

  ## Alcohol
  df$alcohol <- as.factor(df$alcohol)

  ## Prior CVD
  # alternative if CVD not directly available:
  df$cvd_alt <- ifelse(
    is.na(df$myocardial_infarction),
    df$stroke,
    ifelse(
      is.na(df$stroke),
      df$myocardial_infarction,
      df$myocardial_infarction | df$stroke
    )
  )
  df$cardiovascular_disease <- as.factor(
    ifelse(
      is.na(df$cardiovascular_disease),
      df$cvd_alt,
      df$cardiovascular_disease
    )
  )

  ## Markers of LGI
  df$IL6_2016F_std <- scale(df$il6)
  df$IL8_2016F_std <- scale(df$il8)
  df$CRP_2016F_std <- scale(df$crp)
  df$SAA_2016F_std <- scale(df$saa)
  df$TNFa_2016F_std <- scale(df$tnfalpha)

  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(LGIcomp = mean(c(IL6_2016F_std, IL8_2016F_std, CRP_2016F_std,
                                   SAA_2016F_std, TNFa_2016F_std))) -> df
  df$LGIcomp_std <- (df$LGIcomp - mean(df$LGIcomp, na.rm = TRUE))/sd(df$LGIcomp,
                                                                     na.rm = TRUE)
  ## Depression
  df$depression <- as.factor(df$depression)

  ##### Cognition
  ## Attention - Stroop I and Stroop II ODER CSTA and CSTB

  # Stroop I
  #df$STR1ts <- as.numeric(df$attention_test_stroop_1_time)
  #df$STR1ts <- ifelse(df$attention_test_stroop_2_time == "NA", NA, df$attention_test_stroop_1_time)

  df$ZSTR1ts <- (df$attention_test_stroop_1_time - (41.517 + (df$Age_cent * 0.131) + (df$Age2 * 0.003) +
                                                      (df$Education_low * 3.595) + (df$Education_high * -1.507))) / (sd(df$attention_test_stroop_1_time, na.rm = TRUE))

  df <- df %>%
    dplyr::mutate(invZSTR1ts = ZSTR1ts * (-1))
  # Stroop II (important - sex differently coded in vd Elst! 0 = female, 1 = male)
  df$ZSTR2ts <- (df$attention_test_stroop_2_time - (52.458 + (df$Age_cent * 0.209) + (df$Age2 * 0.007) +
                                                      (df$sex_num * -2.390) + (df$Education_low * 4.235) +
                                                      (df$Education_high * -2.346))) / (sd(df$attention_test_stroop_2_time, na.rm = TRUE))

  df <- df %>%
    dplyr::mutate(invZSTR2ts = ZSTR2ts * (-1))

  # Average for Attention composite score
  # Stroop 2 not available in some cohorts
  df <- df %>%
    dplyr::mutate(
      Priority_attention_z = ifelse(
        !is.na(invZSTR2ts),
        ((invZSTR1ts + invZSTR2ts)/2),
        invZSTR1ts
      )
    )

  # ## Episodic memory
  #
  # ## Immediate memory/Short-term memory - 15 WLT Trial 1-3
  # df$WLTscore3 <- (df$WLT1 + df$WLT2 + df$WLT3)

  # Cognitive test that will be selected
  df$priority_memory_selected <- ifelse(
    is.na(df$priority_memory_im_ravlt),
    ifelse(
      is.na(df$priority_memory_im_pwlt),
      df$priority_memory_im_15_word_list_correct,
      df$priority_memory_im_pwlt
    ),
    df$priority_memory_im_ravlt
  )
  df$ZWLTscore3 <- (df$priority_memory_selected - (25.440 + (df$Age_cent * -0.150) + (df$Age2 * -0.0016) +
                                                     (df$sex_num * 2.217) + (df$Education_low * -1.699) +
                                                     (df$Education_high * 1.467))) / sd(df$priority_memory_selected, na.rm = TRUE)

  vtg::log$info(sum(is.na(df$ZWLTscore3)))
  df <- df %>%
    dplyr::rename(Priority_memory_im_z = ZWLTscore3)
  vtg::log$info(sum(is.na(df$Priority_memory_im_z)))

  ## Delayed memory/Long-term memory - 15 WLT --> OR retention score???
  df$priority_memory_dr_selected <- ifelse(
    is.na(df$priority_memory_dr_ravlt),
    ifelse(
      is.na(df$priority_memory_de_pwlt),
      df$priority_memory_dr_15_word_list_correct,
      df$priority_memory_de_pwlt
    ),
    df$priority_memory_dr_ravlt
  )
  df$ZWLTRscore <- (df$priority_memory_dr_selected - (10.924 + (df$Age_cent * -0.073) + (df$Age2 * -0.0009) +
                                                     (df$sex_num * 1.197) + (df$Education_low * -0.844) +
                                                     (df$Education_high * 0.424))) / sd(df$priority_memory_dr_selected, na.rm = TRUE)

  df <- df %>%
    dplyr::rename(Priority_memory_dr_z = ZWLTRscore)


  ## Processing speed - Letter Digit Substitution Test
  df$ZLDST60 <- (df$attention_test_ldst_60_correct - (48.27 + (df$Age_cent * -0.28) + (df$sex_num * 0.81) +
                                                        (df$Education_low * -4.53) + (df$Education_high * 1.12))) /
    sd(df$attention_test_ldst_60_correct, na.rm = TRUE)
  df <- df %>%
    dplyr::rename(Priority_speed_z = ZLDST60)

  # ## Executive function -
  # # Stroop III or STRINT? if STRINT, recalculation in order studies needed!
  # # also, STRINT has 3 missings here; Willemijn says Stroop III
  df$ZSTR3ts <- (df$priority_executive_stroop_3_time - (82.601 + (df$Age_cent * 0.714) + (df$Age2 * 0.023) +
                                                          (df$sex_num * -4.470) + (df$Education_low * 13.285) +
                                                          (df$Education_high * -3.873))) / (sd(df$priority_executive_stroop_3_time, na.rm = TRUE))
  #dat <- dat %>% dplyr::mutate(lnZSTR3ts = log(ZSTR3ts)) # cannot do that because of negative and zero values!
  df <- df %>%
    dplyr::mutate(invZSTR3ts = ZSTR3ts * (-1))
  df <- df %>%
    dplyr::rename(Priority_executive_z = invZSTR3ts)
  #
  # ## Language - Animal fluency
  df$priority_language_selected <- ifelse(
    is.na(df$priority_language_animal_fluency_60_correct),
    df$priority_language_category_fluency_60,
    df$priority_language_animal_fluency_60_correct
  )
  df$ZFLUcor <- (df$priority_language_selected - (24.777 + (df$Age_cent * -0.97) + (df$Education_low * 2.790) +
                                                                     (df$Education_high * 1.586))) / sd(df$priority_language_selected, na.rm = TRUE)
  df <- df %>%
    dplyr::rename(Priority_language_z = ZFLUcor)


  ##### Endothelial dysfunction
  # because R seems to have problems with E-Selectin as name, I use an underscore
  df <- df %>%
    dplyr::rename(E_Selectin = e_selectin)
  df <- df %>%
    dplyr::rename(ICAM = icam)
  df <- df %>%
    dplyr::rename(VCAM = vcam)

  df$ZE_Selectin <- scale(df$E_Selectin)
  df$ZICAM <- scale(df$ICAM)
  df$ZVCAM <- scale(df$VCAM)

  df$ZvWF <- scale(df$vwf)
  # vwf - only available for the MS
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(EDcomp = ifelse(
      is.na(ZvWF),
      ifelse(
        is.na(ZE_Selectin),
        mean(c(ZICAM, ZVCAM)),
        mean(c(ZICAM, ZVCAM, ZE_Selectin))
      ),
      mean(c(ZICAM, ZVCAM, ZE_Selectin, ZvWF)))
    ) -> df

  df$ZEDcomp <- (df$EDcomp - mean(df$EDcomp, na.rm = TRUE))/sd(df$EDcomp,
                                                               na.rm = TRUE)
  return(df)
}
