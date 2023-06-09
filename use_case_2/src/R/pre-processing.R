preprocessing <- function(df) {
  # Identifying the participants that need to be excluded can be done by
  # checking if the variable value is NULL.

  excluded <- unique(df$id[is.na(df$icam) | is.na(df$vwf) |
                             df$wlt_status != 1 |
                             df$stroop_status != 1 | df$ldst_status != 1 |
                             df$fluency_status != 1 | is.na(df$priority_executive_stroop_3_time) |
                             is.na(df$priority_memory_dr_ravlt)])
  vtg::log$info("Excluded '{length(excluded)}' participants")

  included <- unique(df$id[! df$id %in% excluded])
  vtg::log$info("'{length(included)}' participants included in the analysis")

  # Selected participants
  included <- unique(df$id[! df$id %in% excluded])
  df <- df[df$id %in% included,]

  # Mean-center age to avoid multicollinearity
  df$Age_cent <- scale(df$age, scale = FALSE)
  df$Age2 <- df$Age_cent^2

  ## Sex
  df$Sex <- as.factor(df$sex)
  # df <- df %>%
  #   dplyr::mutate(Sex = forcats::fct_recode(
  #     sex,
  #     "0" = "m",
  #     "1" = "f"
  #   ))
  df$sex <- NULL
  df$Sex_num <- as.numeric(df$Sex)

  ## Education - had to change Cogeduc to N_Education_3cat
  df$Education <- as.factor(df$education_category_3)
  df$Education <- factor(df$Education, labels = c("Low", "Medium", "High"))

  # Recode education for norm scores
  df$Education_low <- ifelse(df$Education == "Low",1,0)
  df$Education_high <- ifelse(df$Education == "High",1,0)

  ## BMI
  df <- df %>%  dplyr::rename("BMI" = "bmi")

  ## Hypertension
  df$Hypertension <- as.factor(df$hypertension)
  df$hypertension <- NULL

  df$Drugs_hypertension <- as.factor(df$drugs_hypertension)
  df$drugs_hypertension <- NULL


  ## Hypercholesterolemia/Dyslipidemia
  df$N_Chol_ratio <- df$hdl_ratio

  df$Drugs_hypercholesterolemia <- as.factor(df$drugs_hypercholesterolemia)
  df$drugs_hypercholesterolemia <- NULL


  ## Diabetes mellitus type 2
  df$DM_2 <- as.factor(df$dm_2)
  df$dm_2 <- NULL

  ## Smoking status
  df$Smoking <- as.factor(df$smoking_behavior)
  df$smoking_behavior <- NULL


  ## Alcohol
  df$Alcohol <- as.factor(df$alcohol)

  ## Prior CVD
  df$N_CVD <- as.factor(df$cardiovascular_disease)

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
  df$Depression <- as.factor(df$depression)
  df$depression <- NULL

  ##### Cognition
  ## Attention - Stroop I and Stroop II ODER CSTA and CSTB

  # Stroop I
  df$STR1ts <- as.numeric(df$attention_test_stroop_1_time)
  df$STR1ts <- ifelse(df$attention_test_stroop_2_time == "NA", NA, df$attention_test_stroop_1_time)

  df$ZSTR1ts <- (df$attention_test_stroop_1_time - (41.517 + (df$Age_cent * 0.131) + (df$Age2 * 0.003) +
                                                      (df$Education_low * 3.595) + (df$Education_high * -1.507))) / (sd(df$attention_test_stroop_1_time, na.rm = TRUE))

  df <- df %>%
    dplyr::mutate(invZSTR1ts = ZSTR1ts * (-1))
  # Stroop II (important - sex differently coded in vd Elst! 0 = female, 1 = male)
  df$ZSTR2ts <- (df$attention_test_stroop_2_time - (52.458 + (df$Age_cent * 0.209) + (df$Age2 * 0.007) +
                                                      (df$Sex_num * -2.390) + (df$Education_low * 4.235) +
                                                      (df$Education_high * -2.346))) / (sd(df$attention_test_stroop_2_time, na.rm = TRUE))

  df <- df %>%
    dplyr::mutate(invZSTR2ts = ZSTR2ts * (-1))

  # Average for Attention composite score
  df <- df %>%
    dplyr::mutate(Priority_attention_z = ((invZSTR1ts + invZSTR2ts)/2))
  df <- df %>%
    dplyr::mutate(invZSTR1ts = ZSTR1ts * (-1))

  ## Episodic memory

  ## Immediate memory/Short-term memory - 15 WLT Trial 1-3
  # df$WLTscore3 <- (df$WLT1 + df$WLT2 + df$WLT3)
  df$ZWLTscore3 <- (df$priority_memory_im_ravlt - (25.440 + (df$Age_cent * -0.150) + (df$Age2 * -0.0016) +
                                                     (df$Sex_num * 2.217) + (df$Education_low * -1.699) +
                                                     (df$Education_high * 1.467))) / sd(df$priority_memory_im_ravlt, na.rm = TRUE)
  df <- df %>%
    dplyr::rename(Priority_memory_im_z = ZWLTscore3)

  ## Delayed memory/Long-term memory - 15 WLT --> OR retention score???
  df$ZWLTRscore <- (df$priority_memory_dr_ravlt - (10.924 + (df$Age_cent * -0.073) + (df$Age2 * -0.0009) +
                                                     (df$Sex_num * 1.197) + (df$Education_low * -0.844) +
                                                     (df$Education_high * 0.424))) / sd(df$priority_memory_dr_ravlt, na.rm = TRUE)
  df <- df %>%
    dplyr::rename(Priority_memory_dr_z = ZWLTRscore)


  ## Processing speed - Letter Digit Substitution Test
  df$ZLDST60 <- (df$attention_test_ldst_60_correct - (48.27 + (df$Age_cent * -0.28) + (df$Sex_num * 0.81) +
                                                        (df$Education_low * -4.53) + (df$Education_high * 1.12))) /
    sd(df$attention_test_ldst_60_correct, na.rm = TRUE)
  df <- df %>%
    dplyr::rename(Priority_speed_z = ZLDST60)

  ## Executive function -
  # Stroop III or STRINT? if STRINT, recalculation in order studies needed!
  # also, STRINT has 3 missings here; Willemijn says Stroop III
  df$ZSTR3ts <- (df$priority_executive_stroop_3_time - (82.601 + (df$Age_cent * 0.714) + (df$Age2 * 0.023) +
                                                          (df$Sex_num * -4.470) + (df$Education_low * 13.285) +
                                                          (df$Education_high * -3.873))) / (sd(df$priority_executive_stroop_3_time, na.rm = TRUE))
  #dat <- dat %>% dplyr::mutate(lnZSTR3ts = log(ZSTR3ts)) # cannot do that because of negative and zero values!
  df <- df %>%
    dplyr::mutate(invZSTR3ts = ZSTR3ts * (-1))
  df <- df %>%
    dplyr::rename(Priority_executive_z = invZSTR3ts)

  ## Language - Animal fluency
  df$ZFLUcor <- (df$priority_language_animal_fluency_60_correct - (24.777 + (df$Age_cent * -0.97) + (df$Education_low * 2.790) +
                                                                     (df$Education_high * 1.586))) / sd(df$priority_language_animal_fluency_60_correct, na.rm = TRUE)
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

  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(EDcomp = mean(c(ZICAM, ZVCAM, ZE_Selectin, ZvWF))) -> df

  df$ZEDcomp <- (df$EDcomp - mean(df$EDcomp, na.rm = TRUE))/sd(df$EDcomp,
                                                               na.rm = TRUE)
  return(df)
}
