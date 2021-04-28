###SCARED script created by RK to clean and score SCARED data straight from redcap
#last edited 4.22.21 (still draft)
#to run: first load the SCAREDC and P scoring functions (at bottom) before proceeding
#rk note: loaded in session notes for troubleshooting purposes - can remove once troubleshooting is complete

#laod packages
library(magrittr)
library(foreign)
library(tidyr)
library(texreg)
library(REDCapR)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(eeptools)
library(lubridate)

#load api: note you will need redcap API access in order to proceed
#redcap api token call (will need to request API token from redcap admin)
source("~/Desktop/config.R")
#filters and only pulls study eligible subjects
subjlist_erp<- c("subj","eligibility", "exclusion_criteria")
eligible_erp <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = subjlist_erp
)$data
subjlist_nirs<- c("subj","eligibility", "exclusion_criteria")
eligible_nirs <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = subjlist_nirs
)$data


erplist <- eligible_erp %>%
  filter(eligibility ==1) 
nirslist<- eligible_nirs %>%
  filter(eligibility ==1) 
#full eligible subject list

#relevant dates
demographics <- read.csv("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/Redcap/Demographics/Emo-Demographics.csv")
imp_dates <- demographics %>% select(subj,DOBi,DOBf,DOBm)

fullsubjlist <- full_join(erplist,nirslist) %>%select(subj,eligibility)
fullsubjlist <- left_join(fullsubjlist,imp_dates)
### SCARED C: pulls the scared c data from internal redcap--------
scaredc_erp <- c("subj","scared_c_date","scared_c_1","scared_c_2","scared_c_3","scared_c_4","scared_c_5","scared_c_6","scared_c_7","scared_c_8",
                 "scared_c_9","scared_c_10","scared_c_11","scared_c_12","scared_c_13","scared_c_14","scared_c_15","scared_c_16","scared_c_17",
                 "scared_c_18","scared_c_19","scared_c_20","scared_c_21","scared_c_22","scared_c_23","scared_c_24","scared_c_25","scared_c_26",
                 "scared_c_27","scared_c_28", "scared_c_29","scared_c_30","scared_c_31","scared_c_32","scared_c_33","scared_c_34","scared_c_35","scared_c_36",
                 "scared_c_37","scared_c_38", "scared_c_39","scared_c_40","scared_c_41")

scaredcERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = scaredc_erp
)$data

scaredc_nirs <- c("subj","scared_c_date","scared_c_1","scared_c_2","scared_c_3","scared_c_4","scared_c_5","scared_c_6","scared_c_7","scared_c_8",
                 "scared_c_9","scared_c_10","scared_c_11","scared_c_12","scared_c_13","scared_c_14","scared_c_15","scared_c_16","scared_c_17",
                 "scared_c_18","scared_c_19","scared_c_20","scared_c_21","scared_c_22","scared_c_23","scared_c_24","scared_c_25","scared_c_26",
                 "scared_c_27","scared_c_28", "scared_c_29","scared_c_30","scared_c_31","scared_c_32","scared_c_33","scared_c_34","scared_c_35","scared_c_36",
                 "scared_c_37","scared_c_38", "scared_c_39","scared_c_40","scared_c_41")

scaredcNIRS<- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = scaredc_nirs
)$data

scaredcERP <- scaredcERP %>%
  filter(redcap_event_name=="7year_visit_arm_1")
scaredcNIRS <- scaredcNIRS%>%
  filter(redcap_event_name=="7year_visit_arm_1")


scaredc_all <- full_join(scaredcERP,scaredcNIRS)
#exclude ineligible subjects
scaredc_all <- left_join(fullsubjlist,scaredc_all)

scaredc_full <- scaredc_all[!is.na(scaredc_all$scared_c_date), ]

### SCARED P: pulls the scared p data from internal redcap--------
scaredp_erp <- c("subj","scared_p_date","scared_p_1","scared_p_2","scared_p_3","scared_p_4","scared_p_5","scared_p_6","scared_p_7","scared_p_8",
                 "scared_p_9","scared_p_10","scared_p_11","scared_p_12","scared_p_13","scared_p_14","scared_p_15","scared_p_16","scared_p_17",
                 "scared_p_18","scared_p_19","scared_p_20","scared_p_21","scared_p_22","scared_p_23","scared_p_24","scared_p_25","scared_p_26",
                 "scared_p_27","scared_p_28", "scared_p_29","scared_p_30","scared_p_31","scared_p_32","scared_p_33","scared_p_34","scared_p_35","scared_p_36",
                 "scared_p_37","scared_p_38", "scared_p_39","scared_p_40","scared_p_41")

scaredpERP <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = scaredp_erp
)$data


scaredp_nirs <- c("subj","scared_p_date","scared_p_1","scared_p_2","scared_p_3","scared_p_4","scared_p_5","scared_p_6","scared_p_7","scared_p_8",
                 "scared_p_9","scared_p_10","scared_p_11","scared_p_12","scared_p_13","scared_p_14","scared_p_15","scared_p_16","scared_p_17",
                 "scared_p_18","scared_p_19","scared_p_20","scared_p_21","scared_p_22","scared_p_23","scared_p_24","scared_p_25","scared_p_26",
                 "scared_p_27","scared_p_28", "scared_p_29","scared_p_30","scared_p_31","scared_p_32","scared_p_33","scared_p_34","scared_p_35","scared_p_36",
                 "scared_p_37","scared_p_38", "scared_p_39","scared_p_40","scared_p_41")
scaredpNIRS <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = scaredp_nirs
)$data


scaredpERP <- scaredpERP %>%
  filter(redcap_event_name=="7year_visit_arm_1")
scaredpNIRS <- scaredpNIRS%>%
  filter(redcap_event_name=="7year_visit_arm_1")


scaredp_all <- full_join(scaredpERP,scaredpNIRS)
#exclude ineligible subjects
scaredp_all <- left_join(fullsubjlist,scaredp_all)

scaredp_full <- scaredp_all[!is.na(scaredp_all$scared_p_date), ]

#import scared session notes
session <- read.csv("/Users/rachelkwon/Desktop/scaredsession.csv")

scaredc_full <- full_join(session,scaredc_full)

###----write raw: ideally want to write to server- wrote to desktop for troubleshooting purposes (4/22/21)------

write.csv(scaredc_full,"/Users/rachelkwon/Desktop/scaredc_4.20.21.csv")

#load cleaned scaredc (cleaned - missing data troubleshooting - change later)
scaredcscore <- read.csv("/Users/rachelkwon/Desktop/scaredc_4.20.21.csv")

#run scoring function
scaredcscore <- as.data.frame(scaredcscore)
scaredc_final <- scaredC(scaredcscore)
scaredp_final <- scaredP(scaredp_full)
write.csv(scaredc_final,"/Users/rachelkwon/Desktop/scaredc_scored_4.22.21.csv")
write.csv(scaredp_final,"/Users/rachelkwon/Desktop/scaredp_scored_4.22.21.csv")
#FUNCTIONS ARE HERE 

###---scaredC scoring function -----
scaredC<- function(dataframe){
  ###SCARED C total score *sum of 1-41
  dataframe$scaredC_TS <- rowSums(dataframe[,paste("scared_c_",c(1:41),sep="")], na.rm = FALSE)
  #A total score of >= 25 may indicate likely presence of an Anxiety Disorder//scores higher than 30 are more specific
  #coding schema: 1 = potential present of anxiety disorder, 0= no likely presence
  dataframe$scaredC_anxd_yn <- ifelse(dataframe$scaredC_TS >= 25, 1, 0)
 
   ### Panic Disorder or significant somatic symptoms total
  dataframe$scaredC_panic <- rowSums(dataframe[,paste("scared_c_",c(1,6,9,12,15,18,19,22,24,27,30,34,38),sep="")], na.rm = FALSE)
  #A score of >= 7 may indicate Panic Disorder or Significant Somatic Symptoms
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredC_panic_yn <- ifelse(dataframe$scaredC_panic >= 7, 1, 0)
  
  ###Calculate Generalized Anxiety Disorder score/presence
  dataframe$scaredC_GAD <- rowSums(dataframe[,paste("scared_c_",c(5,7,14,21,23,28,33,35,37),sep="")], na.rm = FALSE)
  #A score of >= 9 may indicate Generalized Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredC_GAD_yn <- ifelse(dataframe$scaredC_GAD >= 9, 1, 0)
  
  ###Calculate total and determine presence of Separation Anxiety 
  dataframe$scaredC_SEP <- rowSums(dataframe[,paste("scared_c_",c(4,8,13,16,20,25,29,31),sep="")], na.rm = FALSE)
  
  #A score of >= 5 may indicate Separation Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredC_SEP_YN <- ifelse(dataframe$scaredC_SEP >= 5, 1, 0)
  
  ###Calculate total and determine presence of Social Anxiety Disorder 
  dataframe$scaredC_SOC <- rowSums(dataframe[,paste("scared_c_",c(3,10,26,32,39,40,41),sep="")], na.rm = FALSE)
  #A score of >= 8 may indicate Social Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredC_SOC_YN <- ifelse(dataframe$scaredC_SOC >= 8, 1, 0)
  
  ###Calculate total and determine presence of Significant School Avoidance 
  dataframe$scaredC_SCHAVD <- rowSums(dataframe[,paste("scared_c_",c(2,11,17,36),sep="")], na.rm = FALSE)
  #A score of >= 3 may indicate Significant School Avoidance
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredC_SCHAVD_yn <- ifelse(dataframe$scaredC_SCHAVD >= 3, 1, 0)
  
  ###Calculate SCARED total score mean
  dataframe$scaredC_TSmean <- rowMeans(dataframe[,paste("scared_c_",c(1:41),sep="")], na.rm = TRUE)
  
  return(dataframe)
}

###---scaredPscoring function -----
scaredP<- function(dataframe){
  ###SCARED P total score *sum of 1-41
  dataframe$scaredP_TS <- rowSums(dataframe[,paste("scared_p_",c(1:41),sep="")], na.rm = FALSE)
  #A total score of >= 25 may indicate likely presence of an Anxiety Disorder//scores higher than 30 are more specific
  #coding schema: 1 = potential present of anxiety disorder, 0= no likely presence
  dataframe$scaredP_anxd_yn <- ifelse(dataframe$scaredP_TS >= 25, 1, 0)
  
  ### Panic Disorder or significant somatic symptoms total
  dataframe$scaredP_panic <- rowSums(dataframe[,paste("scared_p_",c(1,6,9,12,15,18,19,22,24,27,30,34,38),sep="")], na.rm = FALSE)
  #A score of >= 7 may indicate Panic Disorder or Significant Somatic Symptoms
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredP_panic_yn <- ifelse(dataframe$scaredP_panic >= 7, 1, 0)
  
  ###Calculate Generalized Anxiety Disorder score/presence
  dataframe$scaredP_GAD <- rowSums(dataframe[,paste("scared_p_",c(5,7,14,21,23,28,33,35,37),sep="")], na.rm = FALSE)
  #A score of >= 9 may indicate Generalized Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredP_GAD_yn <- ifelse(dataframe$scaredP_GAD >= 9, 1, 0)
  
  ###Calculate total and determine presence of Separation Anxiety 
  dataframe$scaredP_SEP <- rowSums(dataframe[,paste("scared_p_",c(4,8,13,16,20,25,29,31),sep="")], na.rm = FALSE)
  
  #A score of >= 5 may indicate Separation Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredP_SEP_YN <- ifelse(dataframe$scaredP_SEP >= 5, 1, 0)
  
  ###Calculate total and determine presence of Social Anxiety Disorder 
  dataframe$scaredP_SOC <- rowSums(dataframe[,paste("scared_p_",c(3,10,26,32,39,40,41),sep="")], na.rm = FALSE)
  #A score of >= 8 may indicate Social Anxiety Disorder
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredP_SOC_YN <- ifelse(dataframe$scaredP_SOC >= 8, 1, 0)
  
  ###Calculate total and determine presence of Significant School Avoidance 
  dataframe$scaredP_SCHAVD <- rowSums(dataframe[,paste("scared_p_",c(2,11,17,36),sep="")], na.rm = FALSE)
  #A score of >= 3 may indicate Significant School Avoidance
  #Code: 1 = presence of disorder, 0 = no presence of disorder
  dataframe$scaredP_SCHAVD_yn <- ifelse(dataframe$scaredP_SCHAVD >= 3, 1, 0)
  
  ###Calculate SCARED total score mean
  dataframe$scaredP_TSmean <- rowMeans(dataframe[,paste("scared_p_",c(1:41),sep="")], na.rm = TRUE)
  
  return(dataframe)
}
