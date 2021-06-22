#TESI PRR cleaning- created by RK April 21 for Emotion Project
#last edited 4/28/21
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(REDCapR)
library(naniar)

###---call API token here (different for each person)------
#redcap api token call (will need to request API token from redcap admin)
source("~/Desktop/config.R")

###-----redcap api pull--------
subjlist_erp<- c("subj","eligibility", "exclusion_criteria")
eligible_erp <- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = subjlist_erp
)$data
#excluded subs nirs
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
fullsubjlist <- full_join(erplist,nirslist) %>%select(subj,eligibility)


#tesi redcap call 
tesi_field <- c("subj","todays_date_7yr","tesi_1_1","tesi_1_1_desc","tesi_1_1f","tesi_1_1l","tesi_1_1s",
"tesi_1_1a","tesi_1_2","tesi_1_2_desc","tesi_1_2f","tesi_1_2l","tesi_1_2s","tesi_1_2a",
"tesi_1_3","tesi_1_3f","tesi_1_3l","tesi_1_3s","tesi_1_3a","tesi_1_4a","tesi_1_4a_desc",
"tesi_1_4af","tesi_1_4al","tesi_1_4as","tesi_1_4aa","tesi_1_4b","tesi_1_4b_desc","tesi_1_4bf",
"tesi_1_4bl","tesi_1_4bs","texi_1_4bd","tesi_1_4ba","tesi_1_5","tesi_1_5_desc","tesi_1_5f",
"tesi_1_5l","tesi_1_5s","tesi_1_5a","tesi_1_6","tesi_1_6_desc","tesi_1_6f","tesi_1_6l","tesi_1_6s"
,"tesi_1_6a","tesi_1_7","tesi_1_7_desc","tesi_1_7f","tesi_1_7l","tesi_1_7s",
"tesi_1_7a","tesi_2_1","tesi_2_1_desc","tesi_2_1f","tesi_2_1l","tesi_2_1s","tesi_2_1a",
"tesi_2_2","tesi_2_2_desc","tesi_2_2f","tesi_2_2l","tesi_2_2s","tesi_2_2a","tesi_2_3","tesi_2_3_desc"
,"tesi_2_3f","tesi_2_3l","tesi_2_3s","tesi_2_3a","tesi_2_4","tesi_2_4_desc","tesi_2_4_desc2",
"tesi_2_4f","tesi_2_4l","tesi_2_4s","tesi_2_4a","tesi_2_5","tesi_2_5f","tesi_2_5l","tesi_2_5s",
"tesi_2_5h","tesi_2_5a","tesi_3_1","tesi_3_1desc","tesi_3_1f",
"tesi_3_1l","tesi_3_1s","tesi_3_1h","tesi_3_1a","tesi_3_2","tesi_3_2desc","tesi_3_2f","tesi_3_2l",
"tesi_3_2s","tesi_3_2h","tesi_3_2a","tesi_3_3","tesi_3_3desc","tesi_3_3f","tesi_3_3l","tesi_3_3s","tesi_3_3h","tesi_3_3a",
"tesi_4_1","tesi_4_1f","tesi_4_1l","tesi_4_1s","tesi_4_1h","tesi_4_1a","tesi_4_2","tesi_4_2f","tesi_4_2l",
"tesi_4_2s","tesi_4_2a","tesi_4_3","tesi_4_3f","tesi_4_3l","tesi_4_3s","tesi_4_3a","tesi_5_1","tesi_5_1desc",
"tesi_5_1f","tesi_5_1l","tesi_5_1s","tesi_5_1a","tesi_5_2","tesi_5_2desc","tesi_5_2desc_2","tesi_5_2f","tesi_5_2l"
,"tesi_5_2s","tesi_5_2a","tesi_6_1","tesi_6_1desc","tesi_6_1f","tesi_6_1l","tesi_6_1s","tesi_6_1a","tesi_6_2","tesi_7_1s",
"tesi_7_1a","tesi_7_1","tesi_6_2a")

tesi_erp<- redcap_read(
  redcap_uri = uri, 
  token      = tokenerp, 
  fields     = tesi_field
)$data

tesi_nirs <- redcap_read(
  redcap_uri = uri, 
  token      = tokennirs, 
  fields     = tesi_field
)$data

tesi_erp  <- tesi_erp  %>%
  filter(redcap_event_name=="7year_visit_arm_1")
tesi_nirs  <- tesi_nirs%>%
  filter(redcap_event_name=="7year_visit_arm_1")

tesi_full <- full_join(tesi_erp,tesi_nirs)
#exclude ineligible subjects
tesi_full <- left_join(fullsubjlist,tesi_full)
tesi_prr<- tesi_full[!is.na(tesi_full$todays_date_7yr), ]
#endorsement
tesi_endorsement <- tesi_prr %>% select(subj,tesi_1_1,tesi_1_2,tesi_1_3,tesi_1_4a,
                                     tesi_1_4b,tesi_1_5,tesi_1_6,tesi_1_7,tesi_2_1,tesi_2_2,tesi_2_3,tesi_2_4,
                                     tesi_2_5,tesi_3_1,tesi_3_2,tesi_3_3,tesi_4_1,tesi_4_2,tesi_4_3,tesi_5_1
                                     ,tesi_5_2,tesi_6_1,tesi_6_2,tesi_7_1)

tesiendorse_NA<- tesi_endorsement%>% replace_with_na_all(condition = ~.x == 9)
tesiendorse_NA$sumscore <- rowSums( tesiendorse_NA[,2:25],na.rm=TRUE )

#tesi affect scores: 0 - no 1/ affected my child/ 9 - DNR #confirm that there should be this many - 24 affect variables
tesi_affect <- tesi_prr %>% select(subj,tesi_1_1a,tesi_1_2a,tesi_1_3a,tesi_1_4aa,
tesi_1_4ba,tesi_1_5a,tesi_1_6a,tesi_1_7a,tesi_2_1a,tesi_2_2a,tesi_2_3a,tesi_2_4a,
tesi_2_5a,tesi_3_1a,tesi_3_2a,tesi_3_3a,tesi_4_1a,tesi_4_2a,tesi_4_3a,tesi_5_1a,
tesi_5_2a,tesi_6_1a,tesi_6_2a,tesi_7_1a)

tesi_affectNA <- tesi_affect %>% replace_with_na_all(condition = ~.x == 9)


#if statement
#tesi_affectNA %>% filter(!tesiendorse_NA$sumscore =='NA') %>% summarise(rowSums(tesi_affectNA[,2:25],na.rm = TRUE))
tesi_affectNA$affectscore <- rowSums(tesi_affectNA[,2:25],na.rm = TRUE)

#merge files - 

tesi_full <- full_join(tesiendorse_NA, tesi_affectNA, by='subj')

write.csv(tesi_full,"/Users/rachelkwon/Desktop/tesi7test.csv",na='')
