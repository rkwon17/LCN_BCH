# calculating IRR for BI - created by RK 6.22.21

#packages you will need to install/load
library(irr)
library(dplyr)
library(foreign)

bi <- read.spss("/Volumes/dmc-nelson/Groups/DMC-Emotion-Project/Groups/Data/ThreeYear/Temperament/Databases/IRR/biirr_6.22.21.sav",to.data.frame = TRUE)

#---icc----------
#stranger- duration of episode
bi_strangerdur <- bi %>% select(stranger_duration_C1,stranger_duration_C2)
icc(bi_strangerdur,model="twoway",type="agreement",unit="average")
#latency to first touch of toy
bi_stranger_lat <- bi %>% select(stranger_latencytoy_C1,stranger_latencytoy_C2)
icc(bi_stranger_lat,model="twoway",type="agreement",unit="average")
#Duration of proximity to mom
bi_strangdurmom <- bi %>% select(stranger_durationmom_C1,stranger_durationmom_C2)
icc(bi_strangdurmom,model="twoway",type="agreement",unit="average")
#robot - duration of episode
bi_robotdur <- bi %>% select(robot_duration_C1,robot_duration_C2)
icc(bi_robotdur,model="twoway",type="agreement",unit="average")
#Latency to touch robot
bi_robotlat <- bi %>% select(robot_latencytouchrobot_C1,robot_latencytouchrobot_C2)
icc(bi_robotlat,model="twoway",type="agreement",unit="average")
#Duration of proximity to mom
bi_robotmom <- bi %>% select(robot_durationmom_C1,robot_durationmom_C2)
icc(bi_robotmom,model="twoway",type="agreement",unit="average")
#Tunnel - Duration of episode
bi_tunneldur <- bi %>% select(tunnel_duration_C1,tunnel_duration_C2)
icc(bi_tunneldur,model="twoway",type="agreement",unit="average")
#Latency to enter the tunnel completely
bi_tunnellat <- bi %>% select(tunnel_latencyenter_C1,tunnel_latencyenter_C2)
icc(bi_tunnellat,model="twoway",type="agreement",unit="average")
#Duration of proximity to mom
bi_tunnelmom <- bi %>% select(tunnel_durationmom_C1,tunnel_durationmom_C2)
icc(bi_tunnelmom,model="twoway",type="agreement",unit="average")

#kappa calculations
#did the child complete the target behavior? (stranger)
bi_strangertarget <- bi %>% select(stranger_target_C1,stranger_target_C2)
kappa2(bi_strangertarget)
#did the child complete the target behavior? (robot)
bi_robottarget <- bi %>% select(robot_target_C1,robot_target_C2)
kappa2(bi_robottarget)
#did the child complete the target behavior? (tunnel)
bi_tunneltarget <- bi %>% select(tunnel_target_C1,tunnel_target_C2)
kappa2(bi_tunneltarget)
