#--- Script to read and compile raw metadata from the AR6 scenario database, and assign "model family" and "project study".
#--- IPCC AR6 v1.1 metadata "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx" downloaded on 16 Feb 2023 from https://data.ene.iiasa.ac.at/ar6/#/downloads
#--- Last updated: 2023-08-08
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(reshape2)
library(openxlsx)
library(writexl)
library(dplyr)
library(abind)
#====================================================================================================      
#====================================================================================================      

#--- (1a) Read raw metadata file
temp1=openxlsx::read.xlsx('./data_raw/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx',sheet=2,colNames=TRUE)

#--- (1b) Re-assign IMP-Neg scenario to be C2
nrow = which(temp1$IMP_marker=="Neg")
ncol = which(colnames(temp1)=="Category")
temp1[nrow, ncol] = "C2"
temp1[nrow, ncol+1] = "C2: return warming to 1.5°C (>50%) after a high overshoot"

#---(1c) Subset C1-C3 scenarios
ms = temp1[which(temp1$Category %in% c("C1","C2",'C3')), c(1:4,8,12)]; dim(ms)
ms$ms = paste(ms$Model, ms$Scenario, sep='--') # create unique model--scenario identifier for each scenario
ms %>% dplyr::count(Category)
# 541 model-scenarios and 38 models: 97 C1; 134 C2; 310 C3

#====================================================================================================      

#--- (2a) Add new "Model_family" categorization
ms$Model_family = "Others"
ms[which(ms$Model %in% c("AIM/CGE 2.0","AIM/CGE 2.1","AIM/CGE 2.2", "AIM/Hub-Global 2.0")), ncol(ms)] = "AIM"
ms[which(ms$Model %in% c("C-ROADS-5.005")), ncol(ms)] = "C-ROADS"
ms[which(ms$Model %in% c("COFFEE 1.1")), ncol(ms)] = "COFFEE"
ms[which(ms$Model %in% c("EPPA 6")), ncol(ms)] = "EPPA"
ms[which(ms$Model %in% c("GCAM 4.2","GCAM 5.3")), ncol(ms)] = "GCAM"
ms[which(ms$Model %in% c("GEM-E3_V2021")), ncol(ms)] = "GEM-E3"
ms[which(ms$Model %in% c("IMAGE 3.0","IMAGE 3.0.1","IMAGE 3.0.2","IMAGE 3.2")), ncol(ms)] = "IMAGE"
ms[which(ms$Model %in% c("MESSAGE-GLOBIOM 1.0","MESSAGEix-GLOBIOM 1.0","MESSAGEix-GLOBIOM_1.1",
                         "MESSAGEix-GLOBIOM_1.2","MESSAGEix-GLOBIOM_GEI 1.0")), ncol(ms)] = "MESSAGE"
ms[which(ms$Model %in% c("POLES ADVANCE","POLES EMF30","POLES EMF33","POLES ENGAGE","POLES GECO2019")), ncol(ms)] = "POLES"
ms[which(ms$Model %in% c("REMIND 1.6","REMIND 1.7","REMIND 2.1","REMIND-Buildings 2.0","REMIND-MAgPIE 1.5","REMIND-MAgPIE 1.7-3.0",
                         "REMIND-MAgPIE 2.0-4.1","REMIND-MAgPIE 2.1-4.2","REMIND-MAgPIE 2.1-4.3","REMIND-Transport 2.1")), ncol(ms)] = "REMIND"
ms[which(ms$Model %in% c("TIAM-ECN 1.1")), ncol(ms)] = "TIAM"
ms[which(ms$Model %in% c("WITCH 5.0","WITCH-GLOBIOM 3.1","WITCH-GLOBIOM 4.4")), ncol(ms)] = "WITCH"
#which(ms$Model_family=="Others")
length(unique(ms$Model_family)) # 12 model families

ms %>% dplyr::count(Category,Model_family)
ms %>% dplyr::count(Category, Project_study)

#---(2b) Assign additional "Scenario_project" categorization (with "Others" representing aggregate of projects with a small number of scenarios)
projects = sort(unique(ms$Project_study))
ms$Scenario_project = "Others"

ms[which(ms$Category=="C1" & ms$Project_study==projects[5]), ncol(ms)]=projects[5]
ms[which(ms$Category=="C1" & ms$Project_study==projects[8]), ncol(ms)]=projects[8]
ms[which(ms$Category=="C1" & ms$Project_study==projects[9]), ncol(ms)]=projects[9]
ms[which(ms$Category=="C1" & ms$Project_study==projects[15]), ncol(ms)]=projects[15]
ms[which(ms$Category=="C1" & ms$Project_study==projects[19]), ncol(ms)]=projects[19]
ms[which(ms$Category=="C1" & ms$Project_study==projects[20]), ncol(ms)]=projects[20]
ms[which(ms$Category=="C1" & ms$Project_study==projects[24]), ncol(ms)]=projects[24]
ms[which(ms$Category=="C1" & ms$Project_study==projects[28]), ncol(ms)]=projects[28]
ms[which(ms$Category=="C1" & ms$Project_study %in% projects[25:27]), ncol(ms)]="Strefler"

ms[which(ms$Category=="C2" & ms$Project_study==projects[1]), ncol(ms)]=projects[1]
ms[which(ms$Category=="C2" & ms$Project_study==projects[5]), ncol(ms)]=projects[5]
ms[which(ms$Category=="C2" & ms$Project_study==projects[6]), ncol(ms)]=projects[6]
ms[which(ms$Category=="C2" & ms$Project_study==projects[8]), ncol(ms)]=projects[8]
ms[which(ms$Category=="C2" & ms$Project_study==projects[9]), ncol(ms)]=projects[9]
ms[which(ms$Category=="C2" & ms$Project_study==projects[13]), ncol(ms)]=projects[13]
ms[which(ms$Category=="C2" & ms$Project_study==projects[16]), ncol(ms)]=projects[16]
ms[which(ms$Category=="C2" & ms$Project_study==projects[28]), ncol(ms)]=projects[28]

ms[which(ms$Category=="C3" & ms$Project_study==projects[1]), ncol(ms)]=projects[1]
ms[which(ms$Category=="C3" & ms$Project_study==projects[6]), ncol(ms)]=projects[6]
ms[which(ms$Category=="C3" & ms$Project_study==projects[7]), ncol(ms)]=projects[7]
ms[which(ms$Category=="C3" & ms$Project_study==projects[8]), ncol(ms)]=projects[8]
ms[which(ms$Category=="C3" & ms$Project_study==projects[9]), ncol(ms)]=projects[9]
ms[which(ms$Category=="C3" & ms$Project_study==projects[11]), ncol(ms)]=projects[11]
ms[which(ms$Category=="C3" & ms$Project_study==projects[19]), ncol(ms)]=projects[19]
ms[which(ms$Category=="C3" & ms$Project_study==projects[20]), ncol(ms)]=projects[20]
ms[which(ms$Category=="C3" & ms$Project_study==projects[22]), ncol(ms)]=projects[22]
ms[which(ms$Category=="C3" & ms$Project_study==projects[24]), ncol(ms)]=projects[24]
ms[which(ms$Category=="C3" & ms$Project_study==projects[28]), ncol(ms)]=projects[28]

save(ms, file=paste(wdir,'data_generated/01a.AR6-v1.1_C1-C3_metadata.Rdata',sep=''))

#====================================================================================================  