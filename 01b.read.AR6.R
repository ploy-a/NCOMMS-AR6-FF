#--- Script to read and process (interpolate and calculate cumulative values) raw data from the AR6 scenario database.
#--- IPCC AR6 v1.1 raw data "AR6_Scenarios_Database_World_v1.1.csv" downloaded on 16 Feb 2023 from https://data.ene.iiasa.ac.at/ar6/#/downloads
#--- Last updated: 2023-08-08
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(reshape2)
library(openxlsx)
library(writexl)
library(plyr)
library(abind)
library(zoo)
#====================================================================================================      
#====================================================================================================      

#--- (1a) Load previously compiled metadata
load('./data_generated/01a.AR6-v1.1_C1-C3_metadata.Rdata') # ms
ind = which(colnames(ms) %in% c('ms','Category','Model_family','Project_study','Scenario_project','IMP_marker'))
ms2 = ms[,ind]

#--- (1b) Read and extract data for C1-C3 from raw data file to avoid having to read large CSV file multiple times
temp = read.csv('./data_raw/AR6_Scenarios_Database_World_v1.1.csv', header=TRUE) 
dim(temp)
colnames(temp)
unique(temp$Region) # check "World" only
vars=unique(temp$Variable) # 1,442
temp$ms = paste(temp$Model, temp$Scenario, sep='--')

data=temp[which(temp$ms %in% ms$ms),]
data = merge(ms2, data, by='ms')
ind = which(colnames(data)=="X1995")
colnames(data)[ind:ncol(data)]=c(1995:2100)
save(data, file='./data_generated/01b.AR6-v1.1_C1-C3_allvars.Rdata')

#====================================================================================================      

#--- (2a) Extract variables of interest
load('./data_generated/01b.AR6-v1.1_C1-C3_allvars.Rdata') # data
pw=c('C1','C2','C3')
sv=sort(c("Primary Energy", "Final Energy", "Primary Energy|Fossil",
          "Primary Energy|Coal", "Primary Energy|Oil", "Primary Energy|Gas",
     "Primary Energy|Coal|w/ CCS", "Primary Energy|Coal|w/o CCS",
     "Primary Energy|Oil|w/o CCS", "Primary Energy|Oil|w/ CCS",
     "Primary Energy|Gas|w/ CCS", "Primary Energy|Gas|w/o CCS",
     "Primary Energy|Fossil|w/ CCS", "Primary Energy|Fossil|w/o CCS",
     "Primary Energy|Biomass", "Primary Energy|Non-Biomass Renewables", "Primary Energy|Nuclear",
     "Carbon Sequestration|CCS","Carbon Sequestration|CCS|Fossil", "Carbon Sequestration|CCS|Biomass", 
     "Carbon Sequestration|Land Use", "Carbon Sequestration|Land Use|Afforestation", "Carbon Sequestration|Direct Air Capture","Carbon Sequestration|Other",
     "Emissions|CO2", "Emissions|CO2|Energy", "Emissions|CO2|AFOLU",
     "Emissions|CH4", "Emissions|CH4|Energy",
     "Secondary Energy|Electricity|Coal","Secondary Energy|Electricity|Gas","Secondary Energy|Electricity|Oil",
     "Secondary Energy|Electricity|Coal|w/ CCS","Secondary Energy|Electricity|Coal|w/o CCS",
     "Secondary Energy|Electricity|Gas|w/ CCS","Secondary Energy|Electricity|Gas|w/o CCS",
     "Secondary Energy|Electricity|Oil|w/ CCS","Secondary Energy|Electricity|Oil|w/o CCS",
     "Secondary Energy|Electricity|Biomass", "Secondary Energy|Electricity|Biomass|w/ CCS", "Secondary Energy|Electricity|Non-Biomass Renewables",
     "Secondary Energy|Electricity|Hydro","Secondary Energy|Electricity|Solar","Secondary Energy|Electricity|Wind","Secondary Energy|Electricity|Nuclear",
     "Final Energy|Industry|Gases","Final Energy|Transportation|Gases","Final Energy|Residential and Commercial|Gases","Final Energy|Non-Energy Use|Gas",
     "Final Energy|Industry|Liquids","Final Energy|Transportation|Liquids","Final Energy|Residential and Commercial|Liquids","Final Energy|Non-Energy Use|Oil",
     "Final Energy|Industry|Solids","Final Energy|Residential and Commercial|Solids","Final Energy|Non-Energy Use|Coal",
     "Final Energy (excl. feedstocks)|Industry|Gases","Final Energy (excl. feedstocks)|Industry|Liquids","Final Energy (excl. feedstocks)|Industry|Solids",
     "Secondary Energy|Hydrogen|Fossil","Secondary Energy|Hydrogen|Coal","Secondary Energy|Hydrogen|Gas",
     "Price|Carbon",'GDP|PPP','Consumption',
     'Capacity Additions|Electricity|Gas|w/ CCS','Capacity Additions|Electricity|Gas|w/o CCS',
     'Capacity Additions|Electricity|Coal|w/ CCS','Capacity Additions|Electricity|Coal|w/o CCS',
     'Capacity Additions|Electricity|Solar','Capacity Additions|Electricity|Wind','Capacity Additions|Electricity|Storage Capacity',
     'Capital Cost|Electricity|Coal|w/ CCS','Capital Cost|Electricity|Coal|w/o CCS',
     'Capital Cost|Electricity|Gas|w/ CCS','Capital Cost|Electricity|Gas|w/o CCS',
     'Capital Cost|Electricity|Solar|PV','Capital Cost|Electricity|Solar|CSP',
     'Capital Cost|Electricity|Wind|Offshore', 'Capital Cost|Electricity|Wind|Onshore',
     "Capacity|Electricity","Capacity|Electricity|Gas|w/ CCS","Capacity|Electricity|Gas|w/o CCS","Capacity|Electricity|Solar","Capacity|Electricity|Wind","Capacity|Electricity|Storage",
     "AR6 climate diagnostics|Infilled|Emissions|Kyoto Gases (AR6-GWP100)",
     'OM Cost|Fixed|Electricity|Coal|w/ CCS','OM Cost|Fixed|Electricity|Coal|w/o CCS',
     'OM Cost|Fixed|Electricity|Gas|w/ CCS','OM Cost|Fixed|Electricity|Gas|w/o CCS',
     'OM Cost|Fixed|Electricity|Solar|CSP','OM Cost|Fixed|Electricity|Solar|PV',
     'OM Cost|Fixed|Electricity|Wind','OM Cost|Fixed|Electricity|Wind|Offshore','OM Cost|Fixed|Electricity|Wind|Onshore',
     'OM Cost|Electricity|Solar|PV|Rooftop PV', 'OM Cost |Electricity|Solar|PV|Rooftop PV', 'OM Cost|Electricity|Solar|PV|Utility-scale PV', 'OM Cost|Electricity|Storage|Battery Capacity'))

data=data[which(data$Variable %in% sv),]
years=c(1995:2100)
itv=seq(2010,2100,by=5)
ind=which(colnames(data) %in% itv)
ind2=which(colnames(data)=="Unit")
data=data[,c(1:ind2,ind)]

#--- (2b) Linearly interpolate between 2010-2100 to create a dataset at 5-year intervals for all model-scenarios
icol = which(colnames(data)=="2010")
temp = data[,icol:ncol(data)]
temp2 = matrix(NA, nrow=nrow(temp), ncol=ncol(temp))

for(i in 1:nrow(temp)){
  v = as.numeric(temp[i,])
  temp2[i,]=na.approx(v, na.rm=FALSE)
}

colnames(temp2)=seq(2010,2100,5)
count_na_func <- function(x) sum(is.na(x))
apply(data[,12:ncol(data)], 2, count_na_func) 
apply(temp2, 2, count_na_func) 

data = cbind(data[,1:(icol-1)], temp2)

#--- Note: There are duplicate capacity additions variables with different units -- select the rate
ind = which(data$Variable %in% sv[2:8] & data$Unit=="GW")
data=data[-ind,]
save(data, file='./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata')
#==================================================================================================== 

#--- (3) Calculate 2020-2100 cumulative values
library(rioja) # linear interpolation

pw = sort(unique(data$Category))
sv = sort(unique(data$Variable))
itv=seq(2020,2100,by=5)
icols = which(colnames(data) %in% itv)
ind2=which(colnames(data)=="Unit")
df = data[which(data$Variable %in% sv),c(1:ind2,icols)]
  d1 = data[which(data$Variable == "Emissions|CO2"),]; dim(d1) # 541

res=list()  
  
for (i in 1:length(sv)){
  print(sv[i])
  icol=which(colnames(df)=="2020")
  temp2 = df[which(df$Variable==sv[i]),icol:ncol(df)]
  sub_ms = df[which(df$Variable==sv[i]),1]
  temp3 = t(temp2) # years x ms
  df2 = interp.dataset(y=temp3, x=seq(2020, 2100, by=5),xout=c(2020:2100), method="linear", rep.negt=FALSE)
  cml = colSums(df2, na.rm=TRUE)
  res[[i]] = cbind(sub_ms, cml)
  colnames(res[[i]])=c('ms',sv[i])
}

df2 = Reduce(function(x, y) merge(x, y, all=TRUE), res)

#--- Combine with metadata
data = merge(ms, df2, by='ms')
ind2 = which(colnames(data)=="Scenario_project")
data[,c((ind2+1):ncol(data))] = sapply(data[,c((ind2+1):ncol(data))], as.numeric)
save(data, file='./data_generated/02b.AR6-v1.1_cml-2020-2100.select-vars.Rdata')

#==================================================================================================== 
#==================================================================================================== 
