#--- Script to calculate the percentiles for Primary Energy from fossil fuels for AR6 v1.1 C1-C3 scenarios; 
#--- And extract values for the five C1-C3 Illustrative Mitigation Pathways (IMPs)
#--- Last updated: 2023-08-09
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(reshape2)
library(openxlsx)
library(matrixStats)
library(data.table)
library(abind)
library(plyr)
#====================================================================================================      
#--- Additional custom functions
`%notin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) # used to exclude a vector (opposite of %in%)
#====================================================================================================      
#====================================================================================================      

#--- (1a) Load previously compiled data and calculate percentiles at 5-year intervals
load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data
pw = sort(unique(data$Category))
itv = seq(2010,2100,by=5)
sv = c("Primary Energy|Coal","Primary Energy|Oil","Primary Energy|Gas",
       "Primary Energy|Coal|w/o CCS", "Primary Energy|Oil|w/o CCS", "Primary Energy|Gas|w/o CCS",
       "Primary Energy|Coal|w/ CCS","Primary Energy|Oil|w/ CCS", "Primary Energy|Gas|w/ CCS")
percentiles = c('p0','p5','p25','p50','p75','p95','p100')

# Create empty storage arrays
fp = array(NA, dim=c(length(itv),length(sv),length(pw),length(percentiles))) 
nfp = array(NA, dim=c(length(sv),length(pw))) # number of model-scenarios reporting given variable

for (n in 1:length(pw)){
  for (i in 1:length(sv)){
    
    sub = data[data$Category == pw[n] & data$Variable == sv[i],]
    ind = which(colnames(sub) %in% itv)
    sub2 = as.matrix(sub[,ind])
    
    sub3 = sub2[rowSums(is.na(sub2)) != ncol(sub2), ] # remove rows with all NA's
    nfp[i,n] = nrow(sub3) # number of model-scenarios with data
    
    fp[,i,n,1] = apply(sub2, 2, FUN=quantile, probs=0.00, na.rm=TRUE)
    fp[,i,n,2] = apply(sub2,2, FUN=quantile, probs=0.05, na.rm=TRUE)
    fp[,i,n,3] = apply(sub2, 2, FUN=quantile, probs=0.25, na.rm=TRUE)
    fp[,i,n,4] = apply(sub2,2, FUN=quantile, probs=0.50, na.rm=TRUE)
    fp[,i,n,5] = apply(sub2, 2, FUN=quantile, probs=0.75, na.rm=TRUE)
    fp[,i,n,6] = apply(sub2, 2, FUN=quantile, probs=0.95, na.rm=TRUE)
    fp[,i,n,7] = apply(sub2, 2, FUN=quantile, probs=1.00, na.rm=TRUE)
  }
}

data = list('years'=itv, 'vars'=sv, 'scenarios'=pw, 'percentiles'=percentiles, 'data'=fp, 'n_scenarios'=nfp)
save(data, file='./data_generated/03a.PrimaryEnergy-FF.2010-2100-5y.EJ.percentiles.Rdata')

#====================================================================================================      

#--- (2) Extract values for individual IMPs
load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data
pw = c('LD','Ren','SP','Neg','GS') # C1, C1, C1, C2, C3
itv=seq(2010,2100,by=5)
sv=c("Primary Energy|Coal","Primary Energy|Oil","Primary Energy|Gas",
     "Primary Energy|Coal|w/o CCS", "Primary Energy|Oil|w/o CCS", "Primary Energy|Gas|w/o CCS",
     "Primary Energy|Coal|w/ CCS","Primary Energy|Oil|w/ CCS", "Primary Energy|Gas|w/ CCS")
fp = array(NA, dim=c(length(itv),length(sv),length(pw))) # years x FF x scenarios

for (n in 1:length(pw)){
  for (i in 1:length(sv)){
    sub = data[which(data$IMP_marker == pw[n] & data$Variable == sv[i]),]
    if(nrow(sub)==0){
      fp[,i,n]=NA
    }else{
      ind=which(colnames(sub) %in% itv)
      fp[,i,n] = as.matrix(sub[,ind])
    }
  }
}

fp[,7:9,1]=0 # IMP-LD uses zero CCS
fp[,4:6,1]=fp[,1:3,1] # IMP-LD uses zero CCS
colnames(fp)=sv
rownames(fp)=itv
data = list('years'=itv, 'vars'=sv, 'scenarios'=pw, 'data'=fp)
save(data, file='./data_generated/03b.PrimaryEnergy-FF.2010-2100-5y.EJ.IMPs.Rdata')

#==================================================================================================== 
#==================================================================================================== 