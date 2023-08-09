#--- Script for identifying C1-C3 scenarios that do not exceed cumulative limits on AR, BECCS, and DACCS based on Grant et al. 2021 expert consensus survey
#--- Last updated: 2023-08-08
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
#====================================================================================================      

#--- (1a) Load previously compiled cumulative data and impose limits
load('./data_generated/02b.AR6-v1.1_cml-2020-2100.select-vars.Rdata') # data

#--- Note: "Carbon Sequestration|Land Use|Afforestation" is the most accurate variable to capture AR. However, reporting is limited, therefore use "Carbon Sequestration|Land Use" if the former not available.
#--- (Also perform sensitivity test for using different AR proxies)

sv=c("Carbon Sequestration|CCS|Biomass","Carbon Sequestration|Direct Air Capture",
     "Carbon Sequestration|Land Use","Carbon Sequestration|Land Use|Afforestation","Emissions|CO2|AFOLU")
ind = which(colnames(data) %in% sv)
ind2 = which(colnames(data)=="Scenario_project")
df = data[,c(1:ind2,ind)]
colnames(df)[(ind2+1):ncol(df)]=c('cml.beccs.gtco2','cml.daccs.gtco2','cml.landuse.gtco2','cml.ar.gtco2','cml.afolu.gtco2')

df[,(ind2+1):ncol(df)] = df[,(ind2+1):ncol(df)]/1000 # Convert from MtCO2 to GtCO2
df$cml.afolu.gtco2 = df$cml.afolu.gtco2*-1 # Convert sign to match CCS amounts

df$cml.ar.proxy = df$cml.ar.gtco2
ind = which(is.na(df$cml.ar.proxy)==TRUE & is.na(df$cml.landuse.gtco2)==FALSE)
df$cml.ar.proxy[ind] = df$cml.landuse.gtco2[ind]
df$below_lim = FALSE
df$below_lim[which(df$cml.beccs.gtco2 <= 196 & df$cml.ar.proxy <= 224 & df$cml.daccs.gtco2 <= 320)] = TRUE

df$bl_ar = FALSE; df$bl_ar[which(df$cml.beccs.gtco2 <= 196 & df$cml.ar.gtco2 <= 224 & df$cml.daccs.gtco2 <= 320)] = TRUE
df$bl_landuse = FALSE; df$bl_landuse[which(df$cml.beccs.gtco2 <= 196 & df$cml.landuse.gtco2 <= 224 & df$cml.daccs.gtco2 <= 320)] = TRUE
df$bl_afolu = FALSE; df$bl_afolu[which(df$cml.beccs.gtco2 <= 196 & df$cml.afolu.gtco2 <= 224 & df$cml.daccs.gtco2 <= 320)] = TRUE

save(df, file='./data_generated/05a.ms_CDR-limited.Rdata')

#====================================================================================================      

#--- (1b) Check 2040-2060 total CCS
sub.ms = unique(df$ms[which(df$below_lim==TRUE)],)

load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data
sv=sort(c("Carbon Sequestration|CCS","Carbon Sequestration|CCS|Fossil"))
temp = data[which(data$ms %in% sub.ms & data$Variable %in% sv),]     
df=temp
ic1 = which(colnames(df) %in% seq(2040,2060,5))
ic2 = which(colnames(df) %in% seq(2050,2100,5))
df$mca = rowMeans(df[,ic1], na.rm=TRUE) 
df$lta = rowMeans(df[,ic2], na.rm=TRUE) 
df2 = df[,c(1,10,(ncol(df)-1),ncol(df))]

df3 = dcast(df2, ms ~ Variable, value.var="mca")
which(df3$`Carbon Sequestration|CCS`>8600) # none

#====================================================================================================      

#--- (2) Calculate percentiles for plotting Fig. 05
load('./data_generated/05a.ms_CDR-limited.Rdata') # df
load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data

icols = which(colnames(df) %in% c('ms','below_lim','bl_ar','bl_landuse','bl_afolu'))
data = merge(df[,icols], data, by='ms')

pw = sort(unique(data$Category))
sens = c('all', 'below_lim','bl_ar','bl_landuse','bl_afolu')
percentiles = c('p0','p25','p50','p75','p100')
itv = seq(2010,2100,by=5)
sv = c("Primary Energy|Coal","Primary Energy|Oil","Primary Energy|Gas",
     "Primary Energy|Coal|w/o CCS", "Primary Energy|Oil|w/o CCS", "Primary Energy|Gas|w/o CCS",
     "Primary Energy|Coal|w/ CCS", "Primary Energy|Oil|w/ CCS", "Primary Energy|Gas|w/ CCS")

fp = array(NA, dim=c(length(itv),length(sv),length(pw),5,length(sens))) # years x FF x C1-C3 scenarios x values (min, median, max) x sensitivity
nfp = array(NA, dim=c(length(sv),length(pw), length(sens))) # number of model-scenarios
rownames(nfp) = sv; colnames(nfp) = pw

for (i in 1:length(sv)){
  for (n in 1:length(pw)){
    for(j in 1:length(sens)){
      
      if(j==1){sub = data[which(data$Category == pw[n] & data$Variable == sv[i]),]} # all
      if(j==2){sub = data[which(data$Category == pw[n] & data$Variable == sv[i] & data$below_lim==TRUE),]} # below Grant limits
      if(j==3){sub = data[which(data$Category == pw[n] & data$Variable == sv[i] & data$bl_ar==TRUE),]} 
      if(j==4){sub = data[which(data$Category == pw[n] & data$Variable == sv[i] & data$bl_landuse==TRUE),]} 
      if(j==5){sub = data[which(data$Category == pw[n] & data$Variable == sv[i] & data$bl_afolu==TRUE),]} 
      
      if(i==1 & j==1 | i==1 & j==2){print(dim(sub))}
      
      ind=which(colnames(sub) %in% itv)
      sub2 = as.matrix(sub[,ind])
      
      sub3 = sub2[rowSums(is.na(sub2)) != ncol(sub2), ] # remove rows with all NA's
      
      # Count number of model-scenarios with data
      if(is.null(nrow(sub3))==TRUE & length(sub3)==length(itv)){
        nfp[i,n,j] = 1
        }else{nfp[i,n,j] = nrow(sub3) } # number of model-scenarios with data
      
      fp[,i,n,1,j] = apply(sub2, 2, FUN=quantile, probs=0, na.rm=TRUE)
      fp[,i,n,2,j] = apply(sub2, 2, FUN=quantile, probs=0.25, na.rm=TRUE)
      fp[,i,n,3,j] = apply(sub2, 2, FUN=quantile, probs=0.50, na.rm=TRUE)
      fp[,i,n,4,j] = apply(sub2, 2, FUN=quantile, probs=0.75, na.rm=TRUE)
      fp[,i,n,5,j] = apply(sub2, 2, FUN=quantile, probs=1, na.rm=TRUE)
    }
  }
}

d1 = fp
endyear = 2100
scenarios = pw
vars=sv
# # 4 C1; 0 C2; 6 C3 are below Grant et al. cumulative CDR limits

#--- Save data for plotting Fig. 05
data = list('years'=itv, 'vars'=sv, 'scenarios'=pw, 'percentiles'=percentiles, 
            'sensitivity'=sens[c(1,2)], 'data'=fp[,,,,c(1,2)], 'n_scenarios'=nfp[,,c(1,2)])
save(data, file=paste('./data_generated/05b.PrimaryEnergy-FF.2010-2100-5y.EJ.percentiles.CDR-lim.Rdata',sep=''))

#==================================================================================================== 
#==================================================================================================== 
