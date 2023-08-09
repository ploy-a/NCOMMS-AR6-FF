#--- Script to plot Fig. 06
#--- Using the data file "data_feasibility_indicators_18_06_2022.csv" available in this github repository (https://github.com/ploy-a/NCOMMS-AR6-FF)
#--- Last updated: 2023-08-08
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(RColorBrewer)
library(scales)
library(openxlsx)
library(abind)
#====================================================================================================      

#--- (1) Read feasibility time series data
temp = read.csv('./data_raw/data_feasibility_indicators_18_06_2022.csv', header=TRUE)
temp = temp[which(temp$Category %in% c("C1","C2",'C3') & temp$Year >= 2020),] # select 2020-2100
years = seq(2020, 2100, 10); length(years)
temp$ms = paste(temp$Model, '--', temp$Scenario, sep='')

key.vars = c('concern_g_biomass','concern_tech','concern_economic','concern_social','concern_governance')
icols = which(colnames(temp) %in% c("ms", "Category", "IMP_marker", "Year", key.vars))
data = temp[,icols]

#====================================================================================================  

#--- (2) Calculate percentiles for C1-C3 (all and CDR-limited)
load('./data_generated/05a.ms_CDR-limited.Rdata') # df
ms = df[which(df$Model_family != "C-ROADS"),] # C-ROADS does not output Primary Energy-FF
ind = which(colnames(ms) %in% c('ms','below_lim'))
data = merge(ms[,ind], data, by='ms')

pw = sort(unique(data$Category))
itv = seq(2020,2100,by=10)
sv = key.vars
percentiles = c('p0','p25','p50','p75','p100')
sens = c('all','below_lim')

fp = array(NA, dim=c(length(itv),length(sv),length(pw),length(percentiles),length(sens))) # years x FF x scenarios x percentiles x sensitivity
nfp = array(NA, dim=c(length(sv),length(pw), length(sens))) # number of model-scenarios

for (n in 1:length(pw)){
  for (i in 1:length(sv)){
    for(j in 1:length(sens)){
    icols = which(colnames(data) %in% c('ms','Year',key.vars[i]))
    
    if(j==1){sub = data[data$Category == pw[n], icols]}
    if(j==2){sub = data[data$Category == pw[n] & data$below_lim==TRUE, icols]}
    colnames(sub)[3] = 'var'
    
    if((nrow(sub))==0){
      fp[,i,n,,j] = NA
      nfp[i,n,j] = 0 # number of model-scenarios with data
    }else{
      fp[,i,n,1,j] = aggregate(var ~ Year, data = sub, FUN = quantile, probs=0, na.rm=TRUE)$var
      fp[,i,n,2,j] = aggregate(var ~ Year, data = sub, FUN = quantile, probs=0.25, na.rm=TRUE)$var
      fp[,i,n,3,j] = aggregate(var ~ Year, data = sub, FUN = quantile, probs=0.50, na.rm=TRUE)$var
      fp[,i,n,4,j] = aggregate(var ~ Year, data = sub, FUN = quantile, probs=0.75, na.rm=TRUE)$var
      fp[,i,n,5,j] = aggregate(var ~ Year, data = sub, FUN = quantile, probs=1, na.rm=TRUE)$var
      nfp[i,n,j] = length(unique(sub$ms)) # number of model-scenarios with data
    }

    }
  }
}

d1=fp

#====================================================================================================  

#--- (3) Extract data for individual IMPs

pw = c('LD','Ren','SP','Neg','GS') # C1, C1, C1, C2, C3
imp_ms = c('MESSAGEix-GLOBIOM 1.0--LowEnergyDemand_1.3_IPCC',
           'REMIND-MAgPIE 2.1-4.3--DeepElec_SSP2_ HighRE_Budg900',
           'REMIND-MAgPIE 2.1-4.2--SusDev_SDP-PkBudg1000',
           'COFFEE 1.1--EN_NPi2020_400f_lowBECCS',
           'WITCH 5.0--CO_Bridge')
ind = which(data$ms %in% imp_ms)
data = data[ind,]
fp = array(NA, dim=c(length(itv),length(sv),length(pw))) # years x FF x scenarios

for (n in 1:length(pw)){
  for (i in 1:length(sv)){
    icols = which(colnames(data) %in% c('ms','Year',key.vars[i]))
    sub = data[data$ms == imp_ms[n], icols]
    colnames(sub)[3] = 'var'
    test = is.na(sub$var)
    if(length(which(test==TRUE))==9){
      fp[,i,n]  = NA
    }else{
      fp[,i,n] = aggregate(var ~ Year, data = sub, FUN = median, na.rm=TRUE)$var
    }
  }
}

d1_imp = fp

#====================================================================================================  

#--- (4) PLOTTING
p1=brewer.pal(9,"Oranges")
p2=brewer.pal(9,"Blues")
p3=brewer.pal(9,"Greens")
p4 = brewer.pal(12, 'Set3')
p5=brewer.pal(9, 'Greys')
p6=brewer.pal(9, 'YlGnBu')
p7=brewer.pal(8, 'Set2')
p8=brewer.pal(9, "Reds")
p9=brewer.pal(12, "Paired")
cols=c(p2[8],p3[6],p1[6])
cols2=c(p2[5],p3[4],p1[4])
shading=1
labels = matrix(letters[1:15], nrow=3, byrow=TRUE)

xlim=c(2020,2100)
ymin=1
ymax=rep(3,length(key.vars))
cex.axis=1; lwd=2; lwd2=1.5
titles=c('Geo/Biomass','Technological','Economic','Socio-cultural','Institutional')
scenarios = c("C1","C2","C3")

pdf(file=paste('./figures/Fig06.pdf',sep=''), width=7, height=4.8, paper="a4", pointsize=7, colormodel="rgb")
layout(matrix(c(1:15), 3, 5, byrow = TRUE))
par(oma=c(0,4,1,6), mar=c(2,2,2,1.5), mgp = c(0,1,0))

for(n in 1:length(scenarios)){
  for (i in 1:length(key.vars)){
    
    # Plot Median of all + range; And median of CDR-lim. subset  
    plot(NULL, ylab='', xlab='', xaxt='n',yaxt='n', xlim=c(xlim[1],xlim[2]),ylim=c(ymin,ymax[i]),xaxs='i')
    axis(1, cex.axis=cex.axis)
    axis(2, cex.axis=cex.axis, las=2)
    polygon(c(itv,rev(itv)),c(d1[,i,n,5,1],rev(d1[,i,n,1,1])),col=p5[2], border=NA) 
    points(itv, d1[,i,n,3,1],type='l',col=cols[n], lwd=lwd, lty=1) # all
    points(itv, d1[,i,n,3,2],type='l',col=cols[n], lwd=lwd, lty=2) # CDR-lim
    
    abline(h=1.5, col=p9[5], lty=2, lwd=.7)
    abline(h=2.5, col=p9[6], lty=2, lwd=.7)
    
    # Add IMPs
    if(n==1){ 
      points(itv, d1_imp[,i,1], type='l', col=p4[5], lwd=lwd2, lty=2) # LD
      points(itv, d1_imp[,i,2], type='l', col=p4[5], lwd=lwd2, lty=3) # Ren
      points(itv, d1_imp[,i,3], type='l', col=p4[5], lwd=lwd2, lty=4) # SP
      
      if(i==4){legend("topright", legend=c('All (n=82)', 'CDR-lim (n=3)', 
                                           'IMP-LD','IMP-Ren','IMP-SP', 'Full range'),
                      col=c(rep(cols[n],2), rep(p4[5],3), p5[2]), lty=c(1,2,2,3,4,1), lwd=c(rep(1.5,5),8), cex=cex.axis, bty='n')}
      
    } else if (n==2){ # Neg
      points(itv, d1_imp[,i,4], type='l', col=p7[5], lwd=lwd2, lty=4) # Neg
      if(i==4){legend("topright", legend=c('All (n=119)', 'CDR-lim (n=0)', 'IMP-Neg','Full range'),
                      col=c(rep(cols[n],2), p7[5], p5[2]), lty=c(1,2,4,1), lwd=c(rep(1.5,3),8), cex=cex.axis, bty='n')}
      
    } else if (n==3){ # GS
      points(itv, d1_imp[,i,5], type='l', col=p4[6], lwd=lwd2, lty=4) # GS
      
      if(i==4){legend("topright", legend=c('All (n=299)', 'CDR-lim (n=6)', 'IMP-GS','Full range'),
                      col=c(rep(cols[n],2), p4[6], p5[2]), lty=c(1,2,4,1), lwd=c(rep(1.5,3),8), cex=cex.axis, bty='n')}
    }
    
    box(col='black')
    
    mtext(labels[n,i], side=3, adj=.03, line=-1.5, cex=cex.axis, font=2)
    if(n==1){mtext(titles[i], side=3, line=0.5, cex=cex.axis, font=2)}
    if(i==1){mtext(paste(scenarios[n]), side=2, cex=cex.axis, line=3.2, las=2, font=2)}
  } # end fuels
}

dev.off()

#====================================================================================================  
#==================================================================================================== 
           
