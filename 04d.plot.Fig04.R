#--- Script to plot Fig. 04
#--- Last updated: 2023-08-09
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(RColorBrewer)
library(scales)
library(openxlsx)
library(abind)
#====================================================================================================      

#--- (1) Load data and calculate median+IQR variable pathways for each cluster and category
nc = 3 # number of output clusters
cnames = c('lo','med','hi') 
pw = c('C1','C2','C3')
itv = seq(2010,2100,by=5)
percentiles = c('p05','p25','p50','p75','p95')
sv=c("Primary Energy|Gas", "Primary Energy|Gas|w/o CCS", "Primary Energy|Gas|w/ CCS","Primary Energy", "Primary Energy|Coal","Primary Energy|Oil",
     "Primary Energy|Non-Biomass Renewables","Primary Energy|Nuclear", "Carbon Sequestration|CCS|Fossil", "Carbon Sequestration|CCS|Biomass","Carbon Sequestration|Direct Air Capture","Emissions|CO2|AFOLU",
     "Emissions|CH4|Energy", "Price|Carbon", 'Capacity Additions|Electricity|Gas|w/ CCS','Capacity Additions|Electricity|Solar','Capacity Additions|Electricity|Wind','Capacity Additions|Electricity|Storage Capacity',
     "Secondary Energy|Electricity|Gas|w/o CCS", "Secondary Energy|Electricity|Gas|w/ CCS","Final Energy|Industry|Gases","Final Energy|Transportation|Gases","Final Energy|Residential and Commercial|Gases", 'Secondary Energy|Hydrogen|Gas',
     'Capital Cost|Electricity|Gas|w/ CCS',  'Capital Cost|Electricity|Solar|CSP','Capital Cost|Electricity|Wind|Offshore',
     'OM Cost|Fixed|Electricity|Gas|w/ CCS','OM Cost|Fixed|Electricity|Solar|CSP','OM Cost|Fixed|Electricity|Wind|Offshore')

fp = array(NA, dim=c(length(itv),length(sv),length(pw),length(percentiles),nc)) # years x vars x C1-C3 scenarios x percentiles x 3 clusters
n_fp = array(0, dim=c(length(sv),length(pw),nc)) # vars x C1-C3 x cluster

for (n in 1:length(pw)){
  load(paste('./data_generated/04b.',pw[n],'_gas-clusters.Rdata',sep='')) # ms
  ms=ms[,c(1,ncol(ms))]
  colnames(ms)[ncol(ms)]='nc'
  
  #--- NOTE! Need to manually assign lo/med/high names to "nc" 1-3 based on Fig04x.AR6.2010-2100_Primary-Energy-FF_individual-scenarios_by-gas-clusters.pdf
  ms$cluster='lo'
  if(n==1){ms$cluster[ms$nc=="3"]='med'; ms$cluster[ms$nc=="1"]='hi'}
  if(n==2){ms$cluster[ms$nc=="2"]='med'; ms$cluster[ms$nc=="3"]='hi'}
  if(n==3){ms$cluster[ms$nc=="3"]='med'; ms$cluster[ms$nc=="2"]='hi'}
  
  load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data
  data = data[which(data$Category==pw[n] & data$Variable %in% sv),]
  temp = merge(ms, data, by='ms')
  ms$cluster = factor(ms$cluster, levels=c('lo','med','hi'))
  
  for(k in 1:nc){
  for (i in 1:length(sv)){

    sub = temp[which(temp$cluster==cnames[k] & temp$Variable==sv[i]),]
    ind = which(colnames(sub) %in% itv)
    sub2 = as.matrix(sub[,ind])
    if(i %in% 25:length(sv)){ # capital costs
      sub3 = sub2[which(rowSums(sub2, na.rm=T) != 0), ] # If scenarios report zeroes for all years for capital cost, set to NA
      if(is.null(nrow(sub3))==FALSE){sub3 = sub3[rowSums(is.na(sub3)) != ncol(sub3), ]} # remove rows with all NA
    } else{sub3=sub2; sub3 = sub3[rowSums(is.na(sub3)) != ncol(sub3), ] # remove rows with all NA
    }
    
    if(is.null(nrow(sub3))==TRUE & length(sub3)>1){ # only 1 model-scenario
          n_fp[i,n,k] = 1
          fp[,i,n,1,k] = sub3
          fp[,i,n,2,k] = sub3
          fp[,i,n,3,k] = sub3
          fp[,i,n,4,k] = sub3
          fp[,i,n,5,k] = sub3
     }else{
          n_fp[i,n,k] = nrow(sub3) # number of model-scenarios with data
          fp[,i,n,1,k] = apply(sub3, 2, FUN=quantile, probs=0.05, na.rm=TRUE)
          fp[,i,n,2,k] = apply(sub3, 2, FUN=quantile, probs=0.25, na.rm=TRUE)
          fp[,i,n,3,k] = apply(sub3,2, FUN=quantile, probs=0.5, na.rm=TRUE)
          fp[,i,n,4,k] = apply(sub3, 2, FUN=quantile, probs=0.75, na.rm=TRUE)
          fp[,i,n,5,k] = apply(sub3, 2, FUN=quantile, probs=0.95, na.rm=TRUE)
        }
    }
  }
}

d1=fp
d1[,c(9:13),,,]=d1[,9:13,,,]/1000 # Convert from MtCO2 to GtCO2 for FFCCS, BECCS, AFOLU

#====================================================================================================      

#--- (2) Plot Figure 04
p1=brewer.pal(9,"YlOrRd")
p2=brewer.pal(9,"YlGn")
p3=brewer.pal(9,"PuBu")
cols1=c(p3[9],p3[6],p3[4])
cols2=c(p2[9],p2[6],p2[3])
cols3=c(p1[9],p1[7],p1[4])
shading=0.3

xlim=c(2010,2100)
cex.axis=1; lwd=2

titles=c('PE Gas (EJ/yr)','PE Gas w/o CCS (EJ/yr)','PE Gas w/ CCS (EJ/yr)', 'Total PE (EJ/yr)', 'PE Coal (EJ/yr)','PE Oil (EJ/yr)',
         'PE Renewables (EJ/yr)', 'PE Nuclear (EJ/yr)', expression(paste("Fossil CCS (GtCO"[2]*"/yr)")), expression(paste("BECCS (GtCO"[2]*"/yr)")), expression(paste("DACCS (GtCO"[2]*"/yr)")), expression(paste("eAFOLU (GtCO"[2]*"/yr)")),
         expression(paste("eCH"[4]*" energy (GtCH"[4]*"/yr)")), expression(paste("Carbon Price ($2010/tCO"[2]*")")), 'CA gas-CCS (GW/yr)','CA solar (GW/yr)','CA wind (GW/yr)','CA storage (GWh/yr)',
         'Elec-gas-noCCS (EJ/yr)','Elec-gas-CCS (EJ/yr)','Industry use* (EJ/yr)', 'Transportation use* (EJ/yr)','Buildings use* (EJ/yr)', expression(paste("H"[2]*" prod. from gas (EJ/yr)")),
         "CC gas-CCS ($2010/kW)","CC solar-CSP ($2010/kW)","CC wind-off. ($2010/kW)","OMC gas-CCS ($2010/kW/yr)","OMC solar-CSP ($2010/kW/yr)","OMC wind-off. ($2010/kW/yr)")

# Note: Final energy demand here includes FF and non-FF components: 
# gases (from bioenergy, such as biogas and biomethane, or from fossil, such as natural gas), 
# liquids (from bioenergy, such as biofuels, or fossils, such as petrol and kerosene), 
# solids (from biomass or fossils such as coal) and other fuels (including solar and geothermal) across models and scenarios over time (a,c,e). 

labels = (letters[1:length(sv)])
labels[27:30]=c('ab','ac','ad','ae')

pdf(file=paste('./figures/Fig04.pdf',sep=''), width=7, height=5, paper="a4", pointsize=7, colormodel="rgb")
layout(matrix(c(1:30), 5, 6, byrow = TRUE))
par(oma=c(1,2,0.5,0.5), mar=c(2,2,2.5,1.5), mgp = c(0,1,0))

for(n in 1:length(pw)){
  if(n==1){cols=cols1; 
  ymax = c(rep(300,3),1300, 300, 300, 800, 160, rep(16,3), 7, 0.2, 3000, 220, 4000, 2000, 2000, rep(110,5), 60, rep(8000,3), rep(250,3))}
  if(n==2){cols=cols2;
  ymax = c(rep(300,3),1300, 300, 300, 800, 160, rep(20,3), 7, 0.2, 3000, 220, 4000, 2000, 2000, rep(110,5), 60, rep(8000,3), rep(250,3))}
  if(n==3){cols=cols3;
  ymax = c(rep(300,3),1300, 300, 300, 800, 160, rep(20,3), 7, 0.2, 1600, 220, 4000, 2000, 2000, rep(110,5), 60, rep(8000,3), rep(250,3))}
  ymin = rep(0, length(sv))
  ymin[12]=-7
  
for (i in 1:length(sv)){
  
  #--- Cluster 1: Lo (fast phase-down)
  plot(NULL, ylab='', xlab='', xaxt='n',yaxt='n', xlim=c(xlim[1],xlim[2]),ylim=c(ymin[i],ymax[i]),xaxs='i',yaxs='i')
  axis(1, cex.axis=cex.axis)
  axis(2, cex.axis=cex.axis, las=2)
  polygon(c(itv,rev(itv)),c(d1[,i,n,4,1],rev(d1[,i,n,2,1])),col=alpha(cols[1],shading),border=NA) # IQR range
  if(i==12){abline(h=0, col='black', lty=2)}
  
  #--- Cluster 2: Med (More gradual phase-down)
  polygon(c(itv,rev(itv)),c(d1[,i,n,4,2],rev(d1[,i,n,2,2])),col=alpha(cols[2],shading),border=NA) # IQR range
  
  #--- Cluster 3: Hi Rebound/rising
  polygon(c(itv,rev(itv)),c(d1[,i,n,4,3],rev(d1[,i,n,2,3])),col=alpha(cols[3],shading),border=NA) # IQR range
  
  #--- Overlay medians
  points(itv, d1[,i,n,3,3],type='l',col=cols[3], lwd=lwd, lty=1) 
  points(itv, d1[,i,n,3,2],type='l',col=cols[2], lwd=lwd, lty=1) 
  points(itv, d1[,i,n,3,1],type='l',col=cols[1], lwd=lwd, lty=1) 
  
  #--- Add number of model-scenarios
  if(n==1){mtext(paste("nf = ",n_fp[i,n,1],"; ns = ",n_fp[i,n,2],"; nr = ",n_fp[i,n,3],sep=''), side=3, line=-1.2, adj=0.5, cex=cex.axis*.7)}
  if(n==2){mtext(paste("nf = ",n_fp[i,n,1],"; ns = ",n_fp[i,n,2],"; nr = ",n_fp[i,n,3],sep=''), side=3, line=-1.2, adj=0.5, cex=cex.axis*.7)}
  if(n==3){mtext(paste("nd = ",n_fp[i,n,1],"; nr = ",n_fp[i,n,2],"; ni = ",n_fp[i,n,3],sep=''), side=3, line=-1.2, adj=0.5, cex=cex.axis*.7)}
  
  if(n==1 & i==3 | n==2 & i==3){legend(xlim[1], ymax[i]*.9,
                  legend=c('"rebound"','"slow decline"','"fast decline"'),
                  col=c(rev(cols), rev(alpha(cols,shading))), lty=rep(1,6),
                  lwd=c(rep(2,3),rep(8,3)), cex=cex.axis, bty='n') }
  if(n==3 & i==3){legend(xlim[1], ymax[i]*.9,
                                       legend=c('"increase"','"rebound"','"decline"'),
                                       col=c(rev(cols), rev(alpha(cols,shading))), lty=rep(1,6),
                                       lwd=c(rep(2,3),rep(8,3)), cex=cex.axis, bty='n') }
  
  mtext(labels[i], side=1, adj=.03, line=-1.2, cex=cex.axis, font=2)
  mtext(titles[i], side=3, line=0.5, cex=cex.axis*.8, font=2)
  
  } # i sv
} # n C1-C3

dev.off()

#====================================================================================================  
#==================================================================================================== 
           



