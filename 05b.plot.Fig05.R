#--- Script to plot Fig. 05
#--- "PGR2021_data-Figures_2.1-2.2_Oct21-1.xlsx", data on government plans and projections for coal, oil, gas projections from 2021 Production Gap Report available for download at: http://productiongap.org/2021report
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

#--- (1a) Load previously calculated median pathways for all and CDR-limited C1-C3 scenarios
load(paste('./data_generated/05b.PrimaryEnergy-FF.2010-2100-5y.EJ.percentiles.CDR-lim.Rdata',sep=''))
summary(data)
print(data$sensitivity)

endyear=2100
itv=seq(2010, endyear, by=5)
years = data$years
vars = data$vars[1:3]
scenarios = data$scenarios
sens = data$sensitivity
ind = which(itv %in% years)
d1 = data$data[ind,1:3,,,]

#--- (1b) Load previously compiled IMPs
load('./data_generated/03b.PrimaryEnergy-FF.2010-2100-5y.EJ.IMPs.Rdata')
d1_imp=data$data[ind,1:3,]
imp=data$scenarios

#--- (1c) Read PGR2021 government plans and projections (GPP) pathway 2020-2040 
#--- Add 2015-2019 historical data from IEA World Energy Balances and Statistics (2022 edition); https://doi.org/10.1787/data-00510-en
#--- "PGR2021_data-Figures_2.1-2.2_Oct21-1.xlsx", data on government plans and projections for coal, oil, gas projections from 2021 Production Gap Report available for download at: http://productiongap.org/2021report
file = './data_raw/PGR2021_data-Figures_2.1-2.2_Oct21-1.xlsx' 
temp = read.xlsx(file, sheet="Figure 2.2", colNames=TRUE, cols=c(6:11), rows=c(5:41))
temp2 = temp[c(1,10,19),]
gpp = temp2[,-1]
years_gpp = as.integer(colnames(gpp))

years_hist = c(2015:2019)
hist_coal = c(162,152,157,164,167)
hist_oil = c(183,186,185,189,188)
hist_gas = c(124,127,131,138,143)
hist = rbind(hist_coal, hist_oil, hist_gas)
colnames(hist)=years_hist

hist_gpp = cbind(hist,gpp)
hist_gpp_years = c(2015:2020, seq(2025,2040,5))

#====================================================================================================  

#--- (2) PLOTTING
p1=brewer.pal(9,"Oranges")
p2=brewer.pal(9,"Blues")
p3=brewer.pal(9,"Greens")
p4 = brewer.pal(12, 'Set3')
p5=brewer.pal(9, 'Greys')
p6=brewer.pal(9, 'YlGnBu')
p7=brewer.pal(8, 'Set2')

cols=c(p2[8],p3[6],p1[6])
cols2=c(p2[5],p3[4],p1[4])
shading=1

xlim=c(2015,endyear)
ymax=rep(230,3) # by fuel
cex.axis=1; lwd=3; lwd2=2
titles=c('Coal supply (EJ/yr)','Oil supply (EJ/yr)','Gas supply (EJ/yr)')
labels = letters[1:9]

pdf(file=paste('./figures/Fig05.pdf',sep=''), width=7, height=7, paper="a4", pointsize=7, colormodel="rgb")
layout(matrix(c(1:9), 3, 3, byrow = TRUE))
par(oma=c(1,4,1,0), mar=c(3,2,3,2), mgp = c(0,1,0))

for(n in 1:length(scenarios)){
  for (i in 1:length(vars)){
    
    #--- Median + IQR (all) 
    plot(NULL, ylab='', xlab='', xaxt='n',yaxt='n', xlim=c(xlim[1],xlim[2]),ylim=c(0,ymax[i]),xaxs='i',yaxs='i')
    axis(1, cex.axis=cex.axis)
    axis(2, cex.axis=cex.axis, las=2)
    polygon(c(itv,rev(itv)),c(d1[,i,n,4,1],rev(d1[,i,n,2,1])),col=p5[2],border=NA) 
    points(itv, d1[,i,n,3,1],type='l',col=cols[n], lwd=lwd, lty=1) 
    
    #--- Constrained by Grant et al. cumulative CDR BECCS, AR, DACCS limits
    points(itv, d1[,i,n,3,2], type='l', col=cols[n], lwd=lwd2, lty=2)
    
    #--- IMPs
    if(n==1){ 
      points(itv, d1_imp[,i,1], type='l', col=p4[5], lwd=lwd2, lty=2) # LD
      points(itv, d1_imp[,i,2], type='l', col=p4[5], lwd=lwd2, lty=3) # Ren
      points(itv, d1_imp[,i,3], type='l', col=p4[5], lwd=lwd2, lty=4) # SP
      
      if(i==1){legend("topright", legend=c('C1 (n=94)', 'C1 CDR-lim. (n=4)',
                                           'IMP-LD','IMP-Ren','IMP-SP', 'IQR', 'PGR21'),
                      col=c(rep(cols[n],2), rep(p4[5],3), p5[2], 'black'), lty=c(1,2,2,3,4,1,1), lwd=c(rep(1.5,5),8,1.5), cex=cex.axis, bty='n')}
      mtext(labels[i], side=1, adj=.03, line=-1.5, cex=cex.axis, font=2)
      
    #--- Historical + PGR2021 GPP
    points(hist_gpp_years, hist_gpp[i,], type='l', col='black', lwd=lwd, lty=1)
    box(col='black')
      
    } else if (n==2){ # Neg
      points(itv, d1_imp[,i,4], type='l', col=p7[5], lwd=lwd2, lty=4) # Neg
      if(i==1){legend("topright", legend=c('C2 (n=131)', 'C2 CDR-lim. (n=0)', 'IMP-Neg', 'IQR', 'PGR21'),
                      col=c(rep(cols[n],2), p7[5], p5[2], 'black'), lty=c(1,2,4,1,1), lwd=c(1.5,1.5,1.5,8,1.5), cex=cex.axis, bty='n')}
      mtext(labels[3+i], side=1, adj=.03, line=-1.5, cex=cex.axis, font=2)
      
    } else if (n==3){ # GS
      points(itv, d1_imp[,i,5], type='l', col=p4[6], lwd=lwd2, lty=4) # GS
      
      if(i==1){legend("topright", legend=c('C3 (n=310)', 'C3 CDR-lim. (n=6)', 'IMP-GS', 'IQR', 'PGR21'),
                      col=c(rep(cols[n],2), p4[6], p5[2], 'black'), lty=c(1,2,4,1,1), lwd=c(1.5,1.5,1.5,8,1.5), cex=cex.axis, bty='n')}
      mtext(labels[6+i], side=1, adj=.03, line=-1.5, cex=cex.axis, font=2)
    }
    
    mtext(titles[i], side=3, line=0.5, cex=cex.axis)
    if(i==1){mtext(paste(scenarios[n]), side=2, cex=cex.axis, line=3.2, las=2)}
  } # end fuels
}

dev.off()

#====================================================================================================  
#====================================================================================================  

