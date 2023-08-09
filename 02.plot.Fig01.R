#--- Script to plot Figure 1 in manuscript
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

#--- (1) Load previously compiled data for individual pathways and percentiles
load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata')
sv=c("Primary Energy|Coal","Primary Energy|Oil","Primary Energy|Gas")
ind = which(data$Variable %in% sv)
df.inv = data[ind,]

load('./data_generated/02b.AR6-v1.1_cml-2020-2100.select-vars.Rdata')
ind2 = which(colnames(data) %in% c("ms", "Category", sv))
df.cml = data[,ind2]
df.cml=df.cml[,c(1,2,3,5,4)]
df.cml[,3:5] = df.cml[,3:5]/1000 # scale

load('./data_generated/03a.PrimaryEnergy-FF.2010-2100-5y.EJ.percentiles.Rdata')
# data = list('years'=itv, 'vars'=sv, 'scenarios'=pw, 'percentiles'=c('p0','p5','p25','p50','p75','p95','p100'), 'data'=fp, 'n_scenarios'=nfp)
summary(data)
d1 = data$data
years = data$years; itv=years
vars = data$vars
pw = data$scenarios
percentiles = data$percentiles
nfp = data$n_scenarios

#====================================================================================================  

#--- (2) PLOTTING

#---(2a) Define plot parameters
p1 = brewer.pal(9,"Oranges")
p2 = brewer.pal(9,"Blues")
p3 = brewer.pal(9,"Greens")
p4 = brewer.pal(9, "Greys")
cols=c(p2[8],p3[6],p1[6])
cols2=c(p2[4],p3[3],p1[3])
cols3=rep(p4[4],3)
shading=0.3

xlim=c(2010,2100)
ymax=c(rep(350,3), rep(350,3), rep(350,3)) 
cex.axis=1; lwd=2.5
titles=c('Coal supply (EJ/yr)','Oil supply (EJ/yr)','Gas supply (EJ/yr)',
         'Coal without CCS (EJ/yr)','Oil without CCS (EJ/yr)','Gas without CCS (EJ/yr)',
         'Coal with CCS (EJ/yr)','Oil with CCS (EJ/yr)','Gas with CCS (EJ/yr)')

labels = matrix(letters[1:9], nrow=3, byrow=FALSE)
labels2 = c('j','k','l')
vargroups = c('total','wo-CCS','with-CCS')

#--- (2b) Save plot to PDFs

#for(k in 1:3){
k=1
  if(k==1){d2 = d1[,1:3,,]; vars=vars[1:3]; titles=titles[1:3]}
  if(k==2){d2 = d1[,4:6,,]; vars=vars[4:6]; titles=titles[4:6]}
  if(k==3){d2 = d1[,7:9,,]; vars=vars[7:9]; titles=titles[7:9]} 

  pdf(file=paste('./figures/Fig01.pdf',sep=''), width=7, height=5, paper="a4", pointsize=7, colormodel="rgb")
  layout(matrix(c(1:12), 3, 4, byrow = TRUE), widths=c(0.28,0.28,0.28,0.16))
  par(oma=c(1,4.5,1.5,0), mar=c(2,2,2,2), mgp = c(0,1,0))
  
  for (n in 1:length(pw)){
  for (i in 1:length(vars)){

    #--- (1) Plot individual pathways
    sub2 = df.inv[which(df.inv$Category==pw[n] & df.inv$Variable==vars[i]),]
    ncount = nrow(sub2) # number of individual scenarios in that category
      
      plot(NULL, ylab='', xlab='', xaxt='n',yaxt='n', xlim=c(xlim[1],xlim[2]),ylim=c(0,ymax[i]),xaxs='i',yaxs='i')
      axis(1, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis, las=2)
      axis(1, at=seq(2010,2090,20), labels=FALSE)
      
      for (j in 1:ncount){
        icol = which(colnames(sub2)==xlim[1])
        points(itv, sub2[j,icol:ncol(sub2)],type='l',col=cols2[n], lwd=.75, lty=1) 
      }
      
  #--- (2) Add IQR shading
  polygon(c(itv,rev(itv)),c(d2[,i,n,5],rev(d2[,i,n,3])),col=alpha(cols[n],shading),border=NA) 
  
  #--- (3) Overlay median pathways
  points(itv, d2[,i,n,4],type='l',col=cols[n], lwd=lwd, lty=1.5) 
  box(col='black')
  
  if(i==1){mtext(paste(pw[n]), side=2, cex=cex.axis, line=3.5, las=2, col=cols[n], font=2)}
  mtext(labels[n,i], side=3, adj=0.04, line=-1.5, cex=cex.axis, font=2)
  if(n==1){mtext(titles[i], side=3, line=0.7, cex=cex.axis, font=2)}
  
  #--- Add legend for first plot
  if(i==1){legend("topright",
                  legend=c('Individual pathways','Median pathway', 'IQR'),
                  col=c(cols2[n], cols[n], alpha(cols[n],shading)), lty=1, lwd=c(1,lwd,8),
                  cex=cex.axis, bty='n')}
  } # vars
    
  #--- (4) Add boxplot of cumulative values  
    bp = df.cml[which(df.cml$Category==pw[n]),]
    boxplot(bp[,3:5], ylim=c(0,20), boxfill=cols2[n], names=c('Coal','Oil','Gas'), yaxt='n',
            cex.axis=cex.axis) # invisible b
    axis(2, cex.axis=cex.axis, las=2)
    mtext(labels2[n], side=3, adj=.05, line=-1.5, cex=cex.axis, font=2)
    if(n==1){mtext(expression(bold("Cml. (EJ x10"^3*")")), side=3, line=0.4, cex=cex.axis, font=2)}
    
  } # pw

#mtext(paste('*AR6 means for all scenarios',sep=''), side=1, line=1, cex=.8, outer=TRUE)

dev.off()
#}

#====================================================================================================  
#==================================================================================================== 
           
