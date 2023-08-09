#--- Script to perform K-means cluster analysis for C1-C3 gas pathways based on the 2030, 2050, 2070, and 2100 annual values
#--- Last updated: 2023-08-09
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(ggplot2)
library(RColorBrewer)
library(scales)
#====================================================================================================    

#--- Perform cluster analysis

pw = c('C1','C2','C3')
nc=3 # number of output clusters

for(n in 1:length(pw)){
    
    #--- Cluster analysis, identify nc groups within each C1-C3 category
    load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data
    yvars = c("Primary Energy|Gas")
    icols = which(colnames(data) %in% c("ms","Category","2030","2050","2075","2100"))
    ytemp = data[which(data$Category == pw[n] & data$Variable %in% yvars), icols]
    
    df2=ytemp[,-2]
    df2 = na.omit(df2)
    rownames(df2)=df2[,1]
    df3 = scale(df2[,-1])
    print(head(df3))
    
    kc <- kmeans(df3, centers = nc, nstart = 25)
    str(kc)
    
    # Visualize results
    colnames(df3)=c('y2030','y2050','y2075','y2100')
    df3 %>%
      as_tibble() %>%
      mutate(cluster = kc$cluster,
             ms = row.names(df3)) %>%
      ggplot(aes(y2030, y2100, color = factor(cluster), label = ms)) +
      geom_text()
    
    res = as.data.frame(kc$cluster)
    res$ms = rownames(res)
    rownames(res)=NULL; colnames(res)[1]='cluster'
    df4 = merge(df2, res, by='ms')
    ms = df4
    print(head(ms))
    save(ms, file=paste('./data_generated/04b.',pw[n],'_gas-clusters.Rdata',sep='')) # ms
    
} # n C1-C3

#====================================================================================================  

#--- Plot the resulting FF primary energy pathways by these clusters
pw = c('C1','C2','C3')
nc=3 # number of clusters within each Category

#--- Plot parameters
p1=brewer.pal(9,"Oranges")
p2=brewer.pal(9,"Blues")
p3=brewer.pal(9,"Greens")
p4 = brewer.pal(8, 'Set2')
cols=c(p2[8],p3[6],p1[6])
shading=0.3

xlim=c(2010,2100)
ymax=rep(350,3) # by fuel
cex.axis=1.2; lwd=0.5
titles=c('Coal supply (EJ/y)','Oil supply (EJ/y)','Gas supply (EJ/y)')

pdf(file=paste('./figures/Fig04x.AR6.2010-2100_Primary-Energy-FF_individual-scenarios_by-gas-clusters.pdf',sep=''), width=11, height=8.5)
layout(matrix(c(1:(nc*3)), nc, 3, byrow = TRUE))
par(oma=c(1,6,3,0), mar=c(3,2,3,2), mgp = c(0,1,0))

for(n in 1:length(pw)){
  load(paste('./data_generated/04b.',pw[n],'_gas-clusters.Rdata',sep='')) # ms
  
  for(k in 1:nc){
    
    #--- (1) Load data
    load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata') # data
    pw = sort(unique(data$Category))
    itv=seq(2010,2100,by=5)
    sv=c("Primary Energy|Coal","Primary Energy|Oil","Primary Energy|Gas")
    data = merge(ms[,c(1,ncol(ms))], data, by='ms')
    sub = data[which(data$Category==pw[n] & data$cluster==k),]
    
    for (i in 1:length(sv)){
      sub2 = sub[which(sub$Variable==sv[i]),]
      ncount = nrow(sub2) # number of individual scenarios in that category
      
      plot(NULL, ylab='', xlab='', xaxt='n',yaxt='n', xlim=c(xlim[1],xlim[2]),ylim=c(0,ymax[i]),xaxs='i',yaxs='i')
      axis(1, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis, las=2)
      
      for (j in 1:ncount){
        icol = which(colnames(sub2)==xlim[1])
        points(itv, sub2[j,icol:ncol(sub2)],type='l',col=cols[n], lwd=lwd, lty=1) 
      }
      
      mtext(titles[i], side=3, line=0.8, cex=cex.axis)
      if(i==1){mtext(paste('#',k,sep=''), side=2, cex=cex.axis, line=3.2, las=2)}

    } # i ff
  } # k clusters
  
  mtext(paste(pw[n],' scenarios',sep=''), side=3, line=0, cex=cex.axis+0.2, outer=TRUE)
  
} # n pw

dev.off()

#====================================================================================================    
#====================================================================================================    

