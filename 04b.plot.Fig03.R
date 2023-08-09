#--- Script to plot Figure 3 in manuscript
#--- Last updated: 2023-08-09
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(RColorBrewer)
library(scales)
library(reshape2)
library(abind)
library(ggplot2)
#library(cowplot)
library(gridExtra)
#====================================================================================================      

#--- CUSTOM FUNCTIONS

#--- Save legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#--- Exclude a vector
`%notin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 
#====================================================================================================

#--- (1) Set plot parameters
sv=c("Model family", "Scenario project", "BECCS","Fossil CCS", "eAFOLU",'Industry use', 'Carbon price', 'RE supply', 'Others')
itv=seq(2020,2100,by=10)
pw=c('C1','C2','C3')

# Define custom palette w/ specific colour assigned to each independent variable
pal2 = brewer.pal(8, 'Set2')
pal1 = brewer.pal(9, 'Blues')
mypal= pal2
cpal=c("Scenario project"=mypal[3],
       "Model family"=mypal[2],
       "Fossil CCS"=mypal[6],
       "Industry use"=mypal[5],
       "BECCS"=mypal[4],
       "eAFOLU"=mypal[1],
       "Carbon price"=mypal[7],
       "RE supply"=pal1[9],
       "Others"=mypal[8]
       )

#====================================================================================================

#--- (2) Create plots

for(n in 1:length(pw)){
  
  #--- (1) Load ANOVA results
  load('./data_generated/04a.anova-gas.Rdata') # res
  
  #--- (2) Create a dataframe for plotting stacked area chart over time
  data = array(NA, dim=c(length(sv),length(itv)))

  for(k in 1:length(itv)){
    res2 = res[,,k,n]
    res2=as.data.frame(res2)
    res2[,-1]=sapply(res2[,-1],as.numeric)
    res2$r = res2$sum.sq*100/sum(res2$sum.sq)
    data[,k]=res2$r
  }
  
  data = as.data.frame(data)
  colnames(data)=itv
  data$sv = sv
  
  #--- (3) Rank by largest contributor for all time
  sub = data[data$sv!="Others",]
  sub$total = rowSums(sub[,c(-ncol(sub))])
  sub = sub[order(sub$total, decreasing=TRUE),]
  lev = c('Others', rev(sub$sv))
  
  #--- (4) Organize dataframe for plotting
  d2 = melt(data, id.vars='sv')
  colnames(d2)[2]='year'
  d2$year = as.integer(as.character(d2$year))
  d2$sv = factor(d2$sv, levels=lev)
  print(lev)
  
  #--- (5) Create plot
  if(n==1){plot1 = ggplot() + 
    geom_area(d2, mapping = aes(x=year, y=value, fill=sv)) +
    ylab(paste(pw[n],sep='')) +
    xlab(paste("",sep='')) +
    scale_fill_manual(values = cpal, name="Drivers") +
    scale_x_continuous(limits=c(2020,2100), expand = c(0,0)) +
    scale_y_continuous(limits=c(0, 100.1), expand = c(0,0), name="Percent of total variance (%)") +
    ggtitle(paste(pw[n])) +
    theme_bw() +
    theme(plot.margin=unit(c(0.3,0.2,0.3,0), 'cm'),
          axis.text=element_text(size=7),
          axis.title.y=element_text(size=7),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.title = element_text(size=7, color="grey20", face="bold", hjust=0.5),
          legend.position = "none")
  }
  
  if(n==2){plot2 = ggplot() + 
    geom_area(d2, mapping = aes(x=year, y=value, fill=sv)) +
    ylab(paste(pw[n],sep='')) +
    xlab(paste("",sep='')) +
    scale_fill_manual(values = cpal, name="") +
    scale_x_continuous(limits=c(2020,2100), expand = c(0,0)) +
    scale_y_continuous(limits=c(0, 100.1), expand = c(0,0), name="") +
    ggtitle(paste(pw[n])) +
    theme_bw() +
    theme(plot.margin=unit(c(0.3,0.2,0.3,0), 'cm'),
          axis.text=element_text(size=7),
          axis.title.y=element_text(size=7),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.title = element_text(size=7, color="grey20", face="bold", hjust=0.5),
          legend.position = "none")
  }
  
  if(n==3){plot3 = ggplot() + 
    geom_area(d2, mapping = aes(x=year, y=value, fill=sv)) +
    ylab(paste(pw[n],sep='')) +
    xlab(paste("",sep='')) +
    scale_fill_manual(values = cpal, name="") +
    scale_x_continuous(limits=c(2020,2100), expand = c(0,0)) +
    scale_y_continuous(limits=c(0, 100.1), expand = c(0,0), name="") +
    ggtitle(paste(pw[n])) +
    theme_bw() +
    theme(plot.margin=unit(c(0.3,0.2,0.3,0), 'cm'),
          axis.text=element_text(size=7),
          axis.title.y=element_text(size=7),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.title = element_text(size=7, color="grey20", face="bold", hjust=0.5),
          legend.title=element_text(size=7), 
          legend.text=element_text(size=7),
          legend.key.size = unit(16, 'pt')
          ) + guides(fill=guide_legend(ncol=1))
  leg3<-g_legend(plot3) 
  }
  
}

#====================================================================================================  

#---(3) Plot to PDF
pdf(file=paste('./figures/Fig03.pdf',sep=''), width=7, height=2.6, paper="a4", colormodel="rgb")

grid.arrange(
  arrangeGrob(plot1,
              plot2,
              plot3 + theme(legend.position="none"), 
              leg3,
              nrow=1, ncol=4, widths=c(1.15,1.15,1.15,0.55))
)

dev.off()

#==================================================================================================== 
#====================================================================================================     
