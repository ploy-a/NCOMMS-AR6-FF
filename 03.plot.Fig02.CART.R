#--- Script to perform classification and regression tree (CART) analysis to find predictor variables for cumulative primary energy from fossil fuels (Figure 2)
#--- Last updated: 2023-08-09
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(rpart) # for fitting decision/regression trees
library(rpart.plot) # for plotting decision/regression trees
library(Metrics)
#====================================================================================================    

# Custom function: Wrap text if output of categorical variables too long
# Source: https://stackoverflow.com/questions/22618751/rpart-plot-text-shorter

split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with double spaces (needed for strwrap)
  labs <- gsub(",", "/", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=60), collapse="\n")
  }
  labs <- gsub("/", ",", labs)
}

#====================================================================================================    

#--- (1) Define dependent variables
yvars = c("Primary Energy|Coal","Primary Energy|Oil", "Primary Energy|Gas")
ff = c("coal","oil","gas")
ytime = "cml-2020-2100"; j=1
pw = c("C1","C2", "C3")

#--- (2) Independent variables fall under the following categories:
# (1) Total primary energy supply from alternative sources
# (2) Fossil fuel end-use sectors (w/ or w/o CCS), including hydrogen production
# (3) Technologies that would enable more/less FF use: CDR and CCS
# (4) Model family or study project of a given scenario

pdf(file=paste('./figures/Fig02.CART.pdf',sep=''), width=8.3, height=6.4, onefile=TRUE) # save all plots in one PDF file

for(n in 1:length(yvars)){
  if(n==1){xvars=c("Carbon Sequestration|CCS|Biomass","Carbon Sequestration|CCS|Fossil","Emissions|CO2|AFOLU",
                "Final Energy|Industry|Solids", "Final Energy|Residential and Commercial|Solids",
                "Price|Carbon",
                "Primary Energy","Primary Energy|Biomass","Primary Energy|Coal|w/ CCS",
                "Primary Energy|Gas","Primary Energy|Non-Biomass Renewables", "Primary Energy|Nuclear",
                "Primary Energy|Oil",
                "Secondary Energy|Electricity|Coal|w/ CCS","Secondary Energy|Electricity|Coal|w/o CCS")}
  
  if(n==2){xvars=c("Carbon Sequestration|CCS|Biomass","Carbon Sequestration|CCS|Fossil","Emissions|CO2|AFOLU",
                   "Final Energy|Industry|Liquids", "Final Energy|Residential and Commercial|Liquids","Final Energy|Transportation|Liquids",
                   "Price|Carbon",
                   "Primary Energy","Primary Energy|Biomass",
                   "Primary Energy|Coal", "Primary Energy|Gas",
                   "Primary Energy|Non-Biomass Renewables", "Primary Energy|Nuclear",
                   "Secondary Energy|Electricity|Oil")}
  
  if(n==3){xvars=c("Carbon Sequestration|CCS|Biomass","Carbon Sequestration|CCS|Fossil","Emissions|CO2|AFOLU",
                   "Final Energy|Industry|Gases", "Final Energy|Residential and Commercial|Gases","Final Energy|Transportation|Gases",
                   "Price|Carbon",
                   "Primary Energy","Primary Energy|Biomass",
                   "Primary Energy|Coal", "Primary Energy|Gas|w/ CCS", 
                   "Primary Energy|Oil",
                   "Primary Energy|Non-Biomass Renewables", "Primary Energy|Nuclear",
                   "Secondary Energy|Electricity|Gas|w/ CCS","Secondary Energy|Electricity|Gas|w/o CCS")}
  
  xlabels = xvars
  print(n); print(length(xvars))
  
      load('./data_generated/02b.AR6-v1.1_cml-2020-2100.select-vars.Rdata')
      icols = which(colnames(data) %in% c("ms","Category",yvars[n]))
      ytemp = data[,icols]
      colnames(ytemp)=c('ms','Category','y')
    
      icols = which(colnames(data) %in% c("ms", "Model_family", "Scenario_project", xvars))
      df = data[,icols]
      df = merge(ytemp, df, by='ms')
      df$Model_family = as.factor(df$Model_family)
      df$Scenario_project = as.factor(df$Scenario_project)

    for(i in 1:length(pw)){
        
      #--- Subset C1-C3 scenarios
      sub = df[which(df$Category==pw[i] & is.na(df$y)==FALSE),c(-1,-2)]
      rownames(sub)=df[which(df$Category==pw[i] & is.na(df$y)==FALSE),1]
      
      #--- Build the initial regression tree
      # Set x% of number of scenarios as the minimum number of observations in any terminal <leaf> node
      min.n = round(nrow(sub)*0.05)
      #print(min.n)
      rtree <- rpart(y ~ ., data=sub, 
                    method = "anova",
                    control=rpart.control(cp=0.001, xval=10, minbucket=min.n), na.action = na.rpart) # include all predictors in dataframe
      best.cp = rtree$cptable[which.min(rtree$cptable[,"xerror"]),"CP"]
      pruned.rtree <- prune(rtree, cp=best.cp)
      
      #--- Plot the pruned tree
      par(oma=c(0,0,2,0), mar=c(0,0,4,0), mgp = c(0,1,0))
      rpart.plot(pruned.rtree, type=5, digits=1, fallen.leaves=TRUE, extra=1, split.fun=split.fun) 
      
      #--- Evaluate model performance (See https://bookdown.org/mpfoley1973/data-sci/regression-tree.html)
      rpred <- predict(pruned.rtree, sub, type = "vector")
      cs_mse_cart <- mse(sub$y, rpred)
      cs_rmse_cart <- rmse(sub$y, rpred)
      mtext(paste(pw[i],': Predictors for 2020-2100 cumulative ',yvars[n],' (EJ); RMSE = ',round(cs_rmse_cart,0),sep=''), side=3, line=-2, outer=TRUE, cex = 1)

    } # i C1-C3
} # n fossil fuels
    
dev.off()

#==================================================================================================== 
#====================================================================================================    