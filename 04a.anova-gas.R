#--- Script to conduct ANOVA to explore influence of select variables on 2020-2100 gas supply
#--- Last updated: 2023-08-09
#====================================================================================================
wdir='/Users/ploya/Dropbox/SEI.SHARED/PGR 2021/2. CH2 Analysis/Peer_review_version/analysis_copy/' # set working directory
# wdir = xxx # set working directory
setwd(paste(wdir))
library(rms)
library(car)
#====================================================================================================      

load('./data_generated/02a.AR6-v1.1_select-vars_2010-2100-5y-interp.Rdata'); 

sv=c("Primary Energy|Gas",
     "Carbon Sequestration|CCS|Biomass","Carbon Sequestration|CCS|Fossil","Emissions|CO2|AFOLU",
     "Final Energy|Industry|Gases", 
     "Price|Carbon",
     "Primary Energy|Non-Biomass Renewables")

itv=seq(2020,2100,by=10)

ind=which(colnames(data) %in% itv)
ind2=which(colnames(data) %in% c('ms','Category','Model_family','Scenario_project','Variable'))
d1=data[which(data$Variable %in% sv),c(ind2,ind)]

d1$Model_family=factor(d1$Model_family)
d1$Scenario_project=factor(d1$Scenario_project)
pw=sort(unique(d1$Category))

d2 = reshape2::melt(d1, id.vars=c('ms','Category','Model_family','Scenario_project','Variable'))
colnames(d2)[6]='Year'
d3 = reshape2::dcast(d2, ms + Category + Model_family + Scenario_project + Year ~ Variable, value.var='value')
ind=which(colnames(d3)=="Primary Energy|Gas")
colnames(d3)[ind]='y'

res = array(NA, dim=c(length(sv)+2,7,length(itv), length(pw))) # storage array
colnames(res)=c('x','df','sum.sq','mean.sq','f.value','p.value','vif')

for(n in 1:3){
  for(j in 1:length(itv)){
    #print(itv[j])
    d4=d3[which(d3$Category==pw[n] & d3$Year==itv[j]),c(-1,-2,-5)]
    res.aov=aov(y ~., data = d4)
    print(summary(res.aov))
    
    temp = as.data.frame(summary(res.aov)[[1]])
    res[,1,j,n] = rownames(temp)
    rownames(temp) = NULL
    res[,2,j,n] = temp[,1]
    res[,3,j,n] = temp[,2]
    res[,4,j,n] = temp[,3]
    res[,5,j,n] = temp[,4]
    res[,6,j,n] = temp[,5]
    
    # Check for multicollinearity for continuous variables
    res.aov2=aov(y ~., data = d4[,c(-1,-2)])
    res[3:nrow(res),7,j,n] = car::vif(res.aov2)[1:length(sv)]
  }
}

#--- Check max. Variance Inflation Factor (make sure below 5)
temp=as.numeric(res[,7,,])
which(temp>5)
max(temp, na.rm=T) # max. VIF = 3.1

save(res, file='./data_generated/04a.anova-gas.Rdata')
#====================================================================================================      
#====================================================================================================         