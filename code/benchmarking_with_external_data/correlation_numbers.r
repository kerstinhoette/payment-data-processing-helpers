# Calculate aggregate correlations for ESCOE presentation


# Clean memory
rm(list=ls())
.rs.restartR()


library(stringr); library(readxl); library(dplyr); library(reshape2); library(plm); library(collapse)

ddir<-
source("code/network_analysis/explorative_regressions/helpers/functions.r")

# Load data
# 1. National panel
load("national_account_data/industry_level_panel_Payment_ONS_CPA_2digit.RData")
dt$outputs_minus_inputs_amt<-dt$sum_outputs_amt-dt$sum_inputs_amt
dt$outputs_minus_inputs_SUT<-dt$sum_outputs_SUT-dt$sum_inputs_SUT
cn<-colnames(dt)
nat<-pdata.frame(dt, index=c("code","year"))
# 2. National network
load("national_account_data/IOT_edgelist_flow_of_goods_ONS_Payment_CPA_2digit.RData")
net<-edgelist_wide_to_long(df)
net$from_to<-paste(net$from,"_",net$to)
net<-pdata.frame(net, index=c("from_to","year"))
# 3. Regional panel
load("national_account_data/region_industry_level_panel_Payment_ONS_Region80.RData")
dt$outputs_minus_inputs_amt<-dt$sum_outputs_amt-dt$sum_inputs_amt
cnr<-colnames(dt)
reg<-pdata.frame(dt, index=c("code","year"))
rm(dt,df)

growth_rates<-T
gnet<-G(net, stubs = F)
gnet[,!(colnames(gnet)%in%c("from_to","year"))]<-apply(gnet[!(colnames(gnet)%in%c("from_to","year"))],2,FUN=function(x){return(ifelse(is.nan(x),NA,x))})
greg<-G(reg, stubs = F)
greg[,!(colnames(greg)%in%c("code","year"))]<-apply(greg[!(colnames(greg)%in%c("code","year"))],2,FUN=function(x){return(ifelse(is.nan(x),NA,x))})
colnames(reg)<-cnr
colnames(greg)[7]<-colnames(reg)[11]
gnat<-G(nat, stubs = F)
gnat[,!(colnames(gnat)%in%c("code","year"))]<-apply(gnat[!(colnames(gnat)%in%c("code","year"))],2,FUN=function(x){return(ifelse(is.nan(x),NA,x))})
colnames(nat)<-cn
colnames(gnat)[13:ncol(gnat)]<-colnames(nat)[15:ncol(nat)]

# Output directory
outdir<-paste0("../../statistical_output/aggregate_correlations_numbers/")
if(!dir.exists(outdir)){dir.create(outdir,recursive = T)}
fn<-paste0(outdir,"Aggregate_correlations_for_ESCOE.txt")
write(paste(Sys.time()),file = fn,append = F)

yrs<-2015:2021

write(paste0("\n\n\n #########  Network data: Raw transactions between 80 industries in 2019  ########"),file=fn,append=T)
write_corr_to_file(net[,c("AMT","SUT","year")],fn,2019)
write_corr_to_file(net[,c("CNT","SUT","year")],fn,2019)

write(paste0("\n\n\n ######### Growth rates\nNetwork data: Raw transactions between 80 industries in 2019  ########"),file=fn,append=T)
write_corr_to_file(gnet[,c("AMT","SUT","year")],fn,2016:2020)
write_corr_to_file(gnet[,c("CNT","SUT","year")],fn,2016:2020)



write(paste0("\n\n\n #########  Industry-level panel data: 80 industries Payment aggregates correlated with various ONS indicators derived from the SUT in 2019  ########"),file=fn,append=T)
write_corr_to_file(nat[,c("year","sum_inputs_amt","sum_inputs_SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","sum_outputs_SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_amt","outputs_minus_inputs_SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_inputs_amt","Total intermediate consumption at purchasers' prices SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","Compensation of employees SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","Gross operating surplus and mixed income SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","Gross valued added at basic prices SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_amt","Compensation of employees SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_amt","Gross operating surplus and mixed income SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_amt","Gross valued added at basic prices SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_SUT","Compensation of employees SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_SUT","Gross operating surplus and mixed income SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_SUT","Gross valued added at basic prices SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","Total output at basic prices SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","Total exports of Goods SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_outputs_amt","Total domestic output of products at basic prices SUT")],fn,2019)
write_corr_to_file(nat[,c("year","sum_inputs_amt","Total imports of goods and services SUT")],fn,2019)
write_corr_to_file(nat[,c("year","outputs_minus_inputs_amt","Total imports of goods and services SUT")],fn,2019)

write(paste0("\n\n\n #########  Growth rates\nIndustry-level panel data: 80 industries Payment aggregates correlated with various ONS indicators derived from the SUT in 2019  ########"),file=fn,append=T)
write_corr_to_file(gnat[,c("year","sum_inputs_amt","sum_inputs_SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","sum_outputs_SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_inputs_amt","Total intermediate consumption at purchasers' prices SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","Compensation of employees SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","Gross operating surplus and mixed income SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","Gross valued added at basic prices SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_amt","Compensation of employees SUT")],fn,2019)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_amt","Gross operating surplus and mixed income SUT")],fn,2019)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_amt","Gross valued added at basic prices SUT")],fn,2019)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_SUT","Compensation of employees SUT")],fn,2019)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_SUT","Gross operating surplus and mixed income SUT")],fn,2019)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_SUT","Gross valued added at basic prices SUT")],fn,2019)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","Total output at basic prices SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","Total exports of Goods SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_outputs_amt","Total domestic output of products at basic prices SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","sum_inputs_amt","Total imports of goods and services SUT")],fn,2016:2020)
write_corr_to_file(gnat[,c("year","outputs_minus_inputs_amt","Total imports of goods and services SUT")],fn,2016:2020)



write(paste0("\n\n\n #########  Regional data: 80 industries, 13 regions Payment aggregates correlated with ONS regional gross value added in 2019  ########"),file=fn,append=T)
write_corr_to_file(reg[,c("year","sum_inputs_amt","Gross Value Added (current prices)")],fn,2019)
write_corr_to_file(reg[,c("year","sum_outputs_amt","Gross Value Added (current prices)")],fn,2019)
write_corr_to_file(reg[,c("year","outputs_minus_inputs_amt","Gross Value Added (current prices)")],fn,2019)
write_corr_to_file(reg[,c("year","sum_inputs_cnt","Gross Value Added (current prices)")],fn,2019)
write_corr_to_file(reg[,c("year","sum_outputs_cnt","Gross Value Added (current prices)")],fn,2019)

write(paste0("\n\n\n #########   Growth rates\nRegional data: 80 industries, 13 regions Payment aggregates correlated with ONS regional gross value added in 2019  ########"),file=fn,append=T)
write_corr_to_file(greg[,c("year","sum_inputs_amt","Gross Value Added (current prices)")],fn,2016:2020)
write_corr_to_file(greg[,c("year","sum_outputs_amt","Gross Value Added (current prices)")],fn,2016:2020)
write_corr_to_file(greg[,c("year","outputs_minus_inputs_amt","Gross Value Added (current prices)")],fn,2019)
write_corr_to_file(greg[,c("year","sum_inputs_cnt","Gross Value Added (current prices)")],fn,2016:2020)
write_corr_to_file(greg[,c("year","sum_outputs_cnt","Gross Value Added (current prices)")],fn,2016:2020)


x<-as.data.frame(cbind(nat$outputs_minus_inputs_amt, nat$outputs_minus_inputs_SUT, as.character(nat$code)))[nat$year==2019,]
colnames(x)<-c("AMT","SUT","code")
x$AMT<-as.numeric(as.character(x$AMT))
x$SUT<-as.numeric(as.character(x$SUT))
x$color<-ifelse(substr(x$code,1,1)=="C","red","black")
x$color<-ifelse(substr(x$code,1,1)%in%c("A","B"),"green",x$color)
x$color<-ifelse(substr(x$code,1,1)%in%c("D","E","F"),"blue",x$color)
lm<-range(c(x$AMT,x$SUT),na.rm = T)
plot(x$AMT,x$SUT,cex=0,ylim=lm,xlim=lm)
abline(0,1)
text(x$AMT,x$SUT,as.character(x$code),col=x$color)

