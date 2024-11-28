# Run scripts that create descriptive statistics and write them to LATEX. 
# 
# Some of the settings (e.g. about plots, table style and size) need to be reset in the source scripts that are called by this main-file. 

rm(list=ls())
.rs.restartR()
library(rlist); library(xtable); library(stringr); library(corrplot); library(abind); library(igraph); library(tidyr); library(caret); library(reshape2); library(gridExtra); library(cowplot); library(grid); library(moments); library(stargazer); library(scales); library(plm); library(collapse); library(stargazer); library(zoo); library(data.table)

#source("R_scripts_both/regressions/aux/functions.r")
#source("code/network_analysis/descriptives/aux/cosmetics_tex.r")
source("code/network_analysis/explorative_regressions/helpers/functions.r")
dgt<-"5_digit"

# Set number of years for analysis
years<-2015:2023

# Choose level of aggregation (by SIC and region)
aggr<-"5_digit"
reg<-"national" # alternative is reg<-"Scotland_" with aggr<-"SIC_Scotland

# Create output directories
out_dir<-paste0("statistical_output/",paste0(reg,"_")[reg=="regional"],aggr,"/network_analysis/")
if(!dir.exists(out_dir)){dir.create(out_dir, recursive=T)}

dtversion<-"Feb2024"
#dtversion<-"Mar_data"


# Load basic network data -- monthly
load(paste0("Payment_data/",dtversion,"/edgelist_transactions/",dgt,"_data/aggregate_amt_by_month_and_5_sic_national_level.RData"))
amt<-df
load(paste0("Payment_data/",dtversion,"/edgelist_transactions/",dgt,"_data/aggregate_cnt_by_month_and_5_sic_national_level.RData"))
cnt<-df
df<-cbind(amt, cnt[,3:ncol(cnt)])
rm(amt, cnt)
# Transform from flow-of-money into flow-of-goods representation by swapping from & to columns
if(sum(colnames(df)[1:2]==c("from","to"))==2){colnames(df)[1:2]<-c("to","from")}else{stop("check this and ensure flow of goods representation")}
dfm<-df
# Load basic network data -- yearly
load(paste0("Payment_data/",dtversion,"/edgelist_transactions/",dgt,"_data/aggregate_amt_by_year_and_5_sic_national_level.RData"))
amt<-df
colnames(amt)[3:ncol(amt)]<-paste0(colnames(amt)[3:ncol(amt)],"_amt")
load(paste0("Payment_data/",dtversion,"/edgelist_transactions/",dgt,"_data/aggregate_cnt_by_year_and_5_sic_national_level.RData"))
cnt<-df
colnames(cnt)[3:ncol(cnt)]<-paste0(colnames(cnt)[3:ncol(cnt)],"_cnt")
df<-cbind(amt, cnt[,3:ncol(cnt)])
rm(amt, cnt)
# Transform from flow-of-money into flow-of-goods representation by swapping from & to columns
if(sum(colnames(df)[1:2]==c("from","to"))==2){colnames(df)[1:2]<-c("to","from")}else{stop("check this and ensure flow of goods representation")}
dfy<-df
rm(df)

# Set matrix types you want to study
MATRICES<-c("raw_transactions", "input_share", "output_share")
# Set different network truncation methods
TRUNCATION_METHODS<-c("no_truncation", "threshold_pct_0.1", "threshold_pct_0.5", "threshold_pct_1", "threshold_pct_2.5", "threshold_pct_5") # Truncation by fix threshold only makes sense when using input/output shares instead of flows, "threshold"[] ### NOT YET IMPLEMENTED)
# Set up- or downstream network 
# (Note: up/downstream input/output share matrices are not identical because different denominators are used [tbc]. Output share matrix reflects importance of an industry as a customer, input shares reflect the importance of the supplier (see Leontief vs Gosh))
DIRECTIONS<-c("undirected", "upstream", "downstream")

#source("code/network_analysis/granular_network/src/growth_rate_correlations.r")








