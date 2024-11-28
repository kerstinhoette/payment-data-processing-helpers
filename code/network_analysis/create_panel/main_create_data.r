rm(list=ls(pattern=""))
.rs.restartR()
library(igraph); library(readr); library(stringr)

source("code/network_analysis/descriptives/aux/cosmetics_tex.r")

years<-2010:2022

# Choose level of aggregation (by SIC and region)
aggr<-"SIC_Scotland"
reg<-"" # alternative is reg<-"" with aggr<-"SIC_Scotland
# Load data
aggr<-"A16"
aggr<-"A62"
aggr<-"CPA_2digit"
reg<-""
dtversion<-"Feb2024"
load(paste0("Payment_data/",dtversion,"/national_account_data/temp/","IOT_as_edgelist_ONS_Payment_all_years_",aggr,paste0(reg,"_")[reg!=""],".RData"))
df0<-df

source("code/network_analysis/create_panel/aux/create_panel_step1.r")
# In case industries were removed due to poor coverage, do also remove these sectors from the network data
i<-unique(c(df0$from[!(df0$from %in% dt$code)], df0$to[!(df0$to %in% dt$code)]))
if(length(i)>0){df0<-df0[(df0$from != i & df0$to != i),]}
rm(i)
df<-df0
save(df, file=paste0("Payment_data/Mar_data/national_account_data/","IOT_as_edgelist_ONS_Payment_all_years_",aggr,paste0(reg,"_")[reg!=""],".RData"))
#warnings()
#source("R_scripts_both/create_merged_panel/aux/add_industry_controls_nber.r")
#warnings()
#source("R_scripts_both/create_merged_panel/aux/create_merged_panel_step2.r")
#warnings()
source("code/network_analysis/create_panel/aux/compute_netw_measures.r")
#warnings()
#source("R_scripts_both/create_merged_panel/aux/compute_generalized_distances.r")
#warnings()
#source("R_scripts_both/create_merged_panel/aux/compile_spillover_data.r")
#warnings()
#source("R_scripts_both/create_merged_panel/aux/compile_link_formation_data.r")
#warnings()
#source("R_scripts_both/create_merged_panel/aux/compile_trade_data_for_regressions.r")
#warnings()









