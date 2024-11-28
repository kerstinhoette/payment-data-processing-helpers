# Run scripts that create descriptive statistics and write them to LATEX. 
# 
# Some of the settings (e.g. about plots, table style and size) need to be reset in the source scripts that are called by this main-file. 

rm(list=ls())
.rs.restartR()
library(rlist); library(xtable); library(stringr); library(corrplot); library(abind); library(igraph); library(tidyr); library(caret); library(reshape2); library(gridExtra); library(cowplot); library(grid); library(moments); library(stargazer); library(scales); library(plm); library(collapse); library(stargazer)

#source("R_scripts_both/regressions/aux/functions.r")
#source("code/network_analysis/descriptives/aux/cosmetics_tex.r")
source("code/network_analysis/explorative_regressions/helpers/functions.r")
dgt<-"3_digit"

# Set number of years for analysis
years<-2015:2023
# Write aggregate statistics (i.e. avg. across multiple years ("steps"))
AGGR_YEARS<-F
steps<-2
ALL_IN_ONE<-T

# Choose level of aggregation (by SIC and region)
#aggr<-"A16"
#reg<-"" # alternative is reg<-"Scotland_" with aggr<-"SIC_Scotland
#aggr<-"SIC_Scotland" # SIC codes are specific for Scotland (adjusted for 2-digit SIC consistency)
#aggr<-"A62"
aggr<-"CPA_2digit"
aggr<-"CPA"
reg<-"national" # alternative is reg<-"Scotland_" with aggr<-"SIC_Scotland

# Create output directories
out_dir<-paste("statistical_output/",paste0(reg,"_")[reg=="regional"],aggr,"/network_descriptives/",sep="")
if(!dir.exists(out_dir)){dir.create(out_dir, recursive=T)}

dtversion<-"Feb2024"
# Load basic network data
load(paste0("payment_data/",dtversion,"/national_account_data/",dgt,"_data/","IOT_flow_of_goods_ONS_Payment_",aggr,".RData"))
load(paste0("payment_data/",dtversion,"/national_account_data/",dgt,"_data/","industry_panel_Payment_ONS_yearly_",reg,"_level_",aggr,".RData"))

# Set matrix types you want to study
MATRICES<-c("raw_transactions_matrix", "input_share_matrix", "output_share_matrix")
# Set different network truncation methods
TRUNCATION_METHODS<-c("no_truncation", "threshold_pct_1", "threshold_pct_2.5", "threshold_pct_5", "median", "mean") # Truncation by fix threshold only makes sense when using input/output shares instead of flows, "threshold"[] ### NOT YET IMPLEMENTED)
# Set up- or downstream network 
# (Note: up/downstream input/output share matrices are not identical because different denominators are used [tbc]. Output share matrix reflects importance of an industry as a customer, input shares reflect the importance of the supplier (see Leontief vs Gosh))
DIRECTIONS<-c("upstream", "downstream")


# Write summary statistics and rankings for different snapshots in time: 
source("code/network_analysis/descriptives/src/stats_at_diff_times.r")

# Write correlation statistics: 
source("code/network_analysis/descriptives/src/stats_correlation_analysis.r")

# Write correlation statistics: 
source("code/network_analysis/descriptives/src/stats_correlation_analysis_node_level.r")

# Write network statistics and make network plots: 
source("code/network_analysis/descriptives/src/stats_netw.r")

# Plot network density by percentage truncation threshold:
source("code/network_analysis/descriptives/src/plot_density_by_truncation.r")

# Plot aggregate time series data:
source("code/network_analysis/descriptives/src/stats_aggr_time_series.r")

