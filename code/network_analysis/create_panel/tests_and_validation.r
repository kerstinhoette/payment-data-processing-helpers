# Make some validation exercises with the data
# 
rm(list=ls(pattern=""))
library(plm); library(stringr); library(corrplot)
setwd("~/Desktop/inno_netw")
# Load data settings (i.e. aggregation level, whether to use truncated data or subsets to create distance measures etc.)
source("R_scripts_both/settings_data.r")

load(paste0(DATADIR,"other_controls/output_multipliers.RData"))
load(paste0(DATADIR, "panel_data.RData"))
load(paste0(DATADIR, "other_controls/NBER_productivity_data.RData"))
load(paste0(DATADIR, "other_controls/share_based_network_properties.RData"))
d$naics_year<-paste0(d$naics,"_",d$year); dt$naics_year<-paste0(dt$naics,"_",dt$year); dt0$naics_year<-paste0(dt0$naics,"_",dt0$year); network_properties$naics_year<-paste0(network_properties$naics,"_",network_properties$year)

d0<-merge(dt, dt0, by="naics_year", all.x = T, all.y = T)
d0<-merge(d, d0, by="naics_year", all.x = T, all.y = T)
d0<-merge(d0, network_properties, by="naics_year", all.x = T, all.y = T)
d<-d0[c("naics_year", "year.x", "naics.x", "name", "patent_stock", "citation_weighted_patent_stock", "sum_citations_in", "sum_citations_out", 
       "real_sum_output", "real_sum_input", "sum_output", "sum_input", 
       "output_multiplier_sum_input", "output_multiplier_sum_output", "output_multiplier_vship", 
       "output_multiplier_real_sum_input", "output_multiplier_real_sum_output", "output_multiplier_real_vship", 
       "patent_in_pagerank", "patent_in_strength", "patent_indegree", "patent_in_eigencentr", "patent_in_degreecentr", "patent_in_betwcentr", "patent_out_pagerank", "patent_out_strength", "patent_outdegree", "patent_out_eigencentr", "patent_out_degreecentr", "patent_out_betwcentr", "io_in_pagerank", "io_in_strength", "io_indegree", "io_in_eigencentr", "io_in_degreecentr", "io_in_betwcentr", "io_out_pagerank", "io_out_strength", "io_outdegree", "io_out_eigencentr", "io_out_degreecentr", "io_out_betwcentr",   
       "emp", "pay", "prode", "prodh", "prodw", "vship", "matcost", "vadd", "invest", "invent", "energy", "cap", "equip", "plant", 
       "emp_growth_rate", "vadd_growth_rate", 
       "piship", "pimat", "piinv", "pien", "dtfp5", "tfp5", "dtfp4", "tfp4")]
rm(d0, dt, dt0, network_properties)
colnames(d)<-str_remove_all(colnames(d),"\\.x")
d<-d[d$year!="2017",]
d<-d[order(d$naics_year),]
pd<-pdata.frame(d, index = c("naics", "year"))

cl<-which(!(colnames(d) %in% c("naics_year", "year", "naics", "name")))
fn<-paste0(DATADIR, "correlations_lag0.pdf")
pdf(fn)
for(l in seq(0, 30, 5)){
  x0<-d[d$year>=(min(d$year)+l),cl]; x1<-d[d$year<=(max(d$year)-l),cl]
  x<-cor(x0,x1,use = "pairwise.complete.obs")
  corrplot(x, type="upper", tl.cex = 0.4, title=paste0("Pairwise correlations (lag ",l,")"))
}
dev.off()

fn<-paste0(DATADIR, "correlations_with_naicsFE.pdf")
pdf(fn)
for(l in seq(0, 30, 5)){
  cl<-which(!(colnames(d) %in% c("naics_year", "year", "name")))
  #l<-5
  t0<-which(d$year>=(min(d$year)+l)); t1<-which(d$year<=(max(d$year)-l))
  d0<-rbind(d[t0,cl], d[t1,cl])
  x<-(by(d0, d0$naics, FUN=function(x){cor(x[(1:(nrow(x)/2)),], x[(((nrow(x)/2)+1):nrow(x)),], use = "pairwise.complete.obs")}, simplify = T))
  x0<-do.call(rbind.data.frame, x)
  x<-data.frame()
  for(cl in colnames(x0)){
    x<-rbind(x, (apply(x0[which(substr(rownames(x0),8,50)==cl),], 2, FUN=function(x){mean(x, na.rm = T)})))
  }
  #colnames(x)<-rownames(x)<-colnames(x0)
  x<-matrix(unlist(x), nrow = nrow(x), ncol = ncol(x))
  colnames(x)<-rownames(x)<-colnames(x0)
  corrplot(x, type="upper", tl.cex = 0.4, title=paste0("Pairwise correlations (lag ",l,")"))
}
dev.off()





txt<-paste0(DATADIR, "correlation_patterns.txt")

# Print a few pair wise correlation and autocorrelation patterns 
for(cl in colnames(pd)){
  if(cl %in% c("naics_year", "year", "naics", "name")){next}
  
  
  
  print(cl)
  
}


# Calculate autocorrelation
x<-d[order(d$naics_year),]
x0<-x[x$year!=max(x$year),]; x1<-x[x$year!=min(x$year),]
if(sum((x0$year+5) != x1$year)!=0 || sum(x0$naics != x1$naics)>0){stop("not matching")}
cor(x0$sum_output, x1$sum_output)
cor(x1$vadd[x1$year=="2012"], x0$output_multiplier_real_sum_input[x0$year=="1977"])

cor(x1$vadd[x1$year=="2012"], x0$output_multiplier_real_sum_output[x0$year=="1977"])
cor(x1$vadd[x1$year=="2012"], x0$output_multiplier_sum_input[x0$year=="1977"])
cor(x1$vadd[x1$year=="2012"], x0$output_multiplier_sum_output[x0$year=="1977"])
cor(x1$vadd[x1$year=="2012"], x0$output_multiplier_vship[x0$year=="1977"])

mns<-by(x$output_multiplier_vship, x$naics, function(x){mean(x, na.rm=T)})
mdtfp<-by(x$dtfp5, x$naics, function(x){mean(x, na.rm=T)})
cor(x1$vadd[x1$year=="2012"], mns)
cor(x1$sum_output[x1$year=="2012"], mns)
cor(x1$patent_stock[x1$year=="2012"], mns)
cor(mdtfp, mns)

by(pd$sum_output, pd$naics, function(x){acf(x, lag.max = 1, plot = FALSE)})


View(d[c("naics", "year", "vship", "piship", "pimat", "piinv", "pien")])




length(unique(d0$naics))
length(unique(dt$naics))
length(unique(dt0$naics))

length(unique(d0$year))
length(unique(dt0$year))

d0$naics_year<-paste(d0$naics, d0$year, sep="_")
dt0$naics_year<-paste(dt0$naics, dt0$year, sep="_")
dt$naics_year<-paste(dt$naics, dt$year, sep="_")








