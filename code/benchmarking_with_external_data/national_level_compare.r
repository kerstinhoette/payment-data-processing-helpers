### This script benchmarks the national level data against external data sources ###
# 
# It produces a number of benchmarking statistics at the national level and for different time aggregations (monthly, quarterly, annual) 
# 
# External data are: 
#  - OECD key economic indicators, monetary indicators
#  - Payment statistics from PSR 
#  - 
#  For references to the data sets, compilation procedures and further details, see script data_and_code/code/data_processing/3b_supplement_with_external_data_national.r 
#  
# 
#
# Types of benchmarking statistics: 
#  - Ratio and direct comparison against levels (e.g. ratio to GDP, transaction values/volumes reported by Pay.uk, etc.)
#  - Time series correlation patterns of levels and growth rates (e.g. with GDP, money aggregates, unemployment, etc.)
#  - Time series figures of scaled data to show co-evolution
#  - TO BE CONTINUED
#
#
#
rm(list=ls())
.rs.restartR()


library(ggplot2); library(reshape2); library(gridExtra); library(stringr)

# Create folder for outputs
plotdir<-"output/benchmarking_with_external_data/national_level/"
if(!dir.exists(plotdir)){dir.create(plotdir, recursive = T)}


tm<-"year"
tm<-"quarter"
tm<-"month"
for(tm in c("year", "quarter", "month")){
  if(tm=="month"){covid<-"2020.03"}else if(tm=="quarter"){covid<-"2020.1"}else{covid<-"2021"}
  
  load(paste0("Payment_data/Mar_data/macro_aggregates/Payment_and_OECD_key_economic_indicators_",tm,".RData"))
  dkei<-d
  load(paste0("Payment_data/Mar_data/macro_aggregates/Payment_and_OECD_monetary_financial_indicators_",tm,".RData"))
  dmei<-d
  load(paste0("Payment_data/Mar_data/macro_aggregates/Payment_and_OECD_main_economic_indicators_",tm,".RData"))
  dmei2<-d
  load(paste0("Payment_data/Mar_data/macro_aggregates/Payment_and_payuk_payment_statistics_",tm,".RData"))
  d<-merge(dkei, subset(dmei, select = -c(amt, cnt)), by = tm, all.x = T, all.y = T)
  d<-merge(d, subset(dmei2, select = -c(amt, cnt)), by = tm, all.x = T, all.y = T)
  d<-merge(d, subset(payuk, select = -c(amt, cnt)), by = tm, all.x = T, all.y = T)
  rm(dmei, dkei, payuk)
  if(tm %in% c("quarter", "year")){
    load(paste0("Payment_data/Mar_data/macro_aggregates/Payment_and_ons_macro_aggregates_",tm,".RData"))
    d<-merge(d, subset(ons, select = -c(amt, cnt)), by = tm, all.x = T, all.y = T)
    rm(ons)
  }
  
  # Remove incomplete data for 2022
  if(tm == "year"){d<-d[!(d$year %in% c("2022", paste(min(d$year):2014))), ]
  }else if(tm == "month"){ # Get order of time dimension correct & remove all data after Jul-2022
    my<-paste0(rep(month.abb, length(2015:2022)),"-",rep(2015:2022, each=12))
    my<-my[my %in% d$month[!(d$month %in% c(paste0(c("Aug", "Sep", "Oct", "Nov"), "-2022")))]]
    d<-d[match(my, d$month),]
    #d$month<-as.factor(d$month)
    d$month<-as.numeric(substr(d$month, 5,8)) + rep(seq(0.01,0.12,by=0.01), 12)[1:length(d$month)]
    rm(my)
  }else if(tm == "quarter"){
    d<-d[!str_detect(d$quarter, paste0("Q3-2022|Q4-2022|",paste(min(as.numeric(substr(d$quarter,4,7))):2014,collapse = "|"), collapse="|")),]
    d<-d[order(substr(d$quarter,4,7)),]
    d$quarter<-as.numeric(substr(d$quarter,4,7)) + rep(seq(0.1,0.4,by=0.1), 10)[1:length(d$quarter)]
  }
  
  # Scale AMT and CNT data into £ million and 1000s for volumes (to have same items as pay.uk data)
  d$amt<-d$amt*10**(-6)
  d$cnt<-d$cnt/1000
  
  # Add variables on average value per transaction
  d$avg_amt<-d$amt/d$cnt
  d$avg_bacs<-d$`Bacs Value of Items (£m) Total`/d$`Bacs Number of Items (000s) Total`
  d$avg_fps<-d$`FPS Value of Items (£m) Total`/d$`FPS Number of Items (000s) Total`
  d$avg_chaps<-d$`CHAPS Value of Items (£m) Total`/d$`CHAPS Number of Items (000s) Total`
  d$avg_bacs_direct_debit<-d$`Bacs Value of Items (£m) Direct Debits`/d$`Bacs Number of Items (000s) Direct Debits`
  d$avg_bacs_direct_credit<-d$`Bacs Value of Items (£m) All Credits(a, b)`/d$`Bacs Number of Items (000s) Direct Credits`
  d$avg_ic<-d$`Image Clearing Value of Items (£m) Total`/d$`Image Clearing Number of Items (000s) Total`

  # Ensure numeric data type
  d[,2:ncol(d)]<-as.data.frame(sapply(d[2:ncol(d)],as.numeric))
  
  # Plot only data in levels (not growth rates)
  dlevels0<-d[,!str_detect(colnames(d), "Growth|% change")]
  
  for(dt_transform in c("scaled", "levels", "index2015=100")){
    dlevels<-dlevels0
    # Make data transformation (z-scaled data, index data setting 2015=100, raw data in levels)
    if(dt_transform == "scaled"){
      # Scale data (subtract column mean and divide by standard deviation)
      dlevels[,2:ncol(dlevels)]<-apply(dlevels[,2:ncol(dlevels)], 2, scale)
    }else if(dt_transform == "index2015=100"){
      dlevels[,2:ncol(dlevels)]<-apply(dlevels[,2:ncol(dlevels)], 2, FUN = function(x){return(100*x/x[min(which(!is.na(x) & x != 0))])})
    }else if(dt_transform == "levels"){
      #dlevels[,2:ncol(dlevels)]<-apply(dlevels[,2:ncol(dlevels)], 2, FUN = function(x){return(100*x/x[min(which(!is.na(x) & x != 0))])})
    }
    
    # Reshape data for plotting
    dt0<-melt(dlevels, id.vars = tm)
    colnames(dt0)[1]<-"time"
    dt0$time<-as.character(dt0$time)
    
    # List of variables to be plot against amt and cnt
    vars0<-as.character(unique(dt0$variable))
    vars0<-vars0[!(vars0 %in% c("amt", "cnt"))]
    # Set colors for plotting
    clrs<-c("red", "blue", rep("black", length(vars0)))
    names(clrs)<-c("amt", "cnt", vars0)
    for(Payment_var in list("amt", "cnt", c("amt","cnt"))){
      # Set list to be plotted against Payment variable
      vars<-c(c("amt","cnt")[!(c("amt","cnt") %in% Payment_var)], vars0)
      plts<-list()
      for(v0 in 1:length(vars)){
        v<-vars[v0]
        plts[[v0]]<-list()
        dt<-dt0[dt0$variable %in% c(Payment_var, v),]
        p0<-ggplot(dt, aes(x=time, y=value, color=variable, group=variable)) + 
          geom_line() + theme_bw() + ylab("") +
          theme(legend.position = "none", title = element_text(size=6), axis.text.x = element_text(size=3), plot.title = element_text(hjust = 0.5)) + 
          ggtitle(paste0(v)) + scale_color_manual(values = clrs)
        #theme(legend.position = "top", legend.title = element_blank())
        plts[[v0]]<-p0
      } # v0 in 1:length(vars)
      m<-marrangeGrob(plts, nrow=3, ncol=4)
      print(paste(Payment_var, dt_transform, tm))
      ggsave(paste0(plotdir, paste(Payment_var, collapse = "_"), "_vs_external_macro_3x4_",tm,"_", dt_transform, ".pdf"), m, width = 12, height = 9)
      
      rm(m, p0, plts, dt, v, v0)
      # Create a few other plots
      other_plots<-list(Average_values=paste0("avg_",c("amt","bacs","fps","chaps","ic")), 
                        Average_values_bacs=paste0("avg_",c("amt","bacs_direct_debit","bacs_direct_credit")), 
                        Volumes_bacs=c("cnt", paste("Bacs Number of Items (000s)", c("Direct Credits", "Direct Debits"))), 
                        All_retail_schemes_volumes=c("cnt", paste0(c("Bacs", "CHAPS", "FPS", "Image Clearing"), " Number of Items (000s) Total")), 
                        All_retail_schemes_values=c("amt", paste0(c("Bacs", "CHAPS", "FPS", "Image Clearing"), " Value of Items (£m) Total"))
      )
      clrs2<-c(amt="red", avg_amt="red", cnt="blue", avg_bacs="green", avg_bacs_direct_credit="darkgreen", avg_bacs_direct_debit="chartreuse", 
              avg_chaps="darkmagenta", avg_fps="cyan2", avg_ic="chocolate4", 
              rep(c("blue", "green", "darkmagenta", "cyan2", "chocolate4"),2), c("darkgreen", "chartreuse"))
      names(clrs2)[10:21]<-c(c("cnt", paste0(c("Bacs", "CHAPS", "FPS", "Image Clearing"), " Number of Items (000s) Total")), 
      c("amt", paste0(c("Bacs", "CHAPS", "FPS", "Image Clearing"), " Value of Items (£m) Total"), paste("Bacs Number of Items (000s)", c("Direct Credits", "Direct Debits"))))
      
      plts<-list()
      for(v1 in 1:length(other_plots)){
        v<-other_plots[[v1]]
        plts[[(v1)]]<-list()
        dt<-dt0[dt0$variable %in% v,]
        p0<-ggplot(dt, aes(x=time, y=value, color=variable, group=variable)) + 
          geom_line() + theme_bw() + ylab("") +
          theme(legend.position = "bottom", title = element_text(size=6), axis.text.x = element_text(size=3), plot.title = element_text(hjust = 0.5)) + 
          ggtitle(paste0(names(other_plots)[v1])) + scale_color_manual(values = clrs2) 
        p0<-p0 + geom_vline(xintercept = covid, color="black", linetype="dotted")
        
        #theme(legend.position = "top", legend.title = element_blank())
        plts[[(v1)]]<-p0
      } # v0 in 1:length(vars)
      m<-marrangeGrob(plts, nrow=1, ncol=1)
      ggsave(paste0(plotdir, paste0("Other_plots_on_payment_systems_",tm,"_", dt_transform, ".pdf")), m, width = 12, height = 9)
      
      
      
    } # Payment_var in amt, cnt
    rm(Payment_var, vars, vars0, dt0)
  } # for dt_transform in scaled, levels, index 2015=100
  rm(dt_transform)
  
  # Write a few numbers to file 
  if(tm == "year"){
    benchmarking_years<-c("2019", "2021")
    fn<-paste0(plotdir, "benchmark_statistics_overview_", paste(benchmarking_years,collapse = "_"), ".txt")
    vars<-c("year", "amt", "cnt", "Bacs Number of Items (000s) Total", "Bacs Value of Items (£m) Total", 
            "CHAPS Number of Items (000s) Total", "CHAPS Value of Items (£m) Total", 
            "FPS Number of Items (000s) Total", "FPS Value of Items (£m) Total", 
            "Image Clearing Number of Items (000s) Total", "Image Clearing Value of Items (£m) Total", 
            "Total Retail Payments (excl. Cards) Number of Items (000s)", "Total Retail Payments (excl. Cards) Value of Items (£m)", 
            "GDP at market prices: Current prices", "GDP at market prices: Volume measure")
    d1<-d[d$year %in% c(benchmarking_years),vars]
    dvolumes<-d1$cnt/d1[,str_detect(colnames(d1), "Number|Volume")]
    dvalues<-d1$amt/d1[,str_detect(colnames(d1), "Value|Current")]
    # Calculate avg value per item
    davg<-data.frame(cbind(amt=d1$amt/d1$cnt, d1[,str_detect(colnames(d1), "Value|Current")]/d1[,str_detect(colnames(d1), "Number|Volume")]))
    colnames(dvolumes)<-paste("cnt as share of", str_split(colnames(dvolumes), " ", simplify = T)[,1])
    colnames(dvalues)<-paste("amt as share of", str_split(colnames(dvalues), " ", simplify = T)[,1])
    colnames(davg)<-paste("Average value", str_split(colnames(davg), "\\.", simplify = T)[,1])
    rownames(dvolumes)<-rownames(dvalues)<-rownames(davg)<-benchmarking_years
    write(paste("Overview: AMT (amount data from Payment) and pay.UK data on different payment schemes (all instruments) and GDP data from ONS. Data for the benchmark periods", paste(benchmarking_years, collapse = ", "), "\n"), file = fn, append = F)
    capture.output(d1[,str_detect(colnames(d1), "year|amt|Value|Current")], file=fn, append = T)
    write(paste("\n\n\nOverview: CNT (count data from Payment) and pay.UK data on different payment schemes (all instruments) and GDP data from ONS. Data for the benchmark periods", paste(benchmarking_years, collapse = ", "), "\n"), file = fn, append = T)
    capture.output(d1[,str_detect(colnames(d1), "year|cnt|Number|Volume")], file=fn, append = T)
    write(paste("\n\n\nOverview: CNT (count data from Payment) as a share of volumes of pay.UK data on different payment schemes (all instruments) and as a share of GDP volumes from ONS data. Data for the benchmark periods", paste(benchmarking_years, collapse = ", "), "\n"), file = fn, append = T)
    capture.output(dvolumes, file=fn, append = T)
    write(paste("\n\n\nOverview: AMT (amount data from Payment) as a share of values (£m) of pay.UK data on different payment schemes (all instruments) and as a share of GDP volumes from ONS data. Data for the benchmark periods", paste(benchmarking_years, collapse = ", "), "\n"), file = fn, append = T)
    capture.output(dvalues, file=fn, append = T)
    write(paste("\n\n\nOverview: Average values of transactions in Payment data, Pay.UK data, and ONS GDP values per volumes. Data for the benchmark periods", paste(benchmarking_years, collapse = ", "), "\n"), file = fn, append = T)
    capture.output(davg, file=fn, append = T)
    rm(dvolumes, dvalues, davg, d1)
  }

    
  
} # tm in month, quarter, year

rm(tm)

