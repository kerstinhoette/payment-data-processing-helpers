# This script makes a few explorative regression analyses using the industry planel data


# Clean memory
rm(list=ls())
.rs.restartR()

library(plm); library(collapse); library(stringr); library(stargazer); library(broom)

# Load helper functions
source("code/network_analysis/explorative_regressions/helpers/functions.r")

# Choose between 2-digit CPA or A64 codes which are compatible with data from all time periods. 
# (IOT PxP from ONS uses A64 classification of products which is more aggregate than CPA 2-digit)
aggr<-"A16"
aggr<-"CPA"

outputdir<-paste0("statistical_output/",aggr,"/explorative_regressions/")
if(!dir.exists(outputdir)){dir.create(outputdir,recursive = T)}

# Load data (both network and panel)
load(paste0("Payment_data/Mar_data/national_account_data/industry_level_panel_Payment_ONS_",aggr,".RData"))
dt$outputs_minus_inputs_amt<-dt$sum_outputs_amt-dt$sum_inputs_amt
dt$outputs_minus_inputs_cnt<-dt$sum_outputs_amt-dt$sum_inputs_cnt
dt$outputs_minus_inputs_SUT<-dt$sum_outputs_SUT-dt$sum_inputs_SUT
load(paste0("Payment_data/Mar_data/national_account_data/IOT_edgelist_flow_of_goods_ONS_Payment_",aggr,".RData"))
df<-edgelist_wide_to_long(df)
df_is<-get_input_output_shares(df, "input")
df_os<-get_input_output_shares(df, "output")
yrs<-2020

pdf(paste0(outputdir,"Plots_input_output_shares_Payment_ONS_",paste(unique(c(yrs[1],yrs[length(yrs)])),collapse="_"),".pdf"))
rng<-c(0,1)
plot(df_os$SUT[df_os$year%in%yrs], df_is$SUT[df_is$year%in%yrs], col="blue", xlab="Output shares", ylab="Input shares",xlim=rng,ylim=rng)
points(df_os$AMT[df_os$year%in%yrs], df_is$AMT[df_is$year%in%yrs], col="red")
abline(0,1)
legend("topleft", c("ONS SUT", "Payment AMT"), col=c("blue", "red"), pch=1)
dt1<-dt[dt$year%in%yrs,]

rng<-range(c(dt1$sum_inputs_amt,dt1$sum_inputs_SUT,dt1$sum_outputs_amt,dt1$sum_outputs_SUT),na.rm = T)
plot((dt1$sum_inputs_SUT), (dt1$sum_outputs_SUT), col="blue", xlab="Sum of outputs", ylab="Sum of inputs",xlim = rng, ylim=rng)
points((dt1$sum_inputs_amt), (dt1$sum_outputs_amt), col="red")
abline(0,1)
legend("topleft", c("ONS SUT", "Payment AMT"), col=c("blue", "red"), pch=1)

rng<-c(log(dt1$sum_inputs_amt),log(dt1$sum_inputs_SUT),log(dt1$sum_outputs_amt),log(dt1$sum_outputs_SUT))
rng<-range(rng[!is.infinite(rng)],na.rm = T)
plot(log(dt1$sum_inputs_SUT), log(dt1$sum_outputs_SUT), col="blue", xlab="Log sum of outputs", ylab="Log sum of inputs",xlim = rng, ylim=rng)
points(log(dt1$sum_inputs_amt), log(dt1$sum_outputs_amt), col="red")
abline(0,1)
legend("topleft", c("ONS SUT", "Payment AMT"), col=c("blue", "red"), pch=1)

dt1$io_ratio_amt<-dt1$sum_inputs_amt/dt1$sum_outputs_amt
dt1$io_ratio_SUT<-dt1$sum_inputs_SUT/dt1$sum_outputs_SUT
dt1$io_ratio_amt_log<-log(dt1$sum_inputs_amt)/log(dt1$sum_outputs_amt)
dt1$io_ratio_SUT_log<-log(dt1$sum_inputs_SUT)/log(dt1$sum_outputs_SUT)

rng<-range(c(dt1$io_ratio_amt_log,dt1$io_ratio_SUT_log),na.rm = T)
plot(dt1$io_ratio_amt_log,dt1$io_ratio_SUT_log, main="Input-output ratio of logs: Log sum of inputs divided by log sum of outputs", xlab = "Payment AMT", ylab = "ONS SUT",xlim = rng, ylim=rng)
abline(0,1)

rng<-c(dt1$io_ratio_amt,dt1$io_ratio_SUT)
rng<-range(rng[!is.infinite(rng)],na.rm = T)
plot(dt1$io_ratio_amt,dt1$io_ratio_SUT, main="Input-output ratio: Sum of inputs divided by sum of outputs", xlab = "Payment AMT", ylab = "ONS SUT",xlim = rng, ylim=rng)
abline(0,1)

rng<-c(log(1+dt1$io_ratio_amt),log(1+dt1$io_ratio_SUT))
rng<-range(rng[!is.infinite(rng)],na.rm = T)
plot(log(1+dt1$io_ratio_amt),log(1+dt1$io_ratio_SUT), main="Log(1+input-output ratio)", sub="",xlab = "Payment AMT", ylab = "ONS SUT",xlim = rng, ylim=rng)
abline(0,1)

dev.off()

# Here, look only at SUTs
dt<-dt[,c("code_year","code","year","name",colnames(dt)[str_detect(colnames(dt),"_amt|_cnt|SUT")])]
df<-df[,c("year","to","from",colnames(df)[str_detect(colnames(df),"AMT|CNT|SUT")])]
df_is<-df_is[,c("year","to","from",colnames(df_is)[str_detect(colnames(df_is),"AMT|CNT|SUT")])]
df_os<-df_os[,c("year","to","from",colnames(df_os)[str_detect(colnames(df_os),"AMT|CNT|SUT")])]

# Add up- and downstream spillovers
spill_up<-get_network_spillovers(df_is,dt,type="growth",direction="upstream")
spill_down<-get_network_spillovers(df_os,dt,type="growth",direction="downstream")
spill_up$code_year<-paste0(spill_up$code,"_",spill_up$year)
spill_down$code_year<-paste0(spill_down$code,"_",spill_down$year)
dt<-merge(dt, spill_up[!(colnames(spill_up)%in%c("code","year"))], by="code_year",all=T)
dt<-merge(dt,spill_down[!(colnames(spill_down)%in%c("code","year"))], by="code_year",all=T)
rm(spill_up, spill_down)

# Add input/output share data to network data
df$from_to_year<-paste0(df$from,"_",df$to,"_",df$year);df_is$from_to_year<-paste0(df_is$from,"_",df_is$to,"_",df_is$year);df_os$from_to_year<-paste0(df_os$from,"_",df_os$to,"_",df_os$year) 
colnames(df_is)[colnames(df_is)%in%c("AMT","CNT","SUT")]<-paste0("input_shares_",colnames(df_is)[colnames(df_is)%in%c("AMT","CNT","SUT")])
colnames(df_os)[colnames(df_os)%in%c("AMT","CNT","SUT")]<-paste0("output_shares_",colnames(df_os)[colnames(df_os)%in%c("AMT","CNT","SUT")])
df<-merge(df,df_is[,c("input_shares_AMT","input_shares_CNT","input_shares_SUT","from_to_year")],by="from_to_year",all=T)
df<-merge(df,df_os[,c("output_shares_AMT","output_shares_CNT","output_shares_SUT","from_to_year")],by="from_to_year",all=T)
rm(df_is,df_os)

# Add industry level variables to network data (at node level)
dt1<-dt[,!(colnames(dt)%in%c("year","name","code"))]
colnames(dt1)[colnames(dt1) != "code_year"]<-paste0("to_",colnames(dt1)[colnames(dt1) != "code_year"])
df$code_year<-paste0(df$to,"_",df$year)
df<-merge(df,dt1,by="code_year",all=T)
colnames(dt1)[colnames(dt1) != "code_year"]<-paste0("from_",substr(colnames(dt1)[colnames(dt1) != "code_year"],4, 100))
df$code_year<-paste0(df$from,"_",df$year)
df<-merge(df,dt1,by="code_year",all=T)
rm(dt1)
df<-df[,colnames(df)!="code_year"]
df<-df[(!is.na(df$from)&!is.na(df$to)),]
df$from_to<-paste0(df$from,"_",df$to)
colnames(df)<-str_replace_all(colnames(df)," ","_")
colnames(dt)<-str_replace_all(colnames(dt)," ","_")
pdf<-pdata.frame(df,index = c("from_to","year"))
pdf[,!(colnames(pdf)%in%c("from","to","from_to_year","year","from_to"))]<-apply(pdf[,!(colnames(pdf)%in%c("from","to","from_to_year","year","from_to"))],2,FUN=function(x){return(as.numeric(as.character(x)))})
# Transform industry raw data to panel
pd<-pdata.frame(dt, index=c("code","year"))


# Add variables to create industry subsets
pd$import_share_GO<-pd$Total_imports_of_goods_and_services_SUT/pd$Total_output_at_basic_prices_SUT
pd$export_share_TD<-pd$Total_exports_of_goods_and_services_SUT/pd$Total_demand_for_products_SUT
pd$import_share_GVA<-pd$Total_imports_of_goods_and_services_SUT/pd$Gross_valued_added_at_basic_prices_SUT
pd$export_share_GVA<-pd$Total_exports_of_goods_and_services_SUT/pd$Gross_valued_added_at_basic_prices_SUT
pd$trade_share_GVA<-(pd$Total_exports_of_goods_and_services_SUT + pd$Total_imports_of_goods_and_services_SUT)/pd$Gross_valued_added_at_basic_prices_SUT


obj_variables<-c("sum_inputs_SUT","sum_outputs_SUT","Gross_valued_added_at_basic_prices_SUT","Total_exports_of_goods_and_services_SUT","SUT", "SUT_input_shares","SUT_output_shares")
obj<-obj_variables[1]

fntex<-paste0(outputdir,"Regression_outputs.tex")
write(paste0("\\section{Exploratory regression analyses}"), fntex,append=F)

# Set lag for lagged variables
lg<-1


#for(industry_subset in c("all","low_trade_intensity","high_"))
for(obj in obj_variables){ # Run regression on node (industry) and link level, inputs/outputs
  
  fn<-paste0(outputdir,"Regressions_",obj,".txt")
  write(paste0(Sys.time()),fn,append=F)
  write(paste0("\\FloatBarrier \n \\subsection{Predicting ",str_replace_all(str_to_title(obj),"_"," "),"}"),fntex,append=T)
  #write(paste0("\\FloatBarrier \n \\subsubsection{",industry_subset,"}"),fntex,append=T)
  
  
    
  if(obj %in% c("Gross_valued_added_at_basic_prices_SUT")){Payment_amt<-"outputs_minus_inputs_amt"; Payment_cnt<-"outputs_minus_inputs_cnt"
  }else if(obj %in% c("Total_exports_of_goods_and_services_SUT")){Payment_amt<-"sum_outputs_amt"; Payment_cnt<-"sum_outputs_cnt"
  }else{Payment_amt<-str_replace(obj,"SUT","amt"); Payment_cnt<-str_replace(obj,"SUT","cnt")}
  
  if(obj%in%c("SUT", "SUT_input_shares","SUT_output_shares")){
    d0<-pdf; Payment_amt<-str_replace(obj,"SUT","AMT"); Payment_cnt<-str_replace(obj,"SUT","CNT")
    d0[,!(colnames(d0)%in%c("from","to","from_to_year","year","from_to"))]<-apply(d0[,!(colnames(d0)%in%c("from","to","from_to_year","year","from_to"))],2,FUN=function(x){return(as.numeric(as.character(x)))})
    spillvars<-paste0(rep(c("from_","to_"),each=6),rep(c("AMT","CNT","SUT"),4),rep("_spill_",12),rep(c("up","down","up","down"),each=3),rep("stream",12))
    
  }else{
    #d0<-pd; Payment_amt<-str_to_lower(Payment_amt); Payment_cnt<-str_to_lower(Payment_cnt)
    d0<-pd; Payment_amt<-Payment_amt; Payment_cnt<-Payment_cnt
    spillvars<-paste0(rep(c("AMT","CNT","SUT"),2),rep("_spill_",6),rep(c("up","down"),each=3),rep("stream",6))
  }
  
  cls<-colnames(d0)[!(colnames(d0)%in%c("code_year","code","year","from","to","name","from_to","from_to_year"))] # get numeric columns to be transformed into growth rates and/or logs
  if(str_detect(obj, "growth_rates")){d0[,cls]<-G(d0[,cls])}
  if(str_detect(obj, "log")){d0[,cls]<-log(1+d0[,cls])}
  # Make sure infinites and NaNs are set to NA
  d0[,cls]<-as.data.frame(apply(as.matrix(d0[,cls]),2,FUN=function(x){return(ifelse((is.infinite(x)|is.nan(x)),NA,x))}))
  # Possible extensions: remove outliers, but maybe after having determined control variables? 
  # Things to test: 
  # - Subset industries by trade intensity
  # - subset industries with neg and positive growth rate correlation
   
  # Set control variables
  cntrls<-c("Compensation of employees SUT","Gross fixed capital formation SUT","Total exports of goods and services SUT","Total imports of goods and services SUT","Distributors Trading Margins SUT")
  cntrls<-str_replace_all(cntrls," ","_")
  if(sum(str_detect(colnames(d0)[1:5],"from_"))>0){cntrls<-paste0(rep(c("from_","to_"),length(cntrls)),rep(cntrls,each=2))}
  cntrls<-str_replace_all(cntrls," ","_")
  colnames(d0)<-str_replace_all(colnames(d0),"\\.","_")
  colnames(d0)<-str_replace_all(colnames(d0),"__","_")
    
  # Set list of models to be run in regressions
  models<-list(
    simple_correlation_AMT=paste0(obj, " ~ ", Payment_amt),
    simple_correlation_CNT=paste0(obj, " ~ ", Payment_cnt),
    AMT_with_autocorrelation_lag2=paste0(obj, " ~ ", Payment_amt," + lag(",obj,", ",lg,")"), 
    CNT_with_autocorrelation_lag2=paste0(obj, " ~ ", Payment_cnt," + lag(",obj,", ",lg,")"), 
    AMT_CNT_with_autocorrelation_lag2=paste0(obj, " ~ ",Payment_amt," + ",Payment_cnt," + lag(",obj,", ",lg,")"), 
    AMT_CNT_inputs_outputs=paste0(obj, " ~ sum_inputs_amt + sum_outputs_amt + sum_inputs_cnt + sum_outputs_cnt"), 
    AMT_CNT_inputs_outputs_lag2=paste0(obj, " ~ sum_inputs_amt + sum_outputs_amt + lag(sum_inputs_SUT, ",lg,") + lag(sum_outputs_SUT, ",lg,")"), 
    simple_correlation_AMT_spill=paste0(obj, " ~ ", Payment_amt," + ",paste0(spillvars[str_detect(spillvars,"AMT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")),
    simple_correlation_CNT_spill=paste0(obj, " ~ ", Payment_cnt," + ",paste0(spillvars[str_detect(spillvars,"CNT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")),
    AMT_with_autocorrelation_lag2_spill=paste0(obj, " ~ ", Payment_amt," + lag(",obj,", ",lg,")"," + ", paste0(spillvars[str_detect(spillvars,"AMT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")),
    CNT_with_autocorrelation_lag2_spill=paste0(obj, " ~ ", Payment_cnt," + lag(",obj,", ",lg,")"," + ",paste0(spillvars[str_detect(spillvars,"CNT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")),
    AMT_CNT_with_autocorrelation_lag2_spill=paste0(obj, " ~ ",Payment_amt," + ",Payment_cnt," + lag(",obj,", ",lg,")"," + ", paste0(spillvars[str_detect(spillvars,"AMT|CNT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")),
    AMT_CNT_inputs_outputs_spill=paste0(obj, " ~ sum_inputs_amt + sum_outputs_amt + sum_inputs_cnt + sum_outputs_cnt"," + ",paste0(spillvars[str_detect(spillvars,"AMT|CNT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")), 
    AMT_CNT_inputs_outputs_lag2_spill=paste0(obj, " ~ sum_inputs_amt + sum_outputs_amt + lag(sum_inputs_SUT, ",lg,") + lag(sum_outputs_SUT, ",lg,")"," + ",paste0(spillvars[str_detect(spillvars,"AMT|CNT")])," + ",paste0("lag(",spillvars[str_detect(spillvars,"SUT")],", ",lg,")")) 
  )
    
  if(obj%in%c("SUT", "SUT_input_shares","SUT_output_shares")){models<-models[!str_detect(names(models),"inputs_outputs")]}
  m1<-5
  # Now: Run regressions through different model specifications
  for(m1 in 1:length(models)){
    
    m<-as.formula(models[[m1]])
    
    write(paste0("\n\n\n",paste(m)),file=fn,append=T)
    
    p<-plm(m,data=d0, vcov=vcovSCC)
    #capture.output(summary(p,robust=T,vcov=vcovSCC),file=fn,append=T)
    p2<-plm(m,data=d0, effect = "individual", model = "within", vcov=vcovSCC)
    #capture.output(summary(p2,robust=T, vcov=vcovSCC),file=fn,append=T)
    p3<-plm(m,data=d0, effect = "time", model = "within", vcov=vcovSCC)
    #capture.output(summary(p3,robust=T, vcov=vcovSCC),file=fn,append=T)
    p4<-plm(m,data=d0, effect = "twoways", model = "within", vcov=vcovSCC)
    #capture.output(summary(p4,robust=T, vcov=vcovSCC),file=fn,append=T)
    p5<-plm(m,data=d0, effect = "individual", model = "fd", vcov=vcovSCC)
    #capture.output(summary(p5,robust=T, vcov=vcovSCC),file=fn,append=T)
    
    colttls<-c("No FE", "industry FE", "time FE", "industry-time FE", "first difference")
    notestxt<-c("The regressions are run using the R-package plm. Standard errors are", "clustered using vcov=vcovSCC to deal with serial and cross-sectional", "correlation (\\url{https://rdrr.io/cran/plm/man/vcovSCC.html}).")
    write(stargazer(p,p2,p3,p4,p5, column.labels = colttls, align = F, model.names = T, notes = notestxt,  notes.align = "l", font.size = "scriptsize", column.sep.width = "0.1pt", keep.stat = c("adj.rsq","rsq","n","bic"), intercept.bottom = F), file = fntex, append = T)
    
    m<-as.formula(paste(models[[m1]], "+", paste0("lag(",cntrls,",",lg,")",collapse = " + ")))
    p<-plm(m,data=d0)
    #capture.output(summary(p,robust=T),file=fn,append=T)
    p2<-plm(m,data=d0, effect = "individual", model = "within", vcov=vcovHC)
    #capture.output(summary(p2,robust=T, vcov=vcovHC),file=fn,append=T)
    p3<-plm(m,data=d0, effect = "time", model = "within", vcov=vcovSCC)
    #capture.output(summary(p3,robust=T, vcov=vcovHC),file=fn,append=T)
    p4<-plm(m,data=d0, effect = "twoways", model = "within", vcov=vcovHC)
    #capture.output(summary(p4,robust=T, vcov=vcovHC),file=fn,append=T)
    p5<-plm(m,data=d0, effect = "individual", model = "fd", vcov=vcovHC)
    #capture.output(summary(p5,robust=T, vcov=vcovHC),file=fn,append=T)
    
    colttls<-c("No FE", "industry FE", "time FE", "industry-time FE", "first difference")
    
    write(stargazer(p,p2,p3,p4,p5, column.labels = colttls, align = F, model.names = T, notes = notestxt, notes.align = "l", font.size = "scriptsize", column.sep.width = "0.1pt", keep.stat = c("adj.rsq","rsq","n","bic"), intercept.bottom = F), file = fntex, append = T)
    
    
  } # m in models
  
  

} # obj in objective variables












p<-plm(m,data = pd)
summary(p)
p<-plm(m,data = pd, effect = "individual", model = "within")
summary(p)
p<-plm(m,data = pd, effect = "time", model="within")
summary(p)
p<-plm(m,data = pd, effect = "twoways", model="within")
summary(p)
p<-plm(m,data = pd, effect = "individual", model="fd")
summary(p)

# Now, use lagged data
m<-as.formula("sum_outputs_SUT ~ sum_outputs_amt + sum_inputs_amt + sum_outputs_cnt + sum_inputs_cnt + lag(sum_outputs_SUT,1)")
m<-as.formula("log(1+sum_outputs_SUT) ~ log(1+sum_outputs_amt) + log(1+lag(sum_outputs_SUT,1))")
m<-as.formula("log(1+sum_outputs_SUT) ~ log(1+sum_outputs_amt) + log(1+lag(sum_outputs_SUT,1)) + lag(AMT_spill_upstream,1) + lag(AMT_spill_downstream,1) + lag(SUT_spill_upstream,1) + lag(SUT_spill_downstream,1)")
p<-plm(m,data = pd)
summary(p)
p<-plm(m,data = pd, effect = "individual", model = "within")
summary(p)
p<-plm(m,data = pd, effect = "time", model="within")
summary(p)
p<-plm(m,data = pd, effect = "twoways", model="within")
summary(p)
p<-plm(m,data = pd, effect = "individual", model="fd")
summary(p)

# Now, use 2-year lagged data (2 years are the publication delay in SUTs)
m<-as.formula("sum_outputs_SUT ~ sum_outputs_amt + lag(sum_outputs_SUT, ",lg,")")
m<-as.formula("log(1+sum_outputs_SUT) ~ log(1+sum_outputs_amt) + log(1+lag(sum_outputs_SUT, ",lg,"))")
m<-as.formula("log(1+sum_outputs_SUT) ~ log(1+sum_outputs_amt) + log(1+lag(sum_outputs_SUT, ",lg,")) + AMT_spill_upstream + AMT_spill_downstream + lag(SUT_spill_upstream, ",lg,") + lag(SUT_spill_downstream, ",lg,")")

p<-plm(m,data = pd)
summary(p)
p<-plm(m,data = pd, effect = "individual", model = "within")
summary(p)
p<-plm(m,data = pd, effect = "time", model="within")
summary(p)
p<-plm(m,data = pd, effect = "twoways", model="within")
summary(p)
p<-plm(m,data = pd, effect = "individual", model="fd")
summary(p)

#mb<-as.formula("log(1+sum_outputs_SUT) ~ log(1+sum_outputs_amt) + log(1+lag(sum_outputs_SUT,1)) | log(1+lag(sum_outputs_SUT, ",lg,"))")
mb<-as.formula("sum_outputs_SUT ~ sum_outputs_amt + lag(sum_outputs_SUT, ",lg,") + AMT_spill_upstream + AMT_spill_downstream + lag(SUT_spill_upstream, ",lg,") + lag(SUT_spill_downstream, ",lg,") | lag(sum_outputs_SUT,2:99)")
p<-pgmm(mb, data = pd, effect = "twoways", model = "onestep", transformation = "d")
p<-pgmm(mb, data = pd, effect = "individual", model = "twostep", transformation = "ld")
summary(p)


