rm(list=ls())
.rs.restartR()
#library(corrplot); library(ggplot2); library(stringr); library(plm); library(zoo); library(collapse); library(reshape2); 
library(stargazer); library(plm); library(stringr); library(zoo)
source("code/helpers.r")

dgt<-5
aggr<-"A16"
aggr<-"CPA_gdp_data"
aggr<-"CPA_public"
dtversion<-"Dec2023_public"
if(dtversion=="Dec2023_public"){aggr<-"CPA_public"}
dtdir<-paste0("Payment_data/",dtversion,"/national_account_data/",paste0(dgt,"_digit_data/")[dtversion!="Dec2023_public"])

# Capture GDP correlations between industry level deseasonalised GVA and GDP
# Get average value for all releases, and focus on input value data

# Load data
load(paste0(dtdir,"industry_panel_Payment_ONS_monthly_",aggr,".RData"))
#dt$name<-shorten_name(dt$name)
dt$sum_inputs_amt[is.na(dt$sum_inputs_amt)]<-0
dt$sum_outputs_amt[is.na(dt$sum_outputs_amt)]<-0
dt$sum_input_output_amt<-ifelse(is.na(dt$sum_inputs_amt),0,dt$sum_inputs_amt) + ifelse(is.na(dt$sum_outputs_amt),0,dt$sum_outputs_amt)
dt$name<-str_replace_all(dt$name," &",",")
dt$code<-str_replace_all(dt$code," &",", ")

# Keep only relevant subperiod
dt<-dt[str_detect(as.character(dt$time),paste0(2016:2023,collapse = "|")),]
dt$time<-as.yearmon(dt$time)
dt<-dt[order(dt$time),]

# Add growth rates
dt<-pdata.frame(dt, index=c("code", "time"), drop.index = F)
x<-G(dt,12)
colnames(x)<-str_replace_all(colnames(x),paste0("L12G1\\."),"Growth ")
colnames(x)<-str_replace_all(colnames(x),"\\."," ")
dt<-merge(dt, x, by=c("code","time"), all=T)
rm(x)
colnames(dt)<-str_replace_all(colnames(dt),"\\."," ")
dt[,!(colnames(dt)%in%c("code","time","name"))]<-apply(dt[,!(colnames(dt)%in%c("code","time","name"))],2,FUN=function(x){return(ifelse((is.nan(x)|is.infinite(x)),NA,x))})

# Add one-month lagged data
dt<-pdata.frame(dt, index=c("code", "time"), drop.index = F)
x<-L(dt,1)
colnames(x)<-str_replace_all(colnames(x),"L1","Lag")
colnames(x)<-str_replace_all(colnames(x),"\\."," ")
x$time<-as.yearmon(x$time)
dt<-merge(dt, x, by=c("code","time"), all=T)
rm(x)
colnames(dt)<-str_replace_all(colnames(dt),"\\."," ")
dt[,!(colnames(dt)%in%c("code","time","name"))]<-apply(dt[,!(colnames(dt)%in%c("code","time","name"))],2,FUN=function(x){return(ifelse((is.nan(x)|is.infinite(x)),NA,x))})

# Get list of available GVA releases
vs<-unique(str_split(colnames(dt)[str_detect(colnames(dt),"GVA sa")], "_", simplify = T)[,2])

# Choose correlation method
m<-"pearson"

plotdir<-paste0("statistical_output/nowcasting_descriptives/correlations/")
if(!dir.exists(plotdir)){dir.create(plotdir,recursive = T)}

dt0<-dt

cors_overview<-data.frame(code=unique(paste(dt0$code,dt0$name)))
for(same_period in c(T,F)){
  fn<-paste0(plotdir,"Correlation_overview_tables_industry_level","_same_period"[same_period],".tex")
  write("",file=fn)
  if(same_period){
    dt<-dt0[dt0$time %in% dt0$time[!is.na(dt0$`GVA sa_dec2021`)],]
  }
  for(lag in c(T,F)){
    for(growth_rate in c(T,F)){
      for(p in c("sum_inputs_amt", "sum_outputs_amt", "sum_input_output_amt")){
        
        if(growth_rate){
          p<-paste0("Growth ",p)
        }
        
        # Create empty data frame to store all correlation levels
        cors<-data.frame(code=unique(dt$code))
        
        # Iterate through all releases
        for(v in vs){
          
          # Keep only relevant columns
          g<-paste0("Lag "[lag],"Growth "[growth_rate], "GVA sa_",v)
          d<-dt[,c("code",p,g)]
          
          # Calculate correlations for each industry
          x<-sapply(split(d, as.factor(paste(d$code))), function(X){cor(X[,p], X[,g], use ="pairwise.complete.obs",method = m)})
          
          # Add values to data frame
          cors$add<-x[match(cors$code, names(x))]
          colnames(cors)[ncol(cors)]<-paste0(p,"_GVA_",v)
          
          rm(x,d)
        }
        # Add column with aggregate across all releases
        cors$avg_pearson_correlation<-apply(cors[,colnames(cors)!="code"],1,mean)
        # Add column with aggregate across all releases
        cors$variance_of_pearson_correlations<-apply(cors[,colnames(cors)!="code"],1,var)
        
        # Add column with range between min max correlation across releases
        cors$min_max_pearson_correlation<-apply(cors[,!(colnames(cors)%in%c("code"))],1,FUN = function(x){return(max(x, na.rm=T)-min(x,na.rm = T))})
        
        # Add column with range between min max correlation across releases
        cors$range_pearson_correlation<-apply(cors[,!(colnames(cors)%in%c("code","min_max_pearson_correlation"))],1,FUN = function(x){return(paste0(round(min(x),2),";",round(max(x),2)))})
        
        # Cosmetics and customise ordering
        cors$name<-dt$name[match(cors$code,dt$code)]
        cors$code<-paste(cors$code, cors$name)
        cors<-cors[,c("code","avg_pearson_correlation","variance_of_pearson_correlations","range_pearson_correlation","min_max_pearson_correlation",paste0(p,"_GVA_",vs))]
        colnames(cors)<-str_remove_all(colnames(cors),paste0(p,"_GVA_"))
        colnames(cors)<-str_replace_all(colnames(cors),"avg_pearson_correlation","Average")
        colnames(cors)<-str_replace_all(colnames(cors),"variance_of_pearson_correlations","Variance")
        colnames(cors)<-str_replace_all(colnames(cors),"min_max_pearson_correlation","Diff(max-min)")
        colnames(cors)<-str_replace_all(colnames(cors),"range_pearson_correlation","Range")
        cors<-cors[order(cors$Average,decreasing = T),]
        
        if(str_detect(p,"sum_inputs_amt")){plabel<-"input values"
        }else if(str_detect(p,"sum_outputs_amt")){plabel<-"input values"
        }else if(str_detect(p,"sum_input_output_amt")){plabel<-"sum of input and output values"
        }else{plabel<-p}
        
        
        write("\\begin{landscape}",file=fn,append=T)
        capture.output(stargazer(cors, summary = F, digits = 4, title=paste("Pearson correlations between ","growth rates of "[growth_rate],"lagged "[lag], "GVA and ",plabel), label=paste0("tab:pearson_","lag_"[lag],"gva_",p,"_growth"[growth_rate],"_same_period"[same_period]), rownames = F,font.size = "tiny",column.sep.width = "0pt"), file=fn, append = T)
        write("\\end{landscape}",file=fn,append=T)
        
        x<-cors[,c("code","Average","Variance","Diff(max-min)","Range")]
        colnames(x)[2:ncol(x)]<-paste(colnames(x)[2:ncol(x)],"Lag"[lag],p,"same period"[same_period])
        cors_overview<-merge(cors_overview, x, by="code", all=T)
        rm(plabel,cors,x)
      }  # for p in inputs, outputs, sum(inputs,outputs)
    } # for growth in T,F
  } # for lag in T,F
} # for same period in T,F

cors_overview_average<-cors_overview[,str_detect(colnames(cors_overview),"code|Average")]
cors_overview_variance<-cors_overview[,str_detect(colnames(cors_overview),"code|Variance")]
cors_overview_min_max<-cors_overview[,str_detect(colnames(cors_overview),"code|Diff")]
cors_overview_range<-cors_overview[,str_detect(colnames(cors_overview),"code|Range")]


fn<-paste0(plotdir,"summary_same_period.tex")
write("",file=fn)
for(j in c("average","variance","max_min","range")[1:3]){
  if(j=="average"){cors<-cors_overview_average
  }else if(j=="variance"){cors<-cors_overview_variance
  }else if(j=="max_min"){cors<-cors_overview_min_max
  }else if(j=="range"){stop()}
  x0<-data.frame(code=cors_overview$code)
  for(i in c("growth","raw","lag growth","lag")){
    if(i=="lag growth"){
      x<-cors[,str_detect(colnames(cors), "code|Lag Growth")]
    }else if(i=="lag"){
      x<-cors[,(str_detect(colnames(cors), "code|Lag")&!str_detect(colnames(cors), "Growth"))]
    }else if(i=="growth"){
      x<-cors[,(str_detect(colnames(cors), "code|Growth")&!str_detect(colnames(cors), "Lag"))]
    }else if(i=="raw"){
      x<-cors[,(!str_detect(colnames(cors), "Growth|Lag"))]
    }
    x<-x[,(!str_detect(colnames(x),"sum_input_output")&str_detect(colnames(x),"code|same period"))]
    if(ncol(x)!=3){stop("check this")}
    
    if(j=="range"){
      stop()
      x[is.infinite(x[,2]),2]<-NA; x[is.infinite(x[,3]),]<-NA
      x$Min<-min(as.numeric(str_split(x[,2],";",simplify = T)[,1]),as.numeric(str_split(x[,3],";",simplify = T))[,2])
      
    }else{
      x$Average<-(x[,2]+x[,3])/2
      cn<-paste("Average for",j,i,"data")
      colnames(x)[4]<-cn
    }
    x0<-merge(x0,x[,c("code",cn)],by="code",all=T)
  }
  
  x0<-x0[order(x0[,(str_detect(colnames(x0),"growth")&!str_detect(colnames(x0),"lag"))],decreasing = T),]
  if(j=="min_max"){o<-x0$code} # save order to order range data which is non-numeric
  colnames(x0)<-str_remove_all(colnames(x0),paste0(" ",j))
  
  if(j=="min_max"){ttl<-"Average difference between min and max correlations between GVA and input/output time series"
  else{ttl<-paste("Average pearson correlations between GVA and input/output time series ",j)}
  capture.output(stargazer(x0, summary = F, digits = 4, title=ttl, label=paste0("tab:pearson_summary_",j), rownames = F,font.size = "tiny",column.sep.width = "0pt"), file=fn, append = T)
  
}



#
# Explore GDP correlations with and without Covid
# 
# 
# To dos: 
# - Look at energy price crisis
# - Look at quarterly & annual data
# 
# 
# Load macro data
load(paste0(dtdir,"macroeconomic_data_Payment_ONS_month.RData"))
dtm<-d[,str_detect(colnames(d),"time|amt|cnt|GVA sa")]
load(paste0(dtdir,"macroeconomic_data_Payment_ONS_quarter.RData"))
dtq<-d[,str_detect(colnames(d),"time|amt|cnt|GVA sa")]
load(paste0(dtdir,"macroeconomic_data_Payment_ONS_year.RData"))
dty<-d[,str_detect(colnames(d),"time|amt|cnt|GVA sa")]
# Load also industry level data
load(paste0(dtdir,"industry_panel_Payment_ONS_monthly_",aggr,".RData"))
dt$name<-shorten_name(dt$name)
dtmi<-dt
load(paste0(dtdir,"industry_panel_Payment_ONS_quarterly_",aggr,".RData"))
dt$name<-shorten_name(dt$name)
dtqi<-dt
load(paste0(dtdir,"industry_panel_Payment_ONS_yearly_",aggr,".RData"))
dt$name<-shorten_name(dt$name)
dtyi<-dt[,(str_detect(colnames(dt),"time|code|name|amt|cnt|GVA")&(colnames(dt)!="code_year"))]
rm(d, dt)

# Select release version for monthly GDP
vs<-"dec2023"
dtm<-dtm[,str_detect(colnames(dtm),paste0("time|amt|cnt|",vs))]
dtmi<-dtmi[,str_detect(colnames(dtmi),paste0("time|code|name|amt|cnt|nsa|",vs))]
colnames(dtm)<-str_remove_all(colnames(dtm),paste0(" ",vs))
colnames(dtmi)<-str_remove_all(colnames(dtmi),paste0("_",vs))
colnames(dtq)<-str_remove_all(colnames(dtq),paste0(" ",vs))
colnames(dtqi)<-str_remove_all(colnames(dtqi),paste0("_",vs))
colnames(dty)<-str_remove_all(colnames(dty),paste0(" ",vs))
colnames(dtyi)<-str_remove_all(colnames(dtyi),paste0("_",vs))

ONLY_SA<-T

GROWTH_RATES<-F

# Four clusters
FOUR_CLUSTERS<-F

# Cut data after Nov 2021 to have same period for all GDP data series
USE_SAME_SAMPLE<-F
# Look at energy crisis defined by Mar 2022, which is the first month after Russian invasion
ENERGY_CRISIS<-F

for(ENERGY_CRISIS in c(F,T)[1]){
  
  for(GROWTH_RATES in c(F,T)){
    
    if(USE_SAME_SAMPLE && ENERGY_CRISIS){warning("Pre Nov 2021 && Energy crisis time cannot work at the same time; choose energy crisis as cutoff");USE_SAME_SAMPLE<-F}
    
    plotdir<-paste0("statistical_output/GDP_correlations_new/",aggr,"/preNov21_data/"[USE_SAME_SAMPLE],"/postMar22_energy/"[ENERGY_CRISIS],"/growth_rates/"[GROWTH_RATES],"/levels/"[!GROWTH_RATES])
    if(!dir.exists(plotdir)){dir.create(plotdir,recursive = T)}
    
    tm<-"month"
    for(tm in c("month","quarter","year"[!ENERGY_CRISIS])){
      
      if(tm=="month"){dt<-dtm;dti<-dtmi}else if(tm=="quarter"){dt<-dtq;dti<-dtqi}else{dt<-dty;dti<-dtyi}
      
      
      if(ENERGY_CRISIS){
        # Shift year back by one year to obtain the growth rates (not sure if this makes sense in this context...)
        if(GROWTH_RATES){yc<-2021}else{yc<-2022} 
        if(tm=="month"){cutoff<-paste0("Mar ",yc)
        }else if(tm=="quarter"){cutoff<-paste0(yc," Q2")
        }else if(tm=="year"){cutoff<-yc}
        dt<-dt[dt$time>=cutoff,]
        dt<-dt[,!str_detect(colnames(dt)," nsa ")]
        dti<-dti[dti$time>=cutoff,]
        rm(cutoff)
      }else if(USE_SAME_SAMPLE){
        if(tm=="month"){cutoff<-"Nov 2021"
        }else if(tm=="quarter"){cutoff<-"2021 Q3"
        }else if(tm=="year"){cutoff<-2021}
        dt<-dt[dt$time<=cutoff,]
        dti<-dti[dti$time<=cutoff,]
        rm(cutoff)
      }
      
      
      if(GROWTH_RATES){
        x<-ts(dt)
        if(tm=="month"){l<-12}else if(tm=="quarter"){l<-4}else if(tm=="year"){l<-1}
        x<-as.data.frame(G(x,l))
        colnames(x)<-colnames(dt)
        x$time<-dt$time
        dt<-x
        rm(x,l)
        # Now, same for industry level data
        x<-dti
        x<-pdata.frame(x, index=c("code", "time"), drop.index = F)
        if(tm=="month"){l<-12}else if(tm=="quarter"){l<-4}else if(tm=="year"){l<-1}
        x<-G(x,l)
        x$name<-dti$name[!is.na(dti$name)][match(x$code, dti$code[!is.na(dti$name)])]
        colnames(x)<-str_remove_all(colnames(x),paste0(paste0("L",l)[l!=1],"G1\\."))
        colnames(x)<-str_replace_all(colnames(x),"\\."," ")
        if(sum(!(colnames(dti)%in%colnames(x)))>0){stop("some cols may be missing")}
        dti<-x[,colnames(dti)]
        rm(x,l)
      }
      
      
      plotdirm<-paste0(plotdir,"/macro/")
      plotdiri<-paste0(plotdir,"/",aggr,"/","by_cluster/"[FOUR_CLUSTERS])
      if(!dir.exists(plotdirm)){dir.create(plotdirm,recursive = T)}
      if(!dir.exists(plotdiri)){dir.create(plotdiri,recursive = T)}
      
      meth<-c("pearson", "kendall", "spearman")
      
      x<-dt[str_detect(dt$time,paste0(2015:2023,collapse = "|")),!str_detect(colnames(dt),"time")]
      #colnames(x)<-substr(colnames(x),1,12)
      xn<-dt[str_detect(dt$time,paste0(2015:2019,collapse = "|")),!str_detect(colnames(dt),"time")]
      #colnames(xn)<-substr(colnames(xn),1,12)
      for(m in meth){
        C<-cor(x, use = "pairwise.complete.obs", method = m)
        Cn<-cor(xn, use = "pairwise.complete.obs", method = m)
        amt<-C[rownames(C)=="amt",!(colnames(C) %in% c("amt", "cnt"))]
        cnt<-C[rownames(C)=="cnt",!(colnames(C) %in% c("amt", "cnt"))]
        amt_noCovid<-Cn[rownames(Cn)=="amt",!(colnames(Cn) %in% c("amt", "cnt"))]
        cnt_noCovid<-Cn[rownames(Cn)=="cnt",!(colnames(Cn) %in% c("amt", "cnt"))]
        if(m == meth[1]){
          df<-data.frame(GDP_data=c(names(amt), names(cnt), names(amt_noCovid), names(cnt_noCovid)), Pay_data=rep(c("Value", "Count", "Value (pre-Covid)", "Count (pre-Covid)"), each=length(amt)), Pearson=c(amt, cnt, amt_noCovid, cnt_noCovid))
        }else{
          df$add<-c(amt, cnt, amt_noCovid, cnt_noCovid)
          colnames(df)[ncol(df)]<-str_to_title(m)
          if(sum(c(names(amt), names(cnt), names(amt_noCovid), names(cnt_noCovid)) != df$GDP_data)>0){stop("rows do not match")}
        }
      }
      
      clr<-c("red", "blue", "red4", "blue4")
      names(clr)<-c("Value", "Count","Value (pre-Covid)","Count (pre-Covid)")
      m<-meth[1]
      selection<-"all"
      for(selection in c("all", "top10", "bottom10", "top_and_bottom", "selected_sample")){
        for(m in meth){
          df1<-df[,colnames(df) %in% c("GDP_data", "Pay_data", str_to_title(m))]
          colnames(df1)[3]<-"Correlations"
          pdf(paste0(plotdirm, m,"_macro_GDP_",tm,"ly_","growth"[GROWTH_RATES],"levels"[!GROWTH_RATES],"_",selection,".pdf"), width = 10, height = 6)
          g<-ggplot(data = df1, aes(x=reorder(GDP_data, + Correlations), y=Correlations, fill=Pay_data)) 
          g<-g + geom_bar(stat="identity", position=position_dodge()) + 
            scale_fill_manual(values=clr) +
            #scale_fill_brewer(palette = "Paired") + 
            theme(legend.position="bottom") + labs(fill = "Payment data") +
            ylab(paste0(str_to_title(m)," correlation coefficient")) + xlab("") +
            theme_minimal() 
          if(str_detect(selection, "top|all|selected_sample")){
            g<-g + scale_y_continuous(limits = c(-0.5,1), breaks = seq(-0.5,1,0.5)) + 
              geom_hline(yintercept = 0.9, color = "blue", linewidth = 0.25) }
          g <- g + coord_flip()
          plot(g)
          dev.off()
        }
        
      } # selection in all, top, bottom
      rm(g, m, df1, amt_noCovid, cnt, cnt_noCovid, amt, C, x, Cn, xn)
      
      
      m<-meth[2]; cov<-""; pay<-"sum_inputs_amt"; gdp<-"nsa"
      # Repeat the same for industry-level data
      if(exists("df2")){rm(df2)}
      for(m in meth){
        dfi<-data.frame(code=unique(dti$code))
        for(cov in c("", " (pre-Covid)")){
          if(cov == ""){i<-1:nrow(dti)}else if(cov == " (pre-Covid)"){i<-which(!str_detect(dti$time,"2020|2021|2022"))}
          for(pay in c("sum_inputs_amt", "sum_outputs_amt", "sum_inputs_cnt", "sum_outputs_cnt")){
            for(gdp in c("nsa"[!ONLY_SA], "sa")){
              d1<-dti[i,c("code",pay, paste0("GVA ",gdp))]
              x<-sapply(split(d1, as.factor(paste(d1$code))), function(X){cor(X[,pay], X[,paste0("GVA ",gdp)], use ="pairwise.complete.obs",method = m)})
              dfi$add<-x[match(dfi$code, names(x))]
              colnames(dfi)[ncol(dfi)]<-paste0(pay,"_GVA_",gdp,cov)
            } # gdp in sa, nsa
          } # pay in payment data types
        } # cov in c("", "pre-Covid)
        dfi$code<-paste(dfi$code, dti$name[!is.na(dti$name)][match(dfi$code, dti$code[!is.na(dti$name)])])
        df1<-melt(dfi, id.vars = "code")
        colnames(df1)<-str_replace_all(colnames(df1), "value", str_to_title(m))
        if(!exists("df2")){df2<-df1}else{df2<-merge(df2, df1, by=c("code","variable"), all = T)}
      } # m in meth (pearson, kendall, spearman)
      dfi<-df2
      rm(df2, df1, cov, pay, m, gdp, x, i)
      dfi$variable<-str_replace_all(dfi$variable, "sum_inputs_", "Input ")
      dfi$variable<-str_replace_all(dfi$variable, "sum_outputs_", "Output ")
      dfi$variable<-str_replace_all(dfi$variable, "amt_", "Value ")
      dfi$variable<-str_replace_all(dfi$variable, "cnt_", "Count ")
      dfi$variable<-str_replace_all(dfi$variable, "GVA_", "GVA ")
      
      clr<-c("red","orange","green", "blue","red4","orange3","green4", "blue4")
      names(clr)<-c(paste0(c("Input Value","Input Count","Output Value","Output Count"),rep(c(""," (pre-Covid)"),each=4)))
      m<-meth[1]; gdp<-"sa"
      selection<-"bottom5"
      if(FOUR_CLUSTERS){samples<-1:4
      }else{samples<- c("all", "top5", "bottom5", "top_and_bottom")}
      
      for(selection in samples){
        for(m in meth){
          df1<-dfi[,c("code", "variable", str_to_title(m))]
          colnames(df1)[3]<-"Correlations"
          for(gdp in c("sa", paste("nsa")[!ENERGY_CRISIS&&!ONLY_SA])){
            
            df2<-df1[str_detect(df1$variable,paste("GVA",gdp)),]
            df2$variable<-str_remove_all(df2$variable,paste0(" GVA ",gdp))
            
            df2<-df2[order(df2$code,decreasing = T),]
            
            #if(selection != "all"){
            x<-aggregate(.~ code, data=df2[,c(1,3)],FUN=function(x){return(mean(x,na.rm = F))}, na.action = NULL, drop=F)
            x<-x[!is.na(x$Correlations),]
            x<-x[order(x$Correlations,decreasing = T),]
            if(selection=="top_and_bottom"){
              df2<-df2[df2$code %in% x$code[c(1:4,(nrow(x)-3):nrow(x))],]
            }else if(str_detect(selection,"top|all")){
              if(selection=="all"){nselect<-length(x$code)}else{nselect<-as.numeric(str_remove(selection,"top"))}
              df2<-df2[df2$code %in% x$code[1:nselect],]
              rm(nselect)
            }else if(str_detect(selection,"bottom")){
              nselect<-as.numeric(str_remove(selection,"bottom"))
              df2<-df2[df2$code %in% x$code[(nrow(x)-(nselect-1)):nrow(x)],]
              rm(nselect)
            }else if(FOUR_CLUSTERS){
              df2<-df2[str_split(df2$code," ",simplify = T)[,1] %in% str_split(clust_config[[selection]]," ",simplify = T)[,1],]
            }else{stop(paste("implement",selection))}
            
            pdf(paste0(plotdiri, m,"_by_",aggr,"_",gdp,"_",tm,"ly_","growth"[GROWTH_RATES],"levels"[!GROWTH_RATES],"_",selection,".pdf"), width = 10, height = (20-(10*(selection!="all"))))
            
            
            g<-ggplot(data = df2, aes(x=reorder(code, + Correlations), y=Correlations, fill=variable))
            #g<-ggplot(data = df2, aes(x=code, y=Correlations, fill=variable))
            g<-g + geom_bar(stat="identity", position=position_dodge())  +
              scale_fill_manual(values=clr) +
              #scale_fill_brewer(palette = "Paired") + 
              theme(legend.position="left") + labs(fill = "Payment data") +
              ylab(paste0(str_to_title(m)," correlation coefficient")) + xlab("") +
              theme_minimal() 
            if(str_detect(selection, "top|all|cluster")){
              g<-g + scale_y_continuous(limits = c(-0.5,1), breaks = seq(-0.5,1,0.5)) + 
                geom_hline(yintercept = 0.9, color = "blue", linewidth = 0.25) }
            #if(str_detect(selection, "5")){g<-g + theme(axis.text.y=element_text(size=12))}
            g<-g + coord_flip() 
            plot(g)
            dev.off()
          } # gdp in sa, nsa
        } # m in meth
        
      } # selection in all, top5, bottom5
      
      if(tm == "month"){
        stop("here")
      }
      
      rm(g, m, df1, df2, gdp)
    } # tm in month,quarter,year
  } # GROWTH_RATES in F,T
} # ENERGY_CRISIS in F, T

