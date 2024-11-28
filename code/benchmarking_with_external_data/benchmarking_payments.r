# This script creates a table with aggregate benchmarking statistics
# 
# Benchmarking against: 
# - Other payment schemes
# - GDP, M1, M3

rm(list=ls())
.rs.restartR()

library(xtable); library(stargazer); library(stringr); library(xtable); library(ggplot2); library(reshape2); library(zoo); library(pracma); library(scales); library(plm); library(collapse)

dtversion<-"Feb2024"
ddir<-paste0(dtversion,"/macro_aggregates/")

dgt<-3

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

fn<-dir(paste0(dgt,"_digit_data"))
fn<-fn[!str_detect(fn,"regional")]
fn<-fn[!str_detect(fn,"_amt_cnt_")]
# Load data
tm<-"quarter"
tm<-"month"
for(tm in c("year","quarter","month")){
  fnt<-paste0(ddir,dgt,"_digit_data/",fn[str_detect(fn,tm)])
  f<-fnt[1]
  for(f in fnt){
    x<-loadRData(f)
    #print(paste(f, class(x[,tm])))
    #print(paste(x[1:10,tm], collapse=", "))
    if(f == fnt[1]){dt<-x
    }else{dt<-merge(dt,x[,!(colnames(x) %in% c("cnt","amt"))],by=tm,all = T)}
  }
  # Make column names more convenient
  colnames(dt)<-str_replace_all(colnames(dt)," ","_")
  colnames(dt)[str_detect(colnames(dt),"Total_Retail_Payments_")]<-paste0("All_Retail_(exc_Cards)_",c("Number_of_Items_(000s)_Total","Value_of_Items_(£m)_Total"))
  dt$`Image_Clearing_Number_of_Items_(000s)_Total`[dt$`Image_Clearing_Number_of_Items_(000s)_Total`==0]<-NA
  dt$`Image_Clearing_Value_of_Items_(£m)_Total`[dt$`Image_Clearing_Value_of_Items_(£m)_Total`==0]<-NA
  
  for(cl in colnames(dt)[colnames(dt)!=tm]){dt[,cl]<-as.numeric(as.character(dt[,cl]))}
  
  if(tm == "year"){dty<-dt}else if(tm == "quarter"){dtq<-dt}else{dtm<-dt}
  rm(dt, f, fnt, x)
} # tm in c("year","quarter","month")
rm(fn,tm,loadRData,cl)

# Set GDP data sa for 2023 to NA because the data series ends in Q3-2023
dty[dty$year=="2023", which(str_detect(colnames(dty),"Total_GVA_sa"))]<-NA


dtq$quarter<-as.yearqtr(str_replace(dtq$quarter,"Q","0"),format = "%q-%Y")
dtq<-dtq[order(dtq$quarter),]
dtm$month<-as.yearmon(dtm$month,format = "%B-%Y")
dtm<-dtm[order(dtm$month),]
dtm$avg_amt<-1000*dtm$amt/dtm$cnt
dtq$avg_amt<-1000*dtq$amt/dtq$cnt
dty$avg_amt<-1000*dty$amt/dty$cnt
for(cl in c("Bacs","FPS","Image_Clearing","All_Retail_(exc_Cards)","CHAPS")){
  dtm$avg1<-1000*dtm[,paste0(cl,"_Value_of_Items_(£m)_Total")]/dtm[,paste0(cl,"_Number_of_Items_(000s)_Total")]
  colnames(dtm)[ncol(dtm)]<-paste0(cl,"_average_value")
  dtq$avg1<-1000*dtq[,paste0(cl,"_Value_of_Items_(£m)_Total")]/dtq[,paste0(cl,"_Number_of_Items_(000s)_Total")]
  colnames(dtq)[ncol(dtq)]<-paste0(cl,"_average_value")
  dty$avg1<-1000*dty[,paste0(cl,"_Value_of_Items_(£m)_Total")]/dty[,paste0(cl,"_Number_of_Items_(000s)_Total")]
  colnames(dty)[ncol(dty)]<-paste0(cl,"_average_value")
}
rm(cl)

# Get data set with growth rates
#dtmgr<-data.frame(month=dtm$month, (G(dtm[,2:ncol(dtm)], n=12)))
#dtqgr<-data.frame(quarter=dtq$quarter, G(dtq[,2:ncol(dtq)], n=4))
#dtygr<-data.frame(year=dty$year, G(dty[,2:ncol(dty)], n=1))
dtmgr<-data.frame(month=dtm$month, ((G(dtm[,2:ncol(dtm)], n=12))))
dtqgr<-data.frame(quarter=dtq$quarter, ((G(dtq[,2:ncol(dtq)], n=4))))
dtygr<-data.frame(year=dty$year, ((G(dty[,2:ncol(dty)], n=1))))
dtmgr[,2:ncol(dtmgr)]<-as.data.frame(apply(dtmgr[,2:ncol(dtmgr)],2,FUN=function(x){ifelse(is.nan(x)|is.infinite(x),NA,as.numeric(as.character(x)))},simplify=F))
dtqgr[,2:ncol(dtqgr)]<-as.data.frame(apply(dtqgr[,2:ncol(dtqgr)],2,FUN=function(x){ifelse(is.nan(x)|is.infinite(x),NA,as.numeric(as.character(x)))},simplify=F))
dtygr[,2:ncol(dtygr)]<-as.data.frame(apply(dtygr[,2:ncol(dtygr)],2,FUN=function(x){ifelse(is.nan(x)|is.infinite(x),NA,as.numeric(as.character(x)))},simplify=F))
colnames(dtmgr)<-colnames(dtm)
colnames(dtqgr)<-colnames(dtq)
colnames(dtygr)<-colnames(dty)

# Set path to output directory
outdir<-paste0("statistical_output/aggregate_benchmarking/",dgt,"_digit_data/")
no_covid_data<-"_preCovid"
no_covid_data<-"_noCovid"
no_covid_data<-F
if(no_covid_data!=F){
  outdir<-paste0(outdir,no_covid_data,"/")
  if(no_covid_data == "_preCovid"){
    dtm<-dtm[which(dtm$month<"Mar 2020"),]
    dtq<-dtq[which(dtq$quarter<"2020 Q1"),]
    dty<-dty[which(dty$year<"2020"),]
  }else if(no_covid_data == "_noCovid"){
    dtm<-dtm[which(dtm$month<"Mar 2020" | dtm$month >= "Jan 2023"),]
    dtq<-dtq[which(dtq$quarter<"2020 Q1" | dtq$quarter >= "2023 Q1"),]
    dty<-dty[which(dty$year<2020 | dty$year >= 2023),]
  }
  dtmgr<-data.frame(month=dtm$month, (1+(G(dtm[,2:ncol(dtm)], n=12)/100)))
  dtqgr<-data.frame(quarter=dtq$quarter, (1+(G(dtq[,2:ncol(dtq)], n=4)/100)))
  dtygr<-data.frame(year=dty$year, (1+(G(dty[,2:ncol(dty)], n=1)/100)))
  colnames(dtmgr)<-colnames(dtm)
  colnames(dtqgr)<-colnames(dtq)
  colnames(dtygr)<-colnames(dty)
}else{outdir<-paste0(outdir,"/")}
if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}

# Annual numbers in year y
y<-2021
cols<-c("Bacs","FPS","Image_Clearing","All_Retail_(exc_Cards)","CHAPS")
cl<-cols[1]
tb<-data.frame(matrix(NA,nrow=0,ncol=(3+length(cols))))
for(y in seq(2019,2023,2)[seq(2019,2023,2) %in% unique(dty$year)]){
  d1<-dty[dty$year==y,]
  x<-data.frame(matrix(NA,nrow=3,ncol=(3+length(cols))))
  colnames(x)<-colnames(tb)<-c("year","","Payments",cols)
  x$year<-y
  x[,2]<-c("Transaction value","Transaction counts","Average value")
  amt<-(d1$amt/(10**6)); cnt<-d1$cnt/1000
  x$Payments<-c(amt,cnt,(1000*amt/cnt))
  
  print(paste(y,cl,"value",amt,"counts",cnt, "avg", (1000*amt/cnt)))
  for(cl in cols){
    v<-d1[,paste0(cl,"_Value_of_Items_(£m)_Total")]; c<-d1[,paste0(cl,"_Number_of_Items_(000s)_Total")]
    #av<-1000*v/c
    av<-d1$avg_amt
    print(paste(y,cl,"value",v,"counts",c))
    x[,cl]<-c((amt/v),(cnt/c),av)
  }
  tb<-data.frame(rbind(tb,x))
}
colnames(tb)<-colnames(x)
annual_share_of_payuk_data<-tb
# Repeat similar routine to get correlations for payment data with UK schemes (annual, quarterly, monthly)
tb<-data.frame(matrix(NA,nrow=0,ncol=(3+length(cols))))
tm<-"month"; cl<-cols[1]
for(trnsf in c("levels","logs","scaled")){
  # Set data transformation function
  if(trnsf=="levels"){trf<-function(x){return(x)}
  }else if(trnsf=="logs"){trf<-function(x){return(log(x))}
  }else if(trnsf=="index2015=100"){trf<-function(x){return(x)}; stop("IMPLEMENT THIS FUNCTION FIRST")
  }else if(trnsf=="scaled"){trf<-function(x){return(scale(x))}}
  
  for(tm in c("month","quarter", "year")){
    if(tm=="month"){dt<-dtm}else if(tm=="quarter"){dt<-dtq}else{dt<-dty}
    x<-data.frame(matrix(NA,nrow=3,ncol=(3+length(cols))))
    colnames(x)<-colnames(tb)<-c("Transformation","Correlation_among",  "Data_type",  cols)
    x[,1]<-rep(trnsf,nrow(x))
    x[,2]<-rep(str_to_title(paste0(tm,"ly")))
    x[,3]<-c(paste0("Values"), paste0("Counts"), paste0("Avg values"))
    for(cl in cols){
      amt<-trf(dt$amt);cnt<-trf(dt$cnt)
      vd<-trf(dt[,paste0(cl,"_Value_of_Items_(£m)_Total")]); cd<-trf(dt[,paste0(cl,"_Number_of_Items_(000s)_Total")])
      vd[is.infinite(vd)]<-NA; cd[is.infinite(cd)]<-NA
      v<-cor(amt, vd, use="pairwise.complete.obs")
      c<-cor(cnt, cd, use="pairwise.complete.obs")
      #av<-cor(trf(dt$amt/dt$cnt), trf(dt[,paste0(cl,"_Value_of_Items_(£m)_Total")]/dt[,paste0(cl,"_Number_of_Items_(000s)_Total")]), use="pairwise.complete.obs")
      av<-cor(trf(dt$avg_amt), trf(1000*dt[,paste0(cl,"_Value_of_Items_(£m)_Total")]/dt[,paste0(cl,"_Number_of_Items_(000s)_Total")]), use="pairwise.complete.obs")
      x[,cl]<-c(v,c,av)
    }
    tb<-data.frame(rbind(tb,x))
  }
}

correlations_with_other_UK_schemes<-tb
rm(x,cl,v,av,c,d1,y,amt,cnt,cols,tb,vd,cd,dt,trf,trnsf,tm)


## Benchmarking of AMT against GDP, M1, M3. Calculate shares and correlations between annual, quarterly, monthly data
#cols<-c("", "GDP","M1","M3","Prices")

cols<-c("","Bacs","FPS","Image_Clearing","Retail","CHAPS", "GDP nsa", "GDP sa","M1","M3","Prices","Producer Prices")
for(i in 1:2){
  if(i == 1){
    dty1<-dty; dtq1<-dtq; dtm1<-dtm
  }else{dty1<-dtygr; dtq1<-dtqgr; dtm1<-dtmgr}
  
  rows<-c(paste0("Share in ",seq(2019,2023,2)[seq(2019,2023,2) %in% unique(dty$year)]),paste(rep(c("Yearly","Quarterly","Monthly"),3),rep(c("(value)","(logs)","(count)","(logs, count)","(avg)", "(logs, avg)"),each=3)))
  tb<-data.frame(matrix(NA,nrow=length(rows),ncol=(length(cols))))
  colnames(tb)<-cols
  tb[,1]<-rows
  y<-2019
  cl1<-paste0(c("Bacs","FPS","Image_Clearing","All_Retail_(exc_Cards)","CHAPS"), rep(c("_Value_of_Items_(£m)_Total"), 5))
  cl2<-paste0(c("Bacs","FPS","Image_Clearing","All_Retail_(exc_Cards)","CHAPS"), rep(c("_Number_of_Items_(000s)_Total"), 5))
  if(i==1){
    
    for(y in seq(2019,2023,2)[seq(2019,2023,2) %in% unique(dty$year)]){
      d1<-dty1[dty1$year==y,c("amt",paste0(c("Bacs","FPS","Image_Clearing","All_Retail_(exc_Cards)","CHAPS"), "_Value_of_Items_(£m)_Total"),"GDP_at_market_prices:_Current_prices","GDP_at_market_prices:_Chained_volume_measure","M1_GBP","M3_GBP","Consumer_prices:_all_items_Index_2015=100","Producer_prices_-_Manufacturing_Index_2015=100")]
      #d1$`GDP_at_market_prices:_Chained_volume_measure`<-d1$`GDP_at_market_prices:_Chained_volume_measure`*10**6
      #d1$`GDP_at_market_prices:_Current_prices`<-d1$`GDP_at_market_prices:_Current_prices`*10**6
      tb[str_detect(tb[,1],paste(y)),2:ncol(tb)]<-c(c(rep((d1$amt/10**6), 5), rep((d1$amt/10**6),2), rep((d1$amt/10**6), 2),NA)  / d1[,2:ncol(d1)])
    }
    rm(y,d1) 
    # Transform all into index data (GDP nsa/sa are only available as indices)
    x<-apply(as.data.frame(dty1[,colnames(dty1)!="year"]),2,FUN=function(x){return(as.numeric(as.character(x)))})
    z<-matrix(x[which(dty1$year=="2016"),],byrow = T, nrow = nrow(x), ncol = ncol(x))
    dty1[,colnames(dty1)!="year"]<-x/z
    x<-apply(as.data.frame(dtq1[,colnames(dtq1)!="quarter"]),2,FUN=function(x){return(as.numeric(as.character(x)))})
    z<-matrix(x[which(dtq1$quarter=="2016 Q1"),],byrow = T, nrow = nrow(x), ncol = ncol(x))
    dtq1[,colnames(dtq1)!="quarter"]<-x/z
    x<-apply(as.data.frame(dtm1[,colnames(dtm1)!="month"]),2,FUN=function(x){return(as.numeric(as.character(x)))})
    z<-matrix(x[which(dtm1$month=="Jan 2016"),],byrow = T, nrow = nrow(x), ncol = ncol(x))
    dtm1[,colnames(dtm1)!="month"]<-x/z
    rm(x,z)
  }
  
  # For monthly & quarterly data, remove data from 2015, as the Payment data series starts in Aug 2015. 
  dtq1<-dtq1[str_detect(dtq1$quarter, paste0(c(2016:2023,"2015 Q4"),collapse = "|")),]
  dty1<-dty1[dty1$year %in% 2016:2023,]
  
  dy1<-dty1[,c("amt","cnt",cl1,"Total_GVA_nsa","Total_GVA_sa","M1_GBP","M3_GBP","Consumer_prices:_all_items_Index_2015=100","Producer_prices_-_Manufacturing_Index_2015=100",cl2,"avg_amt","Bacs_average_value","FPS_average_value","Image_Clearing_average_value","All_Retail_(exc_Cards)_average_value","CHAPS_average_value")]
  dq1<-dtq1[,c("amt","cnt",cl1,"Total_GVA_nsa","Total_GVA_sa","M1_GBP","M3_GBP","Consumer_prices:_all_items_Index_2015=100","Producer_prices_-_Manufacturing_Index_2015=100",cl2,"avg_amt","Bacs_average_value","FPS_average_value","Image_Clearing_average_value","All_Retail_(exc_Cards)_average_value","CHAPS_average_value")]
  dm1<-dtm1[,c("amt","cnt",cl1,"Total_GVA_nsa","Total_GVA_sa","M1_GBP","M3_GBP","Consumer_prices:_all_items_Index_2015=100","Producer_prices_-_Manufacturing_Index_2015=100",cl2,"avg_amt","Bacs_average_value","FPS_average_value","Image_Clearing_average_value","All_Retail_(exc_Cards)_average_value","CHAPS_average_value")]
  
  
  for(cl in c(3:13)){
    if(cl %in% which(!(str_detect(colnames(dm1),paste0(c("amt","cnt","Bacs","FPS","Image_Clearing","All_Retail","CHAPS"),collapse="|"))))){cl1<-cl2<-cl3<-cl
    }else{
      cl1<-which(str_detect(colnames(dm1),colnames(tb)[(cl-1)]))[1]
      cl2<-which(str_detect(colnames(dm1),colnames(tb)[(cl-1)]))[2]
      cl3<-which(str_detect(colnames(dm1),colnames(tb)[(cl-1)]))[3]
    }
    print(cl)
    print(paste(colnames(dy1)[cl1],colnames(dq1)[cl1],colnames(dm1)[cl1],colnames(tb)[cl-1]))
    print(paste(colnames(dy1)[cl2],colnames(dq1)[cl2],colnames(dm1)[cl2],colnames(tb)[cl-1]))
    print(paste(colnames(dy1)[cl3],colnames(dq1)[cl3],colnames(dm1)[cl3],colnames(tb)[cl-1]))
    tb[(4-((no_covid_data!=F)+(no_covid_data=="_preCovid"))):nrow(tb),(cl-1)]<-c(cor(dy1$amt,dy1[,cl1],use = "pairwise.complete.obs"), 
                             cor(dq1$amt,dq1[,cl1],use = "pairwise.complete.obs"), 
                             cor(dm1$amt,dm1[,cl1],use = "pairwise.complete.obs"), 
                             cor(log(dy1$amt),log(dy1[,cl1]),use = "pairwise.complete.obs"), 
                             cor(log(dq1$amt),log(dq1[,cl1]),use = "pairwise.complete.obs"), 
                             cor(log(dm1$amt),log(dm1[,cl1]),use = "pairwise.complete.obs"),
                             cor(dy1$cnt,dy1[,cl2],use = "pairwise.complete.obs"), 
                             cor(dq1$cnt,dq1[,cl2],use = "pairwise.complete.obs"), 
                             cor(dm1$cnt,dm1[,cl2],use = "pairwise.complete.obs"), 
                             cor(log(dy1$cnt),log(dy1[,cl2]),use = "pairwise.complete.obs"), 
                             cor(log(dq1$cnt),log(dq1[,cl2]),use = "pairwise.complete.obs"), 
                             cor(log(dm1$cnt),log(dm1[,cl2]),use = "pairwise.complete.obs"),
                             cor((dy1$avg_amt),dy1[,cl3],use = "pairwise.complete.obs"), 
                             cor((dq1$avg_amt),dq1[,cl3],use = "pairwise.complete.obs"), 
                             cor((dm1$avg_amt),dm1[,cl3],use = "pairwise.complete.obs"), 
                             cor(log((dy1$avg_amt)),log(dy1[,cl3]),use = "pairwise.complete.obs"), 
                             cor(log((dq1$avg_amt)),log(dq1[,cl3]),use = "pairwise.complete.obs"), 
                             cor(log((dm1$avg_amt)),log(dm1[,cl3]),use = "pairwise.complete.obs"))
  }
  colnames(tb)<-str_replace_all(colnames(tb),"Image_Clearing","IC")
  if(i == 1){
    shares_and_correlations_macro_data<-tb
  }else{shares_and_correlations_macro_data_growth_rates<-tb}
}


rm(cl,dy1,dq1,dm1,rows,cols,tb,i,cl1,cl2,cl3,dty1,dtm1,dtq1)



## Print tables to file
fn<-paste0(outdir,"Summary_stats.tex")
write("",file=fn,append=F)
a<-annual_share_of_payuk_data
a$year<-as.character(a$year)
colnames(a)<-str_replace_all(colnames(a),"_\\(exc_Cards\\)","**")
colnames(a)<-str_replace_all(colnames(a),"Payments","Payments*")
colnames(a)<-str_replace_all(colnames(a),"_"," ")
for(y in unique(a$year)){ 
  write(stargazer(a[a$year==y,colnames(a)!="year"], summary = F, title = paste0("Benchmarking against other payment schemes in the UK in ",y), label = paste0("tab:UK_payments_",y),notes = c("*Count and value data. Other columns show payment data as a share of transactions in other schemes.", "**excluding cards."), rownames = F, font.size = "scriptsize"), file = fn, append = T)
}
rm(a,y)

cr<-correlations_with_other_UK_schemes
colnames(cr)<-str_replace_all(colnames(cr),"_\\.exc_Cards\\.","**")
colnames(cr)<-str_replace_all(colnames(cr),"_"," ")
for(trnsf in unique(correlations_with_other_UK_schemes$Transformation)){
  write(stargazer(cr[cr$Transformation==trnsf,colnames(cr)!="Transformation"], summary = F, title = paste0("Correlations between Payment data and major UK schemes",paste0(" (",trnsf,")")),notes = c("*excluding cards."), rownames = F, font.size = "scriptsize"), file = fn, append = T)
}
rm(cr,trnsf)


cl<-which(!(colnames(shares_and_correlations_macro_data) %in% c("IC", "Retail","Producer")))

write(stargazer(shares_and_correlations_macro_data[,cl], summary = F, title = paste0("Correlations with other payments and macro aggregates"), label = paste0("tab:macro_benchmarking_",dgt),notes = c("Monthly GDP is proxied from indicative ONS non-seasonally adjusted Total GVA data.", "Shares of GDP are calculated using OECD data, as the ONS Total GVA data is only published as an index series.", "Correlations are calculated between index data using the first observation of 2016 as denominator."), rownames = F, font.size = "scriptsize"), file = fn, append = T)

write(stargazer(shares_and_correlations_macro_data_growth_rates[,cl], summary = F, title = paste0("Correlations with other payments and macro aggregates (growth rates)"), label = paste0("tab:macro_benchmarking_",dgt,"_growth"),notes = c("Monthly GDP is proxied from indicative ONS non-seasonally adjusted Total GVA data.", "Not sure for M1/M3 (narrow/broad money aggregates as published by OECD)."), rownames = F, font.size = "scriptsize"), file = fn, append = T)

rm(cl, shares_and_correlations_macro_data, shares_and_correlations_macro_data_growth_rates, annual_share_of_payuk_data, correlations_with_other_UK_schemes)

plotdir0<-paste0(outdir,"/plots/")
if(!dir.exists(plotdir0)){dir.create(plotdir0)}
## Create plots Payment values against other schemes
# 
# If paper selection, only plots relevant for final paper are plotted incl. some cosmetic adjustments
PAPER_SELECTION<-T

tm<-"month"; trnsf<-"log10"; fig<-c("aggregate_timeseries", "monthly_fluctuations_index")[1] # Create plots for two different figures
for(fig in c("aggregate_timeseries", "monthly_fluctuations_index")){
  
  if(PAPER_SELECTION){
    plotdir<-paste0(plotdir0,"/selection_for_paper/",fig,"/")
  }else{
    plotdir<-paste0(plotdir0,"/",fig,"/")
  }
  if(!dir.exists(plotdir)){dir.create(plotdir, recursive = T)}
  
  
  # Set time aggregation needed
  if(fig=="monthly_fluctuations_index"){
    # Only monthly and scaled transformation needed
    tms<-"month"
    transformations<-c("index2015=100", "scaled", "detrended_scaled")
    if(PAPER_SELECTION){transformations<-transformations[1:3]}
  }else{
    tms<-c("year","quarter","month")
    # Plot for different data transformations (log10 used in the final paper)
    transformations<-c("log10", "scaled", "levels", "index2015=100","logs") 
    if(PAPER_SELECTION){
      transformations<-transformations[1]; tms<-tms[3]
    }
  }
  
  
  tm<-"month"
  # Create figures for different time aggregations (month used in final paper)
  for(tm in tms){
    if(tm=="year"){dt<-dty[dty$year%in%2015:2023,]
    }else if(tm=="quarter"){dt<-dtq[which((substr(dtq$quarter,1,4)%in%2015:2023)&!(dtq$quarter%in%c("2023 Q3","2023 Q4"))),]}else{dt<-dtm[which(dtm$month>="Jan 2015" & dtm$month<="Dec 2023"),]}
    # Select variables to plot
    cls<-c(tm,"amt","cnt","Bacs_Number_of_Items_(000s)_Total","Bacs_Value_of_Items_(£m)_Total","CHAPS_Number_of_Items_(000s)_Total","CHAPS_Value_of_Items_(£m)_Total","FPS_Number_of_Items_(000s)_Total","FPS_Value_of_Items_(£m)_Total", "Image_Clearing_Number_of_Items_(000s)_Total","Image_Clearing_Value_of_Items_(£m)_Total","All_Retail_(exc_Cards)_Number_of_Items_(000s)_Total","All_Retail_(exc_Cards)_Value_of_Items_(£m)_Total","M1_GBP","M3_GBP","Total_GVA_nsa", "Total_GVA_sa","Bacs_Number_of_Items_(000s)_Direct_Credits","Bacs_Number_of_Items_(000s)_Direct_Debits","Bacs_Value_of_Items_(£m)_All_Credits(a,_b)","Bacs_Value_of_Items_(£m)_Direct_Debits")
    cls<-cls[cls%in%colnames(dt)]
    dt<-dt[,cls]
    colnames(dt)<-str_remove_all(colnames(dt), "_Number_of_Items|_Value_of_Items|_Total|_\\(exc_Cards\\)")
    colnames(dt)<-str_replace_all(colnames(dt),"_All_Credits\\(a,_b\\)","_Direct_Credits")
    colnames(dt)[2:3]<-c("Payments_(£m)","Payments_(000s)")
    dt$`Payments_(£m)`<-dt$`Payments_(£m)`/10**6
    dt$`Payments_(000s)`<-dt$`Payments_(000s)`/1000
    
    # Add average values
    cls<-unique(str_remove_all(colnames(dt),"_\\(£m\\)|_\\(000s\\)"))
    for(cl in cls){
      if(str_detect(cl,"Direct_")){cl<-str_remove(cl,"Bacs_")}
      x<-dt[colnames(dt)[str_detect(colnames(dt),cl)]]
      if(cl=="Bacs"){x<-x[!str_detect(colnames(x),"Direct")]}
      if(ncol(x)!=2){next
      }else{
        dt$av<-(1000*x[,str_detect(colnames(x),"£m")])/x[,str_detect(colnames(x),"000s")]
        dt$av[is.infinite(dt$av)]<-NA
        colnames(dt)[ncol(dt)]<-paste0(cl,"_(average)")
      }
    }
    rm(cl, cls, x)
    
    for(trnsf in transformations){ # Test different data transformations
      #stop("here")
      dt1<-dt
      # Make data transformation (z-scaled data, index data setting 2015=100, raw data in levels)
      if(str_detect(trnsf,"detrend")){
        dt1[,colnames(dt1)!=tm]<-apply(dt1[,colnames(dt1)!=tm], 2, FUN=function(x){return(detrend(x))})
        #stop("test this")
      } 
      if(trnsf=="scaled"){
        # Scale data (subtract column mean and divide by standard deviation)
        dt1[,colnames(dt1)!=tm]<-apply(dt1[,colnames(dt1)!=tm], 2, scale)
      }else if(trnsf=="index2015=100"){
        dt1[,colnames(dt1)!=tm]<-apply(dt1[,colnames(dt1)!=tm], 2, FUN = function(x){return(100*x/na.omit(x)[1])})
      }else if(trnsf=="logs"){
        dt1[,colnames(dt1)!=tm]<-apply(dt1[,colnames(dt1)!=tm], 2, log)
      }else if(trnsf%in%c("levels","log10")){
        dt1<-dt1
        #dlevels[,colnames(dt)!=tm]<-apply(dlevels[,colnames(dt)!=tm], 2, FUN = function(x){return(100*x/x[min(which(!is.na(x) & x != 0))])})
      }
      dt1[,colnames(dt1)!=tm]<-apply(dt1[,colnames(dt1)!=tm],2,FUN=function(x){return(ifelse(is.infinite(x),NA,x))})
      
      # Reshape data for plotting
      dt0<-melt(dt1, id.vars = tm)
      colnames(dt0)[1]<-"time"
      #dt0$time<-as.character(dt0$time)
      dt0$variable<-as.factor(as.character(str_replace_all(dt0$variable,"_"," ")))
      dt0$variable<-as.factor(as.character(str_replace_all(dt0$variable, " GBP", "")))
      
     
      var<-"000s"
      var<-"£m"
      var<-"average"
      for(var in c("£m","000s","average")){
        #yl<-paste0("Transaction ","values in £m "[var=="£m"],"counts in 000s "[var=="000s"],paste0("(",trnsf,")")[trnsf!="levels"])
        yl<-""
        dtp<-dt0[str_detect(dt0$variable,paste0(var,"|M1|M3|GVA|GDP")),]
        dtp$variable<-as.factor(as.character(str_remove_all(dtp$variable,paste0(" \\(",var,"\\)"))))
        dtp$variable<-as.factor(as.character(str_replace_all(dtp$variable, "Bacs Direct", "Direct")))
        dtp$variable<-as.factor(as.character(str_replace_all(dtp$variable, "Total GVA", "GDP")))

        if(trnsf=="log10"){
          if(var=="£m"){dtp$value<-dtp$value*10^6
          }else if(var=="000s"){dtp$value<-dtp$value*1000}
        }
          
        
        #clr<-c("red"[var=="£m"],"royalblue"[var=="000s"],"green4","black","darkorange","purple","darkgray","darkgreen","green")
        clr<-c("red","green4","black","darkorange","purple","darkgray","darkgreen","green", "skyblue", "skyblue3", rep("navy",2), rep("royalblue",2))
        ltp<-c("solid",rep(11,(length(clr)-7)), rep(c("dashed","solid","solid"),each=2))
        names(clr)<-names(ltp)<-c("Payments","Bacs","CHAPS","FPS","Image Clearing","All Retail","Direct Credits","Direct Debits","M1","M3","GVA nsa","GDP nsa", "GVA sa","GDP sa")
        
        # Set variables to exclude
        if(fig=="monthly_fluctuations_index"){excl<-"|All Retail|Image|M3|GVA"}else{excl<-"|M1|M3|GDP|GVA"}
        # Plot AMT/CNT against values
        pdf(paste0(plotdir,"Payment_",var,"_compared_to_other_schemes_",tm,"_",trnsf,".pdf"))
        #ttl<-"Benchmarking with major UK payment schemes"
        ttl<-""
        p0<-ggplot(dtp[!str_detect(dtp$variable,paste0("All Retail|Credit|Debit",excl)),], aes(x=time, y=value, color=variable, group=variable,linetype=variable)) + 
          geom_line(lwd=1.5) + theme_bw() + ylab(yl) + xlab("") + 
          ggtitle(paste0(ttl)) + scale_color_manual(values = clr) + scale_linetype_manual(values = ltp)
        if(trnsf=="log10"){
          #p0<-p0 + scale_y_continuous(trans = "log10")
          #p0<-p0 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
          #                labels = trans_format("log10", math_format(10^.x))) + annotation_logticks(sides="l")
          p0<-p0 + scale_y_continuous(
            trans = "log10",
            breaks = function(x) {
              brks <- extended_breaks(Q = c(1, 5))(log10(x))
              10^(brks[brks %% 1 == 0])
            },
            labels = math_format(format = log10)
          ) + annotation_logticks(sides="l")
        }
        
        # Switch of the legend if legend manually added as separate plot
        if(!PAPER_SELECTION){
          p0<-p0+theme(legend.title = element_blank(), legend.text = element_text(size=16), legend.position = "top", legend.direction = "horizontal", legend.justification = "center", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
        }else{
          p0<-p0 + theme(legend.position = "none", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
        }
        if(tm == "month"){
          p0 <- p0 + scale_x_yearmon(labels = date_format("%Y"))
        }
        
        plot(p0)
        dev.off()
        
        if(!PAPER_SELECTION){
          # Plot AMT/CNT against All retail
          if(fig=="monthly_fluctuations_index"){
            #ttl<-"Benchmarking with all Retail Payments in the UK (excl Cards)"
            ttl<-""
            pdf(paste0(plotdir,"Payment_",var,"_compared_to_all_retail_",tm,"_",trnsf,".pdf"))
            p0<-ggplot(dtp[str_detect(dtp$variable,"All Retail|Payment"),], aes(x=time, y=value, color=variable, group=variable,linetype=variable)) + 
              geom_line(lwd=1.5) + theme_bw() + ylab(yl) + xlab("") + 
              ggtitle(paste0(ttl)) + scale_color_manual(values = clr) + scale_linetype_manual(values = ltp)
            if(trnsf=="log10"){
              #p0<-p0 + scale_y_continuous(trans = "log10")           
              #p0<-p0 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),                           labels = trans_format("log10", math_format(10^.x))) + annotation_logticks(sides="l")
              p0<-p0 + scale_y_continuous(
                trans = "log10",
                breaks = function(x) {
                  brks <- extended_breaks(Q = c(1, 5))(log10(x))
                  10^(brks[brks %% 1 == 0])
                },
                labels = math_format(format = log10)
              ) + annotation_logticks(sides="l")
            }
            
            # Switch of the legend if legend manually added as separate plot
            if(!PAPER_SELECTION){
              p0<-p0+theme(legend.title = element_blank(), legend.text = element_text(size=16), legend.position = "top", legend.direction = "horizontal", legend.justification = "center", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
            }else{
              p0<-p0 + theme(legend.position = "none", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
            }
            
            plot(p0)
            dev.off()
          }
          
          
        }
        # Plot AMT/CNT against All retail
        # ttl<-"Benchmarking with Bacs Direct Debit & Direct Credit"
        ttl<-""
        pdf(paste0(plotdir,"Payment_",var,"_compared_to_Bacs_instruments_",tm,"_",trnsf,".pdf"))
        if(fig=="aggregate_timeseries"){vrs<-"Debit|Credit|Payment"
        }else{vrs<-"Debit|Credit|Payment|GDP|GVA"}
        p0<-ggplot(dtp[str_detect(dtp$variable,vrs),], aes(x=time, y=value, color=variable, group=variable, linetype=variable)) + 
          geom_line(lwd=1.5) + theme_bw() + ylab(yl) + xlab("") + 
          ggtitle(paste0(ttl)) + scale_color_manual(values = clr) + scale_linetype_manual(values = ltp)
        
        if(trnsf=="log10"){
          #p0<-p0 + scale_y_continuous(trans = "log10")           
          #p0<-p0 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),                           labels = trans_format("log10", math_format(10^.x))) + annotation_logticks(sides="l")
          p0<-p0 + scale_y_continuous(
            trans = "log10",
            breaks = function(x) {
              brks <- extended_breaks(Q = c(1, 5))(log10(x))
              10^(brks[brks %% 1 == 0])
            },
            labels = math_format(format = log10)
          ) + annotation_logticks(sides="l")
        }
        
        # Switch of the legend if legend manually added as separate plot
        if(!PAPER_SELECTION){
          p0<-p0+theme(legend.title = element_blank(), legend.text = element_text(size=16), legend.position = "top", legend.direction = "horizontal", legend.justification = "center", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
        }else{
          p0<-p0 + theme(legend.position = "none", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
        }
        if(tm == "month"){
          p0 <- p0 + scale_x_yearmon(labels = date_format("%Y"))
        }
        
        plot(p0)
        dev.off()
      }
      rm(var,clr,yl,ltp)
      rm(p0,dtp)
    } # for dt_transform in scaled, levels, index 2015=100
    rm(trnsf)
  } # tm in year, quarter, month
  
  if(PAPER_SELECTION){
    # Plot a legend separately, showing only the relevant colors
    #clr<-c("red"[var=="£m"],"royalblue"[var=="000s"],"green4","black","darkorange","purple","darkgray","darkgreen","green")
    
    clr<-c("red","green4","black","darkorange","purple","darkgray","darkgreen","green", "skyblue", "skyblue3", rep("navy",2), rep("royalblue",2))
    ltp<-c("solid",rep(11,(length(clr)-7)), rep(c("solid","solid","solid"),each=2))
    names(clr)<-names(ltp)<-c("Payments","Bacs","CHAPS","FPS","Image Clearing","All Retail","Direct Credits","Direct Debits","M1","M3","GVA nsa","GDP nsa", "GVA sa","GDP sa")
    
    if(fig=="aggregate_timeseries"){
      pdf(paste0(plotdir,"shared_legend_",fig,".pdf"), height = 0.4, width = 10)
      clr2<-clr[!(names(clr)%in%c("All Retail","M1","M3","GVA nsa","GVA sa","GDP nsa","GDP sa","Direct Credits","Direct Debits"))]
      par(mar=c(0,0,0,0))
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(1,3), ylim=c(0,0.5), mar=0)
      legend("top", legend = names(clr2), lty=1, lwd=2, cex=1, bty='n',
             col = clr2,  horiz = T)
      dev.off()
      pdf(paste0(plotdir,"shared_legend2_",fig,".pdf"), height = 0.4, width = 10)
      par(mar=c(0,0,0,0))
      clr2<-clr[(names(clr)%in%c("Payments","Direct Credits","Direct Debits"))]
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(1,3), ylim=c(0,0.5), mar=0)
      legend("top", legend = names(clr2), lty=1, lwd=2, cex=1, bty='n',
             col = clr2, ncol=3, horiz = F)
      dev.off()
    }else{
      pdf(paste0(plotdir,"shared_legend_",fig,".pdf"), height = 0.6, width = 10)
      clr2<-clr[!(names(clr)%in%c("All Retail","Image Clearing","M3"))]
      par(mar=c(0,0,0,0))
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(1,3), ylim=c(0,0.5), mar=0)
      legend("top", legend = names(clr2), lty=ltp, lwd=3, cex=1, bty='n',
             col = clr2, ncol = 4, horiz = F)
      
      dev.off()
      pdf(paste0(plotdir,"shared_legend2_",fig,".pdf"), height = 0.4, width = 10)
      par(mar=c(0,0,0,0))
      clr2<-clr[!(names(clr)%in%c("All Retail","Image Clearing","Direct Credits","Direct Debits","M3", "GDP sa", "GDP nsa"))]
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(1,3), ylim=c(0,0.5), mar=0)
      legend("top", legend = names(clr2), lty=1, lwd=2, cex=1, bty='n',
             col = clr2, ncol=length(clr2), horiz = F)
      dev.off()
    }
  }
  
  rm(cls,p0,clr,clr2,ttl,vrs,tms)
  
} # fig in c("aggregate_timeseries", "monthly_fluctuations_index")
# 
rm(dt, dt1, dt0, tm, fig)

d23$cnt/(d23$`Bacs_Number_of_Items_(000s)_Total`*10**3)

d23$cnt/((d23$`FPS_Number_of_Items_(000s)_Total` + d23$`Image_Clearing_Number_of_Items_(000s)_Total` + d23$`Bacs_Number_of_Items_(000s)_Total`)*10**3)

