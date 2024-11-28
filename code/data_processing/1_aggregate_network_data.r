### This script aggregates the network from the edge list ###
#
# Steps: 
# 1.) Make some transformations to get time aggregates (month, quarter, year, all_periods)
# 2.) Create aggregate data for regional and nation level and different levels of sector aggregation (section, 2-5 digit SIC)
# 
# 
# Clean memory
rm(list=ls())
.rs.restartR()
ddir<-"payment_data/Feb2024/"
library(stringr); library(vctrs)
dgts<-"5"
#dgts<-"3"
#dgts<-"2"
datadir<-paste0(ddir,"edgelist_transactions/",dgts,"_digit_data/")
if(!dir.exists(datadir)){dir.create(datadir, recursive = T)}

# Set file path to continue the data compilation log file
fn<-paste0(ddir,"data_compilation_log_",dgts,"_digit.txt")
if(!file.exists(fn)){warning("The data compilation log file does not exist. Potentially, you should re-run script 0.")}
# 
# Load raw data created in step 0
load(paste0(ddir,"raw_data/",dgts,"_digit/raw_data_merged_",dgts,"_digit.RData"))
### Step 1: aggregate by different time periods (year, quarterly) ###
# subset data by data type (amount or count)
dtyp<-"amt" # if count data use: dtype<-"cnt"
time<-"quarter" # chose: all_periods, year, quarter, month
for(time in c("year", "quarter", "month")){
  for(dtyp in c("amt", "cnt")){
    d0<-d[,c(1,2,which(str_detect(colnames(d), dtyp)))] # keep only relevant columns (amt = amount (amount of transactions), cnt = count (number of transactions observed))
    # get column indices for data columns
    dcol<-which(!(colnames(d0) %in% c("from", "to")))
    d0[,dcol]<-apply(d0[,dcol],2,FUN=function(x){return(as.numeric(as.character(x)))})
    # get list of time periods
    if(time == "year"){
      colnames(d0)[dcol]<-paste0("20",substr(colnames(d0)[dcol],(str_length(colnames(d0)[dcol])-(1)),str_length(colnames(d0)[dcol])))
    }else if(time == "quarter"){
      colnames(d0)<-str_replace(colnames(d0),"jan|feb|mar","Q1")
      colnames(d0)<-str_replace(colnames(d0),"apr|may|jun","Q2")
      colnames(d0)<-str_replace(colnames(d0),"jul|aug|sep","Q3")
      colnames(d0)<-str_replace(colnames(d0),"oct|nov|dec","Q4")
    }else if(time == "all_periods"){
      colnames(d0)[dcol]<-"all_periods"
    }else{if(time != "month"){stop("adjust your code for other time aggregation")}}
    periods<-unique(colnames(d0)[dcol]) 
    if(time != "month"){
      # initialize data frame for year aggregates
      dy<-d0[,-dcol]
      # add columns for year aggregates
      p<-periods[1]
      for(p in periods){
        if(sum(str_detect(colnames(d0),p))==1){next}
        if(p == periods[1]){print(Sys.time())}
        print(paste(p))
        dy<-cbind(dy, apply(d0[,str_detect(colnames(d0),p)], 1, sum, na.rm=T))
        colnames(dy)[ncol(dy)]<-paste0(p)
        if(p == periods[length(periods)]){print(Sys.time())}
      }; rm(p)
    }else{dy<-d0}
    print(paste(dtyp, time, dgts))
    save(dy, file=paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_",dgts,"_digit.RData"))
  } # in amt, cnt
} # in year, quarter, month

rm(d0, d, dcol, periods)

### Step 2: Aggregate by SIC and region ###
# 
# Note: Aggregation by section is done in a separate routine below. 
dtyp<-"cnt" # if count data use: dtype<-"cnt"
time<-"quarter" # chose: all_periods, year, quarter, month

dgts<-c(max(str_length(unique(str_split(dy$from,"_",simplify=T)[,1]))):2) # get max number of digits available (alternatively, you can choose your digit levels manually)

if(max(dgts)>2){
  # run loop for diff. aggregations (2,3,4,5 digit, dependent on data availability) and diff. regional aggregation (regional or national)
  aggr<-data.frame(cbind(digit=dgts, regional=rep(c("national"), each=length(dgts))))
}else if(dgts==2){
  aggr<-data.frame(cbind(digit=dgts, regional=rep(c("national","regional"), each=length(dgts))))
}

for(time in c("year", "quarter", "month")){
  for(dtyp in c("amt", "cnt")){
    # Load data
    load(paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_",max(dgts),"_digit.RData"))
    d0<-dy
    a<-1
    for(a in 1:nrow(aggr)){
      digit<-aggr$digit[a]; reg<-aggr$regional[a]
      t1<-Sys.time()
      print(paste("Aggregation: ", digit, "sic", reg, "----- Time starting:", t1))
      if(digit == 5 && reg == "national"){ # aggregation not necessary, but save with harmonized name
        df<-d0
        save(df, file=paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_",digit,"_sic_",reg,"_level.RData"))
        print(paste("Time start:", t1, "Time end:", Sys.time()))
        rm(t1)
        next
      }
      # Load data from next more disaggr level if available
      if(reg=="national" && file.exists(paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_",digit,"_sic_regional_level.RData"))){
        load(paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_",digit,"_sic_regional_level.RData"))
        # get aggr codes for region-industry columns
        df$from<-paste0(substr(str_split(df$from, "_", simplify = T)[,1],1,digit),paste0("_",str_split(df$from, "_", simplify = T)[,2])[reg=="regional"])
        df$to<-paste0(substr(str_split(df$to, "_", simplify = T)[,1],1,digit),paste0("_",str_split(df$to, "_", simplify = T)[,2])[reg=="regional"])
        
      }else if(file.exists(paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_",(as.numeric(digit)+1),"_sic_",reg,"_level.RData"))){
        load(paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_",(as.numeric(digit)+1),"_sic_",reg,"_level.RData"))
        # get aggr codes for region-industry columns
        if(max(str_length(df$from))>5){
          df$from<-paste0(substr(str_split(df$from, "_", simplify = T)[,1],1,digit),paste0("_",str_split(df$from, "_", simplify = T)[,2])[reg=="regional"])
          df$to<-paste0(substr(str_split(df$to, "_", simplify = T)[,1],1,digit),paste0("_",str_split(df$to, "_", simplify = T)[,2])[reg=="regional"])
        }else{
          df$from<-substr(df$from, 1,digit)
          df$to<-substr(df$to,1,digit)
        }
        
      }else{df<-d0}
      d1<-df; rm(df)
      # get indices of relevant data columns
      dcol<-which(!(colnames(d1) %in% c("from", "to")))
      # initialize empty data.frame
      df<-unique(data.frame(cbind(from=d1$from, to=d1$to)))
      df<-data.frame(cbind(df, matrix(NA, ncol = length(dcol), nrow = nrow(df))))
      colnames(df)[3:ncol(df)]<-colnames(d1)[dcol]
      sour<-unique(d1$from)[1]; dest<-d1$to[1]
      for(sour in unique(d1$from)){ 
        print(sour)
        d2<-d1[d1$from == sour,]
        d3<-data.frame()
        for(dest in unique(d2$to)){
          if(length(dcol)>1){d3<-rbind(d3,apply(d2[d2$to == dest,dcol],2,sum, na.rm=T))
          }else{d3<-rbind(d3,sum(d2[d2$to==dest,dcol], na.rm=T))}
        }
        colnames(d3)<-colnames(d2)[dcol]
        #print(paste(range(which(df$from == sour)[match(df$to[which(df$from == sour)],unique(d2$to))])))
        df[which(df$from == sour)[match(df$to[which(df$from == sour)],unique(d2$to))], 3:ncol(df)]<-d3
      } # sour in d1$from
      save(df, file=paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_",digit,"_sic_",reg,"_level.RData"))
      print(paste("Time start:", t1, "Time end:", Sys.time()))
      rm(t1)
    } # a in 1:nrow(aggregation)
  } # dtyp in amt, cnt
  
}
rm(df, digit, d1, d0, sour, dest, d3, d2, dcol, a, aggr)
#
#
# Aggregate at section level
# 
# Load mapping from 2-digit SIC to section
stop("it would be better to update the SIC code list first. There is a new file available. See 0-script.")
load("public_data/concordances/sic07_codes_and_desc.RData")
sections<-unique(data.frame(cbind(code=as.character(nms$section), name=as.character(nms$V2), codes=nms$`2_digit`))) # special treatment for sections as alphanumeric code maps to multiple one digit codes
sections$name[sections$name==""]<-NA; sections$code[sections$code==""]<-NA
sections$name <- vec_fill_missing(sections$name, direction="down"); sections$code <- vec_fill_missing(sections$code, direction="down")
sections<-na.omit(sections)

# Aggregate 2-digit data to section level 
for(reg in c("regional", "national")){
  load(paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_2_sic_",reg,"_level.RData"))
  # Replace 2-digit code in SIC-region columns by simplified section codes (alphabetic)
  dfs<-substr(df$from, 1, 2); dfd<-substr(df$to, 1, 2)
  sc<-unique(sections$code)[1]
  for(sc in unique(sections$code)){
    scs<-unique(sections$codes[sections$code==sc])
    i<-which(dfs %in% scs)
    df$from[i]<-paste0(sc, substr(df$from[i], 3, 50))
    i<-which(dfd %in% scs)
    df$to[i]<-paste0(sc, substr(df$to[i], 3, 50))
  }; rm(i, sc, scs, dfs, dfd)
  # Aggregate columns for identical codes
  df0<-df
  df<-unique(data.frame(cbind(from=df0$from, to=df0$to)))
  dcol<-which(!(colnames(df0) %in% c("from", "to")))
  df<-data.frame(cbind(df, matrix(NA, ncol = length(dcol), nrow = nrow(df))))
  colnames(df)[3:ncol(df)]<-colnames(df0)[dcol]
  sour<-unique(df0$from)[1]
  for(sour in unique(df0$from)){ 
    print(sour)
    d2<-df0[df0$from == sour,]
    d3<-data.frame()
    dest<-unique(d2$to)[3]
    for(dest in unique(d2$to)){
      if(length(dcol)>1){d3<-rbind(d3,apply(d2[d2$to == dest,dcol],2,sum, na.rm=T))
      }else{d3<-rbind(d3,sum(d2[d2$to==dest,dcol], na.rm=T))}
    }
    colnames(d3)<-colnames(d2)[dcol]
    #print(paste(range(which(df$from == sour)[match(df$to[which(df$from == sour)],unique(d2$to))])))
    df[which(df$from == sour)[match(df$to[which(df$from == sour)],unique(d2$to))], 3:ncol(df)]<-d3
  }; rm(d2,d3,sour,dest,dcol)
  save(df, file=paste0(datadir,"/aggregate_",dtyp,"_by_",time,"_and_section_sic_",reg,"_level.RData"))
  rm(df, df0)
} # reg in regional, national
rm(nms, sections)

