### This script creates National Account (NA) level data for 2010-2023 ###
# Time coverage varies across data sets due to data availability. 
#
# Steps: 
# 1.) Aggregate Payment into CPA NA edgelist
# 2.) Transform and aggregate ONS IO table into NA edgelist
# 3.) Merge Payment and ONS
# 
# 
# Inputs: 
#  - Payment 5-digit SIC edgelist of transactions (choose aggr<-CPA below) or Payment 2-digit SIC edgelist of transactions (choose aggr<-CPA_2digit below)
#  - ONS input-outputs tables downloaded from: https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables [accessed Apr 20, 2023]
#     3 types of ONS data are added: 
#     1.) Supply and Use Tables (SUT): https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables
#     2.) Analytical Input-Output Tables Product-by-Product (PxP): https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed
#     3.) Analytical Input-Output Tables Industry-by-Industry: https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesindustrybyindustry
#  - NA code to SIC concordance table initially compiled by Francois
#  
#  
#  Outputs: 
#  - Table with columns that show industry-to-industry transactions from ONS and from Payment data
#  
#  
#  
#################################################################
# 
# Clean memory
rm(list=ls())
.rs.restartR()

library(stringr); library(readxl); library(igraph); library(reshape2)

# Choose digit level for Payment data
dgt<-5

# Choose desired aggregation level
# (some IOT PxP from ONS use A64 classification of products which is more aggregate than CPA)
#aggr<-"A16"
#aggr<-"CPA_2digit" # CPA that are compatible with 2-digit SIC
#aggr<-"Region80" # Region80 industry codes that are compatible with region-industry GVA (ONS) and 2-digit regionally disaggregate Payment data
aggr<-"CPA" # 105 CPA codes used in ONS IOTs and compatible 5-digit Payment data 
aggr<-"CPA_gdp_data"
#aggr<-"A16"
#aggr<-"Region80"
if(aggr == "Region80"){dgt<-2; warning("2-level digit data will be used because this is the only data available at regional level.")}
if(aggr%in%c("CPA_2digit", "Region80") && dgt==5){stop("Use of CPA_2digit or Region80 does not make sense if data is 5 digit.")}


ddir<-"payment_data/Feb2024/"
datadir<-paste0(ddir,"national_account_data/",dgt,"_digit_data/")
if(!dir.exists(datadir)){dir.create(datadir,recursive=T)}

# Load merged (and pre-cleaned) raw data
load(paste0(ddir,"raw_data/",dgt,"_digit/raw_data_merged_",dgt,"_digit.RData"))

# Load concordance table that maps 2-digit/5-digit data to classifications that are compatible with official ONS data (which varies across datasets)
load("public_data/concordances/concordances.RData")

# Create a list to map SIC in Payment to codes from concordance table as specified by "aggr"
sic0<-data.frame(cbind(sic=unique(c(d$from, d$to)), csic=NA))
if(aggr=="2digit"){for(i in 1:nrow(conc)){sic0$csic[which(substr(sic0$sic,1,str_length(conc$SIC_2digit[i]))==conc$SIC_2digit[i])]<-conc$CPA_2digit[i]}; rm(i)
}else if(aggr %in% c("CPA", "CPA_gdp_data") && dgt == 5){
  # Transform 5-digit code into an SIC version that can be directly mapped to CPA (which map to 2-, 3-, or 4-digit SICs, dependent on the industry and sometimes, 2 or more 2-digit SICs belong to a single CPA)
  sic0$sic2<-sic0$sic
  cds<-conc$SIC[na.omit(match(substr(sic0$sic,1,4),conc$SIC))]
  i<-which(substr(sic0$sic2,1,4) %in% cds)
  sic0$sic2[i]<-substr(sic0$sic2,1,4)[i]
  cds<-conc$SIC[na.omit(match(substr(sic0$sic,1,3),conc$SIC))]
  i<-which(substr(sic0$sic2,1,3) %in% cds)
  sic0$sic2[i]<-substr(sic0$sic2,1,3)[i]
  cds<-conc$SIC[na.omit(match(substr(sic0$sic,1,2),conc$SIC))]
  i<-which(substr(sic0$sic2,1,2) %in% cds)
  sic0$sic2[i]<-substr(sic0$sic2,1,2)[i]
  # Add CPAs
  sic0$csic<-conc[match(sic0$sic2,conc$SIC), aggr]
  rm(i,cds)
  
}else{for(i in 1:nrow(conc)){sic0$csic[which(substr(sic0$sic,1,str_length(conc$SIC_2digit[i]))==conc$SIC_2digit[i])]<-conc[i,aggr]}; rm(i)}

# Aggregate data by time, region, industry
time<-"year"; dtyp<-"amt"
reg<-"national"

d<-melt(d,id.vars = c("from", "to"))

d$value[is.na(d$value)]<-0
reg<-"national"
for(reg in c("national", "regional"[dgt==2])){ 
  
  sic<-sic0
  if(reg=="regional"){sic$csic<-ifelse(!is.na(sic$csic),paste0(sic$csic,"_",str_split(sic$sic,"_",simplify = T)[,2]), NA)}
  
  # Aggregate data to National Accounts level
  for(time in c("year","quarter","month")){
    d0<-d
    #d0<-d0[(d0$value>0 & !is.na(d0$value) & d0$from != "0" & d0$to != "0"),]
    # For aggregation by year/quarter, take row-sums of columns with data from the same year/quarter
    if(time!="month"){
      # Before aggregation, convert time id into more aggregate groups (year or quarter)
      if(time=="year"){
        d0$variable<-substr(d0$variable,5,10) 
      }else if(time=="quarter"){
        d0$variable<-str_replace(d0$variable,"jan|feb|mar","Q1")
        d0$variable<-str_replace(d0$variable,"apr|may|jun","Q2")
        d0$variable<-str_replace(d0$variable,"jul|aug|sep","Q3")
        d0$variable<-str_replace(d0$variable,"oct|nov|dec","Q4")
      }
    }
    if(sum(is.na(sic$csic))>0 && sum(substr(sic$sic[is.na(sic$csic)],1,1)!="0")>0 && dgt != 5){stop(paste("Some of your sector codes are not mapped to an",aggr,"code. Check concordance table"))}
    # Replace original 2- or 5-digit codes in Payment by the more aggregate codes to be aggregated to (e.g.  or Region80)
    d0$from<-sic$csic[match(d0$from, sic$sic)]
    d0$to<-sic$csic[match(d0$to, sic$sic)]
    # Remove non-matched industries
    d0<-d0[which(!is.na(d0$from) & !is.na(d0$to) & !is.na(d0$value)),]
    # Now, aggregate by time, industry, and potentially also by region
    print(Sys.time())
    d0<-aggregate(d0[,"value"], by=list(from=d0[,"from"], to=d0[,"to"], variable=d0[,"variable"]), FUN=sum)
    print(Sys.time()) 
    d0<-reshape(d0, idvar = c("from", "to"), direction = "wide", timevar = "variable", sep = "_")
    colnames(d0)<-str_remove_all(colnames(d0),"x_")
    # Swap from and to to get network of flow of goods instead of money
    colnames(d0)[match(c("from","to"),colnames(d0))]<-c("to","from")
    save(d0, file = paste0(datadir,"/IOT_flow_of_goods_Payment_",time,"ly_",reg,"_level_",aggr,".RData"))
    #stop("I recommend to run .rs.restartR() as the routines before mess up the memory. Sorry, bug in R")
    
    # For annual and national data, add data from ONS IOTs
    if(time=="year" && reg=="national"){
      # Now, transform data when needed and create data frame with various types of ONS data
      if(exists("dfons")){rm(dfons)}
      for(y in c(2021:1997)){
        for(ONS_data in c("IxI", "PxP", "SUT")){
          if((y > 2019 | y %in% c(1997:2009,2011,2012)) && ONS_data != "SUT"){next}
          
          # Set column for matching with concordance (will be changed to "A64" for y == 2016 && ONS_data == PxP)
          cl<-"CPA"
          cpa<-na.omit(unique(conc[,cl]))
          # Load ONS national accounts data
          if(ONS_data == "IxI" && y %in% c(2018:2019)){
            # 1.) Input output analytical table industry by industry
            IOT<-read_excel(paste0("public_data/NA_data/nasu1719in_",y,".xlsx"), sheet="IOT",skip=3)
            IOT$...1<-str_replace_all(IOT$...1,"_","-")
            colnames(IOT)<-str_replace_all(colnames(IOT),"_","-")
            IOT$...1[IOT$...1=="C1101T1106 & C12"]<-colnames(IOT)[colnames(IOT)=="C1101T1106 & C12"]<-"C11.01-6 & C12"
            IOT$...1[IOT$...1=="C241T243"]<-colnames(IOT)[colnames(IOT)=="C241T243"]<-"C241-3"
            IOT$...1[IOT$...1=="F41, F42  & F43"]<-colnames(IOT)[colnames(IOT)=="F41, F42  & F43"]<-"F41, F42 & F43"
            IOT$...1[IOT$...1=="H493T495"]<-colnames(IOT)[colnames(IOT)=="H493T495"]<-"H493-5"
            # Keep only relevant rows and columns
            warning(paste("The following rows will be dropped:",paste(IOT$...1[!(IOT$...1 %in% cpa)],collapse = ", "),ONS_data,y))
            IOT<-as.data.frame(IOT[IOT$...1 %in% cpa,])
            rownames(IOT)[!is.na(IOT$...1)]<-IOT$...1[!is.na(IOT$...1)]
            warning(paste("The following columns will be dropped:",paste(colnames(IOT)[!(colnames(IOT) %in% cpa)],collapse = ", "),ONS_data,y))
            IOT<-IOT[,colnames(IOT) %in% cpa]
            if(sum(rownames(IOT) != colnames(IOT))>0){stop(paste("Rows and columns are not equal",ONS_data))}
          }else if(ONS_data == "PxP" && y %in% c(2020:2013,2010)){
            # if desired aggregation is CPA_2digit, 2016 needs to be dropped
            if(y == 2016 && aggr %in% c("CPA", "CPA_gdp_data", "CPA_2digit", "Region80", "A16")){next}
            
            if(y %in% c(2012:2011)){stop("This code should be skipped.")}
            # 2.) Input output analytical table product by product
            if(y %in% c(2010,2013:2016)){skp<-5}else{skp<-3}
            if(y %in% c(2015:2020)){IOT<-read_excel(paste0("public_data/NA_data/nasu1719pr_",y,".xls","x"[y!=2015]), sheet="IOT",skip=skp)
            }else if(y == 2014){IOT<-read_excel(paste0("public_data/NA_data/inputoutputanalyticaltables2014.xls"), sheet="IOT",skip=skp)
            }else if(y == 2013){IOT<-read_excel(paste0("public_data/NA_data/ukioanalyticaltables2013.xls"), sheet="IOT",skip=skp)
            
            }else if(y == 2010){
              IOT<-read_excel(paste0("public_data/NA_data/ukioanalyticaltablesio1062010detailedpubversion.xls"), sheet="IOT",skip=skp)}
            
            #excel_sheets(paste0("public_data/NA_data/nasu1719pr_",y,".xls","x"[y!=2015]))
            # Keep only relevant rows and columns
            IOT$...1<-str_remove_all(IOT$...1,"CPA_")
            if(y %in% c(2010,2013:2015)){
              sic15<-IOT$...1
              sic15<-str_replace_all(sic15,"-","_")
              sic15<-str_remove_all(sic15,"\\.")
              sic15[sic15 %in% c("01","02","03")]<-paste0("A",sic15[sic15 %in% c("01","02","03")])
              sic15[sic15 %in% c("05","08","09")]<-paste0("B",sic15[sic15 %in% c("05","08","09")])
              sic15[sic15 %in% c("06&07", "06 & 07")]<-"B06 & B07"
              sic15[substr(sic15,1,1)%in%c("1","2","3")]<-paste0("C",sic15[substr(sic15,1,1)%in%c("1","2","3")])
              sic15[sic15%in%c("C1101_6 and 12","C1101_6","C12")]<-"C11.01-6 & C12"
              sic15[substr(sic15,2,3)%in%c("35")]<-str_replace_all(sic15[substr(sic15,2,3)%in%c("35")],"C","D")
              sic15[substr(sic15,2,3)%in%c("36","37","38","39")]<-str_replace_all(sic15[substr(sic15,2,3)%in%c("36","37","38","39")],"C","E")
              sic15[sic15=="41_43"]<-"F41, F42 & F43"
              sic15[substr(sic15,1,2)%in%c(paste(45:47))]<-paste0("G",sic15[substr(sic15,1,2)%in%c(paste(45:47))])
              sic15[substr(sic15,1,2)%in%c(paste(49:53))]<-paste0("H",sic15[substr(sic15,1,2)%in%c(paste(49:53))])
              sic15[substr(sic15,1,2)%in%c(paste(54:56))]<-paste0("I",sic15[substr(sic15,1,2)%in%c(paste(54:56))])
              sic15[substr(sic15,1,2)%in%c("58",paste(61:63))]<-paste0("J",sic15[substr(sic15,1,2)%in%c("58",paste(61:63))])
              sic15[substr(sic15,1,2)%in%c("64","66")]<-paste0("K",sic15[substr(sic15,1,2)%in%c("64","66")])
              sic15[substr(sic15,1,2)%in%c("68")]<-paste0("L",sic15[substr(sic15,1,2)%in%c("68")])
              sic15[sic15=="L681_2"]<-"L68BXL683"
              sic15[sic15=="L682IMP"]<-"L68A"
              sic15[sic15=="L681_2"]<-"L68BXL683"
              sic15[sic15=="59_60"]<-"J59 & J60"
              sic15[sic15 %in% c("65", "651_3")]<-"K65.1-2 & K65.3"
              sic15[substr(sic15,1,2)%in%c(paste(69:75))]<-paste0("M",sic15[substr(sic15,1,2)%in%c(paste(69:75))])
              sic15[substr(sic15,1,2)%in%c(paste(77:82))]<-paste0("N",sic15[substr(sic15,1,2)%in%c(paste(77:82))])
              sic15[substr(sic15,1,2)%in%c(paste(84))]<-paste0("O",sic15[substr(sic15,1,2)%in%c(paste(84))])
              sic15[substr(sic15,1,2)%in%c(paste(85))]<-paste0("P",sic15[substr(sic15,1,2)%in%c(paste(85))])
              sic15[substr(sic15,1,2)%in%c(paste(90:93))]<-paste0("R",sic15[substr(sic15,1,2)%in%c(paste(90:93))])
              sic15[substr(sic15,1,2)%in%c(paste(86))]<-paste0("Q",sic15[substr(sic15,1,2)%in%c(paste(86))])
              sic15[substr(sic15,1,2)%in%c(paste(94:96))]<-paste0("S",sic15[substr(sic15,1,2)%in%c(paste(94:96))])
              sic15[substr(sic15,1,2)%in%c(paste(97))]<-paste0("T",sic15[substr(sic15,1,2)%in%c(paste(97))])
              sic15[sic15=="87_88"]<-"Q87 & Q88"
              
              sic15<-str_replace_all(sic15,"_","-")
              if(y == 2010){
                colnames(IOT)[str_detect(colnames(IOT),"\\.\\.\\.")]<-str_split(colnames(IOT)[str_detect(colnames(IOT),"\\.\\.\\.")],"\\.\\.\\.",simplify = T)[,1]
                colnames(IOT)[1]<-"...1"
              }
              colnames(IOT)[match(IOT$...1[IOT$...1 %in% colnames(IOT)],colnames(IOT))]<-sic15[IOT$...1 %in% colnames(IOT)]
              IOT$...1<-sic15
            }else if(y %in% c(2016,2014)){
              cl<-"A64"
              cpa<-na.omit(unique(conc$A64))
            }
            
            IOT[,3:ncol(IOT)]<-apply(IOT[,3:ncol(IOT)],2,FUN=function(x){return(as.numeric(as.character(x)))})
            IOT<-as.data.frame(IOT)
            
            # Make some manual corrections for 2010
            if(y == 2010){
              i<-which(IOT$...1 == "C11.01-6 & C12")
              if(length(i)!=2){stop("Check this. Rows are not to be merged?")}
              IOT[i[1],3:ncol(IOT)]<-colSums(IOT[i,3:ncol(IOT)])
              IOT<-IOT[-i[2],]
              i<-which(colnames(IOT) == "C11.01-6 & C12")
              if(length(i)!=2){stop("Check this. Columns are not to be merged?")}
              IOT[,i[1]]<-rowSums(IOT[,i])
              IOT<-IOT[,-i[2]]
              # Drop rows after T97 (these seem non market purchases and are named differently in other years)
              IOT$...1[(which(IOT$...1 == "T97")+1):nrow(IOT)]<-paste0("other",IOT$...1[(which(IOT$...1 == "T97")+1):nrow(IOT)])
            }
            
            IOT$...1<-str_replace_all(IOT$...1,"_","-")
            warning(paste("The following rows will be dropped:",paste(IOT$...1[!(IOT$...1 %in% cpa)],collapse = ","),ONS_data,y))
            IOT<-as.data.frame(IOT[IOT$...1 %in% cpa,])
            
            rownames(IOT)<-IOT$...1
            colnames(IOT)<-str_remove_all(colnames(IOT),"CPA_")
            colnames(IOT)<-str_replace_all(colnames(IOT),"_","-")
            #
            warning(paste("The following columns will be dropped:",paste(colnames(IOT)[!(colnames(IOT) %in% cpa)],collapse = ","),ONS_data,y))
            IOT<-IOT[,colnames(IOT) %in% cpa]
            if(sum(rownames(IOT) != colnames(IOT))>0){stop(paste("Rows and columns are not equal",ONS_data))}
            rm(skp)
          }else if(ONS_data == "SUT"){
            # 3.) Supply and use table - intermediate consumption
            # #supublicationtablesbb23v2.xlsx
            IOT<-read_excel("public_data/NA_data/supublicationtablesbb23v2.xlsx", sheet=paste0("Table 2 - Int Con ",y),skip=3)
            #IOT<-read_excel("public_data/NA_data/publicationtablesbb22.xlsx", sheet=paste0("Table 2 - Int Con ",y),skip=3)
            # Keep only relevant rows and columns
            IOT$...1<-str_remove_all(IOT$...1,"CPA_")
            IOT$...1<-str_replace_all(IOT$...1,"_","-")
            warning(paste("The following rows will be dropped:",paste(IOT$...1[!(IOT$...1 %in% cpa)],collapse = ","),ONS_data,y))
            IOT<-as.data.frame(IOT[IOT$...1 %in% cpa,])
            rownames(IOT)<-IOT$...1
            colnames(IOT)<-str_remove_all(colnames(IOT),"CPA_")
            colnames(IOT)<-str_replace_all(colnames(IOT),"_","-")
            # Some rows are differently named than the columns
            colnames(IOT)[colnames(IOT) == "C1101T1106 & C12"]<-"C11.01-6 & C12"
            colnames(IOT)[colnames(IOT) == "C241T243"]<-"C241-3"
            colnames(IOT)[colnames(IOT) == "F41, F42  & F43"]<-"F41, F42 & F43"
            colnames(IOT)[colnames(IOT) == "H493T495"]<-"H493-5"
            warning(paste("The following columns will be dropped:",paste(colnames(IOT)[!(colnames(IOT) %in% cpa)],collapse = ","),ONS_data,y))
            IOT<-IOT[,colnames(IOT) %in% cpa]
            if(sum(rownames(IOT) != colnames(IOT))>0){stop(paste("Rows and columns are not equal",ONS_data))}
          }else{print(warning(paste("Data or code not available for", ONS_data, y))); next}
          
          print(paste("column sum for T97: ", sum(as.numeric(as.character(IOT$T97))), "row sum: ", sum(as.numeric(as.character(IOT[which(rownames(IOT)=="T97"),]))), y, ONS_data))
          IOT<-t(IOT)
          
          # Change matrix style to edgelist
          g<-graph.adjacency(as.matrix(IOT), weighted = T) # make graph using igraph package
          iot<-get.data.frame(g) # get edgelist
          rm(g, IOT)
          # Replace pre-existing, too disaggregate NA codes by 2-digit compatible codes
          if(aggr=="2digit"){
            iot$from<-conc$CPA_2digit[match(iot$from, conc[,cl])]
            iot$to<-conc$CPA_2digit[match(iot$to, conc[,cl])]
          }else{
            iot$from<-conc[match(iot$from, conc[,cl]),aggr]
            iot$to<-conc[match(iot$to, conc[,cl]),aggr]
          }
          # Aggregate whenever needed
          iot<-iot[!is.na(iot$weight),]
          iot$from_to<-paste0(iot$from,"__",iot$to)
          dfiot<-aggregate(iot[,3], by=list(from=iot[,"from"],to=iot[,"to"]), FUN=sum)
          
          # Make little check whether electricity has more from or to sectors
          #print(paste("Number of customers of electricity (sum(dfiot$to==24)", sum(dfiot$to=="24")))
          #print(paste("Number of suppliers of electricity sector (sum(dfiot$from==24)", sum(dfiot$from=="24")))
          
          colnames(dfiot)[3]<-paste0("ONS_",ONS_data,"_",y)
          dfiot$from_to<-paste0(dfiot$from,"_",dfiot$to)
          if(!exists("dfons")){dfons<-dfiot}else{dfons<-merge(dfons,dfiot[,!(colnames(dfiot)%in%c("from","to"))],by="from_to",all.x=T,all.y=T)}
          if(sum(is.na(dfons$from))>0 || sum(is.na(dfons$to)>0)){
            dfons$from[is.na(dfons$from)]<-str_split(dfons$from_to[is.na(dfons$from)],pattern="_",simplify=T)[,1]
            dfons$from[is.na(dfons$to)]<-str_split(dfons$from_to[is.na(dfons$to)],pattern="_",simplify=T)[,1]
            dfons$to[is.na(dfons$from)]<-str_split(dfons$from_to[is.na(dfons$to)],pattern="_",simplify=T)[,2]
            dfons$to[is.na(dfons$to)]<-str_split(dfons$from_to[is.na(dfons$to)],pattern="_",simplify=T)[,2]
          }
          rm(dfiot)
        } # for ONS_data in c("IxI", "PxP", "SUT")
      } # for y in 2020:1997
      
      print(paste("length from:", length(unique(dfons$from)), "length to:", length(unique(dfons$to))))
      print(paste(sort(unique(c(dfons$from, dfons$to)))))
      
      # Swap from and to to get network of flow of goods instead of money
      colnames(d0)[match(c("from","to"),colnames(d0))]<-c("to","from")
      
      # Merge the data with Payment edgelist
      d0$from_to<-paste0(d0$from, "_", d0$to)
      colnames(d0)[!(colnames(d0)%in%c("from","to","from_to"))]<-paste0("Payment_",colnames(d0)[!(colnames(d0)%in%c("from","to","from_to"))])
      colnames(d0)[!(colnames(d0)%in%c("from","to","from_to"))]<-str_replace_all(colnames(d0)[!(colnames(d0)%in%c("from","to","from_to"))],"amt_","amt_20")
      colnames(d0)[!(colnames(d0)%in%c("from","to","from_to"))]<-str_replace_all(colnames(d0)[!(colnames(d0)%in%c("from","to","from_to"))],"cnt_","cnt_20")
      
      # Merge
      df<-merge(dfons, d0[,!(colnames(d0)%in%c("from","to"))], by="from_to", all = T)
      if(is.element(NA, df$from) || is.element(NA, df$to)){
        df$from[is.na(df$from)]<-d0$from[match(df$from_to[is.na(df$from)],d0$from_to)]
        df$to[is.na(df$to)]<-d0$to[match(df$from_to[is.na(df$to)],d0$from_to)]
      }
      if(is.element(NA, df$from) || is.element(NA, df$to)){stop("you risk dropping meaningful rows (or there is a bug before)")}
      df<-df[,colnames(df)!="from_to"] # drop dummy column from_to
      df[3:ncol(df)]<-apply(df[3:ncol(df)],2,FUN=function(x){return(ifelse(is.na(x),0,x))})
      # Divide Payment AMT by £1m to harmonize with ONS table format
      df[,str_detect(colnames(df),"amt")]<-df[,str_detect(colnames(df),"amt")]*(10^-6)
      # Assign sector names for convenience
      df$from_names<-conc[match(df$from,conc[,aggr]),paste0(aggr,"_names")]
      df$to_names<-conc[match(df$to,conc[,aggr]),paste0(aggr,"_names")]
      # Order columns by year
      cn<-colnames(df)
      cn<-cn[str_detect(cn, "[:digit:]")]
      o<-order(str_remove_all(cn, "[:^digit:]"))
      df<-df[,c("from", "to", "from_names", "to_names", cn[o])]
      #save(df, file=paste0("national_account_data/IOT_edgelist_ONS_Payment_",aggr,".RData"))
      # Swap from and to to get network of flow of goods instead of money
      colnames(df)[1:4]<-c("to","from","to_names","from_names")
      save(df, file=paste0(datadir,"/IOT_flow_of_goods_ONS_Payment_",aggr,".RData"))
      rm(o, cn)
      
    } # if time=="year" && reg=="national"
      
  
    
  } # for time in year, quarter, month
} # reg in c("national", "regional"[dgt==2])
  
 








