# This script produces the raw data used for the GDP nowcasting project from the publicly available 2-digit files
# 
# Data source: 
# - https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/industrytoindustrypaymentflowsukexperimentaldataandinsights
# 
# 
# Documentation: 
# - https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/industrytoindustrypaymentflowsuk/2016to2023experimentaldataandinsights
# - https://www.ons.gov.uk/economy/economicoutputandproductivity/output/methodologies/industrytoindustrymonthlypaymentsukqmi
# 
# 
rm(list = ls())
.rs.restartR()

library(readxl); library(data.table); library(stringr); library(reshape2)

# Set version of dataset to be used
dtversion<-"Dec2023_public"

fn<-paste0("payment_data/",dtversion,"/ukindustrytoindustrypaymentflowsjanuary2016tooctober2023.xlsx")
# Get list of available sheets
fls<-excel_sheets(fn)
fls<-fls[str_detect(fls,"Table ")]
for(fl in fls){
  x0<-read_excel(fn,sheet = fl,skip = 5)
  if(fl==fls[1]){x<-x0}else{x<-rbind(x,x0)}
}
rm(x0,fl,fn,fls)
# Clean and format data
colnames(x)<-c("from","to","time","amt","cnt")
x$amt<-as.numeric(as.character(x$amt))
x$cnt<-as.numeric(as.character(x$cnt))

load("public_data/concordances/concordances.RData")

x$from<-conc$CPA_public[match(x$from,conc$SIC_public)]
x$to<-conc$CPA_public[match(x$to,conc$SIC_public)]

x<-melt(x,id.vars = c("from","to","time"))
x$time<-paste(str_to_lower(substr(x$time,1,3)),x$variable,str_split(x$time," ",simplify=T)[,2],sep="_")
x2<-reshape(x[,colnames(x)!="variable"],idvar=c("from","to"),timevar = "time", direction = "wide")
colnames(x2)<-str_remove_all(colnames(x2),"value\\.")


d0<-x2
rm(x2)
dtdir<-paste0("Payment_data/",dtversion,"/national_account_data/")
if(!dir.exists(dtdir)){dir.create(dtdir,recursive = T)}

# To have flow of goods instead of flow of money, swap from and to column names
colnames(d0)[1:2]<-c("to","from")
save(d0, file=paste0(dtdir,"IOT_flow_of_goods_Payment_monthly_CPA_public.RData"))


# Create industry level panel and supplement with GDP data

# Inputs: aggregate by to-time
sum_inputs<-aggregate(d0$value,by=list(code=d0$to, time=d0$time),FUN=function(x){return(sum(x,na.rm = T))})
# Outputs: aggregate by from-time
sum_outputs<-aggregate(d0$value,by=list(code=d0$from, time=d0$time),FUN=function(x){return(sum(x,na.rm = T))})

tm<-"month"

rm(d0,df)
tp<-dtypes[1]
for(tp in dtypes){
  cl<-paste0("sum_inputs_",tp)
  si<-sum_inputs[str_detect(sum_inputs$time,tp),]
  si$time<-str_remove_all(si$time,paste0(tp,"_"))
  i<-match(paste(dt$code,dt$time),paste(si$code,si$time))
  if(sum(is.na(i))>0 && tm!="year"){stop("Check matching")}
  dt[,cl]<-si$x[i]
  cl<-paste0("sum_outputs_",tp)
  
  si<-sum_outputs[str_detect(sum_outputs$time,tp),]
  si$time<-str_remove_all(si$time,paste0(tp,"_"))
  i<-match(paste(dt$code,dt$time),paste(si$code,si$time))
  if(sum(is.na(i))>0 && tm!="year"){stop("Check matching")}
  dt[,cl]<-si$x[i]
}
rm(tp, dtypes, cl, si, sum_inputs, sum_outputs, cn)

if(tm == "month"){
  dt$time<-paste0(substr(dt$time,1,3),"-20",substr(dt$time,5,6))
  dt$time<-as.yearmon(dt$time, format="%b-%Y")
}else if(tm == "quarter"){
  dt$time<-str_replace_all(dt$time, "_", " 20")
  dt$time<-as.yearqtr(dt$time, format = "Q%q %Y")
}

dt<-dt[order(dt$time),]

if(reg == "national"){
  if(aggr %in% c("CPA_gdp_data", "A16")){
    # The following two files can be mapped to 99 CPA_gdp industries
    # Add non-seasonally adjusted GDP
    gdp_nsa<-read_excel("../../public_data/macro_data/indicativemonthlygdpnsapubl.xlsx",sheet = "Monthly Index",skip=4)
    gdp_nsa<-gdp_nsa[,colnames(gdp_nsa) %in% c("Time period and dataset code row", conc$gdp_nsa_codes)]
    gdp_nsa<-melt(gdp_nsa, id.vars = c("Time period and dataset code row"))
    colnames(gdp_nsa)<-c("time", "code", "gdp_nsa")
    gdp_nsa$code<-conc[match(gdp_nsa$code, conc$gdp_nsa_codes),aggr]
    if(tm == "quarter"){
      gdp_nsa$time<-paste0(str_to_title(substr(gdp_nsa$time,5,7)),"-", substr(gdp_nsa$time,1,4))
      gdp_nsa$time<-as.yearmon(gdp_nsa$time, format="%b-%Y")
      gdp_nsa$time<-as.yearqtr(gdp_nsa$time)
    }else if(tm == "year"){
      gdp_nsa$time<-as.numeric(substr(gdp_nsa$time,1,4))
    }
    gdp_nsa<-aggregate(gdp_nsa$gdp_nsa, by=list(gdp_nsa$time, gdp_nsa$code), FUN=sum)
    colnames(gdp_nsa)<-c("time", "code", "gdp_nsa")
    # Add seasonally adjusted GDP
    gdp_sa<-read_excel("../../public_data/macro_data/indicativemonthlydataset_seasonally_adjusted.xlsx", sheet = "OUTPUT Monthly Index", skip = 4)
    gdp_sa<-gdp_sa[,colnames(gdp_sa) %in% c("Time period and dataset code row", conc$gdp_sa_codes)]
    gdp_sa<-melt(gdp_sa, id.vars = c("Time period and dataset code row"))
    colnames(gdp_sa)<-c("time", "code", "gdp_sa")
    gdp_sa$code<-conc[match(gdp_sa$code, conc$gdp_sa_codes),aggr]
    if(tm == "quarter"){
      gdp_sa$time<-paste0(str_to_title(substr(gdp_sa$time,5,7)),"-", substr(gdp_sa$time,1,4))
      gdp_sa$time<-as.yearmon(gdp_sa$time, format="%b-%Y")
      gdp_sa$time<-as.yearqtr(gdp_sa$time)
    }else if(tm == "year"){
      gdp_sa$time<-as.numeric(substr(gdp_sa$time,1,4))
    }
    
    gdp_sa<-aggregate(gdp_sa$gdp_sa, by=list(gdp_sa$time, gdp_sa$code), FUN=sum)
    colnames(gdp_sa)<-c("time", "code", "gdp_sa")
    
    # Merge deseasonalised and non-deseasonalised GDP data
    gdp<-merge(gdp_nsa, gdp_sa, by=c("time", "code"), all=T)
    if(tm=="month"){
      gdp$time<-paste0(str_to_title(substr(gdp$time,5,7)),"-", substr(gdp$time,1,4))
      gdp$time<-as.yearmon(gdp$time, format="%b-%Y")
    }
    dt<-merge(dt, gdp, by=c("time", "code"), all=T)
    
  }
  
  if(tm == "year"){
    # Now, source data from ONS, apply concordance, and add to data frame
    dt0<-dt
    dt$code_year<-paste0(dt$code,"_",dt$time)
    for(ONS_data in c("SUT","IxI","PxP")){
      if(exists("add")){rm(add)}
      
      for(y in c(2020:1997)){
        if((y > 2019 | y %in% c(1997:2009,2011,2012)) && ONS_data != "SUT"){next}
        
        # Set column for matching with concordance (will be changed to "A64" for y == 2016 && ONS_data == PxP)
        cl<-"CPA"
        cpa<-na.omit(unique(conc[,cl]))
        # Load ONS national accounts data
        if(ONS_data == "IxI" && y %in% c(2018:2019)){
          # 1.) Input output analytical table industry by industry
          IOT<-as.data.frame(read_excel(paste0("../../public_data/NA_data/nasu1719in_",y,".xlsx"), sheet="IOT",skip=3))
          IOT$...1[IOT$...1=="C1101T1106 & C12"]<-colnames(IOT)[colnames(IOT)=="C1101T1106 & C12"]<-"C11.01-6 & C12"
          IOT$...1[IOT$...1=="C241T243"]<-colnames(IOT)[colnames(IOT)=="C241T243"]<-"C241_3"
          IOT$...1[IOT$...1=="F41, F42  & F43"]<-colnames(IOT)[colnames(IOT)=="F41, F42  & F43"]<-"F41, F42 & F43"
          IOT$...1[IOT$...1=="H493T495"]<-colnames(IOT)[colnames(IOT)=="H493T495"]<-"H493_5"
          # Keep only relevant rows and columns
          d1<-IOT[,colnames(IOT) %in% c("...1","P3 S1","P3 S13","P3 S14","P3 S15","P51G","P52","P53","P61EU","P61RW","P62","TU")]
          colnames(d1)<-c("code",d1[1,2:ncol(d1)])
          d1$code<-str_replace_all(d1$code,"_","-")
          warning(paste("The following rows will be dropped:",paste(d1$code[!(d1$code %in% cpa)],collapse = ", "),ONS_data,y))
          d1<-d1[d1$code %in% cpa,]
          d2<-as.data.frame(IOT[IOT$SIC %in% c("Total intermediate use at basic prices","Use of imported products, cif","Taxes less subsidies on products","Total intermediate use at purchaser's prices","Compensation of employees","Gross operating surplus and mixed income","Taxes less subsidies on production","Gross value added","Total output at basic prices"),])
          rownames(d2)<-d2$SIC
          colnames(d2)<-str_replace_all(colnames(d2),"_","-")
          warning(paste("The following columns will be dropped:",paste(colnames(d2)[!(colnames(d2) %in% cpa)],collapse = ", "),ONS_data,y))
          d2<-d2[,colnames(d2) %in% cpa]
          cn<-colnames(d2)
          d2<-as.data.frame(t(d2))
          d2$code<-rownames(d2)
          iot<-merge(d1,d2,by="code",all = T)
          rownames(iot)<-iot$code
          rm(d1,d2,IOT)
        }else if(ONS_data == "PxP" && y %in% c(2020:2013,2010)){
          if(y<2015){print(warning(paste(y,ONS_data,"skipped as data might be inconsistent and not sure whether it's worth the effort adding it."))); next}
          # if desired aggregation is CPA_2digit, 2016 needs to be dropped
          if(y == 2016 && aggr %in% c("CPA_2digit","A16","CPA", "CPA_gdp_data", "Region80")){next}
          
          if(y %in% c(2012:2011)){stop("This code should be skipped.")}
          # 2.) Input output analytical table product by product
          if(y %in% c(2010,2013:2016)){skp<-5}else{skp<-3}
          if(y %in% c(2015:2020)){IOT<-read_excel(paste0("../../public_data/NA_data/nasu1719pr_",y,".xls","x"[y!=2015]), sheet="IOT",skip=skp)
          }else if(y == 2014){IOT<-read_excel(paste0("../../public_data/NA_data/inputoutputanalyticaltables2014.xls"), sheet="IOT",skip=skp)
          }else if(y == 2013){IOT<-read_excel(paste0("../../public_data/NA_data/ukioanalyticaltables2013.xls"), sheet="IOT",skip=skp)
          
          }else if(y == 2010){
            IOT<-read_excel(paste0("../../public_data/NA_data/ukioanalyticaltablesio1062010detailedpubversion.xls"), sheet="IOT",skip=skp)}
          
          #excel_sheets(paste0("../../public_data/NA_data/nasu1719pr_",y,".xls","x"[y!=2015]))
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
            if(y == 2010){
              colnames(IOT)[str_detect(colnames(IOT),"\\.\\.\\.")]<-str_split(colnames(IOT)[str_detect(colnames(IOT),"\\.\\.\\.")],"\\.\\.\\.",simplify = T)[,1]
              colnames(IOT)[1]<-"...1"
            }
            colnames(IOT)[match(IOT$...1[IOT$...1 %in% colnames(IOT)],colnames(IOT))]<-sic15[IOT$...1 %in% colnames(IOT)]
            IOT$...1<-sic15
            
            IOT<-as.data.frame(IOT)
            
            if(y == 2015){
              crm<-which(IOT[1,]=="Local Authorities"); ctk<-which(IOT[1,]=="Central Government")
              IOT[2:nrow(IOT),ctk]<-as.numeric(IOT[2:nrow(IOT),ctk])+as.numeric(IOT[2:nrow(IOT),crm])
              colnames(IOT)[ctk]<-"P3 S13"; IOT[1,ctk]<-"Final consumption expenditure by government"
              IOT<-IOT[,-crm]
              crm<-which(IOT[1,]=="Exports of Services - Non EU"); ctk<-which(IOT[1,]=="Exports of Services - EU")
              IOT[2:nrow(IOT),ctk]<-as.numeric(IOT[2:nrow(IOT),ctk])+as.numeric(IOT[2:nrow(IOT),crm])
              colnames(IOT)[ctk]<-"P62"; IOT[1,ctk]<-"Exports of services"
              IOT<-IOT[,-crm]
              rm(crm, ctk)
              ce<-which(str_detect(IOT[1,],"Non-Profit Institutions Serving Households"))
              IOT[1,ce]<-"Final consumption expenditure by non-profit organisations serving households (NPISH)"; colnames(IOT)[ce]<-"P3 S15"
              ce<-which(str_detect(IOT[1,],"Households"))
              IOT[1,ce]<-"Final consumption expenditure by households"; colnames(IOT)[ce]<-"P3 S14"
              ce<-which(str_detect(IOT[1,],"Gross Fixed Capital Formation"))
              IOT[1,ce]<-"Gross fixed capital formation"; colnames(IOT)[ce]<-"P51G"
              ce<-which(str_detect(IOT[1,],"Changes in Inventories"))
              IOT[1,ce]<-"Changes in inventories"; colnames(IOT)[ce]<-"P52"
              ce<-which(str_detect(IOT[1,],"Valuables"))
              IOT[1,ce]<-"Acquisitions less disposals of valuables"; colnames(IOT)[ce]<-"P53"
              ce<-which(str_detect(IOT[1,],"Exports of Goods - EU"))
              IOT[1,ce]<-"Exports of goods to EU"; colnames(IOT)[ce]<-"P61EU"
              ce<-which(str_detect(IOT[1,],"Exports of Goods - Non EU"))
              IOT[1,ce]<-"Exports of goods to rest of the world"; colnames(IOT)[ce]<-"P61RW"
              ce<-which(str_detect(IOT[1,],"Total Demand for Products at Basic Prices"))
              IOT[1,ce]<-"Total Use at basic prices"; colnames(IOT)[ce]<-"TU"
              IOT$`P3 S1`<-NA
              IOT$`P3 S1`[1]<-"Final consumption expenditure"
              IOT$`P3 S1`[2:nrow(IOT)]<-as.numeric(IOT$`P3 S13`[2:nrow(IOT)])+as.numeric(IOT$`P3 S14`[2:nrow(IOT)])+as.numeric(IOT$`P3 S15`[2:nrow(IOT)])
              colnames(IOT)[2]<-"CPA"
            }
            
          }else if(y %in% c(2016,2014)){
            cl<-"A64"
            cpa<-na.omit(unique(conc$A64))
          }
          
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
          
          
          # Keep only relevant rows and columns
          d1<-IOT[,colnames(IOT) %in% c("...1","P3 S1","P3 S13","P3 S14","P3 S15","P51G","P52","P53","P61EU","P61RW","P62","TU","TD")]
          colnames(d1)<-c("code",d1[1,2:ncol(d1)])
          d1$code<-str_replace_all(d1$code,"_","-")
          warning(paste("The following rows will be dropped:",paste(d1$code[!(d1$code %in% cpa)],collapse = ", "),ONS_data,y))
          d1<-d1[d1$code %in% cpa,]
          
          # Cosmetics whenever needed to merge tables later & harmonize order of columns
          colnames(d1)<-str_replace_all(colnames(d1),"changes in inventories","Changes in inventories")
          d1<-d1[,c("code","Final consumption expenditure","Final consumption expenditure by government","Final consumption expenditure by households","Final consumption expenditure by non-profit organisations serving households (NPISH)","Gross fixed capital formation","Changes in inventories","Acquisitions less disposals of valuables","Exports of goods to EU","Exports of goods to rest of the world","Exports of services","Total Use at basic prices")]
          IOT$CPA[IOT$CPA %in% c("Total", "Total intermediate/final use at basic prices","Total Consumption")]<-"Total intermediate use at basic prices"
          IOT$CPA[IOT$CPA %in% c("Total intermediate/final use at purchaser's prices")]<-"Total intermediate use at purchaser's prices"
          IOT$CPA[IOT$CPA %in% c("Imports of goods and services")]<-"Use of imported products, cif"
          #IOT[IOT$CPA=="Taxes less subsidies on products",which(colnames(IOT)%in%cpa)]<-as.numeric(IOT[IOT$CPA=="Taxes less subsidies on products",which(colnames(IOT)%in%cpa)])+as.numeric(IOT[IOT$CPA=="Taxes less subsidies on production",which(colnames(IOT)%in%cpa)])
          #IOT<-IOT[IOT$CPA!="Taxes less subsidies on production",]
          IOT$CPA[IOT$CPA %in% c("Gross operating surplus")]<-"Gross operating surplus and mixed income"
          IOT$CPA[IOT$CPA %in% c("Total Output")]<-"Total output at basic prices"
          
          d2<-as.data.frame(IOT[IOT$CPA %in% c("Total intermediate use at basic prices","Use of imported products, cif","Taxes less subsidies on products","Total intermediate use at purchaser's prices","Compensation of employees","Gross operating surplus and mixed income","Taxes less subsidies on production","Gross value added","Total output at basic prices"),])
          rownames(d2)<-d2$CPA
          colnames(d2)<-str_remove_all(colnames(d2),"CPA_")
          
          colnames(d2)<-str_replace_all(colnames(d2),"_","-")
          warning(paste("The following columns will be dropped:",paste(colnames(d2)[!(colnames(d2) %in% cpa)],collapse = ", "),ONS_data,y))
          d2<-d2[,colnames(d2) %in% cpa]
          cn<-colnames(d2)
          d2<-as.data.frame(t(d2))
          d2$code<-rownames(d2)
          if(y == 2015){d2$"Gross value added"<-NA; d2$"Total intermediate use at purchaser's prices"<-NA}
          d2<-d2[,c("Total intermediate use at basic prices","Use of imported products, cif","Taxes less subsidies on products","Total intermediate use at purchaser's prices","Compensation of employees","Gross operating surplus and mixed income","Taxes less subsidies on production","Gross value added","Total output at basic prices","code")]
          iot<-merge(d1,d2,by="code",all = T)
          rownames(iot)<-iot$code
          rm(d1,d2,IOT,skp)
        }else if(ONS_data == "SUT"){
          # 3.) Supply and use table - intermediate consumption
          IOT<-read_excel("../../public_data/NA_data/publicationtablesbb22.xlsx", sheet=paste0("Table 2 - Int Con ",y),skip=3)
          # Keep only those rows with info on VA, GO, taxes, etc.
          IOT<-as.data.frame(IOT[match(c("Total intermediate consumption at purchasers' prices","Taxes less subsidies on production1","Compensation of employees","Gross operating surplus and mixed income","Gross valued added at basic prices","Total output at basic prices"), IOT$...2),])
          #rownames(IOT)<-c("Total intermCons (pp)","Net production taxes","Wagebill","Gross operating surplus","Gross valued added (bp)","Total output (bp)")
          rownames(IOT)<-c("Total intermediate consumption at purchasers' prices","Taxes less subsidies on production","Compensation of employees","Gross operating surplus and mixed income","Gross valued added at basic prices","Total output at basic prices")
          if(nrow(IOT)<6){stop("One of the variables not captured.")}
          # Keep only relevant columns
          colnames(IOT)<-str_remove_all(colnames(IOT),"CPA_")
          # Some rows are differently named than the columns
          colnames(IOT)[colnames(IOT) == "C1101T1106 & C12"]<-"C11.01-6 & C12"
          colnames(IOT)[colnames(IOT) == "C241T243"]<-"C241_3"
          colnames(IOT)[colnames(IOT) == "F41, F42  & F43"]<-"F41, F42 & F43"
          colnames(IOT)[colnames(IOT) == "H493T495"]<-"H493_5"
          colnames(IOT)<-str_replace_all(colnames(IOT),"_","-")
          warning(paste("The following columns will be dropped:",paste(colnames(IOT)[!(colnames(IOT) %in% cpa)],collapse = ","),ONS_data,y))
          IOT<-IOT[,colnames(IOT) %in% cpa]
          print(paste("column sum for T97: ", sum(as.numeric(as.character(IOT$T97)),na.rm = T), y, ONS_data))
          iot<-data.frame(t(IOT))
          # Ensure numeric data
          iot<-data.frame(apply(iot, 2, FUN=function(x){return(as.numeric(as.character(x)))}))
          rownames(iot)<-colnames(IOT)
          colnames(iot)<-rownames(IOT)
          
          sp<-as.data.frame(read_excel("../../public_data/NA_data/publicationtablesbb22.xlsx", sheet=paste0("Table 1 - Supply ",y),skip=2))
          sp<-sp[!is.na(sp$...1),c(1,3:ncol(sp))]
          colnames(sp)[1]<-"code"
          sp$code<-str_replace_all(str_remove_all(sp$code,"CPA_"),"_","-")
          dm<-as.data.frame(read_excel("../../public_data/NA_data/publicationtablesbb22.xlsx", sheet=paste0("Table 2 - Final Demand ",y),skip=2))
          dm<-dm[,c(1,which(as.character(dm[2,]) %in% c("Households","Non-profit\r\ninstitutions serving\r\nhouseholds","Central\r\ngovernment","Local\r\ngovernment","Total","Gross fixed\r\ncapital\r\nformation","Valuables","Changes in inventories","Total","Exports of goods to EU","Exports of goods to rest of the world","Total exports of Goods","Exports of Services","Total exports of goods and services","Total final demand","Total demand for products")))]
          colnames(dm)<-c("code",paste0(c(rep("Final consumption expenditure ",5),"",rep("Gross capital formation ",3),rep("",7)), as.character(dm[2,2:ncol(dm)])))
          dm<-dm[!is.na(dm$code),]
          dm$code<-str_replace_all(str_remove_all(dm$code,"CPA_"),"_","-")
          dm<-dm[1:105,]
          d<-merge(dm,sp,by="code",all = T)
          rm(sp,dm)
          
          # Merge data while preserving column names
          cn<-c(colnames(iot),colnames(d)[which(colnames(d)!="code")])
          rn<-rownames(iot)
          iot<-data.frame(cbind(iot,d[match(d$code,rownames(iot)),which(colnames(d)!="code")]))
          colnames(iot)<-cn; rownames(iot)<-rn
          rm(d,cn,rn,IOT)
          
          colnames(iot)<-str_replace_all(colnames(iot),"\r\n"," ")
          colnames(iot)<-str_replace_all(colnames(iot),"  "," ")
          
          
        }else{print(warning(paste("Data or code not available for", ONS_data, y))); next}
        
        # Create dummy vector to match too disaggregate NA codes with 2-digit compatible codes
        if(aggr=="2digit"){
          stop("Check this.")
        }else{iot$code<-conc[match(rownames(iot), conc[,cl]),aggr]}
        if(sum(!(iot$code %in% unique(dt$code)))){stop("Some codes not in panel header data.")}
        
        # Ensure numeric data
        iot[,(colnames(iot)!="code")]<-data.frame(apply(iot[,(colnames(iot)!="code")], 2, FUN=function(x){return(as.numeric(as.character(x)))}))
        
        # Aggregate whenever needed
        iot<-aggregate(iot[,(colnames(iot)!="code")], by=list(iot$code),FUN=sum)
        colnames(iot)[1]<-"code_year"
        iot$code_year<-paste0(iot$code_year,"_",y)
        
        
        # Create data frame that will be merged with dt
        if(!exists("add")){add<-iot}else{add<-rbind(add,iot)}
        
        rm(iot)
        
      } # y in 1997:2022
      
      colnames(add)[2:ncol(add)]<-paste(colnames(add)[2:ncol(add)],ONS_data)
      
      dt<-merge(dt,add,by="code_year",all = T)
    } # ONS_data in SUT, PxP, IxI
    
  } # if tm == year
  
}else if(reg == "regional"){ # add available regional data
  # Get region names to extract relevant rows from ONS data; harmonize names to make matching possible
  regions<-sort(unique(str_remove_all(dt$code, "[^a-z,A-Z, ]")))
  regions<-str_replace_all(regions, " the ", " The "); regions<-str_remove_all(regions, "Greater ")
  
  # Add industry-region, industry, region level indicators
  # 
  # 1.) Region-industry (Region80) level GVA
  # Source: https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry
  # Release date: 25/04/2023 [Download date: 17/01/2024]
  # Annual estimates of balanced UK regional gross value added (GVA(B)). Current price estimates, chained volume measures and implied deflators for UK countries, ITL1, ITL2 and ITL3 regions, with a detailed industry breakdown.
  #excel_sheets("../../public_data/regional_data/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx")
  gva<-read_excel("../../public_data/regional_data/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx", sheet="Table1b", skip=1)
  add<-melt(gva[,!(colnames(gva) %in% c("ITL region code","SIC07 description"))], id.vars = c("ITL region name", "SIC07 code"), value.name = "GVA volumes")
  gva<-read_excel("../../public_data/regional_data/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx", sheet="Table1c", skip=1)
  add2<-melt(gva[,!(colnames(gva) %in% c("ITL region code","SIC07 description"))], id.vars = c("ITL region name", "SIC07 code"), value.name = "GVA values")
  add<-merge(add, add2, by=c("ITL region name", "SIC07 code", "variable"),all=T)
  gva<-read_excel("../../public_data/regional_data/regionalgrossvalueaddedbalancedbyindustryandallitlregions.xlsx", sheet="Table1d", skip=1)
  add2<-melt(gva[,!(colnames(gva) %in% c("ITL region code","SIC07 description"))], id.vars = c("ITL region name", "SIC07 code"), value.name = "price deflators")
  gva<-merge(add, add2, by=c("ITL region name", "SIC07 code", "variable"),all=T)
  colnames(gva)[1:3]<-c("region", "code", "time")
  rm(add, add2)
  # Keep only rows with relevant industry codes
  if(aggr == "Region80"){
    gva<-gva[gva$code %in% unique(conc$Region80_code),]
  }else{stop("Check and possibly revise code")}
  # Apply concordance table
  gva$code<-conc$Region80[match(gva$code, conc$Region80_code)]
  gva<-aggregate(gva[,!(colnames(gva) %in% c("region", "code", "time"))],by=list(region=gva$region,code=gva$code,time=gva$time),FUN=function(x){return(sum(x,na.rm=T))})
  # Merge with data compiled before, including Payment data, while dropping some non-matched region-rows from GVA data.
  gva$code<-paste(gva$code,gva$region,sep="_")
  dt<-merge(dt, gva[gva$region %in% regions,colnames(gva)!="region"], by=c("code", "time"),all = T)
  rm(gva)
  
  # SKIP THE NEXT ONE FOR NOW: Northern Ireland and Scotland are not included in this data
  # Add sector level information on GDP
  # Annual GDP for England, Wales and the English regions
  # Source: https://www.ons.gov.uk/datasets/regional-gdp-by-year/editions/time-series/versions/6 
  # Release date: 18/05/2023 [Download date: 17/10/2024]
  # Annual economic activity within England, Wales and the nine English regions (North East, North West, Yorkshire and the Humber, East Midlands, West Midlands, East of England, Greater London, South East, South West).
  #excel_sheets("../../public_data/regional_data/regional-gdp-by-year-time-series-v5-filtered-2022-12-04.xlsx")
  #gdp<-read_excel("../../public_data/regional_data/regional-gdp-by-year-time-series-v6.xlsx", skip=2)
  #gdp<-gdp[,!str_detect(colnames(gdp),"Marking| code")]
  #gdp<-melt(gdp,id.vars = c("Geography","UnofficialStandardIndustrialClassification","GrowthRate","Prices"), value.name = "gdp")
  
  excel_sheets("../../public_data/regional_data/regionalgrossdisposablehouseholdincomeallitlregions.xlsx")
  
  
  
}

colnames(dt)<-str_replace_all(colnames(dt), "gdp_", "GVA ")

save(dt, file=paste0("national_account_data/",dgt,"_digit_data/industry_panel_Payment_ONS_",tm,"ly_",reg,"_level_",aggr,".RData"))

if(tm=="month"){dtm<-dt}else if(tm=="quarter"){dtq<-dt}else{dty<-dt}

















