### This script merges macro level data at the national level with external data from various sources ###
# 
# Data sets (from public_data/macro_data): 
# - OECD key economic indicators (Key Short-Term Economic Indicators https://stats.oecd.org//Index.aspx?QueryId=118961)
# - OECD monetary indicators (Monthly Monetary and Financial Statistics (MEI) https://stats.oecd.org/Index.aspx?DatasetCode=MEI_FIN)
# - Payment statistics from pay.uk for Bacs, FPS, CHAPS, Image Clearing https://www.wearepay.uk/wp-content/uploads/2024/01/Monthly-Payment-Statistics-1990-to-Dec-2023.xls
# - ONS non-seasonally adjusted GDP data: https://www.ons.gov.uk/economy/grossdomesticproductgdp/adhocs/14197indicativemonthlynonseasonallyadjustedgdp
# 
# - TBD: macro variables from ONS
# - Add also some monetary statistics manually (from pay.uk and BoE)
# 
# 
# 
# Note: Data availability differs dependent on whether monthly, quarterly, yearly data added. 
# 
# Steps: 
# 1.) Load aggregate Payment data at monthly, quarterly, yearly level choosing from data compiled on 2- and 5-digit level basis
# 2.) Add external data and save for different time aggregations: 
#   2.a) OECD data key economic indicators
#   2.b) OECD monetary financial indicators
#   2.c) Pay.uk payment system statistics on Bacs, CHAPS, FPS, Image Clearing
#   2.d) ONS GDP and national aggregates
# 
# 
# 
rm(list=ls())
.rs.restartR()
# Set version of dataset to be used
dtversion<-"Dec2023_public"
dtversion<-"Feb2024"

ddir<-paste0("payment_data/",dtversion,"/")

library(stringr); library(readr); library(reshape2); library(readxl); library(zoo); library(tidyr)


# Choose digit level for raw Payment data which should be used
dgt<-3
datadir<-paste0(ddir,"macro_aggregates/",paste0(dgt,"_digit_data/")[dtversion!="Dec2023_public"])
# Step 1: Load Payment data
load(paste0(datadir,"aggregate_amt_cnt_national_year.RData"))
dty<-dt
load(paste0(datadir,"aggregate_amt_cnt_national_quarter.RData"))
dtq<-dt
load(paste0(datadir,"aggregate_amt_cnt_national_month.RData"))
dtm<-dt
if(sum(str_detect(dtm$month,"t_20"))==length(dtm$month)){add<-""}else{add<-"20"}
if(sum(str_detect(dtq$quarter,"_amt_"))==length(dtq$quarter)){dtq$quarter<-str_to_title(str_replace_all(dtq$quarter, "_amt_", paste0("-",add)))}
if(sum(str_detect(dtm$month,"_amt_"))==length(dtm$month)){dtm$month<-str_to_title(str_replace_all(dtm$month, "_amt_", paste0("-",add)))}
# 
# 
# Step 2: 
# 
# 2.a) Load OECD data
oecd <- read_csv("public_data/macro_data/oecd_key_economic_indicators.csv", show_col_types = F)
tm<-"quarter"
for(tm in c("year", "quarter", "month")){
  if(tm == "year"){fr<-"Annual"; d<-dty}else if(tm == "quarter"){fr<-"Quarterly"; d<-dtq}else{fr<-"Monthly"; d<-dtm}
  dt<-oecd[oecd$Frequency == fr,c("Subject", "Measure", "Time", "Unit", "PowerCode", "Reference Period", "Value", "Flags")]
  dt$variable=paste(dt$Subject, dt$Measure, dt$Unit, dt$`Reference Period`)
  dt$variable<-str_remove_all(dt$variable, " NA")
  dt$variable<-str_replace_all(dt$variable, "previous", "prev")
  dt$variable<-str_replace_all(dt$variable, "Percentage", "%")
  dt$variable<-str_replace_all(dt$variable, "Pound Sterling", "GBP")
  dt$variable<-str_replace_all(dt$variable, "US dollar", "USD")
  dt$variable<-str_replace_all(dt$variable, " Level, ratio or index", "")
  dt$variable<-str_replace_all(dt$variable, " on the same period of the", "")
  dt$variable<-str_replace_all(dt$variable, "Gross domestic product", "GDP")
  dt$variable<-str_trim(dt$variable)
  dt<-dt[,c("Time", "Value", "variable")]
  dt0<-melt(dt, id.vars = c("Time", "variable"), measure.vars = "Value")
  dt<-dcast(dt0, Time ~ variable)
  if(tm == "month"){dt$Time<-(str_replace_all(dt$Time, "-", "-20"))}
  colnames(dt)[1]<-tm
  d<-merge(d, dt, by=tm, all = T)
  if(tm != "year"){d<-d[order(str_split(d[,tm],"_|-",simplify = T)[,2]),]}
  save(d, file=paste0(datadir,"/Payment_and_OECD_key_economic_indicators_",tm,".RData"))
  if(is.element("Total_GVA",colnames(d))){stop()}
  print(paste(unique(d[str_detect(d[,tm], "2018"),tm]),collapse=", "))
  rm(d, dt, dt0)
}
rm(oecd, tm, fr)
#
#
# 2.b) Add monetary and financial indicators
oecd<-read_csv("public_data/macro_data/oecd_monetary_financial_indicators.csv", show_col_types = F)
tm<-"quarter"
for(tm in c("year", "quarter", "month")){
  if(tm == "year"){fr<-"Annual"; d<-dty}else if(tm == "quarter"){fr<-"Quarterly"; d<-dtq}else{fr<-"Monthly"; d<-dtm}
  oecd$variable<-paste(oecd$Subject, oecd$Unit, oecd$`Reference Period`)
  dt<-oecd[oecd$Frequency == fr,c("Time", "Value", "variable")]
  dt$variable<-str_replace_all(dt$variable, "Index Index", "Index")
  dt$variable<-str_replace_all(dt$variable, "Index 20", "20")
  dt$variable<-str_replace_all(dt$variable, "National currency per US dollar", "GBP per USD")
  dt$variable<-str_replace_all(dt$variable, "Per cent", "%")
  dt$variable<-str_replace_all(dt$variable, "Percentage", "")
  dt$variable<-str_replace_all(dt$variable, " NA", "")
  dt$variable<-str_replace_all(dt$variable, "2010=100", "2015=100")
  dt$variable<-str_trim(dt$variable)
  dt0<-melt(dt, id.vars = c("Time", "variable"), measure.vars = "Value")
  dt<-dcast(dt0, Time ~ variable)
  colnames(dt)[1]<-tm
  d<-merge(d, dt, by=tm, all.x = T, all.y = T)
  if(tm != "year"){d<-d[order(str_split(d[,tm],"_|-",simplify = T)[,2]),]}
  save(d, file=paste0(datadir,"/Payment_and_OECD_monetary_financial_indicators_",tm,".RData"))
  
  print(paste(unique(d[str_detect(d[,tm], "2018"),tm]),collapse=", "))
  rm(d, dt, dt0)
}
rm(oecd, tm, fr)
#
#
# 2.c) Add monetary and financial indicators
oecd<-read_csv("public_data/macro_data/oecd_main_economic_indicators.csv", show_col_types = F)
oecd2<-read_csv("public_data/macro_data/oecd_main_economic_indicators_M1M3update.csv", show_col_types = F)
tm<-"quarter"
oecd$variable<-paste(oecd$Subject, oecd$Unit, oecd$`Reference Period`, oecd$Measure)
oecd2$variable<-paste(oecd2$Subject, oecd2$Unit, oecd2$`Reference Period`, oecd2$Measure)
# Feb 2024 update: replace monetary aggregates by most recent data from OECD
oecd<-oecd[!(oecd$variable %in% oecd2$variable),]
oecd<-rbind(oecd, oecd2)
rm(oecd2)
for(tm in c("year", "quarter", "month")){
  if(tm == "year"){fr<-"Annual"; d<-dty}else if(tm == "quarter"){fr<-"Quarterly"; d<-dtq}else{fr<-"Monthly"; d<-dtm}
  dt<-oecd[oecd$Frequency == fr,c("Time", "Value", "variable")]
  dt<-dt[c(which(str_detect(dt$variable, "onetary")), which(str_detect(dt$variable, "Deflator|GDP|Sales|Business"))),]
  dt<-unique(dt)
  dt$variable<-str_replace_all(dt$variable, "Index Index", "Index")
  dt$variable<-str_remove_all(dt$variable, "Level, rate or national currency|Level, rate or national currency, ")
  dt$variable<-str_replace_all(dt$variable, "Consumer Price Index", "CPI")
  dt$variable<-str_replace_all(dt$variable, "International", "Internat")
  dt$variable<-str_replace_all(dt$variable, "Investment", "Investm")
  dt$variable<-str_replace_all(dt$variable, "Index 20", "20")
  dt$variable<-str_replace_all(dt$variable, "National currency per US dollar", "GBP per USD")
  dt$variable<-str_replace_all(dt$variable, "Per cent", "%")
  dt$variable<-str_replace_all(dt$variable, "Percentage", "")
  dt$variable<-str_replace_all(dt$variable, " NA", "")
  dt$variable<-str_replace_all(dt$variable, "2010=100", "2015=100")
  dt$variable<-str_replace_all(dt$variable, "2015=100 2015=100", "2015=100")
  dt$variable<-str_replace_all(dt$variable, "Pound Sterling", "GBP")
  dt$variable<-str_replace_all(dt$variable, "previous", "prev")
  dt$variable<-str_replace_all(dt$variable, "Gross Domestic Product (GDP)", "GDP")
  dt$variable<-str_replace_all(dt$variable, "manufacturing", "manufct")
  dt$variable<-str_remove_all(dt$variable, "Monetary aggregates and their components > Broad money and components > | > Retail trade >|Monetary aggregates and their components > Narrow money and components > |National Accounts > |National Accounts Deflators > |Gross Domestic Product > |Capacity utilisation > |Leading Indicators OECD > Reference series > | > National indicator| > Total|Gross Domestic Product - | and net acquisition of valuables|Final|Original series   |M1 and components ")
  dt$variable<-str_replace_all(dt$variable, "> ", "")
  dt$variable<-str_replace_all(dt$variable, "  ", " ")
  dt$variable<-str_replace_all(dt$variable, "M3 M3", "M3")
  dt$variable<-str_replace_all(dt$variable, "  ", " ")
  dt$variable<-str_replace_all(dt$variable, " ,", ",")
  dt$variable<-str_trim(dt$variable)
  dt0<-melt(dt, id.vars = c("Time", "variable"), measure.vars = "Value")
  dt<-dcast(dt0, Time ~ variable)
  colnames(dt)[1]<-tm
  d<-merge(d, dt, by=tm, all.x = T, all.y = T)
  if(tm != "year"){d<-d[order(str_split(d[,tm],"_|-",simplify = T)[,2]),]}
  save(d, file=paste0(datadir,"/Payment_and_OECD_main_economic_indicators_",tm,".RData"))
  
  print(paste(unique(d[str_detect(d[,tm], "2018"),tm]),collapse=", "))
  rm(d, dt, dt0)
}
rm(oecd, tm, fr)




# 2.c) Add payment statistics data from pay.uk
# File has 4 sheets: excel_sheets("public_data/macro_data/Monthly-Payment-Statistics-1990-to-Dec-2023.xls") "BACS"            "CHAPS"           "Faster Payments" "Image Clearing" 
bacs<-read_excel("public_data/macro_data/Monthly-Payment-Statistics-1990-to-Dec-2023.xls", sheet="BACS")
cn<-str_trim(str_remove_all(paste(colnames(bacs), apply(bacs[1:6,],2,FUN=function(x){paste(x, collapse=" ")})), "NA"))
colnames(bacs)<-c("year", "month", "Number of Items (000s) Standing Orders", "Number of Items (000s) Direct Credits", "Number of Items (000s) Direct Debits", " ", "Value of Items (£m) All Credits(a, b)",  "Value of Items (£m) Direct Debits")
# Notes on (a) and (b) from raw tables: (a) In 2015, on Bacs’ behalf, VocaLink conducted work to improve the classification of payment purposes. This removed the anomalous categorisation of some bank input as Standing Orders. Historic data have been revised from January 2014. (b) Includes euro direct credits from January 1999."
bacs<-bacs[7:nrow(bacs),]
bacs<-bacs %>% fill(year) 
bacs$time<-paste(substr(bacs$month,1,3), bacs$year, sep="-")
bacs<-bacs[,-6]
colnames(bacs)[3:7]<-paste("Bacs", colnames(bacs)[3:7])
bacs<-bacs[!is.na(bacs$month),3:ncol(bacs)]
# Number of items is not read properly from the Excel sheet. Needs to be divided by 1000
bacs$`Bacs Number of Items (000s) Direct Credits`<-as.numeric(bacs$`Bacs Number of Items (000s) Direct Credits`)/1000
bacs$`Bacs Number of Items (000s) Direct Debits`<-as.numeric(bacs$`Bacs Number of Items (000s) Direct Debits`)/1000
bacs$`Bacs Number of Items (000s) Standing Orders`<-as.numeric(bacs$`Bacs Number of Items (000s) Standing Orders`)/1000

chaps<-read_excel("public_data/macro_data/Monthly-Payment-Statistics-1990-to-Dec-2023.xls", sheet="CHAPS")
colnames(chaps)<-c("year", "month", paste("CHAPS", c("Number of Items (000s) Sterling", "RM1", "Number of Items (000s) Euro Domestic", "Number of Items (000s) Euro TARGET", "RM2", "Value of Items (£m) Sterling", "RM3", "Value of Items (£m) Domestic",  "Value of Items (£m) TARGET")))               
chaps<-chaps[7:nrow(chaps),which(!str_detect(colnames(chaps), "RM"))]
chaps<-chaps %>% fill(year) 
chaps$time<-paste(substr(chaps$month,1,3), chaps$year, sep="-")
cn<-colnames(chaps)
chaps<-data.frame(apply(chaps,2,FUN=function(x){return(ifelse(x=="-",NA,x))}))
colnames(chaps)<-cn
chaps<-chaps[!is.na(chaps$month),3:ncol(chaps)]

fps<-read_excel("public_data/macro_data/Monthly-Payment-Statistics-1990-to-Dec-2023.xls", sheet="Faster Payments")
colnames(fps)<-c("year", "month", paste("FPS", c("RM1", 
                   "Number of Items (000s) Standing Order Payments", "Number of Items (000s) Single Immediate Payments", "Number of Items (000s) Forward Dated Payments", "Number of Items (000s) Return Payments", "RM2", 
                   "Value of Items (£m) Standing Order Payments", "Value of Items (£m) Single Immediate Payments", "Value of Items (£m) Forward Dated Payments", "Value of Items (£m) Return Payments")))             
fps<-fps[7:nrow(fps),which(!str_detect(colnames(fps), "RM"))]
fps<-fps %>% fill(year) 
fps$time<-paste(substr(fps$month,1,3), fps$year, sep="-")
fps<-fps[!is.na(fps$month),3:ncol(fps)]

ic<-read_excel("public_data/macro_data/Monthly-Payment-Statistics-1990-to-Dec-2023.xls", sheet="Image Clearing")
colnames(ic)<-c("year", "month", paste("Image Clearing", c("RM1", 
                "Number of Items (000s) Cheques (RTPs)", "Number of Items (000s) Credits (ITPs)", 
                "Value of Items (£m) Cheques (RTPs)", "Value of Items (£m) Credits (ITPs)")))
ic<-ic[7:nrow(ic),which(!str_detect(colnames(ic), "RM"))]
ic<-ic %>% fill(year) 
ic$time<-paste(substr(ic$month,1,3), ic$year, sep="-")
ic<-ic[!is.na(ic$month),3:ncol(ic)]

payuk<-merge(bacs, chaps, by="time", all.x = T, all.y = T)
payuk<-merge(payuk, fps, by="time", all.x = T, all.y = T)
payuk<-merge(payuk, ic, by="time", all.x = T, all.y = T)
#payuk<-payuk[payuk$time %in% dtm$month,]
#payuk<-payuk[match(dtm$month, payuk$time),]
rm(chaps, bacs, ic, fps, cn)
payuk[,2:ncol(payuk)]<-as.data.frame(sapply(payuk[2:ncol(payuk)],as.numeric))

# Add totals for convenience reasons
payuk$`Bacs Number of Items (000s) Total`<- apply(payuk[,str_detect(colnames(payuk),"Bacs Number")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`Bacs Value of Items (£m) Total`<-apply(payuk[,str_detect(colnames(payuk),"Bacs Value")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`CHAPS Number of Items (000s) Total`<-apply(payuk[,str_detect(colnames(payuk),"CHAPS Number")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`CHAPS Value of Items (£m) Total`<-apply(payuk[,str_detect(colnames(payuk),"CHAPS Value")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`FPS Number of Items (000s) Total`<-apply(payuk[,str_detect(colnames(payuk),"FPS Number")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`FPS Value of Items (£m) Total`<-apply(payuk[,str_detect(colnames(payuk),"FPS Value")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`Image Clearing Number of Items (000s) Total`<-apply(payuk[,str_detect(colnames(payuk),"Image Clearing Number")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`Image Clearing Value of Items (£m) Total`<-apply(payuk[,str_detect(colnames(payuk),"Image Clearing Value")],1, FUN=function(x){return(sum(x, na.rm = T))})
payuk$`Total Retail Payments (excl. Cards) Number of Items (000s)`<-payuk$`Bacs Number of Items (000s) Total` + payuk$`FPS Number of Items (000s) Total` + payuk$`FPS Number of Items (000s) Total`
payuk$`Total Retail Payments (excl. Cards) Value of Items (£m)`<-payuk$`Bacs Value of Items (£m) Total` + payuk$`FPS Value of Items (£m) Total` + payuk$`FPS Value of Items (£m) Total`
  

# Get data ready for saving
colnames(payuk)[1]<-"month"
payuk<-merge(dtm, payuk, by="month", all.x = T, all.y = T)
#payuk<-payuk[match(dtm$month, payuk$month),]
save(payuk, file=paste0(datadir,"/Payment_and_payuk_payment_statistics_month.RData"))
# Create annual and quarterly payuk data
payuk$quarter<-ifelse(str_detect(payuk$month, "Jan|Feb|Mar"), paste0("Q1",substr(payuk$month,4,8)), payuk$month)
payuk$quarter<-ifelse(str_detect(payuk$quarter, "Apr|May|Jun"), paste0("Q2",substr(payuk$quarter,4,8)), payuk$quarter)
payuk$quarter<-ifelse(str_detect(payuk$quarter, "Jul|Aug|Sep"), paste0("Q3",substr(payuk$quarter,4,8)), payuk$quarter)
payuk$quarter<-ifelse(str_detect(payuk$quarter, "Oct|Nov|Dec"), paste0("Q4",substr(payuk$quarter,4,8)), payuk$quarter)
payuk<-payuk[,c("quarter", colnames(payuk)[!(colnames(payuk) %in% c("month", "quarter"))])]
payuk<-aggregate(payuk[,2:ncol(payuk)], by = list(quarter=payuk$quarter), sum)
#payuk<-payuk[match(dtq$quarter, payuk$quarter),]
save(payuk, file=paste0(datadir,"/Payment_and_payuk_payment_statistics_quarter.RData"))
payuk$year<-substr(payuk$quarter, 4,7)
payuk<-payuk[,c("year", colnames(payuk)[!(colnames(payuk) %in% c("quarter", "year"))])]
payuk<-aggregate(payuk[,2:ncol(payuk)], by = list(year=payuk$year), sum)
save(payuk, file=paste0(datadir,"/Payment_and_payuk_payment_statistics_year.RData"))
rm(payuk)


# 2.d) Add ONS national aggregate data
# 
# 2.d.1) Gross value added (GVA) index 2019 = 100, chained volume measures, UK, seasonally adjusted 
# Source: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/indicativemonthlygdpconsistentwithquarterlynationalaccounts [downloaded on 04/01/2024]
# Note: This data was published on 13 December 2023, estimates of Monthly GDP were published for October 2023. The Index of Services, Index of Production and Construction output in Great Britain publications covering October 2023 are also available. This quarterly national accounts release revises data back to 2022 Q1. Although this release focuses on providing the best quarterly estimate of GDP, an indicative monthly path up to September 2023 is provided in the dataset. The next monthly GDP release (on 12 January 2024) will be consistent with these revisions, and October 2023 will be open for revisions alongside November 2023 data published for the first time. 
# 
# Load different data release versions
rvs<-paste0(rep(c("mar", "jun", "sep", "dec"),4),rep(2021:2024,each=4))
rv<-rvs[4]
if(exists("gdp_sa")){rm(gdp_sa)}
for(rv in rvs){
  print(rv)
  fn<-paste0("public_data/macro_data/indicativemonthlydataset_",rv,".xlsx")
  if(!file.exists(fn)){next}
  
  # 
  excel_sheets(fn)
  if(str_detect(rv,"2021")){
    gdp_tmp<-read_excel(fn, sheet = length(excel_sheets(fn)))
    cls<-c("...1","...2","A","B - F","B - E","C","F","G - U","G - I","H","J","L","M - N","O - Q","P","R - U")
    gdp_tmp<-gdp_tmp[,cls]
    colnames(gdp_tmp)<-c("Time period and dataset code row","Total GVA", "A Agriculture", 
           "B - F Production and construction", "B - E Production", "C Manufacturing", "F Construction", 
           "G - U  Services", "G - I Distribution, transport, hotels and restaurants", "H Transportation and storage", 
           "J Information and communication", "L Real estate activties", "M - N Professional and support", 
           "O - Q Government, health and education", "P Education", "R -U Other services")
  }else{
    gdp_tmp<-read_excel(fn, sheet = length(excel_sheets(fn)), skip = 4) 
    colnames(gdp_tmp)<-str_split(colnames(gdp_tmp),"\\\r",simplify = T)[,1]
    cls<-c("Time period and dataset code row","Total GVA","A","B - F","B - E","C","F","G - U ","G - I","H","J","L","M - N","O - Q","P","R -U")
    gdp_tmp<-gdp_tmp[,cls]
    colnames(gdp_tmp)<-c("Time period and dataset code row","Total GVA", "A Agriculture", 
                         "B - F Production and construction", "B - E Production", "C Manufacturing", "F Construction", 
                         "G - U  Services", "G - I Distribution, transport, hotels and restaurants", "H Transportation and storage", 
                         "J Information and communication", "L Real estate activties", "M - N Professional and support", 
                         "O - Q Government, health and education", "P Education", "R -U Other services")
  }
  # Columns to keep: Total GVA and aggregates on agriculture, production, production and construction, manufacturing, construction, services, G-I Services, transport services
  cls<-c("Time period and dataset code row","Total GVA", "A Agriculture", 
         "B - F Production and construction", "B - E Production", "C Manufacturing", "F Construction", 
         "G - U  Services", "G - I Distribution, transport, hotels and restaurants", "H Transportation and storage", 
         "J Information and communication", "L Real estate activties", "M - N Professional and support", 
         "O - Q Government, health and education", "P Education", "R -U Other services")
  gdp_tmp<-gdp_tmp[,cls]
  
  colnames(gdp_tmp)[3:ncol(gdp_tmp)]<-paste0("Total GVA sa for ",colnames(gdp_tmp)[3:ncol(gdp_tmp)]," ",rv)
  colnames(gdp_tmp)[1:2]<-c("month", paste("Total GVA sa",rv))
  
  if(!exists("gdp_sa")){gdp_sa<-gdp_tmp}else{gdp_sa<-merge(gdp_sa,gdp_tmp,by=c("month"),all=T)}
  
} # rv in rvs (iterate through release versions)
rm(gdp_tmp, rv, rvs, cls)
gdp_sa<-gdp_sa[!is.na(gdp_sa$month) & !(gdp_sa$month%in%c("CONSTANT","CVM £m","PRICE")),]


# 2.d.2) Monthly GDP and main sectors to four decimal places. Monthly index values for monthly gross domestic product (GDP) and the main sectors in the UK to four decimal places.
# Source: https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/monthlygdpandmainsectorstofourdecimalplaces/1997tocurrent/monthlygdpto4dp.xlsx  [downloaded on 04/01/2024]
# Note: This spreadsheet contains the chained volume indices of gross value added estimates of Monthly GDP and its main sectors: Production, Services, Construction and Agriculture. These figures are seasonally adjusted, with a reference year of 2019 (2019 = 100).
rvs<-paste0(rep(c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),4),rep(21:24,each=12))
rv<-rvs[20]
if(exists("gdp_4sec")){rm(gdp_4sec)}
for(rv in rvs){
  print(rv)
  fn<-paste0("public_data/macro_data/monthlygdpto4dp_",rv,".xlsx")
  if(!file.exists(fn)){next}  # 
  print(paste(excel_sheets(fn)))
  gdp_tmp <- read_excel("public_data/macro_data/mgdp4dp.xlsx", sheet="Data_table", skip=3)
  colnames(gdp_tmp)[3:ncol(gdp_tmp)]<-paste0("GDP sa ", colnames(gdp_tmp)[3:ncol(gdp_tmp)], " ", rv)
  colnames(gdp_tmp)[1:2]<-c("month", paste("GDP sa (A-T)",rv))
  
  if(!exists("gdp_4sec")){gdp_4sec<-gdp_tmp}else{gdp_4sec<-merge(gdp_4sec,gdp_tmp,by=c("month"),all=T)}
}
rm(gdp_tmp, rv, rvs)


# 
# 2.d.3) Indicative monthly non-seasonally adjusted GDP Release date: 20 January 2022 Reference number: 14197
# Source: https://www.ons.gov.uk/economy/grossdomesticproductgdp/adhocs/14197indicativemonthlynonseasonallyadjustedgdp [Downloaded: 04/01/2024]
# This spreadsheet contains non-seasonally adjusted gross value added (GVA) indices with a reference year of 2019 (2019 = 100). Industry sections are defined using SIC2007 codes. The data is associated with the Office for National Statistics' Monthly GDP: November 2021, published on the 14th January 2022. 
# These figures are non-seasonally adjusted and have not been quality assured in-depth. Where quality adjustments are applied as part of the GDP compilation process, these are only applied to the seasonally adjusted series. Therefore, we would not consider the non-seasonally adjusted data to be of a quality to publish but may be suitable for internal analysis purposes in the context of the implied seasonal factors.
gdp_nsa<-read_excel("public_data/other_industry_level/indicativemonthlygdpnsapubl.xlsx",sheet = "Monthly Index",skip=4)
colnames(gdp_nsa)<-str_replace_all(colnames(gdp_nsa), "\r\n"," ")
# Columns to keep: Total GVA and aggregates on agriculture, production, production and construction, manufacturing, construction, services, G-I Services, transport services
cls<-c("Time period and dataset code row","Total GVA", "A Agriculture", 
       "B - F Production and construction", "B - E Production", "C Manufacturing", "F Construction", 
       "G - U  Services", "G - I Distribution, transport, hotels and restaurants", "H Transportation and storage", 
       "J Information and communication", "L Real estate activties", "M - N Professional and support", 
       "O - Q Government, health and education", "P Education", "R -U Other services")
gdp_nsa<-gdp_nsa[,cls]
colnames(gdp_nsa)[3:ncol(gdp_nsa)]<-paste0("Total GVA nsa for ",colnames(gdp_nsa)[3:ncol(gdp_nsa)])
colnames(gdp_nsa)[1:2]<-c("month", "Total GVA nsa")
rm(cls)

onsm<-merge(gdp_nsa, gdp_sa, by="month", all = T)
onsm<-merge(onsm, gdp_4sec, by="month", all = T)
rm(gdp_4sec, gdp_nsa, gdp_sa)

onsm$month<-paste0(str_to_title(substr(onsm$month,5,7)),"-",substr(onsm$month,1,4))
ons<-merge(dtm, onsm, by="month",all = T)
#ons$month<-as.yearmon(ons$month, format = "%B-%Y")
ons<-ons[order(as.yearmon(ons$month, format = "%B-%Y")),]
ons$month<-as.character(ons$month)
ons[,2:ncol(ons)]<-apply(ons[,2:ncol(ons)], 2, FUN=function(x){return(as.numeric(as.character(x)))})
save(ons, file=paste0(datadir,"/Payment_and_ons_macro_aggregates_month.RData"))



 # 2.d.4) Dataset GDP – data tables. Annual and quarterly data for UK gross domestic product (GDP) estimates, in chained volume measures and current market prices. 
# Source: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/uksecondestimateofgdpdatatables
# Notes: Quarterly National Accounts, Q3 (July to Sept) 2023; These figures are seasonally adjusted. Publication dates; The data tables in this spreadsheet were originally published at 22nd December 2023. The next publication will be published at 15th February 2024.
excel_sheets("public_data/macro_data/ONS_quarterlynationalaccountsdatatables.xlsx")
gdp_idx<-read_excel("public_data/macro_data/ONS_quarterlynationalaccountsdatatables.xlsx", sheet = "A1 AGGREGATES", skip = 6)
colnames(gdp_idx)<-str_remove_all(colnames(gdp_idx), " \\[note 3\\]")
gdp_idx<-gdp_idx[,!str_detect(colnames(gdp_idx), "deflator")]
gdp_gbp<-read_excel("public_data/macro_data/ONS_quarterlynationalaccountsdatatables.xlsx", sheet = "A2 AGGREGATES", skip = 7)
colnames(gdp_gbp)<-str_remove_all(colnames(gdp_gbp), " \\[note 2\\]")
colnames(gdp_gbp)<-str_remove_all(colnames(gdp_gbp), " \\[note 3\\]")
colnames(gdp_gbp)<-str_remove_all(colnames(gdp_gbp), " \\[note 4\\]")
gdp_gbp<-gdp_gbp[,c("Time period and dataset code row", "Gross national income at market prices: Current prices", 
                    "Gross domestic product at market prices: Current prices", 
                    "Gross value added at basic prices: Current prices",
                    "Gross domestic product at market prices: Chained volume measure",         
                    "Gross value added at basic prices: Chained volume measure",  
                    "Gross value added excluding oil & gas: Chained volume measure")]
gdp_gbp<-gdp_gbp[!is.na(gdp_gbp$`Time period and dataset code row`),]
gdp_idx<-gdp_idx[!is.na(gdp_idx$`Time period and dataset code row`),]
gdp_gbp<-gdp_gbp[1:(which(gdp_gbp$`Time period and dataset code row`=="Percentage change, latest year on previous year")-1),]
gdp_idx<-gdp_idx[1:(which(gdp_idx$`Time period and dataset code row`=="Percentage change, latest year on previous year")-1),]
gdp_gbp<-gdp_gbp[!(gdp_gbp$`Time period and dataset code row` %in% c(NA, "Dataset identifier", "Quarterly", "Time period and dataset code row")),]
gdp_idx<-gdp_idx[!(gdp_idx$`Time period and dataset code row` %in% c(NA, "Dataset identifier", "Quarterly", "Time period and dataset code row")),]
gdp<-merge(gdp_gbp, gdp_idx, by="Time period and dataset code row", all = T)
colnames(gdp)[1]<-"time"
colnames(gdp)<-str_replace_all(colnames(gdp), "Gross domestic product", "GDP")
colnames(gdp)<-str_replace_all(colnames(gdp), "Gross value added", "GVA")
colnames(gdp)<-str_replace_all(colnames(gdp), "gross value added", "GVA")
gdpq<-gdp[str_detect(gdp$time,"Q"),]
gdpy<-gdp[!str_detect(gdp$time,"Q"),]
rm(gdp, gdp_gbp, gdp_idx)

# 2.d.5) Dataset(s): GDP first quarterly estimate time series - A detailed breakdown of the components of GDP.
# Source: https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/secondestimateofgdp/current/pn2.xlsx
# Note: The following table would provide a breakdown into GDP components, quarterly data, but not sufficiently disaggregate by industry (only approx. 12 diff. sectors)
#gdp_bd<-data.frame(read_excel("public_data/macro_data/ONS_GDP_firstquarterlyestimatetimeseries_detailed_breakdown_pn2.xlsx"))
#gdp_bd<-gdp_bd[,!str_detect(colnames(gdp_bd), "Deflator|deflator|Gross Fixed Capital|GFCF|%|growth|Household|NPISH|trade|Trade|Q on Q|Taxes|Exports|imports|consumption|investment")]


# Now, create quarterly and yearly data
onsq<-onsm
onsq$month[str_detect(onsq$month, "Jan|Feb|Mar")]<-paste0("Q1",substr(onsq$month[str_detect(onsq$month, "Jan|Feb|Mar")],4,8))
onsq$month[str_detect(onsq$month, "Apr|May|Jun")]<-paste0("Q2",substr(onsq$month[str_detect(onsq$month, "Apr|May|Jun")],4,8))
onsq$month[str_detect(onsq$month, "Jul|Aug|Sep")]<-paste0("Q3",substr(onsq$month[str_detect(onsq$month, "Jul|Aug|Sep")],4,8))
onsq$month[str_detect(onsq$month, "Oct|Nov|Dec")]<-paste0("Q4",substr(onsq$month[str_detect(onsq$month, "Oct|Nov|Dec")],4,8))
colnames(onsq)[1]<-"quarter"
onsq<-aggregate(.~ quarter, data=onsq,FUN=function(x){return(sum(x,na.rm = F))}, na.action = NULL, drop=F)
#onsq$quarter<-as.yearqtr(onsq$quarter, format = "Q%q-%Y")
onsq<-onsq[order(as.yearqtr(onsq$quarter, format = "Q%q-%Y")),]
onsy<-onsq
onsy$quarter<-as.numeric(substr(onsy$quarter,4,7))
colnames(onsy)[1]<-"year"
onsy<-aggregate(.~ year, data=onsy,FUN=function(x){return(sum(x,na.rm = F))}, na.action = NULL, drop=F)

# Merge quarterly and yearly with other GDP data
colnames(gdpq)[1]<-"quarter"
colnames(gdpy)[1]<-"year"
gdpq$quarter<-paste0(substr(gdpq$quarter,6,7),"-",substr(gdpq$quarter,1,4))
#gdpq$quarter<-as.yearqtr(gdpq$quarter, format = "Q%q-%Y")
onsq<-merge(onsq, gdpq, by="quarter", all=T)
#dtq$quarter<-as.yearqtr(dtq$quarter, format = "Q%q-%Y")
#dtq$quarter<-paste(substr(dtq$quarter,4,7),substr(dtq$quarter,1,2))
onsq<-merge(dtq, onsq, by="quarter", all=T)
ons<-onsq
ons$quarter<-as.character(ons$quarter)
save(ons, file=paste0(datadir,"/Payment_and_ons_macro_aggregates_quarter.RData"))
onsy<-merge(onsy, gdpy, by="year", all=T)
onsy<-merge(dty, onsy, by="year", all=T)
ons<-onsy
save(ons, file=paste0(datadir,"/Payment_and_ons_macro_aggregates_year.RData"))

#
#
#
#
if(1==2){ # Do not run code below, it's outdated. 
excel_sheets("public_data/macro_data/ONS_firstquarterlyestimateofgdpdatatables.xlsx")
ons<-read_excel("public_data/macro_data/ONS_firstquarterlyestimateofgdpdatatables.xlsx", sheet="A2 AGGREGATES")
colnames(ons)<-as.character(ons[7,])
colnames(ons)[1]<-"time"
ons<-ons[which(str_detect(ons$time, "201") & !is.na(ons$time) & !str_detect(ons$time, "Reference")),]
# Get annual data and separete levels and growth rates
onsq<-ons[str_detect(ons$time, "Q"),]
onsy<-ons[!str_detect(ons$time, " Q"),]
onsygr<-onsy[which(duplicated(onsy$time)),]
colnames(onsygr)[2:ncol(onsygr)]<-paste0(colnames(onsygr)[2:ncol(onsygr)], " % change prev year")
ons<-merge(onsy[!duplicated(onsy$time),], onsygr, by="time", all.x = T, all.y = T)
colnames(ons)[1]<-"year"
colnames(ons)<-str_replace_all(colnames(ons), "\\.", " ")
colnames(ons)<-str_replace_all(colnames(ons), "\\[note 4\\]|\\[note 3\\]", "")
colnames(ons)<-str_replace_all(colnames(ons), "Gross domestic product", "GDP")
colnames(ons)<-str_replace_all(colnames(ons), "excluding", "excl")
colnames(ons)<-str_replace_all(colnames(ons), "Chained volume measure", "Volume measure")
colnames(ons)<-str_replace_all(colnames(ons), "adjustment", "adjustm.")
colnames(ons)<-str_replace_all(colnames(ons), "Basic ", "")
colnames(ons)<-str_trim(colnames(ons))
ons<-merge(dty, ons, by="year", all.y = T, all.x = T)
# Get monthly non-s.a. ONS GDP data
ons2<-read_excel("public_data/other_industry_level/indicativemonthlygdpnsapubl.xlsx",sheet = "Monthly Index",skip=4)
ons2<-ons2[c("Time period and dataset code row","Total GVA")]
onsy<-ons2
colnames(onsy)<-c("year","Total GVA")
onsy$year<-gsub("[^0-9.-]", "", onsy$year)
onsy<-aggregate(.~ year, data=onsy,sum)
onsy$year<-substr(onsy$year,1,4)
ons<-merge(ons, onsy, by="year", all.y = T, all.x = T)
save(ons, file=paste0(datadir,"/Payment_and_ons_macro_aggregates_year.RData"))
rm(ons, onsy, onsygr)
# 
# Get quarterly data and separate levels and growth rate data. Note: Growth data is avail for prev. period and prev. year
onsl<-onsq[which(!duplicated(onsq$time)),]
onsgr<-onsq[which(duplicated(onsq$time)),]
onsgr2<-onsgr[which(duplicated(onsgr$time)),]
onsgr<-onsgr[which(!duplicated(onsgr$time)),]
colnames(onsgr)[2:ncol(onsgr)]<-paste(colnames(onsgr)[2:ncol(onsgr)], "% change prev quarter")
colnames(onsgr2)[2:ncol(onsgr2)]<-paste(colnames(onsgr2)[2:ncol(onsgr2)], "% change prev quarter")
ons<-merge(onsl, onsgr, by="time", all.x = T, all.y = T)
ons<-merge(ons, onsgr2, by="time", all.x = T, all.y = T)
colnames(ons)[1]<-"quarter"
ons$quarter<-paste0(substr(ons$quarter,6,7),"-",substr(ons$quarter,1,4))
ons<-merge(dtq, ons, by="quarter", all.x = T, all.y = T)
onsq<-ons2
colnames(onsq)<-c("quarter","Total GVA")
onsq$quarter[str_detect(onsq$quarter,"JAN|FEB|MAR")]<-paste0("Q1-",substr(onsq$quarter[str_detect(onsq$quarter,"JAN|FEB|MAR")],1,4))
onsq$quarter[str_detect(onsq$quarter,"APR|MAY|JUN")]<-paste0("Q2-",substr(onsq$quarter[str_detect(onsq$quarter,"APR|MAY|JUN")],1,4))
onsq$quarter[str_detect(onsq$quarter,"JUL|AUG|SEP")]<-paste0("Q3-",substr(onsq$quarter[str_detect(onsq$quarter,"JUL|AUG|SEP")],1,4))
onsq$quarter[str_detect(onsq$quarter,"OCT|NOV|DEC")]<-paste0("Q1-",substr(onsq$quarter[str_detect(onsq$quarter,"OCT|NOV|DEC")],1,4))
onsq<-aggregate(.~ quarter, data=onsq,sum)
ons<-merge(ons, onsq, by="quarter", all.y = T, all.x = T)
save(ons, file=paste0(datadir,"/Payment_and_ons_macro_aggregates_quarter.RData"))
rm(ons, onsl, onsgr, onsgr2, onsq)

# Create a monthly non-s.a. ONS GDP file
colnames(ons2)<-c("month","Total GVA")
ons2$month<-paste0(str_to_title(substr(ons2$month,5,7)),"-",substr(ons2$month,1,4))
ons<-merge(dtm, ons2, by="month",all = T)
ons<-ons[order(as.yearmon(ons$month,format = "%B-%Y")),]
save(ons, file=paste0(datadir,"/Payment_and_ons_macro_aggregates_month.RData"))
rm(ons, ons2)
} # if 1==2




#### TO BE CONTINUED

# 2.d) Add BoE RTGS settlement statistics manually (could not find it machine readable)
# https://www.bankofengland.co.uk/payment-and-settlement/payment-and-settlement-statistics
boe_rtgs<-data.frame(cbind(time=c("21_Q4", "22_Q1", "22_Q2", "22_Q3", "22_YTD", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"), 
                           chaps_values=c(360810, 379622, 376028, 387719, 381240, 254489, 284591, 277229, 268615, 270400, 298710, 333661, 330095, 329671, 361844, 341171), 
                           chaps_volumes=c(195613, 197026, 208103, 202830, 202567, 135555, 134665, 138245, 144353, 148412, 154006, 165285, 191788, 192292, 175346, 189539), 
                           crest_dvp_values=c(371522, 371149, 374255, 373992, 373119, 322118, 293293, 303717, 274257, 240480, 220970, 270129, 314193, 342119, 382440, 371679), 
                           crest_dvp_volumes=c(13461, 14351, 14091, 13519, 13983, 6859, 7325, 8388, 9050, 9391, 10883, 12063, 12099, 11760, 12750, 13154), 
                           fps_net_values=c(1474, 1468, 1684, 1674, 1608, 188, 502, 586, 606, 663, 677, 775, 900, 1026, 1082, 1388), 
                           bacs_net_values=c(4291, 4658, 4879, 4534, 4686, 3269, 3190, 3071, 3122, 3159, 3193, 3321, 3686, 3915, 4508, 4611), 
                           link_net_values=c(224, 204, 240, 228, 224, 216, 235, 249, 271, 294, 315, 324, 318, 299, 213, 210), 
                           c_ccc=c(rep(NA, 5), 220, 232, 211, 196, 190, 156, 137, 133, 161, NA, NA),
                           Cheque_imaging_net_values=c(57, 79, 67, 56, 67, rep(NA, 8), 193, 77, 65), 
                           Visa_Europe_net_values=c(2582, 2425, 2550, 2219, 2395, NA, NA, 1144, 1149, 1425, 1531, 1776, 2270, 2363, 2368, 2481), 
                           Mastercard_values=c(569, 610, 740, 700, 683, rep(NA, 10), 519),
                           Total_settlement_values=c(741530, 760216, 760442, 771122, 764021, rep(NA, 11))))
  
  
 
