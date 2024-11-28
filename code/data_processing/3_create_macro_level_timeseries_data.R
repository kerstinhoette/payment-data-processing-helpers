### This script generates macro economic aggregates from the Payment data ###
# 
# The macro level aggregates are used for benchmarking with other statistics (e.g. GDP (national/regional), financial statistics, volumes and values reported by pay.uk)
# 
# Steps: 
# 1.) Load edgelist data counts and amounts (for regional/national aggregation, and by month/quarter/year)
# 2.) Aggregate and save
# 
# 
# Notes: Here, the data is taken from the 2-digit level data where no cleaning steps were undertaken. This may need to be considered for future analyses. 
# 
rm(list=ls())
.rs.restartR()

# Set version of dataset to be used
dtversion<-"Dec2023_public"
dtversion<-"Feb2024"

ddir<-paste0("payment_data/",dtversion,"/")

dgt<-5
datadir<-paste0(ddir,"macro_aggregates/",paste0(dgt,"_digit_data/")[dtversion!="Dec2023_public"])
if(!dir.exists(datadir)){dir.create(datadir, recursive=T)}
reg<-"national"; tm<-"quarter"
tms<-c("year", "quarter", "month")
for(reg in c("national", "regional")[1:(1+(dgt==2))]){
  for(tm in tms){
    if(dtversion!="Dec2023_public"){
      load(paste0(ddir,"edgelist_transactions/",dgt,"_digit_data/aggregate_amt_by_",tm,"_and_",dgt,"_sic_",reg,"_level.RData"))
      amt<-data.frame(apply(df[,!(colnames(df)%in%c("from","to"))], 2, FUN=function(x){sum(x,na.rm = T)}))
      load(paste0(ddir,"edgelist_transactions/",dgt,"_digit_data/aggregate_cnt_by_",tm,"_and_",dgt,"_sic_",reg,"_level.RData"))
      cnt<-data.frame(cnt=apply(df[,!(colnames(df)%in%c("from","to"))], 2, FUN=function(x){sum(x,na.rm = T)}))
      rm(df)
      dt<-data.frame(cbind(rownames(amt),amt,cnt))
      colnames(dt)<-c(tm,"amt","cnt")
      rm(amt,cnt)
    }else{
      load(ddir,"national_account_data/IOT_flow_of_goods_Payment_monthly_CPA_public.RData")
      d0<-colSums(d0[,!(colnames(d0)%in%c("from","to"))],na.rm = T)
      dt<-melt(d0)
      dt<-cbind(rownames(dt),dt)
      dt<-cbind(dt[str_detect(rownames(dt),"_amt_"),],dt[str_detect(rownames(dt),"_cnt_"),2])
      colnames(dt)<-c(tm,"amt","cnt")
      dt[,tm]<-str_remove_all(dt[,tm],"_amt") 
      dt[,tm]<-str_replace_all(dt[,tm],"_","-")
      if(tm!="month"){
        if(tm=="quarter"){
          dt$quarter[str_detect(dt$quarter,"jan|feb|mar")]<-paste0("Q1-",substr(dt$quarter[str_detect(dt$quarter,"jan|feb|mar")],5,8))
          dt$quarter[str_detect(dt$quarter,"apr|may|jun")]<-paste0("Q2-",substr(dt$quarter[str_detect(dt$quarter,"apr|may|jun")],5,8))
          dt$quarter[str_detect(dt$quarter,"jul|aug|sep")]<-paste0("Q3-",substr(dt$quarter[str_detect(dt$quarter,"jul|aug|sep")],5,8))
          dt$quarter[str_detect(dt$quarter,"oct|nov|dec")]<-paste0("Q4-",substr(dt$quarter[str_detect(dt$quarter,"oct|nov|dec")],5,8))
        }else{dt$year<-substr(dt$year,5,8)}
        dt<-aggregate(dt[,2:3], by=list(dt[,tm]), FUN=function(x){return(sum(x,na.rm = T))})
        colnames(dt)[1]<-tm
      }else{
        dt$month<-paste0(str_to_title(substr(dt$month,1,3)),substr(dt$month,4,8))
      } # if tm == month
    } # if dtversion == Dec2023_public

    save(dt, file=paste0(datadir,"aggregate_amt_cnt_",reg,"_",tm,".RData"))
  }
}

rm(tm,tms,reg,d0)


