# Produce a latex version of the concordance table

rm(list=ls())
.rs.restartR()

library(xtable); library(stringr)

load("public_data/concordances/concordances.RData")

fn<-paste("statistical_output/concordance_table_SIC_CPA.tex")

# Determine which columns to show
cls<-c("SIC","SIC_names","CPA","CPA_names")
cnc<-unique(conc[,cls])
cnc<-cnc[order(cnc$SIC),]
cnc<-cnc[!is.na(cnc$SIC),]

#cnc$`Input-Output Classification (SIC2007)`[cnc$`Input-Output Classification (SIC2007)`=="23OTHER"]<-"23 OTHER"
#cnc$`Input-Output Classification (SIC2007)`[cnc$`Input-Output Classification (SIC2007)`=="68.2IMP"]<-"68.2 IMP"
cnc$CPA<-str_replace_all(cnc$CPA, "OTHER", " OTH")
cnc$CPA<-str_replace_all(cnc$CPA, "L68BXL683", "L68 BX L683")
cnc$CPA<-str_replace_all(cnc$CPA, "_", " -")
#cnc$A16<-str_replace_all(cnc$A16, ",", ", ")

cnc<-apply(cnc, 2, FUN=function(x){return(ifelse(duplicated(x),".",x))})


ali<-ifelse((str_length(cnc[1,])<10),"p{0.25cm}|","p{2.5cm}|")
ali[1]<-paste0("|",ali[1])

captext<-paste("Concordance table from SIC to CPA codes.")

colnames(cnc)<-str_replace_all(colnames(cnc),"_"," ")
#colnames(cnc)<-str_replace_all(colnames(cnc),"Input-Output Classification","IO")


print(xtable(cnc, align=c("|p{0.5cm}|",ali), caption=paste(captext)), include.rownames = F, tabular.environment="longtable", size="tiny", floating = F, sanitize.colnames.function = function(x){x})

print(xtable(cnc, align=c("|p{0.5cm}|",ali), caption=paste(captext)), include.rownames = F, tabular.environment="longtable", size="tiny", floating = F, sanitize.colnames.function = function(x){x}, file=fn)




