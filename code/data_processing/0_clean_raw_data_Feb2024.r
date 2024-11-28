### This script processes the raw data into a convenient format ###
# 
# Steps: 
# 1.) Load files ONS Payment data raw csv files
# 2.) Replace non-valid SIC codes by "0" for "Unknown"
# 3.) Make some harmonization and preplace placeholders for statistical disclosure controlled data points "*" by NA
# 4.) Save merged raw data 
# 
# 
# 
# Data inputs: 
# - Payment raw data (either 2-digit or 5-digit data, statistical disclosure controlled)
# - List of SIC codes; 2 versions publicly available (note: the two versions do not perfectly overlap)
#     Sources: 
#       a) https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/527619/SIC07_CH_condensed_list_en.csv/preview [downloaded on 16/08/2023]
#       b) https://www.ons.gov.uk/methodology/classificationsandstandards/ukstandardindustrialclassificationofeconomicactivities/uksic2007 [downloaded on 16/08/2023]
#     
# 
# Clean memory
rm(list=ls())
.rs.restartR()
library(readr)
#library(data.table); library(stringr); library(vctrs); library(reshape2); library(igraph); library(readxl)
# Specify digit-level of source data
dgt<-"5_digit"
#dgt<-"3_digit"
#dgt<-"2_digit"
dtversion<-"Feb2024"
# Path to data directory
datadir<-paste0("payment_data/",dtversion,"/raw_data/",dgt,"/")
#setwd(datadir)
#
# Write some statistics and info about data compilation to file
fn<-paste0(datadir,"data_compilation_log_",dgt,".txt")
write(paste0("Documentation step 0: merge raw data and cleaning for ",dgt," level data.\n\n"),file=fn,append=F)
# 
# Step 1: Load and merge raw data
# 
# load data
f<-dir(path=datadir) # get list of file names
f<-f[str_detect(f,"csv")]
f<-f[!str_detect(f,"nation")]
d<-read_csv(paste0(datadir,"/",f))
d<-as.data.frame(apply(d,2,FUN = function(x){return(as.numeric(as.character(x)))}, simplify = F))
write(paste0("Lines of the raw data (representing bilateral transactions): ",nrow(d), " Number of columns: ",ncol(d)), file=fn, append=T)
#
# Reorder columns
cls<-c(which(str_detect(colnames(d),"source")),which(str_detect(colnames(d),"dest")))
if(length(cls)==4){d<-d[order(d[,cls[1]], d[,cls[2]], d[,cls[3]], d[,cls[4]]),]
}else if(length(cls)==2){d<-d[order(d[,cls[1]], d[,cls[2]]),]}
#
# Harmonize names
colnames(d)[str_detect(colnames(d),"dest_sic")]<-"dest_sic"
colnames(d)[str_detect(colnames(d),"source_sic")]<-"source_sic"
# 
# Get digit level of available disaggregation
dgts<-max(str_length(d$dest_sic))
#
# Add a leading zero to those codes that are at lower digit level because leading zero ignored in numeric data format 
# (5-digit harmonization needed in subsequent steps)
dsic<-unique(c(d$source_sic,d$dest_sic))
dsic0<-dsic
write(paste0("\n\n",sum(str_length(dsic)<dgts), " sector SIC codes have less than ",dgts," digits. \n\nTwo patches are applied: leading zeroes are added and tested for matching with list of SIC codes from ONS. For codes that were non-matching, a second test is applied and zeroes are added at the end of the code. This yields a few additional matches. Statistics are provided below. \n"), file=fn, append=T)
d$source_sic[str_length(d$source_sic)<dgts]<-paste0("0",d$source_sic[str_length(d$source_sic)<dgts])
d$dest_sic[str_length(d$dest_sic)<dgts]<-paste0("0",d$dest_sic[str_length(d$dest_sic)<dgts])
d$source_sic[str_length(d$source_sic)<dgts]<-paste0("0",d$source_sic[str_length(d$source_sic)<dgts])
d$dest_sic[str_length(d$dest_sic)<dgts]<-paste0("0",d$dest_sic[str_length(d$dest_sic)<dgts])

# Step 2: Set non-valid SIC codes to zero
if(dgts == 5){
  # Load list with valid SIC codes (only for 5-digit level data)
  write(paste0("The official lists of SIC codes are obtained from ONS websites. Two lists are used as they do not 100% overlap. Links are provided in the script 0_merge_and_clean_raw_data.r \n"), file=fn, append=T)
  #nms<-read_excel("../../../../public_data/concordances/publisheduksicsummaryofstructureworksheet.xlsx", sheet = "reworked structure")
  #nms<-nms[str_length(nms$`Most disaggregated level`)==5,c("Description", "Sub Class","Most disaggregated level")] # Note: This alternative list of SIC codes is not valid in this project!
  nms2<-fread("public_data/concordances/SIC07_CH_condensed_list_en.csv",sep=",",data.table = F)
  #nms3<-fread("../../../../public_data/concordances/SICs_from_companiesHouse.csv", select = c(2:3), sep=";", header = F, data.table = F)
  #sum(!(nms3$V2 %in% nms2$`SIC Code`))
  #sum(!(nms2$`SIC Code` %in% nms3$V2)) # List obtained from Companies website is identical with SIC 07 condensed list
  # Note: There is something strange with the SIC 5-digit codes from ONS as different source data files do not perfectly overlap. The ONS Payment data appears to make use of both files as one achieves the greatest coverage if using the mixed files
  #sic<-as.character(unique(c(nms$`Most disaggregated level`,nms2$`SIC Code`)))
  sic<-as.character(unique(nms2$`SIC Code`))
  sic[str_length(sic)<5]<-paste0("0",sic[str_length(sic)<5])
  sic<-unique(sic)
  
  
  #sic2<-as.character(unique(c(nms2$`SIC Code`)))
  #sic1<-as.character(unique(c(nms$`Most disaggregated level`)))
  #sic1[str_length(sic1)<5]<-paste0("0",sic1[str_length(sic1)<5])
  #sic2[str_length(sic2)<5]<-paste0("0",sic2[str_length(sic2)<5])
  #sic1<-unique(sic1)
  #sic2<-unique(sic2)
  #nms$Description[nms$`Most disaggregated level`%in%sic1[!(sic1 %in% sic2)]]
  #nms2$Description[nms2$`SIC Code` %in% sic2[!(sic2 %in% sic1)]]
  
  dsic<-as.character(unique(c(d$source_sic, d$dest_sic)))
  dna<-dsic[!(dsic %in% sic)]
  write(paste0("Unique SIC codes in ONS list: ",length(sic),"\nUnique SIC codes in Payment data: ", length(dsic), "\nNumber of Payment SIC codes that could not be matched in first trial with leading zeroes: ",length(dna)), file=fn, append=T)
  print(paste(length(dna), "codes were not matched"))
  # For some codes, a match can be found if a "0" is attached, but not added as a pre-fix. Remove leading "0" and paste "0" at the end of the 5-digit code
  #dna2<-dna[substr(dna,1,1)=="0"]
  #d$source_sic[d$source_sic %in% dna2]<-paste0(substr(d$source_sic[d$source_sic %in% dna2],2,10),"0")
  #d$dest_sic[d$dest_sic %in% dna2]<-paste0(substr(d$dest_sic[d$dest_sic %in% dna2],2,10),"0")
  #dsic<-as.character(unique(c(d$source_sic, d$dest_sic)))
  #dna<-dsic[!(dsic %in% sic)]
  #print(paste(length(dna), "codes were also not matched after leading 0 was replaced by 0 as suffix"))
  #write(paste0("Number of Payment SIC codes that could not be matched in second trial with appended zeroes: ",length(dna),"\n\nAll codes that could not be matched are set to 0. \nThe non-matched codes are: \n",paste0(dna,collapse = "; ")), file=fn, append=T)
  print(paste("Set",paste(dna,collapse = "; "),"to zero."))
  d$source_sic[d$source_sic %in% dna]<-"0"
  d$dest_sic[d$dest_sic %in% dna]<-"0"
  rm(dna, sic, dsic, nms2)
}else{
  d$source_sic[d$source_sic %in% c("00", "000")]<-"0"
  d$dest_sic[d$dest_sic %in% c("00", "000")]<-"0"
}

write(paste0("\nNumber of rows with invalid classification code of either destintation or source sector: ", sum((d$source_sic==0 | d$dest_sic==0))), file=fn, append=T)

if(is.element("dest_region",colnames(d))){from<-paste0(d$source_sic, "_", d$source_region); to<-paste0(d$dest_sic, "_", d$dest_region)
}else{from<-paste0(d$source_sic); to<-paste0(d$dest_sic)}

d<-data.frame(cbind(from, to, d[,colnames(d)[!str_detect(colnames(d), "sic|region")]]))

# Convert statistcially disclosive data points into NAs
for(cl in 3:ncol(d)){d[,cl]<-as.numeric(as.character(d[,cl]))}

rm(from, to, cl, cls)

write(paste0("Data points that where whitened due to statistical disclosure control and tagged with * in the raw data are set to NA. \n"), file=fn, append=T)

write(paste0("Data (including invalid rows) saved to ",paste0("raw_data_merged_",dgt,".RData")), file=fn, append=T)

save(d, file=paste0(datadir,"raw_data_merged_",dgt,".RData"))

