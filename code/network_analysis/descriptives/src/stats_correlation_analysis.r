# This script makes a simple correlation analysis comparing Payment and ONS input-output data

df0<-df

# Set path to plotting directory
plotdir<-paste0(out_dir,"/Correlation_plots/")
if(!dir.exists(plotdir)){dir.create(plotdir, recursive = T)}

# Keep only relevant years
df0<-df0[which(colnames(df0) %in% c("from","to") | (str_detect(colnames(df0),paste0(c(years),collapse = "|")) )) ]

# Shorten column names for plotting
colnames(df0)<-str_remove_all(colnames(df0),"_IOT")
colnames(df0)<-str_replace_all(colnames(df0), "_", " ")
colnames(df0)<-str_replace_all(colnames(df0), " 20", " ")

cn<-colnames(df0)
# Get sum of from/to flows at industry level (used to calculate shares matrices)
outputs <- aggregate(. ~ to, df0[which(!(cn %in% c("from", "from names", "to names")))], sum)
inputs <- aggregate(. ~ from, df0[which(!(cn %in% c("to", "from names", "to names")))], sum)
outputs<-merge(df0["to"],outputs,all.x = T,all.y = T)
inputs<-merge(df0["from"],inputs,all.x = T,all.y = T)

# Settings for which periods and source data correlation pairs should be written to correlation
SUT_only<-F; EXCLUDE_IxI<-F
tm<-16:21
#tm<-c(15:18,c(19:20)[SUT_only]) # time periods to compare
ds<-c("Value","Count","SUT","PxP"[!SUT_only],"IxI"[!SUT_only && !EXCLUDE_IxI]) # datasets to compare

# Set similarity measures to be calculated
SIMILARITY_MEASURES<-c("Pearson_correlation", 
                       "Kendall_correlation", # Rank correlation measure
                       "inverse_Euclidean", # square root of sum of quadratic distances (may be a bit sensitive to outliers but can deal with zero values) (aka Minkowski grade 2)
                       "inverse_Manhattan" # sum of absolute distances (aka Minkowski grade 1)
                       #"cosine_similarity" # Angle between two vectors 
)

# Initialize empty table where columns are correlation values (average during 2015-2018 (or 2015-2020 if SUT only)) and rows different data transformations applied
cortab<-data.frame()

DTYPE<-MATRICES[1]; d_type<-""; tr<-TRUNCATION_METHODS[1]
for(DTYPE in MATRICES){
  cn<-colnames(df0)
  cl<-which(!(cn%in%c(c("to", "from", "to names", "from names"))))
  
  if(DTYPE == "input_share_matrix"){df1[,cl]<-df0[,cl]/outputs[,2:ncol(outputs)]
  }else if(DTYPE == "output_share_matrix"){df1[,cl]<-df0[,cl]/inputs[,2:ncol(inputs)]
  }else{df1<-df0}
  # Set zero links to NA to be ignored in correlations and truncation process
  df1[,cl]<-apply(df1[,cl],2,FUN=function(x){return(ifelse((is.nan(x)|(x==0)|is.infinite(x)),NA,x))})
  rm(cl, cn)
  
  
  # Running this code for different directions does not make sense! Symmetric correlation measures. 
  #for(d_type in DIRECTIONS){
  
  #cn<-colnames(df1)
  #if(d_type=="upstream"){
  #  df2<-df1
  #}else if(d_type=="downstream"){ # swap from and to columns to get the transposed network
  #  df2<-df1[,c(c("to", "from", "to names", "from names"),cn[!(cn %in% c("to", "from", "to names", "from names"))])]
  #  colnames(df2)<-cn
  #}
  #rm(cn)
  df2<-df1
  d_type<-""
  tr<-TRUNCATION_METHODS[1]
  for(tr in TRUNCATION_METHODS[1]){
    
    df3<-df2
    # Keep only relevant years and create correlation matrix
    cl<-which(str_detect(colnames(df3), paste0(tm,collapse = "|")))
    df3[,cl]<-apply(df3[,cl],2,FUN=function(x){return(as.numeric(as.character(x)))})
   
    # Keep only edges in graph that exceed threshold level
    if(tr != "no_truncation"){
      for(col in which(!(colnames(df3) %in% c("from", "to", "from names", "to names")))){
        x<-df3[,col]
        if(tr == "median"){thr<-median(x[x>0],na.rm=T)
        }else if(tr == "mean"){thr<-mean(x[x>0],na.rm=T)
        }else if(str_detect(tr,"threshold_pct_")){
          if(DTYPE=="raw_transactions_matrix"){next
          }else if(str_detect(DTYPE,"share_matrix")){thr<-as.numeric(str_remove(tr,"threshold_pct_"))}
        }else{warning(paste(tr,"truncation method not implemented")); next}
        df3[,col]<-ifelse((!is.na(x) & x<thr),0,x)
      }
      rm(col,x,thr)
    }
    
    msr<-SIMILARITY_MEASURES[1]; scl<-"raw"
    for(msr in SIMILARITY_MEASURES[1]){
      for(scl in c("raw", "log(1+x)", "log")){
        print(paste(msr, scl, tr))
        add_txt<-""
        X<-as.matrix(df3[,cl])
        X[is.na(X)]<-0
        zero_links<-"Zero are only kept if two industries are connected in any of the networks."
        if(str_detect(scl, "log")){
          if(msr %in% c("Kendall_correlation")){print(paste("Skip log for",msr,"does not make sense", DTYPE, d_type, tr)); next}
          if(scl == "log(1+x)"){
            X<-log(X+1)
          }else if(scl == "log"){
            X<-log(X)
            X[is.infinite(X)]<-NA
            zero_links<-"Links are removed if the weight is zero in any of the two compared networks (due to log(0))."
          }
        }else if(scl != "raw"){stop("Implement transformation or cleaning method.")}
        
        
        txt<-paste0("Pairwise_network_similarities_",tr,"_",DTYPE,"",d_type,"_",msr,"_",scl,"_data")
        
        if(msr %in% c("Pearson_correlation", "Kendall_correlation")){
          m<-str_to_lower(str_split(msr,"_",simplify = T))[1]
          C<-cor(X, method = m, use="pairwise.complete.obs")
          rm(m)
        }else if(msr %in% c("inverse_Euclidean","inverse_Manhattan")){
          m<-str_to_lower(str_split(msr,"_",simplify = T))[2]
          
          add_txt<-paste0(add_txt,"\n")
          if(DTYPE == "raw_transactions_matrix"){
            X<-apply(X,2,FUN=function(x){return(scale(x))})
            add_txt<-paste0(add_txt, " Data is z-scaled before distances are calculated.")
          }
          X<-t(X) # dist() function operates row-wise
          dst<-as.matrix(dist(X, method=m, diag = T))
          sim<-1/dst
          sim[is.infinite(sim)]<-NA
          # Rescale similarity matrix by (max-min) range to obtain values between 0 and 1
          #prcss<-preProcess(sim, method = "range")
          #C <- as.matrix(predict(prcss, as.data.frame(sim)))
          C<-sim*(1/(max(sim,na.rm=T)))
          add_txt<-paste0(add_txt, " Similarity matrix is scaled by its max to obtain values in [0,1].")
          rm(sim, dst, m)
        }else if(msr == "cosine_similarity"){
          if(scl=="log"){
            X[is.na(X)]<-0
            zero_links<-"Zero-links are kept by setting them to zero after having taken log."
          }
          C<-as.matrix(cosSparse(X))
          colnames(C)<-rownames(C)<-colnames(X)
        }else{stop(paste(msr,"method is unknown."))}
        
        rownames(C)<-colnames(C)<-str_replace_all(rownames(C),"amt", "Value")
        rownames(C)<-colnames(C)<-str_replace_all(rownames(C),"cnt", "Count")
        rownames(C)<-colnames(C)<-str_remove_all(rownames(C),"ONS ")
        rownames(C)<-colnames(C)<-str_remove_all(rownames(C),"Payment ")
        
        ttl<-paste0("Pairwise ",str_replace_all(msr,"_"," ")," of network edgelists (",scl," data)")
        if(length(tm)<3){nmz<-1}else{nmz<-0.6}
        pdf(paste0(plotdir,txt,"_",paste0(tm[1],"_",tm[length(tm)]),"_color_with_number.pdf"), width=7, height=7)
        corrplot(C, type="upper",method="color", col = COL2('RdBu', 200), col.lim = c(0,1), addCoef.col ='white', tl.col="black", number.cex=nmz, diag=F, mar=c(1,1,2,1))
        dev.off()
        pdf(paste0(plotdir,txt,"_",paste0(tm[1],"_",tm[length(tm)]),"_color.pdf"), width=7, height=7)
        corrplot(C, type="upper",method="color", col = COL2('RdBu', 200), col.lim = c(0,1), tl.col="black", diag=F, mar=c(1,1,2,1))
        dev.off()
        
        # Add avg. correlation and autocorrelation for overlapping subperiod to table
        i<-which(str_detect(rownames(C), paste0(tm,collapse = "|")) & str_detect(colnames(C), paste0(ds,collapse = "|")))
        if(sum(rownames(C) != colnames(C))>0){stop("Row and col names should be equal.")}
        c<-C[i,i]
        d1<-ds[1]; d2<-ds[2]
        cr<-data.frame(matrix(NA,nrow=1,ncol=0))
        d11<-1
        for(d11 in 1:length(ds)){
          d1<-ds[d11]
          d22<-d11
          for(d22 in d11:length(ds)){
            d2<-ds[d22]
            # Extract subset of the correlation matrix with correlations between d1 and d2
            c1<-as.data.frame(c[str_detect(rownames(c),d1), str_detect(colnames(c),d2)])
            colnames(c1)<-colnames(c)[str_detect(colnames(c),d2)]; rownames(c1)<-rownames(c)[str_detect(rownames(c),d1)]
            # Correlations are averaged for 1-year lags (resp. the previous year whenever data is missing, e.g. for PxP 2016)
            # Make manual patch if 2016 data is excluded for PxP and compute autocorrel. betw. 2015 and 2017 instead
            if(d1=="PxP"&&d2=="PxP"&&aggr=="CPA_2digit"){colnames(c1)<-str_replace_all(colnames(c1),"15","16"); rownames(c1)<-str_replace_all(rownames(c1),"15","16")} 
            c2<-c()
            t1<-intersect(as.numeric(str_split(colnames(c1)," ",simplify = T)[,1]),as.numeric(str_split(rownames(c1)," ",simplify = T)[,1]))
            if(d1==d2){for(i in t1[2:ncol(c1)]){c2<-c(c2,c1[str_detect(colnames(c1),paste(i)),str_detect(colnames(c1),paste(i-1))])}
            }else{for(i in t1){c2<-c(c2,c1[str_detect(rownames(c1),paste(i)),str_detect(colnames(c1),paste(i))])}}
            if(is.element(NA, c2) || (is.element(1, c2) && str_detect(msr, "correl"))){stop("unexpected NA or 1")}
            cr<-cbind(cr, mean(c2))
            colnames(cr)[ncol(cr)]<-paste0(d1,"-",d2)
          }
        }
        cortab<-rbind(cortab, cr)
        rownames(cortab)[nrow(cortab)]<-paste(DTYPE,d_type,tr,msr,scl)
        rm(X, C, ttl, add_txt, txt, zero_links)
        
      } # scl in scaling methods
    } # msr in SIMILARITY_MEASURES
    
    
    rm(df3)
  } # tr in TRUNCATION METHODS
  rm(df2)
  #} # d_type in DIRECTIONS
} # DTYPE in MATRICES
rm(tr, DTYPE, d_type, df1, inputs, outputs, SIMILARITY_MEASURES, scl, msr)
rm(c, c1, cr, c2, d1, d11, d2, d22, ds, i, t1, cl)


save(cortab, file=paste0(out_dir, "Edgecorrelations_overview_",paste0(tm,collapse = "_"),".RData"))
load(paste0(out_dir, "Edgecorrelations_overview_",paste0(tm,collapse = "_"),".RData"))
rn<-rownames(cortab)
cortab<-round(cortab,3)
cortab<-as.data.frame(apply(cortab,2,FUN=function(x){return(str_replace_all(as.character(x),"0\\.","."))}))
rownames(cortab)<-rn
rm(rn)
  
rownames(cortab)<-str_remove_all(rownames(cortab),"_matrix|_similarity|_correlation")
rownames(cortab)<-str_replace_all(rownames(cortab),"inverse","inv")
rownames(cortab)<-str_replace_all(rownames(cortab),"_"," ")

cortab$Truncation<-ifelse(str_detect(rownames(cortab),"no truncation"), "none", "mean")
cortab$Truncation<-ifelse(str_detect(rownames(cortab),"median"), "median", cortab$Truncation)
cortab$Transf<-ifelse(str_detect(rownames(cortab),"log\\(1\\+x\\)"), "log(1+x)", "log")
cortab$Transf<-ifelse(str_detect(rownames(cortab)," raw"), "raw", cortab$Transf)
cortab$Method<-ifelse(str_detect(rownames(cortab),"Pearson"), "Pearson", "Kendall")
cortab$Method<-ifelse(str_detect(rownames(cortab),"Euclidean"), "Euclidean", cortab$Method)
cortab$Method<-ifelse(str_detect(rownames(cortab),"Manhattan"), "Manhattan", cortab$Method)
cortab$Method<-ifelse(str_detect(rownames(cortab),"cosine"), "Cosine", cortab$Method)
cortab$Network<-str_split(rownames(cortab),"  ",simplify = T)[,1]
i<-c(min(which(cortab$Network=="raw transactions")),min(which(cortab$Network=="input share")),min(which(cortab$Network=="output share")))
ct<-cortab[,c("Truncation","Transf","Method",colnames(cortab)[!str_detect(colnames(cortab),"Truncation|Transf|Method|Network")])]

add<-data.frame(cbind(c(paste0("\\hline \\multicolumn{",ncol(ct),"}{l}{",c("Raw transactions", "Input shares", "Output shares"),"} \\hline"))),matrix("",nrow=3,ncol=(ncol(ct)-1)))
colnames(add)<-colnames(ct)
ct<-rbind(add[1,],ct[i[1]:nrow(ct),])
ct<-rbind(ct[(1:i[2]-1),],add[2,],ct[i[2]:nrow(ct),])
ct<-rbind(ct[1:(i[3]-1),],add[3,],ct[i[3]:nrow(ct),])
i<-which((ct$Method %in% c("Euclidean","Manhattan","Cosine") | (ct$Transf %in% c("log")) | (ct$Truncation %in% c("median"))))
ct[i,1]<-paste0("%",ct[i,1])


stargazer(ct, summary = F, column.sep.width = "0pt", rownames = F)

rm(df0, i, atm, ct, cortab)
