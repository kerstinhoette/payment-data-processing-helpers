# This script makes a simple correlation analysis comparing Payment and ONS input-output data

dt0<-dt

# Set path to plotting directory
plotdir<-paste0(out_dir,"/Correlation_plots_industry_level/")
if(!dir.exists(plotdir)){dir.create(plotdir, recursive = T)}

ONS_benchmark<-"SUT"

dt0$`Total exports of goods and services IxI`<-dt0$`Exports of goods to EU IxI`+dt0$`Exports of goods to rest of the world IxI`
dt0$`Total exports of goods and services PxP`<-dt0$`Exports of goods to EU PxP`+dt0$`Exports of goods to rest of the world PxP`

vars0<-c("code","year","sum_inputs_amt", "sum_outputs_amt", "sum_inputs_cnt", "sum_outputs_cnt", "sum_inputs_IxI", "sum_outputs_IxI", "sum_inputs_PxP", "sum_outputs_PxP", "sum_inputs_SUT", "sum_outputs_SUT", 
        paste(c("Compensation of employees", "Gross operating surplus and mixed income", "Gross valued added at basic prices", "Gross value added", "Total output at basic prices", "Gross fixed capital formation", "Total exports of goods and services", "Total final demand", "Final consumption expenditure", "Total supply of products at purchasers prices"),ONS_benchmark))
colnames(dt0)<-str_replace_all(colnames(dt0),"purchasers'","purchasers")
colnames(dt0)<-str_replace_all(colnames(dt0),"time","year")
vars0<-vars0[vars0%in%colnames(dt0)]
dt0gr<-pdata.frame(dt0, index = c("code", "year"))
dt0gr<-data.frame(cbind(dt0gr[,c("code_year","name")], G(dt0gr, stubs = F, n=1)))
colnames(dt0gr)<-str_replace_all(colnames(dt0gr),"\\."," ")
dt0gr[,!(colnames(dt0gr)%in%c("code_year","name","code","year"))]<-data.frame(apply(dt0gr[,!(colnames(dt0gr)%in%c("code_year","name","code","year" ))], 2, FUN=function(x){return(as.numeric(ifelse((is.nan(x)|is.infinite(x)),NA,x)))}, simplify = F))
colnames(dt0gr)<-str_replace_all(colnames(dt0gr),"\\."," ")
dt00<-dt0

tp<-"growth_rates"
tp<-"levels"
for(tp in c("levels","growth_rates")){
  
  if(tp == "levels"){dt0<-dt00
  }else if(tp == "growth_rates"){dt0<-dt0gr}else{stop("Data type not specified")}
  
  # Keep only relevant years
  dt0<-dt0[dt0$year %in% years, vars0]
  
  # Shorten column names for plotting
  colnames(dt0)<-str_remove_all(colnames(dt0),"sum_")
  colnames(dt0)<-str_replace_all(colnames(dt0),"inputs","Input")
  colnames(dt0)<-str_replace_all(colnames(dt0),"outputs","Output")
  colnames(dt0)<-str_replace_all(colnames(dt0),"_amt","_Value")
  colnames(dt0)<-str_replace_all(colnames(dt0),"_cnt","_Count")
  colnames(dt0)<-shorten_ONS_variable_names(colnames(dt0), legend = T, fn_legend = paste0(out_dir,"Legend_long_short_variable_names_",ONS_benchmark,".tex"))
  
  # Settings for which periods and source data correlation pairs should be written to correlation
  SUT_only<-F; EXCLUDE_IxI<-F
  tm<-18:19
  dt0$year<-str_replace_all(dt0$year,"201","1")
  dt0$year<-str_replace_all(dt0$year,"202","2")
  dt0<-dt0[dt0$year %in% tm,]
  
  ds<-str_replace_all(colnames(dt0)[!(colnames(dt0)%in%c("code","year"))], "_", " ")
  
  # Set similarity measures to be calculated
  SIMILARITY_MEASURES<-c("Pearson_correlation", 
                         "Spearman_correlation",
                         "Kendall_correlation", # Rank correlation measure
                         "inverse_Euclidean", # square root of sum of quadratic distances (may be a bit sensitive to outliers but can deal with zero values) (aka Minkowski grade 2)
                         "inverse_Manhattan", # sum of absolute distances (aka Minkowski grade 1)
                         "cosine_similarity" # Angle between two vectors 
  )
  
  # Reshape from long to wide format (one row per industry, one column for each year-variable)
  dt0<-reshape(dt0, idvar="code", direction = "wide", timevar = "year", sep="_")
  # Keep only columns with available data (exclude missing year obersvations for some data sets)
  dt0<-dt0[,apply(dt0,2,FUN=function(x){return(ifelse(sum(!is.na(x))>0,T,F))})]
  cn<-colnames(dt0)
  
  # Initialize empty table where columns are correlation values (average during 2015-2018 (or 2015-2020 if SUT only)) and rows different data transformations applied
  cortab<-data.frame()
  
  msr<-SIMILARITY_MEASURES[1]; scl<-"raw"
  for(msr in SIMILARITY_MEASURES[1:2]){
    for(scl in c("raw", "log(1+x)", "log")){
      print(paste(msr, scl))
      add_txt<-""
      
      # Get numeric matrix
      X<-as.matrix(dt0[,cn!="code"])
      X[is.na(X)]<-0
      
      colnames(X)<-str_replace_all(colnames(X),"_"," ")
      
      # Apply data transformation
      if(str_detect(scl, "log")){
        if(msr %in% c("Kendall_correlation","Spearman_correlation")){print(paste("Skip log for",msr,"does not make sense")); next}
        if(scl == "log(1+x)"){
          X<-log(X+1)
        }else if(scl == "log"){
          X<-log(X)
          X[is.infinite(X)]<-NA
          zero_links<-"Observations are removed if zero in any of the two compared networks (due to log(0))."
        }
      }else if(scl != "raw"){stop("Implement transformation or cleaning method.")}
      
      
      txt<-paste0("Pairwise_industry_similarities_",msr,"_",scl,"_data_",tp)
      
      if(msr %in% c("Pearson_correlation", "Kendall_correlation", "Spearman_correlation")){
        m<-str_to_lower(str_split(msr,"_",simplify = T))[1]
        C<-cor(X, method = m, use="pairwise.complete.obs")
        rm(m)
      }else if(msr %in% c("inverse_Euclidean","inverse_Manhattan")){
        m<-str_to_lower(str_split(msr,"_",simplify = T))[2]
        
        add_txt<-paste0(add_txt,"\n")
        X<-apply(X,2,FUN=function(x){return(scale(x))})
        add_txt<-paste0(add_txt, " Data is z-scaled before distances are calculated.")
        
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
          zero_links<-"Zero-valued observations are kept by setting them to zero after having taken log."
        }
        C<-as.matrix(cosSparse(X))
        colnames(C)<-rownames(C)<-colnames(X)
      }else{stop(paste(msr,"method is unknown."))}
      
      ttl<-paste0("Pairwise ",str_replace_all(msr,"_"," ")," of industry-level variables (",scl," data)")
      ttl<-""; vars <-"payments_ONS_controls"
      vars<-c("inputs_outputs","payments_ONS_controls")[2]
      for(vars in c("inputs_outputs","payments_ONS_controls")){
        tm1<-19
        if(vars == "inputs_outputs"){
          tm1<-18:19
          vs<-which((substr(rownames(C),(str_length(rownames(C))-1), str_length(rownames(C))) %in% tm1) & str_detect(rownames(C),"Input|Output "))
        }else if(vars == "payments_inputs_ONS_controls"){
          vs<-which((substr(rownames(C),(str_length(rownames(C))-1), str_length(rownames(C))) %in% tm1) & (str_detect(rownames(C), paste0(ONS_benchmark,"|Input Value|Input Count"))))
          vs<-vs[rownames(C)[vs]!=paste("Input",ONS_benchmark)]
        }else if(vars == "payments_outputs_ONS_controls"){
          vs<-which((substr(rownames(C),(str_length(rownames(C))-1), str_length(rownames(C))) %in% tm1) & (str_detect(rownames(C), paste0(ONS_benchmark,"|Output Value|Output Count"))))
          vs<-vs[rownames(C)[vs]!=paste("Output",ONS_benchmark)]
        }else if(vars == "payments_ONS_controls"){
          vs<-which((substr(rownames(C),(str_length(rownames(C))-1), str_length(rownames(C))) %in% tm1) & (str_detect(rownames(C),paste0(ONS_benchmark,"|Value|Count"))))
          vs<-vs[rownames(C)[vs]!="SUT"]
        }else{stop("Make sure you get the right indices of variables you want to plot.")}
        
        # Plot only observations from 2018-2020
        Cpl<-C[vs,vs]
        if(length(tm1)==1){rownames(Cpl)<-colnames(Cpl)<-str_remove_all(colnames(Cpl),paste0(" ", tm1))}
        if(str_detect(vars, "_ONS_controls")){
          colnames(Cpl)<-str_remove_all(colnames(Cpl),paste0(" ",ONS_benchmark))
          if(vars=="payments_ONS_controls"){
            Cpl<-Cpl[str_detect(rownames(Cpl),"Input|Output"),!(str_detect(colnames(Cpl),"Input|Output"))]
          }else{stop("Choose rows and cols to plot")}
        }
        
        if(str_detect(vars,"_ONS_controls")){
          pdf(paste0(plotdir,txt,"_",vars,"_color_",ONS_benchmark,"_",tp,".pdf"))
          if(min(Cpl,na.rm = T)<0){
            corrplot(Cpl, type="full", method="color", addCoef.col ='white', tl.col="black", is.corr = F, number.cex=1, diag=T, title = ttl)
          }else{
            corrplot(Cpl, type="full", method="color", addCoef.col ='white', tl.col="black", is.corr = F, number.cex=1, diag=T, col = COL1('Blues', 200), col.lim = c(0,1), title = ttl)
          }
          
          dev.off()
        }else{
          if(min(Cpl,na.rm = T)<0){lm<-c(-1,1)}else{lm<-c(0,1)}
          pdf(paste0(plotdir,txt,"_",vars,"_",paste0(tm1,collapse = "_"),"_color_",tp,".pdf"), width=7, height=7)
          corrplot(Cpl, type="upper", method="color", col = COL2('RdBu', 200), col.lim = lm, tl.col="black", number.cex=0.5, diag=F, title = ttl)
          #mtext(paste0("Correlations calculated between edgelists. ",zero_links," \nData: ", str_replace_all(paste("Aggregation:",aggr,reg),"_"," "),add_txt), col="black", cex=0.7, side=3, line=-30)
          dev.off()
          pdf(paste0(plotdir,txt,"_",vars,"_",paste0(tm1,collapse = "_"),"_color_with_numbers","_",tp,".pdf"), width=7, height=7)
          corrplot(Cpl, type="upper", method="color", col = COL2('RdBu', 200), col.lim = lm, addCoef.col ='white', tl.col="black", number.cex=0.5, diag=F, title = ttl)
          #mtext(paste0("Correlations calculated between edgelists. ",zero_links," \nData: ", str_replace_all(paste("Aggregation:",aggr,reg),"_"," "),add_txt), col="black", cex=0.7, side=3, line=-30)
          dev.off()
          rm(lm)
        }
        
        
      } # vars in inputs-outputs, payments_ONS_controls  
      
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
          if(ncol(c1)==1 && sum(str_detect(colnames(c),d2))>1){print(paste(d1,d2))}
          #if(colnames(c1)[1]=="c[str_detect(rownames(c), d1), str_detect(colnames(c), d2)]"){stop()}
          if(ncol(c1)==sum(str_detect(rownames(c),d1)) && nrow(c1) == sum(str_detect(colnames(c),d2)) && ncol(c1)!=nrow(c1)){c1<-as.data.frame(t(c1))}
          colnames(c1)<-colnames(c)[str_detect(colnames(c),d2)] 
          rownames(c1)<-rownames(c)[str_detect(rownames(c),d1)]
          # Correlations are averaged for 1-year lags (resp. the previous year whenever data is missing, e.g. for PxP 2016)
          # Make manual patch if 2016 data is excluded for PxP and compute autocorrel. betw. 2015 and 2017 instead
          if(str_detect(d1,"PxP")&&str_detect(d2,"PxP")&&aggr=="CPA_2digit"){colnames(c1)<-str_replace_all(colnames(c1),"15","16"); rownames(c1)<-str_replace_all(rownames(c1),"15","16")} 
          c2<-c()
          #t1<-intersect(as.numeric(str_split(colnames(c1)," ",simplify = T)[,3]),as.numeric(str_split(rownames(c1)," ",simplify = T)[,3]))
          t1<-intersect(as.numeric(substr(colnames(c1),(str_length(colnames(c1))-2),str_length(colnames(c1)))),as.numeric(substr(rownames(c1),(str_length(rownames(c1))-2),str_length(rownames(c1)))))
          if(d1==d2 & length(t1)>1){for(i in t1[2:ncol(c1)]){c2<-c(c2,c1[str_detect(colnames(c1),paste(i)),str_detect(colnames(c1),paste(i-1))])}
          }else{for(i in t1){c2<-c(c2,c1[str_detect(rownames(c1),paste(i)),str_detect(colnames(c1),paste(i))])}}
          if(is.element(NA, c2) || (is.element(1, c2) && str_detect(msr, "correl") && length(t1)>1)){warning(paste("unexpected NA or 1",tp,d1,d2,length(c2),paste(c2,collapse=", ")))}
          cr<-cbind(cr, mean(c2))
          colnames(cr)[ncol(cr)]<-paste0(d1,"-",d2)
        }
      }
      cortab<-rbind(cortab, cr)
      rownames(cortab)[nrow(cortab)]<-paste(msr,scl)
      rm(X, C, ttl, add_txt, txt)
      
    } # scl in scaling methods
  } # msr in SIMILARITY_MEASURES
  
  
  save(cortab, file=paste0(plotdir, "Node_correlations_overview_",ONS_benchmark,"_",tp,".RData"))

  
  load(paste0(plotdir, "Node_correlations_overview_",ONS_benchmark,"_",tp,".RData"))
  rn<-rownames(cortab)
  cortab<-round(cortab,3)
  cortab<-as.data.frame(apply(cortab,2,FUN=function(x){return(str_replace_all(as.character(x),"0\\.","."))}))
  rownames(cortab)<-rn
  rm(rn)
  
  rownames(cortab)<-str_remove_all(rownames(cortab),"_matrix|_similarity|_correlation")
  rownames(cortab)<-str_replace_all(rownames(cortab),"inverse","inv")
  rownames(cortab)<-str_replace_all(rownames(cortab),"_"," ")
  
  cortab$Transf<-ifelse(str_detect(rownames(cortab),"log\\(1\\+x\\)"), "log(1+x)", "log")
  cortab$Transf<-ifelse(str_detect(rownames(cortab)," raw"), "raw", cortab$Transf)
  cortab$Method<-ifelse(str_detect(rownames(cortab),"Pearson"), "Pearson", "Kendall")
  cortab$Method<-ifelse(str_detect(rownames(cortab),"Euclidean"), "Euclidean", cortab$Method)
  cortab$Method<-ifelse(str_detect(rownames(cortab),"Manhattan"), "Manhattan", cortab$Method)
  cortab$Method<-ifelse(str_detect(rownames(cortab),"cosine"), "Cosine", cortab$Method)
  ct<-cortab[,c("Transf","Method",colnames(cortab)[!str_detect(colnames(cortab),"Transf|Method")])]
  
  
  i<-which((ct$Method %in% c("Euclidean","Manhattan","Cosine") | (ct$Transf %in% c("log"))))
  ct[i,1]<-paste0("%",ct[i,1])
  rm(i)
  
  cl<-c(which(!str_detect(colnames(ct),paste0("IxI|"[ONS_benchmark!="IxI"],"PxP|"[ONS_benchmark!="PxP"],"SUT|"[ONS_benchmark!="SUT"],"Transf|Method")) & str_detect(colnames(ct), "Input|Output")))
  cl0<-which(colnames(ct) %in% c("Transf","Method"))
  
  ct1<-ct[,c(cl0,cl[1:9])]
  ct2<-ct[,c(cl0,cl[10:18])]
  ct3<-ct[,c(cl0,cl[19:27])]
  ct4<-ct[,c(cl0,cl[28:36])]
  ct5<-ct[,c(cl0,cl[37:45])]
  ct6<-ct[,c(cl0,cl[46:54])]
  ct7<-ct[,c(cl0,cl[55:63])]
  add<-data.frame(rbind(colnames(ct1), colnames(ct2), colnames(ct3), colnames(ct4), colnames(ct5), colnames(ct6), colnames(ct7)))
  colnames(ct1)<-colnames(ct2)<-colnames(ct3)<-colnames(ct4)<-colnames(ct5)<-colnames(ct6)<-colnames(ct7)<-colnames(add)
  ctp<-rbind(add[1,], ct1, add[2,], ct2, add[3,], ct3, add[4,], ct4, add[5,], ct5, add[6,], ct6, add[7,], ct7)
  rm(ct1, ct2, ct3, ct4, ct5, ct6, ct7, add)
  
  stargazer(ctp, summary = F, column.sep.width = "0pt", rownames = F, colnames = F, label=paste0("tab:industry_correlations_robustness_",ONS_benchmark,"_",tp), title=paste0("Overview of industry level auto- and cross-correlations for ",ONS_benchmark," indicators (",tp,")"), font.size = "tiny")
  
} # for tp in levels, growth rates
rm(tp, SUT_only, dt00, dt0gr)
rm(ct, ctp, cl, cl0, EXCLUDE_IxI, scl, msr,c, c1, cortab, cr, dt0, c2, d1, d11, d2, d22, ds, t1, tm, Cpl, tm1, vs, zero_links, vars, cn)

