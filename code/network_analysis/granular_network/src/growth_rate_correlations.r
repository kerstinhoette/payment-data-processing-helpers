tm<-"year"

dtjoh<-paste0(out_dir,"/data_for_growth_rate_correlations/")
if(!dir.exists(dtjoh)){dir.create(dtjoh)}

if(sum(str_detect(dir(dtjoh),"distances_merged_with_growth_rates"))<2){
  
  write.csv(dfm, file = paste0(dtjoh,"raw_network_montly.csv"))
  write.csv(dfy, file = paste0(dtjoh,"raw_network_yearly.csv"))
  
  
  
  # The following loop creates (for monthyly & annual data, and for counts & values) 
  # (1) input/output share networks (as edgelists), 
  # (2) and industry level panel data with aggregate inputs/outputs in time series format to use function of the plm package
  for(tm in c("month", "year")){
    
    if(tm == "month"){df0<-dfm}else if(tm == "year"){df0<-dfy}
    # Remove non-classified transactions
    df0<-df0[(df0$to != "0" & df0$from != "0"),]
    # Change format
    df1<-melt(df0, id.vars = c("from","to"))
    # Remove empty rows (no transaction in given time period)
    df1<-df1[df1$value>0,]
    # Get aggregate inputs and outputs
    inputs<-aggregate(. ~ to + variable, df1[,c("to","value","variable")], FUN=function(x){return(sum(x,na.rm = T))}, na.action = NULL)
    outputs<-aggregate(. ~ from + variable, df1[,c("from","value","variable")], FUN=function(x){return(sum(x,na.rm = T))}, na.action = NULL)
    # Get input shares
    df1$input_share<-df1$value/inputs$value[match(paste(df1$to,df1$variable), paste(inputs$to,inputs$variable))]
    df1$output_share<-df1$value/outputs$value[match(paste(df1$from,df1$variable), paste(outputs$from,outputs$variable))]
    colnames(df1)<-str_replace(colnames(df1),"value","raw_transactions")
    # Remove NaN's (caused by division by zero)
    df1$input_share[is.nan(df1$input_share)]<-NA
    df1$output_share[is.nan(df1$output_share)]<-NA
    
    # Transform time and variable columns into time series format and create industry level panel data set
    if(tm == "year"){
      df1$time<-as.numeric(gsub("\\D", "", df1$variable))
      df1$variable<-substr(df1$variable,6,8)
      colnames(inputs)<-c("code", "variable", "inputs"); colnames(outputs)<-c("code", "variable", "outputs")
      pdt<-merge(inputs, outputs, by=c("code","variable"), all = T)
      rm(inputs, outputs)
      amt<-pdt[str_detect(pdt$variable,"_amt"),];cnt<-pdt[str_detect(pdt$variable,"_cnt"),]
      colnames(amt)[3:4]<-paste0(colnames(amt)[3:4],"_amt"); colnames(cnt)[3:4]<-paste0(colnames(cnt)[3:4],"_cnt")
      amt$variable<-str_remove_all(amt$variable,"_amt"); cnt$variable<-str_remove_all(cnt$variable,"_cnt") 
      pdt<-merge(amt, cnt, by=c("code","variable"), all = T)
      rm(amt, cnt)
      pdt$time<-as.numeric(substr(pdt$variable,1,4))
      pdt<-pdt[,c("code","time","inputs_amt","outputs_amt","inputs_cnt","outputs_cnt")]
      dfy1<-df1; pdty<-pdt; rm(df1,pdt)
    }else if(tm == "month"){
      # Rename columns and merge inputs/outputs & split by amt/cnt to create a panel
      colnames(inputs)<-c("code", "variable", "inputs"); colnames(outputs)<-c("code", "variable", "outputs")
      pdt<-merge(inputs, outputs, by=c("code","variable"), all = T)
      rm(inputs, outputs)
      amt<-pdt[str_detect(pdt$variable,"_amt_"),];cnt<-pdt[str_detect(pdt$variable,"_cnt_"),]
      colnames(amt)[3:4]<-paste0(colnames(amt)[3:4],"_amt"); colnames(cnt)[3:4]<-paste0(colnames(cnt)[3:4],"_cnt")
      amt$variable<-str_remove_all(amt$variable,"_amt"); cnt$variable<-str_remove_all(cnt$variable,"_cnt") 
      pdt<-merge(amt, cnt, by=c("code","variable"), all = T)
      rm(amt, cnt)
      df1$time<-as.yearmon(paste0(substr(df1$variable,1,3),substr(df1$variable,9,10)),"%b%y")
      df1$variable<-substr(df1$variable,5,7)
      pdt$time<-as.yearmon(paste0(substr(pdt$variable,1,3),substr(pdt$variable,5,6)),"%b%y")
      pdt<-pdt[,c("code","time","inputs_amt","outputs_amt","inputs_cnt","outputs_cnt")]
      pdtm<-pdt; dfm1<-df1; rm(df1, pdt)
    }
  } # tm in year, month
  rm(df0)
  
  # Use monthly data to calculate 1-year growth rates
  # Transform in panel data format
  pdt<-pdata.frame(pdtm, index = c("code", "time"), drop.index = F)
  # Standard growth formula
  pdt<-cbind(pdt, G(pdt, cols = c("inputs_amt","outputs_amt","inputs_cnt","outputs_cnt"), n=12, keep.ids = F))
  # Log diff approximation for growth rates
  pdt<-cbind(pdt, G(pdt, cols = c("inputs_amt","outputs_amt","inputs_cnt","outputs_cnt"), n=12, logdiff = T, keep.ids = F))
  # Rename columns
  colnames(pdt)[str_detect(colnames(pdt), "L12")]<-paste0(rep(c("inputs_amt","outputs_amt","inputs_cnt","outputs_cnt"),2),rep(c("_growth", "_growth_log"), each=4))
  pdtm<-na.omit(pdt)
  
  
  # Make same with annual data for robustness check
  # Transform in panel data format
  pdt<-pdata.frame(pdty, index = c("code", "time"), drop.index = F)
  # Standard growth formula. Note: it's an unbalanced panel and "zeros" are not shown, i.e. treated as NA
  pdt<-cbind(pdt, G(pdt, cols = c("inputs_amt","outputs_amt","inputs_cnt","outputs_cnt"), n=1, keep.ids = F))
  # Log diff approximation for growth rates
  pdt<-cbind(pdt, G(pdt, cols = c("inputs_amt","outputs_amt","inputs_cnt","outputs_cnt"), n=1, logdiff = T, keep.ids = F))
  # Rename columns
  colnames(pdt)[(ncol(pdt)-7):ncol(pdt)]<-paste0(rep(c("inputs_amt","outputs_amt","inputs_cnt","outputs_cnt"),2),rep(c("_growth", "_growth_log"), each=4))
  pdty<-na.omit(pdt)
  rm(pdt)
  
  write.csv(pdtm, file=paste0(dtjoh, "Monthly_industry_panel_with_growth_rates.csv"))
  write.csv(pdty, file=paste0(dtjoh, "Annual_industry_panel_with_growth_rates.csv"))
  
  
  MATRICES<-c("raw_transactions", "input_share", "output_share")
  
  nt<-MATRICES[2]; tp<-"amt" # (as an alternative, distances could be also calculated by using input/output shares of the count data as truncation thresholds)
  TRUNCATION_METHODS<-c("no_truncation","threshold_pct_0.01","threshold_pct_0.025","threshold_pct_0.05","threshold_pct_0.1","threshold_pct_0.5","threshold_pct_1","threshold_pct_2.5", "threshold_pct_5")
  # Select year for calculating distances as a snapshot for the distances
  yr<-"2019"
  yr<-unique(dfy1$time)[2]
  for(nt in MATRICES[2:3]){
    
    if(exists("dist_tab")){rm(dist_tab)}
    if(exists("dist_taby")){rm(dist_taby)}
    for(yr in unique(dfy1$time)){
      
      dfn<-dfy1[str_detect(dfy1$time, as.character(yr)),]
      
      print(paste(nt, yr, nrow(dfn), nrow(dfy1)))
      
      for(tr in TRUNCATION_METHODS){
        
        if(str_detect(tr, "threshold_pct_")){thr<-as.numeric(str_remove_all(tr, "threshold_pct_"))/100
        }else if(tr == "no_truncation"){thr<-0
        }else{stop("implement trunction")}
        
        if(sum(dfn$raw_transactions == 0)>0){stop("remove zero links")}
        x<-as.matrix(dfn[dfn$variable==tp,c("from","to",nt)])
        x<-x[as.numeric(x[,3])>=thr,c(1,2)]
        # Get list of node pairs by distance
        g<-graph_from_edgelist(x, directed = T)
        
        #dst<-distance_table(g,directed = T)
        
        # Get distances (from directed upstream (downstream) network when using input (output) shares)
        if(nt == "input_share"){dst<-distances(g, mode="in", algorithm = "dijkstra")
        }else if(nt == "output_share"){dst<-distances(g, mode="in", algorithm = "dijkstra")}
        # Transform into data frame for conveneince
        dst<-get.data.frame(graph.adjacency(dst,weighted=TRUE, mode = "directed"))
        # Set infinite distances (if nodes are disconnected) to NA
        dst$weight[is.nan(dst$weight)]<-NA
        dst$weight[is.infinite(dst$weight)]<-NA
        
        # Add truncation threshold label to column name
        colnames(dst)<-str_replace(colnames(dst),"weight",paste0("node_distance_",tr))
        dst<-data.frame(cbind(year=yr, dst))
        
        # Add calculated distances to a data frame
        if(!exists("dist_tab")){
          dist_tab<-dst
        }else{
          dist_tab<-merge(dist_tab, dst, by=c("year", "from", "to"), all = T)
        }
      } # tr
      
      if(!exists("dist_taby")){
        dist_taby<-dist_tab
      }else{
        dist_taby<-rbind(dist_taby, dist_tab)
      }
      rm(dist_tab)
    } # yr in years
    dist_tab<-dist_taby
    rm(dist_taby)
    write.csv(dist_tab, file = paste0(dtjoh, "distances_by_truncation_threshold_using_",nt,"_",tp,"_data.csv"))
    
  } # nt
  
  rm(x, dst, g, thr, tr, dfn, yr, nt, tm, tp)
  rm(dist_tab)
  .rs.restartR()
  
  nt<-"input_share"; tp<-"amt"
  dist_tab0<-read.csv(paste0(dtjoh, "distances_by_truncation_threshold_using_",nt,"_",tp,"_data.csv"))
  if(is.element("X",colnames(dist_tab0))){dist_tab0<-dist_tab0[,2:ncol(dist_tab0)]}
  colnames(dist_tab0)<-str_replace_all(colnames(dist_tab0),"year","time")
  dist_tab0<-dist_tab0[dist_tab0$time>2015,]
  # Remove all rows that have only NAs as distance value
  i<-which(apply(dist_tab0,1,FUN=function(x){sum(is.na(x))})==(ncol(dist_tab0)-3))
  if(length(i)>0){dist_tab0<-dist_tab0[-i,]}
  rm(i)
  dist_tab<-dist_tab0
  
  # Merge distance table with growth rate data
  p<-pdty[pdty$time%in%c(2016:2023),]
  p$code<-as.integer(as.character(p$code))
  if(length(unique(dist_tab$time))==1){
    stop("verify this again")
    # Create a time array to match growth rates for different years to distance table (which is one-dimensional in time dimension)
    tm<-data.frame(cbind(time=rep(unique(as.character(p$time)), nrow=dist_tab), from=rep(dist_tab$from,each=length(unique(p$time))), to=rep(dist_tab$to,each=length(unique(p$time)))))
    # Add time column to distance table and copy rows with distance information for each time observation
    dist_tab<-merge(tm, dist_tab, by=c("from","to","time"), all = T)
  }
  
  # Create vectors with indices to match growth rates to distance table
  fr<-match(paste(dist_tab$from,dist_tab$time),paste(p$code,p$time)); to<-match(paste(dist_tab$to,dist_tab$time),paste(p$code,p$time))
  # Create table with growth rates of from and to sectors to calculate correlations conveniently
  temp<-cbind(dist_tab, p[fr,str_detect(colnames(p),"growth")], p[to,str_detect(colnames(p),"growth")])
  colnames(temp)<-c(colnames(dist_tab),paste0(rep(c("from_","to_"),each=sum(str_detect(colnames(p),"growth"))), colnames(p)[str_detect(colnames(p),"growth")]))
  dist_taby<-temp
  rm(temp,fr,to,p,dist_tab)
  write.csv(dist_taby, file=paste0(dtjoh, "distances_merged_with_growth_rates_annual_",nt,"_",tp,".csv"))
  
  
  
  # Create the same for monthly data
  dist_tab<-dist_tab0[dist_tab0$time>2015,colnames(dist_tab0)!="X"]
  p<-pdtm[str_detect(pdtm$time,paste0(c(2016:2023),collapse = "|")),]
  # Remove all rows that have only NAs as distance value
  i<-which(apply(dist_tab,1,FUN=function(x){sum(is.na(x))})==(ncol(dist_tab)-3))
  if(length(i)>0){dist_tab<-dist_tab[-i,]}
  rm(i)
  # Remove all rows that have only NAs as growth rates
  i<-which(apply(p,1,FUN=function(x){sum(is.na(x))})==sum(str_detect(colnames(p),"growth")))
  if(length(i)>0){p<-p[-i,]}
  rm(i)
  # Keep only industry pairs for which growth rates are not NA
  cd<-unique(p$code)
  i<-which(dist_tab$from %in% cd & dist_tab$to %in% cd)
  dist_tab<-dist_tab[i,]
  # Create month year dummy column for distance table
  id<-paste(dist_tab$from,dist_tab$to,dist_tab$time)
  dm<-paste(rep(month.abb,length(id)),rep(dist_tab$time,each=12))
  id12<-rep(id, each=12)
  i<-match(id12, id)
  dist_tab<-dist_tab[i,]
  dist_tab$time<-dm
  rm(id,id12,i,dm)
  # Keep only data that can be matched to growth data
  i<-which(paste(dist_tab$from,dist_tab$time) %in% paste(p$code,p$time) & paste(dist_tab$to,dist_tab$time) %in% paste(p$code,p$time))
  dist_tab<-dist_tab[i,]
  # Merge data
  fr<-match(paste(dist_tab$from,dist_tab$time),paste(p$code,p$time))
  add<-p[fr,]
  if(sum(add$code != dist_tab$from)>0 || sum(add$time != dist_tab$time)>0){stop("seems there is an error in matching")}
  colnames(add)<-paste0("from_",colnames(add))
  dist_tab<-cbind(dist_tab, add[,str_detect(colnames(add),"growth")])
  to<-match(paste(dist_tab$to,dist_tab$time),paste(p$code,p$time))
  add<-p[to,]
  if(sum(add$code != dist_tab$to)>0 || sum(add$time != dist_tab$time)>0){stop("seems there is an error in matching")}
  colnames(add)<-paste0("to_",colnames(add))
  dist_tab<-cbind(dist_tab, add[,str_detect(colnames(add),"growth")])
  dist_tabm<-dist_tab
  write.csv(dist_tabm, file=paste0(dtjoh, "distances_merged_with_growth_rates_montly_",nt,"_",tp,".csv"))
  rm(dist_tab, add, p, to, fr, dist_tab0, cd, i, dist_tabm)
  
}


rm(dfm, dfy)
nt<-MATRICES[2]; tp<-"amt" # (as an alternative, distances could be also calculated by using input/output shares of the count data as truncation thresholds)
dist_tabm<-fread(paste0(dtjoh, "distances_merged_with_growth_rates_montly_",nt,"_",tp,".csv"),data.table=F)
dist_taby<-fread(paste0(dtjoh, "distances_merged_with_growth_rates_annual_",nt,"_",tp,".csv"),data.table=F)
dist_tabm<-dist_tabm[,colnames(dist_tabm)!="V1"]
dist_taby<-dist_taby[,colnames(dist_taby)!="V1"]

meth<-"spearman"
cl<-colnames(dist_tabm)[str_detect(colnames(dist_tabm), "node_distance_")][1]
i<-1; frq<-"month"
# Select time period for calculating correlations; excluding Covid means dropping all data from 2020 onwards
# Note: Likely, one should also drop 2016 for the annual data; also because ONS said that the data from 2015 is of poor quality
tm<-c(2016:2023) 
for(meth in c("pearson","spearman")){
  
  for(frq in c("month", "year")){

    if(frq == "month"){
      dist_tab<-dist_tabm[str_detect(dist_tabm$time, paste0(tm,collapse="|")),]
    }else if(frq == "year"){
      dist_tab<-dist_taby[str_detect(dist_taby$time, paste0(tm,collapse="|")),]
    }
    
    
    # Get growth rate correlations for each distance level & each truncation threshold
    if(exists("tab0")){rm(tab0)}
    for(cl in colnames(dist_tab)[str_detect(colnames(dist_tab), "node_distance_")]){
      for(i in min(dist_tab[,cl], na.rm = T):max(dist_tab[,cl],na.rm = T)){
        tab<-data.frame(distance=i, truncation=(str_remove(cl, "node_distance_")))
        j<-which(dist_tab[,cl]==i)
        grcols<-colnames(dist_tab)[str_detect(colnames(dist_tab),"growth")]
        grcols<-str_remove_all(grcols, "from_")
        grcols<-unique(str_remove_all(grcols, "to_"))
        cl2<-grcols[1]
        for(cl2 in grcols){
          fr<-dist_tab[j,paste0("from_",cl2)]
          to<-dist_tab[j,paste0("to_",cl2)]
          cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
          tab$add<-cr
          colnames(tab)[ncol(tab)]<-paste0(cl2)
        } # cl2 in growth columns
        
        if(!exists("tab0")){tab0<-tab}else{tab0<-rbind(tab0, tab)}
        
        #fr<-dist_tabm$from_inputs_amt_growth[j]
        #to<-dist_tabm$to_inputs_amt_growth[j]
        #cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
        #tab$inputs_amt_growth<-cr
        #print(paste(cl, i, "input amt", cr))
        #fr<-dist_tabm$from_outputs_amt_growth[j]
        #to<-dist_tabm$to_outputs_amt_growth[j]
        #cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
        #tab$outputs_amt_growth<-cr
        #print(paste(cl, i, "output amt", cr))
        #fr<-dist_tabm$from_inputs_cnt_growth[j]
        #to<-dist_tabm$to_inputs_cnt_growth[j]
        #cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
        #tab$inputs_cnt_growth<-cr
        #print(paste(cl, i, "input cnt", cr))
        #fr<-dist_tabm$from_outputs_cnt_growth[j]
        #to<-dist_tabm$to_outputs_cnt_growth[j]
        #cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
        #tab$outputs_cnt_growth<-cr
        #print(paste(cl, i, "output cnt", cr))
        #if(!exists("tab0")){tab0<-tab}else{tab0<-rbind(tab0, tab)}
      } # i in levels of available distances
    } # cl in truncation levels
    
    
    save(tab0, file=paste0(dtjoh, "Growth_rate_",frq,"_",meth,"_correlation_tables_",nt,"_",tp,"_",paste0(tm,collapse = "|"),".RData"))
    
  } # frq in month, year

  if(1==2){ 
    if(exists("tab0")){rm(tab0)}
    for(cl in colnames(dist_taby)[str_detect(colnames(dist_taby), "node_distance_")]){
    for(i in min(dist_taby[,cl], na.rm = T):max(dist_taby[,cl],na.rm = T)){
      tab<-data.frame(distance=i, truncation=(str_remove(cl, "node_distance_")))
      j<-which(dist_taby[,cl]==i)
      fr<-dist_taby$from_inputs_amt_growth[j]
      to<-dist_taby$to_inputs_amt_growth[j]
      cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
      tab$inputs_amt_growth<-cr
      print(paste(cl, i, "input amt", cr))
      fr<-dist_taby$from_outputs_amt_growth[j]
      to<-dist_taby$to_outputs_amt_growth[j]
      cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
      tab$outputs_amt_growth<-cr
      print(paste(cl, i, "output amt", cr))
      fr<-dist_taby$from_inputs_cnt_growth[j]
      to<-dist_taby$to_inputs_cnt_growth[j]
      cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
      tab$inputs_cnt_growth<-cr
      print(paste(cl, i, "input cnt", cr))
      fr<-dist_taby$from_outputs_cnt_growth[j]
      to<-dist_taby$to_outputs_cnt_growth[j]
      cr<-cor(fr,to, use = "pairwise.complete.obs", method=meth)
      tab$outputs_cnt_growth<-cr
      print(paste(cl, i, "output cnt", cr))
      if(!exists("tab0")){tab0<-tab}else{tab0<-rbind(tab0, tab)}
    }
  }
    taby<-tab0
  } # if 1==2
  
  rm(tab0, cl, i, cr, fr, to, tab, frq, cl2)
  
} # meth in spearman, pearson


rm(meth, tm, tp, j, dist_tabm, dist_taby, pdtm, pdty, nt, grcols, pcor, dist_tab)


plotdir<-paste0(out_dir, "Growth_rate_correlations/")
if(!dir.exists(plotdir)){dir.create(plotdir)}


# Load data and plot
frq<-"month"; meth<-"spearman"; nt<-MATRICES[2]; tp<-"amt"; tm<-c(2016:2023)

for(frq in c("month","year")){
  for(meth in c("pearson", "spearman")){
    
    load(paste0(dtjoh, "Growth_rate_",frq,"_",meth,"_correlation_tables_",nt,"_",tp,"_",paste0(tm,collapse = "|"),".RData"))
    tab0$truncation<-ifelse(tab0$truncation=="no_truncation",0,as.numeric(str_remove(tab0$truncation,"threshold_pct_")))
    
    pdt0<-melt(tab0, id.vars=c("distance","truncation"))
    
    # Keep only a subset of the truncation levels available
    trs<-c(0,0.01,0.025,0.05)
    pdt0<-pdt0[pdt0$truncation%in%trs,]
    pdt0$truncation<-as.character(pdt0$truncation)
    pdt0$truncation<-str_replace_all(pdt0$truncation,"0.05","5%")
    pdt0$truncation<-str_replace_all(pdt0$truncation,"0.025","2.5%")
    pdt0$truncation<-str_replace_all(pdt0$truncation,"0.01","1%")
    pdt0$truncation<-str_replace_all(pdt0$truncation,"0","none")
    
    pdt0$Truncation<-as.factor(pdt0$truncation)
    pdt0$variable<-str_remove_all(pdt0$variable,"_growth")
    pdt0$variable<-str_replace_all(pdt0$variable,"inputs_","Input ")
    pdt0$variable<-str_replace_all(pdt0$variable,"outputs_","Output ")
    pdt0$variable<-str_replace_all(pdt0$variable,"amt","Value")
    pdt0$variable<-str_replace_all(pdt0$variable,"cnt","Count")
    
    v<-"log_"
    yl<-paste0(str_to_title(meth)," correlation coefficient")
    for(v in c("","log_")){
      pdf(paste0(plotdir,"Growth_correl_",meth,"_",v,frq,"_",tp,"_",nt,"_",paste0(tm,collapse = "_"),".pdf"),height=4,width =12)
      
      if(v == "log_"){
        pdt<-pdt0[str_detect(pdt0$variable,"log"),] #[pdt0$variable==v,]
        pdt$variable<-str_remove_all(pdt$variable,"_log")
      }else{
        pdt<-pdt0[!str_detect(pdt0$variable,"log"),] #[pdt0$variable==v,]
      }
      
      g<-ggplot(pdt, aes(x=distance,y=value,color=Truncation)) + geom_line() + geom_point()
      g<-g + xlim(c(1,5)) + theme_bw() + ylab(yl) + xlab("Pairwise node distance")
      g<-g + facet_wrap(~variable, ncol=4)  
      g<-g + theme(legend.position = c(0.04,0.2), legend.background = element_rect(colour = "black",linewidth = 0.2), legend.title = element_blank())
      #g<-g + theme(legend.position = c(0.1,0.65), legend.background = element_rect(colour = "black",linewidth = 0.2))
      plot(g)  
      
      dev.off()
    }
    
  } # meth in pearson, spearman
} # frq in month, year

rm(meth, g, pdt, pdt0, tab0, nt, tm, frq, v, trs, yl, plotdir)



