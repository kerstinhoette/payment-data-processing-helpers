library(matlib); library(readr)
# Calculate and plot CCDF of Katz Bonacich centrality
# 
# 
df0<-dfy
# Remove non-classified transactions
df0<-df0[(df0$to != "0" & df0$from != "0"),]
REMOVE_PUBLIC_SECTOR<-T
if(REMOVE_PUBLIC_SECTOR){
  df0<-df0[(df0$to != "84110" & df0$from != "84110"),]
}
# Change format
df0<-melt(df0, id.vars = c("from","to"))

# Function used later to get the centrality
katz_bonacich_centrality <- function(adjacency_matrix, alpha, beta = 0, max_iter = 1000, tol = 1e-12, names = T) {
  n<-nrow(adjacency_matrix)
  A<-adjacency_matrix
  I<-diag(n)
  
  # Initialize start vector
  centrality <- rep(1, n)
  if(!is.null(names)){names(centrality)<-row.names(adjacency_matrix)}
  old_centrality <- centrality
  # Run recursive loop
  for (iter in 1:max_iter) {
    centrality <- alpha * A %*% centrality + beta
    # Normalise
    centrality <- centrality / sum(centrality)
    # Check for convergence
    if (sum(abs(centrality - old_centrality)) < tol) {
      print(paste0("Convergence with ", sum(abs(centrality - old_centrality))," precision"))
      break
    }
    old_centrality <- centrality
  }
  return(centrality)
}
# Function used later to get the centrality
influence_vector <- function(adjacency_matrix, alpha, beta = 0, max_iter = 1000, tol = 1e-12, names = T) {
  n<-nrow(adjacency_matrix)
  A<-adjacency_matrix
  I<-diag(n)
  
  iv<-(alpha/n) * solve(I - (1-alpha) * A) %*% rep(1,n)
  
  if(names){names(iv)<-rownames(A)}
  
  return(iv)
}



plotdir<-paste0(out_dir,"Centrality_and_powerlaw/","public_admin_removed/"[REMOVE_PUBLIC_SECTOR])
if(!dir.exists(plotdir)){dir.create(plotdir)}

# Use 0.5 as alpha/lambda value (as used e.g. by Magermann 2016). 
alpha <- 0.5  # 
yr<-years[2]; tp<-"amt"; dir<-"input_share"
TRUNCATE<-F
fn<-paste0(plotdir,"Powerlaw_fit_statistics.txt")
write("Test statistics from fitting power law using pl.fit function in R:\n\n",file=fn,append = F)
if(exists("dall")){rm(dall)}; if(exists("plfit_stats")){rm(plfit_stats)}
for(tp in c("amt","cnt")){
  for(TRUNCATE in c(F, T)){
  for(alpha in c(0.5, 0.3, 0.7)){
    for(yr in years){
      
      # Remove empty rows (no transaction in given time period) & keep only relevant year
      df1<-df0[(df0$value>0 & str_detect(df0$variable, as.character(yr)) & str_detect(df0$variable,tp)),]
      
      # Truncation implemented on the basis of raw transaction values to make sure that rowsums of input share matrix equal one, also after truncation
      if(TRUNCATE){
        thr<-0.1
        df1<-df1[df1$value > quantile(df1$value,thr),]
      }
      
      # Get aggregate inputs and outputs
      inputs<-aggregate(. ~ to + variable, df1[,c("to","value","variable")], FUN=function(x){return(sum(x,na.rm = T))}, na.action = NULL)
      outputs<-aggregate(. ~ from + variable, df1[,c("from","value","variable")], FUN=function(x){return(sum(x,na.rm = T))}, na.action = NULL)
      # Get input shares
      df1$input_share<-df1$value/inputs$value[match(paste(df1$to,df1$variable), paste(inputs$to,inputs$variable))]
      df1$output_share<-df1$value/outputs$value[match(paste(df1$from,df1$variable), paste(outputs$from,outputs$variable))]
      rm(inputs, outputs)
      colnames(df1)<-str_replace(colnames(df1),"value","raw_transactions")
      # Remove NaN's (caused by division by zero)
      df1$input_share[is.nan(df1$input_share)]<-NA
      df1$output_share[is.nan(df1$output_share)]<-NA
      df2<-df1[,c("from", "to", dir)]
      
      colnames(df2)[3]<-"weight"
      
      df2<-df2[(df2$from!="0" & df2$to!="0" & df2$weight>0 & !is.na(df2$weight)),]
      
      g<-graph_from_edgelist(as.matrix(df2[,c(1,2)]))
      E(g)$weights<-df2$weight
      rm(df2)
      
      #centr<-alpha.centrality(g, alpha = 0.5,  weights = E(g)$weights, loops = T)
      #centr2<-alpha.centrality(g, alpha = 0.5)
      #centr3<-katzcent(g, alpha = 0.5)
      #msrs<-proper_centralities(g)
      #centr4<-calculate_centralities(g, include = c("Alpha Centrality", "Bonacich power centralities of positions", "Page Rank", "Katz Centrality (Katz Status Index)"))
      
      adj<-get.adjacency(g, sparse=F, attr = "weights")
      row.names(adj)<-colnames(adj)<-names(V(g))
      adj<-adj[order(rownames(adj)),order(colnames(adj))]
      #x<-katz_bonacich_centrality(adj, alpha)
      x<-influence_vector(adj, alpha)
      
      range(x)

      #fit_power_law(x)
      pl.fit <- power.law.fit(x)
      #print(paste0("Results for ",yr," ", tp, " truncated "[TRUNCATE], " lambda ", alpha, ": ", paste(names(pl.fit), pl.fit, collapse = "; ")))
      
      write(paste0("/n/nResults for ",yr," ", tp, " truncated "[TRUNCATE], " lambda ", alpha, ": \n", paste(names(pl.fit), pl.fit, collapse = "; ")), file=fn, append = T)
      
      msr<-paste("Katz-Bonacich",alpha, dir, tp, "truncated data by 10% quantile"[TRUNCATE])
      if(!exists("dall")){
        dall<-data.frame(cbind(year=yr, code=row.names(x), measure=msr, value=x[,1]))
        plfit_stats<-data.frame(measure=msr,year=yr,data_type=tp,pl.fit)
      }else{
        dall<-rbind(dall,data.frame(cbind(year=yr, code=row.names(x), measure=msr, value=x[,1])))
        plfit_stats<-rbind(plfit_stats, data.frame(measure=msr,year=yr,data_type=tp,pl.fit))
      }
      
      rm(pl.fit, g, x, adj)
      
    } # yr in years
  } # alpha in 
} # TRUNCATE in F,T
} # tp in amt, cnt

rm(alpha, msr, thr, TRUNCATE, yr)

dall$value<-as.numeric(dall$value); dall$year<-as.factor(dall$year)



# Plot and make cosmetics
alpha<-0.5; TRUNCATE<-F; yrs<-as.character(seq(2017,2023,2))
for(alpha in c(0.5, 0.3, 0.7)){
  #alpha<-c(0.5, 0.3, 0.7)
  for(TRUNCATE in c(F,T)){
    
    pdt<-dall[str_detect(dall$measure, paste(alpha,collapse="|")),]
    if(!TRUNCATE){pdt<-pdt[!str_detect(pdt$measure, "truncated"),]
    }else{pdt<-pdt[str_detect(pdt$measure, "truncated"),]}
    pdt<-pdt[pdt$year %in% yrs,]
    pdt$alpha<-paste("lambda = ",str_split(pdt$measure," ",simplify = T)[,2])
    pdt$data_type<-ifelse(str_detect(pdt$measure,"amt"),"Value","Count")
    sts<-plfit_stats[plfit_stats$measure%in%unique(pdt$measure),]
    #pdt$year<-(paste0(pdt$year,", g = ",round(plfit_stats$alpha-1))[match(paste(pdt$measure,pdt$year),paste(plfit_stats$measure,plfit_stats$year))])
    gammas<-sts$alpha
    names(gammas)<-paste(sts$year, sts$data_type)
    
    
    pdt<-pdt[order(pdt$value),]
    
    pdt$ccdf<-NA
    for(yr in unique(pdt$year)){
      for(tp in unique(pdt$data_type)){
        pdt2<-pdt[pdt$year==yr & pdt$data_type==tp,]
        x<-pdt$value[pdt$year==yr & pdt$data_type==tp]
        f<-ecdf(x)
        pdt$ccdf[pdt$year==yr & pdt$data_type==tp]<-1-f(x)
      }
    }
    
    
    x<-pdt$value
    f<-ecdf(x)
    #(plot(x, 1-f(sort(x)), type="s", lwd=1, log="xy"))
    pdt<-data.frame(cbind(pdt,y=(1-f(x))))
    
    g<-ggplot(pdt, aes(x=value,y=ccdf,color=year)) + geom_point(size=0.75) #+ geom_line(linewidth=0.5) 
    g<-g + xlab(paste0("Katz-Bonacich centrality")) + ylab("CCDF") + theme_bw()
    g<-g + scale_y_continuous(
      trans = "log10", 
      breaks = function(x) {
        brks <- extended_breaks(Q = c(1, 5))(log10(x))
        10^(brks[brks %% 1 == 0])
      },
      labels = math_format(format = log10)
    ) + annotation_logticks(sides="l") 
    g<- g + scale_x_continuous(
      trans = "log10",
      breaks = function(x) {
        brks <- extended_breaks(Q = c(1, 5))(log10(x))
        10^(brks[brks %% 1 == 0])
      },
      labels = math_format(format = log10)
    ) + annotation_logticks(sides="b")
    g<-g + theme(legend.position = c(0.935,0.75), legend.background = element_rect(colour = "black",linewidth = 0.2), legend.title = element_blank())#, legend.text = paste() ) #, legend.key = element_text(as.expression(pdt$year)))
    
    g<-g + facet_wrap(~data_type)
    
    pdf(paste0(plotdir,"CCDF_influence_vector_",alpha,"_",paste0(yrs,collapse="|"),"_truncated_data"[TRUNCATE],".pdf"),width=8,height=4)
    plot(g)
    dev.off()
    
  } # TRUNCATE
} # alpha in 0.5, 0.3, 0.7
rm(g,f,alpha,pdt,x,yrs,TRUNCATE,tp,gammas,sts)

# Write little latex table with test statistics
tab<-plfit_stats[plfit_stats$year %in% seq(2017,2023,2),]
tab<-tab[str_detect(tab$measure,"0\\.5"),!str_detect(colnames(tab),"continuous")]
tab$measure<-ifelse(str_detect(tab$measure,"truncated"),"truncated"," ")
tab$alpha<-as.numeric(tab$alpha)-1
colnames(tab)<-str_replace(colnames(tab),"alpha","gamma")

tab[,4:ncol(tab)]<-apply(tab[,4:ncol(tab)],2,FUN=function(x){return((round(as.numeric(as.character(x)),3)))})
if(1==2){
  tab<-rbind(c(NA,"Value",rep(NA,6)), 
             tab[tab$measure==" " & tab$data_type=="amt",],
             c(NA,"Truncated data",rep(NA,6)),
             tab[tab$measure!=" " & tab$data_type=="amt",],
             c(NA,"Count",rep(NA,6)), 
             tab[tab$measure==" " & tab$data_type=="cnt",],
             c(NA,"Truncated data",rep(NA,6)),
             tab[tab$measure!=" " & tab$data_type=="cnt",])
  tab<-tab[,c(2,4:8)]
}else{
  tab1<-tab[tab$data_type=="amt",]; tab2<-tab[tab$data_type=="cnt",]
  colnames(tab1)<-paste(colnames(tab1),"(Value)")
  colnames(tab2)<-paste(colnames(tab2),"(Count)")
  cls<-which(str_detect(colnames(tab),"gamma|xmin|logLik|KS"))
  tab<-cbind(tab1[,c(1,2,cls)],tab2[,cls])
  tab<-rbind(tab[tab$`measure (Value)`!="truncated",],c(NA,"Data truncated at 10% quantile of transaction value",rep(NA,(ncol(tab)-2))),tab[tab$`measure (Value)`=="truncated",])
  tab<-tab[,2:ncol(tab)]
}

colnames(tab)[1]<-"Year"
write(stargazer(tab,summary=F,rownames = F),file=paste0(plotdir,"Powerlaw_fit_statistics_selection.tex"))
rm(tab)

#load("public_data/concordances/concordances.RData")
nms<-read_delim("public_data/concordances/SICs_from_companiesHouse.csv",delim = ";", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE)
dall$name<-nms$X3[match(dall$code,nms$X2)]
rm(nms)

# Write outliers to table (e.g. top-20 sectors by influence vector for each data set)
tab<-dall[str_detect(dall$measure,"0.5") & dall$year%in%seq(2017,2023,2),]
tab<-tab[order(tab$value,decreasing = T),]
tab$value<-round(tab$value,4)
y<-2017
top<-10
if(exists("taball")){rm(taball)}
for(y in seq(2017,2023,2)){
  tabv<-tab[which(tab$year==y & str_detect(tab$measure,"amt") & !str_detect(tab$measure,"trunc"))[1:top],]
  tabc<-tab[which(tab$year==y & str_detect(tab$measure,"cnt") & !str_detect(tab$measure,"trunc"))[1:top],]
  tabvt<-tab[which(tab$year==y & str_detect(tab$measure,"amt") & str_detect(tab$measure,"trunc"))[1:top],]
  tabct<-tab[which(tab$year==y & str_detect(tab$measure,"cnt") & str_detect(tab$measure,"trunc"))[1:top],]
  tab0<-cbind(tabv,tabvt,tabc,tabct)
  
  tab0<-tab0[,str_detect(colnames(tab0),"value|code|name")]
  tab0<-rbind(c(paste(y),rep(NA,(ncol(tab0)-1))), c("Value","","","Value truncated","","","Count","","","Count truncated","",""),tab0)
  if(!exists("taball")){taball<-tab0}else{taball<-rbind(taball,tab0)}
}
rm(tab0,tabc,tabct,tabv,tabvt)

tabpr<-taball[c(1:(top+2),(((top+2)*3)+1):nrow(taball)),c(1:3,7:9)]

write(stargazer(tabpr,summary=F,rownames = F),file=paste0(plotdir,"Influence_vector_top10_2017_2023.tex"))





