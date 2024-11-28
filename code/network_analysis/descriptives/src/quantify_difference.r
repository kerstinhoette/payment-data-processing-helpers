# Difference quantification
# 
# log10 epsilon_ij = abs( log10 (Z^{Value}_{ij}) / sum_{i,j} (Z^{Value}_{ij})   -   Z^{Value}_{ij}) / sum_{i,j} (Z^{Value}_{ij}) )

df0<-df

y<-years[5]

KEEP_ZERO<-T; LOG_DIFF<-F
# Collect data for different years and data sets
if(exists("dists")){rm(dists)}
for(KEEP_ZERO in c(T,F)){
  for(LOG_DIFF in c(T,F)){
    
    #if(LOG_DIFF && KEEP_ZERO){next}
    lbl<-paste0("Fractional"[LOG_DIFF],"Absolute"[!LOG_DIFF],"_difference","_incl_zero"[KEEP_ZERO])
    
    for(y in years){
      
      # Compare Payment_amt to ONS_IxI and to ONS_SUT
      
      damt<-df[,str_detect(colnames(df),paste0("amt_",y))]
      if(KEEP_ZERO){
        damt[is.na(damt)]<-0
      }else{damt[damt==0]<-NA}
      
      o<-"SUT_"
      for(o in c("SUT_","IxI_","PxP_")){
        if(sum(str_detect(colnames(df),paste0(o,y)))==0){next}
        dons<-df[,str_detect(colnames(df),paste0(o,y))]  
        
        # Consider only links that are non-zero in both data sets
        if(KEEP_ZERO){
          dons[is.na(dons)]<-0
          # Keep only those where the link is at least one of the two data sets non-zero
          i<-which((dons + damt)>0)
          zons<-(dons[i]/sum(dons[i]))
          zamt<-(damt[i]/sum(damt[i]))
          if(LOG_DIFF){zons<-zons+1; zamt<-zamt+1}
          
        }else{
          dons[dons==0]<-NA
          i<-which(!is.na(dons) & !is.na(damt))
          zons<-(dons[i]/sum(dons[i]))
          zamt<-(damt[i]/sum(damt[i]))
          
        }
        if(LOG_DIFF){lepsl<-(abs(log10(zamt) - log10(zons)))
        }else{lepsl<-log10(abs((zamt) - (zons)) * 0.5*(sum(dons[i]) + sum(damt[i])))
        }
        
        add<-data.frame(cbind(to=df$to[i], from=df$from[i], 
                              ons=rep(substr(o,1,3),length(i)), year=rep(y,length(i)), 
                              measure=rep(lbl, length(i)),
                              distance=lepsl))
        
        if(!exists("dists")){
          dists<-add
        }else{dists<-rbind(dists, add)}
        
        #g<-ggplot(data.frame(lepsl), aes(lepsl)) + theme_bw() + 
        #  geom_histogram() + ylab("Frequency") + xlab(paste0("Fractional difference")) + 
        #  xlim(0,6) + ylim(0,400)
        #plot(g)  
        
      }  # o in SUT, IxI, PxP
    } # y in years
  } # LOG DIFF in T, F
} # KEEP ZERO in T, F
rm(damt, dons, zons, i, zamt, lepsl, o)
dists$distance<-as.numeric(as.character(dists$distance))


plotdir<-paste0(out_dir,"Difference_quantification/")
if(!dir.exists(plotdir)){dir.create(plotdir)}


clr<-c("red3", "orange", "navyblue", "skyblue3", "purple")
#names(clr)<-c("AMT", "CNT", "IxI", "PxP", "SUT")
names(clr)<-c("Value", "Count", "IxI", "PxP", "SUT")

# Now, plot the data for chosen year and data sets
y<-2019; ons<-c("SUT", "IxI", "PxP")
if(exists("qurtl")){rm(qurtl)}
for(y in years[5]){
  for(KEEP_ZERO in c(F,T)){
    for(LOG_DIFF in c(T,F)){
      
      #if(LOG_DIFF && KEEP_ZERO){next}
      lbl<-paste0("Fractional"[LOG_DIFF],"Absolute"[!LOG_DIFF],"_difference","_incl_zero"[KEEP_ZERO])
      
      dpl<-dists[(dists$year==y & dists$ons %in% ons & dists$measure==lbl),]
      if(nrow(dpl)==0){stop("no data for this combi, check!")}
      
      
      i<-which(is.infinite(log10(dpl$distance)))
      if(length(i)>0){
        dpl<-dpl[-i,]
        print(paste(length(i),"values removed because Inf if log 10 taken."))
      }
      rm(i)
      
      xl<-paste0("Log10 of the ","fractional"[LOG_DIFF],"scaled percentage"[!LOG_DIFF]," difference"," (one-sided zero-links included)"[KEEP_ZERO])
      
      pdf(paste0(plotdir,paste0(lbl,"_hist_",y,"_",paste(ons,collapse="_"),".pdf")), height = 4, width = 10)
      
      g<-ggplot(data=dpl, aes(x=distance, fill=ons)) + 
        geom_histogram(alpha=1, position = "identity", bins=30) + theme_bw() + 
        scale_fill_manual(values=clr) + 
        ylab("Frequency") + xlab(xl) + 
        facet_wrap(~ons) + 
        theme(
          legend.position="none",
          panel.spacing = unit(0.1, "lines"),
          strip.text.x = element_text(size = 8)
        ) 
      #g<-g + scale_x_continuous()
        #) + annotation_logticks(sides="l") 
      #g<-g + scale_x_continuous(
      #  trans = "log10",
      #  breaks = function(x) {
      #    brks <- extended_breaks(Q = c(1, 5))(log10(x))
      #    10^(brks[brks %% 1 == 0])
      #  },
      #  labels = math_format(format = log10)
      #) + annotation_logticks(sides="l") 
      plot(g)
      
      dev.off()
      
      for(o in ons){
        # Get also tables with quartiles
        if(!exists("qurtl")){
          qurtl<-data.frame(quantile(10**(dpl$distance[dpl$ons==o]), digits = 4))
        }else{qurtl<-cbind(qurtl, data.frame(quantile(10**(dpl$distance[dpl$ons==o]), digits = 4)))}
        colnames(qurtl)[ncol(qurtl)]<-paste(o,y,lbl)
        
      }
      
    } # LOG DIFF
  } # KEEP ZERO  

} # y in years


x<-t(qurtl)
x<-x[order(rownames(x)),]

write(stargazer(x), file= paste0(plotdir,"Table_quartiles.tex") )


rm(x, qurtl, o, ons, lbl, y, dpl, g, dists, clr, xl, add)
rm(df0)


