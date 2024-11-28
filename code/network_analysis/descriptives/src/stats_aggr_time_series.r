outputdir<-paste(out_dir, "Time_seires_plots/", sep="")
if(!file.exists(outputdir)){dir.create(outputdir, recursive = T)}

# Load raw data that is not aggr. by year
load(paste0("Payment_data/Mar_data/national_account_data/",reg,"_"[reg=="regional"],"industry_panel_network_statistics_",aggr,".RData"))


plotdir<-outputdir
vars<-c("inputs", "outputs")

if(!is.na(as.numeric(dt$code[1]))){dt<-dt[order(as.numeric(as.character(dt$code))),]
}else{dt<-dt[order((as.character(dt$code))),]}

dt0<-dt[,c("code_year", "year", "code", "name", "aggr_code", "aggr_name", "long_name", colnames(dt)[str_detect(colnames(dt),paste0(vars,collapse = "|"))])]

dt0[,str_detect(colnames(dt0),"_cnt")]<-dt0[,str_detect(colnames(dt0),"_cnt")]/100

for(cl in c("amt","cnt","PxP","SUT")){
  if(sum(str_detect(colnames(dt0),cl))!=2){if(sum(str_detect(colnames(dt0),cl))>2){stop("check var selection")}else{next}}
  dt0$input_output_ratio_<-dt0[,paste0("inputs_",cl)]/dt0[,paste0("outputs_",cl)]
  colnames(dt0)[ncol(dt0)]<-paste0(colnames(dt0)[ncol(dt0)],cl)
  dt0$input_output_sum_<-dt0[,paste0("inputs_",cl)]+dt0[,paste0("outputs_",cl)]
  colnames(dt0)[ncol(dt0)]<-paste0(colnames(dt0)[ncol(dt0)],cl)
  
}
vars<-c(vars, "input_output_ratio", "input_output_sum")

clr<-c("navyblue", "royalblue2", "red3", "orange")
names(clr)<-c("PXP", "SUT", "AMT", "CNT")

ALL_TIMESERIES_IN_ONE<-T

secs<-unique(dt0$name)
n<-secs[1]; v<-vars[1]

SCALE<-F; INDEX<-F; LOG<-F
trnsf<-c("", "_scaled", "_index2015=100", "_log", "_same_axis", "_log_same_axis", "_relative_Payment", "_relative_Payment_index2015=100", "_relative_Payment_same_axis")
for(trnsf in c("", "_scaled", "_index2015=100", "_log", "_same_axis", "_log_same_axis", "_relative_Payment", "_relative_Payment_index2015=100", "_relative_Payment_same_axis")){
  SCALE<-F; INDEX<-F; LOG<-F
  if(trnsf == "_scaled"){SCALE<-T
  }else if(str_detect(trnsf, "_index2015=100")){INDEX<-T
  }else if(trnsf %in% c("_log", "_log_same_axis")){LOG<-T}
  
  for(v in vars){
    
    #if(v == "input_output_ratio" && trnsf != ""){next}
    
    dt1<-dt0[,c("year","name",colnames(dt0)[str_detect(colnames(dt0),v)])]
    
    if(str_detect(trnsf, "_relative_Payment")){
      dt1[,str_detect(colnames(dt1), "_SUT")]<-dt1[,str_detect(colnames(dt1), "_amt")]/dt1[,str_detect(colnames(dt1), "_SUT")]
      dt1[,str_detect(colnames(dt1), "_PxP")]<-dt1[,str_detect(colnames(dt1), "_amt")]/dt1[,str_detect(colnames(dt1), "_PxP")]
      dt1<-dt1[,c("year","name",colnames(dt1)[str_detect(colnames(dt1),"SUT|PxP")])]
      dt1<-dt1[dt1$year %in% 2015:2020,]
    }
    
    if(str_detect(trnsf, "_same_axis")){ # Get limit for y-axis for plotting
      y<-as.numeric(as.character(unlist(dt1[,3:ncol(dt1)])))
      if(str_detect(trnsf, "_log")){y<-log(y[y!=0])}
      ymx<-max(y,na.rm = T)
      if(sum(na.omit(y) > (mean(y, na.rm=T)*10))<(0.05*length(na.omit(y)))){ymx<-max(y[y<(mean(y, na.rm=T)*50)],na.rm = T)}
      rm(y)
    }
    
    if(ALL_TIMESERIES_IN_ONE){
      fn<-paste0(plotdir,"Timeseries_",v,"_all_industries",trnsf,".pdf"); plts<-list()
      
      if(length(secs)>78){nc<-8; nr<-ceiling(length(secs)/8)
      }else if(length(secs)>32){nc<-6; nr<-ceiling(length(secs)/6)
      #par(mfrow=c(ceiling(length(secs)/6),6))
      }else if(length(secs)>3){nc<-4; nr<-ceiling(length(secs)/4)
      #par(mfrow=c(ceiling(length(secs)/4),4))
      }else{nc<-lengths(secs); nr=1}
      #mfrow=c(1,length(secs))}
      
    }
    for(n in unique(dt0$name)){
      if(!ALL_TIMESERIES_IN_ONE){pdf(paste0(plotdir,"Timeseries_",v,"_",n,trnsf,".pdf"))    }
      
      
      dt2<-dt1[dt1$name==n,]
      # Drop year 2022 because it's only until July
      dt2<-dt2[dt2$year!=2022,]
      rng<-unlist(dt2[,!(colnames(dt2)%in%c("year","name"))])
      rng<-rng[!is.infinite(rng)]
      rng<-range(rng, na.rm = T)
      subttl<-paste("Range:",paste(round(rng, digit=(0+3*(max(rng)<50))), collapse = " to "))
      
      if(LOG){
        dt2[,!(colnames(dt2)%in%c("year","name"))]<-apply(dt2[,!(colnames(dt2)%in%c("year","name"))],2,FUN=function(x){return(log(x))})
      }
      if(SCALE){
        dt2[,!(colnames(dt2)%in%c("year","name"))]<-apply(dt2[,!(colnames(dt2)%in%c("year","name"))],2,FUN=function(x){return(scale(x))})
      }
      if(INDEX){
        for(cl in which(!(colnames(dt2)%in%c("year","name")))){dt2[,cl]<-100*dt2[,cl]/dt2[dt2$year==2015,cl]}; rm(cl)
      }
      
      dt3<-melt(dt2, id.vars = c("year", "name"))
      dt3<-na.omit(dt3)
      dt3$variable<-str_to_upper(str_remove_all(dt3$variable,paste0(v,"_")))
      
      ylab<-paste(str_to_title(str_replace_all(v,"_"," ")), "(Index 2015 = 100)"[INDEX && !str_detect(trnsf, "relative_Payment")], paste("(AMT as share of ONS)")[str_detect(trnsf, "relative_Payment")])
      
      p<-ggplot(dt3, aes(y=value,x=year,group=variable,color=variable)) + geom_point() + geom_line() + theme_bw() +#ylab() + xlab("Year") + 
        scale_colour_manual(values=clr) +
        labs(title=paste(n), x="Year", y=ylab, color="") + theme(axis.text = element_text(size = 8), axis.title.y  = element_text(size = (12-round(str_length(ylab)/10))))
      
      if(str_detect(trnsf, "index|scaled")){
        p<-p+labs(subtitle = subttl)
      }
        
      
      
      if(str_detect(trnsf,"_same_axis")){p<-p+ylim(0,ymx)}
      
      
      if(!ALL_TIMESERIES_IN_ONE){plot(p); dev.off()
      }else{pnl<-p + theme(legend.position = "none")
      plts<-list.append(plts, pnl)
      rm(pnl)
      }
    } # n in dt0$name
    if(ALL_TIMESERIES_IN_ONE){
      p <- p + theme(legend.position = "top", legend.direction = "horizontal")
      lg<-get_legend(p)
      df_plot_arranged <- grid.arrange(arrangeGrob(nullGrob(), lg, nullGrob(), nrow = 1), 
                                       arrangeGrob(grobs=plts, nrow=nr,ncol=nc),
                                       ncol = 1, heights = c(1,3*nr)#, top = textGrob("Title Text Holder Here", gp = gpar(fotsize = 12, font = 2))
      )
      
      pdf(fn, width = (4*nc), height = 3*nr)
      plot(df_plot_arranged)
      #plot(grid.arrange(lg, arrangeGrob(grobs=plts, ncol = nc, nrow=nr), nrow = 2, heights = c(1, 3*nr)))
      #p2<-grid_arrange_shared_legend(grobs=plts, nrow=nr,ncol=nc)
      #ggsave(fn, marrangeGrob(plts,lg, nrow=nr,ncol=nc), width = (4*nc), height = 3*nr)
      #ggsave(fn, marrangeGrob(plts,lg, nrow=nr,ncol=nc), width = (4*nc), height = 3*nr)
      #ggsave(fn, marrangeGrob(plts,nrow=nr,ncol=nc, common.legend = TRUE, legend="bottom"), width = (4*nc), height = 3*nr)
      dev.off()
      rm(plts, df_plot_arranged, nc, nr, fn)
    } # if ALL_TIMESERIES_IN_ONE  
    rm(p)
  } # v in vars
  
  
} # trnsf in data transformations
rm(v,trnsf,ymx,vars,secs,n,nms,lg,dt3,dt2,dt1,dt0,INDEX,LOG,SCALE,clr,rng,subttl)








