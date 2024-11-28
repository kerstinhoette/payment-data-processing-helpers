library(xtable); library(stargazer); library(stringr); library(xtable); library(ggplot2); library(reshape2); library(zoo)


# Create plot density of input/output share network by truncation threshold in pct

if(AGGR_YEARS==T){stop("Year aggregation not implemented")}

df0<-df

outputdir<-paste(out_dir,"/Density_by_truncation/", sep="")
if(!dir.exists(outputdir)){dir.create(outputdir, recursive = T)}

df1<-edgelist_wide_to_long(df0)
is<-get_input_output_shares(df1, direction = "input")
os<-get_input_output_shares(df1, direction = "output")

# Color settings
clr<-c("red3", "orange", "navyblue", "skyblue3", "purple")
#names(clr)<-c("AMT", "CNT", "IxI", "PxP", "SUT")
names(clr)<-c("Value", "Count", "IxI", "PxP", "SUT")


dnsty<-function(ed,n){return(ifelse(ed>0,((ed)/(n*(n-1))),0))}

 # Set number of nodes (number of industries)
nn<-length(unique(c(df1$to,df$from)))
# Set networks
nws<-c("AMT","CNT","SUT","PxP","IxI")
nws<-c("Value","Count","SUT","PxP","IxI")

colnames(is)<-colnames(os)<-str_replace_all(colnames(is),"X","x")
colnames(is)<-colnames(os)<-str_replace_all(colnames(is),"AMT","Value")
colnames(is)<-colnames(os)<-str_replace_all(colnames(is),"CNT","Count")


if(sum(nws %in% colnames(os)) != length(nws) || sum(nws %in% colnames(is)) != length(nws) ){stop("nws does not match with colnames")}

# Set all NA links to 0
is[,nws]<-apply(is[,nws],2,FUN=function(x){return(ifelse(is.na(x),0,x))})
os[,nws]<-apply(os[,nws],2,FUN=function(x){return(ifelse(is.na(x),0,x))})


y<-years[5];nw<-"Value";tr<-0.00
for(y in years){
  if(exists("tb")){rm(tb)}
  is0<-is[is$year==y,]
  os0<-os[os$year==y,]
  # Different truncation thresholds to go through
  trls<-seq(0,1,by=0.0025)
  for(nw in nws){
    # Initialize vector to store densities
    dsi<-c(); dso<-c()
    tr<-trls[10]
    for(tr in trls){
      # Count number of links above threshold
      nei<-sum(is0[!is.na(is0[,nw]),nw]>tr)
      neo<-sum(os0[!is.na(os0[,nw]),nw]>tr)
      # calculate density and store in a vector
      dsi<-c(dsi, dnsty(nei,nn))
      dso<-c(dso, dnsty(nei,nn))
    }
    # Store in data frame (long format for easy plotting)
    tb1<-data.frame(Threshold=trls, variable=rep(nw,length(dsi)), Density_up=dsi, Density_down=dso)
    if(!exists("tb")){tb<-tb1}else{tb<-rbind(tb,tb1)}
    rm(dso, dsi, tb1, tr)
  } # nw in networks
  
  pdf(paste0(outputdir,"Input_share_network_",y,".pdf"))
  p<-ggplot(tb, aes(y=Density_up, group=variable, color=variable, x=Threshold)) + 
    theme_bw() + geom_line() +# stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +# + 
    #stat_smooth(method = lm, formula = y ~ poly(x, 10), se = FALSE, lwd=1) +
    scale_color_manual(values=clr) + labs(x="Threshold", y="Density", color="") + # title=paste("Upstream (input shares) network in",y),
    theme(legend.position = "none", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
  p<-p + scale_x_continuous(trans = "sqrt", limits = c(0,0.05)) +  #+ scale_x_sqrt(limits=c(0,0.1)) + 
  annotation_logticks(sides="b") #+ xlim(c(0,1))
  #ß + theme(axis.text = element_text(size = 8), axis.title.y  = element_text(size = (12-round(str_length(ylab)/10))))
  plot(p)
  dev.off()
  
  pdf(paste0(outputdir,"Output_share_network_",y,".pdf"))
  p<-ggplot(tb, aes(y=Density_down, group=variable, color=variable, x=Threshold)) + 
    theme_bw() + geom_line() + # stat_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) + #stat_smooth(method = lm, formula = y ~ poly(x, 15), se = FALSE) +
    scale_color_manual(values=clr) + labs(x="Threshold", y="Density", color="") + #title=paste("Downstream (output shares) network in",y),
    theme(legend.position = "none", title = element_text(size=18), axis.text = element_text(size=18), plot.title = element_text(hjust = 0.5))
    p<-p + scale_x_continuous(trans = "sqrt", limits = c(0,0.05)) + # scale_x_sqrt(limits=c(0,0.1)) + 
      annotation_logticks(sides="b") #+ xlim(c(0,1))
  #ß + theme(axis.text = element_text(size = 8), axis.title.y  = element_text(size = (12-round(str_length(ylab)/10))))
  plot(p)
  dev.off()
  
  pdf(paste0(outputdir,"shared_legend.pdf"), height = 0.5, width = 10)
  par(mar=c(0,0,0,0))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(1,3), ylim=c(0,0.5), mar=0)
  legend("top", legend = names(clr), lty=1, lwd=3, cex=1, bty='n',
         col = clr, ncol = 5, horiz = F)
  
  dev.off()
  
} # y in years

#transformations for log: "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".

