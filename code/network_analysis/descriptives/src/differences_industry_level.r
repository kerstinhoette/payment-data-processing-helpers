# Plot deviations in industry size with x axis ONS data and y axis Payments

dt0<-dt

plotdir<-paste0(out_dir, "Scatterplots_industry_size/")
if(!dir.exists(plotdir)){dir.create(plotdir)}

ons<-c("IxI", "PxP", "SUT")
y<-"2019"; o<-ons[1]
for(y in years[5]){
  
  d<-dt[dt$time==y, (str_detect(colnames(dt), "sum_|code|name") & colnames(dt)!="code_year" & !str_detect(colnames(dt), "cnt"))]
  
  
  dpl<-melt(d, id.vars = c("code", "name"))
  
  amt<-dpl[str_detect(dpl$variable,"amt"),]
  dpl<-dpl[!str_detect(dpl$variable,"amt"),]
  
  dpl$ons<-str_split(dpl$variable,"_",simplify = T)[,3]
  
  dpl$variable<-str_split(dpl$variable,"_",simplify = T)[,2]
  amt$variable<-str_split(amt$variable,"_",simplify = T)[,2]
  
  dpl$Payments<-amt$value[match(paste0(dpl$code, dpl$variable),paste0(amt$code, amt$variable))]
  
  dpl<-dpl[order(dpl$variable),]
  dpl$variable<-as.factor(paste(str_to_title(dpl$variable),"-",dpl$ons))
  
  mx<-max(c(dpl$value,dpl$Payments),na.rm = T)
  
  g<-ggplot(data = dpl, aes(x=value,y=Payments)) + geom_point()
  
  g<-g + ylim(c(0,log10(mx))) + xlim(c(0,log10(mx)))
  
  g<-g + facet_wrap(~ variable) + theme_bw() + xlab("") + geom_abline(intercept = 0, slope = 1, linewidth = 1, color = "red")
  
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
  
  pdf(paste0(plotdir,"Scatterplot_inputs_outputs_",y,".pdf"), width=9, height = 6)
  plot(g)
  dev.off()
  
  
} # y in years
