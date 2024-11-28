# Create data workfile
dt0<-dt

# Set list of variables to be analyzed
vars<-colnames(dt)[!(colnames(dt) %in% c("code_year","year","code","name","aggr_code","long_name", "aggr_name"))]

if(AGGR_YEARS){
  if(min(years)<2015){
    i<-seq(1, length(years[years>2014]), (length(years[years>2014])/steps))
    yr_titles<-c("pre-2015",paste0(years[years>2014][i],"-",years[years>2014][(i+((length(years[years>2014])/steps)-1))]))
    yrs<-c(2014,years[years>2014][(i+((length(years[years>2014])/steps)-1))])
    rm(i)
  }else{
    i<-seq(1, length(years), (length(years)/steps))
    yr_titles<-c(paste0(years[i],"-",years[(i+((length(years)/steps)-1))]))
    yrs<-years[(i+((length(years)/steps)-1))]
    rm(i)
  }
  
  # Take avg of data across time window
  dt$year<-as.numeric(dt$year)
  #dt$year<-ifelse(dt$year %in% yrs, dt$year, yrs[yrs > dt$year & yrs < (dt$year + (length(years)/steps))])
  dt$year<-ifelse(dt$year %in% yrs, dt$year, NA)
  dt<-dt %>% fill(year, .direction="up")
  dt$code_year<-paste(dt$code, dt$year, sep = "_")
  
  dt1 <- aggregate(. ~ code_year, dt[,(colnames(dt) %in% c("code_year", vars))], mean, na.rm=T, na.action = NULL)
  dt<-merge(unique(dt[,!(colnames(dt) %in% vars)]), dt1, by="code_year", all.x = T, all.y = T)
  rm(dt1)
}else{yrs<-years; yr_titles<-paste(yrs)}


plotdir<-paste(out_dir, "/Distribution_plots/",sep="")
if(!dir.exists(plotdir)){dir.create(plotdir,recursive=T)}
fn_distr<-paste(plotdir,"Distributions_",paste0(reg,"_")[reg!=""], aggr, "_",steps, ".pdf")

#source("code/network_analysis/descriptives/aux/plot_distributions.r")
#rm(fn_distr,plotdir)

#topranks<-10
#dirtop<-paste(out_dir, "/Industry_ranking_top_",topranks,"/",sep="")
#if(!dir.exists(paste(dirtop))){dir.create(dirtop,recursive=T)}
#vars_top<-vars
# Write top-sectors to table and print in txt/ tex
#source("code/network_analysis/descriptives/aux/write_top10_to_table.r")
#rm(fn_top, vars_top, tr, top, topranks, vn, vars, n0, nt, lbl, fagn, full_name, DTYPE, d_type, y)
#rm(d)


# Reset data to original data
dt<-dt0
df<-df0
rm(dt0, df0)
