# Helper functions


# Transform edgelist in wide format into long format
# Inputs: dwide: edgelist in wide format; dsources (yrs) indicates which columns by data source (by year) to include
edgelist_wide_to_long<-function(dwide, dsources=c("AMT","CNT","SUT","PXP","IXI"), yrs=c(1997:2022)){
  require(reshape2)
  ids<-c("to", "from", "to_names", "from_names")[c("to", "from", "to_names", "from_names") %in% colnames(dwide)]
  if(length(ids)<2){stop("from & to seem to be missing in colnames")}
  yrs<-str_remove_all(colnames(dwide), "^[:digit:]")
  dlong<-melt(dwide, id.vars = ids)
  dlong$year<-as.numeric(gsub("\\D", "", dlong$variable))
  dlong$variable<-str_remove_all(dlong$variable, "[:digit:]")
  dlong1<-dlong[str_detect(str_to_upper(dlong$variable), dsources[1]),colnames(dlong)!="variable"]
  colnames(dlong1)[colnames(dlong1)=="value"]<-dsources[1]
  for(ds in dsources[2:length(dsources)]){
    dlong1<-merge(dlong1, dlong[str_detect(str_to_upper(dlong$variable),ds),colnames(dlong)!="variable"], by=c(c(ids, "year")), all = T)
    colnames(dlong1)[colnames(dlong1)=="value"]<-ds
  }
  dlong<-dlong1
  return(dlong)
}



# Transform long edgelist of transaction values into shares
# Inputs: df: long edgelist of transactions must have the columns from, to, direction %in% c("input", "output") indicating input or output shares
#direction<-"input"; divisor<-NULL; dsources<-c("AMT","CNT","SUT","PXP","IXI"); freq<-"year"
get_input_output_shares<-function(df, direction="input", divisor=NULL, dsources=c("AMT","CNT","SUT","PXP","IXI")){
  df0<-df
  # Preserve original colnumn names
  #cn<-unique(str_remove_all(colnames(df)[!colnames(df)%in%c("to","from","to_names","from_names","year")], paste0(1998:2022,"_",collapse="|")))
  dsources<-colnames(df)[str_detect(str_to_upper(colnames(df)),paste0(str_to_upper(dsources),collapse = "|"))]
  if(sum(c("from","to")%in%colnames(df))!=2){stop("Function requires 2 columns labelled to, from to identify source and destination.")}
  # Add year to industry labels to make sure it divides by the correct value
  if(is.element("year", colnames(df))){df$from<-paste0(df$year,"_",df$from); df$to<-paste0(df$year,"_",df$to)
  }else if(sum(c("quarter", "month") %in% colnames(df))){stop("You likely want to adjust your code to make it compatible for monthly/quarterly data.")}else{stop("check this")}
  
  df$id<-df[,c("from"[direction=="output"],"to"[direction=="input"])]
  # Get vector for division (can be either provided or inferred as sum of inputs/outputs from network data)
  if(is.null(divisor)){
    div<-aggregate(. ~ id, df[,c("id",dsources)], sum, na.rm=T, na.action = NULL)
  }else{
    # Check if divisor data provided in right format (i.e. must have same column names as network data)
    if(!is.element("id",colnames(divisor))){divisor$id<-paste0(divisor[,freq],"_",divisor$code)}
    if(sum(dsources %in% colnames(divisor))<length(dsources)){stop("Divisor data either incomplete or wrong labelled.")}
    div<-divisor[,c("id",dsources)]
  }
  # Expand div data to enable convenient division
  div<-merge(df[,c("id","from","to")],div,by="id",all=T)
  div<-div[order(div$id),]
  df<-df[order(df$id),]
  # Element-wise division of df by div
  for(s in dsources){
    df[,s]<-df[,s]/div[,s]
    # Set NaN and infite to NA
    df[is.nan(as.matrix(df[,s])),s]<-NA
    df[is.infinite(as.matrix(df[,s])),s]<-NA
  }
  # Get back to original format of input data
  o<-match(paste0(df0$year,"_",df0$from,"-",df0$year,"_",df0$to),paste0(df$from,"-",df$to))
  df0[,dsources]<-df[o,dsources]
  return(df0)
  
}


# Compile network spillover effects at industry level
# Inputs: net: network data provided as an edgelist; idt: industry level data to be used to calculate spillovers (e.g. industry size or employment), as a default, sum of inputs or outputs will be used based on sum of transactions (if network data not provided as shares); type: choose whether spillovers should arise from growth rates or levels; dsources: possible source data sets; direction %in% c("upstream","downstream"): indicates whether up- or downstream spillovers (from input or output links). For downstream spillovers, the default source of spills is downstream industries' inputs, for upstream spillovers, upstream industries' output. 
#net<-df_os; dsources<-c("AMT","CNT","SUT","PXP","IXI"); type<-"growth"; idt<-dt; direction<-"downstream"; lag<-NULL
get_network_spillovers<-function(net, idt=NULL, type="growth", dsources=c("AMT","CNT","SUT","PXP","IXI"), direction="upstream", lag=NULL){
  dsources<-dsources[dsources %in% colnames(net)]
  if(is.null(idt)){
    if(max(as.matrix(net[,dsources]),na.rm = T)<100){warning("It seems you've used an input or output share matrix as network input. If you do so, you should additionally provide industry level data from which the spillovers should arise, e.g. industry size or so. This cannot be inferred from the share matrix.")}
    net1<-data.frame(cbind(id=paste0(net$year,"_",net$from["upstream"],net$to["downstream"]),net[,dsources]))
    idt<-aggregate(. ~ id, net1[,c("id",dsources)], sum, na.action = NULL, na.rm=T)
  }else if(sum(str_detect(colnames(idt),"sum_input"))>0 && sum(str_detect(colnames(idt),"sum_output"))>0){
    if(direction=="upstream"){idt<-idt[,c("code","year",colnames(idt)[str_detect(colnames(idt),"sum_outputs")])]
    }else if(direction=="downstream"){idt<-idt[,c("code","year",colnames(idt)[str_detect(colnames(idt),"sum_inputs")])]}
    colnames(idt)[3:ncol(idt)]<-str_to_upper(str_remove_all(colnames(idt)[3:ncol(idt)],"sum_inputs_|sum_outputs_"))
  }else if(sum(colnames(idt)!=c("code","year",dsources))>0){
    stop("Implement code to make colnames of idt aligned with next steps.")
  }
  if(!is.null(lag)){stop("Implement a routine for lags. Normally, you should be able to get these lagged spills from you final data set; hence check if you really need it and think about the assumptions about the network structure you make.")}
  
  # Transform idt data into growth rates if asked for
  if(type=="growth"){
    if(is.element("year", colnames(idt))){dif<-1}else{stop("Update code for growth rates for non-yearly data, e.g. dif<-12 for monthly; check if also n needs to be set to 12 in this case.")}
    g<-pdata.frame(idt,index=c("code","year"))
    g<-G(g, n=1, diff=dif, logdiff = F,stubs = F)
    idt[,dsources]<-g[,dsources]
    rm(g)
  }
  
  yrs<-sort(intersect(unique(idt$year),unique(net$year)))
  y<-yrs[22]
  # Make sure that you merge idt by input link if upstream spillovers and output link if downstream spills.
  if(direction=="upstream"){net$code<-net$from;net$spill_dest<-net$to
  }else if(direction=="downstream"){net$code<-net$to;net$spill_dest<-net$from}
  # Calculate spillovers for each year
  spill<-data.frame()
  for(y in unique(idt$year)){
    ny<-net[net$year==y,c("code","spill_dest",dsources)]
    iy<-idt[idt$year==y,c("code",dsources)]
    # Now, expand dimensionality of iy data by merge with net$code. 
    iy<-iy[match(ny$code, iy$code),]
    sp<-data.frame(cbind(ny[,c("code","spill_dest")],(ny[,dsources]*iy[,dsources])))
    sp<-aggregate(. ~ spill_dest, as.data.frame(sp[,c("spill_dest",dsources)]),sum,na.rm=T,na.action = NULL)
    # Set zero values to NA (otherwise, it might be misleading in subsequent analyses. Most zero-values arise from missing observations for the first year when calculating growth rates, na.rm causes zero if all values are NA)
    sp[sp==0]<-NA
    sp$year<-y
    spill<-rbind(spill, sp)
  }
  
  spill<-spill[,c("spill_dest","year",dsources)]
  colnames(spill)<-c("code","year",paste0(dsources,"_spill_",direction))
  
  return(spill)
}



# Shorten sector names for convenience
# Inputs: Takes a string vector of sector names (code written for CPA 80 compatible with 2-digit SIC)
shorten_sector_names<-function(n){
  n0<-n
  n0<-str_remove_all(n0, " and related services| activities| related services|  and motorcycles| and equipment| and pharmaceutical preparations| and chemical products| and cork|Manufacture of |Products of | and related products| product| services")
  n0[str_detect(n0,"Fish and other fishing")]<-"Fish, aquaculture"
  n0[str_detect(n0,"foods")]<-"Food manufct."
  n0[str_detect(n0,"Extraction Of Crude Petroleum")]<-"Gas, oil, ores extraction"
  n0[str_detect(n0,"Other mining and quarrying")]<-"Mining, quarrying"
  n0<-str_replace_all(n0, "support services", "support")
  n0[str_detect(n0,"Wood and ")]<-"Wood, plaiting products"
  n0<-str_replace_all(n0, "except", "exc.")
  n0[str_detect(n0,"Motor vehicles, trailers and ")]<-"Motor vehicles, trailers"
  n0<-str_replace_all(n0, "equipment", "equ.")
  n0[str_detect(n0,"Repair and installation of machinery")]<-"Machinery Repair, installation"
  n0[str_detect(n0,"Electricity, gas, steam")]<-"Electricity, gas, AC"
  n0[str_detect(n0,"water treatment and supply")]<-"water treatm., supply"
  n0[str_detect(n0,"Waste collection, treatment and disposal")]<-"Waste treatm., material recovery"
  n0[str_detect(n0,"Remediation and other waste")]<-"Other waste managem."
  n0[str_detect(n0,"Wholesale and retail trade and repair of motor vehicle")]<-"Motor vehicles trade"
  n0[str_detect(n0,"Wholesale trade, exc")]<-"Wholesale exc. motor vhcl."
  n0[str_detect(n0,"Retail trade, exc")]<-"Retail exc. motor vhcl."
  n0[str_detect(n0,"Land transport and transport via pipelines")]<-"Land transport, pipelines"
  n0[str_detect(n0,"Warehousing and support for transportation")]<-"Wwarehousing"
  n0[str_detect(n0,"Motion Picture, Video")]<-"Video, TV, Music, Broadcast"
  n0[str_detect(n0,"Financial, exc")]<-"Finance, exc. insurance"
  n0[str_detect(n0,"Insurance, reinsurance and pension")]<-"Insurance, pensions"
  n0[str_detect(n0,"Services auxiliary")]<-"Aux. financial serv."
  n0[str_detect(n0,"Services of head offices")]<-"Head offices, managem."
  n0[str_detect(n0,"Architectural and engineering")]<-"Architects, techn. analysis"
  n0[str_detect(n0,"Scientific research")]<-"Scientific R&D"
  n0[str_detect(n0,"Other professional, scientific and technica")]<-"Scient., tech. services"
  n0[str_detect(n0,"Veterinary")]<-"Veterinary services"
  n0[str_detect(n0,"Travel agency")]<-"Travel services"
  n0[str_detect(n0,"Office administrative")]<-"Office admin., support"
  n0[str_detect(n0,"Public administration and defence")]<-"Public admin., defence, SSC"
  n0[str_detect(n0,"Sports and amusement and recreation")]<-"Sports, amusem., recreation"
  n0[str_detect(n0,"Libraries, archives")]<-"Libraries, museums"
  n0[str_detect(n0,"Social Work Activities")]<-"Care, social work"
  n0[str_detect(n0,"Creative, arts and entertainment")]<-"Arts, entertainment"
  n0[str_detect(n0,"Services furnished by membership organisations")]<-"Serv. by membership organis."
  n0[str_detect(n0,"Repair of computers and personal and household goods")]<-"Computer, househ. goods repair"
  n0[str_detect(n0,"Other personal")]<-"Other personal serv."
  n0[str_detect(n0,"Services to buildings and landscape")]<-"Building, landscape serv."
  n0[str_detect(n0,"Computer programming, consultancy")]<-"Computer progr., consulting"
  n0[str_detect(n0,"Services of households as employers of domestic personnel")]<-"Household personnel"
  n0<-str_to_title(n0)
  n0<-str_replace_all(n0,";",",")
  n0<-str_replace_all(n0," And ",", ")
  n0<-str_replace_all(n0,"  "," ")
  n0<-str_replace_all(n0,", ","/")
  
  return(n0)
}


# Shorten variable names from ONS tables & optionally, write legend between short and long name to latex file
# Inputs: n, string vector with long names; optionally: fn, file name for legend 
# Output: n0, string vector with shortened names 
shorten_ONS_variable_names<-function(n, legend=F, fn_legend=NULL){
  
  if(legend && is.null(fn_legend)){warning("No legend was written to latex because no file name provided.")}
  n1<-n
  n<-str_replace_all(n, "Compensation of employees", "Labour income")
  n<-str_replace_all(n, "Gross operating surplus and mixed income", "GOS")
  n<-str_replace_all(n, "Total output at basic prices", "Total output")
  n<-str_replace_all(n, "Gross valued added at basic prices", "GVA")
  n<-str_replace_all(n, "Gross value added", "GVA")
  n<-str_replace_all(n, "Total exports of goods and services", "Exports")
  n<-str_replace_all(n, "Total final demand", "Final demand")
  n<-str_replace_all(n, "Total demand for products", "Product demand")
  n<-str_replace_all(n, "Total supply of products at purchasers prices", "Supply")
  n<-str_replace_all(n, "Final consumption expenditure", "Consumption")
  n<-str_replace_all(n, "Gross fixed capital formation", "Investment")
  
  n1<-data.frame(cbind(Original_name=n1, Short=n))
  
  if(legend){
    require(stargazer)
    colnames(n1)<-str_replace_all(colnames(n1), "_", " ")
    n1<-n1[n1$`Original name` != n1$Short,]
    lt<-stargazer(n1, summary = F, rownames = F, label="tab:legend_variable_names", title="Legend of variable names")
    if(is.null(fn_legend)){
      warning("No legend was written to latex because no file name provided.")
      print(lt)
    }else(write(lt, file=fn_legend))
    
  }
  
  
  return(n)
}


# Write correlations to a file
# Input: x: data frame with two numeric columns plus an optional year column; fn: file name; y=NULL: optional indicating the year
write_corr_to_file<-function(x, fn, y=NULL){
  if(!is.null(y) & is.element("year", colnames(x))){x<-x[x$year%in%y,(colnames(x)!="year")]}
  
  if(ncol(x)!=2){return(print("Invalid input: data.frame with two columns needed"))}
  x[is.infinite(x[,1]),1]<-NA; x[is.infinite(x[,2]),2]<-NA
  x[is.nan(x[,1]),1]<-NA; x[is.nan(x[,2]),2]<-NA
  x<-x[(rowSums(x,na.rm = T)>0),]
  x<-na.omit(x)

  write(paste("\n\nPairwise correlation of",paste(colnames(x),collapse = " and "),paste("in",paste(y,collapse="-"))[!is.null(y)]),file = fn,append = T)
  write(paste("Pearson correlation:",cor(x[,1],x[,2],use = "pairwise.complete.obs")),file = fn,append=T)
  write(paste("Kendall rank correlation:",cor(x[,1],x[,2],method = "kendall",use = "pairwise.complete.obs")),file = fn,append=T)
  if(min(x)<0){print("No log correlation calculated because of negative valued entries.")
  }else if(min(abs(x),na.rm = T)<1){write(paste("Pearson correlation of log(1+x):",cor(log(1+x[,1]),log(1+x[,2]),use = "pairwise.complete.obs")),file = fn,append=T)
  }else{write(paste("Pearson correlation of log(x):",cor(log(x[,1]),log(x[,2]),use = "pairwise.complete.obs")),file = fn,append=T)}
}







