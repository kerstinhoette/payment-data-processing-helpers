
MAKE_PLOT<-F
df0<-df

outputdir<-paste(out_dir,"/Tables_network_properties/", sep="")
if(!dir.exists(outputdir)){dir.create(outputdir, recursive = T)}

load("public_data/concordances/concordances.RData")
# Create labels: 
ll<-na.omit(unique(data.frame(cbind(code=conc[,aggr]), name=shorten_sector_names(conc[,paste0(aggr,"_names")]),
                              long_name=conc[,paste0(aggr,"_names")], aggr_code=conc$A16, 
                              aggr_name=shorten_sector_names(conc$A16_names), aggr_long_name=conc$A16_names)))
ll<-ll[order(ll$code),]
# Set graphical parameters for plotting
ll$color<-rainbow(length(unique(ll$aggr_code)),start=(2/6))[match(ll$aggr_code, unique(ll$aggr_code))]
rm(conc)

DTYPE<-MATRICES[1] # TBD: Expand code to input/output share matrix
# Run statistics for the follwing types of networks: 
ntwrks<-c("amt", "cnt", "pxp", "sut", "ixi")
# Set different network truncation methods

# Get sum of from/to flows at industry level (used to calculate shares matrices)
#outputs <- aggregate(. ~ to, df0[which(!(colnames(df0) %in% c("from", "from_names", "to_names")))], sum)
#inputs <- aggregate(. ~ from, df0[which(!(colnames(df0) %in% c("to", "from_names", "to_names")))], sum)
#outputs<-merge(df0["to"],outputs,all.x = T,all.y = T)
#inputs<-merge(df["from"],inputs,all.x = T,all.y = T)


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
}else{yrs<-years; yr_titles<-paste(yrs)}

ALL_TABLES_IN_ONE<-F
if(ALL_TABLES_IN_ONE){
  fn_net_all<-paste0(outputdir,"Network_statistics_",steps,".tex")
  write("", fn_net_all, append=F)
}
for(DTYPE in MATRICES){
  
  if(ALL_TABLES_IN_ONE == "ALL_BY_DTYPE"){
    fn_net_all<-paste0(outputdir,"Network_statistics_",DTYPE,"_",steps,".tex")
    write("", fn_net_all, append=F)
  }

  
  df1<-edgelist_wide_to_long(df0)
  if(sum(str_detect(TRUNCATION_METHODS,"threshold_pct_"))>0){
    is<-get_input_output_shares(df1, direction = "input")
    os<-get_input_output_shares(df1, direction = "output")
  }
  
  if(DTYPE=="input_share_matrix"){
    df1<-get_input_output_shares(df1, direction = "input")
  }else if(DTYPE=="output_share_matrix"){df1<-get_input_output_shares(df1, direction = "output")}
  
  #cn<-colnames(df1)
  #cl<-which(!(cn%in%c(c("to", "from", "to_names", "from_names"))))
  #if(DTYPE == "input_share_matrix"){
  #  df1[,cl]<-df1[,cl]/outputs[,match(colnames(df1)[cl],colnames(outputs))]
  #}else if(DTYPE == "output_share_matrix"){df1[,cl]<-df1[,cl]/inputs[,2:ncol(inputs)]
  #}else{df1<-df0}
  #df1[,cl]<-apply(df1[,cl],2,FUN=function(x){return(ifelse((is.nan(x)|is.na(x)|is.infinite(x)),0,x))})
  
  d_type<-"upstream"
  for(d_type in c("upstream", "downstream")){
    
    cn<-colnames(df1)
    if(d_type=="upstream"){
      df<-df1
    }else if(d_type=="downstream"){ # swap from and to columns to get the transposed network
      df<-df1[,c(c("year", "to", "from", "to_names", "from_names"),cn[!(cn %in% c("to", "from", "to_names", "from_names","year"))])]
      colnames(df)[1:5]<-c("year", "from", "to", "from_names", "to_names")
    }else{df<-df1}
    
    
    
    
    top10dir<-paste0(outputdir, "/Top10_hubs_and_authorities/")
    if(!dir.exists(top10dir)){dir.create(top10dir, recursive=T)}
    fn_a<-paste0(top10dir, "Top10_authorities_",DTYPE,"_",d_type,"_",steps,".txt")
    fn_h<-paste0(top10dir,"Top10_hubs_",DTYPE,"_",d_type,"_",steps,".txt")
    
    
    y<-years[1]
    app<-F
    write(paste(""), file=fn_a, append=app)
    write(paste(""), file=fn_h, append=app)
    y1<-1; tr<-2
    tr<-TRUNCATION_METHODS[1]
    if(exists("gr0")){rm(gr0)}
    for(tr in TRUNCATION_METHODS){
      
      for(y1 in 1:length(yrs)){
        y<-yrs[y1]
        
        if(AGGR_YEARS){
          if(y1 == 1){ys<-years[which(years <= y)]}else{ys<-years[which(years <= y & years > yrs[(y1-1)])]}
        }else{ys <- y}
        
        y2<-ys[1]
        
        cn<-colnames(df)
        if(length(ys)>1){
          stop("Revise this code. network data was transformed to long format! Need to adapt code using aggregate() function and aggregate by year.")
          ons_pxp<-apply(df[,which(str_detect(cn, "PXP") & str_detect(cn, paste0(ys,collapse = "|")))], 1, FUN=function(x){return(mean(x,na.rm = T))})
          #ons_ixi<-apply(df[,which(str_detect(cn, "IOT_IXI") & str_detect(cn, paste0(ys,collapse = "|")))], 1, mean)
          ons_sut<-apply(df[,which(str_detect(cn, "SUT") & str_detect(cn, paste0(ys,collapse = "|")))], 1, FUN=function(x){return(mean(x,na.rm = T))})
          Payment_amt<-apply(df[,which(str_detect(cn, "Payment") & str_detect(cn, "amt") & str_detect(cn, paste0(ys,collapse = "|")))], 1, FUN=function(x){return(mean(x,na.rm = T))})
          Payment_cnt<-apply(df[,which(str_detect(cn, "Payment") & str_detect(cn, "cnt") & str_detect(cn, paste0(ys,collapse = "|")))], 1, FUN=function(x){return(mean(x,na.rm = T))})
          
        }else{
          dfn<-df[df$year==ys,]
          ons_pxp<-dfn[,str_detect(cn, "PXP")]
          ons_ixi<-dfn[,str_detect(cn, "IXI")]
          ons_sut<-dfn[,str_detect(cn, "SUT")]
          Payment_amt<-dfn[,str_detect(cn, "AMT")]
          Payment_cnt<-dfn[,str_detect(cn, "CNT")]
        }
        
        
        # Keep only edges in graph that exceed threshold level
        if(tr == "no_truncation"){thr<-c(rep(0,5))
        }else if(tr == "median"){thr<-c(median(ons_pxp[ons_pxp>0], na.rm = T), median(ons_ixi[ons_ixi>0], na.rm = T), median(ons_sut[ons_sut>0], na.rm = T), median(Payment_amt[Payment_amt>0], na.rm = T), median(Payment_cnt[Payment_cnt>0], na.rm = T))
        }else if(tr == "mean"){thr<-c(mean(ons_pxp[ons_pxp>0], na.rm = T), median(ons_ixi[ons_ixi>0], na.rm = T), mean(ons_sut[ons_sut>0], na.rm = T), mean(Payment_amt[Payment_amt>0], na.rm = T), mean(Payment_cnt[Payment_cnt>0], na.rm = T))
        }else if(str_detect(tr,"threshold_pct")){
          if(d_type=="upstream"){x<-is[is$year==ys,]}else if(d_type=="downstream"){x<-os[os$year==ys,]}else{stop("implement for undirected network")}
          cls<-which(colnames(x)%in%c("AMT","CNT","SUT","PXP","IXI"))
          x[,cls]<-apply(x[,cls],2,FUN=function(x){return(ifelse((100*x)>as.numeric(str_remove(tr,"threshold_pct_")),1,NA))})
          ons_pxp<-ons_pxp*x$PXP; ons_ixi<-ons_ixi*x$IXI; ons_sut<-ons_sut*x$SUT; Payment_amt<-Payment_amt*x$AMT; Payment_cnt<-Payment_cnt*x$CNT
          thr<-c(rep(0,5))
        }else{stop("Implement trunction method")}
        
        names(thr)<-c("pxp", "ixi", "sut", "amt", "cnt")
        if((y %in% c(2015:2018) && is.element(NaN, thr["sut"])) || (y %in% c(2018:2019) && is.element(NaN, thr["ixi"])) || (y%in%c(2015:2021) && is.element(NaN, thr[3:4])) || (y<=2018 && y!=2016 && is.element(NaN, thr["pxp"]))){stop("thr should not be NaN")}
        g_ons_pxp<-graph.data.frame(dfn[(ons_pxp>thr[["pxp"]] & !is.na(ons_pxp)),c("from", "to")])
        E(g_ons_pxp)$weight<-ons_pxp[(ons_pxp>thr[["pxp"]] & !is.na(ons_pxp))]
        g_ons_ixi<-graph.data.frame(dfn[(ons_ixi>thr[["ixi"]] & !is.na(ons_ixi)),c("from", "to")])
        E(g_ons_ixi)$weight<-ons_ixi[(ons_ixi>thr[["ixi"]] & !is.na(ons_ixi))]
        g_ons_sut<-graph.data.frame(dfn[(ons_sut>thr[["sut"]] & !is.na(ons_sut)),c("from", "to")])
        E(g_ons_sut)$weight<-ons_sut[(ons_sut>thr[["sut"]] & !is.na(ons_sut))]
        g_Payment_amt<-graph.data.frame(dfn[(Payment_amt>thr[["amt"]] & !is.na(Payment_amt)),c("from", "to")])
        E(g_Payment_amt)$weight<-Payment_amt[(Payment_amt>thr[["amt"]]& !is.na(Payment_amt))]
        g_Payment_cnt<-graph.data.frame(dfn[(Payment_cnt>thr[["cnt"]]& !is.na(Payment_cnt)),c("from", "to")])
        E(g_Payment_cnt)$weight<-Payment_cnt[(Payment_cnt>thr[["cnt"]]& !is.na(Payment_cnt))]
        rm(ons_pxp, ons_sut, ons_ixi, Payment_amt, Payment_cnt,thr,cn)
        
        
        # Create lists of networks for plotting and calculating statistics
        z<-list(y=y, amt=simplify(g_Payment_amt), cnt=simplify(g_Payment_cnt), sut=simplify(g_ons_sut), pxp=simplify(g_ons_pxp), ixi=simplify(g_ons_ixi))
        names(z)[2:length(z)]<-paste0(names(z)[2:length(z)], "_", tr)
        if(exists("gr0")){gr0<-list.append(gr0, z)
        }else{gr0<-list(z)}
        rm(z)
        
        n<-"pxp"
        n<-"amt"
        for(n in ntwrks){
          if(n == "pxp"){x<-g_ons_pxp
          }else if(n == "sut"){x<-g_ons_sut
          }else if(n == "amt"){x<-g_Payment_amt
          }else if(n == "cnt"){x<-g_Payment_cnt
          }else if(n == "ixi"){x<-g_ons_ixi
          }else{stop("not intended")}
          
          if(length(x)==0){next}
          #if(MAKE_PLOT){
          # xp<-simplify(x)
          #pdf(paste(outputdir,n,"_net_",y,".pdf", sep=""))
          #plot(xp, loop=F, vertex.size=5, vertex.shape="none", vertex.label.cex=1, edge.arrow.size=0.001, arrow.mode=0)
          #dev.off()
          #}
          hs<-hub_score(x)
          h<-data.frame(code=names(hs$vector), hub_score=hs$vector, name=ll$name[match(names(hs$vector), ll$code)])
          h_top<-h[order(h$hub_score, decreasing = T),]
          h_top[,2]<-data.frame(round(h_top[,2], digits=3))
          if(nrow(h_top)<nrow(ll)){
            fill<-data.frame(matrix(NA, nrow=(nrow(ll)-nrow(h_top)), ncol=3))
            colnames(fill)<-colnames(h_top)
            h_top<-rbind(h_top, fill)
          }
          colnames(h_top)<-paste(n, "_", c("n", "hub", "nm"), sep="")
          if(!exists("hubs_top")){
            hubs_top<-cbind(rank=c(1:nrow(h_top)), h_top)
          }else{
            hubs_top<-merge(hubs_top, data.frame(cbind(rank=1:(nrow(h_top)), h_top)), by="rank", all=T)
          }
          rm(h_top, h)
          
          as<-authority_score(x)
          a<-data.frame(code=names(as$vector), auth_score=as$vector, name=ll$name[match(names(as$vector), ll$code)])
          a_top<-a[order(a$auth_score, decreasing = T),]
          a_top[,2]<-round(a_top[,2], digits=3)
          
          colnames(a_top)<-paste(n, "_", c("n", "auth", "nm"), sep="")
          if(!exists("auth_top")){
            auth_top<-cbind(rank=c(1:nrow(a_top)), a_top)
          }else{
            auth_top<-merge(auth_top, data.frame(cbind(rank=1:(nrow(a_top)), a_top)), by="rank", all=T)
          }
        } # n in ntwrks
        
        write(paste0("\n Top hubs in ", y, " for ",paste(ntwrks,collapse = ", ")), file=fn_h, append = T)
        write.table(hubs_top[1:10,], row.names = F, sep="\t", file=fn_h, append=T)
        write(paste0("\n Top authorities in ", y, " for ",paste(ntwrks,collapse = ", ")), file=fn_a, append = T)
        write.table(hubs_top[1:10,], row.names = F, sep="\t", file=fn_a, append=T)
        
      } # y in years  
      rm(hs, hubs_top, a_top, auth_top, as, a, g_ons_pxp, g_ons_sut, g_Payment_amt, g_Payment_cnt, x,  gn, gn_ixi, g_ons_ixi)
      
    } # tr in TRUNCATION_METHODS
    names(gr0)<-paste(rep(yrs, length(TRUNCATION_METHODS)), rep(TRUNCATION_METHODS, each=length(yrs)))
    rm(tr, n, ys, y2, y1)
    
    if(MAKE_PLOT){

      plotdir<-paste(out_dir,"/Network_plots/",sep="")
      if(!dir.exists(plotdir)){dir.create(plotdir,recursive = T)}
      
      
      tr<-TRUNCATION_METHODS[1]; y<-as.character(yrs[1]) 
      for(tr in TRUNCATION_METHODS){
        for(y in yrs){
          y<-as.character(y)
          g<-gr0[[names(gr0)[(str_detect(names(gr0),tr) & str_detect(names(gr0), as.character(y)))]]]
          ys<-yr_titles[str_detect(yr_titles,y)]
          if(AGGR_YEARS){
            if(y < 2015 && is.element("pre-2015",yr_titles)){ys<-2010:2014
            }else if(y != "post-2018"){
              ys<-as.numeric(substr(ys,1,4)):as.numeric(substr(ys,6,9))
            }else{ys<-2019:2022}
          }
          
          clr<-ll$color
          names(clr)<-ll$name
          
          # Plot legend
          llu<-unique(data.frame(cbind(code=ll$aggr_code, name=ll$aggr_name, color=ll$color)))
          lg<-data.frame(x=1:nrow(llu), y=rep(1, nrow(llu)), llu)
          pnt<-rep(seq(1, 20*((nrow(llu)/2)+1), 20),each=2)
          plot.new()
          pdf(paste(plotdir,"legend_for_nets.pdf", sep=""),width=24, height=4)
          
          plot(rep(pnt,2), c(rep(c(2,1), (length(pnt)/2)), rep(c(0.5,2.5), (length(pnt)/2))) , pch=21, 
               bg=c(as.character(llu$color),"white","white", rep("white",length(pnt))), 
               col=c("white"), 
               axes=F, ylab="", xlab="", cex=3.75, mar=c(0))
          #plot(pnt, rep(c(0,3), (length(pnt)/2)), pch=21,            bg=c("white"), col=c("white"), axes=F, ylab="", xlab="", cex=3.75, mar=c(0))
          text((pnt+2.25), c(2,1), adj=c(0,0.5), labels=c(as.character(llu$name),"",""), cex=(1.35), mar=c(0))
          
          dev.off()
          rm(pnt, llu, lg)
          
          # Obtain measures of the node size
          szs<-dt[dt$time %in% ys,c(which(colnames(dt)=="code" | str_detect(colnames(dt),"puts_")))]
          colnames(szs)<-str_to_lower(colnames(szs))
          #if(y>2018 || y<2015){ # Remove columns with missing data after 2018
            crm<-c()
            for(cl in 1:ncol(szs)){if(sum(is.na(szs[,cl]))==nrow(szs)){crm<-c(crm,cl)}}
            if(length(crm)>0){szs<-szs[,-crm]}
          #}
          if(AGGR_YEARS){szs <- aggregate(. ~ code, szs, FUN = function(x){return(mean(x,na.rm=T))})}
          
          if(ALL_IN_ONE){
            pdf(paste0(plotdir, DTYPE, "_", d_type, "_", tr, "_", steps,"_", y, ".pdf"), width=(8), height=4*(length(ntwrks)/2))
            par(mfrow = c((length(ntwrks)/2),2)) #, mar=c(1.75,0,1.75,0))     
          }
          nt<-ntwrks[1]
          for(nt in ntwrks){
            nm<-str_to_upper(nt)
            gn<-g[[names(g)[str_detect(names(g),nt)]]]
            if(length(gn)==0){print(paste("Network data for",nt,y,"not available.")); next}
            gn<-simplify(gn)
            # Remove disconnected nodes from network for plotting
            if(sum(degree(gn)==0)>0){
              print(paste(paste(ll$name[ll$code %in% names(V(gn))[degree(gn)==0]],collapse=", "),"have size zero in",nt,d_type,"and are removed."))
              gn<-delete_vertices(gn, names(V(gn))[degree(gn)==0])
            }
            
            # Set node size and remove nodes with size zero
            s<-szs[match(names(V(gn)),szs$code),which(str_to_lower(colnames(szs))==paste0("sum_", "in"[d_type=="downstream"],"out"[d_type=="upstream"],"puts_",nt))]
            if(sum((is.na(s) | s==0))>0){
              print(paste(paste(ll$name[ll$code %in% names(V(gn))[(is.na(s) | s==0)]],collapse=", "),"have size zero in",nt,d_type,"and are removed."))
              gn<-delete_vertices(gn, names(V(gn))[(is.na(s) | s==0)])
              s<-szs[match(names(V(gn)),szs$code),which(str_to_lower(colnames(szs))==paste0("sum_","in"[d_type=="downstream"],"out"[d_type=="upstream"],"puts_",nt))]
            }
            s<-scale(s, center = F) * 10
            #s<-log(s)
            if(sum(s==0,na.rm=T)>0){stop("s is 0")}
            #Set node color
            clr<-ll$color[match(names(V(gn)),ll$code)]
            
            names(s)<-names(clr)<-names(V(gn))
            # Set node weights
            w<-log(1+E(gn)$weight)
            w[which(is.nan(w))]<-0
            if(!is.null(w)){w<-scale(w, center = F) }else{w<-1}  
            
            subtxt<-paste("# Nodes: ", length(V(gn)), ", # Links: ", length(E(gn)), sep="")
            print(paste(nt,"--- s:",paste(range(s),collapse = ","),"--- w:",paste(range(w),collapse = ",")))
            
            if(!ALL_IN_ONE){pdf(paste0(plotdir, DTYPE, "_", d_type,"_",tr,"_",steps,"_", y,"_",nt,".pdf"), width=(8), height=4*(length(ntwrks)/2))}
            plot(gn, loop=F, 
                 #xlim=c(-0.75,0.75), ylim=c(-0.75, 0.75),
                 vertex.size=s, 
                 vertex.shape="circle", vertex.label=NA,
                 vertex.color=adjustcolor(clr, alpha.f = 0.75),
                 #edge.cex=0.000010*w,
                 edge.color="gray40",edge.arrow.size=0.01, arrow.mode=0,
                 #main=nm, sub=subtxt,
                 margin=c(0)
                 #margin=c(-0.2,0,-0.2,0)
            )
            if(ALL_IN_ONE){title(main=nm, sub=subtxt, adj=0.5, line=0, cex.main=1, cex.sub=0.5)
            }else{title(sub=subtxt, adj=0.5, line=0, cex.sub=(1)); dev.off()}
            
            rm(gn, clr, w, s, nm, subtxt)
            
          } # nt in ntwrks
          
          if(ALL_IN_ONE){dev.off()}
          
          rm(nt, szs, ys, g)
        } # y in yrs
      } # tr in TRUNCATION METHODS
      rm(tr, y)

      
    }# make plots
    
    # Write some aggregate network statistics to table.
    source("code/network_analysis/descriptives/src/helpers/aggregate_statistics.r")
  } # d_type in upstream, downstream
  rm(d_type)
  
} # for DTYPE in raw_transaction, input and output share matrix (MATRICES)
rm(DTYPE,tbl,df1)
rm(outputdir,gr0,ll)
rm(fn_h, fn_a,df0)
rm(app, ntwrks, top10dir)

tbl1<-data.frame()
for(DTYPE in MATRICES){
  for(d_type in DIRECTIONS){
    load(paste0(out_dir,"Aggregate_network_statistics_",DTYPE,"_",d_type,".RData"))
    tbl<-as.data.frame(tbl)
    tbl$Network<-DTYPE
    tbl$Direction<-d_type
    tbl$Variable<-rownames(tbl)
    rownames(tbl)<-NULL
    tbl1<-rbind(tbl1, tbl)
  }
}
rm(tbl)

tabdir<-paste0(out_dir,"Tables_network_stats/")
if(!dir.exists(tabdir)){dir.create(tabdir)}
tr<-TRUNCATION_METHODS[1]; y<-yrs[1]
fn<-paste0(tabdir,"Aggregate_network_statistics.tex")
write("",file=fn,append=F)
for(tr in TRUNCATION_METHODS){
  for(y in yrs){
    cl<-c(which(colnames(tbl1) %in% c("Network","Direction","Variable")), which(str_detect(colnames(tbl1), tr) & str_detect(colnames(tbl1), as.character(y))))
    
    tbl2<-tbl1[,cl]
    colnames(tbl2)<-str_trim(str_remove_all(colnames(tbl2), paste(y,tr,collapse = "|")))
    colnames(tbl2)<-str_replace_all(colnames(tbl2), "pxp", "PxP")
    colnames(tbl2)<-str_replace_all(colnames(tbl2), "ixi", "IxI")
    colnames(tbl2)<-str_replace_all(colnames(tbl2), "sut", "SUT")
    colnames(tbl2)<-str_replace_all(colnames(tbl2), "amt", "Value")
    colnames(tbl2)<-str_replace_all(colnames(tbl2), "cnt", "Count")

    # Remove columns with only NA content. 
    crm<-c()
    for(cl in which(!(colnames(tbl2)%in%c("Network","Direction","Variable")))){
      tbl2[tbl2[,cl]%in%c("NaN","NA"),cl]<-NA
      if(sum(tbl2[!is.na(tbl2[,cl]),cl], na.rm = T)==0){crm<-c(crm,cl)}
    } 
    if(length(crm)>0){tbl2<-tbl2[,-crm]}
    rm(crm,cl)
    
    # Comment out rows that are identical across networks (density, degree, etc.)
    rw<-apply(tbl2[,!colnames(tbl2)%in%c("Network","Direction")],1,FUN = function(x){return(paste(x, collapse=" "))})
    tbl3<-tbl2[!duplicated(rw),]
    rwn<-as.data.frame(table(paste(tbl3$Variable, tbl3$Network)))
    v2<-str_trim(str_remove_all(as.character(rwn$Var1)[rwn$Freq==1], paste(tbl3$Network,collapse = "|")))
    if(length(v2)>0){tbl3$Direction[tbl3$Variable %in% v2]<-""}
    rwd<-as.data.frame(table(paste(tbl3$Variable, tbl3$Direction)))
    v2<-as.character(rwd$Var1)[rwd$Freq==1]
    #if(length(v2)>0){stop("take action")}
    rm(rw, rwn, rwd, v2)
    
    # Add manually additional rows for subsections in table (by network and direction)
    cl<-which(!(colnames(tbl3)%in%c("Network","Direction")))
    dd<-unique(paste(tbl3$Network, tbl3$Direction))
    add<-data.frame(dd, matrix(NA, nrow = length(dd), ncol=(length(cl)-1)))
    colnames(add)<-colnames(tbl3)[cl]
    i<-match(add[,1], paste(tbl3$Network, tbl3$Direction))
    add[,1]<-str_to_sentence(str_replace_all(str_remove_all(add[,1],"_matrix"),"_"," "))
    add[,1]<-str_replace_all(add[,1],"share","shares")
    add[,1]<-paste("\\\\hline  \\\\hline ",add[,1],"\\\\hline")
    if(nrow(add)==3){
      tbl<-rbind(add[1,],tbl3[i[1]:(i[2]-1),cl],
                 add[2,],tbl3[i[2]:(i[3]-1),cl],
                 add[3,],tbl3[i[3]:nrow(tbl3),cl])
      }else if(nrow(add)==6){
        tbl<-rbind(add[1,],tbl3[i[1]:(i[2]-1),cl],
                   add[2,],tbl3[i[2]:(i[3]-1),cl],
                   add[3,],tbl3[i[3]:(i[4]-1),cl],
                   add[4,],tbl3[i[4]:(i[5]-1),cl],
                   add[5,],tbl3[i[5]:(i[6]-1),cl],
                   add[6,],tbl3[i[6]:nrow(tbl3),cl])
      }else if(nrow(add)==7){
        tbl<-rbind(add[1,],tbl3[i[1]:(i[2]-1),cl],
                   add[2,],tbl3[i[2]:(i[3]-1),cl],
                   add[3,],tbl3[i[3]:(i[4]-1),cl],
                   add[4,],tbl3[i[4]:(i[5]-1),cl],
                   add[5,],tbl3[i[5]:(i[6]-1),cl],
                   add[6,],tbl3[i[6]:(i[7]-1),cl],
                   add[7,],tbl3[i[7]:nrow(tbl3),cl])
      }else{stop("adjust code")}
    
    rm(add, cl, i)
    # Write to latex table
    write(stargazer(tbl, rownames = F, summary = F, title=paste0("Properties of the payment and ONS-based input-output networks in ",y), label = paste("tab:network_stats",y,tr,sep="_"),notes = paste0("Network truncation: ",str_replace_all(tr,"_"," ")), digits = 3, font.size = "scriptsize", column.sep.width = "1pt"), file=fn,append = T)
  } # y in yrs
} # tr in TRUNCATION METHODS
rm(tbl2, tbl3, tbl, tr, y, tabdir)




