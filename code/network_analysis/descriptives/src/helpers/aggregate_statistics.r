#dnsty<-function(ed,n){return(ifelse(ed>0,((ed)/(n*(n-1))),0))}


# Aggregate network statistics: 
y1<-5; tr<-TRUNCATION_METHODS[2]
for (tr in TRUNCATION_METHODS){
  
  for(y1 in 1:length(yrs)){
    z<-gr0[[which(names(gr0) == paste(yrs[y1], tr))]]
    n<-ntwrks[1]
    for(n in ntwrks){
      nm<-paste(n, yrs[y1], sep=" ")
      xp<-z[[which(str_detect(names(z),n))]]
      #dens<-dnsty(nrow(get.data.frame(xp)), length(unique(ll$code)))
      dens<-edge_density(xp, loops=T)
      recip<-reciprocity(xp, ignore.loops = F)
      trns<-transitivity(xp, type = "global", isolates = "zero") # clustering coefficient
      #trc<-triad_census(xp)[16]
      dia<-diameter(xp, directed = T)
      diauw<-diameter(xp, directed = T, weights = NA)
      mdist<-mean_distance(xp, directed=T)
      mdistud<-mean_distance(xp, directed=F)
      asd<-assortativity_degree(xp, directed = T)
      asdu<-assortativity_degree(xp, directed = F)
      avg_deg_in<-mean(degree(xp, v=V(xp), mode="in"), na.rm=T)
      avg_str_in<-mean(strength(xp, vids=V(xp), mode="in", weights=E(xp)$weight), na.rm=T)
      #avg_deg_out<-mean(degree(xp, v=V(xp), mode="out"), na.rm=T)
      #avg_str_out<-mean(strength(xp, vids=V(xp), mode="out", weights=E(xp)$weight), na.rm=T)
      avg_weight<-mean(E(xp)$weight) #*100
      if(DTYPE == "raw_transactions_matrix"){
        #if(n %in% c("amt")){avg_weight<-avg_weight/10**6; avg_str_in<-avg_str_in/10**6
        #}else if(n %in% c("cnt")){avg_weight<-avg_weight/10**3; avg_str_in<-avg_str_in/10**3}
      }
      #w<-E(xp)$weight
      l<-names(V(xp))
      s<-dt[which(dt$year == yrs[y1])[match(l,dt$code[dt$year == yrs[y1]])], which(str_to_lower(colnames(dt)) == paste0("sum_outputs_",n))]
      s[is.na(s)]<-0
      xp<-set_vertex_attr(xp, "size", index=V(xp), s)
      ass_output<-assortativity(xp, V(xp)$size, directed = T)
      assu_output<-assortativity(xp, V(xp)$size, directed = F)
      s<-dt[which(dt$year == yrs[y1])[match(l,dt$code[dt$year == yrs[y1]])], which(str_to_lower(colnames(dt)) == paste0("sum_inputs_",n))]
      s[is.na(s)]<-0
      xp<-set_vertex_attr(xp, "size", index=V(xp), s)
      ass_input<-assortativity(xp, V(xp)$size, directed = T)
      assu_input<-assortativity(xp, V(xp)$size, directed = F)
      
      rw<-data.frame(Density=dens, "Avg_degree"= avg_deg_in, "Avg_strength"=avg_str_in, "Avg_weight"=avg_weight,
                     "Reciprocity"=recip, "Transitivity"=trns, 
                     "Diameter_weighted"=dia, "Diameter"=diauw, 
                     "Avg_distance_weighted" = mdist, "Avg_distance" = mdistud, 
                     "Assortativity_by_degree_directed"=asd, "Assortativity_by_degree"=asdu, 
                     "Asortativity_by_output_size_directed"=ass_output, "Assortativity_by_output_size"=assu_output, "Asortatativity_by_input_size_directed"=ass_input, "Assortativity_by_input_size"=assu_input) 
      row.names(rw)<-paste(nm, tr)
      
      if(!exists("tbl") || (tr == TRUNCATION_METHODS[1] && n == ntwrks[1] && y1==1)){
        tbl<-rw
      }else{tbl<-rbind(tbl, rw)}
      
      
      rm(rw, xp, ass, assu, s, l, s, asd, asdu, asd, ass_output, ass_input, assu_output, assu_input, cl, crm, avg_deg_in, avg_str_in, avg_weight, dens, dia, diauw, j, l, mdist, mdistud, nm, recip, trns)
    } # n in ntwrks
    
    
  } # y1 in 1:length(yrs)
  
} # tr in TRUNCATION 

rm(z, tr, y1, n)

tbl<-t(tbl)

rownames(tbl)<-str_replace_all(rownames(tbl),"_"," ")
rownames(tbl)<-str_replace_all(rownames(tbl),"Avg","Average")
save(tbl, file = paste0(out_dir,"Aggregate_network_statistics_",DTYPE,"_",d_type,".RData"))


