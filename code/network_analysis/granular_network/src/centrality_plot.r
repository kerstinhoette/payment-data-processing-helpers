library(poweRlaw); library(tailloss); library(igraph)


# Calculate and plot CCDF of Katz Bonacich centrality
# 
df0<-dfy
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
## THIS WOULD REQUIRE A YEAR-BY-YEAR IMPLEMENTATION!
# Truncate the network an keep only links > 10% quantile
#df1$input_share[df1$input_share < quantile(df1$input_share,0.1)]<-NA
#df1$output_share[df1$output_share < quantile(df1$output_share,0.1)]<-NA
# As an alternative, use 0.1% threshold

TRUNCATE<-F
if(TRUNCATE){
  df1$input_share[df1$input_share < 0.05]<-NA
  df1$output_share[df1$output_share < 0.05]<-NA
  
}


# Transform time and variable colums into time format
df1$time<-as.numeric(gsub("\\D", "", df1$variable))
df1$variable<-substr(df1$variable,6,8)
rm(inputs, outputs)
# 
#alpha_centrality(graph, nodes = V(graph), alpha = 1,loops = FALSE, exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE )

alpha<-0.5 # taken from Glenn Magermann paper

# Use the annual network from 2019 (and for robustness of 2018 for plotting)
y<-2018; tp<-"amt"

df2<-df1[(df1$variable==tp & df1$time == y),c("from", "to", "input_share")]

colnames(df2)[3]<-"weight"


df2<-df2[(df2$from!="0" & df2$to!="0" & df2$weight>0 & !is.na(df2$weight)),]

g<-graph_from_edgelist(as.matrix(df2[,c(1,2)]))
E(g)$weights<-df2$weight

centr<-alpha.centrality(g, alpha = 0.5,  weights = E(g)$weights, loops = T)
centr2<-alpha.centrality(g, alpha = 0.5)
centr3<-katzcent(g, alpha = 0.5)
centr3<-page_rank(g, directed = T)$vector
msrs<-proper_centralities(g)
centr4<-calculate_centralities(g, include = c("Alpha Centrality", "Bonacich power centralities of positions", "Page Rank", "Katz Centrality (Katz Status Index)"))


CDF<-ecdf(centr)
plot(CDF)
CDF<-ecdf(centr2)
plot(CDF)
CDF<-ecdf(centr3)
plot(CDF)
CDF<-ecdf(centr4$`Page Rank`)
plot(CDF)

x<-centr
x<-centr2
x<-centr4$`Katz Centrality (Katz Status Index)`
x<-centr4$`Alpha Centrality`
x<-centr4$`Page Rank`
x<-centr5

f<-ecdf(x)
(plot(sort(x), 1-f(sort(x)), type="s", lwd=1, log="xy"))
x<-sort(x); y<-1-f(sort(x))
df<-data.frame(cbind(x))

g<-ggplot(df, aes(x=x,y=y)) + geom_line() + theme_bw()

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

plot(g)

range(x)

fit_power_law(x)

pl.fit <- power.law.fit(x)

# Plotten der Fit-Kurve
plot(pl.fit, main="Power Law Fit")

hill_estimator <- function(x) {
  n <- length(x)
  k_max <- min(100, floor(n/2))  # Wähle die maximale Anzahl von Exponenten
  alpha_hat <- numeric(k_max)
  
  # Berechne die Hill-Schätzer für verschiedene k-Werte
  for (k in 1:k_max) {
    x_sorted <- sort(x, decreasing = TRUE)
    alpha_hat[k] <- mean(log(x_sorted[1:k]) - log(x_sorted[k + 1]))
  }
  
  # Wähle den Schätzer mit dem kleinsten MSE
  mse <- numeric(k_max)
  for (k in 1:k_max) {
    mse[k] <- mean((alpha_hat[k_max] - alpha_hat[k])^2)
  }
  
  k_opt <- which.min(mse)
  alpha <- alpha_hat[k_opt]
  xmin <- min(x)
  
  # Rückgabe der Ergebnisse
  return(list(alpha = alpha, xmin = xmin))
}

he<-hill_estimator(x)
he
z<-seq(min(x),max(x),0.001)
plot(z,z**he$alpha)

