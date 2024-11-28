# Helper functions
# 
# 
# Shorten sector name

shorten_name<-function(x){
  
  if(!is.character(x) && !is.factor(x)){stop("suggested name column is not character like")}
  # shorten sector names
  x<-str_replace_all(x,"Buying and selling, renting and operating of own or leased real estate, excluding imputed rental","Buying, selling, renting & operating real estate, excluding imputed rental")
  x<-str_replace_all(x,"Travel agency, tour operator and other reservation service and related activities","Travel agency, tour operator, other reservation services")
  x<-str_replace_all(x,"Motion picture, video and TV programme production, sound recording and music publishing activities","Motion picture & TV programme production, sound recording, music publishing")
  
  x<-str_replace_all(x," and related activities", "")
  x<-str_replace_all(x,"ing activities", "ing")
  x<-str_replace_all(x,"Manufacture of ", "Mnfct. of ")
  x<-str_replace_all(x,"except", "exc.")
  x<-str_replace_all(x,"excluding", "excl.")
  x<-str_replace_all(x," and ", " & ")
  
  return(x)
}