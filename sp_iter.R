files = list.files()

for(f in 3:6){

df = read.csv(files[f],stringsAsFactors =F)
str(df)
for(s in seq(5,15,1)){
en = F
ex = T
eq = 40000

df$new_prf = rep(NA,nrow(df))

df$new_eq = rep(NA,nrow(df))
sl = s
for(i in 1:nrow(df)){
  #print(eq)
  if(is.na(df$Units[i]) == F & ex == T & en == F){
    
    en = T
    ex = F
    un = df$Units[i]
    ini = df$Total.Val[i]
    l = ifelse(df$Signal_long[i] == "BUY",T,F)
    
    next  
  }
  if(en == T & is.na(df$Total.Equity[i]) == T & ex == F){
   
    pdf = ifelse(l == T, un*df$Close[i] - ini, ini - un*df$Close[i] )
    #print(paste( pdf, (sl*ini/100)))
    if(pdf < 0){
      
    if(abs(pdf) > (sl*ini/100)){
      #print(paste(i,"entered"))
      df$new_prf[i] = pdf
      eq = eq + pdf
      df$new_eq[i] = eq
      ex = T
      en = F
      next
     }
    }
    next
  }
  
  if(en == T & is.na(df$Total.Equity[i]) == F){
    
    eq = eq + df$Profitability[i]
      df$new_eq[i] = eq 
    df$new_prf[i] = df$Profitability[i]
    ex = T
    en = F
    next
    
  }
}

write.csv(df,file=paste(files[f],"Stoploss",sl,".csv"),row.names=F)
}
}
