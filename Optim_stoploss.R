library(TTR)
library(doParallel) 
library(foreach)

cl<-makeCluster(4)
registerDoParallel(cl)

days = 20
slope= 5

spy = read.csv("SPY_Daily.csv",stringsAsFactors = F)
str(spy)
plot(spy$Close)
spy$sma20 = EMA(spy$Close,days)
spy$Trend = rep(NA,nrow(spy))
for(i in (2*days+1):nrow(spy)){
  x=1:days
  m = lm(spy$sma20[(i-days+1):i] ~ x)
  slope = m[[1]][2]
  angle = atan(slope)*180/pi
  if(angle > 10){
    spy$Trend[i] = "UP"
  }else if(angle < -10){
    spy$Trend[i] = "DOWN"
  }else if(angle > -10 & angle < 10){
    spy$Trend[i] = "NEU"
  }
}

str(spy)

foreach(d = c(1:60)) %dopar%{
  for(loop in 1:(d)){
    
    
    entered = F
    
    spy$total_val = rep(NA,nrow(spy))
    spy$units_loop = rep(NA,nrow(spy))
    spy$total_equity = rep(NA,nrow(spy))
    spy$pnl = rep(NA,nrow(spy))
    spy$pnl_percent = rep(NA,nrow(spy))
    
    units=0
    prev_am = 0
    eq=40000
    
    
    for(i in ((d+1):nrow(spy))){
      
      if(((spy$Close[i] > max(spy$Close[(i-1) : (d)]))  & entered == F) | 
         ((spy$Close[i] < min(spy$Close[(i-1) : (d)]))  & entered == F) ){
        
        long = ifelse(((spy$Close[i] > max(spy$Close[(i-1) : (d)]))),T,F )
        
        entered = T
        units = floor(20000/spy$Close[i])
        spy$units_loop[i] = units
        spy$total_val[i] = units*spy$Close[i]
        prev_am = spy$total_val[i]
        
      }
      
      if((units*spy$Close[i] - prev_am) > ((stoploss*prev_am)/100)){
        entered = F
        
        
        spy$total_val[i] = units*spy$Close[i]
        curr_am = spy$total_val[i]
        
        dif = ifelse(long==T,curr_am-prev_am,prev_am - curr_am)
        
        
        spy$pnl[i] = ifelse(dif>0, (dif - 20)*0.7,
                            (dif - 20))
        eq = eq+spy$pnl[i]
        spy$total_equity[i] = eq
        spy$pnl_percent[i] = spy$pnl[i]*100/prev_am
      }
      
      if(((spy$Close[i] < min(spy$Close[(i-1) : (loop)]))  & entered == T) | 
         ((spy$Close[i] > max(spy$Close[(i-1) : (loop)]))  & entered == T) ){
        
        entered = F
        
        
        spy$total_val[i] = units*spy$Close[i]
        curr_am = spy$total_val[i]
        
        dif = ifelse(long==T,curr_am-prev_am,prev_am - curr_am)
        
        
        spy$pnl[i] = ifelse(dif>0, (dif - 20)*0.7,
                            (dif - 20))
        eq = eq+spy$pnl[i]
        spy$total_equity[i] = eq
        spy$pnl_percent[i] = spy$pnl[i]*100/prev_am
        
      }
      

      
    }
    
    
    
    
    write.csv(spy,file= paste("SPY_Straight",d ,"-",loop,".csv"),row.names=F)
    
  }}

stopCluster(cl)
