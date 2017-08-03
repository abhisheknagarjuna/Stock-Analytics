library(jmotif)
library(TSMining)

data(test)
setwd("C:/Stock Analytics")
spy = read.csv("SPY_Daily.csv",stringsAsFactors = F)
spy2 = spy[1:100,]
#Perform univariate motif discovery for the first dimension data in the example data
res.1 <- Func.motif(ts = spy2$Close, global.norm = TRUE, local.norm = FALSE,
                    window.size = 5, overlap = 0, w = 5, a = 3, mask.size = 3, eps = .01)
data.vis <- Func.visual.SingleMotif(single.ts=spy2$Close, window.size=10, motif.indices=res.1$Indices)
#To visualize general information of motifs discovered on the whole time series
library(ggplot2)
ggplot(data = data.vis$data.1) +
  geom_line(aes(x = 1:dim(data.vis$data.1)[1], y = X)) +
  geom_point(aes(x = 1:dim(data.vis$data.1)[1], y = X, color=Y))
#To visualize the detailed information of the 1st motif
ggplot(data = data.vis$data.2[[1]]) + geom_line(aes(x = Time, y = Value, linetype=Instance))



spy2 = spy[1:600,]
library(ggplot2)
#Visualization
res.1 <- Func.motif(ts = spy2$Close, global.norm = TRUE, local.norm = FALSE,
                    window.size = 5, overlap = 0, w = 5, a = 3, mask.size = 3, eps = .01)
data.wcc <- Func.visual.SingleMotif(single.ts = spy2$Close, window.size = 5, motif.indices = res.1$Indices)

#Determine the total number of motifs discovered in the time series of WCC
n <- length(unique(data.wcc$data.1$Y))
#Make the plot
1:dim(data.wcc$data.1)[1]
spy2$Date = as.Date(spy2$Date,format = "%d-%m-%Y")
ggplot(data = data.wcc$data.1) +  
  geom_line(aes(x = spy2$Date, y = X)) +
  geom_point(aes(x = spy2$Date, y = X, color=Y, shape=Y))+
  scale_shape_manual(values = seq(from = 1, to = n)) +
  guides(shape=guide_legend(nrow = 2)) +
  xlab("Date") + ylab("$") +
  theme(panel.background=element_rect(fill = "white", colour = "black"),
        legend.position="top",
        legend.title=element_blank())


library(plotly)
p <- spy2 %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) %>%
  add_lines(y = ~AAPL.Open, line = list(color = 'black', width = 0.75)) %>%
  layout(showlegend = FALSE)
