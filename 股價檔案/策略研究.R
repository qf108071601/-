library(rvest)
library(pbapply)
library(TTR)
library(dygraphs)
library(lubridate)
library(tidyquant)
library(timetk)
library(tidyverse)

pacman::p_load(dygraphs,DT)


library(quantmod)
tickers <- c("AAPL", "MSFT","GOOGL","IBM","FB")
getSymbols(tickers)

library(BiocParallel)
f = function(x) Ad(get(x))

options(MulticoreParam=quote(MulticoreParam(workers=4)))
param <- SnowParam(workers = 2, type = "SOCK")

vec=c(tickers[1],tickers[2],tickers[3],tickers[4])
#vec=c(paste0(quote(tickers),"[",1:length(tickers),"]",collapse=","))
multicoreParam <- MulticoreParam(workers = 7)

bio=bplapply(tickers, f, BPPARAM = multicoreParam)
biodata<-do.call(cbind, bio)

AdjustedPrices = biodata
dateWindow <- c("2016-01-01", "2017-09-01")

dygraph(AdjustedPrices, main = "Value", group = "stock") %>%
  dyRebase(value = 100) %>%
  dyRangeSelector(dateWindow = dateWindow)

df <- data.frame(Date=index(AAPL),coredata(AAPL))

sdata<- df %>% dplyr::select(-Date)

#sdata<-tk_xts(data,date_var=Date)

df_50=as.data.frame.matrix(apply(sdata, 2, SMA,50))
colnames(df_50)=paste0(colnames(df_50),"_sma50")

df_200=as.data.frame.matrix(apply(sdata, 2, SMA,200))
colnames(df_200)=paste0(colnames(df_200),"_sma200")

df_all<-cbind.data.frame(Date=df$Date,df_200,df_50) %>% drop_na()

# sma 50
f50<- function(x) SMA(x,50)

# sma 50
f200<- function(x) SMA(x,200)

#library(pryr)
#data %>% plyr::colwise() %>% f50

df_all<-tk_xts(df_all,date_var=Date)


var=names(df_all)[str_detect(names(df_all), "AAPL")]


dateWindow=c("2014-01-01","2018-02-01")

dygraph(df_all[,var],main = 'Apple Moving Averages') %>%
  dySeries('AAPL.Adjusted_sma50', label = 'sma 50') %>%
  dySeries('AAPL.Adjusted_sma200', label = 'sma 200') %>%
  dyRangeSelector(height = 30) %>%
  dyShading(from = '2016-01-01', to = '2016-9-01', color = '#CCEBD6') %>%
  dyShading(from = '2016-9-01', to = '2017-01-01', color = '#FFE6E6')%>%
  dyRangeSelector(dateWindow = dateWindow)





