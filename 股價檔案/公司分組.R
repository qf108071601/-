library(rvest)
library(qs)
library(tidyverse)
library(httr)
library(jsonlite)

L = qread("C:/Users/Lai/Desktop/股價檔案/水泥_日期別")
LL = bind_rows(L, .id = "Date")

N = paste0(LL$證券代號,":",LL$證券名稱) %>% unique()
NN = sapply(1:length(N), function(a){
  str_split(N,":")[[a]][1]
})
水泥 = lapply(1:length(N), function(a){
  LL %>% filter(證券代號==NN[a]
                    )
})
names(水泥) = N
qsave(水泥,"C:/Users/Lai/Desktop/股價檔案/水泥_公司別")


L = qread("C:/Users/Lai/Desktop/股價檔案/半導體_日期別")
LL = bind_rows(L, .id = "Date")

N = paste0(LL$證券代號,":",LL$證券名稱) %>% unique()
NN = sapply(1:length(N), function(a){
  str_split(N,":")[[a]][1]
})
水泥 = lapply(1:length(N), function(a){
  LL %>% filter(證券代號==NN[a]
  )
})
names(水泥) = N
qsave(水泥,"C:/Users/Lai/Desktop/股價檔案/半導體_公司別")

L = qread("C:/Users/Lai/Desktop/股價檔案/金融股_日期別")
LL = bind_rows(L, .id = "Date")

N = paste0(LL$證券代號,":",LL$證券名稱) %>% unique()
NN = sapply(1:length(N), function(a){
  str_split(N,":")[[a]][1]
})
水泥 = lapply(1:length(N), function(a){
  LL %>% filter(證券代號==NN[a]
  )
})
names(水泥) = N
qsave(水泥,"C:/Users/Lai/Desktop/股價檔案/金融股_公司別")

L = qread("C:/Users/Lai/Desktop/股價檔案/航運股_日期別")
LL = bind_rows(L, .id = "Date")

N = paste0(LL$證券代號,":",LL$證券名稱) %>% unique()
NN = sapply(1:length(N), function(a){
  str_split(N,":")[[a]][1]
})
水泥 = lapply(1:length(N), function(a){
  LL %>% filter(證券代號==NN[a]
  )
})
names(水泥) = N
qsave(水泥,"C:/Users/Lai/Desktop/股價檔案/航運股_公司別")
