library(rvest)
library(qs)
library(tidyverse)
library(httr)
library(jsonlite)

fun = function(date,type1,type2){  
  url <- paste0("https://www.twse.com.tw/exchangeReport/MI_INDEX?response=csv&date="
                ,date,"&type=",type1,type2)
  
  res = GET(url)
  
  data = (rawToChar(res$content))
  
  d = strsplit(data,split = "\n")
  
  frame = lapply(1:length(d[[1]]), function(a){
    dd = strsplit(d[[1]][a],"(\",\")")
    n = dd[[1]] %>%length()
    
    if (n>10) {
      dd[[1]][1] = gsub("(\")","",dd[[1]][1])
      dd[[1]][16] = gsub("(\",\r)","",dd[[1]][16])
      
      ddd = sapply(1:length(dd[[1]]), function(a){
        dd[[1]][a] = gsub("(,)","",dd[[1]][a])
      })
      ddd = as.matrix(t(ddd))
    }
  })
  
  
  
  a = sapply(1:length(frame), function(a){
    is.null(frame[[a]])
  })
  
  f = frame[which(a==FALSE)]
  ff = f[[2]] %>% as.matrix()
  
  for (i in 3:length(f)) {
    fff = f[[i]] %>% as.matrix()
    ff = rbind(ff,fff)
  }
  colnames(ff) = f[[1]]
  return(ff)
  
}

seq <- seq(from=20210501, to=20210515, by=1)

l1 = lapply(seq, function(a){
  return(tryCatch(fun(date = a,type1=0,type2=1) %>% 
                        as.data.frame(.,stringsAsFactors=F),
         error=function(e) NULL))
})
names(l1) = seq

seq <- seq(from=20210516, to=20210531, by=1)

l2 = lapply(seq, function(a){
  return(tryCatch(fun(date = a,type1=0,type2=1) %>% 
                    as.data.frame(.,stringsAsFactors=F),
                  error=function(e) NULL))
})
names(l2) = seq

M5 = c(l1,l2)

names(M5) = as.Date(0:30, origin = "2021-05-01")

# qsave(M5,"C:/Users/Lai/Desktop/股價檔案/水泥五月")

#01,24,17,15
