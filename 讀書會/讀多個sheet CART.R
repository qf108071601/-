## 讀多個xlsx的sheet 

library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)

filename <-"C:/Users/Lai/Desktop/Mastering R for QF/1022/道瓊.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets

SheetList = lapply(1:25, function(x){
  SheetList[[x]] %>% as.data.frame()
})


# SheetList[[15]] %>% purrr::map(~as.numeric(.)) %>% 
#   as.data.frame()%>%  map(~sum(is.na(.))) %>% as.matrix()


col = c("NET_REV",
        "EV_TO_EBITA",
        "CUR_RATIO",
        "QUICK_RATIO",
        "EV_TO_EBITA")


SheetList = lapply(1:25, function(x){
  data = SheetList[[x]] %>%
    purrr::map(~as.numeric(.)) %>%  
    as.data.frame()
  data[,which(colnames(data) %in% col==F)]
})


  

data = SheetList[[1]] %>% 
  purrr::map(~as.numeric(.)) %>% 
  as.data.frame()%>% 
  na.omit()


# %>% 
#   map(~median(.)) %>%
#   as.data.frame(row.names = sheets[1])

n = c(2:25)
for (i in n) {
  x = SheetList[[i]] %>% 
  purrr::map(~as.numeric(.)) %>% 
  as.data.frame()%>% 
    na.omit() %>% 
  map(~median(.)) %>%
  as.data.frame(row.names = sheets[i])
  data = rbind(data,x)
}

# CASH_TO_TOT_ASSET: 現金/總資產
# NET_FIX_ASSET_TO_TOT_ASSET：淨固定資產/總資產
# ASSET_PER_EMPL:每千名員工資產
# PX_TO_CASH_FLOW: 價格/每股現金流量
# LT_DEBT_TO_TOT_CAP: 長期負債/總資本
# BS_TOT_CAP: 總資本
# PE_RATIO: 本益比


## 分析
#########

# 9. dataecision tree

data_tree <- data[,c(2:15)]

colnames(data_tree)

library(rpart)
tree <- rpart(RETURN_ON_INV_CAPITAL ~., data = data_tree, maxdepth = 6 ,cp =0.005)
tree <- prune(tree, cp = 0.01)

x = tree$variable.importance %>% as.data.frame() %>% rownames()
x = x[1:3]
