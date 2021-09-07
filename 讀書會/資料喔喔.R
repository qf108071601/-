
library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)

filename <-"C:/Users/Lai/Desktop/sp500.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets

# SheetList = lapply(1:9, function(x){
#   SheetList[[x]] %>% as.data.frame()
# })

x = cbind(SheetList[[1]][,1],sheets[1])

for (i in 2:9) {
  x = rbind(x,cbind(SheetList[[i]][,1],sheets[i]))
}

      

      